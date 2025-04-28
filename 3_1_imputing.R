##Missing values
library(dplyr)
library(ggplot2)
library(tidyr)
library(mice)
library(kableExtra)

#########checking for missing values for for dependent variable
df_total_long_2019 <- readRDS("df_total_long_2019.rds")

####
#drop all missing values in either dependent variable 
df_2019_clean <- df_total_long_2019 %>% 
  filter(!is.na(profit_margin) | !is.na(revenue_IHS))


#Check percentage of missing values in each variable
colSums(is.na(df_2019_clean)) / nrow(df_2019_clean) * 100

##plot
  #Vector of variables to include
vars_2 <- c("revenue_IHS", "profit_margin", "employees_IHS", "total_assets_IHS", "firm_age")

  #Calculate missingness
missing_table_2 <- sapply(df_2019_clean[vars_2], function(x) mean(is.na(x)) * 100)

  #Create a data frame
missing_df_2 <- data.frame(
  Variable = c("Revenue", "Profit Margin", "Employees", "Total Assets", "Firm Age"),
  Missing_Percentage = round(missing_table_2, 2))

  #Sort by missingness
missing_df_2 <- missing_df_2[order(-missing_df_2$Missing_Percentage), ]

  #create table in html 
missing_df_2 %>%
  kable("html", caption = "Percentage of Missing Values by Variable") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE)

####6. Save in R 
saveRDS (df_2019_clean,"df_2019_clean.rds")

########################IMPUTING 
  #Define the variables to use in the imputation
vars_to_use <- c("firm_ID", "year", "employees_IHS", "revenue_IHS", "total_assets_IHS",
                 "profit_margin", "firm_age_C", "company_size", "industry")

  #Subset the data
df_mice <- df_2019_clean[, vars_to_use]

  #Convert character variables to factors
df_mice <- df_mice %>%
  mutate(across(where(is.character), as.factor))

  #Define imputation methods
methods <- make.method(df_mice)

  #Assign custom imputation methods
methods["company_size"] <- "polyreg"   # categorical ordinal
methods["firm_ID"] <- ""               # do not impute
methods["year"] <- ""                  # do not impute
methods["industry"] <- ""              # do not impute
methods["profit_margin"] <- ""         # do not impute DV
methods["revenue_IHS"] <- ""           # do not impute DV
methods["firm_age_C"] <- ""            # do not impute
  # Others default to "pmm" (e.g., employees, assets)

  #Create a predictor matrix using quickpred
pred_matrix <- quickpred(df_mice,
                         mincor = 0.1,
                         exclude = c("firm_ID", "year", "industry", "profit_margin", "revenue_IHS", "firm_age_C"),
                         include = c("employees_IHS", "total_assets_IHS", "company_size"))

  #Run the imputation
set.seed(123)
imp <- mice(df_mice,
            m = 5,                  
            method = methods,
            predictorMatrix = pred_matrix,
            maxit = 10,
            printFlag = TRUE)

  #Save the imputed data object
saveRDS(imp, file = "imputed_data_conditional_did.rds")

imp <- readRDS("imputed_data_conditional_did.rds")

#assess imputation: 
#Function to compare observed vs imputed stats
compare_imputed_summary <- function(imp, vars) {
  df_imputed <- complete(imp, 1)  
  original <- imp$data
  
  results <- lapply(vars, function(v) {
    obs_vals <- original[[v]][!is.na(original[[v]])]
    imp_vals <- df_imputed[[v]]
    
    data.frame(
      Variable = v,
      Mean_Observed = mean(obs_vals, na.rm = TRUE),
      Mean_Imputed = mean(imp_vals, na.rm = TRUE),
      Mean_Diff_Perc = round(100 * (mean(imp_vals, na.rm = TRUE) - mean(obs_vals, na.rm = TRUE)) / mean(obs_vals, na.rm = TRUE), 2),
      
      Median_Observed = median(obs_vals, na.rm = TRUE),
      Median_Imputed = median(imp_vals, na.rm = TRUE),
      
      SD_Observed = sd(obs_vals, na.rm = TRUE),
      SD_Imputed = sd(imp_vals, na.rm = TRUE)
    )
  })
  
  do.call(rbind, results)
}

  #Variables to check — only numeric/imputed ones
vars_to_check <- c("total_assets_IHS", "employees_IHS", "firm_age_C")  

  #Run the comparison
imputation_summary <- compare_imputed_summary(imp, vars_to_check)

imputation_summary

  #Create table
imputation_summary %>%
  kable("html", caption = "Summary of Observed vs. Imputed Values", digits = 2, align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, position = "center") %>%
  add_header_above(c(" " = 1, "Mean" = 3, "Median" = 2, "SD" = 2))


    #check density plot
  #Extract long-format imputed dataset
df_imp_long <- complete(imp, action = "long", include = TRUE)

  #Filter only the imputed values and observed ones
df_imp_long <- df_imp_long %>%
  mutate(type = ifelse(.imp == 0, "Observed", "Imputed")) %>%
  filter(.imp == 0 | .imp > 0)  

  #Gather variables to long format
df_imp_plot <- df_imp_long %>%
  pivot_longer(cols = c("total_assets_IHS", "employees_IHS", "firm_age_C"),
               names_to = "variable", values_to = "value")

  #Plot with ggplot
plot_imp <- ggplot(df_imp_plot, aes(x = value, fill = type)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Density Plots: Observed vs Imputed Values",
       x = "Value", y = "Density") +
  scale_fill_manual(values = c("Observed" = "steelblue", "Imputed" = "darkred"))

plot_imp

#merge back with other variables; one complete dataset for exploration or modeling
df_imputed <- complete(imp, 1)

  #Merge imputed values back into the original full dataset
df_imputed <- df_2019_clean %>%
  select(-one_of(names(df_imputed))) %>%  
  bind_cols(df_imputed) 

#create two variables for firm size 
#Back-transform IHS variables and filter pre-GDPR
df_pre <- df_imputed %>%
  filter(year < 2018) %>%
  mutate(
    employees_raw = sinh(employees_IHS),
    assets_raw = sinh(total_assets_IHS))

#pre-GDPR employees and assets per firm
df_pre_firm <- df_pre %>%
  filter(!is.na(employees_raw) | !is.na(assets_raw)) %>%
  group_by(firm_ID) %>%
  summarise(
    employees_pre = mean(employees_raw, na.rm = TRUE),
    assets_pre = mean(assets_raw, na.rm = TRUE))

#Based on employees
df_pre_firm <- df_pre_firm %>%
  mutate(
    firm_size_employees = case_when(
      employees_pre <= 50 ~ "small",
      employees_pre <= 250 ~ "medium",
      employees_pre > 250 ~ "large",
      TRUE ~ NA_character_
    ),
    
    firm_size_assets = case_when(
      assets_pre <= 4840 ~ "small",
      assets_pre <= 19250 ~ "medium",
      assets_pre > 19250 ~ "large",
      TRUE ~ NA_character_
    )
  ) %>%
  select(firm_ID, firm_size_employees, firm_size_assets)

df_imputed <- left_join(df_imputed, df_pre_firm, by = "firm_ID")

#Save 
saveRDS(df_imputed, file = "df_imputed.rds")


df_imputed <- readRDS("df_imputed.rds")

table(df_imputed$industry)
table(df_imputed$treatment)
table(df_imputed$nace_main_2_digits)
