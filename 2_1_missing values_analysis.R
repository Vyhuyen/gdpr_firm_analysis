##Analysing patterns of missing values
library(dplyr)
library(knitr)
library(kableExtra)
library(modelsummary)

#load dataset
df_total_long_2019 <- readRDS("df_total_long_2019.rds")

#Check percentage of missing values in each variable
colSums(is.na(df_total_long_2019)) / nrow(df_total_long_2019) * 100

##create plot
# Vector of variables to include
vars <- c("revenue_IHS", "profit_margin", "employees_IHS", "total_assets_IHS", "firm_age")

  #Calculate missingness
missing_table <- sapply(df_total_long_2019[vars], function(x) mean(is.na(x)) * 100)

  #Create a data frame
missing_df <- data.frame(
  Variable = c("Revenue", "Profit Margin", "Employees", "Total Assets", "Firm Age"),
  Missing_Percentage = round(missing_table, 2))

  #Sort by missingness
missing_df <- missing_df[order(-missing_df$Missing_Percentage), ]

  #create table in html 
missing_df %>%
  kable("html", caption = "Percentage of Missing Values by Variable") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE)


#Logistic Regression to diagnose missingness patterns
  #Profits
    #Create missingness indicator (1 = missing, 0 = observed)
df_total_long_2019$miss_profit <- as.integer(is.na(df_total_long_2019$profit_margin))

    #Logistic regression: does missingness depend on other variables?
model_na_profit <- glm(miss_profit ~ company_size + firm_age + employees_IHS + total_assets_IHS + industry + year ,
                   data = df_total_long_2019, family = binomial)

summary(model_na_profit)

  #Revenue
    #Create missingness indicator (1 = missing, 0 = observed)
df_total_long_2019$miss_revenue <- as.integer(is.na(df_total_long_2019$revenue_IHS))

    #Logistic regression: does missingness depend on other variables?
model_na_revenue <- glm(miss_revenue ~ company_size + firm_age + employees_IHS + total_assets_IHS + industry + year ,
                       data = df_total_long_2019, family = binomial)

summary(model_na_revenue)

  #Total assets 
    #Create missingness indicator (1 = missing, 0 = observed)
df_total_long_2019$miss_total_assets <- as.integer(is.na(df_total_long_2019$total_assets_IHS))

    #Logistic regression: does missingness depend on other variables?
model_na_total_assets <- glm(miss_total_assets ~ company_size + firm_age + employees_IHS + revenue_IHS +
                             profit_margin + industry + year ,
                        data = df_total_long_2019, family = binomial)

summary(model_na_total_assets)

  #Employees
    #Create missingness indicator (1 = missing, 0 = observed)
df_total_long_2019$miss_employees <- as.integer(is.na(df_total_long_2019$employees_IHS))

    #Logistic regression: does missingness depend on other variables?
model_na_employees <- glm(miss_employees ~ company_size + firm_age + revenue_IHS +
                               profit_margin + total_assets_IHS + industry + year ,
                             data = df_total_long_2019, family = binomial)

# Create a list of models
models <- list(
  "Profit Margins" = model_na_profit,
  "Revenues" = model_na_revenue,
  "Employees" = model_na_employees,
  "Total Assets" = model_na_total_assets)

#Clean covariate labels
labels <- c(
  "company_sizemedium" = "Medium Firm",
  "company_sizesmall" = "Small Firm",
  "firm_ageold" = "Old Firm",
  "firm_ageyoung" = "Young Firm",
  "employees_IHS" = "Employees (IHS)",
  "total_assets_IHS" = "Total Assets (IHS)",
  "revenue_IHS" = "Revenue (IHS)",
  "profit_margin" = "Profit Margin",
  "industry" = "Industry FE",
  "year" = "Year FE")

#Create table and export as HTML
modelsummary(models,
             coef_map = labels,
             stars = TRUE,
             output = "1_logisitc regression models_na.html",
             title = "Logistic Regression: Predictors of Missingness")



