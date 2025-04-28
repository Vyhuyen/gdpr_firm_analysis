#1_Data cleaning
library(dplyr)
library(stringr)
library(ggplot2)


##########
df_merged_long <- readRDS("df_merged_long.rds")

##data cleaning (similar to Frey&Presidente): drop outliers in the dependent variables and IHS-transformation
# profit

#Calculate 0.1 and 99.9 percentiles
lower_bound_profit <- quantile(df_merged_long$profit_margin, 0.1, na.rm = TRUE)
upper_bound_profit <- quantile(df_merged_long$profit_margin, 0.99, na.rm = TRUE)

#Identify firms with extreme profit margins in any year
firms_to_remove_profit <- df_merged_long %>%
  filter(profit_margin < lower_bound_profit | profit_margin > upper_bound_profit) %>%
  distinct(company_name)

#Drop these firms entirely
df_total_long <- df_merged_long %>%
  filter(!company_name %in% firms_to_remove_profit$company_name)

#check for outliers
summary(df_total_long$profit_margin)

##data cleaning (similar to Frey&Presidente): drop outliers in the dependent variables
#revenue
#outliers
summary(df_merged_long$revenue)

#Calculate 0.1 and 99.9 percentiles
lower_bound_revenue <- quantile(df_merged_long$revenue, 0.001, na.rm = TRUE)
upper_bound_revenue <- quantile(df_merged_long$revenue, 0.999, na.rm = TRUE)

#Identify firms with extreme profit margins in any year
firms_to_remove_revenue <- df_merged_long %>%
  filter(revenue < lower_bound_revenue | revenue > upper_bound_revenue) %>%
  distinct(company_name)  

#Drop these firms entirely
df_total_long <- df_total_long %>%
  filter(!company_name %in% firms_to_remove_revenue$company_name)

#check for outliers
summary(df_total_long$revenue)

#right skewed --> IHS-transformation

####total assets
summary(df_total_long$total_assets)

#Calculate 0.1 and 99.9 percentiles
lower_bound_total_assets <- quantile(df_merged_long$total_assets, 0.001, na.rm = TRUE)
upper_bound_total_assets <- quantile(df_merged_long$total_assets, 0.999, na.rm = TRUE)

#Identify firms with extreme profit margins in any year
firms_to_remove_total_assets <- df_merged_long %>%
  filter(total_assets < lower_bound_total_assets| total_assets > upper_bound_total_assets) %>%
  distinct(company_name)  

#Drop these firms entirely
df_total_long <- df_total_long %>%
  filter(!company_name %in% firms_to_remove_total_assets$company_name)

#check for outliers
summary(df_total_long$total_assets)

#right skwed --> IHS transformation

###employees
#visualize outliers
summary(df_merged_long$employees)

#Calculate 0.1 and 99.9 percentiles
lower_bound_employees <- quantile(df_merged_long$employees, 0.001, na.rm = TRUE)
upper_bound_employees <- quantile(df_merged_long$employees, 0.999, na.rm = TRUE)

#Identify firms with extreme profit margins in any year
firms_to_remove_employees <- df_merged_long %>%
  filter(employees < lower_bound_employees | employees > upper_bound_employees) %>%
  distinct(company_name)  

#Drop these firms entirely
df_total_long <- df_total_long %>%
  filter(!company_name %in% firms_to_remove_employees$company_name)

#check for outliers
summary(df_total_long$employees)

#right skwed --> IHS transformation

#IHS transformation
df_total_long <- df_total_long %>%
  mutate(
    revenue_IHS = asinh(revenue), 
    total_assets_IHS = asinh(total_assets), 
    employees_IHS = asinh(employees))


label(df_total_long$revenue_IHS) <- "Revenues (IHS-transformed)"
label(df_total_long$total_assets_IHS) <- "Total Assets (IHS-transformed)"
label(df_total_long$employees_IHS) <- "Employees (IHS-transformed)"

#add variable: firm age_C (continous) 
df_total_long$founding_date <- as.Date(df_total_long$founding_date)

df_total_long <- df_total_long %>%
  mutate(
    founding_year = str_extract(founding_date, "^\\d{4}"), 
    founding_year = as.Date(founding_date),
    firm_age_C = as.numeric(format(Sys.Date(), "%Y")) - as.numeric(format(founding_year, "%Y")))

df_total_long$firm_age_C  <-as.numeric(df_total_long$firm_age_C)
label(df_total_long$firm_age_C) <- "Firm age (cont.)"

#add variable: firms age (categorical)
summary(df_total_long$firm_age)

df_total_long <- df_total_long %>%
  mutate(firm_age = case_when(
    firm_age_C < 16  ~ "young",
    firm_age_C >= 16 & firm_age_C < 30  ~ "medium old",
    firm_age_C >= 30  ~ "old",
    TRUE ~ NA_character_))

df_total_long$firm_age  <-as.factor(df_total_long$firm_age )
label(df_total_long$firm_age) <- "Firm age (cat.)"

table(df_total_long$firm_age)


#add: NACE  Rev.2 two digit level
df_total_long <- df_total_long %>%
  mutate(nace_main_2_digits = str_extract(nace_main_activity, "^\\d{2}"))

#add industry level 
df_total_long <- df_total_long%>%
  mutate(industry = case_when(
          nace_main_2_digits %in% c("01", "02", "03") ~ "A-Agriculture",
          nace_main_2_digits %in% c("05", "06", "07", "08", "09") ~ "B-Mining and Quarrying",
          nace_main_2_digits %in% as.character(10:33) ~ "C-Manufacturing",
          nace_main_2_digits == "35" ~ "D-Electricity, Gas, Steam and Air Conditioning Supply",
          nace_main_2_digits %in% as.character(36:39) ~ "E-Water Supply; Sewerage, Waste Management and Remediation Activities",
          nace_main_2_digits %in% as.character(41:43) ~ "F-Construction",
          nace_main_2_digits %in% as.character(45:47) ~ "G-Wholesale and Retail Trade; Repair of Motor Vehicles and Motorcycles",
          nace_main_2_digits %in% as.character(49:53) ~ "H-Transportation and Storage",
          nace_main_2_digits %in% as.character(55:56) ~ "I-Accommodation and Food Service Activities",
          nace_main_2_digits %in% as.character(58:63) ~ "J-Information and Communication",
          nace_main_2_digits %in% as.character(64:66) ~ "K-Financial and Insurance Activities",
          nace_main_2_digits %in% as.character(68) ~ "L-Real Estate Activities",
          nace_main_2_digits %in% as.character(69:75) ~ "M-Professional, Scientific and Technical Activities",
          nace_main_2_digits %in% as.character(77:82) ~ "N-Administrative and Support Service Activities",
          nace_main_2_digits %in% as.character(84) ~ "O-Public Administration and Defence; Compulsory Social Security",
          nace_main_2_digits %in% as.character(85) ~ "P-Education",
          nace_main_2_digits %in% as.character(86:88) ~ "Q-Human Health and Social Work Activities",
          nace_main_2_digits %in% as.character(90:93) ~ "R-Arts, Entertainment and Recreation",
          nace_main_2_digits %in% as.character(94:96) ~ "S-Other Service Activities",
          nace_main_2_digits %in% as.character(97:98) ~ "T-Activities of Households as Employers",
          nace_main_2_digits %in% as.character(99) ~ "U-Activities of Extraterritorial Organizations",
           TRUE ~ "Unknown"  # Catch-all for undefined categories
         ))


#add unique firm_id because some firms have the same name
df_total_long <- df_total_long %>%
  mutate(firm_ID = as.integer
         (factor(paste(company_name, nace_main_2_digits, founding_date, sep = "_"))))

#translate company size values into english
df_total_long <- df_total_long %>%
  mutate(company_size = recode(company_size,
                               "klein" = "small",
                               "mittel" = "medium",
                               "groß" = "large"))

#define treatment group
df_total_long <- df_total_long %>%
  mutate(treatment = ifelse(nace_main_2_digits %in% c(58,59, 60, 61, 62, 63, 64, 65, 66), 1, 0))

#remove M because ambigous effect of GDPR 
df_total_long<- subset(df_total_long, industry != "M-Professional, Scientific and Technical Activities")


#6. Save in R 
saveRDS(df_total_long, "df_total_long.rds")

####data cleaning: 
df_total_long <- readRDS("df_total_long.rds")

#only keep firms with last balance date of 2019 or later
df_total_long_2019 <- df_total_long %>% 
  filter(last_balance_date >= 2019)

df_total_long_2019 <- df_total_long_2019 %>% 
  select(-r_d_costs)

df_total_long_2019 <- df_total_long_2019 %>% 
  select(-nace_main_secondary_activity, -profit_IHS, 
         -nace_main_activity, -founding_date, -export, -register_number, -founding_year)

n_distinct(df_total_long_2019$company_name)
n_distinct(df_total_long_2019$firm_ID)

#6. Save in R 
saveRDS(df_total_long_2019, "df_total_long_2019.rds")


##histogram to assess distribution
#Histogram of IHS-transformed total assets
hist_assets_2 <- ggplot(df_total_long, aes(x = total_assets_IHS)) +
  geom_histogram(fill = "darkblue", bins = 50, alpha = 0.7) +
  theme_minimal() +
  labs(title = "Total Assets (IHS Transformed)",
       x = "IHS(Total Assets)",
       y = "Frequency")

hist_employees_2 <- ggplot(df_total_long, aes(x = employees_IHS)) +
  geom_histogram(fill = "darkblue", bins = 50, alpha = 0.7) +
  theme_minimal() +
  labs(title = "Employees (IHS Transformed)",
       x = "Employees (IHS Transformed)",
       y = "Frequency")

hist_revenue_2 <- ggplot(df_total_long, aes(x = revenue_IHS)) +
  geom_histogram(fill = "darkblue", bins = 50, alpha = 0.7) +
  theme_minimal() +
  labs(title = "Revenue (IHS Transformed)",
       x = "Revenue (IHS Transformed)",
       y = "Frequency")

hist_profit_2 <- ggplot(df_total_long, aes(x = profit_IHS)) +
  geom_histogram(fill = "darkblue", bins = 50, alpha = 0.7) +
  theme_minimal() +
  labs(title = "Profit (IHS Transformed)",
       x = "Profit (IHS Transformed)",
       y = "Frequency")

