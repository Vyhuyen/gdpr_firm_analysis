##Complete Case
library(dplyr)
library(ggplot2)
library(tidyr)
library(mice)
library(kableExtra)

#########checking for missing values for for dependent variable
df_total_long_2019 <- readRDS("df_total_long_2019.rds")

#####likewise deletion for dependent variables & covariates --> create two datasets
df_revenue_complete <- df_total_long_2019 %>% 
  filter (!is.na(revenue_IHS), 
          !is.na(total_assets_IHS), 
          !is.na(firm_age),           #will be the same for firm_age_C
          !is.na(employees_IHS))

n_distinct(df_revenue_complete$firm_ID)

df_profit_complete <- df_total_long_2019 %>% 
  filter (!is.na(profit_margin), 
          !is.na(total_assets_IHS), 
          !is.na(firm_age),           #will be the same for firm_age_C
          !is.na(employees_IHS))

n_distinct(df_profit_complete$firm_ID)
          
###
#classify variables for firm size
  #revenue
#Back-transform IHS variables and filter pre-GDPR
df_pre_revenue <- df_revenue_complete %>%
  filter(year < 2018) %>%
  mutate(
    employees_raw = sinh(employees_IHS),
    assets_raw = sinh(total_assets_IHS))

#pre-GDPR employees and assets per firm
df_pre_firm_revenue <- df_pre_revenue %>%
  filter(!is.na(employees_raw) | !is.na(assets_raw)) %>%
  group_by(firm_ID) %>%
  summarise(
    employees_pre = mean(employees_raw, na.rm = TRUE),
    assets_pre = mean(assets_raw, na.rm = TRUE))

#Classify separately
#Based on employees
df_pre_firm_revenue <- df_pre_firm_revenue %>%
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

df_revenue_complete <- left_join(df_revenue_complete, df_pre_firm_revenue, by = "firm_ID")

#Check percentage of missing values in each variable
colSums(is.na(df_revenue_complete)) / nrow(df_revenue_complete) * 100
 #67% missingness

 #profit
#Back-transform IHS variables and filter pre-GDPR
df_pre_profit <- df_profit_complete %>%
  filter(year < 2018) %>%
  mutate(
    employees_raw = sinh(employees_IHS),
    assets_raw = sinh(total_assets_IHS)  )

#pre-GDPR employees and assets per firm
df_pre_firm_profit <- df_pre_profit %>%
  filter(!is.na(employees_raw) | !is.na(assets_raw)) %>%
  group_by(firm_ID) %>%
  summarise(
    employees_pre = mean(employees_raw, na.rm = TRUE),
    assets_pre = mean(assets_raw, na.rm = TRUE)  )

#Classify separately
#Based on employees
df_pre_firm_profit <- df_pre_firm_profit %>%
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

df_profit_complete <- left_join(df_profit_complete, df_pre_firm_profit, by = "firm_ID")

#Check percentage of missing values in each variable
colSums(is.na(df_profit_complete)) / nrow(df_profit_complete) * 100
#20% missingness


saveRDS(df_profit_complete, "df_profit_complete.rds")

saveRDS(df_revenue_complete, "df_revenue_complete.rds")


df_revenue_complete <- readRDS("df_revenue_complete.rds")
df_profit_complete <- readRDS("df_profit_complete.rds")

table(df_revenue_complete$industry)

table(df_profit_complete$treatment)




