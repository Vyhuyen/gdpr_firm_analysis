#analysis: size
library(dplyr)
library(tidyr)
library(ggplot2)
library(did)
library(WeightIt)
library(knitr)

#load data 
df_imputed_entropy <- readRDS("df_imputed_entropy.rds")
df_profit_entropy <- readRDS("df_profit_entropy.rds")
df_revenue_entropy <- readRDS("df_revenue_entropy.rds")


####subset based on firm size: df_imputed 
#total assets
df_small_assets <- df_imputed_entropy %>% 
  filter(firm_size_assets == "small")

df_medium_assets <- df_imputed_entropy %>% 
  filter(firm_size_assets == "medium")

df_large_assets <- df_imputed_entropy %>% 
  filter(firm_size_assets == "large")

#employees
df_small_employees <- df_imputed_entropy %>% 
  filter(firm_size_employees == "small")

df_medium_employees <- df_imputed_entropy %>% 
  filter(firm_size_employees == "medium")

df_large_employees <- df_imputed_entropy %>% 
  filter(firm_size_employees == "large")

###function to perform balancing on subsample
perform_entropy_balancing <- function(df, covariates, treat_var = "treatment", year_var = "year", cutoff_year = 2018) {
  
  df_pre <- df %>% filter(.data[[year_var]] < cutoff_year)
  
  formula <- as.formula(paste(treat_var, "~", paste(covariates, collapse = " + ")))
  ebal_model <- weightit(
    formula,
    data = df_pre,
    method = "ebal"
  )
  
  df$weights <- NA
  df$weights[df[[year_var]] < cutoff_year] <- ebal_model$weights
  
  df <- df %>%
    group_by(firm_ID) %>%
    mutate(weights = ifelse(is.na(weights), first(weights[!is.na(weights)]), weights)) %>%
    ungroup()
  
  return(df)
}

######apply function to subsample
covariates <- c("total_assets_IHS", "employees_IHS", "firm_age_C")

df_small_assets <- perform_entropy_balancing(df_small_assets, covariates)
df_medium_assets <- perform_entropy_balancing(df_medium_assets, covariates)
df_large_assets <- perform_entropy_balancing(df_large_assets, covariates)

df_small_employees <- perform_entropy_balancing(df_small_employees, covariates)
df_medium_employees <- perform_entropy_balancing(df_medium_employees, covariates)
df_large_employees <- perform_entropy_balancing(df_large_employees, covariates)
#######


##create function with did package 
run_did_model_size <- function(data,
                          outcome,
                          id_var = "firm_ID",
                          time_var = "year",
                          treat_indicator = "treatment",
                          gdpr_year = 2018,
                          weights_var = NULL,
                          panel = FALSE,
                          model_label = "Model") {
  
  data$G <- ifelse(data[[treat_indicator]] == 1, gdpr_year, 0)
  
  att_model <- att_gt(
    yname = outcome,
    tname = time_var,
    idname = id_var,
    gname = "G",
    xformla = ~ firm_age_C + total_assets_IHS + employees_IHS,
    data = data,
    panel = panel,
    weightsname = weights_var
  )
  
  event_model <- aggte(att_model, type = "dynamic")
  
  plot <- ggdid(event_model) +
    labs(
      title = paste("Event Study for", outcome),
      x = "Years Relative to Treatment (GDPR)",
      y = "ATT"
    ) +
    theme_minimal()
  
  if (!is.null(event_model$att.egt) && length(event_model$att.egt) > 0) {
    event_df <- data.frame(
      Event_Time = event_model$egt,
      Estimate = event_model$att.egt,
      Std_Error = event_model$se.egt,
      CI_Lower = event_model$att.egt - 1.96 * event_model$se.egt,
      CI_Upper = event_model$att.egt + 1.96 * event_model$se.egt,
      Model = model_label
    )
  } else {
    warning(paste("Model", model_label, "did not return dynamic effect estimates."))
    event_df <- NULL
  }
  
  return(list(
    att_gt = att_model,
    event_study = event_model,
    plot = plot,
    summary_df = event_df
  ))
}

##################run models
####Large firms
##Revenues 
#total assets
att_revenue_asset_large <- run_did_model_size(
  data = df_large_assets,
  outcome = "revenue_IHS",
  weights_var = "weights",
  gdpr_year = 2018,
  model_label = "Revenue: Large Firms (size based on Total Assets)")

#plot:
att_revenue_asset_large$plot +
  labs(
    title = "Revenues (size based on Total Assets)",
    x = "Years Relative to GDPR Enforcement",
    y = "Average Treatment Effect on the Treated (ATT)"
  ) +
  theme(plot.title = element_text(face = "bold", size = 16))


att_revenue_asset_large$event_study
att_revenue_asset_large

###employees
att_revenue_employees_large <- run_did_model_size(
  data = df_large_employees,
  outcome = "revenue_IHS",
  weights_var = "weights",
  gdpr_year = 2018,
  model_label = "Revenue: Large Firms (based on employees)")

#plot:
att_revenue_employees_large$plot+
  labs(
    title = "Revenues (size based on Employees)",
    x = "Years Relative to GDPR Enforcement",
    y = "Average Treatment Effect on the Treated (ATT)"
  ) +
  theme(plot.title = element_text(face = "bold", size = 16))

att_revenue_employees_large$event_study
att_revenue_employees_large



##profit 
#total assets
att_profit_asset_large <- run_did_model_size(
  data = df_large_assets,
  outcome = "profit_margin",
  weights_var = "weights",
  gdpr_year = 2018,
  model_label = "Profit: Large Firms (based on Total Assets)")

att_profit_asset_large

###-3 & -4 violate parallel trends assumptions --> exclude 
df_large_assets_1 <- df_large_assets %>%
  filter(year !=2014 & year !=2015)

#run again with excluded years
# Run for profit (imputed)
att_profit_asset_large <- run_did_model_size(
  data = df_large_assets_1,
  outcome = "profit_margin",
  weights_var = "weights",
  gdpr_year = 2018,
  model_label = "Profit: Large Firms (based on Total Assets)")

#plot:
att_profit_asset_large$plot +
  labs(
    title = "Profit Margins (size based on Total Assets)",
    x = "Years Relative to GDPR Enforcement",
    y = "Average Treatment Effect on the Treated (ATT)"
  ) +
  theme(plot.title = element_text(face = "bold", size = 16))

att_profit_asset_large


##employees
att_profit_employees_large <- run_did_model_size(
  data = df_large_employees,
  outcome = "profit_margin",
  weights_var = "weights",
  gdpr_year = 2018,
  model_label = "Profit: Large Firms (based on employees)")

#plot:
att_profit_employees_large$plot

att_profit_employees_large$event_study

###-1 & -3 violate parallel trends assumptions --> exclude 
df_large_employees_1 <- df_large_employees %>%
  filter(year !=2017 & year !=2015 & year !=2016)

#run again with excluded years
# Run for profit (imputed)
att_profit_employees_large <- run_did_model_size(
  data = df_large_employees_1,
  outcome = "profit_margin",
  weights_var = "weights",
  gdpr_year = 2018,
  model_label = "Profit: Large Firms (based on employees)")

#plot:
att_profit_employees_large$plot +
  labs(
    title = "Profit Margins (size based on Employees)",
    x = "Years Relative to GDPR Enforcement",
    y = "Average Treatment Effect on the Treated (ATT)"
  ) +
  theme(plot.title = element_text(face = "bold", size = 16))

att_profit_employees_large


#######medium sized firms
###revenue
##total assets
att_revenue_asset_medium <- run_did_model_size(
  data = df_medium_assets,
  outcome = "revenue_IHS",
  weights_var = "weights",
  gdpr_year = 2018,
  model_label = "Revenue: Medium-sized Firms (based on Total Assets)")

#plot
att_revenue_asset_medium$plot +
  labs(
    title = " Revenues (size based on Total Assets)",
    x = "Years Relative to GDPR Enforcement",
    y = "Average Treatment Effect on the Treated (ATT)"
  ) +
  theme(plot.title = element_text(face = "bold", size = 16))

att_revenue_asset_medium$event_study


##employees
att_revenue_employees_medium <- run_did_model_size(
  data = df_medium_employees,
  outcome = "revenue_IHS",
  weights_var = "weights",
  gdpr_year = 2018,
  model_label = "Revenue: Medium-sized Firms (based on employees)")

#plot:
att_revenue_employees_medium$plot +
  labs(
    title = " Revenues (size based on employees)",
    x = "Years Relative to GDPR Enforcement",
    y = "Average Treatment Effect on the Treated (ATT)"
  ) +
  theme(plot.title = element_text(face = "bold", size = 16))

att_revenue_employees_medium$event_study


###profit 
#total assets
att_profit_asset_medium <- run_did_model_size(
  data = df_medium_assets,
  outcome = "profit_margin",
  weights_var = "weights",
  gdpr_year = 2018,
  model_label = "Profit: Medium-sized Firms (based on Total Assets)")

#plot:
att_profit_asset_medium$plot +
  labs(
    title = " Profit Margins (size based on total assets)",
    x = "Years Relative to GDPR Enforcement",
    y = "Average Treatment Effect on the Treated (ATT)"
  ) +
  theme(plot.title = element_text(face = "bold", size = 16))

att_profit_asset_medium$event_study


##employees
att_profit_employees_medium <- run_did_model_size(
  data = df_medium_employees,
  outcome = "profit_margin",
  weights_var = "weights",
  gdpr_year = 2018,
  model_label = "Profit: Medium-sized Firms (based on employees)")

#plot
att_profit_employees_medium$plot +
  labs(
    title = " Profit Margins (size based on employees)",
    x = "Years Relative to GDPR Enforcement",
    y = "Average Treatment Effect on the Treated (ATT)"
  ) +
  theme(plot.title = element_text(face = "bold", size = 16))

att_profit_employees_medium$event_study


#####small 
##revenues
#total assets
att_revenue_asset_small <- run_did_model_size(
  data = df_small_assets,
  outcome = "revenue_IHS",
  weights_var = "weights",
  gdpr_year = 2018,
  model_label = "Revenue: Small Firms (based on Total Assets)")

#plot
att_revenue_asset_small$plot +
  labs(
    title = " Revenues (size based on total assets)",
    x = "Years Relative to GDPR Enforcement",
    y = "Average Treatment Effect on the Treated (ATT)"
  ) +
  theme(plot.title = element_text(face = "bold", size = 16))

att_revenue_asset_small$event_study


##employees
att_revenue_employees_small <- run_did_model_size(
  data = df_small_employees,
  outcome = "revenue_IHS",
  weights_var = "weights",
  gdpr_year = 2018,
  model_label = "Revenue: Small Firms (based on employees)")

#plot
att_revenue_employees_small$plot+
  labs(
    title = " Revenues (size based on employees)",
    x = "Years Relative to GDPR Enforcement",
    y = "Average Treatment Effect on the Treated (ATT)"
  ) +
  theme(plot.title = element_text(face = "bold", size = 16))

att_revenue_employees_small$event_study


##profit
#total assets
att_profit_asset_small <- run_did_model_size(
  data = df_small_assets,
  outcome = "profit_margin",
  weights_var = "weights",
  gdpr_year = 2018,
  model_label = "Profit: Small Firms (based on Total Assets)")

att_profit_asset_small$summary_df


###-1 violate parallel trends assumptions --> exclude 
df_small_assets_1 <- df_small_assets %>%
  filter(year !=2017)

#run again with excluded years
att_profit_asset_small <- run_did_model_size(
  data = df_small_assets_1,
  outcome = "profit_margin",
  weights_var = "weights",
  gdpr_year = 2018,
  model_label = "Profit: Small Firms (based on Total Assets)")

#plot
att_profit_asset_small$plot +
  labs(
    title = "Profit Margins (size based on total assets)",
    x = "Years Relative to GDPR Enforcement",
    y = "Average Treatment Effect on the Treated (ATT)"
  ) +
  theme(plot.title = element_text(face = "bold", size = 16))

att_profit_asset_small$event_study


##employees
att_profit_employees_small <- run_did_model_size(
  data = df_small_employees,
  outcome = "profit_margin",
  weights_var = "weights",
  gdpr_year = 2018,
  model_label = "Profit: Small Firms (based on employees)")

att_profit_employees_small$summary_df

###-1 violate parallel trends assumptions --> exclude 
df_small_employees_1 <- df_small_employees %>%
  filter(year !=2017)

#run again with excluded years
att_profit_employees_small <- run_did_model_size(
  data = df_small_employees_1,
  outcome = "profit_margin",
  weights_var = "weights",
  gdpr_year = 2018,
  model_label = "Profit: Small Firms (based on employees)")

#plot
att_profit_employees_small$plot+
  labs(
    title = " Profit Margins (size based on employees)",
    x = "Years Relative to GDPR Enforcement",
    y = "Average Treatment Effect on the Treated (ATT)"
  ) +
  theme(plot.title = element_text(face = "bold", size = 16))

att_profit_employees_small$event_study


##########describtive summary
#function
summarize_dataset <- function(data, dataset_name) {
  data %>%
    select(employees_IHS, revenue_IHS, total_assets_IHS, profit_margin, firm_age_C) %>%
    pivot_longer(cols = everything(), names_to = "Variable") %>%
    group_by(Variable) %>%
    summarise(
      Dataset = dataset_name,
      N = sum(!is.na(value)),
      Mean = mean(value, na.rm = TRUE),
      SD = sd(value, na.rm = TRUE),
      Min = min(value, na.rm = TRUE),
      Max = max(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Variable = case_when(
      Variable == "employees_IHS" ~ "Number of Employees (IHS-transformed)",
      Variable == "revenue_IHS" ~ "Revenue (IHS-transformed)",
      Variable == "total_assets_IHS" ~ "Total Assets (IHS-transformed)",
      Variable == "profit_margin" ~ "Profit Margin (%)",
      Variable == "firm_age_C" ~ "Firm Age (Years)",
      TRUE ~ Variable
    ))
}

#apply to datasets
summary_small_assets <- summarize_dataset(df_small_assets, "Small (Assets)")
summary_medium_assets <- summarize_dataset(df_medium_assets, "Medium (Assets)")
summary_large_assets <- summarize_dataset(df_large_assets, "Large (Assets)")

summary_small_employees <- summarize_dataset(df_small_employees, "Small (Employees)")
summary_medium_employees <- summarize_dataset(df_medium_employees, "Medium (Employees)")
summary_large_employees <- summarize_dataset(df_large_employees, "Large (Employees)")

#combine
summary_all <- bind_rows(
  summary_small_assets,
  summary_medium_assets,
  summary_large_assets,
  summary_small_employees,
  summary_medium_employees,
  summary_large_employees
)

#table
kable(summary_all, digits = 2, caption = "Summary Statistics Across Firm Size Groups (Selected Variables)")
