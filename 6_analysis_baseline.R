#analysis:difference in difference 
library(dplyr)
library(ggplot2)
library(did)


#load data 
df_imputed_entropy <- readRDS("df_imputed_entropy.rds")
df_imputed_entropy_1 <- readRDS("df_imputed_entropy_1.rds")
df_profit_entropy <- readRDS("df_profit_entropy.rds")
df_revenue_entropy <- readRDS("df_revenue_entropy.rds")

##create function with did package 
run_did_model <- function(data,
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
#revenue
#imputed
att_revenue_imp_2018 <- run_did_model(
  data = df_imputed_entropy,
  outcome = "revenue_IHS",
  weights_var = "weights",
  model_label = "Revenue – Imputed")

# To view the plot:
att_revenue_imp_2018$plot

att_revenue_imp_2018$event_study

############beautify
#plot
att_revenue_imp_2018$plot +
  labs(
    title = "Dynamic Treatment Effects of GDPR on Revenue",
    x = "Years Relative to GDPR Enforcement",
    y = "Average Treatment Effect on the Treated (ATT)"
  ) +
  theme(plot.title = element_text(face = "bold", size = 16))

##########
# Run for profit (imputed)
att_profit_imp_2018 <- run_did_model(
  data = df_imputed_entropy_1,
  outcome = "profit_margin",
  weights_var = "weights",
  gdpr_year = 2018,
  model_label = "Profit – Imputed")

# To view the plot:
att_profit_imp_2018$plot

att_profit_imp_2018$event_study


############beautify
#plot
att_profit_imp_2018$plot +
  labs(
    title = "Dynamic Treatment Effects of GDPR on Profit Margins",
    x = "Years Relative to GDPR Enforcement",
    y = "Average Treatment Effect on the Treated (ATT)"
  ) +
  theme(plot.title = element_text(face = "bold", size = 16))


##################
#complete case 
att_revenue_cc_2018 <- run_did_model(
  data = df_revenue_entropy,
  outcome = "revenue_IHS",
  weights_var = "weights",
  gdpr_year = 2018, 
  model_label = "Revenue – Complete Case")

# To view the plot:
att_revenue_cc_2018$plot

att_revenue_cc_2018$event_study

att_revenue_cc_2018

############beautify
#plot
att_revenue_cc_2018$plot +
  labs(
    title = "Dynamic Treatment Effects of GDPR on Revenue",
    x = "Years Relative to GDPR Enforcement",
    y = "Average Treatment Effect on the Treated (ATT)", 
    subtitle = "With complete cases observation"
  ) +
  theme(plot.title = element_text(face = "bold", size = 16))

#profit
att_profit_cc_2018 <- run_did_model(
  data = df_profit_entropy,
  outcome = "profit_margin",
  weights_var = "weights",
  gdpr_year = 2018, 
  model_label = "Profit – Complete Case") 

# To view the plot:
att_profit_cc_2018$plot

att_profit_cc_2018$event_study

############beautify
#plot
att_profit_cc_2018$plot +
  labs(
    title = "Dynamic Treatment Effects of GDPR on Profit Margins",
    x = "Years Relative to GDPR Enforcement",
    y = "Average Treatment Effect on the Treated (ATT)", 
    subtitle = "With complete cases observation"
  ) +
  theme(plot.title = element_text(face = "bold", size = 16))


############robustness: placebo with different years########################
#########2017
#revenue
att_revenue_imp_2017 <- run_did_model(
  data = df_imputed_entropy,
  outcome = "revenue_IHS",
  weights_var = "weights",
  gdpr_year = 2017) 

# To view the plot:
att_revenue_imp_2017$plot +
  labs(
    title = "Dynamic Treatment Effects of GDPR on Revenues",
    x = "Years Relative to GDPR Enforcement",
    y = "Average Treatment Effect on the Treated (ATT)", 
    subtitle = "Placebo test with 2017",
  ) +
  theme(plot.title = element_text(face = "bold", size = 16))

att_revenue_imp_2017$event_study

#profit
att_profit_imp_2017 <- run_did_model(
  data = df_imputed_entropy,
  outcome = "profit_margin",
  weights_var = "weights",
  gdpr_year = 2017)  

# To view the plot:
att_profit_imp_2017$plot +
  labs(
    title = "Dynamic Treatment Effects of GDPR on Profit Margin",
    x = "Years Relative to GDPR Enforcement",
    y = "Average Treatment Effect on the Treated (ATT)", 
    subtitle = "Placebo test with 2017",
  ) +
  theme(plot.title = element_text(face = "bold", size = 16))

att_profit_imp_2017$event_study


#2016
#revenue
att_revenue_imp_2016 <- run_did_model(
  data = df_imputed_entropy,
  outcome = "revenue_IHS",
  weights_var = "weights",
  gdpr_year = 2016)  

# To view the plot:
att_revenue_imp_2016$plot +
  labs(
    title = "Dynamic Treatment Effects of GDPR on Revenues",
    x = "Years Relative to GDPR Enforcement",
    y = "Average Treatment Effect on the Treated (ATT)", 
    subtitle = "Placebo test with 2016",
  ) +
  theme(plot.title = element_text(face = "bold", size = 16))

# To view the summary data:
att_revenue_imp_2016$event_study

#profit
att_profit_imp_2016 <- run_did_model(
  data = df_imputed_entropy,
  outcome = "profit_margin",
  weights_var = "weights",
  gdpr_year = 2016) 

#To view the plot:
att_profit_imp_2016$plot +
  labs(
    title = "Dynamic Treatment Effects of GDPR on Profit Margin",
    x = "Years Relative to GDPR Enforcement",
    y = "Average Treatment Effect on the Treated (ATT)", 
    subtitle = "Placebo test with 2016",
  ) +
  theme(plot.title = element_text(face = "bold", size = 16))

att_profit_imp_2016$event_study



