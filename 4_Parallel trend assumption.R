##Parallel trend assumption
library(dplyr)
library(ggplot2)
library(did)
library(patchwork)

#load
df_profit_complete <- readRDS("df_profit_complete.rds")
df_revenue_complete <- readRDS("df_revenue_complete.rds")
df_imputed <- readRDS("df_imputed.rds")

#parallel trend assumption: use did package 
#Set treatment group assignment timing
df_imputed$G <- ifelse(df_imputed$treatment == 1, 2018, 0)
df_profit_complete$G <- ifelse(df_profit_complete$treatment == 1, 2018, 0)
df_revenue_complete$G <- ifelse(df_revenue_complete$treatment == 1, 2018, 0)

#run event study
#imputed
#revenue
event_revenue_imp_1 <- att_gt(
  yname = "revenue_IHS",
  tname = "year",
  idname = "firm_ID",
  gname = "G",
  xformla = ~ firm_age_C + total_assets_IHS + employees_IHS,
  data = df_imputed,
  panel = FALSE)

#Aggregate to dynamic (event study) form
agg_event_revenue_imp_1 <- aggte(event_revenue_imp_1, type = "dynamic")

summary(agg_event_revenue_imp_1)

# Plot using ggdid
ggdid(agg_event_revenue_imp_1) +
      ylim(-0.2, 0.2) +
        labs(title = "Pre-Test: Revenues", y = "Treatment Effect", x = "Event Time") +
        theme_minimal()


#profit
#imputed 
event_profit_imp_1 <- att_gt(
  yname = "profit_margin",
  tname = "year",
  idname = "firm_ID",
  gname = "G",
  xformla = ~ firm_age_C + total_assets_IHS + employees_IHS,
  data = df_imputed,
  panel = FALSE)

# Aggregate to dynamic (event study) form
agg_event_profit_imp_1 <- aggte(event_profit_imp_1, type = "dynamic")

summary(agg_event_profit_imp_1)
event_profit_imp_1

agg_event_profit_imp_1

# Plot using ggdid
ggdid(agg_event_profit_imp_1 +
        labs(title = "Pre-Test: Profit Margins", y = "Treatment Effect", x = "Event Time") +
        theme_minimal())

###plot everything together
# Create ggplot objects using ggdid()
plot_revenue_imp_1    <- ggdid(agg_event_revenue_imp_1) + 
  ggtitle("Pre-Test: Revenues") + theme_minimal()

plot_profit_imp_1     <- ggdid(agg_event_profit_imp_1) + 
  ggtitle("Pre-Test: Profit Margin") + theme_minimal()


#Combine only imputed
combined_plot_1 <- (plot_revenue_imp_1 / plot_profit_imp_1)

combined_plot_1

###########complete case 
#revenues
event_revenue_cc_1 <- att_gt(
  yname = "revenue_IHS",
  tname = "year",
  idname = "firm_ID",
  gname = "G",
  xformla = ~ firm_age_C + total_assets_IHS + employees_IHS,
  data = df_revenue_complete,
  panel = FALSE)

#Aggregate to dynamic (event study) form
agg_event_revenue_cc_1 <- aggte(event_revenue_cc_1, type = "dynamic")

summary(agg_event_revenue_cc_1)

#Plot using ggdid
ggdid(agg_event_revenue_cc_1 + 
        ylim(-0.2, 0.2) +
        labs(title = "Pre-test: Revenue (Complete Case)", y = "Treatment Effect", x = "Event Time") +
        theme_minimal())

##profits
event_profit_cc_1 <- att_gt(
  yname = "profit_margin",
  tname = "year",
  idname = "firm_ID",
  gname = "G",
  xformla = ~ firm_age_C + total_assets_IHS + employees_IHS,
  data = df_profit_complete,
  panel = FALSE)

#Aggregate to dynamic (event study) form
agg_event_profit_cc_1 <- aggte(event_profit_cc_1, type = "dynamic")

summary(agg_event_profit_cc_1)

#Plot using ggdid
ggdid(agg_event_profit_cc_1 + 
        labs(title = "Profit – Imputed (C&S)", y = "Treatment Effect", x = "Event Time") +
        theme_minimal())





