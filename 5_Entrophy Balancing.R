#entropy balancing
library(dplyr)
library(tidyr)
library(ggplot2)
library(did)
library(WeightIt)
library(knitr)
library(kableExtra)
library(patchwork)


#########load
df_profit_complete <- readRDS("df_profit_complete.rds")
df_revenue_complete <- readRDS("df_revenue_complete.rds")
df_imputed <- readRDS("df_imputed.rds")


# Set treatment group assignment timing
df_imputed$G <- ifelse(df_imputed$treatment == 1, 2018, 0)
df_profit_complete$G <- ifelse(df_profit_complete$treatment == 1, 2018, 0)
df_revenue_complete$G <- ifelse(df_revenue_complete$treatment == 1, 2018, 0)

###########entropy balancing 
#imputed data
###Filter to pre-treatment years only
df_pre_imp <- df_imputed %>% 
  filter(year < 2018)

#Estimate entropy balancing weights
ebal_imp <- weightit(treatment ~ total_assets_IHS + employees_IHS + firm_age_C,
                     data = df_pre_imp,
                     method = "ebal")  

#Add weights to full dataset
df_imputed$weights <- NA

#Apply only in the pre-treatment sample
df_imputed$weights[df_imputed$year < 2018] <- ebal_imp$weights

#post-treatment years, use same weights as last pre-period
df_imputed_entropy <- df_imputed %>%
  group_by(firm_ID) %>%
  mutate(weights = ifelse(is.na(weights), first(weights[!is.na(weights)]), weights)) %>%
  ungroup()

saveRDS(df_imputed_entropy, "df_imputed_entropy.rds")

########using Callaway & Sant'Anna
##check parallel trend assumption for imput
#revenue
event_revenue_imp_2_ent <- att_gt(
  yname = "revenue_IHS",           
  tname = "year",
  idname = "firm_ID",
  gname = "G",                 
  xformla = ~ firm_age_C + total_assets_IHS + employees_IHS,               
  data = df_imputed_entropy,
  weightsname = "weights",
  panel = FALSE)              

#event study
agg_event_revenue_imp_2_ent <- aggte(event_revenue_imp_2_ent, type = "dynamic")

summary(agg_event_revenue_imp_2_ent)

#Plot using ggdid
ggdid(agg_event_revenue_imp_2_ent)

#profits
event_profit_imp_2_ent <- att_gt(
  yname = "profit_margin",           
  tname = "year",
  idname = "firm_ID",
  gname = "G",                 
  xformla = ~ firm_age_C + total_assets_IHS + employees_IHS,           
  data = df_imputed_entropy,
  weightsname = "weights",
  panel = FALSE)              

# Aggregate to dynamic (event study) form
agg_event_profit_imp_2_ent <- aggte(event_profit_imp_2_ent, type = "dynamic")

summary(agg_event_profit_imp_2_ent)

event_profit_imp_2_ent

# Plot
ggdid(agg_event_profit_imp_2_ent)

###plot everything together
# Create ggplot objects using ggdid()
plot_revenue_imp_2_ent    <- ggdid(agg_event_revenue_imp_2_ent) + 
  ggtitle("Pre-Test: Revenue") + theme_minimal()

plot_profit_imp_2_ent   <- ggdid(agg_event_profit_imp_2_ent) + 
  ggtitle("Pre-Test: Profit-Margin") + theme_minimal()


# Combine them in a 2x2 panel layout
combined_plot_2_ent <- plot_revenue_imp_2_ent / plot_profit_imp_2_ent

combined_plot_2_ent

########################drop observations for 2017
#Drop the year 2017
df_imputed_entropy_1 <- df_imputed_entropy %>%
  filter(year != 2017)

#save
saveRDS(df_imputed_entropy_1, "df_imputed_entropy_1.rds")

#check again
#revenue
event_revenue_imp_3_ent <- att_gt(
  yname = "revenue_IHS",           
  tname = "year",
  idname = "firm_ID",
  gname = "G",                
  xformla = ~ firm_age_C + total_assets_IHS + employees_IHS,         
  data = df_imputed_entropy,
  weightsname = "weights",
  panel = FALSE)            

# Aggregate to dynamic (event study) form
agg_event_revenue_imp_3_ent <- aggte(event_revenue_imp_3_ent, type = "dynamic")

event_revenue_imp_3_ent

summary(agg_event_revenue_imp_3_ent)

#plot
ggdid(agg_event_revenue_imp_3_ent)

#profits
event_profit_imp_3_ent <- att_gt(
  yname = "profit_margin",           
  tname = "year",
  idname = "firm_ID",
  gname = "G",                 
  xformla = ~ firm_age_C + total_assets_IHS + employees_IHS,        
  data = df_imputed_entropy_1,
  weightsname = "weights",
  panel = FALSE)              

#Event study
agg_event_profit_imp_3_ent <- aggte(event_profit_imp_3_ent, type = "dynamic")

summary(agg_event_profit_imp_3_ent)

event_profit_imp_3_ent

#Plot
ggdid(agg_event_profit_imp_3_ent)


###plot everything together
plot_revenue_imp_3_ent    <- ggdid(agg_event_revenue_imp_3_ent) + 
  ggtitle("Pre-Test: Revenues") + theme_minimal()

plot_profit_imp_3_ent     <- ggdid(agg_event_profit_imp_3_ent) + 
  ggtitle("Pre-Test: Profit Margin") + theme_minimal()

#panel layout
combined_plot_3_ent <- (plot_revenue_imp_3_ent/ plot_profit_imp_3_ent)

combined_plot_3_ent

########################Balance diagnostic
#covariates 
covariates <- c("firm_age_C", "total_assets_IHS", "employees_IHS")

#function to calculate weighted mean
weighted_mean <- function(x, w) sum(x * w) / sum(w)

#unweighted mean
balance_unweighted <- df_imputed %>%
  group_by(treatment) %>%
  summarize(across(all_of(covariates), mean, na.rm = TRUE)) %>%
  mutate(Type = "Unweighted")

#entropy balanced
balance_weighted <- df_imputed_entropy %>%
  group_by(treatment) %>%
  summarize(across(
    all_of(covariates),
    ~ sum(.x * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE)
  )) %>%
  mutate(Type = "Weighted")

#Combine
balance_table <- bind_rows(balance_unweighted, balance_weighted) %>%
  arrange(Type, treatment)

balance_table %>%
  mutate(Type = ifelse(Type == "Unweighted", "Unweighted Mean", "Weighted Mean")) %>%
  arrange(Type, treatment) %>%
  rename(
    `Treatment Group` = treatment,
    `Firm Age (C)` = firm_age_C,
    `Total Assets (IHS)` = total_assets_IHS,
    `Employees (IHS)` = employees_IHS,
    `Type of Mean` = Type
  ) %>%
  kable(format = "html", digits = 2, caption = "Balance Diagnostics Before and After Entropy Balancing") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  add_header_above(c(" " = 1, "Covariate Means" = 3, " " = 1))



###################complete case
##check parallel trend assumption 
#profit
event_profit_cc_2_ent <- att_gt(
  yname = "profit_margin",           
  tname = "year",
  idname = "firm_ID",
  gname = "G",                 
  xformla = ~1,                
  data = df_profit_entropy,
  weightsname = "weights",
  panel = FALSE)              

#Event study
agg_event_profit_cc_2_ent <- aggte(event_profit_cc_2_ent, type = "dynamic")

summary(agg_event_profit_cc_2_ent)

#plot
ggdid(agg_event_profit_cc_2_ent)


#revenue
##check parallel trend assumption 
event_revenue_cc_2_ent <- att_gt(
  yname = "revenue_IHS",           # e.g., revenue_IHS or profit_margin
  tname = "year",
  idname = "firm_ID",
  gname = "G",                 # 2018 for treated, 0 for control
  xformla = ~1,                # no additional controls
  data = df_revenue_entropy,
  weightsname = "weights",
  panel = FALSE)              # or TRUE if firm panel is reliable


#event study 
agg_event_revenue_cc_2_ent <- aggte(event_revenue_cc_2_ent, type = "dynamic")

summary(agg_event_revenue_cc_2_ent)

#plot
ggdid(agg_event_revenue_cc_2_ent)


#############descriptive statistics
#pre-gdpr
df_imputed_entropy <- df_imputed_entropy %>%
  mutate(post_gdpr = ifelse(year >= 2018, 1, 0))

#Summary statistics
summary_pre_post <- df_imputed_entropy %>%
  select(post_gdpr, employees_IHS, revenue_IHS, total_assets_IHS, profit_margin, firm_age_C) %>%
  pivot_longer(cols = -post_gdpr, names_to = "Variable") %>%
  group_by(post_gdpr, Variable) %>%
  summarise(
    N = sum(!is.na(value)),
    Mean = mean(value, na.rm = TRUE),
    SD = sd(value, na.rm = TRUE),
    Min = min(value, na.rm = TRUE),
    Max = max(value, na.rm = TRUE),
    .groups = "drop")

#Variable names
summary_pre_post <- summary_pre_post %>%
  mutate(Variable = case_when(
    Variable == "employees_IHS" ~ "Number of Employees (IHS-transformed)",
    Variable == "revenue_IHS" ~ "Revenue (IHS-transformed)",
    Variable == "total_assets_IHS" ~ "Total Assets (IHS-transformed)",
    Variable == "profit_margin" ~ "Profit Margin (%)",
    Variable == "firm_age_C" ~ "Firm Age (Years)",
    TRUE ~ Variable
  ))

#publication-ready table
kable(
  summary_pre_post,
  digits = 2,
  caption = "Summary Statistics Before and After GDPR",
  col.names = c("Variable", "Period", "N", "Mean", "Standard Deviation", "Minimum", "Maximum"))


####with excluded period
#pre-gdpr
df_imputed_entropy_1 <- df_imputed_entropy_1 %>%
  mutate(post_gdpr = ifelse(year >= 2018, 1, 0))

#summary statistics
summary_pre_post_1 <- df_imputed_entropy_1 %>%
  select(post_gdpr, employees_IHS, revenue_IHS, total_assets_IHS, profit_margin, firm_age_C) %>%
  pivot_longer(cols = -post_gdpr, names_to = "Variable") %>%
  group_by(post_gdpr, Variable) %>%
  summarise(
    N = sum(!is.na(value)),
    Mean = mean(value, na.rm = TRUE),
    SD = sd(value, na.rm = TRUE),
    Min = min(value, na.rm = TRUE),
    Max = max(value, na.rm = TRUE),
    .groups = "drop")

#Variable names
summary_pre_post_1 <- summary_pre_post_1 %>%
  mutate(Variable = case_when(
    Variable == "employees_IHS" ~ "Number of Employees (IHS-transformed)",
    Variable == "revenue_IHS" ~ "Revenue (IHS-transformed)",
    Variable == "total_assets_IHS" ~ "Total Assets (IHS-transformed)",
    Variable == "profit_margin" ~ "Profit Margin (%)",
    Variable == "firm_age_C" ~ "Firm Age (Years)",
    TRUE ~ Variable))

#Publication-ready table
kable(
  summary_pre_post_1,
  digits = 2,
  caption = "Summary Statistics Before and After GDPR",
  col.names = c("Variable", "Period", "N", "Mean", "Standard Deviation", "Minimum", "Maximum"))

##########complete case
###Entropy balancing
##revenues
#profit
df_pre_revenue <- df_revenue_complete %>% 
  filter(year < 2018)

#Estimate entropy balancing weights
ebal_revenue <- weightit(treatment ~ total_assets_IHS + employees_IHS + firm_age_C,
                         data = df_pre_revenue,
                         method = "ebal")  # Entropy balancing

#Add weights to full dataset
df_revenue_complete$weights <- NA

#Apply the estimated weights only to units in the pre-treatment sample
df_revenue_complete$weights[df_revenue_complete$year < 2018] <- ebal_revenue$weights

#For post-treatment years, use same weights as last pre-period 
df_revenue_entropy <- df_revenue_complete %>%
  group_by(firm_ID) %>%
  mutate(weights = ifelse(is.na(weights), first(weights[!is.na(weights)]), weights)) %>%
  ungroup()

saveRDS(df_revenue_entropy, "df_revenue_entropy.rds")


###profit
df_pre_profit <- df_profit_complete %>% 
  filter(year < 2018)

#Estimate entropy balancing weights
ebal_profit <- weightit(treatment ~ total_assets_IHS + employees_IHS + firm_age_C,
                        data = df_pre_profit,
                        method = "ebal")  # Entropy balancing

#Add weights to full dataset
df_profit_complete$weights <- NA

#Apply the estimated weights only to units in the pre-treatment sample
df_profit_complete$weights[df_profit_complete$year < 2018] <- ebal_profit$weights

#For post-treatment years, use same weights as last pre-period (common approach)
df_profit_entropy <- df_profit_complete %>%
  group_by(firm_ID) %>%
  mutate(weights = ifelse(is.na(weights), first(weights[!is.na(weights)]), weights)) %>%
  ungroup()

saveRDS(df_profit_entropy, "df_profit_entropy.rds")
