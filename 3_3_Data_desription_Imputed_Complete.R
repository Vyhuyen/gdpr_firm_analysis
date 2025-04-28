##Missing values
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(purrr)
library(readr)
library(tidyr)
library(naniar)
library(fixest) 
library(forcats)
library(scales)

#########load
df_profit_complete <- readRDS("df_profit_complete.rds")
df_revenue_complete <- readRDS("df_revenue_complete.rds")
df_imputed <- readRDS("df_imputed.rds")

##Summary statistics 
summary_stats_long <- function(df, label) {
  tibble(
    dataset = label,
    
    `Profit Margin` = list(c(mean(df$profit_margin, na.rm = TRUE), 
                             sd(df$profit_margin, na.rm = TRUE), 
                             sum(!is.na(df$profit_margin)))),
    
    `Revenue (IHS)` = list(c(mean(df$revenue_IHS, na.rm = TRUE), 
                             sd(df$revenue_IHS, na.rm = TRUE), 
                             sum(!is.na(df$revenue_IHS)))),
    
    `Employees (IHS)` = list(c(mean(df$employees_IHS, na.rm = TRUE), 
                               sd(df$employees_IHS, na.rm = TRUE), 
                               sum(!is.na(df$employees_IHS)))),
    
    `Total Assets (IHS)` = list(c(mean(df$total_assets_IHS, na.rm = TRUE), 
                                  sd(df$total_assets_IHS, na.rm = TRUE), 
                                  sum(!is.na(df$total_assets_IHS)))),
    
    `Firm Age` = list(c(mean(df$firm_age_C, na.rm = TRUE), 
                        sd(df$firm_age_C, na.rm = TRUE), 
                        sum(!is.na(df$firm_age_C))))
  ) %>%
    pivot_longer(-dataset, names_to = "Variable", values_to = "Stats") %>%
    mutate(
      Mean = map_dbl(Stats, 1),
      SD   = map_dbl(Stats, 2),
      N    = map_dbl(Stats, 3)
    ) %>%
    select(dataset, Variable, Mean, SD, N)
}

summary_profit <- summary_stats_long(df_profit_complete, "Complete Case: Profit")
summary_revenue <- summary_stats_long(df_revenue_complete, "Complete Case: Revenue")
summary_imputed <- summary_stats_long(df_imputed, "Imputed")

summary_combined_long <- bind_rows(summary_profit, summary_revenue, summary_imputed)

summary_combined_long %>%
  kable(format = "html", digits = 2, caption = "Summary Statistics: Complete-Case and Imputed Datasets") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, font_size = 12)


###density plots
  #Add a dataset label
df_profit_complete$dataset <- "Complete Case: Profit"
df_revenue_complete$dataset <- "Complete Case: Revenue"
df_imputed$dataset <- "Imputed"

#Combine the datasets
df_combined <- bind_rows(df_profit_complete, df_revenue_complete, df_imputed)

#Select and pivot the variables for density plot
df_long_sum_stats <- df_combined %>%
  select(dataset, profit_margin, revenue_IHS, employees_IHS, total_assets_IHS) %>%
  pivot_longer(cols = -dataset, names_to = "variable", values_to = "value")

#Plot all densities using facet_wrap
ggplot(df_long_sum_stats, aes(x = value, fill = dataset, color = dataset)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ variable, scales = "free", ncol = 2) +
  theme_minimal() +
  labs(
    title = "Density Plots: Comparing datasets for imputed values and complete cases",
    x = "Value",
    y = "Density"
  ) +
  scale_fill_manual(values = c("Complete Case: Profit" = "blue",
                               "Complete Case: Revenue" = "green",
                               "Imputed" = "red")) +
  scale_color_manual(values = c("Complete Case: Profit" = "blue",
                                "Complete Case: Revenue" = "green",
                                "Imputed" = "red"))


#####compare size
#total assets
# Define a function to summarize firm size counts per dataset
get_size_assets_distribution <- function(df, dataset_name) {
  df %>%
    count(firm_size_assets) %>%
    mutate(dataset = dataset_name,
           proportion = n / sum(n))
}

# Apply function to all datasets
size_imputed_assets <- get_size_assets_distribution(df_imputed, "Imputed")
size_profit_assets <- get_size_assets_distribution(df_profit_complete, "Profit Complete")
size_revenue_assets <- get_size_assets_distribution(df_revenue_complete, "Revenue Complete")

# Combine into one data frame
size_comparison_assets <- bind_rows(size_imputed_assets, size_profit_assets, size_revenue_assets)

#Plot proportions
plot_assets <- ggplot(size_comparison_assets, aes(x = firm_size_assets, y = proportion, fill = dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Relative Firm Size Distribution Across Datasets",
       x = "Firm Size (based on total assets)",
       y = "Proportion") +
  theme_minimal()


#employees
# Define a function to summarize firm size counts per dataset
get_size_employees_distribution <- function(df, dataset_name) {
  df %>%
    count(firm_size_employees) %>%
    mutate(dataset = dataset_name,
           proportion = n / sum(n))}

# Apply function to all datasets
size_imputed_employees <- get_size_employees_distribution(df_imputed, "Imputed")
size_profit_employees <- get_size_employees_distribution(df_profit_complete, "Profit Complete")
size_revenue_employees <- get_size_employees_distribution(df_revenue_complete, "Revenue Complete")

# Combine into one data frame
size_comparison_employees <- bind_rows(size_imputed_employees, size_profit_employees, size_revenue_employees)


#Plot proportions
plot_employees<- ggplot(size_comparison_employees, aes(x = firm_size_employees, y = proportion, fill = dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Relative Firm Size Distribution Across Datasets",
       x = "Firm Size (based on employees)",
       y = "Proportion") +
  theme_minimal()

#plot together
# Display them side-by-side
combined_plot <- plot_assets + plot_employees +
  plot_layout(ncol = 2)  # two columns (side by side)

# Show the plot
combined_plot

######industries 
industry_lookup <- c(
  "F" = "Construction",
  "J" = "Information & Communication",
  "C" = "Manufacturing",
  "G" = "Wholesale & Retail",
  "K" = "Finance & Insurance",
  "I" = "Accommodation & Food Services",
  "N" = "Admin & Support Services",
  "L" = "Real Estate",
  "E" = "Utilities & Waste",
  "A" = "Agriculture"
)

# Add a short code column (assuming your original industry names have prefixes like "F-Construction")
industry_counts <- industry_counts %>%
  mutate(short_code = substr(industry, 1, 1),
         industry_short = industry_lookup[short_code])

# Plot with short codes as axis labels
ggplot(industry_counts, aes(x = reorder(industry_short, firm_count), y = firm_count)) +
  geom_bar(stat = "identity", fill = "#4682B4", width = 0.7) +
  geom_text(aes(label = scales::comma(firm_count)), 
            hjust = -0.1, size = 3.5) + 
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Top 10 Industries by Firm Count",
       x = "Industry (Short Code)",
       y = "Number of Firms") +
  theme_minimal(base_size = 13)



