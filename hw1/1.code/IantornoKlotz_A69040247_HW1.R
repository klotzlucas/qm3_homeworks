################################################################################
#
# GPCO 446 - Quantitative Methods III - Spring 2026
# Homework I
# Author: Lucas Iantorno Klotz
# last modified: -
# 
################################################################################

# In this script, I provide the data analysis for homework 1.

# I usually have a master file where I load the packages and define a few functions.
# I'm going to add them here.

# Dear TA, the plots' titles are displayed on the writeup. I commented the lines
# that set the title to the plot.

# You will also note that the variables' names are a bit distinct from the original
# raw data. That's because I use janitor::clean_name() which standardize the names.

# |||||||||||||||||||||
# 0. Preamble --------
# |||||||||||||||||||||


rm(list = ls())
gc()

options(scipen = 20)                                                            # Avoid scientific notation when looking at data.frames

pack <- c('tidyverse', 'stargazer', 'readxl', 'patchwork', 'knitr',
          'ggeffects', 'interactions', 'modelsummary', 'margins',
          'lmtest', 'estimatr', 'car')

pacman::p_load(pack, install = F, character.only = T)                           # loading packages



th <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  # theme for ggplot figures. This is the theme I usually work with.
            plot.title =  element_text(size = 16, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 12, hjust = 0.5),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            axis.title =  element_text(size=16), legend.position="bottom", legend.title = element_blank(),
            legend.text = element_text(size=16),
            legend.key.size = unit(1.5, "cm"),
            axis.text =   element_text(size=12),
            #axis.text.x = element_text(angle = 45, hjust = 1),
            plot.caption = element_text(hjust = 0, size=11),
            strip.text = element_text(size = 15),
            plot.caption.position =  "plot")


#setwd("/Users/liklotz/Library/CloudStorage/OneDrive-Pessoal/UCSD/winter_quarter_2026/QM_II/homeworks/final_project") # <- this should be changed to your directory and commented out!
#("~/Desktop/QM2 R Materials/Week 9") # <- Update this to your directory and comment it out after setting it!

# ||||||||||||||||||||||||||
# 1. Load Datasets ------
# ||||||||||||||||||||||||||

# Load the datasets into R
aid_data <- read_excel("raw_data/chinese-aid-data-2000-2021.xlsx")|>
  janitor::clean_names()|> # better variable names
  filter(status %in% c("Pipeline: Commitment", "Completion", "Implementation"))|> # let's consider the projects that actually received aid or was backed by an official commitment
  filter(is.na(covid))|>                                                        # let's disregard COVID related aid 
  filter(year %in% c(2013:2021))|> # our period of interest
  select(entity, year, sector_code, adjusted_amount_constant_usd_2021)


our_world_data <- read_excel("raw_data/our-world-in-data-2013-2023.xlsx")|>
  filter(year %in% c(2013:2018))|> # our period of interest
  select(1,2,4,6:9)


# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# 2. Transforming Chinese Aid into a Cross-Country Dataset ----
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

summary_stats <- aid_data |>
  filter(year %in% c(2019:2021))|>
  group_by(entity)|>
  summarise(across(c(adjusted_amount_constant_usd_2021), list(
    N    = ~sum(!is.na(.)),
    mean = ~mean(., na.rm=TRUE),
    sd   = ~sd(., na.rm=TRUE),
    min  = ~min(., na.rm=TRUE),
    p25  = ~quantile(., .25, na.rm=TRUE),
    med  = ~median(., na.rm=TRUE),
    p75  = ~quantile(., .75, na.rm=TRUE),
    max  = ~max(., na.rm=TRUE),
    n_NA = ~sum(is.na(.))
  )))


# The goal here is to compute:
# 1. **Total aid received by each country (aid_all_sector)** – Median over 2019-2021.
# 2. Convert all aid values to **million USD** for readability.
# 3. **Round all values to two decimal places** for readability.


# Identify relevant columns
aid_amount_col <- "adjusted_amount_constant_usd_2021"


# Filter data for years 2019-2021
aid_data_filtered <- aid_data %>%
  filter(year >= 2019 & year <= 2021)

# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
## 2.1 Compute Total Aid per Country (Median Over 2019-2021) -----
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

# Aid amounts across entity-year (total aid received per country-year)
aid_total <- aid_data_filtered %>%
  group_by(entity) %>%
  summarise(med_aid_all_sector = round(median(!!sym(aid_amount_col), na.rm = TRUE) / 1e6, 2),  # Convert to million USD & round
            avg_aid_all_sector = round(mean(!!sym(aid_amount_col), na.rm = TRUE) / 1e6, 2),  # Convert to million USD & round
            .groups = "drop")

# I believe the best choice here is to go with the median level of aid per country.
# The number of projects vary substantially and so does their aid values.

# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# SECTOR ANALYSIS NOT DONE DUE TO HIGH MISSING DATA
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||


# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
### 2.1.1 Compute Sector-Specific Aid per Country (Median Over 2019-2021) -----
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

# Average aid amounts across entity-year-sector
#aid_by_sector <- aid_data_filtered %>%
# group_by(entity, !!sym(sector_col)) %>%
#summarise(sector_aid = round(median(!!sym(aid_amount_col), na.rm = TRUE) / 1e6, 2),  # Convert to million USD & round
# .groups = "drop")

# Reshape data to have separate columns for each sector
#aid_med_by_sector_pivot <- aid_by_sector %>%
# pivot_wider(names_from = !!sym(sector_col), values_from = sector_aid, names_prefix = "aid_")

# |||||||||||||||||||||||||||||||||||||||||||||
### 2.1.2 Merge Total and Sector-Specific Aid Data ------
# |||||||||||||||||||||||||||||||||||||||||||||

# Merge total aid and sector-wise aid into a single dataset
#aid_final <- aid_total %>%
# left_join(aid_med_by_sector_pivot, by = "entity")

# |||||||||||||||||||||||||||||||||||||
### 2.1.3 Handling Missing Sectoral Aid Values -------
# |||||||||||||||||||||||||||||||||||||

# TA's Assumption: If a country has NA in a sector, it means no aid was received in that sector for 2019-2021.
# We replace NA with 0 to reflect this assumption.

# aid_final[is.na(aid_final)] <- 0

# Note: I don't think this is reasonable. There are cases where the project's 
# status is said to be completed and the adjusted amount variable is NA. Hence, that project
# in a particular sector did receive money but for some reason the adjusted amount
# does not show the value. We can't, thus, simply input 0 as if a particular sector
# did not receive aid.

# I think that given this issue I might not even do sector analysis.

# |||||||||||||||||||||||||||||||||||||||||
### 2.1.4 Collapse Dataset to Cross-Country Format -----
# |||||||||||||||||||||||||||||||||||||||||

# Ensure only relevant variables remain for the final dataset
#cross_country_aid <- aid_total|>
# select(1,2,4:27)


# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||


# Optionally, save the final dataset
write.csv(aid_total, "2.output/data/cross_country_aid_2019_2021.csv", row.names = FALSE)


# ||||||||||||||||||||||||||||||||||||||||||||||||
## 2.2 Processing Baseline Chinese Aid Data (2013-2018) ------
# ||||||||||||||||||||||||||||||||||||||||||||||||

# Repeat the process like in section 2.1

# The goal here is to compute:
# 1. **Baseline total aid received by each country (aid_all_sector_baseline)** – Median over 2013-2018.
# 2. Convert all aid values to **million USD** for readability.
# 3. Replace **NA values in sectoral aid with 0**, assuming no aid was given in that sector.

# Filter data for years 2013-2018
aid_data_baseline <- aid_data %>%
  filter(year >= 2013 & year <= 2018)

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
### 2.2.1 Compute Baseline Total Aid per Country (Averaged Over 2013-2018) ------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

# Median aid amounts across entity-year (total aid received per country-year)
aid_total_baseline <- aid_data_baseline %>%
  group_by(entity) %>%
  summarise(aid_all_sector_baseline = round(median(!!sym(aid_amount_col), na.rm = TRUE) / 1e6, 2),  # Convert to million USD & round
            .groups = "drop")

# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# SECTOR ANALYSIS NOT DONE DUE TO HIGH MISSING DATA
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
### 2.2.2 Compute Baseline Sector-Specific Aid per Country (Averaged Over 2013-2018) -----
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

# Average aid amounts across entity-year-sector
#aid_by_sector_baseline <- aid_data_baseline %>%
# group_by(entity, !!sym(sector_col)) %>%
#summarise(sector_aid_baseline = round(median(!!sym(aid_amount_col), na.rm = TRUE) / 1e6, 2),  # Convert to million USD & round
#         .groups = "drop")

# Reshape data to have separate columns for each sector
#aid_med_by_sector_baseline_pivot <- aid_by_sector_baseline %>%
# pivot_wider(names_from = !!sym(sector_col), values_from = sector_aid_baseline, names_prefix = "aid_baseline_")

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
### 2.2.3 Merge Baseline Total and Sector-Specific Aid Data -------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

# Merge total baseline aid and sector-wise baseline aid into a single dataset
#aid_baseline_final <- aid_total_baseline %>%
#left_join(aid_med_by_sector_baseline_pivot, by = "entity")

# ||||||||||||||||||||||||||||||||||||||
# Handling Missing Sectoral Aid Values
# ||||||||||||||||||||||||||||||||||||||

# Assumption: If a country has NA in a sector, it means no aid was received in that sector for 2013-2018.
# We replace NA with 0 to reflect this assumption.
#aid_baseline_final[is.na(aid_baseline_final)] <- 0

# Not gonna do that for the reasons mentioned above.

# |||||||||||||||||||||||||||||||||||||||||
### 2.2.4 Collapse Dataset to Cross-Country Format -----
# |||||||||||||||||||||||||||||||||||||||||

# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||


# save the final dataset
write.csv(aid_total_baseline, "2.output/data/cross_country_aid_baseline_2013_2018.csv", row.names = FALSE)

# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# 3. Transforming OWD into a cross-country dataset -------
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||

# The goal here is to create a **cross-country dataset** by aggregating 
# country-year observations into country-level indicators.
# We will compute ** average values for key political and economic variables** 
# (e.g., inequality, governance, democracy) **up to 2018**, ensuring that 
# these predictors temporally precede the dependent variable (Chinese aid from 2019 onward).

# We'll use the averages here because overall these variables haven't change much between
# 13 and 18 in each country.


# Select relevant variables and filter data up to 2018
variables_to_average <- c(
  "ti-corruption-perception-index",  # Corruption perception index
  "share-of-population-in-extreme-poverty",  # Poverty rate
  "economic-inequality-gini-index",  # Income inequality (Gini index)
  "gdp-per-capita-worldbank",  # GDP per capita
  "democracy-index-eiu"  # Democracy index
)

# Convert variables to numeric properly, replacing "." with NA
our_world_data <- our_world_data %>%
  mutate(across(all_of(variables_to_average), ~ as.numeric(na_if(as.character(.), "."))))

# Compute the average of selected variables for each country, ensuring NA if all values are missing
averaged_our_world_data <- our_world_data %>%
  group_by(entity) %>%
  summarise(across(all_of(variables_to_average), 
                   ~ ifelse(all(is.na(.)), NA, mean(., na.rm = TRUE)), 
                   .names = "{.col}_avg"), 
            .groups = "drop")

# Optionally, save the final dataset
write.csv(averaged_our_world_data, "2.output/data/averaged_our_world_data.csv", row.names = FALSE)


# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# 4. Merging All Datasets into a Single Cross-Country Dataset -------
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

# Load the datasets
averaged_our_world_data <- read.csv("2.output/data/averaged_our_world_data.csv")
cross_country_aid_2019_2021 <- read.csv("2.output/data/cross_country_aid_2019_2021.csv")
cross_country_aid_baseline_2013_2018 <- read.csv("2.output/data/cross_country_aid_baseline_2013_2018.csv")

# ||||||||||||||||||||||||||||||||||||||||||||||||||||
## 4.1 Merge the datasets by 'entity' (country name) ------
# ||||||||||||||||||||||||||||||||||||||||||||||||||||

cross_country_final <- averaged_our_world_data %>%
  left_join(cross_country_aid_baseline_2013_2018, by = "entity") %>%
  left_join(cross_country_aid_2019_2021, by = "entity")

# ||||||||||||||||||||||||||||||||||
### 4.1.1 View and Save Final Merged Dataset ----
# ||||||||||||||||||||||||||||||||||

# Print dataset preview
print(cross_country_final)

# Save merged dataset to CSV
write.csv(cross_country_final, "2.output/data/cross_country_final.csv", row.names = FALSE)

# ||||||||||||||||||||||||||||||||||||||||||||||
# 5. Visualize Corruption and Aid patterns ------
# ||||||||||||||||||||||||||||||||||||||||||||||

cross_country_final<-read.csv("2.output/data/cross_country_final.csv")

# Let's take a look at the variables of interest distribution and relationships.


# ||||||||||||||||||||||||||||||||||||||||||||||
## 5.1 Histogram Corruption (Index) -----
# |||||||||||||||||||||||||||||||||||||||||||||||

histo_corruption <- ggplot(cross_country_final, aes(x = ti.corruption.perception.index_avg)) +
  geom_histogram(
    aes(y = after_stat(count/sum(count)) * 100), # after_stat() replaces ..variable..
    bins = 20,                           
    fill = "#2e67be", 
    alpha = 0.5, 
    color = "black"
  ) +
  labs(# title = TITLE IS DISPLAYED ON THE WRITEUP ,
    x = "Perceived Corruption Index (Avg. 2013-2018)",
    y = "Frequency (%)") +
  th

print(histo_corruption)

ggsave("2.output/figures/histogram_corruption.png", plot = histo_corruption, width = 12, height = 10, dpi = 300)

# ||||||||||||||||||||||||||||||||||||||||||||||
## 5.2 Histogram Chinese Aida -----
# |||||||||||||||||||||||||||||||||||||||||||||||

histo_aid <- ggplot(cross_country_final, aes(x = med_aid_all_sector)) +
  geom_histogram(
    aes(y = after_stat(count/sum(count)) * 100), # after_stat() replaces ..variable..
    bins = 20,                           
    fill = "#2A9D8F", 
    alpha = 0.5, 
    color = "black"
  ) +
  labs(# title = TITLE IS DISPLAYED ON THE WRITEUP ,
    x = "Chinese Aid (Millions USD, Median 2019-2021)",
    y = "Frequency (%)") +
  th

print(histo_aid)

ggsave("2.output/figures/histogram_aid.png", plot = histo_aid, width = 12, height = 10, dpi = 300)


# Clearly right-skiwed. We'll need to use log().

# ||||||||||||||||||||||||||||||||||||||||||||||
## 5.3 Histogram GDP per capita -----
# |||||||||||||||||||||||||||||||||||||||||||||||

histo_gdp <- ggplot(cross_country_final, aes(x = gdp.per.capita.worldbank_avg)) +
  geom_histogram(
    aes(y = after_stat(count/sum(count)) * 100), # after_stat() replaces ..variable..
    bins = 20,                           
    fill = "#E58724", 
    alpha = 0.5, 
    color = "black"
  ) +
  labs(# title = TITLE IS DISPLAYED ON THE WRITEUP ,
    x = "GDP per capita (USD, Avg. 2013-2018)",
    y = "Frequency (%)") +
  th

print(histo_gdp)

ggsave("2.output/figures/histogram_gdp.png", plot = histo_gdp, width = 12, height = 10, dpi = 300)


# right-skiwed. We'll need to use log().

# ||||||||||||||||||||||||||||||||||||||||||||||
## 5.4 Scatter Plot of Aid and Corruption (Index) -----
# |||||||||||||||||||||||||||||||||||||||||||||||

# Create scatter plot
scatter_aid_corruption<-ggplot(cross_country_final, aes(x = ti.corruption.perception.index_avg, y = log1p(med_aid_all_sector))) +
  geom_point(color = "#2e67be", alpha = 0.6) +  # Scatter points
  geom_smooth(method = "lm", se = TRUE, color = "#E58724", linetype = "dashed") +  # Regression line
  labs(
    #title = DISPLAYED ON THE MEMO
    x = "Perceived Corruption Index (Avg. 2013-2018)",
    y = "Log of Chinese Aid (Median 2019-2021)"
  ) +
  th

print(scatter_aid_corruption)

ggsave("2.output/figures/scatter_aid_corruption.png", plot = scatter_aid_corruption, width = 12, height = 10, dpi = 300)

# There's somewhat a weak negative relationship. That is, more clean, less aid.



# |||||||||||||||||||||||||||
# 6. Regression Analysis ----
# |||||||||||||||||||||||||||

cross_country_final<-read.csv("2.output/data/cross_country_final.csv") |>
  mutate(log_aid_all_sector = log1p(med_aid_all_sector),                        # log(1 + x) transformation
         log_gdp_per_capita = log1p(gdp.per.capita.worldbank_avg),
         log_aid_baseline = log1p(aid_all_sector_baseline))

# Useful correlations

cor(cross_country_final$log_gdp_per_capita, cross_country_final$ti.corruption.perception.index_avg, use = "complete.obs")
cor(cross_country_final$log_gdp_per_capita, cross_country_final$share.of.population.in.extreme.poverty_avg, use = "complete.obs")
cor(cross_country_final$log_gdp_per_capita, cross_country_final$democracy.index.eiu_avg, use = "complete.obs")

## Model 1: Bivariate regression of log_aid_all_sector on corription index ----
model_1 <- lm_robust(log_aid_all_sector ~ ti.corruption.perception.index_avg, 
                     data = cross_country_final, se_type = "HC1")

# Testing for heteroskedasticity
bptest(model_1)


## Model 2: Add Gini index ----
model_2 <- lm_robust(log_aid_all_sector ~ ti.corruption.perception.index_avg + log_gdp_per_capita,
                     data = cross_country_final, se_type = "HC1")
# Testing for heteroskedasticity
bptest(model_2)



## Model 3: Add share of population in poverty -----
model_3 <- lm_robust(log_aid_all_sector ~ ti.corruption.perception.index_avg + 
                       log_gdp_per_capita + log_aid_baseline, 
                     data = cross_country_final, se_type = "HC1")
# Testing for heteroskedasticity
bptest(model_3)



## Model 4: Add democracy index ----
model_4 <- lm_robust(log_aid_all_sector ~ ti.corruption.perception.index_avg + 
                       log_gdp_per_capita + log_aid_baseline +
                       share.of.population.in.extreme.poverty_avg, 
                     data = cross_country_final, se_type = "HC1")

# Testing for heteroskedasticity
bptest(model_4)


# Model 5: Add GDP per capita
model_5 <- lm_robust(log_aid_all_sector ~ ti.corruption.perception.index_avg + 
                       log_gdp_per_capita + log_aid_baseline +
                       share.of.population.in.extreme.poverty_avg +
                       economic.inequality.gini.index_avg,
                     data = cross_country_final, se_type = "HC1")
# Testing for heteroskedasticity
bptest(model_5)



# Model 6: Add baseline aid (2013-2018)
model_6 <- lm_robust(log_aid_all_sector ~ ti.corruption.perception.index_avg + 
                       log_gdp_per_capita + log_aid_baseline +
                       share.of.population.in.extreme.poverty_avg +
                       economic.inequality.gini.index_avg +
                       democracy.index.eiu_avg, 
                     data = cross_country_final, se_type = "HC1")

# Testing for heteroskedasticity
bptest(model_6)



# Save Regression Results Using modelsummary 


# Define the list of models
models <- list(
  "(1)" = model_1,
  "(2)" = model_2,
  "(3)" = model_3,
  "(4)" = model_4,
  "(5)" = model_5,
  "(6)" = model_6
)

modelsummary(
  models = models,
  out = "html",
  stars = T
)


# Save as LaTeX with robust standard errors
modelsummary(models, 
             stars = c("*" = 0.1,
                       "**" = 0.05,
                       "***" = 0.01),
             coef_rename = c("ti.corruption.perception.index_avg" = "Corruption Index",
                             "log_gdp_per_capita" = "Log of Avg. GDP per capita (13-18)",
                             "log_aid_baseline" = "Log of Median Aid Baseline (13-18)",
                             "share.of.population.in.extreme.poverty_avg" = "Share Extreme Poverty",
                             "economic.inequality.gini.index_avg" = "Gini Index",
                             "democracy.index.eiu_avg" = "Democracy Index"),
             
             gof_omit = "AIC|BIC|Log|Sigma",
             statistic = "std.error",  # Displays robust SEs
             output = "2.output/tables/regression_results_log_aid.tex" )



# ||||||||||||||||||||||||||||||||||
# 7. Regression Diagnostics -----
# ||||||||||||||||||||||||||||||||||


## 7.1 Balance tests ----

# There's a 30% reduction in the number of obs from the baseline model to
# the final one. We investigate whether the dropped countries are significantly
# different from the group that has all variables.

# Define the variables
vars <- c("ti.corruption.perception.index_avg", 
          "economic.inequality.gini.index_avg", 
          "share.of.population.in.extreme.poverty_avg",
          "democracy.index.eiu_avg", 
          "log_gdp_per_capita", 
          "log_aid_baseline")

# Nice labels for each variable
var_labels <- c(
  "ti.corruption.perception.index_avg" = "Corruption Index",
  "economic.inequality.gini.index_avg" = "Gini Index",
  "share.of.population.in.extreme.poverty_avg" = "Extreme Poverty (%)",
  "democracy.index.eiu_avg" = "Democracy Index",
  "log_gdp_per_capita" = "Log GDP per Capita",
  "log_aid_baseline" = "Log Aid (Baseline)"
)

# Create in-sample indicator based on completeness of all vars
model_data <- cross_country_final |>
  filter(!is.na(log_aid_all_sector) & !is.na(ti.corruption.perception.index_avg))|>
  mutate(in_sample = complete.cases(across(all_of(vars))))

# Build one plot per variable
plots <- map(vars, function(var) { # for each value of the vars vector the function(var) will be run
  plot_data <- model_data |>
    filter(!is.na(.data[[var]])) |>
    group_by(in_sample) |>
    summarise(
      mean_val = mean(.data[[var]], na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) |>
    mutate(group = ifelse(in_sample, "In Sample\n(N = 80)", paste0("Excluded\n(N = ", n[in_sample == FALSE], ")"))) 
  
  # In sample means the countries that have all the variables (no NAs).
  # There's a group of 36 countries that have at least one NA across the controls.
  # Across countrols, however, there's heterogeneity in which country is left
  # out.
  
  # T-test to test difference in means
  in_vals  <- model_data %>% filter(in_sample == TRUE)  %>% pull(.data[[var]])
  out_vals <- model_data %>% filter(in_sample == FALSE) %>% pull(.data[[var]])
  p_val <- tryCatch(t.test(in_vals, out_vals)$p.value, error = function(e) NA)
  p_label <- ifelse(is.na(p_val), "p = NA", paste0("p = ", round(p_val, 3)))
  
  ggplot(plot_data, aes(x = group, y = mean_val, fill = in_sample)) +
    geom_col(width = 0.5) +
    annotate("text", x = 1.5, y = max(plot_data$mean_val) * 1.05,
             label = p_label, size = 3.5) +
    scale_fill_manual(values = c("TRUE" = "#2c7bb6", "FALSE" = "#d7191c"),
                      guide = "none") +
    labs(title = var_labels[var], x = NULL, y = "Mean") +
    th
})

# Combine into grid
fig<-wrap_plots(plots, ncol = 3)
print(fig)

ggsave("2.output/figures/balance_test.png", plot = fig, width = 12, height = 10, dpi = 300)



## 7.2 OVB ----

# We'll need to re-run the models with separated datasets to calculate the residuals

fit_and_plot <- function(data, formula, title) {
  
  # Get the variables from the formula
  vars_needed <- all.vars(as.formula(formula))
  
  # Filter to complete cases for those variables
  data_clean <- data |> filter(complete.cases(across(all_of(vars_needed))))
  
  # Fit model
  model <- lm_robust(as.formula(formula), data = data_clean, se_type = "HC1")
  
  # Calculate residuals
  data_clean <- data_clean |>
    mutate(residuals_model = .data[[vars_needed[1]]] - model$fitted.values)
  
  # Plot
  p <- ggplot(data_clean, aes(x = ti.corruption.perception.index_avg, y = residuals_model)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = title, x = "Perceived Corruption Index (Avg. 2013 - 2018)", y = "Residuals") +
    th
  
  list(model = model, data = data_clean, plot = p)
}


r1 <- fit_and_plot(cross_country_final, "log_aid_all_sector ~ ti.corruption.perception.index_avg", "Baseline Model")
r2 <- fit_and_plot(cross_country_final, "log_aid_all_sector ~ ti.corruption.perception.index_avg + log_gdp_per_capita", "Adding Log of GDP per capita") 
r3 <- fit_and_plot(cross_country_final, "log_aid_all_sector ~ log_gdp_per_capita + log_aid_baseline", "Adding Baseline Log of Aid") 
r4 <- fit_and_plot(cross_country_final, "log_aid_all_sector ~ ti.corruption.perception.index_avg + log_gdp_per_capita + log_aid_baseline + share.of.population.in.extreme.poverty_avg", "Adding % Extreme Poverty")
r5 <- fit_and_plot(cross_country_final, "log_aid_all_sector ~ ti.corruption.perception.index_avg + log_gdp_per_capita + log_aid_baseline + share.of.population.in.extreme.poverty_avg + economic.inequality.gini.index_avg", "Adding Gini Index")
r6 <- fit_and_plot(cross_country_final, "log_aid_all_sector ~ ti.corruption.perception.index_avg + log_gdp_per_capita + log_aid_baseline + share.of.population.in.extreme.poverty_avg + economic.inequality.gini.index_avg + democracy.index.eiu_avg", "Adding Democracy Index")

combined_plot <- r1$plot / r2$plot / r3$plot / r4$plot / r5$plot / r6$plot

print(combined_plot)

ggsave(
  filename = paste0("2.output/figures/residual_plots_.png"),
  plot = combined_plot,
  width = 10,
  height = 12
)

## 7.3 Residuals ----

# Refit with lm() for diagnostics (same formula, same data)
m1_lm <- lm(log_aid_all_sector ~ ti.corruption.perception.index_avg, 
            data = cross_country_final |> filter(!is.na(log_aid_all_sector) & !is.na(ti.corruption.perception.index_avg)))

m2_lm <- lm(log_aid_all_sector ~ ti.corruption.perception.index_avg + log_gdp_per_capita,
            data = cross_country_final |> filter(!is.na(log_aid_all_sector) & !is.na(ti.corruption.perception.index_avg) & !is.na(log_gdp_per_capita)))

m3_lm <- lm(log_aid_all_sector ~ ti.corruption.perception.index_avg + log_gdp_per_capita + log_aid_baseline,
            data = cross_country_final |> filter(!is.na(log_aid_all_sector) & !is.na(ti.corruption.perception.index_avg) & !is.na(log_gdp_per_capita) & !is.na(log_aid_baseline)))

m4_lm <- lm(log_aid_all_sector ~ ti.corruption.perception.index_avg + log_gdp_per_capita + log_aid_baseline +
              share.of.population.in.extreme.poverty_avg,
            data = cross_country_final |> filter(!is.na(log_aid_all_sector) & !is.na(ti.corruption.perception.index_avg) & !is.na(log_gdp_per_capita) & !is.na(log_aid_baseline) & !is.na(share.of.population.in.extreme.poverty_avg)))

m5_lm <- lm(log_aid_all_sector ~ ti.corruption.perception.index_avg + log_gdp_per_capita + log_aid_baseline +
              share.of.population.in.extreme.poverty_avg + economic.inequality.gini.index_avg,
            data = cross_country_final |> filter(!is.na(log_aid_all_sector) & !is.na(ti.corruption.perception.index_avg) & !is.na(log_gdp_per_capita) & !is.na(log_aid_baseline) & !is.na(share.of.population.in.extreme.poverty_avg) & !is.na(economic.inequality.gini.index_avg)))

m6_lm <- lm(log_aid_all_sector ~ ti.corruption.perception.index_avg + log_gdp_per_capita + log_aid_baseline +
              share.of.population.in.extreme.poverty_avg + economic.inequality.gini.index_avg + democracy.index.eiu_avg,
            data = cross_country_final |> filter(complete.cases(across(all_of(c("log_aid_all_sector", "ti.corruption.perception.index_avg", "log_gdp_per_capita", "log_aid_baseline", "share.of.population.in.extreme.poverty_avg", "economic.inequality.gini.index_avg", "democracy.index.eiu_avg"))))))

# Bind all diagnostics together
diagnostics_data <- bind_rows(
  tibble(model = "Model 1", fitted = m1_lm$fitted.values, residuals = m1_lm$residuals),
  tibble(model = "Model 2", fitted = m2_lm$fitted.values, residuals = m2_lm$residuals),
  tibble(model = "Model 3", fitted = m3_lm$fitted.values, residuals = m3_lm$residuals),
  tibble(model = "Model 4", fitted = m4_lm$fitted.values, residuals = m4_lm$residuals),
  tibble(model = "Model 5", fitted = m5_lm$fitted.values, residuals = m5_lm$residuals),
  tibble(model = "Model 6", fitted = m6_lm$fitted.values, residuals = m6_lm$residuals)
) %>%
  mutate(model = factor(model, levels = paste("Model", 1:6)))

# Plot
residuals_plot <- ggplot(diagnostics_data, aes(x = fitted, y = residuals)) +
  geom_point(color = "darkgray", size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(se = FALSE, color = "blue", linewidth = 0.5) +
  facet_wrap(~ model, ncol = 3) +
  labs(x = "Fitted Values", y = "Residuals") +
  th

print(residuals_plot)

ggsave("2.output/figures/residuals_vs_fitted_all.png", plot = residuals_plot, width = 12, height = 8)

png("2.output/figures/added_variable_plots.png", width = 800, height = 600)
avPlots(m4_lm)                                                                  # analyzing each variables influence on the outcome
dev.off()


## 7.4 Normality of the errors
qq_data <- bind_rows(
  tibble(model = "Model 1", residuals = m1_lm$residuals),
  tibble(model = "Model 2", residuals = m2_lm$residuals),
  tibble(model = "Model 3", residuals = m3_lm$residuals),
  tibble(model = "Model 4", residuals = m4_lm$residuals),
  tibble(model = "Model 5", residuals = m5_lm$residuals),
  tibble(model = "Model 6", residuals = m6_lm$residuals)
) %>%
  mutate(model = factor(model, levels = paste("Model", 1:6)))

qq_plot <- ggplot(qq_data, aes(sample = residuals)) +
  stat_qq(color = "darkgray", size = 1.5) +
  stat_qq_line(color = "red", linetype = "dashed") +
  facet_wrap(~ model, ncol = 3) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  th

print(qq_plot)

ggsave("2.output/figures/qq_plots_all.png", plot = qq_plot, width = 12, height = 8)

## 7.5 Multicolinearity ------

# Model 2
m2_lm <- lm(log_aid_all_sector ~ ti.corruption.perception.index_avg + log_gdp_per_capita,
            data = cross_country_final) # vif() does not run with lm_robust

vif(m2_lm) # no colinearity

# Model 3

m3_lm <- lm(log_aid_all_sector ~ ti.corruption.perception.index_avg + 
              log_gdp_per_capita + log_aid_baseline, 
            data = cross_country_final) # vif() does not run with lm_robust

vif(m3_lm) # no colinearity

# Model 4

m4_lm <- lm(log_aid_all_sector ~ ti.corruption.perception.index_avg + 
              log_gdp_per_capita + log_aid_baseline +
              share.of.population.in.extreme.poverty_avg, 
            data = cross_country_final) # vif() does not run with lm_robust

vif(m4_lm) 

# Model 5
m5_lm <- lm(log_aid_all_sector ~ ti.corruption.perception.index_avg + 
              log_gdp_per_capita + log_aid_baseline +
              share.of.population.in.extreme.poverty_avg +
              economic.inequality.gini.index_avg, 
            data = cross_country_final) # vif() does not run with lm_robust

vif(m5_lm) # moderate colinearity

# Model 6

m6_lm <- lm(log_aid_all_sector ~ ti.corruption.perception.index_avg + 
              log_gdp_per_capita + log_aid_baseline +
              share.of.population.in.extreme.poverty_avg +
              economic.inequality.gini.index_avg +
              democracy.index.eiu_avg, 
            data = cross_country_final) # vif() does not run with lm_robust

vif(m6_lm) # moderate colinearity



## 7.6 Outliers ----

# Re-run model 4

data_model_4 <- cross_country_final |>
  filter(!is.na(log_aid_all_sector) & !is.na(ti.corruption.perception.index_avg) & !is.na(log_gdp_per_capita) & !is.na(log_aid_baseline) & !is.na(share.of.population.in.extreme.poverty_avg))


# Calculate outlier diagnostics

### 1.1 Studentized Residuals -----
data_model_4$studentized_residuals <- rstudent(m4_lm)

# Identify potential outliers based on the rule: abs(residual) > 2
outliers_residuals <- data_model_4 %>%
  filter(abs(studentized_residuals) > 2)

cat("\nObservations with Studentized Residuals > 2:\n")
print(outliers_residuals[, c("entity",  "studentized_residuals")])

### 1.2 Leverage ------

data_model_4$leverage <- hatvalues(m4_lm)

# Rule of thumb: Leverage > 2k+2)/n
k <- length(coef(m4_lm)) - 1  # Number of predictors
n <- nrow(data_model_4)
leverage_threshold <- (2*k + 2) / n

outliers_leverage <- data_model_4 %>%
  filter(leverage > leverage_threshold)

cat("\nObservations with High Leverage (> (2k+2)/n):\n")
print(outliers_leverage[, c("entity",  "studentized_residuals")])


### 1.3 Cook's Distance ---------
data_model_4$cooks_distance <- cooks.distance(m4_lm)

# Rule of thumb: Cook's Distance > 4/n
cooks_threshold <- 4 / n

outliers_cooks <- data_model_4 %>%
  filter(cooks_distance > cooks_threshold)

cat("\nObservations with High Cook's Distance (> 4/n):\n")
print(outliers_cooks[, c("entity",  "studentized_residuals")])


### 1.4 Difference in Fitted Values (DFFITS) -----

data_model_4$dffits <- dffits(m4_lm)

# Rule of thumb: |DFFITS| > 2 * sqrt(k/n)
dffits_threshold <- 2 * sqrt(k / n)

outliers_dffits <- data_model_4 %>%
  filter(abs(dffits) > dffits_threshold)

cat("\nObservations with High DFFITS (> 2√(k/n)):\n")
print(outliers_dffits[, c("entity",  "studentized_residuals")])



### Identify Outliers and Egregious Outliers Based on Thresholds ----

data_model_4 <- data_model_4 |>
  mutate(
    outlier = as.integer(
      abs(studentized_residuals) > 2 |
        leverage > leverage_threshold |
        cooks_distance > cooks_threshold |
        abs(dffits) > dffits_threshold
    ),
    # Define egregious outlier: flagged by ALL metrics
    egregious_outlier = as.integer(
      (abs(studentized_residuals) > 2) +
        (leverage > leverage_threshold) +
        (cooks_distance > cooks_threshold) +
        (abs(dffits) > dffits_threshold) >= 4
    )
  )

outlier_summary <- data_model_4 |>
  mutate(n_metrics_flagged = 
           (abs(studentized_residuals) > 2) +
           (leverage > leverage_threshold) +
           (cooks_distance > cooks_threshold) +
           (abs(dffits) > dffits_threshold)
  ) |>
  filter(n_metrics_flagged > 1) |>
  arrange(desc(n_metrics_flagged))

cat("Observations flagged by more than one metric:", nrow(outlier_summary), "\n")

### Regression Models with and without Outliers -----

set.seed(03162026)
# Filter out identified outliers

# Model 1: Full Dataset
model_1 <- lm_robust(log_aid_all_sector ~ ti.corruption.perception.index_avg + 
                       log_gdp_per_capita + log_aid_baseline +
                       share.of.population.in.extreme.poverty_avg, 
                     data = data_model_4, se_type = "HC1")

# Model 2: Without Outliers

clean_data<- data_model_4|>
  filter(outlier == 0)

model_2 <- lm_robust(log_aid_all_sector ~ ti.corruption.perception.index_avg + 
                       log_gdp_per_capita + log_aid_baseline +
                       share.of.population.in.extreme.poverty_avg, 
                     data = clean_data, se_type = "HC1")

# Model 3: droping random observations

random_obs<-data_model_4|>
  slice_sample(n =73)

model_3 <- lm_robust(log_aid_all_sector ~ ti.corruption.perception.index_avg + 
                       log_gdp_per_capita + log_aid_baseline +
                       share.of.population.in.extreme.poverty_avg, 
                     data = random_obs, se_type = "HC1")

# Define the list of models
models <- list(
  "(1)" = model_1,
  "(2)" = model_2,
  "(3)" = model_3)

modelsummary(
  models = models,
  out = "html",
  stars = T
)


# Save as LaTeX with robust standard errors
modelsummary(models, 
             stars = c("*" = 0.1,
                       "**" = 0.05,
                       "***" = 0.01),
             coef_rename = c("ti.corruption.perception.index_avg" = "Corruption Index",
                             "log_gdp_per_capita" = "Log of Avg. GDP per capita (13-18)",
                             "log_aid_baseline" = "Log of Median Aid Baseline (13-18)",
                             "share.of.population.in.extreme.poverty_avg" = "Share Extreme Poverty"),
             gof_omit = "AIC|BIC|Log|Sigma",
             statistic = "std.error",  # Displays robust SEs
             output = "2.output/tables/regression_results_log_aid_outliers.tex")



