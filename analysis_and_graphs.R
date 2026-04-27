## Code converted from STATA (in replication package) to R

library(data.table)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(fixest)

# 1. Load ONLY the columns needed to save RAM
# Variable 102 = Shock, 119 = Graduation, 49 = Birth Year, 85 = Gender, 50 = SES
df <- fread("edu_data.csv", select = c("cohort", "male", "occscore_pop_q", 
                                       "delta_unemp_estimate_youth_std", "finish_12_100", "city1920"))

# 2. Check the range of your data to avoid 0 observations
print(range(df$cohort, na.rm = TRUE)) 

# 3. Clean and interact (SES: 1 = Blue Collar, 3 = White Collar)
df_clean <- df %>%
  filter(!is.na(cohort) &!is.na(finish_12_100)) %>%
  mutate(
    high_ses = as.numeric(occscore_pop_q == 3),
    low_ses  = as.numeric(occscore_pop_q == 1),
    gender_label = ifelse(male == 1, "Boys", "Girls"),
    # Create a unique city ID for the regression fixed effects
    city_id = as.factor(city1920)
  )

# Verify the count
print(nrow(df_clean))

# Regression
  # Dependent variable: finish_12_100 (Graduation)
  # Treatment: delta_unemp_estimate_youth_std (Unemployment shock)
  # Fixed effects: city_id (City) + cohort (Birth year)
fit_table4 <- feols(finish_12_100 ~ delta_unemp_estimate_youth_std | city_id + cohort, 
                    data = df_clean, 
                    cluster = ~city_id)

summary(fit_table4)

# Heterogenous effects
# High-SES Boys
fit_high_ses_boys <- feols(grad_hisp ~ unemp_std | city_id + birth_year, 
                           data = df[male == 1 & high_ses == 1], 
                           cluster = ~city_id)

# Low-SES Boys
fit_low_ses_boys <- feols(grad_hisp ~ unemp_std | city_id + birth_year, 
                          data = df[male == 1 & high_ses == 0], 
                          cluster = ~city_id)

# Girls
fit_girls <- feols(grad_hisp ~ unemp_std | city_id + birth_year, 
                   data = df[male == 0], 
                   cluster = ~city_id)

# Extracting results for graphics
results_summary <- data.frame(
  group = c("High-SES Boys", "Low-SES Boys", "Girls"),
  estimate = c(coef(fit_high_ses_boys)["unemp_std"], 
               coef(fit_low_ses_boys)["unemp_std"], 
               coef(fit_girls)["unemp_std"]),
  std_error = c(se(fit_high_ses_boys)["unemp_std"], 
                se(fit_low_ses_boys)["unemp_std"], 
                se(fit_girls)["unemp_std"])
)

# Calculate 95% Confidence Intervals
results_summary <- results_summary %>%
  mutate(conf_low = estimate - 1.96 * std_error,
         conf_high = estimate + 1.96 * std_error)

## Graphs

# Graph 1: Line Chart
  # The historical context
  # Shows rapid rise of "High School Movement" from 1910-1940

trend_data <- df %>% 
  group_by(cohort) %>%
  summarise(grad_rate = mean(finish_12_100, na.rm = TRUE))

ggplot(trend_data, aes(x = cohort, y = grad_rate)) +
  geom_line(color = "#004f71", linewidth = 1.5) +
  theme_economist_white(gray_bg = FALSE) +
  scale_y_continuous(position = "right") +
  labs(title = "The great climb",
       subtitle = "High school graduation rate by birth cohort, %",
       x = "Birth Year", y = NULL, 
       caption = "Source: Janas (2026)") +
  theme(
    # Align titles to far-left edge
    plot.title.position = "plot",
    plot.caption.position = "plot",
    # Left align and add vertical spacing for titles
    plot.title = element_text(hjust = 0, face = "bold", size = 18, margin = margin(b = 15)),
    plot.subtitle = element_text(hjust = 0, size = 12, margin = margin(b = 35)),
    # Increase space for x and y-axis title
    axis.title.y = element_text(margin = margin(l = 40)),
    axis.title.x = element_text(margin = margin(t = 20)),
    # Align caption to left with larger margin
    plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 20)),
    # Remove gridlines
    panel.grid.major.x = element_blank()
  )

# Graph 2: Scatter Graph
  # Gendered shock
  # youth unemployment shocks drove graduation for boys, not girls
ggplot(df_clean, aes(x = delta_unemp_estimate_youth_std, y = finish_12_100, color = gender_label)) + 
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) + # Creates trend lines for genders based on linear regressions, with no standard error bars
  scale_color_economist() +
  theme_economist_white(gray_bg = FALSE) +
  theme(legend_position = "top", legend.title = element_blank()) +
  scale_y_continuous(position = "right") +
  labs(title = "Class of the Depression",
       subtitle = "Graduation rate (%) vs. local unemployment shock (std dev)",
       x = "Severity of unemployment shock", y = NULL, caption = "Source: Janas (2026)") +
  theme(
    # Align titles to far-left edge
    plot.title.position = "plot",
    plot.caption.position = "plot",
    # Left align and add vertical spacing for titles
    plot.title = element_text(hjust = 0, face = "bold", size = 18, margin = margin(b = 15)),
    plot.subtitle = element_text(hjust = 0, size = 12, margin = margin(b = 35)),
    # Increase space for x and y-axis title
    axis.title.y = element_text(margin = margin(l = 40)),
    axis.title.x = element_text(margin = margin(t = 20)),
    # Align caption to left with larger margin
    plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 20)),
    # Remove gridlines
    panel.grid.major.x = element_blank()
  )


# Graph 3: Coefficient Plot
  # Shows high-income boys could leverage the crisis to stay in school
# Run separate regressions for each group 
res_high_boys <- feols(finish_12_100 ~ delta_unemp_estimate_youth_std | city_id + cohort, 
                       data = df_clean[male == 1 & high_ses == 1])
res_low_boys  <- feols(finish_12_100 ~ delta_unemp_estimate_youth_std | city_id + cohort, 
                       data = df_clean[male == 1 & low_ses == 1])
res_girls     <- feols(finish_12_100 ~ delta_unemp_estimate_youth_std | city_id + cohort, 
                       data = df_clean[male == 0])

# Combine results for plotting
coef_data <- data.frame(
  Group = c("High-SES Boys", "Low-SES Boys", "All Girls"),
  Beta = c(coef(res_high_boys), coef(res_low_boys), coef(res_girls)),
  SE = c(se(res_high_boys), se(res_low_boys), se(res_girls))
)

ggplot(coef_data, aes(x = Beta, y = reorder(Group, Beta))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_errorbarh(aes(xmin = Beta - 1.96*SE, xmax = Beta + 1,96*SE), height = 0, color = "#00a4ce", linewidth = 2) +
  geom_point(color = "#ed1c24", size = 4) +
  theme_economist_white(gray_bg = FALSE) +
  labs(title = "Wealth and willpower",
       subtitle = "Impact of unemployment shock on graduation (percentage points)",
       x = "Effect Size", y = NULL) +
  theme(
    # Align titles to far-left edge
    plot.title.position = "plot",
    plot.caption.position = "plot",
    # Left align and add vertical spacing for titles
    plot.title = element_text(hjust = 0, face = "bold", size = 18, margin = margin(b = 15)),
    plot.subtitle = element_text(hjust = 0, size = 12, margin = margin(b = 35)),
    # Increase space for x and y-axis title
    axis.title.y = element_text(margin = margin(l = 40)),
    axis.title.x = element_text(margin = margin(t = 20)),
    # Align caption to left with larger margin
    plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 20)),
    # Remove gridlines
    panel.grid.major.x = element_blank()
  )


  











