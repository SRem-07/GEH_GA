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

## Data processing for graph 1
trend_data <- df_clean %>% 
  filter(cohort >= 1905 & cohort <= 1925) %>% 
  group_by(cohort) %>%
  summarise(grad_rate = mean(finish_12_100, na.rm = TRUE))

# Regressions for graph 2
df_janas <- df_clean %>%
  filter(cohort >= 1907 & cohort <= 1919)

fit_boys <- feols(finish_12_100 ~ delta_unemp_estimate_youth_std | city1920 + cohort, 
                  data = df_clean[df_clean$male == 1, ])

df_janas$pred_grad <- predict(fit_boys, newdata = df_janas)

fit_girls <- feols(finish_12_100 ~ delta_unemp_estimate_youth_std | city1920 + cohort, 
                   data = df_clean[male == 0])

## Data processing and regressions for graph 3
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

# Reusable object for custom theme
plot_theme_custom <- theme(
  # Align titles to far-left edge
  plot.title.position = "plot",
  plot.caption.position = "plot",
  # Left align and add vertical spacing for titles
  plot.title = element_text(hjust = 0, face = "bold", size = 20, margin = margin(b = 15)),
  plot.subtitle = element_text(hjust = 0, size = 13, color = "gray30", margin = margin(b = 35)),
  # Increase space for x and y-axis title
  axis.title.y = element_text(margin = margin(l = 40), size = 11),
  axis.title.x = element_text(margin = margin(t = 20), size = 11),
  axis.text = element_text(size = 10),
  plot.margin = margin(20, 20, 20, 20), # Adds a 'breathing room' border
  # Align caption to left with larger margin
  plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 20)),
  panel.grid.major.x = element_blank(),
  panel.grid.minor = element_blank()
)

# Graph 1: Line Chart
# The historical context
# Shows rapid rise of "High School Movement" from 1910-1940

ggplot(trend_data, aes(x = cohort, y = grad_rate)) +
  geom_line(color = "#004f71", linewidth = 1.5) +
  theme_economist_white(gray_bg = FALSE) +
  scale_y_continuous(position = "right") +
  labs(title = "The great climb",
       subtitle = "High school graduation rate by birth cohort, %",
       x = "Birth Year", y = NULL, 
       caption = "Source: Janas (2026)") +
  plot_theme_custom

# Graph 2: Scatter Graph
# Gendered shock
# youth unemployment shocks drove graduation for boys, not girls

ggplot(df_janas, aes(x = delta_unemp_estimate_youth_std, y = pred_grad, color = gender_label)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.5) +
  scale_color_economist() +
  scale_y_continuous(position = "right") +
  theme_economist_white(gray_bg = FALSE) +
  labs(title = "Class of the Depression",
       subtitle = "Predicted graduation rate (%) vs. unemployment shock (std dev)",
       x = "Severity of unemployment shock", y = "Graduation rate (%)") +
  theme(plot.title.position = "plot",
        legend.position = "top", # CORRECTED: Use dot instead of underscore
        legend.title = element_blank()) +
  plot_theme_custom

# Graph 3: Coefficient Plot
# Shows high-income boys could leverage the crisis to stay in school
ggplot(coef_data, aes(x = Beta, y = reorder(Group, Beta))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_errorbarh(aes(xmin = Beta - 1.96*SE, xmax = Beta + 1,96*SE), height = 0, color = "#00a4ce", linewidth = 2.5) +
  geom_point(color = "#ed1c24", size = 4) +
  scale_x_continuous(expand = expansion(mult = 0.2)) +
  geom_text(aes(label = round(Beta, 2)), vjust = -1.5, size = 4, fontface = "bold") +
  theme_economist_white(gray_bg = FALSE) +
  labs(title = "Wealth and willpower",
       subtitle = "Impact of unemployment shock on graduation (percentage points)",
       x = "Effect Size", y = NULL) +
  plot_theme_custom
