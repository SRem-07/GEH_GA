# ==============================================================================
# Replication Code for:
# "Crises and Educational Attainment" — Pawel Janas
# Journal of Economic History
#
# R translation of replication-code-icspr.do
# Original Stata code: 8/6/2025
# ==============================================================================
# Notes on R equivalents used:
#   - reghdfe  -> fixest::feols() with multi-way fixed effects
#   - estout   -> modelsummary::modelsummary() / gt / kableExtra
#   - coefplot -> ggplot2 + broom::tidy()
#   - xtile    -> ntile() from dplyr
#   - probit weights -> weights argument in feols()
# ==============================================================================

library(tidyverse)    # data wrangling + ggplot2
library(fixest)       # feols() — equivalent to reghdfe
library(modelsummary) # regression tables — equivalent to esttab
library(broom)        # tidy() for model results
library(scales)       # axis formatting
library(ggrepel)      # optional: cleaner labels

# ------------------------------------------------------------------------------
# 0. Load data
# ------------------------------------------------------------------------------
# Adjust path as needed. Assumes the .dta has been converted to .csv.
df <- read_csv("edu_data.csv")


# ==============================================================================
# FIGURES
# ==============================================================================

# ------------------------------------------------------------------------------
# Figure 1, Panel B
# High school completion across Depression severity
# ------------------------------------------------------------------------------

df <- df |>
  mutate(
    blue_collar  = case_when(
      is.na(occscore_pop_q) ~ NA_real_,
      occscore_pop_q == 1   ~ 1,
      TRUE                  ~ 0
    ),
    white_collar = case_when(
      is.na(occscore_pop_q) ~ NA_real_,
      occscore_pop_q == 3   ~ 1,
      TRUE                  ~ 0
    ),
    finish_12_son       = finish_12_100 / 100,
    finish_blue_12_son  = finish_12_son * blue_collar,
    finish_white_12_son = finish_12_son * white_collar,
    ones       = if_else(is.na(occscore_pop_q), NA_real_, 1),
    ones_blue  = if_else(!is.na(blue_collar) & blue_collar == 1, 1, 0),
    ones_white = if_else(!is.na(white_collar) & white_collar == 1, 1, 0)
  )

# Tertile of unemployment shock (equivalent to Stata xtile n(3))
df <- df |>
  mutate(retail_q = ntile(delta_unemp_estimate_youth_std, 3))

# Aggregate to retail_q x cohort level
df <- df |>
  group_by(retail_q, cohort) |>
  mutate(
    n_sample      = sum(ones, na.rm = TRUE),
    test          = sum(finish_12_son, na.rm = TRUE),
    test_white    = sum(finish_white_12_son, na.rm = TRUE),
    test_blue     = sum(finish_blue_12_son, na.rm = TRUE),
    total_white   = sum(white_collar, na.rm = TRUE),
    total_blue    = sum(blue_collar, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    finish_white_share = test_white / total_white,
    finish_blue_share  = test_blue  / total_blue,
    finish_share       = test / n_sample
  )

# Index to 1930 baseline within each retail_q group
df <- df |>
  group_by(retail_q) |>
  mutate(
    finish_blue_share_30  = finish_blue_share[cohort == 1930][1],
    finish_white_share_30 = finish_white_share[cohort == 1930][1]
  ) |>
  ungroup() |>
  mutate(
    xaxis               = cohort - 1,
    finish_white_delta  = finish_white_share - finish_white_share_30,
    finish_blue_delta   = finish_blue_share  - finish_blue_share_30
  )

# Collapse to one row per retail_q x cohort for plotting
fig1b_data <- df |>
  distinct(retail_q, cohort, xaxis, finish_share) |>
  filter(retail_q %in% c(1, 3))

fig1b <- ggplot(fig1b_data, aes(x = xaxis, y = finish_share,
                                group = factor(retail_q),
                                linetype = factor(retail_q),
                                shape = factor(retail_q))) +
  geom_line(color = "black") +
  geom_point(color = "black", size = 2) +
  geom_vline(xintercept = 1929.5, linetype = "dashed") +
  geom_hline(yintercept = 1, color = "black", alpha = 0.1, linetype = "dashed") +
  scale_x_continuous(
    name   = "Year cohort turns 17",
    breaks = seq(1925, 1938, 1),
    limits = c(1924, 1939)
  ) +
  scale_y_continuous(
    name   = "",
    breaks = seq(0.40, 0.60, 0.02),
    limits = c(0.40, 0.60)
  ) +
  scale_shape_manual(
    values = c("1" = 1, "3" = 16),
    labels = c("1" = "Q1 unemployment county", "3" = "Q3 unemployment county")
  ) +
  scale_linetype_manual(
    values = c("1" = "solid", "3" = "solid"),
    labels = c("1" = "Q1 unemployment county", "3" = "Q3 unemployment county")
  ) +
  labs(
    title    = "High school completion across Depression severity",
    shape    = NULL,
    linetype = NULL
  ) +
  theme_bw() +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave("retail_motivation.png", fig1b, width = 8, height = 5, dpi = 300)


# ------------------------------------------------------------------------------
# Figure 1, Panel C
# Delta high school completion across Depression severity + parent occupation
# ------------------------------------------------------------------------------

fig1c_data <- df |>
  distinct(retail_q, cohort, xaxis,
           finish_blue_delta, finish_white_delta) |>
  filter(retail_q %in% c(1, 3)) |>
  pivot_longer(
    cols      = c(finish_blue_delta, finish_white_delta),
    names_to  = "occ_type",
    values_to = "delta"
  ) |>
  mutate(
    group_label = case_when(
      retail_q == 1 & occ_type == "finish_blue_delta"  ~ "Q1 unemp. - Q1 occ",
      retail_q == 3 & occ_type == "finish_blue_delta"  ~ "Q3 unemp - Q1 occ",
      retail_q == 1 & occ_type == "finish_white_delta" ~ "Q1 unemp. - Q3 occ",
      retail_q == 3 & occ_type == "finish_white_delta" ~ "Q3 unemp. - Q3 occ"
    ),
    line_type = if_else(occ_type == "finish_white_delta", "dashed", "solid")
  )

fig1c <- ggplot(fig1c_data,
                aes(x = xaxis, y = delta,
                    group = group_label,
                    linetype = line_type,
                    shape    = factor(retail_q))) +
  geom_line(color = "black") +
  geom_point(color = "black", size = 2) +
  geom_vline(xintercept = 1929.5, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black", alpha = 0.1, linetype = "dashed") +
  scale_x_continuous(
    name   = "Year cohort turns 17",
    breaks = seq(1925, 1938, 1),
    limits = c(1924, 1939)
  ) +
  scale_y_continuous(
    name   = "",
    breaks = seq(-0.04, 0.14, 0.02),
    limits = c(-0.04, 0.14)
  ) +
  scale_linetype_identity() +
  scale_shape_manual(values = c("1" = 1, "3" = 16)) +
  labs(
    title    = "\u0394 High school completion across Depression severity + parent occupation",
    color    = NULL,
    linetype = NULL,
    shape    = NULL
  ) +
  guides(shape = guide_legend(
    labels = c("1" = "Q1 unemp.", "3" = "Q3 unemp."),
    title  = NULL
  )) +
  theme_bw() +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave("retail_motivation_b.png", fig1c, width = 8, height = 5, dpi = 300)


# ==============================================================================
# Figure 2
# Coefficient plots by occupation quintile, gender, and outcome
# (Equivalent to reghdfe with interactions + coefplot)
# ==============================================================================

# Helper: build the right-hand side formula for reghdfe-style specs
# Fixed effects:  id_ + cohort FE + state x cohort FE (absorbed)
# Regressors: delta_unemp x post_d4 interaction + controls

# Stata: absorb(id_  ib1925.cohort  i.stateicp1930#ib1925.cohort)
# R/fixest: | id_ + cohort + stateicp1930^cohort

occ_controls <- paste0(
  "c(share_occu1, share_occu2, share_occu3, share_occu4, share_occu5,",
  "share_occu6, share_occu7, share_occu8, share_occu9, share_occu10)[cohort]"
)

# Note: fixest uses i(post_d4, delta_unemp_estimate_youth_std, ref=0) for
# the interaction c.delta##ib0.post_d4 in Stata.
# post_d4 takes values 0 (reference), 1, 2 per the coefplot labels.

fig2_results <- list()

for (k in 1:3) {
  for (j in c(0, 1)) {
    for (outcome in c("finish_9_100", "finish_12_100", "finish_13_100", "educ_cont")) {
      
      sub <- df |> filter(male == j, occscore_pop_q == k)
      
      fit <- tryCatch(
        feols(
          as.formula(paste0(
            outcome, " ~
            i(post_d4, delta_unemp_estimate_youth_std, ref = 0) +
            deposit_pc_std + manu_pc_std +
            i(cohort, share_occu1)  + i(cohort, share_occu2)  +
            i(cohort, share_occu3)  + i(cohort, share_occu4)  +
            i(cohort, share_occu5)  + i(cohort, share_occu6)  +
            i(cohort, share_occu7)  + i(cohort, share_occu8)  +
            i(cohort, share_occu9)  + i(cohort, share_occu10) +
            i(race1920) + i(nativity1920) |
            id_ + cohort + stateicp1930^cohort"
          )),
          data    = sub,
          weights = ~probit_w,
          cluster = ~id_
        ),
        error = function(e) NULL
      )
      
      key <- paste(k, j, outcome, sep = "_")
      fig2_results[[key]] <- list(fit = fit, k = k, j = j, outcome = outcome)
    }
  }
}

# --- Plotting helper for Figure 2 ---
plot_fig2 <- function(results_list, gender_val, outcome_var,
                      x_limits, x_breaks, title_str, filename) {
  
  coef_data <- map_dfr(
    results_list,
    function(entry) {
      if (is.null(entry$fit)) return(NULL)
      if (entry$j != gender_val || entry$outcome != outcome_var) return(NULL)
      tidy(entry$fit, conf.int = TRUE, conf.level = 0.90) |>
        filter(str_detect(term, "post_d4::")) |>
        mutate(
          occ_q   = entry$k,
          term_label = case_when(
            str_detect(term, "::1:") ~ "Age: [15-19] x \u0394Unemp",
            str_detect(term, "::2:") ~ "Age: [11-14] x \u0394Unemp",
            TRUE ~ term
          )
        )
    }
  )
  
  if (nrow(coef_data) == 0) {
    message("No data for: ", filename)
    return(invisible(NULL))
  }
  
  p <- ggplot(coef_data,
              aes(x = estimate, y = term_label,
                  xmin = conf.low, xmax = conf.high,
                  color = factor(occ_q),
                  shape = factor(occ_q))) +
    geom_vline(xintercept = 0, linewidth = 0.6) +
    geom_pointrange(
      aes(xmin = conf.low, xmax = conf.high),
      position = position_dodge(width = 0.4),
      fatten = 3
    ) +
    scale_x_continuous(
      limits = x_limits,
      breaks = x_breaks
    ) +
    scale_color_manual(
      values = c("1" = "black", "2" = "grey40", "3" = "grey70"),
      labels = c("1" = "Q1", "2" = "Q2", "3" = "Q3")
    ) +
    scale_shape_manual(
      values = c("1" = 16, "2" = 17, "3" = 15),
      labels = c("1" = "Q1", "2" = "Q2", "3" = "Q3")
    ) +
    labs(
      title  = title_str,
      x      = NULL, y = NULL,
      color  = NULL, shape = NULL
    ) +
    theme_bw() +
    theme(
      legend.position  = "bottom",
      panel.grid.minor = element_blank()
    )
  
  ggsave(filename, p, width = 7, height = 4, dpi = 300)
  invisible(p)
}

# Figure 2, Panels — Women
plot_fig2(fig2_results, 0, "finish_9_100",  c(-2, 2),   seq(-2, 2, 0.5),
          "Women — Outcome: I[Finish 9 years+]",  "edu9_female.png")
plot_fig2(fig2_results, 0, "finish_12_100", c(-2, 2),   seq(-2, 2, 0.5),
          "Women — Outcome: I[Finish 12 years+]", "edu12_female.png")
plot_fig2(fig2_results, 0, "finish_13_100", c(-2, 2),   seq(-2, 2, 0.5),
          "Women — Outcome: I[Finish 13 years+]", "edu13_female.png")
plot_fig2(fig2_results, 0, "educ_cont",     c(-0.1, 0.1), seq(-0.1, 0.1, 0.02),
          "Women — Outcome: Years of Education",  "cont_female.png")

# Figure 2, Panels — Men
plot_fig2(fig2_results, 1, "finish_9_100",  c(-2, 2),   seq(-2, 2, 0.5),
          "Men — Outcome: I[Finish 9 years+]",   "edu9_male.png")
plot_fig2(fig2_results, 1, "finish_12_100", c(-1, 4),   seq(-1, 4, 0.5),
          "Men — Outcome: I[Finish 12 years+]",  "edu12_male.png")
plot_fig2(fig2_results, 1, "finish_13_100", c(-2, 2),   seq(-2, 2, 0.5),
          "Men — Outcome: I[Finish 13 years+]",  "edu13_male.png")
plot_fig2(fig2_results, 1, "educ_cont",     c(-0.1, 0.22), seq(-0.1, 0.22, 0.04),
          "Men — Outcome: Years of Education",   "cont_male.png")


# ==============================================================================
# Figure 3
# Age-specific effect of unemployment shock on education
# (Equivalent to reghdfe with c.delta##ib18.age1930)
# ==============================================================================

fig3_results <- list()

for (j in c(0, 1)) {
  for (outcome in c("finish_12_100", "educ_cont")) {
    fit <- tryCatch(
      feols(
        as.formula(paste0(
          outcome, " ~
          i(age1930, delta_unemp_estimate_youth_std, ref = 18) |
          id_ + cohort"
        )),
        data    = df |> filter(male == j),
        weights = ~probit_w,
        cluster = ~id_
      ),
      error = function(e) NULL
    )
    fig3_results[[paste(j, outcome, sep = "_")]] <- list(fit = fit, j = j, outcome = outcome)
  }
}

plot_fig3 <- function(results_list, outcome_var, title_str, filename) {
  
  coef_data <- map_dfr(
    results_list,
    function(entry) {
      if (is.null(entry$fit) || entry$outcome != outcome_var) return(NULL)
      tidy(entry$fit, conf.int = TRUE, conf.level = 0.90) |>
        filter(str_detect(term, "age1930::")) |>
        mutate(
          gender = if_else(entry$j == 0, "Women", "Men"),
          age    = as.integer(str_extract(term, "(?<=::)\\d+"))
        )
    }
  )
  
  if (nrow(coef_data) == 0) {
    message("No data for: ", filename)
    return(invisible(NULL))
  }
  
  p <- ggplot(coef_data,
              aes(x = age, y = estimate,
                  ymin = conf.low, ymax = conf.high,
                  color = gender, group = gender)) +
    geom_hline(yintercept = 0, linewidth = 0.6) +
    geom_line() +
    geom_point(size = 2) +
    geom_ribbon(aes(fill = gender), alpha = 0.15, linetype = "dashed") +
    scale_x_continuous(
      name   = "Age in 1930",
      breaks = 11:23
    ) +
    scale_color_manual(values = c("Women" = "black", "Men" = "grey40")) +
    scale_fill_manual(values  = c("Women" = "black", "Men" = "grey40")) +
    labs(
      title = title_str,
      y     = NULL, color = NULL, fill = NULL
    ) +
    theme_bw() +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1),
      legend.position  = "bottom",
      panel.grid.minor = element_blank()
    )
  
  ggsave(filename, p, width = 7, height = 4, dpi = 300)
  invisible(p)
}

plot_fig3(fig3_results, "finish_12_100",
          "Outcome: I[Finish 12 years+] by 1940",
          "gender_coefplot_12.png")

plot_fig3(fig3_results, "educ_cont",
          "Outcome: Years of Education Completed by 1940",
          "gender_coefplot_educ.png")


# ==============================================================================
# TABLES
# ==============================================================================

# ------------------------------------------------------------------------------
# Table 2
# Relationship of unemployment measure with other characteristics (city level)
# ------------------------------------------------------------------------------

city_df <- df |>
  select(id_, d_output_log_29_33, delta_unemp_estimate_youth,
         state_fips, RLDF3329, manu_share, whol_share, ret_share,
         unemp_1937_urb, log_city_pop) |>
  distinct()

# Standardise each variable
standardize <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

city_df <- city_df |>
  mutate(across(
    c(d_output_log_29_33, delta_unemp_estimate_youth,
      RLDF3329, manu_share, whol_share, ret_share, unemp_1937_urb),
    list(std = standardize)
  ))

tab2_outcomes <- c("RLDF3329_std", "d_output_log_29_33_std",
                   "unemp_1937_urb_std", "whol_share_std",
                   "ret_share_std", "manu_share_std")

tab2_models <- lapply(tab2_outcomes, function(y) {
  lm(
    as.formula(paste(y, "~ delta_unemp_estimate_youth_std")),
    data    = city_df,
    weights = log_city_pop
  )
})
names(tab2_models) <- c("Delta Retail Sales", "Delta Manu. Output",
                        "Unemployment 1937", "Wholesale Share",
                        "Retail Share", "Manu Share")

# Note: for clustered SEs by state_fips use sandwich/lmtest or fixest
# Here we use fixest for consistency with clustered SEs
tab2_models_fe <- lapply(tab2_outcomes, function(y) {
  feols(
    as.formula(paste(y, "~ delta_unemp_estimate_youth_std")),
    data    = city_df,
    weights = ~log_city_pop,
    cluster = ~state_fips
  )
})
names(tab2_models_fe) <- names(tab2_models)

modelsummary(
  tab2_models_fe,
  coef_map     = c("delta_unemp_estimate_youth_std" = "\u0394 Unemp (std.)"),
  stars        = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map      = c("nobs", "r.squared"),
  output       = "unemp_covar.tex",
  title        = "Table 2: Unemployment measure and city characteristics",
  fmt          = 3
)


# ------------------------------------------------------------------------------
# Table 3: Summary statistics
# ------------------------------------------------------------------------------

sum_vars <- c("educ_cont", "finish_9_100", "finish_10_100", "finish_11_100",
              "finish_12_100", "finish_13_100", "unemp_estimate_youth",
              "delta_unemp_estimate_youth", "deposit_pc", "manu_pc",
              "age1930", "male")

complete_mask <- complete.cases(
  df[, c("delta_unemp_estimate_youth_std", "post_d4", "deposit_pc_std",
         "manu_pc_std", paste0("share_occu", 1:10),
         "race1920", "nativity1920", "stateicp1930", "probit_w")]
)

sum_df <- df[complete_mask, sum_vars]

datasummary(
  All(sum_df) ~ N + Mean + SD + Median + P25 + P75,
  data   = sum_df,
  output = "summary_stats_linked_sons.tex",
  title  = "Table 3: Summary Statistics"
)


# ------------------------------------------------------------------------------
# Helper: standard reghdfe specification (Tables 4-8)
# Stata absorb: id_  ib1930.cohort  i.stateicp1930#ib1930.cohort
# R fixest:     id_ + cohort + stateicp1930^cohort
# ------------------------------------------------------------------------------

run_main_spec <- function(outcome, data, ref_cohort = 1930,
                          extra_rhs = "", extra_fe = "") {
  fe_str <- paste0(
    "id_ + cohort + stateicp1930^cohort",
    if (nchar(extra_fe) > 0) paste0(" + ", extra_fe) else ""
  )
  rhs <- paste0(
    "i(post_d4, delta_unemp_estimate_youth_std, ref = 0) +
     deposit_pc_std + manu_pc_std +
     i(cohort, share_occu1)  + i(cohort, share_occu2)  +
     i(cohort, share_occu3)  + i(cohort, share_occu4)  +
     i(cohort, share_occu5)  + i(cohort, share_occu6)  +
     i(cohort, share_occu7)  + i(cohort, share_occu8)  +
     i(cohort, share_occu9)  + i(cohort, share_occu10) +
     i(race1920) + i(nativity1920)",
    if (nchar(extra_rhs) > 0) paste0(" + ", extra_rhs) else ""
  )
  fml <- as.formula(paste0(outcome, " ~ ", rhs, " | ", fe_str))
  feols(fml, data = data, weights = ~probit_w, cluster = ~id_)
}


# ------------------------------------------------------------------------------
# Table 4, Panel A
# Baseline: all observations, pooled gender
# ------------------------------------------------------------------------------

tab4a_outcomes <- c("finish_9_100", "finish_10_100", "finish_11_100",
                    "finish_12_100", "finish_13_100", "educ_cont")

tab4a_models <- lapply(tab4a_outcomes, function(y) {
  run_main_spec(y, df)
})
names(tab4a_models) <- tab4a_outcomes

# Keep only the post_d4 x delta_unemp interaction terms (drop ref = 0 category)
coef_keep <- function(term) str_detect(term, "post_d4::[^0].*delta_unemp")

modelsummary(
  tab4a_models,
  coef_omit    = "(?!.*post_d4::[^0].*delta_unemp)",
  stars        = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map      = c("nobs", "adj.r.squared"),
  output       = "diff_in_diff_linked.tex",
  title        = "Table 4 Panel A: Main Results"
)


# ------------------------------------------------------------------------------
# Table 4, Panel B
# Sibling fixed effects (absorb: serial1920 instead of id_)
# Restricted to nsibs_linked > 1 and age_difference < 7
# ------------------------------------------------------------------------------

tab4b_data <- df |> filter(nsibs_linked > 1, age_difference < 7)

tab4b_models <- lapply(tab4a_outcomes, function(y) {
  fml <- as.formula(paste0(
    y, " ~
    i(post_d4, delta_unemp_estimate_youth_std, ref = 0) +
    deposit_pc_std + manu_pc_std +
    i(cohort, share_occu1)  + i(cohort, share_occu2)  +
    i(cohort, share_occu3)  + i(cohort, share_occu4)  +
    i(cohort, share_occu5)  + i(cohort, share_occu6)  +
    i(cohort, share_occu7)  + i(cohort, share_occu8)  +
    i(cohort, share_occu9)  + i(cohort, share_occu10) +
    i(race1920) + i(nativity1920) |
    serial1920 + cohort + stateicp1930^cohort"
  ))
  feols(fml, data = tab4b_data, weights = ~probit_w, cluster = ~id_)
})
names(tab4b_models) <- tab4a_outcomes

modelsummary(
  tab4b_models,
  coef_omit = "(?!.*post_d4::[^0].*delta_unemp)",
  stars     = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map   = c("nobs", "adj.r.squared"),
  output    = "diff_in_diff_linked_bro.tex",
  title     = "Table 4 Panel B: Sibling Fixed Effects"
)


# ------------------------------------------------------------------------------
# Table 5, Panel A
# By gender (male == 0 women, male == 1 men)
# Outcomes: finish_9, finish_12, finish_13, educ_cont
# ------------------------------------------------------------------------------

tab5_outcomes <- c("finish_9_100", "finish_12_100", "finish_13_100", "educ_cont")

tab5a_models <- list()
for (j in c(0, 1)) {
  for (y in tab5_outcomes) {
    key <- paste0(if (j == 0) "female" else "male", "_", y)
    tab5a_models[[key]] <- run_main_spec(y, df |> filter(male == j))
  }
}

modelsummary(
  tab5a_models,
  coef_omit = "(?!.*post_d4::[^0].*delta_unemp)",
  stars     = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map   = c("nobs", "adj.r.squared"),
  output    = "diff_in_diff_linked_sex.tex",
  title     = "Table 5 Panel A: Results by Gender"
)


# ------------------------------------------------------------------------------
# Table 5, Panel B
# Sibling FE by gender — sisters and brothers separately
# (shares only go to share_occu9 for women; shares x post_d4 for men)
# ------------------------------------------------------------------------------

tab5b_models <- list()

# Sisters (male == 0)
for (y in tab5_outcomes) {
  fml <- as.formula(paste0(
    y, " ~
    i(post_d4, delta_unemp_estimate_youth_std, ref = 0) +
    deposit_pc_std + manu_pc_std +
    i(race1920) + i(nativity1920) +
    i(cohort, share_occu1) + i(cohort, share_occu2) +
    i(cohort, share_occu3) + i(cohort, share_occu4) +
    i(cohort, share_occu5) + i(cohort, share_occu6) +
    i(cohort, share_occu7) + i(cohort, share_occu8) +
    i(cohort, share_occu9) |
    serial1920 + cohort + stateicp1930^cohort"
  ))
  tab5b_models[[paste0("sister_", y)]] <- feols(
    fml,
    data    = df |> filter(male == 0, age_difference < 7),
    weights = ~probit_w,
    cluster = ~id_
  )
}

# Brothers (male == 1) — shares interacted with post_d4
for (y in tab5_outcomes) {
  fml <- as.formula(paste0(
    y, " ~
    i(post_d4, delta_unemp_estimate_youth_std, ref = 0) +
    deposit_pc_std + manu_pc_std +
    i(post_d4, share_occu1)  + i(post_d4, share_occu2)  +
    i(post_d4, share_occu3)  + i(post_d4, share_occu4)  +
    i(post_d4, share_occu5)  + i(post_d4, share_occu6)  +
    i(post_d4, share_occu7)  + i(post_d4, share_occu8)  +
    i(post_d4, share_occu9)  + i(post_d4, share_occu10) +
    i(race1920) + i(nativity1920) |
    serial1920 + cohort + stateicp1930^cohort"
  ))
  tab5b_models[[paste0("brother_", y)]] <- feols(
    fml,
    data    = df |> filter(male == 1, age_difference < 7),
    weights = ~probit_w,
    cluster = ~id_
  )
}

modelsummary(
  tab5b_models,
  coef_omit = "(?!.*post_d4::[^0].*delta_unemp)",
  stars     = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map   = c("nobs", "adj.r.squared"),
  output    = "diff_in_diff_linked_bro_sis.tex",
  title     = "Table 5 Panel B: Sibling FE by Gender"
)


# ------------------------------------------------------------------------------
# Table 6, Panel A
# Triple interaction with father's occupational quintile (occscore_pop_q3)
# Reference cohort: 1925 (ib1925 in Stata)
# ------------------------------------------------------------------------------

# Note: Stata absorbs id_ + ib1925.cohort + stateicp1930#ib1925.cohort
# The triple interactions c.x##ib0.post_d4##i.occscore_pop_q3 map to
# i(post_d4, delta_unemp_estimate_youth_std, ref=0):i(occscore_pop_q3) in fixest

tab6a_models <- list()

for (j in c(0, 1)) {
  for (y in tab5_outcomes) {
    
    sub <- df |> filter(male == j)
    
    fml <- as.formula(paste0(
      y, " ~
      delta_unemp_estimate_youth_std +
      i(post_d4, ref = 0) +
      i(occscore_pop_q3) +
      i(post_d4, delta_unemp_estimate_youth_std, ref = 0) +
      i(occscore_pop_q3, delta_unemp_estimate_youth_std) +
      i(post_d4, occscore_pop_q3, ref = 0) +
      i(post_d4, delta_unemp_estimate_youth_std, ref = 0):i(occscore_pop_q3) +
      deposit_pc_std + deposit_pc_std:i(occscore_pop_q3) +
      manu_pc_std    + manu_pc_std:i(occscore_pop_q3) +
      i(race1920) + i(race1920):i(occscore_pop_q3) +
      i(nativity1920) + i(nativity1920):i(occscore_pop_q3) |
      id_ + cohort + stateicp1930^cohort"
    ))
    
    tab6a_models[[paste0(if (j == 0) "women" else "men", "_", y)]] <-
      tryCatch(
        feols(fml, data = sub, weights = ~probit_w, cluster = ~id_),
        error = function(e) NULL
      )
  }
}

modelsummary(
  Filter(Negate(is.null), tab6a_models),
  stars   = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "adj.r.squared"),
  output  = "diff_in_diff_linked_hypo.tex",
  title   = "Table 6 Panel A: Heterogeneity — Father's Occupation"
)


# ------------------------------------------------------------------------------
# Table 6, Panel B
# Triple interaction with own adult occupational quintile (adult_occ_q3)
# absorb: id_##adult_occ_q3 + ib1925.cohort + stateicp1930#ib1925.cohort
# ------------------------------------------------------------------------------

tab6b_models <- list()

for (j in c(0, 1)) {
  for (y in tab5_outcomes) {
    
    sub <- df |> filter(male == j)
    
    fml <- as.formula(paste0(
      y, " ~
      delta_unemp_estimate_youth_std +
      i(post_d4, ref = 0) +
      i(adult_occ_q3) +
      i(post_d4, delta_unemp_estimate_youth_std, ref = 0) +
      i(adult_occ_q3, delta_unemp_estimate_youth_std) +
      i(post_d4, adult_occ_q3, ref = 0) +
      i(post_d4, delta_unemp_estimate_youth_std, ref = 0):i(adult_occ_q3) +
      deposit_pc_std + deposit_pc_std:i(adult_occ_q3) +
      manu_pc_std    + manu_pc_std:i(adult_occ_q3) +
      i(race1920) + i(race1920):i(adult_occ_q3) +
      i(nativity1920) + i(nativity1920):i(adult_occ_q3) |
      id_^adult_occ_q3 + cohort + stateicp1930^cohort"
    ))
    
    tab6b_models[[paste0(if (j == 0) "women" else "men", "_", y)]] <-
      tryCatch(
        feols(fml, data = sub, weights = ~probit_w, cluster = ~id_),
        error = function(e) NULL
      )
  }
}

modelsummary(
  Filter(Negate(is.null), tab6b_models),
  stars   = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "adj.r.squared"),
  output  = "diff_in_diff_linked_hypo3.tex",
  title   = "Table 6 Panel B: Heterogeneity — Own Adult Occupation"
)


# ------------------------------------------------------------------------------
# Table 7
# Labor market outcomes: log wage, labor force, in school, emergency work,
# occupational score
# ------------------------------------------------------------------------------

tab7_wage   <- c("log_wage")
tab7_other  <- c("labforce_i_100", "inschool_i_100", "emergency_i_100", "occscore1940")

tab7_models <- list()

for (j in c(0, 1)) {
  gender_str <- if (j == 0) "female" else "male"
  
  # log wage with experience controls
  fml_wage <- as.formula(
    "log_wage ~
     i(post_d4, delta_unemp_estimate_youth_std, ref = 0) +
     deposit_pc_std + manu_pc_std + exp_ + exp2_ +
     i(race1920) + i(nativity1920) +
     i(cohort, share_occu1)  + i(cohort, share_occu2)  +
     i(cohort, share_occu3)  + i(cohort, share_occu4)  +
     i(cohort, share_occu5)  + i(cohort, share_occu6)  +
     i(cohort, share_occu7)  + i(cohort, share_occu8)  +
     i(cohort, share_occu9)  + i(cohort, share_occu10) |
     id_ + cohort + stateicp1930^cohort"
  )
  tab7_models[[paste0(gender_str, "_log_wage")]] <-
    tryCatch(
      feols(fml_wage, data = df |> filter(male == j),
            weights = ~probit_w, cluster = ~id_),
      error = function(e) NULL
    )
  
  # Other outcomes (no experience controls)
  for (y in tab7_other) {
    tab7_models[[paste0(gender_str, "_", y)]] <-
      tryCatch(
        run_main_spec(y, df |> filter(male == j)),
        error = function(e) NULL
      )
  }
}

modelsummary(
  Filter(Negate(is.null), tab7_models),
  coef_omit = "(?!.*post_d4::[^0].*delta_unemp)",
  stars     = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map   = c("nobs", "adj.r.squared"),
  output    = "diff_in_diff_linked_other.tex",
  title     = "Table 7: Labor Market Outcomes"
)


# ------------------------------------------------------------------------------
# Table 8
# Robustness: finish_12 with alternative specs
# (no movers, + education spending, + WPA quartiles)
# Reference cohort: 1930 (same as main spec)
# ------------------------------------------------------------------------------

tab8_models <- list()

for (j in c(0, 1)) {
  gender_str <- if (j == 0) "female" else "male"
  sub <- df |> filter(male == j)
  
  # Baseline
  tab8_models[[paste0(gender_str, "_base")]] <-
    run_main_spec("finish_12_100", sub)
  
  # No movers
  tab8_models[[paste0(gender_str, "_nomo")]] <-
    run_main_spec("finish_12_100", sub |> filter(mover == 0))
  
  # + Education spending (exp_total_3430 x post_d4)
  tab8_models[[paste0(gender_str, "_edu")]] <-
    run_main_spec(
      "finish_12_100", sub,
      extra_rhs = "i(post_d4, exp_total_3430, ref = 0)"
    )
  
  # + WPA quartiles x cohort
  tab8_models[[paste0(gender_str, "_wpa")]] <-
    run_main_spec(
      "finish_12_100", sub,
      extra_rhs = "i(cohort, wpa_pc_q)"
    )
}

modelsummary(
  tab8_models,
  coef_omit = "(?!.*post_d4::[^0].*delta_unemp)",
  stars     = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map   = c("nobs", "adj.r.squared"),
  output    = "diff_in_diff_linked_robust1.tex",
  title     = "Table 8: Robustness — finish_12_100"
)


# Same structure for educ_cont (second robust table)

tab8b_models <- list()

for (j in c(0, 1)) {
  gender_str <- if (j == 0) "female" else "male"
  sub <- df |> filter(male == j)
  
  tab8b_models[[paste0(gender_str, "_base")]] <-
    run_main_spec("educ_cont", sub)
  
  tab8b_models[[paste0(gender_str, "_nomo")]] <-
    run_main_spec("educ_cont", sub |> filter(mover == 0))
  
  tab8b_models[[paste0(gender_str, "_edu")]] <-
    run_main_spec(
      "educ_cont", sub,
      extra_rhs = "i(post_d4, exp_total_3430, ref = 0)"
    )
  
  tab8b_models[[paste0(gender_str, "_wpa")]] <-
    run_main_spec(
      "educ_cont", sub,
      extra_rhs = "i(cohort, wpa_pc_q)"
    )
}

modelsummary(
  tab8b_models,
  coef_omit = "(?!.*post_d4::[^0].*delta_unemp)",
  stars     = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map   = c("nobs", "adj.r.squared"),
  output    = "diff_in_diff_linked_robust2.tex",
  title     = "Table 8: Robustness — educ_cont"
)

message("Replication complete.")