# =============================================================================
# Hospital Readmissions Reduction Program-Analytics Project
# FY 2025 | CMS Data
# Three datasets: HRRP (condition-level), General Info, Supplemental (hospital-level)
# =============================================================================


# -----------------------------------------------------------------------------
# 1. LOAD PACKAGES
# -----------------------------------------------------------------------------

library(tidyverse)
library(broom)
library(car)
library(here)


# -----------------------------------------------------------------------------
# 2. FILE PATHS (dynamic)
# -----------------------------------------------------------------------------

hrrp_path <- here("FY_2025_Hospital_Readmissions_Reduction_Program_Hospital 1.csv")
gen_path  <- here("Hospital_General_Information.csv")
supp_path <- here("Copy_of_FY2025_Final_Rule_Supplemental_File 1.csv")


# -----------------------------------------------------------------------------
# 3. READ DATA
# -----------------------------------------------------------------------------

hrrp <- readr::read_csv(hrrp_path, show_col_types = FALSE)
gen  <- readr::read_csv(gen_path,  show_col_types = FALSE)
supp <- readr::read_csv(supp_path, skip = 1, show_col_types = FALSE)


# -----------------------------------------------------------------------------
# 4. CLEAN DATA
# -----------------------------------------------------------------------------

# --- HRRP: condition-level readmission data ---
hrrp <- hrrp %>%
  mutate(
    `Facility ID`  = as.numeric(`Facility ID`),
    excess_ratio   = suppressWarnings(as.numeric(`Excess Readmission Ratio`)),
    pred_rate      = suppressWarnings(as.numeric(`Predicted Readmission Rate`)),
    exp_rate       = suppressWarnings(as.numeric(`Expected Readmission Rate`)),
    num_discharges = suppressWarnings(as.numeric(`Number of Discharges`)),
    state          = factor(State),
    # Clean measure names to short labels for better plot readability
    condition = case_when(
      `Measure Name` == "READM-30-AMI-HRRP"      ~ "AMI",
      `Measure Name` == "READM-30-HF-HRRP"       ~ "Heart Failure",
      `Measure Name` == "READM-30-COPD-HRRP"     ~ "COPD",
      `Measure Name` == "READM-30-PN-HRRP"       ~ "Pneumonia",
      `Measure Name` == "READM-30-CABG-HRRP"     ~ "CABG",
      `Measure Name` == "READM-30-HIP-KNEE-HRRP" ~ "Hip/Knee",
      TRUE ~ `Measure Name`
    ),
    condition = factor(condition)
  )

# --- GEN: hospital-level general information ---
gen <- gen %>%
  mutate(
    `Facility ID`    = suppressWarnings(as.numeric(`Facility ID`)),
    rating_num       = suppressWarnings(as.numeric(`Hospital overall rating`)),
    has_er           = if_else(`Emergency Services` == "Yes", 1L, 0L),
    is_rated         = if_else(!is.na(suppressWarnings(as.numeric(`Hospital overall rating`))), 1L, 0L),
    # Quality composite measures-how many measures better vs worse than national avg
    readm_better     = suppressWarnings(as.numeric(`Count of READM Measures Better`)),
    readm_worse      = suppressWarnings(as.numeric(`Count of READM Measures Worse`)),
    mort_worse       = suppressWarnings(as.numeric(`Count of MORT Measures Worse`)),
    safety_worse     = suppressWarnings(as.numeric(`Count of Safety Measures Worse`)),
    # Simplified hospital type (3 categories instead of 6)
    hosp_type_simple = case_when(
      `Hospital Type` == "Acute Care Hospitals"    ~ "Acute Care",
      `Hospital Type` == "Critical Access Hospitals" ~ "Critical Access",
      TRUE                                           ~ "Other"
    ) %>% factor()
  ) %>%
  # Net readmission performance: positive = more measures better than worse
  mutate(readm_net = readm_better - readm_worse)

# --- SUPP: hospital-level penalty and financial data ---
names(supp) <- trimws(names(supp))

supp <- supp %>%
  mutate(
    `Hospital CNN`        = suppressWarnings(as.numeric(`Hospital CNN`)),
    dual_prop             = suppressWarnings(as.numeric(`Dual proportion`)),
    peer_group            = suppressWarnings(as.integer(`Peer group assignment`)),
    pay_adj_factor        = suppressWarnings(as.numeric(`Payment adjustment factor`)),
    payment_penalty       = 1 - pay_adj_factor,         # 0 = no penalty, positive = penalized
    penalty_binary        = if_else(pay_adj_factor < 1, 1L, 0L),
    penalty_pct           = suppressWarnings(as.numeric(`Payment reduction percentage`)),
    # Condition-specific ERRs for hospital-level analysis
    err_ami               = suppressWarnings(as.numeric(`ERR for AMI`)),
    err_hf                = suppressWarnings(as.numeric(`ERR for HF`)),
    err_copd              = suppressWarnings(as.numeric(`ERR for COPD`)),
    err_pn                = suppressWarnings(as.numeric(`ERR for pneumonia`)),
    err_cabg              = suppressWarnings(as.numeric(`ERR for CABG`)),
    err_hip_knee          = suppressWarnings(as.numeric(`ERR for THA/TKA`))
  ) %>%
  rename(`Facility ID` = `Hospital CNN`)


# -----------------------------------------------------------------------------
# 5. DISTRIBUTION PLOTS
# -----------------------------------------------------------------------------

ggplot(hrrp, aes(x = excess_ratio)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ condition, scales = "free_y") +
  labs(
    x     = "Excess Readmission Ratio",
    y     = "Count",
    title = "Distribution of Excess Readmission Ratios by Condition"
  ) +
  theme_minimal()

ggplot(gen %>% filter(!is.na(rating_num)), aes(x = factor(rating_num))) +
  geom_bar(fill = "steelblue") +
  labs(
    x     = "Hospital Overall Rating (Stars)",
    y     = "Number of Hospitals",
    title = "Distribution of Hospital Star Ratings",
    subtitle = paste0(scales::comma(sum(gen$is_rated == 0)), " hospitals have no rating (Not Available)")
  ) +
  theme_minimal()

ggplot(supp %>% filter(!is.na(dual_prop)), aes(x = dual_prop)) +
  geom_histogram(bins = 30, fill = "darkorange", color = "white") +
  labs(
    x     = "Dual-Eligible Proportion",
    y     = "Count",
    title = "Distribution of Dual-Eligible Patient Proportion Across Hospitals"
  ) +
  theme_minimal()

# Payment penalty distribution
ggplot(supp %>% filter(!is.na(payment_penalty)), aes(x = payment_penalty)) +
  geom_histogram(bins = 40, fill = "tomato", color = "white") +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    x     = "Payment Penalty (% reduction)",
    y     = "Count",
    title = "Distribution of CMS Payment Penalties",
    subtitle = paste0(scales::comma(sum(supp$penalty_binary == 1, na.rm = TRUE)),
                      " of ", scales::comma(sum(!is.na(supp$penalty_binary))),
                      " hospitals (78.6%) received a payment penalty")
  ) +
  theme_minimal()


# -----------------------------------------------------------------------------
# 6. MERGE DATA
# -----------------------------------------------------------------------------

data_all <- hrrp %>%
  left_join(
    gen %>%
      select(
        `Facility ID`, ownership = `Hospital Ownership`,
        hosp_type_simple, rating_num, has_er, is_rated,
        readm_net, readm_better, readm_worse, mort_worse, safety_worse
      ),
    by = "Facility ID"
  ) %>%
  left_join(
    supp %>%
      select(`Facility ID`, dual_prop, peer_group,
             payment_penalty, penalty_binary, penalty_pct),
    by = "Facility ID"
  )


# -----------------------------------------------------------------------------
# 7. BUILD ANALYSIS DATAFRAMES
# -----------------------------------------------------------------------------

# --- A) Condition-level analysis df (one row per hospital per condition) ---
analysis_df <- data_all %>%
  mutate(
    condition  = factor(condition),
    ownership  = factor(ownership),
    peer_group = factor(peer_group),
    log_excess = log(excess_ratio),             # log-transformed outcome
    state      = factor(State)
  ) %>%
  filter(
    !is.na(excess_ratio),
    !is.na(rating_num),
    !is.na(dual_prop)
  )

# --- B) Hospital-level df (one row per hospital, for penalty analysis) ---
hosp_df <- data_all %>%
  distinct(`Facility ID`, .keep_all = TRUE) %>%
  mutate(
    ownership        = factor(ownership),
    peer_group       = factor(peer_group),
    hosp_type_simple = factor(hosp_type_simple),
    state            = factor(State)
  ) %>%
  filter(!is.na(penalty_binary))


# -----------------------------------------------------------------------------
# 8. RELATIONSHIP PLOTS
# -----------------------------------------------------------------------------

# Excess ratio vs star rating by condition
ggplot(analysis_df, aes(x = factor(rating_num), y = excess_ratio)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.alpha = 0.3) +
  facet_wrap(~ condition) +
  labs(
    x     = "Hospital Star Rating",
    y     = "Excess Readmission Ratio",
    title = "Excess Readmission Ratio by Star Rating-Per Condition"
  ) +
  theme_minimal()

# Excess ratio vs dual proportion
ggplot(analysis_df, aes(x = dual_prop, y = excess_ratio)) +
  geom_point(alpha = 0.2, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "tomato") +
  facet_wrap(~ condition) +
  labs(
    x     = "Dual-Eligible Proportion",
    y     = "Excess Readmission Ratio",
    title = "Dual-Eligible Proportion vs Excess Readmission Ratio by Condition"
  ) +
  theme_minimal()

# Excess ratio vs ownership
ggplot(analysis_df, aes(x = reorder(ownership, excess_ratio, median, na.rm = TRUE),
                        y = excess_ratio)) +
  geom_boxplot(fill = "darkorchid", alpha = 0.7, outlier.alpha = 0.2) +
  coord_flip() +
  labs(
    x     = "Hospital Ownership",
    y     = "Excess Readmission Ratio",
    title = "Excess Readmission Ratio by Hospital Ownership Type"
  ) +
  theme_minimal()

# Readmission net score vs excess ratio
ggplot(analysis_df %>% filter(!is.na(readm_net)),
       aes(x = readm_net, y = excess_ratio)) +
  geom_point(alpha = 0.2, color = "darkorange") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    x     = "Readmission Net Performance Score (Better - Worse)",
    y     = "Excess Readmission Ratio",
    title = "Overall Readmission Quality vs Condition-Specific Excess Ratio",
    subtitle = "Negative score = more measures worse than better nationally"
  ) +
  theme_minimal()


# -----------------------------------------------------------------------------
# 9. BASELINE MODELS (original)
# -----------------------------------------------------------------------------

# Model 1-Full baseline
model_lm <- lm(
  excess_ratio ~ rating_num + has_er + dual_prop + peer_group + ownership + condition,
  data = analysis_df
)
summary(model_lm)
car::Anova(model_lm)

# Model 2-Refined baseline
model_lm2 <- lm(
  excess_ratio ~ rating_num + peer_group + ownership + condition,
  data = analysis_df
)
summary(model_lm2)
car::Anova(model_lm2)

# Coefficient plot-baseline
broom::tidy(model_lm2) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = reorder(term, estimate), y = estimate)) +
  geom_pointrange(
    aes(ymin = estimate - std.error, ymax = estimate + std.error),
    color = "steelblue"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(
    x     = "Model Terms",
    y     = "Coefficient Estimate",
    title = "Regression Coefficients-Refined Baseline (Model 2)"
  ) +
  theme_minimal()


# -----------------------------------------------------------------------------
# 10. IMPROVED MODELS
# -----------------------------------------------------------------------------

# Model 3-Add readm_net (hospital-level readmission quality composite)
# readm_net = Count(READM measures better) - Count(READM measures worse) nationally
model_lm3 <- lm(
  excess_ratio ~ rating_num + peer_group + ownership + condition + readm_net,
  data = analysis_df %>% filter(!is.na(readm_net))
)
summary(model_lm3)
car::Anova(model_lm3)

# Model 4-Add state fixed effects (geographic variation)
model_lm4 <- lm(
  excess_ratio ~ rating_num + peer_group + ownership + condition + state,
  data = analysis_df
)
summary(model_lm4)
car::Anova(model_lm4)

# Model 5-Combined: readm_net + state FE (best expected model)
model_lm5 <- lm(
  excess_ratio ~ rating_num + peer_group + ownership + condition + readm_net + state,
  data = analysis_df %>% filter(!is.na(readm_net))
)
summary(model_lm5)
car::Anova(model_lm5)

# Model 6-Log-transformed outcome (addresses right skew)
model_log <- lm(
  log_excess ~ rating_num + peer_group + ownership + condition,
  data = analysis_df
)
summary(model_log)

# Model 7-Interaction: does star rating effect vary by peer group?
model_interaction <- lm(
  excess_ratio ~ rating_num * peer_group + ownership + condition,
  data = analysis_df
)
summary(model_interaction)
car::Anova(model_interaction)


# -----------------------------------------------------------------------------
# 11. MODEL COMPARISON TABLE
# -----------------------------------------------------------------------------

model_comparison <- bind_rows(
  glance(model_lm)          %>% mutate(model = "Model 1: Full Baseline"),
  glance(model_lm2)         %>% mutate(model = "Model 2: Refined Baseline"),
  glance(model_lm3)         %>% mutate(model = "Model 3: + Readm Net"),
  glance(model_lm4)         %>% mutate(model = "Model 4: + State FE"),
  glance(model_lm5)         %>% mutate(model = "Model 5: Readm Net + State FE"),
  glance(model_log)         %>% mutate(model = "Model 6: Log Outcome"),
  glance(model_interaction) %>% mutate(model = "Model 7: Interaction")
) %>%
  select(model, r.squared, adj.r.squared, AIC, BIC, p.value) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

print(model_comparison)

# Visualise R-squared across models
model_comparison %>%
  ggplot(aes(x = reorder(model, adj.r.squared), y = adj.r.squared,
             fill = adj.r.squared)) +
  geom_col() +
  geom_text(aes(label = round(adj.r.squared, 3)), hjust = -0.1, size = 3.5) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  coord_flip() +
  labs(
    x     = NULL,
    y     = "Adjusted R-squared",
    title = "Model Comparison-Adjusted R-squared",
    fill  = "Adj R²"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# -----------------------------------------------------------------------------
# 12. CONDITION-SPECIFIC MODELS
# -----------------------------------------------------------------------------

condition_models <- analysis_df %>%
  filter(!is.na(readm_net)) %>%
  group_by(condition) %>%
  group_map(~ {
    fit <- lm(excess_ratio ~ rating_num + peer_group + ownership + readm_net,
              data = .x)
    glance(fit) %>% mutate(
      condition = as.character(unique(.x$condition)),
      n         = nrow(.x)
    )
  }, .keep = TRUE) %>%
  bind_rows() %>%
  select(condition, r.squared, adj.r.squared, AIC, p.value, n) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  arrange(desc(adj.r.squared))

print(condition_models)

# Condition-specific R-squared plot
ggplot(condition_models, aes(x = reorder(condition, adj.r.squared),
                             y = adj.r.squared, fill = adj.r.squared)) +
  geom_col() +
  geom_text(aes(label = paste0(round(adj.r.squared * 100, 1), "%")),
            hjust = -0.15, size = 3.5) +
  scale_fill_gradient(low = "lightsalmon", high = "tomato") +
  coord_flip() +
  labs(
    x     = "Condition",
    y     = "Adjusted R-squared",
    title = "Which Conditions Are Best Explained by Hospital Characteristics?",
    subtitle = "Model: Excess Ratio ~ Star Rating + Peer Group + Ownership + Readm Net"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# -----------------------------------------------------------------------------
# 13. PAYMENT PENALTY ANALYSIS (hospital-level)
# Outcome: Was the hospital penalized? (78.6% of hospitals in FY2025)
# -----------------------------------------------------------------------------

# --- 13a. Penalty rate by ownership ---
penalty_own <- hosp_df %>%
  filter(!is.na(ownership), !is.na(penalty_binary)) %>%
  group_by(ownership) %>%
  summarise(
    pct_penalized = mean(penalty_binary, na.rm = TRUE),
    mean_penalty  = mean(payment_penalty, na.rm = TRUE),
    n             = n(),
    .groups       = "drop"
  ) %>%
  filter(n >= 20)

ggplot(penalty_own,
       aes(x = reorder(ownership, pct_penalized), y = pct_penalized)) +
  geom_col(fill = "tomato") +
  geom_text(aes(label = paste0(round(pct_penalized * 100, 1), "%")),
            hjust = -0.1, size = 3.5) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.05)) +
  coord_flip() +
  labs(
    x     = "Hospital Ownership",
    y     = "% Penalized",
    title = "Penalty Rate by Hospital Ownership Type",
    subtitle = "FY 2025 HRRP-78.6% of hospitals received a payment reduction"
  ) +
  theme_minimal()

# --- 13b. Penalty rate by peer group ---
penalty_peer <- hosp_df %>%
  filter(!is.na(peer_group), !is.na(penalty_binary)) %>%
  group_by(peer_group) %>%
  summarise(
    pct_penalized = mean(penalty_binary, na.rm = TRUE),
    mean_penalty  = mean(payment_penalty, na.rm = TRUE),
    n             = n(),
    .groups       = "drop"
  )

ggplot(penalty_peer, aes(x = factor(peer_group), y = pct_penalized,
                         fill = mean_penalty)) +
  geom_col() +
  geom_text(aes(label = paste0(round(pct_penalized * 100, 1), "%")),
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.05)) +
  scale_fill_gradient(low = "lightyellow", high = "tomato",
                      labels = scales::percent_format()) +
  labs(
    x    = "Dual-Eligible Peer Group (1 = lowest dual proportion)",
    y    = "% Penalized",
    fill = "Mean Penalty",
    title = "Penalty Rate and Mean Penalty Amount by Peer Group",
    subtitle = "Peer groups stratify hospitals by dual-eligible patient share"
  ) +
  theme_minimal()

# --- 13c. Top 10 states by mean penalty ---
penalty_state <- hosp_df %>%
  filter(!is.na(state), !is.na(payment_penalty)) %>%
  group_by(state) %>%
  summarise(
    mean_penalty  = mean(payment_penalty, na.rm = TRUE),
    pct_penalized = mean(penalty_binary, na.rm = TRUE),
    n             = n(),
    .groups       = "drop"
  ) %>%
  filter(n >= 10) %>%
  arrange(desc(mean_penalty))

ggplot(penalty_state %>% slice_max(mean_penalty, n = 15),
       aes(x = reorder(state, mean_penalty), y = mean_penalty)) +
  geom_col(fill = "tomato") +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() +
  labs(
    x     = "State",
    y     = "Mean Payment Penalty",
    title = "Top 15 States by Mean HRRP Payment Penalty",
    subtitle = "States with ≥ 10 hospitals in FY 2025 HRRP data"
  ) +
  theme_minimal()

# --- 13d. Dual proportion vs penalty amount ---
ggplot(hosp_df %>% filter(!is.na(dual_prop), !is.na(payment_penalty)),
       aes(x = dual_prop, y = payment_penalty)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "tomato") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    x     = "Dual-Eligible Patient Proportion",
    y     = "Payment Penalty",
    title = "Do Hospitals with More Dual-Eligible Patients Face Higher Penalties?",
    subtitle = "Key equity concern: safety-net hospitals may be unfairly penalized"
  ) +
  theme_minimal()

# --- 13e. Logistic regression-what predicts being penalized? ---
model_penalty_logit <- glm(
  penalty_binary ~ peer_group + ownership + dual_prop,
  data   = hosp_df %>% filter(!is.na(peer_group), !is.na(ownership),
                              !is.na(dual_prop)),
  family = binomial
)
summary(model_penalty_logit)

# Odds ratios
broom::tidy(model_penalty_logit, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = reorder(term, estimate), y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  color = "steelblue") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  coord_flip() +
  labs(
    x     = "Predictor",
    y     = "Odds Ratio (exp[β])",
    title = "Logistic Regression-Predictors of Receiving a Payment Penalty",
    subtitle = "OR > 1 = higher odds of penalty; OR < 1 = lower odds"
  ) +
  theme_minimal()


# -----------------------------------------------------------------------------
# 14. SUMMARY TABLES
# -----------------------------------------------------------------------------

# Mean excess ratio by star rating
star_summary <- analysis_df %>%
  group_by(rating_num) %>%
  summarise(
    mean_excess = mean(excess_ratio),
    sd_excess   = sd(excess_ratio),
    n           = n(),
    .groups     = "drop"
  )
print(star_summary)

ggplot(star_summary, aes(x = factor(rating_num), y = mean_excess)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(
    aes(ymin = mean_excess - sd_excess, ymax = mean_excess + sd_excess),
    width = 0.2
  ) +
  labs(
    x     = "Hospital Overall Rating (Stars)",
    y     = "Mean Excess Readmission Ratio",
    title = "Mean Excess Readmission Ratio by Star Rating"
  ) +
  theme_minimal()

# Mean excess ratio by peer group
peer_summary <- analysis_df %>%
  group_by(peer_group) %>%
  summarise(
    mean_excess = mean(excess_ratio),
    sd_excess   = sd(excess_ratio),
    n           = n(),
    .groups     = "drop"
  )
print(peer_summary)

ggplot(peer_summary, aes(x = factor(peer_group), y = mean_excess)) +
  geom_col(fill = "darkorange") +
  geom_errorbar(
    aes(ymin = mean_excess - sd_excess, ymax = mean_excess + sd_excess),
    width = 0.2
  ) +
  labs(
    x     = "Dual-Eligible Peer Group",
    y     = "Mean Excess Readmission Ratio",
    title = "Mean Excess Readmission Ratio by Peer Group"
  ) +
  theme_minimal()

# Mean excess ratio by ownership
own_summary <- analysis_df %>%
  group_by(ownership) %>%
  summarise(
    mean_excess = mean(excess_ratio),
    sd_excess   = sd(excess_ratio),
    n           = n(),
    .groups     = "drop"
  )
print(own_summary)

ggplot(own_summary, aes(x = reorder(ownership, mean_excess), y = mean_excess)) +
  geom_point(size = 3, color = "purple") +
  geom_errorbar(
    aes(ymin = mean_excess - sd_excess, ymax = mean_excess + sd_excess),
    width = 0.15, color = "purple"
  ) +
  coord_flip() +
  labs(
    x     = "Hospital Ownership",
    y     = "Mean Excess Readmission Ratio",
    title = "Mean Excess Readmission Ratio by Ownership Type"
  ) +
  theme_minimal()


# -----------------------------------------------------------------------------
# 15. DIAGNOSTIC PLOTS (baseline vs best model)
# -----------------------------------------------------------------------------

analysis_df <- analysis_df %>%
  mutate(
    fitted_base = fitted(model_lm2),
    resid_base  = resid(model_lm2),
    fitted_best = fitted(model_lm4),
    resid_best  = resid(model_lm4)
  )

# Residuals vs Fitted - Baseline
ggplot(analysis_df,
       aes(x = fitted_base, y = resid_base, color = factor(rating_num))) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_hline(yintercept = 0, linewidth = 0.6, linetype = "dashed") +
  scale_color_brewer(palette = "Set2") +
  labs(
    x = "Fitted Values", y = "Residuals", color = "Star Rating",
    title = "Residuals vs Fitted-Baseline Model (Model 2)"
  ) +
  theme_minimal()

# Residuals vs Fitted-State FE model
ggplot(analysis_df,
       aes(x = fitted_best, y = resid_best, color = factor(rating_num))) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_hline(yintercept = 0, linewidth = 0.6, linetype = "dashed") +
  scale_color_brewer(palette = "Set2") +
  labs(
    x = "Fitted Values", y = "Residuals", color = "Star Rating",
    title = "Residuals vs Fitted-State Fixed Effects Model (Model 4)"
  ) +
  theme_minimal()

# Observed vs Fitted-State FE model
ggplot(analysis_df,
       aes(x = fitted_best, y = excess_ratio, color = factor(peer_group))) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.7) +
  scale_color_brewer(palette = "Set1") +
  labs(
    x = "Fitted Values", y = "Observed Excess Readmission Ratio",
    color = "Peer Group",
    title = "Observed vs Fitted-State Fixed Effects Model (Model 4)"
  ) +
  theme_minimal()