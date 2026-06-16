# Shared setup for the cue-energy manuscript.
# Sourced by cue-energy.qmd so that all data, models, and the objects referenced in
# the prose, figures, and tables come from a single source of truth. The article
# chunks (in manuscript/*.qmd) handle only plotting/rendering of these objects.
# Paths are project-relative; Quarto runs with execute-dir: project (see _quarto.yaml).

library(tidyverse)
library(survey)
library(broom)       # tidy() for the cue-model coefficient table
library(patchwork)   # stack the two cue-model coefficient plots
library(car)         # linearHypothesis() for the DiD tests
library(flextable)   # DiD table rendered natively for html/pdf/docx

# Weighted survey data
cueEnergyData <- read.csv("data/cueEnergyDataWeighted.csv")

# Survey design
design <- svydesign(ids = ~1, weights = ~weight, data = cueEnergyData)

# --- Experimental conditions: cell counts and randomization balance ----------
# Condition labels from the three treatment dummies. These objects feed the
# methods prose (inline cell counts) and the balance table (tbl-balance).
cueEnergyData <- cueEnergyData |>
  mutate(condition = case_when(
    trump.cue   == 1 ~ "Trump",
    climate.cue == 1 ~ "Climate",
    control     == 1 ~ "Control"
  ) |> factor(levels = c("Control", "Climate", "Trump")))

# Unweighted Ns per condition (randomization is about raw assignment, not weights)
n_by_condition <- as.list(table(cueEnergyData$condition))
n_total   <- nrow(cueEnergyData)
n_control <- n_by_condition$Control
n_climate <- n_by_condition$Climate
n_trump   <- n_by_condition$Trump

# Covariates for the balance check: demographics first, then the pre-treatment
# partisan moderators (showing these are balanced guards against the worry that
# the political-beliefs grouping is post-treatment).
balance_vars <- tibble::tribble(
  ~var,          ~label,                     ~type,
  "age",         "Age (years)",              "cont",
  "male",        "Male",                     "bin",
  "white",       "White (non-Hispanic)",     "bin",
  "edu",         "Education (1–8)",     "cont",
  "inc",         "Income (1–11)",       "cont",
  "college",     "College graduate",         "bin",
  "democrat",    "Democrat",                 "bin",
  "republican",  "Republican",               "bin",
  "libDem",      "Liberal Democrat",         "bin",
  "conRep",      "Conservative Republican",  "bin"
)

balance_table <- balance_vars |>
  mutate(stats = pmap(list(var, type), function(v, type) {
    x  <- cueEnergyData[[v]]
    mu <- tapply(x, cueEnergyData$condition, mean, na.rm = TRUE)
    p  <- if (type == "bin") {
      suppressWarnings(chisq.test(table(cueEnergyData$condition, x))$p.value)
    } else {
      anova(lm(x ~ cueEnergyData$condition))$`Pr(>F)`[1]
    }
    tibble(Control = mu[["Control"]], Climate = mu[["Climate"]],
           Trump = mu[["Trump"]], p = p)
  })) |>
  tidyr::unnest(stats) |>
  transmute(
    Variable = label,
    Control  = ifelse(type == "bin", sprintf("%.1f%%", 100 * Control), sprintf("%.2f", Control)),
    Climate  = ifelse(type == "bin", sprintf("%.1f%%", 100 * Climate), sprintf("%.2f", Climate)),
    Trump    = ifelse(type == "bin", sprintf("%.1f%%", 100 * Trump),   sprintf("%.2f", Trump)),
    `p-value` = sprintf("%.3f", p)
  )

# Shared renderer so the article (pdf/docx/html) shows a single native table.
make_balance_flextable <- function() {
  note_text <- paste(
    "Cell entries are unweighted condition means: percentages for binary",
    "indicators and means for continuous measures. The p-value tests for",
    "differences across the three conditions (chi-square for indicators,",
    "one-way ANOVA for continuous measures); non-significant values indicate",
    "the covariate is balanced across conditions, as expected under random",
    "assignment."
  )
  balance_table |>
    flextable() |>
    align(j = 1,   align = "left",   part = "all") |>
    align(j = 2:5, align = "center", part = "all") |>
    bold(part = "header") |>
    fontsize(size = 9, part = "all") |>
    padding(padding = 3, part = "all") |>
    add_footer_lines(values = paste("Note:", note_text)) |>
    fontsize(size = 8, part = "footer") |>
    italic(part = "footer") |>
    set_table_properties(layout = "fixed") |>
    width(j = 1,   width = 2.2) |>
    width(j = 2:5, width = 0.95)
}

# --- Cue models: agreement with the cue questions by political beliefs -------
cuemodel1 <- svyglm(renewables.opinion ~ conRep + libDem, design = design,
                    subset = (trump.cue == 1))   # Trump cue
cuemodel2 <- svyglm(renewables.opinion ~ conRep + libDem, design = design,
                    subset = (climate.cue == 1)) # Climate cue

# Tidy coefficients for the cue-model coefficient plot (fig-cueModels)
coefs <- bind_rows(
  tidy(cuemodel1, conf.int = TRUE) |> mutate(model = "Trump Cue"),
  tidy(cuemodel2, conf.int = TRUE) |> mutate(model = "Climate Cue")
) |>
  filter(term != "(Intercept)") |>
  mutate(term = dplyr::recode(term,
                              "conRep" = "Conservative\nRepublicans",
                              "libDem" = "Liberal\nDemocrats"))

# --- Preferred energy mix models ---------------------------------------------
# Treatment and party factors used by the preferred-mix models.
survey_df <- cueEnergyData |>
  mutate(
    treatment = case_when(
      trump.cue   == 1 ~ "Trump",
      climate.cue == 1 ~ "Climate",
      control     == 1 ~ "Control"
    ) |> factor(levels = c("Control", "Climate", "Trump")),

    party = case_when(
      libDem == 1 ~ "Liberal Democrats",
      conRep == 1 ~ "Conservative Republicans"
    ) |> factor(levels = c("Conservative Republicans", "Liberal Democrats"))
  )

svy_design <- svydesign(ids = ~1, weights = ~weight, data = survey_df)
svy_design_partisans <- subset(svy_design, !is.na(party))

sources       <- c("alloc.fossil", "alloc.wind", "alloc.solar", "alloc.hydro", "alloc.nuclear")
source_labels <- c("Fossil Fuels", "Wind", "Solar", "Hydro", "Nuclear")

models <- map(sources, function(s) {
  f <- as.formula(paste(s, "~ party * treatment"))
  svyglm(f, design = svy_design_partisans)
}) |>
  set_names(source_labels)

# --- Predicted preferred mix by party x treatment ----------------------------
newdata <- expand_grid(
  party     = c("Liberal Democrats", "Conservative Republicans"),
  treatment = c("Control", "Climate", "Trump"),
  source    = c("Fossil Fuels", "Wind", "Solar", "Hydro", "Nuclear")
) |>
  mutate(
    party     = factor(party, levels = c("Conservative Republicans", "Liberal Democrats")),
    treatment = factor(treatment, levels = c("Control", "Climate", "Trump"))
  )

pred_df <- imap_dfr(models, function(mod, source_name) {
  nd <- newdata |> filter(source == source_name)

  # svyglm predictions with SEs; CIs built manually on design df
  p        <- predict(mod, newdata = nd, se.fit = TRUE, type = "response")
  fit_vals <- as.numeric(p)
  se_vals  <- SE(p)

  df_design <- degf(mod$survey.design)
  crit      <- qt(0.975, df = df_design)

  nd |>
    mutate(
      fit = fit_vals,
      se  = se_vals,
      lwr = fit - crit * se,
      upr = fit + crit * se
    )
})

# Factor ordering for plots and prose indexing
pred_df <- pred_df |>
  mutate(
    treatment = factor(treatment, levels = c("Control", "Climate", "Trump")),
    source    = factor(source, levels = c("Fossil Fuels", "Nuclear", "Hydro", "Wind", "Solar")),
    party     = factor(party, levels = c("Liberal Democrats", "Conservative Republicans"))
  )

# --- Preferred change from the current mix shown to respondents ---------------
# Current US electricity mix shown to respondents (percent of total).
existing_mix <- tibble(
  source   = c("Fossil Fuels", "Wind", "Solar", "Hydro", "Nuclear"),
  existing = c(58, 10, 7, 5, 18)
)

change_df <- pred_df |>
  left_join(existing_mix, by = "source") |>
  mutate(
    change_fit = fit - existing,
    change_lwr = lwr - existing,
    change_upr = upr - existing,
    treatment  = factor(treatment, levels = c("Control", "Climate", "Trump")),
    source     = factor(source, levels = c("Fossil Fuels", "Nuclear", "Hydro", "Wind", "Solar")),
    party      = factor(party, levels = c("Liberal Democrats", "Conservative Republicans"))
  )

# --- Partisan gap (Dem - Rep) in preferred mix, with CIs ---------------------
get_gap_ci_svy <- function(mod, treatment_levels) {
  nd <- expand_grid(
    party     = factor(c("Liberal Democrats", "Conservative Republicans"),
                       levels = c("Conservative Republicans", "Liberal Democrats")),
    treatment = factor(treatment_levels, levels = treatment_levels)
  )

  X         <- model.matrix(delete.response(terms(mod)), data = nd)
  df_design <- degf(mod$survey.design)
  crit      <- qt(0.975, df = df_design)

  map_dfr(treatment_levels, function(t) {
    x_dem    <- X[nd$party == "Liberal Democrats" & nd$treatment == t, ]
    x_rep    <- X[nd$party == "Conservative Republicans" & nd$treatment == t, ]
    contrast <- x_dem - x_rep

    est <- as.numeric(contrast %*% coef(mod))
    se  <- sqrt(as.numeric(contrast %*% vcov(mod) %*% contrast))

    tibble(
      treatment = t,
      gap       = est,
      se        = se,
      lwr       = est - crit * se,
      upr       = est + crit * se
    )
  })
}

gap_df <- imap_dfr(models, function(mod, source_name) {
  get_gap_ci_svy(mod, treatment_levels = c("Control", "Climate", "Trump")) |>
    mutate(source = source_name)
}) |>
  mutate(
    treatment = factor(treatment, levels = c("Control", "Climate", "Trump")),
    source    = factor(source, levels = c("Fossil Fuels", "Nuclear", "Hydro", "Wind", "Solar")),
    direction = if_else(gap > 0, "Dem", "Rep")
  )

gap_sorted <- gap_df |>
  mutate(
    label     = paste0(source, " — ", treatment),
    label     = fct_reorder(label, abs(gap)),
    direction = if_else(gap > 0, "Dem", "Rep")
  )

gap_ranked <- gap_sorted |> arrange(desc(abs(gap)))

# --- Pairwise contrasts behind fig-pred --------------------------------------
# Design-based tests of the differences a reader sees in fig-pred. For each
# energy source: (a) the partisan gap (Dem - Rep) within each condition, and
# (b) the within-party shift across conditions (each cue vs control, and Trump
# vs climate). Estimates, SEs, and p-values use the survey design df.
pred_contrasts <- imap_dfr(models, function(mod, source_name) {
  nd <- expand_grid(
    party     = factor(c("Conservative Republicans", "Liberal Democrats"),
                       levels = c("Conservative Republicans", "Liberal Democrats")),
    treatment = factor(c("Control", "Climate", "Trump"),
                       levels = c("Control", "Climate", "Trump"))
  )
  X         <- model.matrix(delete.response(terms(mod)), data = nd)
  df_design <- degf(mod$survey.design)

  row_of <- function(p, t) X[nd$party == p & nd$treatment == t, ]

  test_contrast <- function(name, type, cvec) {
    est <- as.numeric(cvec %*% coef(mod))
    se  <- sqrt(as.numeric(cvec %*% vcov(mod) %*% cvec))
    tstat <- est / se
    tibble(source = source_name, type = type, contrast = name,
           estimate = est, se = se, t = tstat,
           p_value = 2 * pt(-abs(tstat), df = df_design))
  }

  D <- "Liberal Democrats"; R <- "Conservative Republicans"
  bind_rows(
    # (a) partisan gap (Dem - Rep) within each condition
    test_contrast("Gap (Dem - Rep): Control", "Partisan gap",
                  row_of(D, "Control") - row_of(R, "Control")),
    test_contrast("Gap (Dem - Rep): Climate", "Partisan gap",
                  row_of(D, "Climate") - row_of(R, "Climate")),
    test_contrast("Gap (Dem - Rep): Trump", "Partisan gap",
                  row_of(D, "Trump") - row_of(R, "Trump")),
    # (b) within-party shift across conditions
    test_contrast("Republicans: Climate - Control", "Within-party",
                  row_of(R, "Climate") - row_of(R, "Control")),
    test_contrast("Republicans: Trump - Control", "Within-party",
                  row_of(R, "Trump") - row_of(R, "Control")),
    test_contrast("Republicans: Trump - Climate", "Within-party",
                  row_of(R, "Trump") - row_of(R, "Climate")),
    test_contrast("Democrats: Climate - Control", "Within-party",
                  row_of(D, "Climate") - row_of(D, "Control")),
    test_contrast("Democrats: Trump - Control", "Within-party",
                  row_of(D, "Trump") - row_of(D, "Control")),
    test_contrast("Democrats: Trump - Climate", "Within-party",
                  row_of(D, "Trump") - row_of(D, "Climate"))
  )
}) |>
  mutate(
    source = factor(source, levels = source_labels),
    stars  = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.10  ~ "†",
      TRUE            ~ ""
    )
  )

# Inline helper: estimate + significance for a given source/contrast.
# e.g. pred_diff("Fossil Fuels", "Republicans: Trump - Control")
pred_diff <- function(src, con, digits = 1, abs = FALSE) {
  row <- pred_contrasts[pred_contrasts$source == src & pred_contrasts$contrast == con, ]
  est <- if (abs) base::abs(row$estimate) else row$estimate
  sprintf("%.*f percentage points (p %s)", digits, est,
          ifelse(row$p_value < 0.001, "< 0.001", paste("=", sprintf("%.3f", row$p_value))))
}

# Wide display table behind fig-pred (one column per source) + shared renderer.
pred_contrasts_order <- c(
  "Gap (Dem - Rep): Control",
  "Gap (Dem - Rep): Climate",
  "Gap (Dem - Rep): Trump",
  "Republicans: Climate - Control",
  "Republicans: Trump - Control",
  "Republicans: Trump - Climate",
  "Democrats: Climate - Control",
  "Democrats: Trump - Control",
  "Democrats: Trump - Climate"
)

pred_contrasts_table <- pred_contrasts |>
  mutate(cell = sprintf("%.1f%s", estimate, stars)) |>
  select(contrast, source, cell) |>
  pivot_wider(names_from = source, values_from = cell) |>
  mutate(contrast = factor(contrast, levels = pred_contrasts_order)) |>
  arrange(contrast)

make_pred_contrasts_flextable <- function() {
  note_text <- paste(
    "Cell entries are estimated differences in the predicted preferred share",
    "(percentage points) underlying Figure 2. Partisan-gap rows are the",
    "Liberal Democrats − Conservative Republicans difference within each",
    "condition; within-party rows are the change for that party when moving",
    "from one condition to another (positive = the later condition is higher).",
    "Estimates use survey weights; standard errors and p-values are design-based.",
    "*** p < 0.001, ** p < 0.01, * p < 0.05, † p < 0.10."
  )
  pred_contrasts_table |>
    rename(Comparison = contrast) |>
    select(Comparison, `Fossil Fuels`, Wind, Solar, Hydro, Nuclear) |>
    flextable() |>
    align(j = 1,   align = "left",   part = "all") |>
    align(j = 2:6, align = "center", part = "all") |>
    bold(part = "header") |>
    fontsize(size = 8, part = "all") |>
    padding(padding = 3, part = "all") |>
    add_footer_lines(values = paste("Note:", note_text)) |>
    fontsize(size = 7, part = "footer") |>
    italic(part = "footer") |>
    set_table_properties(layout = "fixed") |>
    width(j = 1,   width = 2.0) |>
    width(j = 2:6, width = 0.86)
}

# --- Difference-in-Differences: do treatments shift the partisan gap? --------
did_combined <- imap_dfr(models, function(mod, source_name) {
  cf <- coef(mod)
  vc <- vcov(mod)

  contrasts <- list(
    list(
      name = "Climate Cue vs Control",
      est  = cf["partyLiberal Democrats:treatmentClimate"],
      se   = sqrt(vc["partyLiberal Democrats:treatmentClimate",
                     "partyLiberal Democrats:treatmentClimate"]),
      hyp  = "partyLiberal Democrats:treatmentClimate = 0"
    ),
    list(
      name = "Trump Cue vs Control",
      est  = cf["partyLiberal Democrats:treatmentTrump"],
      se   = sqrt(vc["partyLiberal Democrats:treatmentTrump",
                     "partyLiberal Democrats:treatmentTrump"]),
      hyp  = "partyLiberal Democrats:treatmentTrump = 0"
    ),
    list(
      name = "Trump Cue vs Climate",
      est  = cf["partyLiberal Democrats:treatmentTrump"] -
             cf["partyLiberal Democrats:treatmentClimate"],
      se   = sqrt(
        vc["partyLiberal Democrats:treatmentTrump",
           "partyLiberal Democrats:treatmentTrump"] +
        vc["partyLiberal Democrats:treatmentClimate",
           "partyLiberal Democrats:treatmentClimate"] -
        2 * vc["partyLiberal Democrats:treatmentTrump",
               "partyLiberal Democrats:treatmentClimate"]
      ),
      hyp  = "partyLiberal Democrats:treatmentTrump - partyLiberal Democrats:treatmentClimate = 0"
    )
  )

  map_dfr(contrasts, function(con) {
    test <- linearHypothesis(mod, con$hyp, test = "F")
    tibble(
      source   = source_name,
      contrast = con$name,
      estimate = as.numeric(con$est),
      se       = as.numeric(con$se),
      lwr      = estimate - 1.96 * se,
      upr      = estimate + 1.96 * se,
      F        = test$F[2],
      df       = test$Df[2],
      p_value  = test$`Pr(>F)`[2]
    )
  })
})

did_omnibus <- imap_dfr(models, function(mod, source_name) {
  omnibus <- linearHypothesis(mod, c(
    "partyLiberal Democrats:treatmentClimate = 0",
    "partyLiberal Democrats:treatmentTrump = 0"
  ), test = "F")

  tibble(
    source  = source_name,
    F       = omnibus$F[2],
    df1     = omnibus$Df[2],
    df2     = omnibus$Res.Df[2],
    p_value = omnibus$`Pr(>F)`[2]
  )
})

# Helper for inline reporting of DiD shifts in the manuscript text.
# Returns the estimated change in the partisan gap (percentage points) with its
# 95% CI for a given energy source and contrast, e.g.
#   did_shift("Fossil Fuels", "Trump Cue vs Control")
did_shift <- function(src, con, digits = 1) {
  row <- did_combined[did_combined$source == src & did_combined$contrast == con, ]
  sprintf("%.*f percentage points", digits, row$estimate)
}

# Display table (omnibus row + per-contrast rows, one column per source)
did_table <- bind_rows(
  did_omnibus |>
    mutate(
      stars = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01  ~ "**",
        p_value < 0.05  ~ "*",
        p_value < 0.10  ~ "†",
        TRUE             ~ ""
      ),
      contrast = "Omnibus F-test (any treatment effect on gap)",
      cell     = sprintf("F = %.2f%s", F, stars)
    ) |>
    select(source, contrast, cell),

  did_combined |>
    mutate(
      stars = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01  ~ "**",
        p_value < 0.05  ~ "*",
        p_value < 0.10  ~ "†",
        TRUE             ~ ""
      ),
      cell = sprintf("%.2f%s [%.2f, %.2f]", estimate, stars, lwr, upr)
    ) |>
    select(source, contrast, cell)
) |>
  pivot_wider(names_from = source, values_from = cell)

# Shared renderer so the article (pdf/docx/html) and the notebook show the same
# table; flextable renders natively in each output format when run in-document.
make_did_flextable <- function() {
  note_text <- paste(
    "Top row shows the omnibus F-test for whether treatments jointly shift",
    "the partisan gap on each energy source. Subsequent rows show the estimated",
    "change in the partisan gap (Liberal Democrats − Conservative Republicans)",
    "when moving from one treatment condition to another, with 95% confidence",
    "intervals in brackets. Positive values indicate the gap widened. Estimates",
    "from svyglm models fit separately by energy source, using survey weights;",
    "standard errors and confidence intervals are design-based.",
    "*** p < 0.001, ** p < 0.01, * p < 0.05, † p < 0.10."
  )
  did_table |>
    rename(Contrast = contrast) |>
    select(Contrast, `Fossil Fuels`, Wind, Solar, Hydro, Nuclear) |>
    flextable() |>
    align(j = 1,   align = "left",   part = "all") |>
    align(j = 2:6, align = "center", part = "all") |>
    bold(part = "header") |>
    fontsize(size = 8, part = "all") |>
    padding(padding = 3, part = "all") |>
    add_footer_lines(values = paste("Note:", note_text)) |>
    fontsize(size = 7, part = "footer") |>
    italic(part = "footer") |>
    # Fixed layout with explicit widths (inches) summing to < text width so the
    # table fits the page in PDF; long cells wrap rather than overflow the margin.
    set_table_properties(layout = "fixed") |>
    width(j = 1,   width = 1.7) |>
    width(j = 2:6, width = 0.92)
}

# --- Robustness: DiD with demographic controls -------------------------------
# Re-estimate the preferred-mix models adding pre-treatment covariates as
# additive controls. The party x treatment interactions (the DiD estimands) are
# unchanged in name, so the same contrast logic applies. This confirms the cue
# effects on the partisan gap are not driven by the chance covariate imbalances
# (age, race) noted in the balance check. Education enters as the continuous
# `edu` measure; `college` is omitted to avoid redundancy with it.
controls_rhs <- "age + male + white + edu + inc"

models_controls <- map(sources, function(s) {
  f <- as.formula(paste(s, "~ party * treatment +", controls_rhs))
  svyglm(f, design = svy_design_partisans)
}) |>
  set_names(source_labels)

# Reusable DiD-table builder (mirrors the main did_table construction) so the
# baseline and controlled specifications are computed identically.
compute_did_table <- function(model_list) {
  did_c <- imap_dfr(model_list, function(mod, source_name) {
    cf <- coef(mod)
    vc <- vcov(mod)

    contrasts <- list(
      list(
        name = "Climate Cue vs Control",
        est  = cf["partyLiberal Democrats:treatmentClimate"],
        hyp  = "partyLiberal Democrats:treatmentClimate = 0"
      ),
      list(
        name = "Trump Cue vs Control",
        est  = cf["partyLiberal Democrats:treatmentTrump"],
        hyp  = "partyLiberal Democrats:treatmentTrump = 0"
      ),
      list(
        name = "Trump Cue vs Climate",
        est  = cf["partyLiberal Democrats:treatmentTrump"] -
               cf["partyLiberal Democrats:treatmentClimate"],
        hyp  = "partyLiberal Democrats:treatmentTrump - partyLiberal Democrats:treatmentClimate = 0"
      )
    )

    map_dfr(contrasts, function(con) {
      test <- linearHypothesis(mod, con$hyp, test = "F")
      se   <- if (grepl("-", con$hyp)) {
        sqrt(
          vc["partyLiberal Democrats:treatmentTrump",
             "partyLiberal Democrats:treatmentTrump"] +
          vc["partyLiberal Democrats:treatmentClimate",
             "partyLiberal Democrats:treatmentClimate"] -
          2 * vc["partyLiberal Democrats:treatmentTrump",
                 "partyLiberal Democrats:treatmentClimate"]
        )
      } else {
        nm <- sub(" = 0", "", con$hyp)
        sqrt(vc[nm, nm])
      }
      tibble(
        source   = source_name,
        contrast = con$name,
        estimate = as.numeric(con$est),
        se       = as.numeric(se),
        lwr      = estimate - 1.96 * se,
        upr      = estimate + 1.96 * se,
        p_value  = test$`Pr(>F)`[2]
      )
    })
  })

  omni_c <- imap_dfr(model_list, function(mod, source_name) {
    omnibus <- linearHypothesis(mod, c(
      "partyLiberal Democrats:treatmentClimate = 0",
      "partyLiberal Democrats:treatmentTrump = 0"
    ), test = "F")
    tibble(source = source_name, F = omnibus$F[2], p_value = omnibus$`Pr(>F)`[2])
  })

  stars <- function(p) case_when(
    p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", p < 0.10 ~ "†", TRUE ~ ""
  )

  bind_rows(
    omni_c |>
      mutate(contrast = "Omnibus F-test (any treatment effect on gap)",
             cell = sprintf("F = %.2f%s", F, stars(p_value))) |>
      select(source, contrast, cell),
    did_c |>
      mutate(cell = sprintf("%.2f%s [%.2f, %.2f]", estimate, stars(p_value), lwr, upr)) |>
      select(source, contrast, cell)
  ) |>
    pivot_wider(names_from = source, values_from = cell)
}

did_table_controls <- compute_did_table(models_controls)

make_did_controls_flextable <- function() {
  note_text <- paste(
    "Replicates the difference-in-differences table from the main text, adding",
    "age, sex, race, education, and income as additive controls to each svyglm",
    "model. The top row shows the omnibus F-test for whether treatments jointly",
    "shift the partisan gap on each energy source; subsequent rows show the",
    "estimated change in the partisan gap (Liberal Democrats − Conservative",
    "Republicans) when moving from one condition to another, with 95% confidence",
    "intervals in brackets. Positive values indicate the gap widened. Estimates",
    "use survey weights; standard errors and confidence intervals are design-based.",
    "*** p < 0.001, ** p < 0.01, * p < 0.05, † p < 0.10."
  )
  did_table_controls |>
    rename(Contrast = contrast) |>
    select(Contrast, `Fossil Fuels`, Wind, Solar, Hydro, Nuclear) |>
    flextable() |>
    align(j = 1,   align = "left",   part = "all") |>
    align(j = 2:6, align = "center", part = "all") |>
    bold(part = "header") |>
    fontsize(size = 8, part = "all") |>
    padding(padding = 3, part = "all") |>
    add_footer_lines(values = paste("Note:", note_text)) |>
    fontsize(size = 7, part = "footer") |>
    italic(part = "footer") |>
    set_table_properties(layout = "fixed") |>
    width(j = 1,   width = 1.7) |>
    width(j = 2:6, width = 0.92)
}
