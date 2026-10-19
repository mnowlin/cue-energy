# Party-based analyses for the R&R (Environmental Politics, FENP-2026-0506).
# Main groups are all Democrats and all Republicans (leaners included) rather
# than liberal Democrats / conservative Republicans (see reviews/revision-plan.md).
# Renewables = wind + solar + hydro. Objects defined here are intended to replace
# the ideology x party objects in scripts/manuscript-setup.R once the text is
# revised. Paths are project-relative.

library(tidyverse)
library(survey)

cueEnergyData <- read.csv("data/cueEnergyDataWeighted.csv")

# --- Analysis variables ------------------------------------------------------
survey_df <- cueEnergyData |>
  mutate(
    treatment = case_when(
      trump.cue   == 1 ~ "Trump",
      climate.cue == 1 ~ "Climate",
      control     == 1 ~ "Control"
    ) |> factor(levels = c("Control", "Climate", "Trump")),
    party = factor(party3, levels = c("Democrat", "Republican", "Independent")),
    pid.strength = factor(pid.strength, levels = c("Strong", "Weak", "Leaner", "Independent")),
    pid7.f      = factor(pid7),
    ideology.f  = factor(ideology),
    alloc.renew = alloc.wind + alloc.solar + alloc.hydro,
    wt.equal    = 1,
    # Pre-treatment Trump trust (0-10): about half the sample is at 0
    trust.trump = case_when(
      trust.president == 0 ~ "None (0)",
      trust.president <= 6 ~ "Some (1-6)",
      trust.president >= 7 ~ "High (7-10)"
    ) |> factor(levels = c("None (0)", "Some (1-6)", "High (7-10)")),
    # Speeders: under 30% of the median completion time
    speeder = time.taken.seconds < 0.3 * median(time.taken.seconds, na.rm = TRUE)
  )

outcomes <- c("Fossil Fuels" = "alloc.fossil", "Renewables" = "alloc.renew",
              "Nuclear" = "alloc.nuclear", "Wind" = "alloc.wind",
              "Solar" = "alloc.solar", "Hydro" = "alloc.hydro")

# Main design uses demographic-only weights (age, sex, race, college) because
# party ID was measured post-treatment; the original weights that also rake on
# party ID and the unweighted design are robustness checks
design_main  <- svydesign(ids = ~1, weights = ~weight.demo, data = survey_df)
design_party <- svydesign(ids = ~1, weights = ~weight,      data = survey_df)
design_unwt <- svydesign(ids = ~1, weights = ~wt.equal,    data = survey_df)

partisans <- function(des) subset(des, party %in% c("Democrat", "Republican"))

# --- Contrast helper ---------------------------------------------------------
# Linear contrast between two cells of a fitted svyglm, using the design df.
# `cells` is a data frame of covariate values; a and b index its rows.
cell_contrast <- function(mod, cells, a, b) {
  X    <- model.matrix(delete.response(terms(mod)), data = cells,
                       xlev = mod$xlevels)
  cvec <- X[a, ] - X[b, ]
  est  <- as.numeric(cvec %*% coef(mod))
  se   <- sqrt(as.numeric(cvec %*% vcov(mod) %*% cvec))
  df   <- degf(mod$survey.design)
  tibble(estimate = est, se = se,
         conf.low = est - qt(0.975, df) * se,
         conf.high = est + qt(0.975, df) * se,
         p.value = 2 * pt(-abs(est / se), df))
}

# Difference of two contrasts (e.g., change in the partisan gap)
diff_contrast <- function(mod, cells, a1, b1, a2, b2) {
  X    <- model.matrix(delete.response(terms(mod)), data = cells,
                       xlev = mod$xlevels)
  cvec <- (X[a1, ] - X[b1, ]) - (X[a2, ] - X[b2, ])
  est  <- as.numeric(cvec %*% coef(mod))
  se   <- sqrt(as.numeric(cvec %*% vcov(mod) %*% cvec))
  df   <- degf(mod$survey.design)
  tibble(estimate = est, se = se,
         conf.low = est - qt(0.975, df) * se,
         conf.high = est + qt(0.975, df) * se,
         p.value = 2 * pt(-abs(est / se), df))
}

# --- Main models: outcome ~ party * treatment --------------------------------
fit_party_models <- function(des) {
  map(outcomes, \(y) svyglm(as.formula(paste(y, "~ party * treatment")),
                            design = partisans(des)))
}

party_cells <- expand_grid(
  party     = factor(c("Democrat", "Republican"), levels = levels(survey_df$party)),
  treatment = factor(c("Control", "Climate", "Trump"), levels = levels(survey_df$treatment))
)
cell <- function(p, t) which(party_cells$party == p & party_cells$treatment == t)

# Within-party treatment effects (tests H1-H4), partisan gaps, and gap changes
party_effects <- function(models) {
  imap_dfr(models, function(mod, src) {
    within <- expand_grid(party = c("Democrat", "Republican"),
                          comparison = c("Climate - Control", "Trump - Control", "Trump - Climate")) |>
      mutate(res = map2(party, comparison, \(p, cmp) {
        trts <- str_split_1(cmp, " - ")
        cell_contrast(mod, party_cells, cell(p, trts[1]), cell(p, trts[2]))
      })) |>
      unnest(res) |>
      mutate(type = "Within-party effect")

    gaps <- tibble(party = "Rep - Dem", comparison = c("Control", "Climate", "Trump")) |>
      mutate(res = map(comparison, \(t) cell_contrast(mod, party_cells,
                                                      cell("Republican", t), cell("Democrat", t)))) |>
      unnest(res) |>
      mutate(type = "Partisan gap")

    # Change in the Rep - Dem gap across conditions (party x treatment interaction)
    gap_change <- tibble(party = "Rep - Dem",
                         comparison = c("Climate - Control", "Trump - Control", "Trump - Climate")) |>
      mutate(res = map(comparison, \(cmp) {
        trts <- str_split_1(cmp, " - ")
        diff_contrast(mod, party_cells,
                      cell("Republican", trts[1]), cell("Democrat", trts[1]),
                      cell("Republican", trts[2]), cell("Democrat", trts[2]))
      })) |>
      unnest(res) |>
      mutate(type = "Change in partisan gap")

    bind_rows(within, gaps, gap_change) |> mutate(source = src)
  }) |>
    relocate(source, type)
}

models_main <- fit_party_models(design_main)
models_party <- fit_party_models(design_party)
models_unwt <- fit_party_models(design_unwt)

effects_main <- party_effects(models_main)
effects_party <- party_effects(models_party)
effects_unwt <- party_effects(models_unwt)

# Predicted mix by party x condition (for the consolidated results figure)
pred_party <- imap_dfr(models_main, function(mod, src) {
  p    <- predict(mod, newdata = party_cells, se.fit = TRUE)
  crit <- qt(0.975, degf(mod$survey.design))
  party_cells |>
    mutate(source = src, fit = as.numeric(p), se = SE(p),
           conf.low = fit - crit * se, conf.high = fit + crit * se)
})

# --- Heterogeneity by partisan strength --------------------------------------
# Within each party: outcome ~ treatment * pid.strength (strong / weak / leaner)
strength_cells <- expand_grid(
  treatment    = factor(c("Control", "Climate", "Trump"), levels = levels(survey_df$treatment)),
  pid.strength = factor(c("Strong", "Weak", "Leaner"), levels = levels(survey_df$pid.strength))
)

effects_strength <- expand_grid(party = c("Democrat", "Republican"),
                                source = c("Fossil Fuels", "Renewables", "Nuclear")) |>
  mutate(res = map2(party, source, function(p, src) {
    des <- subset(design_main, party == p & !is.na(pid.strength))
    mod <- svyglm(as.formula(paste(outcomes[[src]], "~ treatment * pid.strength")), design = des)
    expand_grid(pid.strength = c("Strong", "Weak", "Leaner"),
                comparison = c("Climate - Control", "Trump - Control")) |>
      mutate(res = map2(pid.strength, comparison, \(s, cmp) {
        trts <- str_split_1(cmp, " - ")
        a <- which(strength_cells$treatment == trts[1] & strength_cells$pid.strength == s)
        b <- which(strength_cells$treatment == trts[2] & strength_cells$pid.strength == s)
        cell_contrast(mod, strength_cells, a, b)
      })) |>
      unnest(res)
  })) |>
  unnest(res)

# --- Heterogeneity by pre-treatment Trump trust ------------------------------
# trust.president precedes the cues, so it is a pre-treatment moderator that is
# immune to the post-treatment concern about party ID (R3), and a rough proxy
# for prior exposure to / agreement with Trump's energy position (R2)
trust_cells <- expand_grid(
  treatment   = factor(c("Control", "Climate", "Trump"), levels = levels(survey_df$treatment)),
  trust.trump = factor(levels(survey_df$trust.trump), levels = levels(survey_df$trust.trump))
)

effects_trust <- tibble(source = c("Fossil Fuels", "Renewables", "Nuclear")) |>
  mutate(res = map(source, function(src) {
    mod <- svyglm(as.formula(paste(outcomes[[src]], "~ treatment * trust.trump")),
                  design = subset(design_main, !is.na(trust.trump)))
    expand_grid(trust.trump = levels(survey_df$trust.trump),
                comparison = c("Climate - Control", "Trump - Control")) |>
      mutate(res = map2(trust.trump, comparison, \(g, cmp) {
        trts <- str_split_1(cmp, " - ")
        a <- which(trust_cells$treatment == trts[1] & trust_cells$trust.trump == g)
        b <- which(trust_cells$treatment == trts[2] & trust_cells$trust.trump == g)
        cell_contrast(mod, trust_cells, a, b)
      })) |>
      unnest(res)
  })) |>
  unnest(res)

# --- Robustness: excluding speeders ------------------------------------------
effects_nospeed <- party_effects(fit_party_models(subset(design_main, !speeder)))

# --- Post-treatment checks: party ID and Trump approval by condition ---------
# Party ID, strength, and ideology were asked after the cues; if the cues moved
# them, their distributions would differ across randomized conditions
posttreat_balance <- tibble(
  variable = c("Party (D/R/I)", "Party x strength (7-point)", "Ideology (7-point)"),
  formula  = c("~ party + treatment", "~ pid7.f + treatment", "~ ideology.f + treatment")
) |>
  mutate(test = map(formula, \(f) svychisq(as.formula(f), design = design_unwt)),
         statistic = map_dbl(test, \(t) unname(t$statistic)),
         p.value   = map_dbl(test, \(t) t$p.value)) |>
  select(-test)

# Trump approval (1-7, post-treatment) by condition within party
approval_models <- map(c(Democrat = "Democrat", Republican = "Republican"), \(p)
  svyglm(trump.approval ~ treatment, design = subset(design_main, party == p)))
approval_by_condition <- imap_dfr(approval_models, \(mod, p)
  broom::tidy(mod, conf.int = TRUE) |> filter(term != "(Intercept)") |> mutate(party = p))

# Pre-treatment Trump trust should also be balanced (randomization check)
trust_balance <- svyglm(trust.president ~ treatment, design = design_unwt)

# --- Console summary when run directly ---------------------------------------
if (sys.nframe() == 0) {
  fmt <- function(df) df |> mutate(across(where(is.numeric), \(x) round(x, 3)))
  cat("\n== Main effects (demographic weights, all partisans) ==\n")
  print(fmt(filter(effects_main, source %in% c("Fossil Fuels", "Renewables", "Nuclear"))) |> select(-se), n = Inf)
  cat("\n== Within-party effects: weights raked on demographics + party ID ==\n")
  print(fmt(filter(effects_party, type == "Within-party effect", source %in% c("Fossil Fuels", "Renewables", "Nuclear"))) |> select(-se), n = Inf)
  cat("\n== Within-party effects: unweighted ==\n")
  print(fmt(filter(effects_unwt, type == "Within-party effect", source %in% c("Fossil Fuels", "Renewables", "Nuclear"))) |> select(-se), n = Inf)
  cat("\n== Within-party effects: excluding speeders ==\n")
  print(fmt(filter(effects_nospeed, type == "Within-party effect", source %in% c("Fossil Fuels", "Renewables", "Nuclear"))) |> select(-se), n = Inf)
  cat("\n== Effects by partisan strength ==\n")
  print(fmt(effects_strength) |> select(-se), n = Inf)
  cat("\n== Effects by pre-treatment Trump trust ==\n")
  print(fmt(effects_trust) |> select(-se), n = Inf)
  cat("\n== Post-treatment balance (party ID, ideology) ==\n")
  print(fmt(posttreat_balance))
  cat("\n== Trump approval by condition (vs control) ==\n")
  print(fmt(approval_by_condition) |> select(party, term, estimate, p.value))
  cat("\n== Pre-treatment Trump trust by condition ==\n")
  print(fmt(broom::tidy(trust_balance)))
}
