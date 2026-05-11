# =============================================================================
# Survey Weighting Diagnostic Script (with tidycensus targets)
# For opt-in panels with continuous age + binary indicators
#
# Variables expected in your data:
#   age         - continuous (years)
#   white       - 1 = white (non-Hispanic), 0 = otherwise
#   male        - 1 = male,  0 = female
#   college     - 1 = bachelor's or higher, 0 = less than BA
#   dem         - 1 = Democrat (incl. leaners), 0 = otherwise
#   rep         - 1 = Republican (incl. leaners), 0 = otherwise
#                 (independents are dem == 0 & rep == 0)
#
# SETUP:
#   1. Free Census API key: https://api.census.gov/data/key_signup.html
#   2. Run once: tidycensus::census_api_key("YOUR_KEY", install = TRUE)
#      then restart R.
# =============================================================================

# ---- 0. Setup ---------------------------------------------------------------
# install.packages(c("anesrake", "survey", "dplyr", "tidyr", "tidycensus"))
library(anesrake)
library(survey)
library(dplyr)
library(tidyr)
library(tidycensus)

#census_api_key("0d973d7729111e2f509c106a9df3d4dee3d390c8", install = TRUE)

ACS_YEAR   <- 2023      # most recent 1-year ACS available
ACS_SURVEY <- "acs1"    # "acs1" or "acs5"
GEOGRAPHY  <- "us"      # or "state" with state = "TX", etc.


# ---- 1. Pull ACS targets ----------------------------------------------------
# If a variable ID below errors out for your ACS_YEAR, run
#   View(load_variables(ACS_YEAR, ACS_SURVEY))
# to find the current ID.

# ----- 1a. Age x sex, adults 18+ (Table B01001) -----
# Male age bands 18+:   B01001_007 through _025
# Female age bands 18+: B01001_031 through _049
male_vars <- sprintf("B01001_%03d",  7:25)
fem_vars  <- sprintf("B01001_%03d", 31:49)

age_sex_raw <- get_acs(
  geography = GEOGRAPHY,
  variables = c(male_vars, fem_vars),
  year      = ACS_YEAR,
  survey    = ACS_SURVEY
)

# Ordered band labels for the 19 male and 19 female variables
bands <- c("18-19","20","21","22-24","25-29","30-34","35-39","40-44",
           "45-49","50-54","55-59","60-61","62-64","65-66","67-69",
           "70-74","75-79","80-84","85+")

var_lookup <- c(setNames(rep("Male",   length(male_vars)), male_vars),
                setNames(rep("Female", length(fem_vars)),  fem_vars))
band_lookup <- c(setNames(bands, male_vars),
                 setNames(bands, fem_vars))

age_sex <- age_sex_raw %>%
  mutate(sex  = var_lookup[variable],
         band = band_lookup[variable]) %>%
  select(sex, band, estimate)

# Collapse to 4 age categories
band_to_cat <- c(
  "18-19"="18-29","20"="18-29","21"="18-29","22-24"="18-29","25-29"="18-29",
  "30-34"="30-44","35-39"="30-44","40-44"="30-44",
  "45-49"="45-64","50-54"="45-64","55-59"="45-64",
  "60-61"="45-64","62-64"="45-64",
  "65-66"="65+","67-69"="65+","70-74"="65+",
  "75-79"="65+","80-84"="65+","85+"="65+"
)

age_sex <- age_sex %>%
  dplyr::mutate(age_cat = band_to_cat[band]) %>%
  dplyr::group_by(sex, age_cat) %>%
  dplyr::summarize(n = sum(estimate), .groups = "drop")

total_adult <- sum(age_sex$n)

age_target <- age_sex %>%
  dplyr::group_by(age_cat) %>%
  dplyr::summarize(p = sum(n) / total_adult, .groups = "drop")
age_target_vec <- setNames(age_target$p, age_target$age_cat)
age_target_vec <- age_target_vec[c("18-29","30-44","45-64","65+")]

sex_target <- age_sex %>%
  dplyr::group_by(sex) %>%
  dplyr::summarize(p = sum(n) / total_adult, .groups = "drop")
male_target_vec <- c(
  "0" = sex_target$p[sex_target$sex == "Female"],
  "1" = sex_target$p[sex_target$sex == "Male"]
)


# ----- 1b. White non-Hispanic, adults 18+ (Table B01001H) -----
# Adult bands in B01001H: male _007-_016, female _022-_031
wnh_male <- sprintf("B01001H_%03d",  7:16)
wnh_fem  <- sprintf("B01001H_%03d", 22:31)

wnh_raw <- get_acs(
  geography = GEOGRAPHY,
  variables = c(wnh_male, wnh_fem),
  year      = ACS_YEAR,
  survey    = ACS_SURVEY
)
wnh_total <- sum(wnh_raw$estimate)

white_target_vec <- c(
  "0" = 1 - wnh_total / total_adult,
  "1" =     wnh_total / total_adult
)


# ----- 1c. College education, adults 25+ (Table B15002) -----
# College+ = Bachelor's, Master's, Professional, Doctorate
# Male:   _015, _016, _017, _018  (total male 25+ = _002)
# Female: _032, _033, _034, _035  (total female 25+ = _019)
educ_vars <- c("B15002_015","B15002_016","B15002_017","B15002_018",
               "B15002_032","B15002_033","B15002_034","B15002_035",
               "B15002_002","B15002_019")

educ_raw <- get_acs(
  geography = GEOGRAPHY,
  variables = educ_vars,
  year      = ACS_YEAR,
  survey    = ACS_SURVEY
)

educ_wide <- educ_raw %>%
  select(variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)

college_plus <- with(educ_wide,
                     B15002_015 + B15002_016 + B15002_017 + B15002_018 +
                     B15002_032 + B15002_033 + B15002_034 + B15002_035)
total_25plus <- with(educ_wide, B15002_002 + B15002_019)

# NOTE: ACS reports educational attainment only for 25+. The college rate
# below is the share of adults 25+. If your survey includes 18-24 year-olds,
# you have two reasonable choices:
#   (a) apply this 25+ rate to the whole sample (simpler; standard practice)
#   (b) compute a blended target using the assumption that 18-24 college
#       completion is much lower (~12-15%)
# Approach (a) is implemented here.
college_target_vec <- c(
  "0" = 1 - college_plus / total_25plus,
  "1" =     college_plus / total_25plus
)


# ----- 1d. Party ID — NOT FROM CENSUS -----
# Census doesn't measure partisanship. Use a documented external benchmark.
# Options in rough order of defensibility:
#   - Pew NPORS (annual ABS panel)
#   - ANES Time Series (election years)
#   - Rolling avg of Gallup quarterly party ID
# REPLACE these illustrative numbers with your chosen benchmark and cite it.
pid_target_vec <- c("Dem" = 0.45, "Rep" = 0.46, "Ind" = 0.09) # from PEW: https://www.pewresearch.org/politics/fact-sheet/party-affiliation-fact-sheet-npors/



# ----- 1e. Assemble targets -----
pop_targets <- list(
  age_cat = age_target_vec,
  white   = white_target_vec,
  male    = male_target_vec,
  college = college_target_vec,
  pid3    = pid_target_vec
)

cat("\n========================================================\n")
cat(" POPULATION TARGETS\n")
cat(" Source: ACS", ACS_YEAR, "(", ACS_SURVEY, "), geography =", GEOGRAPHY, "\n")
cat("========================================================\n")
cat("\nAge (18+):\n");              print(round(pop_targets$age_cat, 3))
cat("\nWhite non-Hispanic:\n");     print(round(pop_targets$white, 3))
cat("\nMale:\n");                   print(round(pop_targets$male, 3))
cat("\nCollege (BA+, 25+ rate):\n"); print(round(pop_targets$college, 3))
cat("\nParty ID (external):\n");    print(round(pop_targets$pid3, 3))


# ---- 2. Load survey data ----------------------------------------------------
survey <- read.csv("data/cueEnergyData.csv")

# Simulated opt-in panel for demonstration
#set.seed(42)
#n <- 1500
#survey <- data.frame(
#  age     = round(rnorm(n, mean = 52, sd = 16)),
#  white   = rbinom(n, 1, 0.66),
#  male    = rbinom(n, 1, 0.47),
#  college = rbinom(n, 1, 0.52),
#  dem     = rbinom(n, 1, 0.45)
#)
#survey$age <- pmin(pmax(survey$age, 18), 90)
#survey$rep <- ifelse(survey$dem == 1, 0, rbinom(n, 1, 0.45))


# ---- 3. Recode for weighting ------------------------------------------------
age_breaks <- c(18, 30, 45, 65, Inf)
age_labels <- c("18-29", "30-44", "45-64", "65+")

survey <- survey %>%
  mutate(
    age_cat = cut(age, breaks = age_breaks, labels = age_labels,
                  right = FALSE, include.lowest = TRUE),
    white_f   = factor(white,   levels = c(0, 1)),
    male_f    = factor(male,    levels = c(0, 1)),
    college_f = factor(college, levels = c(0, 1)),
    pid3 = factor(
      case_when(
        democrat == 1 ~ "Dem",
        republican == 1 ~ "Rep",
        TRUE     ~ "Ind"
      ),
      levels = c("Dem", "Rep", "Ind")
    )
  )

stopifnot(!any(is.na(survey$age_cat)),
          !any(is.na(survey$white_f)),
          !any(is.na(survey$male_f)),
          !any(is.na(survey$college_f)),
          !any(is.na(survey$pid3)))


# ---- 4. PRE-WEIGHT DIAGNOSTICS ----------------------------------------------
cat("\n========================================================\n")
cat(" SAMPLE vs POPULATION — BEFORE WEIGHTING\n")
cat("========================================================\n")

compare_marginals <- function(sample_var, target_vec, label) {
  sample_prop <- prop.table(table(sample_var))
  target_vec  <- target_vec[names(sample_prop)]
  out <- data.frame(
    Level      = names(sample_prop),
    Sample     = round(as.numeric(sample_prop), 3),
    Population = round(as.numeric(target_vec), 3),
    Diff_pp    = round(100 * (as.numeric(sample_prop) -
                              as.numeric(target_vec)), 1)
  )
  cat("\n--", label, "--\n")
  print(out, row.names = FALSE)
  invisible(out)
}

compare_marginals(survey$age_cat,   pop_targets$age_cat, "Age")
compare_marginals(survey$white_f,   pop_targets$white,   "White (1) vs non-white (0)")
compare_marginals(survey$male_f,    pop_targets$male,    "Male (1) vs female (0)")
compare_marginals(survey$college_f, pop_targets$college, "College+ (1) vs less (0)")
compare_marginals(survey$pid3,      pop_targets$pid3,    "Party ID")


# ---- 5. RAKE ----------------------------------------------------------------
rake_targets <- list(
  age_cat   = pop_targets$age_cat,
  white_f   = pop_targets$white,
  male_f    = pop_targets$male,
  college_f = pop_targets$college,
  pid3      = pop_targets$pid3
)
names(rake_targets$white_f)   <- c("0", "1")
names(rake_targets$male_f)    <- c("0", "1")
names(rake_targets$college_f) <- c("0", "1")

survey$caseid <- seq_len(nrow(survey))

raked <- anesrake(
  inputter     = rake_targets,
  dataframe    = survey,
  caseid       = survey$caseid,
  cap          = 5,
  choosemethod = "total",
  type         = "pctlim",
  pctlim       = 0.05,
  nlim         = 5,
  iterate      = TRUE,
  force1       = TRUE
)

survey$weight <- raked$weightvec


# ---- 6. POST-WEIGHT DIAGNOSTICS ---------------------------------------------
cat("\n========================================================\n")
cat(" SAMPLE vs POPULATION — AFTER RAKING\n")
cat("========================================================\n")

weighted_prop <- function(var, w) {
  tab <- tapply(w, var, sum)
  tab / sum(tab)
}

compare_weighted <- function(var, w, target_vec, label) {
  wp <- weighted_prop(var, w)
  target_vec <- target_vec[names(wp)]
  out <- data.frame(
    Level      = names(wp),
    Weighted   = round(as.numeric(wp), 3),
    Population = round(as.numeric(target_vec), 3),
    Diff_pp    = round(100 * (as.numeric(wp) -
                              as.numeric(target_vec)), 1)
  )
  cat("\n--", label, "--\n")
  print(out, row.names = FALSE)
  invisible(out)
}

compare_weighted(survey$age_cat,   survey$weight, pop_targets$age_cat, "Age")
compare_weighted(survey$white_f,   survey$weight, pop_targets$white,   "White")
compare_weighted(survey$male_f,    survey$weight, pop_targets$male,    "Male")
compare_weighted(survey$college_f, survey$weight, pop_targets$college, "College+")
compare_weighted(survey$pid3,      survey$weight, pop_targets$pid3,    "Party ID")


# ---- 7. WEIGHT DISTRIBUTION & EFFECTIVE N -----------------------------------
cat("\n========================================================\n")
cat(" WEIGHT DIAGNOSTICS\n")
cat("========================================================\n")

w <- survey$weight
ess  <- sum(w)^2 / sum(w^2)
deff <- nrow(survey) / ess

cat("\nWeight summary:\n");  print(summary(w))
cat(sprintf("\nSD of weights:        %.3f\n", sd(w)))
cat(sprintf("Min / Max:            %.3f / %.3f\n", min(w), max(w)))
cat(sprintf("Ratio max/min:        %.2f\n", max(w) / min(w)))
cat(sprintf("\nNominal N:            %d\n", nrow(survey)))
cat(sprintf("Effective N (Kish):   %.0f\n", ess))
cat(sprintf("ESS / N ratio:        %.2f\n", ess / nrow(survey)))
cat(sprintf("Design effect (DEFF): %.2f\n", deff))
cat(sprintf("MoE inflation:        %.2fx vs unweighted\n", sqrt(deff)))

if (ess / nrow(survey) < 0.5) {
  cat("\n*** WARNING: ESS/N < 0.5 — substantial precision loss.\n")
  cat("    Consider: (a) loosening party ID target, (b) tighter weight cap,\n")
  cat("    (c) vote recall instead of party ID, or (d) MRP.\n")
}


# ---- 8. JOINT CELL CHECK ----------------------------------------------------
cat("\n========================================================\n")
cat(" JOINT CHECK: College x Party ID\n")
cat(" (Raking matches MARGINS only — joint cells may still be off)\n")
cat("========================================================\n")

cat("\nUnweighted (% of sample):\n")
print(round(100 * prop.table(table(survey$college_f, survey$pid3)), 1))

cat("\nWeighted (% of weighted sample):\n")
joint_w <- tapply(survey$weight,
                  list(survey$college_f, survey$pid3),
                  sum)
print(round(100 * joint_w / sum(joint_w), 1))


# ---- 9. SURVEY DESIGN FOR ANALYSIS ------------------------------------------
design <- svydesign(ids = ~1, weights = ~weight, data = survey)

cat("\n========================================================\n")
cat(" Survey design 'design' ready for analysis.\n")
cat(" Use svymean(), svyglm(), svyby(), etc.\n")
cat("========================================================\n")



