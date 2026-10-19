# make datasets: builds data/cueEnergyData*.csv from data/raw-data.csv
# run from the project root; raw data (with IP/location fields) stays in the gitignored data/ folder
## load package
library(dplyr)

## load data
energyPrefs <- read.csv("data/raw-data.csv")

## set FALSE to rebuild only the cue-energy datasets without overwriting the
## exports used by the sibling projects (cue-WTP, cue-actions, energy-actors-trust)
export_other_projects <- FALSE
names(energyPrefs)

## recodes
energyPrefs[energyPrefs == "0 (No trust)"] <- 0
energyPrefs[energyPrefs == "10 (Complete Trust)"] <- 10

energyPrefs[energyPrefs == "0 (Not at all concerned)"] <- 0
energyPrefs[energyPrefs == "10 (Extremely concerned)"] <- 10

energyPrefs[energyPrefs == "Strongly disagree"] <- 1
energyPrefs[energyPrefs == "Disagree"] <- 2
energyPrefs[energyPrefs == "Neutral"] <- 3
energyPrefs[energyPrefs == "Agree"] <- 4
energyPrefs[energyPrefs == "Strongly agree"] <- 5

energyPrefs[energyPrefs == "1 (Prioritize fossil fuels)"] <- 1
energyPrefs[energyPrefs == "4 (Prioritize fossil fuels and renewable sources equally)"] <- 4
energyPrefs[energyPrefs == "7 (Prioritize renewable sources)"] <- 7

energyPrefs[energyPrefs == "Strongly disapprove"] <- 1
energyPrefs[energyPrefs == "Disapprove"] <- 2
energyPrefs[energyPrefs == "Neither approve nor disapprove"] <- 3
energyPrefs[energyPrefs == "Approve"] <- 4
energyPrefs[energyPrefs == "Strongly approve"] <- 5

energyPrefs[energyPrefs == "Strongly liberal"] <- 1
energyPrefs[energyPrefs == "Liberal"] <- 2
energyPrefs[energyPrefs == "Slightly liberal"] <- 3
energyPrefs[energyPrefs == "Middle of the road"] <- 4
energyPrefs[energyPrefs == "Slightly conservative"] <- 5
energyPrefs[energyPrefs == "Conservative"] <- 6
energyPrefs[energyPrefs == "Strongly conservative"] <- 7

energyPrefs$democrat <- ifelse(energyPrefs$party.id == "Democratic Party" | energyPrefs$party.lean == "Democratic Party", 1, 0)
energyPrefs$republican <- ifelse(energyPrefs$party.id == "Republican Party (or GOP)" | energyPrefs$party.lean == "Republican Party (or GOP)", 1, 0)

energyPrefs$libDem <- ifelse(
  (energyPrefs$party.id == "Democratic Party" | energyPrefs$party.lean == "Democratic Party") & 
    energyPrefs$ideology %in% c(1, 2, 3),
  1, 0
)

energyPrefs$conRep <- ifelse(
  (energyPrefs$party.id == "Republican Party (or GOP)" | energyPrefs$party.lean == "Republican Party (or GOP)") & 
    energyPrefs$ideology %in% c(5, 6, 7),
  1, 0
)

## partisanship (party ID with leaners, strength, 7-point scale)
## party.id / party.strength / party.lean were asked post-treatment
energyPrefs$party3 <- case_when(
  energyPrefs$democrat == 1   ~ "Democrat",
  energyPrefs$republican == 1 ~ "Republican",
  TRUE                        ~ "Independent"
)

### strong = completely identify; weak = somewhat/slightly; leaner = independent/other who leans
energyPrefs$pid.strength <- case_when(
  energyPrefs$party.id %in% c("Democratic Party", "Republican Party (or GOP)") &
    energyPrefs$party.strength == "Completely" ~ "Strong",
  energyPrefs$party.id %in% c("Democratic Party", "Republican Party (or GOP)") &
    energyPrefs$party.strength %in% c("Somewhat", "Slightly") ~ "Weak",
  energyPrefs$party.lean %in% c("Democratic Party", "Republican Party (or GOP)") ~ "Leaner",
  energyPrefs$party3 == "Independent" ~ "Independent"
)

### 1 = strong Democrat ... 4 = pure independent ... 7 = strong Republican
energyPrefs$pid7 <- case_when(
  energyPrefs$party3 == "Democrat"   & energyPrefs$pid.strength == "Strong" ~ 1,
  energyPrefs$party3 == "Democrat"   & energyPrefs$pid.strength == "Weak"   ~ 2,
  energyPrefs$party3 == "Democrat"   & energyPrefs$pid.strength == "Leaner" ~ 3,
  energyPrefs$party3 == "Independent"                                      ~ 4,
  energyPrefs$party3 == "Republican" & energyPrefs$pid.strength == "Leaner" ~ 5,
  energyPrefs$party3 == "Republican" & energyPrefs$pid.strength == "Weak"   ~ 6,
  energyPrefs$party3 == "Republican" & energyPrefs$pid.strength == "Strong" ~ 7
)

energyPrefs$male <- ifelse(energyPrefs$sex == "Male", 1, 0)
energyPrefs$white <- ifelse(energyPrefs$race == "Caucasian or White", 1, 0)

energyPrefs$edu[energyPrefs$education == "High school"] <- 1 
energyPrefs$edu[energyPrefs$education == "Some college"] <- 2 
energyPrefs$edu[energyPrefs$education == "Trade/vocational/technical"] <- 3
energyPrefs$edu[energyPrefs$education == "Associates"] <- 4
energyPrefs$edu[energyPrefs$education == "Bachelors"] <- 5
energyPrefs$edu[energyPrefs$education == "Masters"] <- 6
energyPrefs$edu[energyPrefs$education == "Professional"] <- 7
energyPrefs$edu[energyPrefs$education == "Doctorate"] <- 8

energyPrefs$college <- ifelse(energyPrefs$edu >= 5, 1, 0)

energyPrefs$wtp.fossil[is.na(energyPrefs$wtp.fossil)] <- 0
energyPrefs$wtp.renewable[is.na(energyPrefs$wtp.renewable)] <- 0

energyPrefs$inc[energyPrefs$income == "Less than $19,999"] <- 1 
energyPrefs$inc[energyPrefs$income == "$20,000 to $39,999"] <- 2 
energyPrefs$inc[energyPrefs$income == "$40,000 to $59,999"] <- 3
energyPrefs$inc[energyPrefs$income == "$60,000 to $79,999"] <- 4
energyPrefs$inc[energyPrefs$income == "$80,000 to $99,999"] <- 5
energyPrefs$inc[energyPrefs$income == "$100,000 to $119,999"] <- 6
energyPrefs$inc[energyPrefs$income == "$120,000 to $139,999"] <- 7
energyPrefs$inc[energyPrefs$income == "$140,000 to $159,999"] <- 8
energyPrefs$inc[energyPrefs$income == "$160,000 to $179,999"] <- 9
energyPrefs$inc[energyPrefs$income == "$180,000 to $199,999"] <- 10
energyPrefs$inc[energyPrefs$income == "Above $200,000"] <- 11

energyPrefs <- energyPrefs %>%
  mutate(across(c(trust.epa,trust.national.labs,trust.nas,trust.doe,trust.state.officials,trust.utilities,
                  trust.env.groups,trust.local.officials,trust.university.scientists,trust.federal.scientists,
                  trust.state.scientists,trust.president,trust.congress,trust.local.news,trust.national.news,
                  concern.cost,concern.reliability,concern.pollution,concern.ai.demand,concern.loss,
                  renewables.opinion,fed.actions.intro,fed.action.tax.credits,fed.action.coal.keep.open,
                  fed.action.coal.leasing,fed.action.cancel.wind,fed.action.nuclear.licensing,priority.scale,
                  trump.approval,ideology,libDem,conRep,male,white,edu,college,wtp.fossil,wtp.renewable,income), as.numeric))

## survey weights
### weighting for age, race, gender, college education, and partisanship 

# ---- 0. Setup ---------------------------------------------------------------
# install.packages(c("anesrake", "survey", "dplyr", "tidyr", "tidycensus"))
library(anesrake)
library(survey)
library(tidyr)
library(tidycensus)

# Census API key is read from CENSUS_API_KEY in .Renviron (tidycensus::census_api_key(install = TRUE))

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
energyPrefsWeighted <- energyPrefs

# ---- 3. Recode for weighting ------------------------------------------------
age_breaks <- c(18, 30, 45, 65, Inf)
age_labels <- c("18-29", "30-44", "45-64", "65+")

energyPrefsWeighted <- energyPrefsWeighted %>%
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

stopifnot(!any(is.na(energyPrefsWeighted$age_cat)),
          !any(is.na(energyPrefsWeighted$white_f)),
          !any(is.na(energyPrefsWeighted$male_f)),
          !any(is.na(energyPrefsWeighted$college_f)),
          !any(is.na(energyPrefsWeighted$pid3)))


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

compare_marginals(energyPrefsWeighted$age_cat,   pop_targets$age_cat, "Age")
compare_marginals(energyPrefsWeighted$white_f,   pop_targets$white,   "White (1) vs non-white (0)")
compare_marginals(energyPrefsWeighted$male_f,    pop_targets$male,    "Male (1) vs female (0)")
compare_marginals(energyPrefsWeighted$college_f, pop_targets$college, "College+ (1) vs less (0)")
compare_marginals(energyPrefsWeighted$pid3,      pop_targets$pid3,    "Party ID")


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

energyPrefsWeighted$caseid <- seq_len(nrow(energyPrefsWeighted))

raked <- anesrake(
  inputter     = rake_targets,
  dataframe    = energyPrefsWeighted,
  caseid       = energyPrefsWeighted$caseid,
  cap          = 5,
  choosemethod = "total",
  type         = "pctlim",
  pctlim       = 0.05,
  nlim         = 5,
  iterate      = TRUE,
  force1       = TRUE
)

energyPrefsWeighted$weight <- raked$weightvec

# ---- 5b. RAKE: demographics only (no party ID) ------------------------------
# Party ID was measured post-treatment, so this weight is a robustness check
# that excludes it from the raking targets
raked_demo <- anesrake(
  inputter     = rake_targets[c("age_cat", "white_f", "male_f", "college_f")],
  dataframe    = energyPrefsWeighted,
  caseid       = energyPrefsWeighted$caseid,
  cap          = 5,
  choosemethod = "total",
  type         = "pctlim",
  pctlim       = 0.05,
  nlim         = 5,
  iterate      = TRUE,
  force1       = TRUE
)

energyPrefsWeighted$weight.demo <- raked_demo$weightvec


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

compare_weighted(energyPrefsWeighted$age_cat,   energyPrefsWeighted$weight, pop_targets$age_cat, "Age")
compare_weighted(energyPrefsWeighted$white_f,   energyPrefsWeighted$weight, pop_targets$white,   "White")
compare_weighted(energyPrefsWeighted$male_f,    energyPrefsWeighted$weight, pop_targets$male,    "Male")
compare_weighted(energyPrefsWeighted$college_f, energyPrefsWeighted$weight, pop_targets$college, "College+")
compare_weighted(energyPrefsWeighted$pid3,      energyPrefsWeighted$weight, pop_targets$pid3,    "Party ID")

# ---- 7. WEIGHT DISTRIBUTION & EFFECTIVE N -----------------------------------
cat("\n========================================================\n")
cat(" WEIGHT DIAGNOSTICS\n")
cat("========================================================\n")

w <- energyPrefsWeighted$weight
ess  <- sum(w)^2 / sum(w^2)
deff <- nrow(energyPrefsWeighted) / ess

cat("\nWeight summary:\n");  print(summary(w))
cat(sprintf("\nSD of weights:        %.3f\n", sd(w)))
cat(sprintf("Min / Max:            %.3f / %.3f\n", min(w), max(w)))
cat(sprintf("Ratio max/min:        %.2f\n", max(w) / min(w)))
cat(sprintf("\nNominal N:            %d\n", nrow(energyPrefsWeighted)))
cat(sprintf("Effective N (Kish):   %.0f\n", ess))
cat(sprintf("ESS / N ratio:        %.2f\n", ess / nrow(energyPrefsWeighted)))
cat(sprintf("Design effect (DEFF): %.2f\n", deff))
cat(sprintf("MoE inflation:        %.2fx vs unweighted\n", sqrt(deff)))

if (ess / nrow(energyPrefsWeighted) < 0.5) {
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
print(round(100 * prop.table(table(energyPrefsWeighted$college_f, energyPrefsWeighted$pid3)), 1))

cat("\nWeighted (% of weighted sample):\n")
joint_w <- tapply(energyPrefsWeighted$weight,
                  list(energyPrefsWeighted$college_f, energyPrefsWeighted$pid3),
                  sum)
print(round(100 * joint_w / sum(joint_w), 1))

## export data for different projects
names(energyPrefs)
names(energyPrefsWeighted)

## cue-energy: party3, pid.strength, pid7, ideology, the pre-treatment trust.president
## and concern items, and time.taken.seconds added for the R&R
cue_vars <- c("renewables.opinion","alloc.fossil","alloc.wind","alloc.solar","alloc.hydro","alloc.nuclear",
              "trump.approval","democrat","republican","libDem","conRep","age","male","white","edu","college","inc",
              "trump.cue","climate.cue","control",
              "party3","pid.strength","pid7","ideology","trust.president",
              "concern.cost","concern.reliability","concern.pollution","concern.ai.demand","concern.loss",
              "time.taken.seconds")

cueEnergyData <- energyPrefs[, cue_vars]
cueEnergyDataWeighted <- energyPrefsWeighted[, c(cue_vars, "weight", "weight.demo")]

write.csv(cueEnergyData, "data/cueEnergyData.csv", row.names = FALSE)
write.csv(cueEnergyDataWeighted, "data/cueEnergyDataWeighted.csv", row.names = FALSE)

if (export_other_projects) {

cueWTPDataWeighted <- energyPrefsWeighted[, c("priority.scale","wtp.fossil","wtp.renewable","concern.cost","trump.approval","democrat","republican","libDem","conRep","age","male","white","edu","college","inc","trump.cue","climate.cue","control","weight")]

write.csv(cueWTPDataWeighted, "../cue-WTP/cueWTPDataWeighted.csv", row.names = FALSE)

cueActionsDataWeighted <- energyPrefsWeighted[, c("fed.action.tax.credits","fed.action.coal.keep.open","fed.action.coal.leasing","fed.action.cancel.wind","fed.action.nuclear.licensing","trump.approval","democrat","republican","libDem","conRep","age","male","white","edu","college","inc","trump.cue","climate.cue","control","weight")]

write.csv(cueActionsDataWeighted, "../cue-actions/cueActionsDataWeighted.csv", row.names = FALSE)

energyActorsDataWeighted <- energyPrefsWeighted[, c("trust.epa","trust.national.labs","trust.nas","trust.doe","trust.state.officials","trust.utilities","trust.env.groups","trust.local.officials","trust.university.scientists","trust.federal.scientists","trust.state.scientists","trust.president","trust.congress","trust.local.news","trust.national.news","concern.cost","concern.reliability","concern.pollution","concern.ai.demand","concern.loss","trump.approval","democrat","republican","libDem","conRep","age","male","white","edu","college","inc","weight")]

write.csv(energyActorsDataWeighted, "../energy-actors-trust/energyActorsDataWeighted.csv", row.names = FALSE)

}
