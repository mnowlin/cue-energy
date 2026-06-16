# draft analysis and code 

## packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(purrr)
library(broom)
library(patchwork)
library(marginaleffects)
library(car)
library(kableExtra)

## data
cueEnergyData <- read.csv("data/cueEnergyData.csv")

## models and predictions - unweighted 
models <- list(
  ffmodelTrump <- lm(alloc.fossil ~ conRep + libDem, data = cueEnergyData, subset = (trump.cue == 1)),
  WindmodelTrump <- lm(alloc.wind ~ conRep + libDem, data = cueEnergyData, subset = (trump.cue == 1)),
  SolarmodelTrump <- lm(alloc.solar ~ conRep + libDem, data = cueEnergyData, subset = (trump.cue == 1)),
  HydromodelTrump <- lm(alloc.hydro ~ conRep + libDem, data = cueEnergyData, subset = (trump.cue == 1)),
  NuclearmodelTrump <- lm(alloc.nuclear ~ conRep + libDem, data = cueEnergyData, subset = (trump.cue == 1)),
  ffmodelClimate <- lm(alloc.fossil ~ conRep + libDem, data = cueEnergyData, subset = (climate.cue == 1)),
  WindmodelClimate <- lm(alloc.wind ~ conRep + libDem, data = cueEnergyData, subset = (climate.cue == 1)),
  SolarmodelClimate <- lm(alloc.solar ~ conRep + libDem, data = cueEnergyData, subset = (climate.cue == 1)),
  HydromodelClimate <- lm(alloc.hydro ~ conRep + libDem, data = cueEnergyData, subset = (climate.cue == 1)),
  NuclearmodelClimate <- lm(alloc.nuclear ~ conRep + libDem, data = cueEnergyData, subset = (climate.cue == 1)),
  ffmodelControl <- lm(alloc.fossil ~ conRep + libDem, data = cueEnergyData, subset = (control == 1)),
  WindmodelControl <- lm(alloc.wind ~ conRep + libDem, data = cueEnergyData, subset = (control == 1)),
  SolarmodelControl <- lm(alloc.solar ~ conRep + libDem, data = cueEnergyData, subset = (control == 1)),
  HydromodelControl <- lm(alloc.hydro ~ conRep + libDem, data = cueEnergyData, subset = (control == 1)),
  NuclearmodelControl <- lm(alloc.nuclear ~ conRep + libDem, data = cueEnergyData, subset = (control == 1))
)

new_data <- data.frame(
  conRep = c(0,1),
  libDem = c(1,0))

results <- map_dfr(models, ~ as.data.frame(predict(.x, newdata = new_data, interval = "confidence")), .id = "model")

graphData <- expand_grid(
  treatment = c("Trump","Climate","Control"),
  source    = c("Fossil Fuels", "Wind", "Solar", "Hydro", "Nuclear"),
  party     = c("Liberal Democrats", "Conservative Republicans")
)

pred_df <- bind_cols(graphData, as_tibble(results))

pred_df$fit[1]
pred_df$lwr[1]

gap_df <- pred_df |>
  select(party, treatment, source, fit) |>
  pivot_wider(names_from = party, values_from = fit) |>
  mutate(gap = `Liberal Democrats` - `Conservative Republicans`) |>
  mutate(
    treatment = factor(treatment, levels = c("Control", "Climate", "Trump")),
    source    = factor(source, levels = c("Fossil Fuels", "Nuclear", "Hydro", "Wind", "Solar")),
    direction = if_else(gap > 0, "Dem", "Rep")
  )

ggplot(gap_df, aes(x = treatment, y = gap)) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
  geom_segment(aes(xend = treatment, y = 0, yend = gap),
               color = "grey60", linewidth = 0.6) +
  geom_point(aes(color = direction), size = 3) +
  facet_wrap(~ source, nrow = 1) +
  scale_color_manual(
    values = c("Dem" = "#2166AC", "Rep" = "#B2182B"),
    labels = c("Dem" = "Democrats prefer more",
               "Rep" = "Republicans prefer more"),
    name = NULL
  ) +
  labs(
    x = NULL,
    y = "Predicted partisan gap (percentage points)\nLiberal Democrats − Conservative Republicans",
    title = "Partisan differences in preferred energy mix, by treatment",
    caption = "Points are predicted differences from OLS models fit separately by energy source."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )


#ggplot(pred_df, aes(x = treatment, y = fit, color = party, group = party)) +
#  geom_line(position = position_dodge(width = 0.3), linewidth = 0.6, alpha = 0.7) +
#  geom_pointrange(
#    aes(ymin = lwr, ymax = upr),
#    position = position_dodge(width = 0.3),
#    size = 0.6
#  ) +
#  facet_wrap(~ source, nrow = 1) +
#  scale_color_manual(values = c("Liberal Democrats" = "#2166AC",
#                                "Conservative Republicans" = "#B2182B")) +
#  labs(
#    x = NULL,
#    y = "Predicted share of energy mix (%)",
#    color = NULL,
#    title = "",
#    caption = "Points are predicted values from OLS; vertical bars are 95% confidence intervals."
#  ) +
#  theme_minimal(base_size = 12) +
#  theme(
#    legend.position = "bottom",
#    axis.text.x = element_text(size = 5, hjust = 0.5),
#    strip.text = element_text(face = "bold")
#  )

ggplot(change_df, aes(x = change_fit, y = treatment, color = party)) +
  geom_vline(xintercept = 0, color = "grey30", linewidth = 0.5) +
  geom_pointrange(
    aes(xmin = change_lwr, xmax = change_upr),
    position = position_dodge(width = 0.5),
    size = 0.5
  ) +
  facet_wrap(~ source, nrow = 1) +
  scale_color_manual(values = c("Liberal Democrats"        = "#2166AC",
                                "Conservative Republicans" = "#B2182B")) +
  labs(
    x = "",
    y = NULL,
    color = NULL,
    title = "How much do partisans want to shift the energy mix from the status quo?"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))
