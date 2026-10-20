# Consolidated results figure for the R&R (replaces fig-pred, fig-gap, and
# tbl-pred-contrasts). Within-party treatment effects on the preferred energy
# mix, one panel per outcome on a shared x-axis, with filled points marking
# p < .05 (R3: show significance of the hypothesis-relevant comparisons).
# Objects come from scripts/revision-analysis.R; the ggplot call is written so
# it can be moved into a cue-energy.qmd chunk once the results text is revised.

source("scripts/revision-analysis.R")

effects_plot_df <- effects_main |>
  filter(type == "Within-party effect",
         source %in% c("Fossil Fuels", "Renewables", "Nuclear")) |>
  mutate(
    source     = factor(source, levels = c("Fossil Fuels", "Renewables", "Nuclear")),
    party      = factor(paste0(party, "s"), levels = c("Democrats", "Republicans")),
    comparison = factor(comparison,
                        levels = rev(c("Trump - Control", "Climate - Control", "Trump - Climate")),
                        labels = rev(c("Trump cue vs. control", "Climate cue vs. control",
                                       "Trump cue vs. climate cue"))),
    sig        = if_else(p.value < 0.05, "p < .05", "n.s.")
  )

fig_effects <- ggplot(effects_plot_df,
                      aes(x = estimate, y = comparison, color = party)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), linewidth = 0.6,
                 position = position_dodge(width = 0.6)) +
  geom_point(aes(fill = interaction(party, sig)), shape = 21, size = 2.8, stroke = 0.9,
             position = position_dodge(width = 0.6)) +
  facet_wrap(~ source, nrow = 1) +
  scale_color_manual(values = c("Democrats" = "#2166AC", "Republicans" = "#B2182B"),
                     name = NULL) +
  scale_fill_manual(values = c("Democrats.p < .05"   = "#2166AC",
                               "Republicans.p < .05" = "#B2182B",
                               "Democrats.n.s."      = "white",
                               "Republicans.n.s."    = "white"),
                    guide = "none") +
  guides(color = guide_legend(reverse = TRUE)) +
  labs(
    x = "Difference in preferred share (percentage points)",
    y = NULL,
    caption = paste(
      "Points are within-party differences in the predicted preferred share from survey-weighted",
      "linear models (outcome ~ party × condition; weights for age, sex, race, education, and party ID);",
      "bars are 95% confidence intervals with design-based standard errors.",
      "Filled points: p < .05; hollow points: not significant.",
      "Renewables = wind + solar + hydropower. Democrats and Republicans include leaners.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position    = "bottom",
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text         = element_text(face = "bold"),
    plot.caption       = element_text(hjust = 0, size = 8)
  )

if (sys.nframe() == 0) {
  out <- commandArgs(trailingOnly = TRUE)
  ggsave(if (length(out)) out[1] else "fig-effects-preview.png",
         fig_effects, width = 8, height = 3.8, dpi = 300, bg = "white")
}
