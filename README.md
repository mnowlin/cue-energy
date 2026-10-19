# Party or Climate? The Polarization of the US Public's Energy Preferences

Manuscript and reproducible analysis examining how partisan and climate change
cues influence the US public's support for different energy sources. The hypotheses were preregistered using the Open Science Framework (OSF): https://osf.io/s2ku8/overview?view_only=8d073e8b77f44768a27184b9b0aff785

The analysis uses survey-weighted regression models and reproduces all tables
and figures reported in the manuscript.

## Abstract

In the US, support for renewable energy among Republicans has declined in recent years, while support for fossil fuels has grown. This paper uses a preregistered survey experiment (N = 3,113) to test two possible explanations for this shift, including cues linking renewables to climate change and a cue about Trump's energy views. Respondents were given the current US energy mix for producing electricity across five sources (fossil fuels, wind, solar, hydropower, and nuclear), and then randomly assigned to a Trump cue, a climate cue, or a control condition before indicating their preferred energy mix. Conservative Republicans with the Trump cue favored more fossil fuels and fewer renewables, but were not responsive to the climate cue. Liberal Democrats were largely unaffected by either cue. However, all groups preferred a mix with fewer fossil fuels and more renewables than the current US supply, regardless of treatment condition.

See [`abstract.md`](abstract.md).

## Layout

```
cue-energy.qmd               Manuscript source (renders to HTML, PDF, DOCX)
cue-energy-supplemental.qmd  Supplemental materials (renders to HTML, PDF, DOCX)
abstract.md                  Manuscript abstract
_quarto.yaml                 Quarto project config
_output/                     Rendered outputs (HTML, PDF, DOCX)
scripts/
  manuscript-setup.R         Sourced by the .qmd files: cleans data, fits models,
                             builds figures and tables (no side effects)
  export-cited-refs.R        Pre-render step: trims the master .bib to cited keys
  draft-analysis.R           Early exploratory analysis (superseded by manuscript-setup.R)
  survey_weighting_diagnostic.R  Diagnostics for survey weighting
  make-datasets.R            Builds data/cueEnergyData*.csv (recodes + raking weights)
                             from the raw survey export (run from project root)
  revision-analysis.R        R&R analyses: party-based models, robustness checks
  revision-figures.R         R&R consolidated results figure
data/                        Survey data + codebook (NOT in git -- see below)
figure/                      Exported figure PNGs
```

## Reproducing the analysis

Package versions are recorded with `renv`. Run `renv::restore()` to install them.

- **Full manuscript:** `quarto render` → outputs to `_output/`
  (HTML, PDF, and DOCX for both the manuscript and supplemental materials)

## Data

The `data/` folder is **not tracked in git**. Restore it before rendering:

- `data/raw-data.csv` — raw survey export (contains IP/location fields; never commit)
- `data/cueEnergyData.csv` — processed survey responses
- `data/cueEnergyDataWeighted.csv` — survey-weighted data
- `data/cue-energy-survey.docx` — survey instrument

## Notes

- `references.bib` and `american-political-science-association.csl` are used at
  render time by the pre-render step; the trimmed `references.bib` is generated
  from the master bibliography, so keep the master `.bib` up to date.
- `scripts/draft-analysis.R` holds early exploratory code and is superseded by
  `scripts/manuscript-setup.R`.
