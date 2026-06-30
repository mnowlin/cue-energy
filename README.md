# Party or Climate? The Polarization of the US Public's Energy Preferences

Manuscript and reproducible analysis examining how partisan and climate change
cues influence the US public's support for different energy sources. The hypotheses were preregistered using the Open Science Framework (OSF): https://osf.io/s2ku8/overview?view_only=8d073e8b77f44768a27184b9b0aff785

The analysis uses survey-weighted regression models and reproduces all tables
and figures reported in the manuscript.

## Layout

```
cue-energy.qmd               Manuscript source (renders to HTML, PDF, DOCX)
cue-energy-supplemental.qmd  Supplemental materials (renders to HTML, PDF, DOCX)
_quarto.yaml                 Quarto project config
_output/                     Rendered outputs (HTML, PDF, DOCX)
scripts/
  manuscript-setup.R         Sourced by the .qmd files: cleans data, fits models,
                             builds figures and tables (no side effects)
  export-cited-refs.R        Pre-render step: trims the master .bib to cited keys
  draft-analysis.R           Early exploratory analysis (superseded by manuscript-setup.R)
  survey_weighting_diagnostic.R  Diagnostics for survey weighting
data/                        Survey data + codebook (NOT in git -- see below)
figure/                      Exported figure PNGs
```

## Reproducing the analysis

Requires R with: `tidyverse`, `survey`, `broom`, `patchwork`, `car`, `flextable`.

- **Full manuscript:** `quarto render` → outputs to `_output/`
  (HTML, PDF, and DOCX for both the manuscript and supplemental materials)

## Data

The `data/` folder is **not tracked in git**. Restore it before rendering:

- `data/cueEnergyData.csv` — raw survey responses
- `data/cueEnergyDataWeighted.csv` — survey-weighted data
- `data/cue-energy-survey.docx` — survey instrument

## Notes

- `references.bib` and `american-political-science-association.csl` are used at
  render time by the pre-render step; the trimmed `references.bib` is generated
  from the master bibliography, so keep the master `.bib` up to date.
- `scripts/draft-analysis.R` holds early exploratory code and is superseded by
  `scripts/manuscript-setup.R`.
