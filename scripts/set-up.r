dir.create("data")
dir.create("scripts")
dir.create("output")
dir.create("manuscript")
dir.create("presentation")

## set-up manuscript folder 
project_folder <- "/Users/matthewnowlin/Library/CloudStorage/OneDrive-UTArlington/01-IN PROGRESS/Research/cue-energy"

file.copy("/Users/matthewnowlin/Library/CloudStorage/Dropbox/Projects/Manuscript-Files/template.qmd",
          to=project_folder, copy.mode = TRUE)

file.rename("/Users/matthewnowlin/Library/CloudStorage/OneDrive-UTArlington/01-IN PROGRESS/Research/cue-energy/template.qmd", 
            "/Users/matthewnowlin/Library/CloudStorage/OneDrive-UTArlington/01-IN PROGRESS/Research/cue-energy/cue-energy.qmd")

## find and replace cue-energy with project name