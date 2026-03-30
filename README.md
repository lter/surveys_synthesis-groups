# Synthesis Working Group Survey-Processing Code

The code in this repository anonymizes and aggregates Qualtrics survey data on synthesis working group (SPARC and full) and generates reports of the same. **Survey data _is not_ shared in this repository.**

## Workflow Explanation

- `01_standardize-surveys/` - Loads raw surveys and standardizes them so later scripts can be more streamlined
    - Requirements: (1) Have pre-2020 survey variants downloaded manually from Box, (2) have an API key for Qualtrics for downloading post-2020 survey variants directly
- `02_demographic/` - Quality controls (QCs) demographic survey and creates all necessary graphs
    - Requirements: Successfully run _all_ scripts in `01_standardize-surveys/`
- `03_full` - QCs all three surveys administered to full groups and creates all necessary graphs. Also creates graphs for questions that are asked in multiple survey iterations to show cohort change over time
    - Requirements: Successfully run _all_ scripts in `01_standardize-surveys/`
- `04_sparc` - QCs SPARC survey data and creates necessary graphs
    - Requirements: Successfully run _all_ scripts in `01_standardize-surveys/`
- `05_upload-outputs/` - Upload either survey graphs or 
    - Requirements: (1) connect your Google Drive self with the `googledrive` R package (tutorial [here](https://lter.github.io/scicomp/tutorial_googledrive-pkg.html)), and (2) successfully run at least one script that produces a graph or render one of the report Quarto files

## Other Script Explanations

- `-setup.r` - Creates graphs needed by at least two other scripts
- `tools/` - Centralizes some QC / visualization functions used by at least two other scripts
- `report_<survey-name>.qmd`- Rendered reports for each of the surveys that include graphs produced by respective 'visualization' scripts
    - To save on computing, these files just embed graphs produced elsewhere, so you'll need to successfully run the respective visualization script before these report files will render
