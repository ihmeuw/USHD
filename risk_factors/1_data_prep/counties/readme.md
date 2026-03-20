---
editor_options: 
  markdown: 
    wrap: 72
---

# Extraction code for risk factor data -- in `survey_data` directory

## BRFSS

### Scripts

-   `prep_brfss.r` - the main script that loads in, standardizes, and
    compiles the BRFSS files.

-   `gen_brfss_mcnty_rake_wts.r` - mored to `/archive` because we are
    implementing a new method to get survey weights.

-   `match_general_smart_brfss.R` - script that creates crosswalk
    between unique IDs in general BRFSS and SMART BRFSS files. Used to
    attach the CBSA/MD geographic information from SMART onto the full
    set of variables provided in general BRFSS.

-   `brfss_state_cnty_crosswalk.R` - create crosswalk between general
    BRFSS unique IDs and county information provided by individual
    states.

-   `vet_brfss.R` - Creates diagnostic plots and tabulations to check
    the extracted BRFSS metadata.

-   `map_data_coverage.R` - produce maps, based on the compiled dataset,
    that show the counties and metropolitan areas for which we have
    un-supressed county or CBSA/MSA identifiers (the rest is
    state-remainder data). Remember that the file
    `FILEPATH`
    exists, and is used to create the cnty/mcnty/CBSA crosswalk and the
    CBSA shapefiles used in this script.

-   `brfss_extraction_prep/` - scripts used to set up the
    `brfss_files_and_variables.csv` that guides the extractions
    -   `update_extraction_metadata.R` - Used to fill in the
        `brfss_files_and_variables.csv` spreadsheet, which contains a
        row for every file we want to extract and a column for each
        variable of interest (cells contain the code for that value in
        each file). Checks that each code in the files_and_variables
        spreadsheet is present in the corresponding files, and will fill
        in variables that seem to fit the category but we did not
        manually provide a variable code for.

    -   `_functions.R` - helper functions called in
        `update_extraction_metadata.R`

    -   `combine_versions_metadata.R` - reads in and combines versions
        of the files_and_variables spreadsheets used in previous
        extraction efforts. This script does not need to be rerun.

    -   `update_brfss_files.R` - used to make a clean list of all the
        BRFSS files we want to extract, including public use BRFSS
        files, LU national BRFSS files, SMART BRFSS, and state files.
        This script does not need to be rerun.

### Files

-   `brfss_files_and_variables.csv` -- This spreadsheet is the guide for
    the extraction performed in `prep_brfss.r`. Spreadsheet contains a
    row for every file we want to extract and a column for each variable
    of interest (cells contain the code for that value in each file).
    This file can be updated manually, or with the
    `update_extraction_metadata.R` script in
    `FILEPATH`
    (recommended to run script after updating because has important
    checks).

-   `physical_activity_met.csv` -- crosswalk between the number and type
    of activity respondents reported in BRFSS. Used for recoding the met
    physical activity variables. (read in by `prep_brfss.r`). This may
    need to be updated for future BRFSS rounds as new activities are
    added. MET values can be retrieved from the BRFSS documentation
    (such as the SAS code they provide for calculating activity levels)

## NHANES

-   `prep_nhanes.r` -- Compiles all NHANES microdata and recodes
    variables for consistency across years, including adjusting for
    changes in measurement equipment.
-   `nhanes_files_and_variables.csv` -- contains paths to each of the
    NHANES files and corresponding variables to extract

## NHIS

-   `prep_nhis.r` -- Compiles all NHIS microdata (1997-2018 version,
    2019+ version, and 2014 NHOPI sample) and recode variables for
    consistency across years/forms.

-   `nhis_files_and_variables.csv file_list.csv` -- contains paths to
    each of the NHIS files and corresponding variables to extract

## Gallup

-   `prep_gallup.r` -- Compiles all Gallup Well-Being Microdata and
    recodes the variables for consistency across years/forms.

-   `gallup_files_and_variables.csv file_list.csv` -- contains paths to
    each of the Gallup files and corresponding variables to extract.

## CPS

-   `prep_cps.r`

-   does not need a files and vars spreadsheet

## General

-   `vetting_plots.R` -- a script sourced in the non-BRFSS prep scripts
    that automatically makes diagnostic plots in the output directory.

-   `_launch_extractions.R` -- launch script for each of the
    extractions.

-   `_functions` -- contains utility functions useful for reading in and
    checking the files in the various prep scripts.