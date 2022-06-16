## Generating a "settings.yaml" file
"settings.yaml" is a single source of truth and a replacement for .csv files for passing settings parameters. It exists in the root directory of the model. If it does not exist yet, you can generate using `write_consolidated_settings()` from `sae.shared` package. This function expects that:

* "settings.csv" and "submitted_cause_list.csv" files are present in the model root directory.
* Cause specific "settings.csv" files are present in cause sub-directories.

The following script loads the latest version of `sae.shared` package using `lbd.loader`. The general "setting.csv" file is first parsed to yaml format. Then settings in "submitted_cause_list.csv" are scanned and the new settings are added to the consolidated list. Finally the cause-specific "settings.csv" files are compared against the consolidated list and the new settings are added. The compiled list is written to the "settings.yaml" file in the model-root-directory.

Note that calling with `copy_j = TRUE`, copies files referenced in the settings, from FILEPATH to `{dir}/from_J_drive`. The new paths to the files are updated in the "settings.yaml". Metadata for this operation is written to `metadata_{year}_{month}_{day}_{hour}_{minute}.yaml` file in `from_J_drive`. This enables sbatch jobs e.g. raking and post-estimation to run on non-archive nodes.

```
# load "sae.shared" library with "lbd.loader"
library(lbd.loader, lib.loc = sprintf("FILEPATH]"", R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
lbd.loader::load_package('sae.shared')
# model dir
dir <- "FILEPATH"
# consolidate settings and write to settings.yaml file
write_consolidated_settings(dir, copy_J = TRUE)
```

Example of "settings.yaml" file:

```
settings:
  area_var: mcnty
  race_together: no
  edu_labels: Education 1
  edu_groups: 1.0
  n.sims: 1000.0
  covars_trans:
    income_median: log(x)
    pop_density: log(x)
  sexes:
  - 1.0
  - 2.0
  covars:
  - income_median
  - pop_density
  - edu_ba
  - poverty
  geoagg_files:
    natl: FILEPATH
    state: FILEPATH
  model_class: tmb_models
  by_edu: no
  shape_file: FILEPATH
  age_std_file: FILEPATH
  by_race: no
  LU_folder: FILEPATH
  raking_area_var: state
  race_labels: Race/Ethnicity 1
  races: 1.0
  all_pop_id: 1.0
  rake_to_gbd: yes
  covar_file: FILEPATH
  adjmat_file: FILEPATH
  ref_lt_file: FILEPATH
  pop_file: FILEPATH
  model: 6.0
  rake_to_gbd_version:
    status: best
    gbd_round_id: 6.0
    decomp_step: step5
    version: 135.0
  edu_together: no
  ages:
  - 0.0
  - 1.0
  - 5.0
  - 10.0
  - 15.0
  - 20.0
  - 25.0
  - 30.0
  - 35.0
  - 40.0
  - 45.0
  - 50.0
  - 55.0
  - 60.0
  - 65.0
  - 70.0
  - 75.0
  - 80.0
  - 85.0
  begin_year: 1980
  end_year: 2018
  deaths_file: FILEPATH
  fixed_age_time: no
submitted_causes:
  _all:
    level: 0.0
    cause_id: 294.0
    cause_outline: Total
    parent_id: 294.0
    path_to_top_parent: '294'
    run_model: 1.0
```

## Structure
YAML is a data-serialization language used largely for configuration files. For our case it is sufficient to understand the following markup syntaxes:
* Colon (`:`) separates key-value pair which is equivalent to name and value of a named list in R.
* Space indentation signifies nested elements (list of lists in R).
* Dash (`-`) implies scalar elements (un-named vector in R).

In the above "settings.yaml" example, the content can be interpreted as two main lists: `settings` and `submitted-causes`. Other indented items are the nested lists within them. 

If you are interested in learning more about YAML [see here](https://yaml.org/spec/1.2.2/#21-collections). 
## Using a "settings.yaml" file
There are methods built into `ModelSettings` to read-in "settings.yaml" file into R list objects. Once you have loaded `sae.shared` package with `lbd.loader`, you can use these methods to load the settings.

```
# load "sae.shared" library with "lbd.loader"
library(lbd.loader, lib.loc = sprintf("FILEPATH", R.version$major, strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
lbd.loader::load_package('sae.shared')
# model dir
dir <- "FILEPATH"

# reading yaml settings from dir into modelSettings object
modelSettings <- ModelSettings$from_yaml(dir)
# general model settings
general_settings <- modelSettings$settings
# submitted-cause-list settings
submitted_causes <- modelSettings$submitted_causes
# cause specific settings for "_neo"
neo_settings <- modelSettings$settings_for_cause("_neo")$settings
```

## Updating settings.yaml file
While "settings.yaml" file can be directly edited, the recommendation is not to do so. Once you start manually changing .yaml file, you will have to track the changes between both .csv and .yaml settings. Instead, make changes to the relevant "settings.csv" or "submitted_cause_list.csv" files. Then rerun the function `write_consolidated_settings()` to re-create "settings.yaml" file which will contain the updates.

## Errors/warnings !
* You might run into error that says `$SETTING for $CAUSE in cause setting and submitted_cause_list setting do not match!!` which is usually the case when there is a typo in one of the cause-settings or general settings file. They should match exactly.
* The warning that says `Caution! cause_setting not in main_settings or submitted_cause.  $CAUSE: $SETTING` should be rare. However, if you run into this, please make sure that it is intentional.

## Misc !!
* Refrain from using any variation of **yes** or **no** words in settings (.csv files). They are reserved keywords (booleans) in yaml language.