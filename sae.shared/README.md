### Description 
This codebase hosts shared tools, scripts, and related utilities for Small Area Estimation at IHME.
  
### Running post-estimation
  
1. Make a copy of the model directory to be used for post-estimation. This can be done with `inst/tools/provision_rake_dir.sh` script, which creates symbolic links back to the original model directory. To use, run:
  
    ```
    python inst/tools/provision_rake_dir MODEL_DIRECTORY
    ```
  
2. Consolidate the settings to a single `yaml` file by running "write_consolidated_settings.r". "settings.yaml" takes precedence over ".csv" settings. Running this script will also copy files from `FILEPATH` drive to the model dir and update "settings.yaml" file which enables the later jobs to run on non-archive nodes. 
  
3. Run in the following order:
    * Raking: `raking/_run_all_raking_flexible.r`
    * Aggregation: `post_estimation/launch_aggregation.r`

4. Monitoring: Post-estimation pipelines are managed by Jobmon.

