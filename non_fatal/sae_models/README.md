## Non-fatal YLD-raking workflows

In-order to run raking workflows, the current directory must be set to `non_fatal/sae_models`. The scripts that launch raking jobs and checks the result reside in `sae_models/raking`. The aggregation launch scripts reside in `sae_models/post_estimation`. 

Each launch script loads the `sae.shared` package by sourcing `sae_models/functions/load_sae_shared.R` script. This package is the core of all calculations required to run a complete raking workflow. By default, it is loaded as a package with `library()`. IFF necessary, you may clone the `sae.shared` repository, make the code changes and load the source-repository with `devtools::load_all()`.

[Link to sae.shared repository](FILEPATH)

```
# load sae.shared package-------------------------------------------------------------------------------------------------
# sae.shared is dependent on lbd.loader
library(lbd.loader, lib.loc = "FILEPATH")

sae.shared_repo <- NULL
# If working directly out of cloned repo of sae.shared, pass its path instead (IFF necessary).
# sae.shared_repo <- "~/code/sae.shared"

if (is.null(sae.shared_repo)) {
  # loads the most current library maintained by LSAE. If specific version is required,
  # pass it with "version" argument.
  lbd.loader::load_package("sae.shared")
} else {
  lbd.loader::load_package(path = sae.shared_repo)
}
```

### Submit scripts
#### 1. Raking
```
qsub \
-N yld_all_cause_rake \
-l m_mem_free=60G,fthread=4,h_rt=02:30:00,archive=TRUE \
-P PROJECT \
-now no \
-q QUEUE \
-v sing_image=default \
-o  /path-to-model-dir/job-output \
-j yes \
-cwd \
FILEPATH \
raking/run_all_yld_raking.r --from_dir /path-to-model-dir \
--to_dir gbd \
--from_geo mcnty \
--to_geo state \
--jobmon_resume FALSE \
--fail_fast TRUE \
--queue QUEUE
```

#### 2. Aggregation
```
qsub \
-N yld_agg_all \
-l m_mem_free=20G,fthread=4,h_rt=05:00:00,archive=TRUE \
-P PROJECT \
-now no \
-q QUEUE \
-v sing_image=default \
-o  /path-to-model-dir/job-output \
-j yes \
-cwd \
FILEPATH \
--dir /path-to-model-dir \
--testing FALSE \
--jobmon_resume FALSE \
--queue QUEUE
```


#### 3. Check Results
Run the `raking/check_result.r` script interactively. The script submits a job to the cluster that checks and validates outputs produced in the above raking and aggregation steps.

Outputs are written to the `/path-to-model-dir/check_raked_results` directory.  Individual difference files for geography check are written to geo_diff directory with names like yld_all_cause_geo_diff.rds.  Note that the geo_diff directory includes file that cache the GBD results to speed up reruns (names like: all_cause_yld_gbd_results_cache.RDS).

If there are any issues with checks, the script will error out. However, it is best to check through the log output at the directory specified with `-o` flag to verify that all the checks have succeeded. For a geography check, the following output in log determines that all checks succeeded. 
```
all geography checks passed!
```