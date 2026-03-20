# launch the script to compare performance of ensemble weight sets specific to USHD

# USER SPECIFY: ################################################################
# The dates you want to compare
dates <- c("2023_01_18_09_27_06",
           "2023_02_21_13_13_03",
           "2023_02_21_16_20_42")

################################################################################

# set up ------------------------------------------------------------------

nf_repo <- paste0("FILEPATH")
risk_repo <- paste0("FILEPATH")

funcs <- list.files(paste0(nf_repo, "/functions/"))
for (func in funcs) {
  try(source(paste0(nf_repo, "/functions/", func)))
}

root = "FILEPATH"

out_dir <- paste0(root, "compare_versions/", make_time_stamp())
dir.create(out_dir)

fwrite(as.list(dates), file = paste0(out_dir, "/compare_dates.csv"))

sbatch(code = paste0(risk_repo, "ensemble/modeling/1b_weight_fitting_component/vetting/compare_performance_ushd.R"),
      arguments = c(out_dir),
      project = "PROJECT",
      name = "compare_weight_sets",
      fthread = 3,
      m_mem_free = "20G",
      sgeoutput = paste0("FILEPATH"),
      h_rt = "00:20:00",
      archive = F,
      queue = "QUEUE"
)
