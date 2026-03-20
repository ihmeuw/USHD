code <- paste0('FILEPATH')

sbatch(code = paste0(code, "/run-paf.r"), name = paste0("run_paf", run_date), 
       arguments = c('370', '2009', '10'), 
       queue = 'QUEUE', fthread = 1, m_mem_free = "10G", h_rt = "01:00:00", archive = TRUE, 
       sgeoutput = output_dir_draws_est, sing_image = 'FILEPATH')
