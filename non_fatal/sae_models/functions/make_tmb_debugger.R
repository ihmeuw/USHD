# HOW TO DEBUG TMB interactively:
#   
#   
# Attempted to modify TMB:gdbsource function

# This script is roughly based on TMB:gdbsource.
# It will provide commands you can copy-and-paste into command line to 
# run a TMB model interatively in debug mode.
make_tmb_debugger <- function(model_file = NULL, # path to TMB model you want to debug (R script)
                            # Shell and image -- Probably don't want to change (these are the versions we use for sbatch) and
                            #   it needs TMB installed
                            shell = "FILEPATH",
                            sing_image = "FILEPATH"){
  if (!file.exists(model_file)) 
    stop("File '", model_file, "' not found")
  
  # write message reporting the singularity image used
  message("Using singularity image: ", sing_image)
  
  gdbscript <- tempfile(tmpdir = paste0("FILEPATH"))
  
  # some arguments to pass to debugger
  cat("run\nbt\nquit\n", file = gdbscript)
  # make executable
  system(paste("chmod +x", gdbscript))
  
  # This will execute the script interatively in debugger mode ()
  cmd <- paste(shell,  sing_image, " -d gdb --debugger-args=\"-x", 
               gdbscript, "\" -s", model_file)
  
  message("Copy-and-paste the following command into console that has an active srun")
  cat("\n\n",noquote(cmd))
  
  message("\n\nThis will open an interactive R session. Then run")
  
  cat("\n\n", noquote(sprintf("source(\"%s\")", model_file)), "\n\nor\n\n", noquote(sprintf("library(TMB); TMB::gdbsource(\"%s\", interactive = T)", model_file)))
  


  message("\nNote that R doesn't recognize the session as interactive when you call TMB::gdbsource\n, so the if(interactive())... portion at the top of the R model script won't work.\nComment out the command args so that only the args intended are used) ")
}


make_tmb_debugger2 <- function(file = NULL, # path to TMB model you want to debug (R script)
                               interactive = NULL,
                              # Shell and image -- Probably don't want to change (these are the versions we use for sbatch) and
                              #   it needs TMB installed
                              sing_image = "FILEPATH"){
  if (!file.exists(file)) 
    stop("File '", file, "' not found")
  
  gdbscript <- tempfile(tmpdir = paste0("FILEPATH"))
  
  if(interactive){
    shell <- "FILEPATH"
    gdbcmd <- c(paste("run --vanilla < ", file), "bt")
    gdbcmd <- paste(gdbcmd, "\n", collapse = "")
    cat(gdbcmd, file = gdbscript)
    system(paste("chmod +x", gdbscript))
    
    cmd <- paste(shell, sing_image, " -d gdb --debugger-args=\'-x", gdbscript, 
                 "\"")
    print(cmd)
    system(cmd, intern = FALSE, ignore.stdout = FALSE, ignore.stderr = TRUE)
    return(NULL)
  } else {
    stop("interactive =F doesn't work right now. Interactive = T should work.")
    cat("run\nbt\nquit\n", file = gdbscript)
    system(paste("chmod +x", gdbscript))
    
    tmpout <- paste0(tempfile(tmpdir = paste0("FILEPATH")), ".txt")
    cat("", file = tmpout)
    cmd <- paste("FILEPATH", tmpout)
    print(cmd)
    system(cmd)
    txt <- readLines(tmpout)
    return(txt)
  }
}