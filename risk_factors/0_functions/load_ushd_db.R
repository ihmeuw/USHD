####################################################################################################
## Description: Load USHD database and establish connection. For further documentation, refer to:
##              FILEPATH
##
####################################################################################################

library(lbd.loader, lib.loc = sprintf("FILEPATH",
                                      R.version$major,
                                      strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
library("ushd.dbr", lib.loc = lbd.loader::pkg_loc("ushd.dbr"))

# running below line ensures successful database connection and prevents interference from other packages
ushd_client$save_covariate_population
