####################################################################################################
## Description: Prep an adjacency matrix for the merged counties.
##
## Output:      "FILEPATH" -- a data.table of
##                listing pairs of areas that are adjacent.
##              "FILEPATH" -- a dgTMatrix
##                object where each row and column refers to a given area (sorted by fips code) and
##                element (i,j) is 1 if i and j are neighbors and 0 otherwise. The diagonal also = 1.
####################################################################################################

library(data.table)
library(zoo)
library(car)
library(Matrix)


rm(list = ls())
root <- ifelse(Sys.info()[1]=="Windows", "FILEPATH", "FILEPATH")
main_dir <- paste0(root, "FILEPATH")
parent_dir <- paste0(root, "FILEPATH")

## Load merged counties file
loc <- fread(paste0(parent_dir, "counties/merged_counties.csv"))

#Create adjacency matrix ----------------------------------------------------------------------------------------------------------------------------------------------------------
## Load adjacency file (from the Census Bureau, current to 2010 census counties)
adj <- fread(paste0(root, "FILEPATH"))
adj <- adj[, c(2, 4), with = F]
setnames(adj, c("cnty1", "cnty2"))
adj[, cnty1 := na.locf(cnty1)]
adj <- adj[cnty1 < 60000,]
uniqueN(adj$cnty1)

## Map 2010 county adjacencies to merged county adjacencies
print("merging and collapsing")
adj[, cnty1 := recode(cnty1, loc[, paste(paste(cnty, mcnty, sep = "="), collapse = ";")])]
adj[, cnty2 := recode(cnty2, loc[, paste(paste(cnty, mcnty, sep = "="), collapse = ";")])]
adj <- unique(adj)
uniqueN(adj$cnty1)

## Make all Hawaii counties neighbors (otherwise they have no neighbors except themselves)
adj <- rbind(adj, CJ(cnty1 = loc[state_name == "Hawaii", unique(mcnty)],
                     cnty2 = loc[state_name == "Hawaii", unique(mcnty)]))#21570 obs

## Make sure that adjacencies are symmetric
adj <- unique(rbind(adj, data.table(cnty1 = adj$cnty2, cnty2 = adj$cnty1)))#21568 obs

## don't let counties be adjacent to themselves
adj <- adj[cnty1 != cnty2]#18458 obs

load(paste0(parent_dir, "FILEPATH"))

cbsa.cnty.data <- cbsa.cnty.data[,c('area.codeint','mcnty')]

adjmerge1 <- merge(adj, cbsa.cnty.data, by.x='cnty1', by.y='mcnty', all.y=F)#18458 obs 
setnames(adjmerge1,'area.codeint','area.code1')

#do the same for cnty2
adjmerge2 <- merge(adjmerge1, cbsa.cnty.data, by.x='cnty2', by.y='mcnty', all.y=F)#18458 obs 
setnames(adjmerge2,'area.codeint','area.code2')

#keep only area.code columns
adjmerge2 <- adjmerge2[,c('area.code1','area.code2')]

#drop duplicate rows
adjmerge2<-unique(adjmerge2)#744 obs

## Make sure that adjacencies are symmetric
adjmerge2 <- unique(rbind(adjmerge2, data.table(area.code1 = adjmerge2$area.code2, area.code2 = adjmerge2$area.code1)))#13476 obs

# don't let areas be adjacent to themselves
adjmerge2 <- adjmerge2[area.code1 != area.code2]#622 obs

## Convert to an adjacency matrix
adjmat <- sparseMatrix(i = adjmerge2$area.code1 + 1, j = adjmerge2$area.code2 + 1, x = rep(1, nrow(adjmerge2)), giveCsparse = F)

## Save
print("saving")
saveRDS(adjmerge2, file = paste0(parent_dir, "/CBSAs/adjacency_list.rds"))
saveRDS(adjmat, file = paste0(parent_dir, "/CBSAs/adjacency_matrix.rds"))



     