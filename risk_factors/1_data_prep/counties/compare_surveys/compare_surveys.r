####################################################################################################
## Description: Compare basic demographic and additional variables across surveys
####################################################################################################

library(data.table)
library(ggplot2)

rm(list=ls())
root <- ifelse(Sys.info()[1]=="Windows", "FILEPATH", "FILEPATH")
main_dir <- paste0(root, "FILEPATH")


## Load NHANES data, keep only age 20+
load(paste0(main_dir, "_data_prep/counties/survey_data/nhanes_microdata.rdata"))
data <- data[age >= 20, ]
data[, sex := factor(sex, levels=c(1,2), labels=c("Males", "Females"))]
data[, age := cut(age, breaks=c(20, 30, 40, 50, 60, 70, 100), right=F)]
data[, bmi_cat := cut(bmi, breaks=c(0, 25, 30, 35, 100), right=F)]
data[, smoke := factor(smoke_any, levels=0:1, labels=c("No", "Yes"))]
data[, hlthplan := factor(hlthplan, levels=0:1, labels=c("No", "Yes"))]
data[, diabetes := factor(diq, levels=0:1, labels=c("Diagnosed: No", "Diagnosed: Yes"))]
data[, highbp := factor(as.numeric(highbp == 1), levels=0:1, labels=c("Diagnosed: No", "Diagnosed: Yes"))]
data[, year := as.numeric(substr(svyyear, 1, 4))]
setnames(data, "int_wt", "wt")
id.vars <- c("year", "sex", "wt")

categorical.vars <- c("race", "marital", "edu", "age", "bmi_cat", "hlthplan", "smoke", "diabetes", "highbp")

nhanes <- lapply(categorical.vars, function(var) {
  temp <- data[, c(id.vars, var), with=F]
  temp <- temp[!is.na(get(var)),]
  temp[, respondent := 1:nrow(temp)]
  factor_variables <- levels(temp[, get(var)])
  temp <- dcast.data.table(temp, paste("respondent + year + sex + wt ~", var), value.var=var, length)
  
  temp <- lapply(factor_variables, function(fvar) {
    temp[, list(category = fvar, percent = 100*weighted.mean(get(fvar), wt)), by='sex,year']
  })
  temp <- rbindlist(temp)
  temp[, source := "NHANES"]
  temp[, variable := var]
  temp_plus_one_year <- copy(temp)
  temp <- rbind(temp, temp_plus_one_year[, year:= year+1])
  setkeyv(temp, c("year", "sex", "variable", "source"))
})
nhanes <- rbindlist(nhanes)

## Load BRFSS data, keep only age 20+, year >= 1999
load(paste0(main_dir, "_data_prep/counties/survey_data/brfss_microdata.rdata"))
data <- data[age >= 20 & year >= 1999, ]
data[, sex := factor(sex, levels=c(1,2), labels=c("Males", "Females"))]
data[, age := cut(age, breaks=c(20, 30, 40, 50, 60, 70, 100), right=F)]
data[, race := factor(ifelse(race == "native", "other", as.character(race)), levels=c("white", "black", "hisp", "other"))] # group native into other
data[, smoke := factor(smoke_any, levels=0:1, labels=c("No", "Yes"))]
data[, hlthplan := factor(hlthplan, levels=0:1, labels=c("No", "Yes"))]
data[, diabetes := factor(diabetes, levels=0:1, labels=c("Diagnosed: No", "Diagnosed: Yes"))]
data[, highbp := factor(as.numeric(highbp == 1), levels=0:1, labels=c("Diagnosed: No", "Diagnosed: Yes"))]
id.vars <- c("year", "sex", "wt")

# correct BMI using BMI correction model from obesity analysis
load(paste0(main_dir, "obesity/correction_models/final_model/bmi_correction_model.rdata"))
data[, sr_bmi := weight/(height^2)]
data[, bmi := correct_bmi(sr_bmi, sex[1], correct_bmi_fit, F), by='sex']
data[, bmi_cat := cut(bmi, breaks=c(0, 25, 30, 35, 100), right=F)]
rm(correct_bmi, correct_bmi_fit); gc()

brfss <- lapply(categorical.vars, function(var) {
  temp <- data[, c(id.vars, var), with=F]
  temp <- temp[!is.na(get(var)),]
  temp[, respondent := 1:nrow(temp)]
  factor_variables <- levels(temp[, get(var)])
  temp <- dcast.data.table(temp, paste("respondent + year + sex + wt ~", var), value.var=var, length)
  
  temp <- lapply(factor_variables, function(fvar) {
    temp[, list(category = fvar, percent = 100*weighted.mean(get(fvar), wt)), by='sex,year']
  })
  temp <- rbindlist(temp)
  temp[, source := "BRFSS"]
  temp[, variable := var]
  setkeyv(temp, c("year", "sex", "variable", "source"))
})
brfss <- rbindlist(brfss)

data <- rbindlist(list(nhanes, brfss))
data[, source := factor(source)]
data[, variable := factor(variable)]

pdf(paste0(main_dir, "_data_prep/counties/compare_surveys/compare_surveys.pdf"), width=14, height=8)
for (this_var in categorical.vars) {
  p <- ggplot(data=data[variable==this_var], aes(year, percent)) + 
    geom_point(aes(colour=source)) + geom_line(aes(colour=source)) + facet_grid(sex ~ category) + 
    labs(x="Year", y="Percent of Sample Population", title=paste("Comparison of NHANES and BRFSS", this_var, "variable"))
  print(p)
}
dev.off()
