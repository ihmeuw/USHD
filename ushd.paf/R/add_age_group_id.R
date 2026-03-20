ushd.add_age_group_id <- function(dt) {
    ages <- sort(unique(dt$age))
    lookup <- data.table(age = ages, age_group_id = suppressWarnings(sae.shared::translate_ages(ages, "age_group_id")))
    # https://stackoverflow.com/a/44491274
    dt[lookup, on = .(age)]
}
