load("./data/known_structured_19_30.rda")


known <- as.data.table(known) 
library(lubridate)
known[, Month_c := parse_date_time(Month, 'ym')]
known = known[order(Month_c)]
#known[, PRICE := na.locf(PRICE, na.rm = F), by=list(SalOrg, Material)]

to_exclude = c('Month_c', 'Month', 'month', 'year', 'Material', 'demand', 'SalOrg') 
cols <- names(known)[!names(known)%in%to_exclude]
out_cols = paste(cols, 'imp', sep = "_")
known[, c(out_cols) := lapply(.SD, function(x){ na.locf(x, na.rm = F) }),by=list(SalOrg, Material) , .SDcols = cols]
