load("./data/agg_data_all_features_dep_vars_22_00.rda")


known <- as.data.table(known) 
library(lubridate)
known[, Month_c := parse_date_time(Month, 'ym')]
known = known[order(Month_c)]
known[, PRICE := na.locf(PRICE, na.rm = F), by=list(SalOrg, Material)]

to_exclude = c('Month_c', 'Month', 'month', 'year', 'Material', 'demand', 'SalOrg') 
cols <- names(known)[!names(known)%in%to_exclude]
out_cols = paste(cols, 'imp', sep = "_")
known[, c(out_cols) := lapply(.SD, function(x){ na.locf(x, na.rm = F) }),by=list(SalOrg, Material) , .SDcols = cols]

asd = known[, .(MOQ), by = .(Material, SalOrg, Month)]
Moq_set = unique(asd)

save(Moq_set, file = "./data/moq.rda")


avg_ = known[, .(avg_demand = mean(demand)), by = .(SalOrg, Material, Month_c)]
avg_ = unique(avg_)

avg_one = avg_
avg_one = unique(avg_one[, .(demand = avg_demand), by = .(SalOrg, Material)])
avg_one = unique(avg_one[,,by = .(SalOrg, Material)])

data_unknown = fread("./data/eval_correct.csv")
data_unknown = merge(data_unknown, avg_one[, .(SalOrg, Material, demand)], all.x = T, all.y = F)
media = fread("/Users/lizzzi111/Downloads/median.csv")
media[, demand2 := demand] 
media[, .(ID,  Month, demand2)]
data_unknown = data_unknown[order(ID),]
data_unknown[, demand2 := media[, demand2]] 
data_unknown[is.na(demand), demand := demand2]
data_unknown[demand==1, demand := (demand+demand2+demand2)/3]

sub = data_unknown[, .(ID, demand)]
write.csv(sub, "./data/demand_1_plus_best_meanof3.csv", quote = F, row.names = F)

avg_04 = known[, .(avg_demand = mean(demand)), by = .(Material, Month_c)]

data_unknown = fread("./data/eval_correct.csv")
media = fread("/Users/lizzzi111/Downloads/median.csv")
data_unknown = merge(data_unknown, media, all.x = T, all.y = F)
data_unknown[date == "2017-04", demand := 0.90*demand ]
data_unknown[date == "2017-05", demand := 1.1*demand ]
data_unknown[date == "2017-06", demand := 0.9*demand ]
sub = data_unknown[, .(ID, demand)]
write.csv(sub, "./data/calibrationof_median_more.csv", quote = F, row.names = F)
