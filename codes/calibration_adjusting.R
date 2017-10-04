# Calibration submissions

data_unknown = fread("./data/eval_correct.csv")
media = fread("/Users/lizzzi111/Downloads/median.csv")
data_unknown = merge(data_unknown, media, all.x = T, all.y = F)
data_unknown[date == "2017-04", demand := 0.90*demand ]
data_unknown[date == "2017-05", demand := 1.1*demand ]
data_unknown[date == "2017-06", demand := 0.9*demand ]
sub = data_unknown[, .(ID, demand)]
write.csv(sub, "./data/calibrationof_median_more.csv", quote = F, row.names = F)

data_unknown = fread("./data/eval_correct.csv")
media = fread("/Users/lizzzi111/Downloads/median.csv")
data_unknown = merge(data_unknown, media, all.x = T, all.y = F)
data_unknown[date == "2017-04", demand := 0.95*demand ]
data_unknown[date == "2017-05", demand := 1.05*demand ]
data_unknown[date == "2017-06", demand := 0.95*demand ]
sub = data_unknown[, .(ID, demand)]
write.csv(sub, "./data/calibrationof_median.csv", quote = F, row.names = F)