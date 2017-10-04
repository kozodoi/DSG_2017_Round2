p1 <- read.csv(file.path("xgb_new_f.csv"))$demand
p2 <- read.csv(file.path("xgb_new_f-2.csv"))$demand
p3 <- read.csv(file.path("xgb_lags_nikita_3models.csv"))$demand
p4 <- read.csv(file.path("subm_mean_boost_5models-2.csv"))$demand
p5 <- read.csv(file.path("subm_mean_boost_5models.csv"))$demand
p6 <- read.csv(file.path("subm_mean_boost_adjusted.csv"))$demand
p7 <- read.csv(file.path("subm_mean_boost.csv"))$demand

median <- apply(cbind(p1,p2,p3,p4,p5,p6,p7), 1, function(x) median(x))

subm <- read.csv(file.path("xgb_new_f.csv"))

subm$demand <- median

write.table(as.data.frame(subm), file.path(subm.folder, "median.csv"), sep = ",", row.names = F, quote = F)