xgb_new = read.csv("/Users/lizzzi111/Downloads/xgb_new.csv")
xgb_new$score = 10.91791
sum(xgb_new$demand)

xgb_new2 = read.csv("/Users/lizzzi111/Downloads/xgb_new 2.csv")
xgb_new2$score = 10.97585
sum(xgb_new2$demand)

xgb_arima= read.csv("/Users/lizzzi111/Downloads/arima_nikita_new.csv")
xgb_arima$score = 11.17453
sum(xgb_arima$demand)

xgb_best= read.csv("/Users/lizzzi111/Downloads/xgb_lags_nikita.csv")
xgb_best$score = 10.75923
sum(xgb_best$demand)

xgb_20lag= read.csv("/Users/lizzzi111/Downloads/xgb_new_f_lags20.csv")

sum(xgb_best$demand)

ff = data_frame(xgb_new1091 = xgb_new$demand, xgb_new1097 =xgb_new2$demand,
                xgb_arima1117=xgb_arima$demand , xgb_best1075=xgb_best$demand,
                xgb_20lag= xgb_20lag$demand)
gg = rowMeans(ff)
dt = data.frame(ID = xgb_arima$ID, demand = gg)
summary(dt$demand)

write.csv(dt, file = "./data/subm_mean_boost.csv", sep = ',', row.names = F)

dt$demand[dt$demand > quantile(dt$demand, 0.75)] = dt$demand[dt$demand > quantile(dt$demand, 0.75)]*0.75
dt$demand[dt$demand > quantile(dt$demand, 0.75)] = dt$demand[dt$demand > quantile(dt$demand, 0.75)]*0.55
dt$demand[dt$demand > quantile(dt$demand, 0.85)] = dt$demand[dt$demand > quantile(dt$demand, 0.85)]*0.75
summary(dt$demand)

write.csv(dt, file = "./data/subm_mean_boost_5models.csv", sep = ',', row.names = F, quote = F)



# 0
dt = data.frame(ID = xgb_best$ID, demand = 0)
# 0.5
dt = data.frame(ID = xgb_best$ID, demand = 0.5)
# 1
dt = data.frame(ID = xgb_best$ID, demand = 1)

write.csv(dt, file = "./data/check_05.csv", sep = ',', row.names = F, quote = F)


demand1 = sample(known$demand, 116028, replace = T)
demand2 = sample(known$demand, 116028, replace = T)
demand3 = sample(known$demand, 116028, replace = T)
demand4 = sample(known$demand, 116028, replace = T)
demand5 = sample(known$demand, 116028, replace = T)
demand6 = sample(known$demand, 116028, replace = T)
demand7 = sample(known$demand, 116028, replace = T)
demand8 = sample(known$demand, 116028, replace = T)
demand9 = sample(known$demand, 116028, replace = T)
demand10 = sample(known$demand, 116028, replace = T)
demand11 = sample(known$demand, 116028, replace = T)

ff = data_frame(demand1, demand2, demand3, demand4, demand5, demand6,
                demand7, demand8, demand9, demand10, demand11)
gg = rowMeans(ff)
dt = data.frame(ID = xgb_arima$ID, demand = gg)

known[, demand := sum(OrderQty), by = .(Month_c, Material, SalOrg)]
