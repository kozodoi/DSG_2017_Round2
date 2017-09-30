###################################
#                                 #
#             SETTINGS            #
#                                 #
###################################

# clearing the memory
rm(list = ls())

# setting directory depending on a user
if (Sys.info()[8] == "lizzzi111")           {setwd("~/Documents/DSG_2017_Finals/")} 
if (Sys.info()[8] == "kozodoi")             {setwd("~/Documents/Competitions/DSG_2017_Finals/")}
if (Sys.info()[8] == "nataliasverchkova")   {setwd("~/Documents/DSG/DSG_2017_Finals/")}
if (Sys.info()[8] == "oleksiyostapenko")    {setwd("/Users/oleksiyostapenko/Documents/HU_Berlin/ML/DSG/DSG_2017_Finals")}

# setting inner folders
code.folder <- "codes"
data.folder <- "data"
func.folder <- "functions"
subm.folder <- "submissions"

# loading libraries
if (require(pacman) == FALSE) install.packages("pacman")
library(pacman)
p_load(dplyr, data.table, caret, Metrics, xgboost, vtreat, plyr)

# loading all functions
source(file.path(code.folder, "code_0_helper_functions.R"))
source(file.path(code.folder, "code_0_parameters.R"))


###################################
#                                 #
#         CREATING FEATURES       #
#                                 #
###################################

# loading data
load(file.path(data.folder, "data_known.rda"))
load(file.path(data.folder, "data_unknown.rda"))

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


library(lubridate)
data_known = as.data.table(data_known)
data_known[, `:=`  (month = month(First_MAD),  year = year(First_MAD)) ]
# 
# data_known[, Plant_main := Mode(Plant), by = .(SalOrg, Material, month, year)]
# print('1')
# data_known[, ItemCat_lv0i := sum(ItemCat == 'lvOi'), by = .(SalOrg, Material, month, year)]
# print('2')
# data_known[, ItemCat_v1V8 := sum(ItemCat == 'v1V8'), by = .(SalOrg, Material, month, year)]
# print('3')
# data_known[, ItemCat_8vSV := sum(ItemCat == '8vSV'), by = .(SalOrg, Material, month, year)]
# print('4')
# data_known[, ItemCat_P8ip := sum(ItemCat == 'P8ip'), by = .(SalOrg, Material, month, year)]
# print('5')
# data_known[, LT_mean  := mean(LT, na.rm = T), ,by = .(SalOrg, Material, month, year)]
# print('6')
# #data_known[, LT_mode  := Mode(LT), ,by = .(SalOrg, Material, month, year)]
# #print('7')
# data_known[, LT_max  := max(LT), ,by = .(SalOrg, Material, month, year)]
# print('8')
# data_known[, LT_min := min(LT), ,by = .(SalOrg, Material, month, year)]
# print('9')
# data_known[, LogABC_Nc4e := sum(LogABC == 'Nc4e'), by = .(SalOrg, Material, month, year)]
# print('10')
# data_known[, LogABC_hfuf := sum(LogABC == 'hfuf'), by = .(SalOrg, Material, month, year)]
# print('11')
# data_known[, LogABC_XRJD := sum(LogABC == 'XRJD'), by = .(SalOrg, Material, month, year)]
# print('12')
# data_known[, MOQ_mean  := mean(MOQ, na.rm = T), ,by = .(SalOrg, Material, month, year)]
# print('13')
# data_known[, ROP_mean  := mean(ROP, na.rm = T), ,by = .(SalOrg, Material, month, year)]
# print('14')
# data_known[, SafetyStk_mean  := mean(SafetyStk, na.rm = T), ,by = .(SalOrg, Material, month, year)]
# print('15')
# data_known[, Plant_unique:= count(.N), by = .(SalOrg, Material, month, year, Plant)]
# print('16')
# 
# sub = data_known[,.(SalOrg, Material, month, year, Plant_unique, SafetyStk_mean,
#               ROP_mean, MOQ_mean, LogABC_XRJD, LogABC_hfuf, LogABC_Nc4e, LT_min,
#               LT_max, LT_mode, LT_mean, ItemCat_P8ip, ItemCat_8vSV, ItemCat_v1V8, ItemCat_lv0i,
#               Plant_main)]
# sub = unique(sub)
# 
# save(sub,   file = file.path(data.folder, "aggr_liza.rda"))

data_known[, Month_c := parse_date_time(Month, 'ym')]
data_known = data_known[order(Month_c)]

# factors check
#data_known[, count_order := count(.N), by = .(ordre)]
cols <- c('DC', 'Plant', 'ItemCat', 'LogABC', 'PL', 'MktABC', 'SubFct', 'Gamma', 
          'Manufacturer', 'Business')
out_cols = paste(cols, 'Mode', 'salorg_material',sep = "_")
out_cols_count = paste(cols, 'unique_count', 'salorg_material',sep = "_")
data_known[, c(out_cols) := lapply(.SD, function(x){ Mode(x) }),by=list(SalOrg, Material) , .SDcols = cols]
data_known[, c(out_cols_count) := lapply(.SD, function(x){ length(unique(x)) }),by=list(SalOrg, Material) , .SDcols = cols]
salorg_material = data_known[,.SD, .SDcols = c(out_cols_count,out_cols)]
salorg_material = unique(salorg_material)
save(salorg_material, file='./aggregated/salorg_material_fac.rda')


out_cols = paste(cols, 'Mode', 'salorg_material_month_year',sep = "_")
out_cols_count = paste(cols, 'unique_count', 'salorg_material_month_year',sep = "_")
data_known[, c(out_cols) := lapply(.SD, function(x){ Mode(x) }),by=list(SalOrg, Material, month, year) , .SDcols = cols]
data_known[, c(out_cols_count) := lapply(.SD, function(x){ length(unique(x)) }),by=list(SalOrg, Material, month, year) , .SDcols = cols]
salorg_material_month_year = data_known[,.SD, .SDcols = c(out_cols_count,out_cols)]
salorg_material_month_year = unique(salorg_material_month_year)
save(salorg_material_month_year, file='./aggregated/salorg_material_month_year_fac.rda')
