
cols <- c("LogABC" , 'PL', 'MktABC',   "Gamma","SalOrg" )
out_cols = paste(cols, 'imp', sep = "_")


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
known <- as.data.table(known) 
known[, c(out_cols) := lapply(.SD, function(x){ Mode(x) }),by=list(Material) , .SDcols = cols]


known[, Gross_Weight_imp := count(.N), by=list(Material, Gross_Weight)]
known[, Gross_Volume_imp := count(.N), by=list(Material, Volume)]

kf = known[, .(LogABC_imp, PL_imp, MktABC_imp, Gamma_imp, 
               SalOrg_imp, Material, Gross_Weight, Gross_Weight_imp, Gross_Volume_imp, Volume)]
kf = unique(kf)
save(kf, file="./data/counts_and_modes.rda")
