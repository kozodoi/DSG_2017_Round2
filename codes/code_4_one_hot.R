load(file.path(data.folder, 'data_known.rda'))

data_known <- data_known %>% 
  rowwise() %>%
  mutate(month  = as.numeric(substr(Month, 6, 7)),
         year = substr(Month, 1, 4))

data_known <- data_known %>% 
  rowwise() %>%
  mutate(quarter  = ifelse(month<=3, 1,
                         ifelse(month>3 & month<=6, 2,
                               ifelse(month>6 & month<=9, 3, 4))))
data_known_agg <- data_known %>% 
  ungroup() %>% 
  group_by(SalOrg, Material, year, quarter) %>% 
  summarise(demandQu = sum(OrderQty))

data_known_agg <- data_known_agg %>% 
  rowwise() %>% 
  mutate(Quater = paste0(year, '-', quarter)) %>% 
  ungroup() %>% 
  group_by(SalOrg, Material) %>%
  arrange(Quater) %>% 
  mutate(demandQu_LEAD = lead(demandQu, 1),
         demandQu_LAG1 = lag(demandQu, 1),
         demandQu_LAG2 = lag(demandQu, 2),
         demandQu_LAG3 = lag(demandQu, 3),
         demandQu_LAG4 = lag(demandQu, 4))


data_known = as.data.frame(data_known_agg)



data_known$super_factor <- interaction(data_known$SalOrg, data_known$Material, 
                                       as.factor(data_known$month), as.factor(data_known$year))
#c <- (as.data.frame(model.matrix(super_factor~DC-1, data_known)))
data_known <- as.data.frame(data_known)
data_known_expanded <- data_known[colnames(data_known)%in%c('SalOrg', 'Material', 'month', 'year', 'super_factor')]
data_known_expanded <- cbind(data_known_expanded, 
                             as.data.frame(model.matrix(super_factor~DC-1, data_known)))
data_known_expanded <- cbind(data_known_expanded, 
                             as.data.frame(model.matrix(super_factor~Plant-1, data_known)))
data_known_expanded <- cbind(data_known_expanded, 
                             as.data.frame(model.matrix(super_factor~ItemCat-1, data_known)))
data_known_expanded <- cbind(data_known_expanded, 
                             as.data.frame(model.matrix(super_factor~LogABC-1, data_known)))
data_known_expanded <- cbind(data_known_expanded, 
                             as.data.frame(model.matrix(super_factor~PL-1, data_known)))
data_known_expanded <- cbind(data_known_expanded, 
                             as.data.frame(model.matrix(super_factor~MktABC-1, data_known)))
data_known_expanded <- cbind(data_known_expanded, 
                             as.data.frame(model.matrix(super_factor~SubFct-1, data_known)))
data_known_expanded <- cbind(data_known_expanded, 
                             as.data.frame(model.matrix(super_factor~Gamma-1, data_known)))
data_known_expanded <- cbind(data_known_expanded, 
                             as.data.frame(model.matrix(super_factor~Business-1, data_known)))
#data_known_expanded[,6:ncol(data_known_expanded)] <- lapply(data_known_expanded[,6:ncol(data_known_expanded)], function(x) as.numeric(as.character(x)))
data_known_expanded_MaYMoC <- data_known_expanded %>% 
  distinct() 
#data_known_expanded_MaYMoC[,c(6:ncol(data_known_expanded_MaYMoC))] <- lapply(data_known_expanded_MaYMoC[,c(6:ncol(data_known_expanded_MaYMoC))], function(x) as.numeric(as.character(x)))
#str(data_known_expanded_MaYMoC)
data_known_expanded_MaYMoC <- as.data.frame(data_known_expanded_MaYMoC) %>%
  select(-super_factor) %>%
  group_by(SalOrg, Material, month, year) %>%
  summarise_all(funs(max(., na.rm = T)))
#1 %in% c(1,0,0)
#save(data_known_expanded_MaYMoC, file=‘./aggrdata/data_known_expanded_MaYMoC.rda’)
# ----------------------------------------------------------------------------------------
data_known$super_factor_SMAMO <- interaction(data_known$SalOrg, data_known$Material, 
                                             as.factor(data_known$month))
data_known_expanded_SMAMO <- data_known[colnames(data_known)%in%c('SalOrg', 'Material', 'month','super_factor_SMAMO')]
data_known_expanded_SMAMO <- cbind(data_known_expanded_SMAMO, 
                                   as.data.frame(model.matrix(super_factor_SMAMO~DC-1, data_known)))
data_known_expanded_SMAMO <- cbind(data_known_expanded_SMAMO, 
                                   as.data.frame(model.matrix(super_factor_SMAMO~Plant-1, data_known)))
data_known_expanded_SMAMO <- cbind(data_known_expanded_SMAMO, 
                                   as.data.frame(model.matrix(super_factor_SMAMO~ItemCat-1, data_known)))
data_known_expanded_SMAMO <- cbind(data_known_expanded_SMAMO, 
                                   as.data.frame(model.matrix(super_factor_SMAMO~LogABC-1, data_known)))
data_known_expanded_SMAMO <- cbind(data_known_expanded_SMAMO, 
                                   as.data.frame(model.matrix(super_factor_SMAMO~PL-1, data_known)))
data_known_expanded_SMAMO <- cbind(data_known_expanded_SMAMO, 
                                   as.data.frame(model.matrix(super_factor_SMAMO~MktABC-1, data_known)))
data_known_expanded_SMAMO <- cbind(data_known_expanded_SMAMO, 
                                   as.data.frame(model.matrix(super_factor_SMAMO~SubFct-1, data_known)))
data_known_expanded_SMAMO <- cbind(data_known_expanded_SMAMO, 
                                   as.data.frame(model.matrix(super_factor_SMAMO~Gamma-1, data_known)))
data_known_expanded_SMAMO <- cbind(data_known_expanded_SMAMO, 
                                   as.data.frame(model.matrix(super_factor_SMAMO~Business-1, data_known)))
data_known_expanded_SMAMO <- as.data.frame(data_known_expanded_SMAMO) %>%
  distinct()
data_known_expanded_SMAMO <- data_known_expanded_SMAMO %>% 
  select(-super_factor_SMAMO) %>%
  mutate(SalOrg = as.character(SalOrg),
         Material=as.character(Material))
data_known_expanded_SMAMO <- data_known_expanded_SMAMO %>% 
  group_by(SalOrg, Material, month) %>%
  summarise_all(funs(max(., na.rm = T)))
#save(data_known_expanded_SMAMO, file=‘./aggrdata/data_known_expanded_SMAMO.rda’)  
# ----------------------------------------------------------------------------------------
data_known$super_factor_SMAY <- interaction(data_known$SalOrg, data_known$Material, 
                                            as.factor(data_known$year))
data_known_expanded_SMAY <- data_known[colnames(data_known)%in%c('SalOrg', 'Material', 'year', 'super_factor_SMAY')]
data_known_expanded_SMAY <- cbind(data_known_expanded_SMAY, 
                                  as.data.frame(model.matrix(super_factor_SMAY~DC-1, data_known)))
data_known_expanded_SMAY <- cbind(data_known_expanded_SMAY, 
                                  as.data.frame(model.matrix(super_factor_SMAY~Plant-1, data_known)))
data_known_expanded_SMAY <- cbind(data_known_expanded_SMAY, 
                                  as.data.frame(model.matrix(super_factor_SMAY~ItemCat-1, data_known)))
data_known_expanded_SMAY <- cbind(data_known_expanded_SMAY, 
                                  as.data.frame(model.matrix(super_factor_SMAY~LogABC-1, data_known)))
data_known_expanded_SMAY <- cbind(data_known_expanded_SMAY, 
                                  as.data.frame(model.matrix(super_factor_SMAY~PL-1, data_known)))
data_known_expanded_SMAY <- cbind(data_known_expanded_SMAY, 
                                  as.data.frame(model.matrix(super_factor_SMAY~MktABC-1, data_known)))
data_known_expanded_SMAY <- cbind(data_known_expanded_SMAY, 
                                  as.data.frame(model.matrix(super_factor_SMAY~SubFct-1, data_known)))
data_known_expanded_SMAY <- cbind(data_known_expanded_SMAY, 
                                  as.data.frame(model.matrix(super_factor_SMAY~Gamma-1, data_known)))
data_known_expanded_SMAY <- cbind(data_known_expanded_SMAY, 
                                  as.data.frame(model.matrix(super_factor_SMAY~Business-1, data_known)))
data_known_expanded_SMAY <- data_known_expanded_SMAY %>% 
  distinct() %>% 
  select(-super_factor_SMAY) %>%
  group_by(SalOrg, Material, year) %>%
  summarise_all(funs(max(., na.rm = T)))

data_known_expanded_SMAY2 <- as.data.frame(data_known_expanded_SMAY) %>%
  distinct()
data_known_expanded_SMAY <- data_known_expanded_SMAY %>% 
  select(-super_factor_SMAY) %>%
  mutate(SalOrg = as.factor(SalOrg),
         Material=as.factor(Material))

data_known_expanded_SMAY=data.table(data_known_expanded_SMAY)

names = setdiff(colnames(data_known_expanded_SMAY), c('SalOrg', 'Material', 'year'))

for (name in names){

  data_known_expanded_SMAY[ , eval(name):=max(get(name), na.rm = TRUE) , by=.(SalOrg, Material, year) ]

}

data_known_expanded_SMAY <- as.data.frame(data_known_expanded_SMAY) %>%
  distinct() %>% 
  mutate(year =as.character(year))

#save(data_known_expanded_SMAY, file=‘./aggrdata/data_known_expanded_SMAY.rda’)  
# ----------------------------------------------------------------------------------------
load(file.path(data.folder, ‘all_var_to_keep.rda’))
ohe_all <- all %>%
  left_join(data_known_expanded_SMAY, by=c(‘SalOrg’, ‘Material’, ‘year’)) %>% 
  left_join(data_known_expanded_SMAMO, by=c(‘SalOrg’, ‘Material’, ‘month’)) 
# %>% 
#   left_join(data_known_expanded_MaYMoC, by=c(‘SalOrg’, ‘Material’, ‘year’, ‘month’))
ohe_all[is.na(ohe_all)] <- 0
all_ohe_df <- as.data.frame(ohe_all)
save(all_ohe_df, file=‘./data/data_known_ohe.rda’)  
write.csv(all_ohe_df, file=‘./data/data_known_ohe.csv’, row.names = F)
class(all_ohe_df)
ohe_all_sum <- ohe_all %>% 
  group_by(SalOrg,Material,month,year) %>% 
  summarise_each(as.numeric(.))


known_ohe <- known %>% 
  mutate(year =as.character(year)) %>% 
  left_join(data_known_expanded_SMAY, by=c('SalOrg', 'Material', 'year'))
