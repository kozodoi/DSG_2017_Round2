### PARAMETERS FOR EVERYONE

## Seed
set.seed(213)

## Dependent variable name
dv <- ""

## Variables types
num_vars <- c("OrderQty", "LT", "MOQ", "ROP", "SafetyStk", "Gross_Weight", "Length", "Width", "Height", "Volume",
              "CBO_CBO_Qty_Shortage", "Age_ZN_ZI_years", "Comp_reference_number", "Name_Of_Competitor",
              "COMP_PRICE_MIN", "COMP_PRICE_AVG", "COMP_PRICE_MAX", "PRICE", "NEAREST_COMP_PRICE_MIN",
              "NEAREST_COMP_PRICE_MAX") # numeric
fac_vars <- c("DC", "Ship_To", "Plant", "Material", "ItemCat", "ordre", "LogABC", "PL", "MktABC", "SubFct",
              "Gamma", "Manufacturer", "Business", "Month", "DP_FAMILY_CODE", "PRODUCT_STATUS", 
              "ORIGINAL_SUPPLIER", "SUBRANGE", "SalOrg")  # factors
dat_vars <- c("First_MAD")  # dates
ign_vars <- c("ID")  # ignore variables of id and partitioning

## Variables which we drop from the xgboost
drop_vars <- c("")

## Xgboost optimal parameters
par1 <- 0.3
par2 <- 500