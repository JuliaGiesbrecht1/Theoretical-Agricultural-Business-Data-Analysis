# Mastered Data joined and analysis

# Join education and health data
# Mastered data is 1:1
master <- master_agro %>% 
  left_join(Education_health_joined, by = c("clust", "nh"))
View(master)

###################################################################
########### Mastered Date Starting Block - Tweak as needed ########

# Filter all records for clust and house combo that do not have a profit record
master_df <- master %>% 
  filter(!is.na(profit))

# add crop per acre
master_df <- master_df %>% 
  mutate(crop_acre = crop_qty/area)


# Drop the main crop made as 67% is null
# fdrop are and proifit as they are dirrect factors of profict per acre
master_df <- master_df %>% 
  select(-c(main_crop, lnd_amt_owned, profit))

#fill na with zero
master_df[is.na(master_df)] <- 0

###################################################################
###################################################################


# calculating the product of dimensions of dataframe 
totalcells = prod(dim(master_df))
print("Total number of cells ")
print(totalcells)

# calculating the number of cells with na
missingcells = sum(is.na(master_df))
print("Missing value cells")
print(missingcells)

# calculating percentage of missing values
percentage = (missingcells * 100 )/(totalcells)
print("Percentage of missing values' cells")
print (percentage)

# What % of the data is missing for each colmun? 
map(master_df, ~mean(is.na(.))) # We have fields with a substantiate amount of missing data.  
# master_df[is.na(master_df)] <- 0






















# # Total Profit
# master_df$total_profit = master_df$agri1c + master_df$agri2c
# # Add binary for profit (was is proftable or not)
# master_df <- mutate(master_df, profit_bin = (master_df$total_profit >= 0) * 1)
# # # take the log of profit
# # master_df <- mutate(master_df, profit_log = log(master_df$total_profit))
# # master_df[is.na(master_df)] <- 0
# # master_df[(master_df == '-Inf')] <- 0
# 
# 
# 
# #### Drop Uneeded and Unwanted columns
# colnames(master)
# master <- select(master_df, -c(agri1, agri1c, agri2, agri2c, hhagdepn))
# 
# #FillNAs
# master <- master %>% mutate(across('lnd_amt_owned':'crop_variety', ~ replace_na(., 0)))
# master[is.na(master)] <- 0
# map(master, ~mean(is.na(.)))
# 
# 
# # Create region as a string column
# # Mutate the livestk code
# master<- mutate(master, rgn = 'rgn_')
# # Concat columns
# master$rgn_str = paste(master$rgn,master$region, sep = '')
# 
# # Mutate the crop variety
# master<- mutate(master, crp_ = 'crop_')
# # Concat columns
# master$crp_str = paste(master$crp_,master$crop_variety, sep = '')
# 
# #drop rgn and crp place holders
# master <- select(master, -c(rgn, crp_, region, crop_variety))
# master$temp = 1
# 
# # One hot encode rgn
# # Drop the uneeded columuns
# master_rgn = master[ , c("clust", "nh", 'rgn_str', 'temp')] 
# # Spread the df to be at the clust nh level
# master_rgn <- spread(master_rgn, key= rgn_str, value=temp)
# master_rgn[is.na(master_rgn)] <- 0
# 
# master <- master %>% 
#   left_join(master_rgn, by = c("clust", "nh"))
# 
# # One hot encode crop
# # Drop the uneeded columuns
# master_crp = master[ , c("clust", "nh", 'crp_str', 'temp')] 
# # Spread the df to be at the clust nh level
# master_crp <- spread(master_crp, key= crp_str, value=temp)
# master_crp[is.na(master_crp)] <- 0
# 
# master <- master %>% 
#   left_join(master_crp, by = c("clust", "nh"))
# 
# master <- select(master, -c(rgn_str, crp_str))
# 
# 
# 
# 
# 
# 
# # calculating the product of dimensions of dataframe 
# totalcells = prod(dim(master))
# print("Total number of cells ")
# print(totalcells)
# 
# # calculating the number of cells with na
# missingcells = sum(is.na(master))
# print("Missing value cells")
# print(missingcells)
# 
# # calculating percentage of missing values
# percentage = (missingcells * 100 )/(totalcells)
# print("Percentage of missing values' cells")
# print (percentage)
# 
# # What % of the data is missing for each colmun? 
# map(master, ~mean(is.na(.)))
# 
# # Backward feature elimination
# fitall <- lm(total_profit ~ ., data = master)
# summary(fitall)
# 
# step(fitall, direction = "backward")
# 
# 
# backward_lm <- lm(formula = total_profit ~ lnd_amt_owned + `live_stk_cd 10` + 
#                     `live_stk_cd 5` + `live_stk_cd 7` + `live_stk_cd 8` + Num_eqt + 
#                     total_land + main_crop + s8eq3 + s8eq4 + s8eq8 + `Education Attainment` + 
#                     `Years in apprenticeship` + `write English` + `Total Number of children born` + 
#                     `Vaccination Status` + `Health Conditions in last two weeks` + 
#                     `Amount paid for medicine` + profit_bin + rgn_1 + rgn_2 + 
#                     rgn_3 + rgn_4 + rgn_5 + rgn_6 + rgn_7 + rgn_8 + crop_1, data = master)
# summary(backward_lm)
# 
# 
# fitstart = lm(total_profit ~ 1, data = master)
# step(fitstart, direction = "forward", scope=formula(fitall))
# 
# 
# 
# # Backward feature emlimination
# model1 <- lm(total_profit ~ . , data = master)