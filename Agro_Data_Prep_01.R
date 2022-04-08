# Import packages
library(tidyverse)
library(haven)
library(dplyr)
library(ggplot2)
library(corrplot)
library("ggcorrplot")     

################ Data Imports and Testing ##########################
####################################################################


# Agro Data -- sec8A11  -- PK: clust and nh
############################################################
# Agro Land -- Holds total land owned and how much for share corping
sec8a1<- read_dta("Raw_Data/glss4_new/glss4_new/sec8a1.dta")

############################################################
# Agro Data -- sec8A2  -- PK: clust, nh, livstkcd
############################################################
# Agro Land --Agriculture: Livestock and Fishing
sec8a2<- read_dta("Raw_Data/glss4_new/glss4_new/sec8a2.dta")

############################################################
# Agro Data -- sec8A3  -- PK: clust, nh, eqcdown
############################################################
# Agro Land --Agriculture: Livestock and Fishing
sec8a3<- read_dta("Raw_Data/glss4_new/glss4_new/sec8a3.dta")

############################################################
# Agro Data -- sec8b  -- PK: clust, house number, and farm code
############################################################
# Plot Details 
sec8b<- read_dta("Raw_Data/glss4_new/glss4_new/sec8b.dta")

############################################################
# Agro Data -- sec8c1 -- PK: clust, house number, crop code and farmland holder id
############################################################
# Harvest and disposal or staiple grains, feild and cash crops 
sec8c1<- read_dta("Raw_Data/glss4_new/glss4_new/sec8c1.dta")

############################################################
# Agro Data -- sec8c2 -- PK:
############################################################
# Harvest and disposal of fruits and vegetables
sec8c2<- read_dta("Raw_Data/glss4_new/glss4_new/sec8c2.dta")

############################################################
# Agro Data -- sec8d -- PK:
############################################################
# Harvest and disposal of fruits and vegetables
sec8d<- read_dta("Raw_Data/glss4_new/glss4_new/sec8d.dta")

############################################################
# Agro Data -- other_farm -- PK:
############################################################
# Other agro income --
other_farm<- read_dta("Raw_Data/glss4_new/glss4_new/sec8e.dta")

############################################################
# Agro Data -- sec8f -- PK:
############################################################
# agro cost and expense
sec8f<- read_dta("Raw_Data/glss4_new/glss4_new/sec8f.dta")

############################################################
# Agro Data -- sec8g -- PK:
############################################################
# agro processing of agricultural produce
sec8g<- read_dta("Raw_Data/glss4_new/glss4_new/sec8g.dta")

############################################################
# Agro Data -- sec8HID -- PK:
############################################################
# ID's of respondance for home consumption
sec8hid<- read_dta("Raw_Data/glss4_new/glss4_new/sec8hid.dta")

############################################################
# Agro Data -- sec8H -- PK:
############################################################
# agro consumption of home produce
sec8h<- read_dta("Raw_Data/glss4_new/glss4_new/sec8h.dta")

############################################################
# Finacial Data -- PK: clust and nh
############################################################
# Educ Exp
agg2<- read_dta("Raw_Data/glss4_new/glss4_new/aggregates/agg2.dta")

############################################################
# Adding the district and region info
############################################################
#Bring in the region and district info at the house and clust level
sec0b<- read_dta("Raw_Data/glss4_new/glss4_new/sec0b.dta")

####################################################################
############ Finsihed Data Imports and Testing #####################




########### Data Manipulation ######################################
####################################################################

### Pivot the live stock QTY to get it at the clust nh level

##############################################################
# Select columns wanted and rename ##### $$$$ !!!!!
lnd_amt_raw = sec8a1 %>%
  select(clust, nh, s8aq4, s8aq17) %>%
  rename(lnd_amt_owned = s8aq4, lnd_amt_s_crop = s8aq17)

# Select columns wanted and rename
liv_stk_piv = sec8a2 %>%
  select(clust, nh, livstcd, s8aq22a) %>%
  rename(livstqty = s8aq22a)

# Mutate the livestk code
liv_stk_piv<- mutate(liv_stk_piv, livstcd_base = 'live_stk_cd')
# Concat columns
liv_stk_piv$livstcd2 = paste(liv_stk_piv$livstcd_base,liv_stk_piv$livstcd)
# Drop the unneeded columns
liv_stk_piv = liv_stk_piv[ , c("clust", "nh", 'livstcd2', 'livstqty')] 
# Spread the df to be at the clust nh level
liv_stk_piv <- spread(liv_stk_piv, key=livstcd2, value=livstqty)

##############################################################

### Count the number of equipment used for clust and nh

##############################################################
agg_eqt_distinct <- sec8a3 %>% 
  group_by(clust, nh) %>%
  summarise(Unique_eqt_cnt = n_distinct(eqcdown)) %>% 
  arrange(., desc(Unique_eqt_cnt))

##############################################################

### Count the total units of equipment used for clust and nh

##############################################################
agg_eqt_count <- sec8a3 %>% 
  select(clust, nh, s8aq34) %>%
  group_by(clust, nh) %>%
  summarise(
    eqt_ttl = sum(s8aq34, na.rm = TRUE)
  )
##############################################################

### total land size by clust and nh but account for unit type conversion

##############################################################

# get the clust nh and profit at the household level
profit_prep <- agg2 %>%
  select(clust, nh, agri1c, agri2c) %>%
  group_by(clust, nh) %>%
  summarise(
    profit = sum(agri1c) + sum(agri2c)
  )

#subet the farm df to just important info
farm_subset_df = sec8b %>%
  select(clust, nh, s8bq4a, s8bq4b) %>%
  rename(agro_ent_size = s8bq4a, uom = s8bq4b) %>% 
  filter(uom != 4) # romove 4 as it an unlisted measurment type

# Func for size in acre
acre_size <- function(agro_ent_size, uom) {
  area = case_when(
    uom == 1 ~ agro_ent_size, # Acre
    uom == 2 ~ agro_ent_size, # Poles, 1 pole = 1 acre
    uom == 3 ~ agro_ent_size /9, # Rope is 9th of acre
    TRUE ~ 0
  )
  area
}

# apply the acreage function
farm_subset_df <- farm_subset_df %>%
  mutate(farm_area = acre_size(agro_ent_size, uom))

# get total farm area by acre
farm_area_df <- farm_subset_df %>%
  select(clust, nh, farm_area) %>%
  group_by(clust, nh) %>%
  summarise(
    area = sum(farm_area)
  )

profit_per_acre <- farm_area_df %>% 
  inner_join(profit_prep, by = c("clust", "nh")) %>% 
  select(clust, nh, area, profit) %>%
  mutate(profit_acre = profit/area)
  

#############################################################

### get the total qty and variety of crops

#############################################################
crop_df <- sec8c1 %>% 
  group_by(clust, nh) %>%
  summarise(crop_qty = sum(s8cq3a), 
            crop_variety = n_distinct(cropcd)) %>% 
  arrange(., desc(crop_qty))

#Find the max harvest crop per house and clust (What do that do most of)
max_crop_key <- aggregate(x= crop_df$crop_qty,
                          by= list(crop_df$clust, crop_df$nh),
                          FUN=max)

colnames(max_crop_key) <- c("clust", "nh", "s8cq3a")

sec8c1_temp <-sec8c1[ , c("clust", "nh", 'cropcd', 's8cq3a')] 
#Join the temp table to get most used crop
join_temp <- sec8c1_temp %>% 
  inner_join(max_crop_key, by = c("clust", "nh", 's8cq3a' ))
join_temp_df <-join_temp[ , c("clust", "nh", 'cropcd')]


#join the most used crop back to other crop info
crop_df <-  crop_df%>% 
  inner_join(join_temp_df, by = c("clust", "nh"))
colnames(crop_df) <- c("clust", "nh", "crop_qty", 'crop_variety', 'main_crop')

############################################################

### get the distinct and regional information at the clust an nh level

############################################################

# Create a 1:1 for nh clust region and distict
rgn_key <- select(sec0b, c(clust, nh, region))
rgn_key <- distinct(rgn_key)

###################################################################
########### End Data Manipulation #################################


########### Join Data Mastering  ###################################
###################################################################

### Join the data together ###
master_agro <- lnd_amt_raw %>% 
  inner_join(rgn_key, by = c("clust", "nh")) %>% 
  # left_join(liv_stk_piv, by = c("clust", "nh")) %>% 
  left_join(agg_eqt_distinct, by = c("clust", "nh")) %>% 
  left_join(crop_df, by = c("clust", "nh")) %>% 
  #left_join(other_farm, by = c("clust", "nh")) %>% 
  left_join(profit_per_acre, by = c("clust", "nh"))
  






































