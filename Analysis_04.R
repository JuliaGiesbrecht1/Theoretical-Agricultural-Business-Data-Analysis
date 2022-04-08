#Modeling

# Backward feature elimination
fitall <- lm(profit_acre ~ ., data = master_df)
summary(fitall)

step(fitall, direction = "backward")

# Recopmended Varaibls to look into
backward_lm <- lm(formula = profit_acre ~ clust + nh + area + region + 
                    Unique_eqt_cnt + crop_qty + crop_variety + Education_Attainment + 
                    Vaccination_Status + b_Health_conditions_in_last_two_weeks + 
                    crop_acre, data = master_df)
summary(backward_lm)


# Basic analysis
#####################################
# Box plot
#####################################
# Plot boxplot using ggplot function
plot <- ggplot(master_df, aes(x=factor(region), y=profit_acre))+
  geom_boxplot()+
  theme( legend.position = "none" )
plot


#####################################
#density
#####################################

theme_set(
  theme_classic() + 
    theme(legend.position = "top")
)

# profit density per acre by rgn
ggplot(master_df, aes(x = profit_acre)) +
  geom_density(aes(color = factor(region), fill = factor(region)), 
               position = "identity", alpha = 0.1) +
  coord_cartesian(xlim = c(-100000,1000000))


# profit density per acre by crp
ggplot(master_df, aes(x = profit_acre)) +
  geom_density(aes(color = factor(crop_variety), fill = factor(crop_variety)), 
               position = "identity", alpha = 0.1) +
  coord_cartesian(xlim = c(-100000,1000000))


# profit density per acre by education attainment
ggplot(master_df, aes(x = profit_acre)) +
  geom_density(aes(color = factor(Education_Attainment), fill = factor(Education_Attainment)), 
               position = "identity", alpha = 0.1) +
  coord_cartesian(xlim = c(-100000,1000000))

# profit density per acre by num children
ggplot(master_df, aes(x = profit_acre)) +
  geom_density(aes(color = factor(Total_Number_of_children_born), fill = factor(Total_Number_of_children_born)), 
               position = "identity", alpha = 0.1) +
  coord_cartesian(xlim = c(-100000,1000000))



colnames(master_df)
#####################
# Scatter plot
#####################

#plot the crop and profit per
# Profit per acre vs total crops
plot(master_df$crop_qty, master_df$profit_acre, 
     main = "R Scatter Plot",
     xlab = "Profit_per_acr",
     ylab = "crop_qty",
     las = 1,
     ylim = c(-10000, 10000000), 
     xlim = c(-10, 1000))

plot(master_df$crop_acre, master_df$profit_acre, 
     main = "R Scatter Plot",
     xlab = "crop_per_acr",
     ylab = "profit_per_acre",
     las = 1,
     ylim = c(-10000, 10000000), 
     xlim = c(-10, 1000))
abline(lm(master_df$profit_acre~master_df$crop_acre), col = "red", lwd = 3)

##########
#Correlation
##########
ggcorrplot(cor(x = master_df)) 




## Modeling

#Basic model from the backwards feature selection choice on the profit per acre
model1 <- lm(formula = profit_acre ~ clust + nh + area + region + 
               Unique_eqt_cnt + crop_qty + crop_variety + Education_Attainment + 
               Vaccination_Status + b_Health_conditions_in_last_two_weeks + 
               crop_acre, data = master_df)

summary(model1)

#Facroing the regio -- slight model improvment and has significant regions
model2 <- lm(formula = profit_acre ~ clust + nh + area + factor(region) + 
               Unique_eqt_cnt + crop_qty + crop_variety + Education_Attainment + 
               Vaccination_Status + b_Health_conditions_in_last_two_weeks + 
               crop_acre, data = master_df)

summary(model2)


