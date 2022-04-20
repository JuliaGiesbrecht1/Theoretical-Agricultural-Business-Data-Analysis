library(corrplot)

# Backward feature elimination
fitall <- lm(profit_acre ~ .-clust-nh, data = master_df)
summary(fitall)
# Pick the main variables
acme_var = master_df %>% select(profit_acre, region, Unique_eqt_cnt, area,
                                crop_acre, Education_Attainment,
                                b_Health_conditions_in_last_two_weeks, nh_num_people)
summary(acme_var)
# Get correlations
c <- cor(acme_var %>% select(!c("profit_acre")))
corrplot(c, method="circle")
# Regress profit by acre on the main acme set
main <- lm(profit_acre ~ region + Unique_eqt_cnt + area + crop_acre + 
             Education_Attainment + b_Health_conditions_in_last_two_weeks + 
             nh_num_people, data=acme_var)
summary(main)
# Plot model diagnostics starting with the standardized residuals.
hist(rstandard(main), xlab="Standardized Residuals")
# Now plot the residuals vs fitted.
plot(fitted(main), resid(main), xlab="Fitted", ylab="Residuals")


# Regress profit by acre on all with factor(region) and education^2
edu <- lm(profit_acre ~ factor(region) + Unique_eqt_cnt + area + crop_acre + nh_num_people +
            I(Education_Attainment^2) + Education_Attainment + b_Health_conditions_in_last_two_weeks, data=acme_var)
summary(edu)
# Plot model diagnostics starting with the standardized residuals.
hist(rstandard(edu), xlab="Standardized Residuals")
# Now plot the residuals vs fitted.
plot(fitted(edu), resid(edu), xlab="Fitted", ylab="Residuals")

