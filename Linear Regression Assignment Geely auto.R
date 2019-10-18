
#### ---- Linear Regression Assignment ---- ####
rm(list = ls())
dev.off()

car_price <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = FALSE)

str(car_price)
View(car_price[1:10, ])


#### ---- Data Preparation ---- ####

# Checking for NAs
sum(is.na(car_price))
# There are no NAs in dataset

sum(car_price == "" | car_price == " ")
# No blanks

# Converting to lower case to avoid case duplicacy/mismatch
require(dplyr)
car_price <- mutate_if(car_price, is.character, toupper)

# Check unique values in each column
sapply(car_price, n_distinct)

# Checking for duplicates
sum(duplicated(car_price))

sum(duplicated(car_price[,-1]))
# There are no duplicates 


#### EDA begins ####

# Univariate Analysis 

names(car_price)

# 1. Car_ID is primary key (unique observation identifier) 
car_price$car_ID <- as.character(car_price$car_ID)


# 2. "Symboling" is in integer form but we know it is interval categorical
table(car_price$symboling)

car_price %>% group_by(symboling) %>% summarise(avg = mean(price)) %>% arrange(symboling)

car_price$symboling <- as.character(car_price$symboling)

# 3. "CarName" is a categorical variable and has 147 levels which is too high
length(unique(car_price$CarName))

require(stringr)
# Extracting "brand" from "CarName" to analyze if brand name has impact on price
car_brand <- str_split(car_price$CarName, pattern = " ", simplify = T, n = 2)
# n = 2 for splitting at only 1st pattern occurence.

car_price$brand <- car_brand[,1]

# Dropping CarName from further analysis
car_price$CarName <- NULL

sum(duplicated(car_price[,-1]))

# Checking levels of "brand"
length(unique(car_price$brand))

unique(car_price$brand)
# There are many brand names mis-spelled or short-formed

# Correcting the brand names
car_price$brand[which(car_price$brand == "MAXDA")] <- "MAZDA"
car_price$brand[which(car_price$brand == "PORCSHCE")] <- "PORSCHE"
car_price$brand[which(car_price$brand == "TOYOUTA")] <- "TOYOTA"
car_price$brand[which(car_price$brand == "VOKSWAGEN")] <- "VOLKSWAGEN"
car_price$brand[which(car_price$brand == "VW")] <- "VOLKSWAGEN"

unique(car_price$brand)
# Seems clean.


# 3 to 8 are categorical variables
sapply(car_price[, 3:8], table)


# 9. wheelbase
plot(car_price$wheelbase, car_price$price)
plot(sort(car_price$wheelbase))

# Outlier treatment
quantile(car_price$wheelbase, seq(0, 1, 0.01))
car_price$wheelbase[car_price$wheelbase > 115.544] <- 117

plot(sort(car_price$wheelbase))
plot(car_price$wheelbase, car_price$price)


# 10. carlength
plot(car_price$carlength, car_price$price)
plot(sort(car_price$carlength))

# Outlier treatment
quantile(car_price$carlength, seq(0, 1, 0.01))
car_price$carlength[car_price$carlength < 155.900] <- 155

plot(sort(car_price$carlength))
plot(car_price$carlength, car_price$price)


# 11. carwidth
plot(car_price$carwidth, car_price$price)
plot(quantile(car_price$carwidth, seq(0, 1, 0.01)))

quantile(car_price$carwidth, seq(0, 1, 0.01))
car_price$carwidth[car_price$carwidth < 63.600] <- 63

plot(quantile(car_price$carwidth, seq(0, 1, 0.01)))
plot(car_price$carwidth, car_price$price)


# 12. carheight
plot(car_price$carheight, car_price$price)
plot(quantile(car_price$carheight, seq(0, 1, 0.01)))


# 13. curbweight
plot(car_price$curbweight, car_price$price)
plot(quantile(car_price$curbweight, seq(0, 1, 0.01)))

quantile(car_price$curbweight, seq(0, 1, 0.01))
car_price$curbweight[car_price$curbweight < 1874.00] <- 1860

plot(quantile(car_price$carwidth, seq(0, 1, 0.01)))
plot(car_price$curbweight, car_price$price)


# 14,15 & 17 are categorical variables
sapply(car_price[, c(14,15,17)], table)


# 16. enginesize
plot(car_price$enginesize, car_price$price)
plot(quantile(car_price$enginesize, seq(0, 1, 0.01)))

quantile(car_price$enginesize, seq(0, 1, 0.01))
car_price$enginesize[car_price$enginesize > 209.00] <- 220

plot(quantile(car_price$enginesize, seq(0, 1, 0.01)))
plot(car_price$enginesize, car_price$price)


# 18. boreratio
plot(car_price$boreratio, car_price$price)
plot(quantile(car_price$boreratio, seq(0, 1, 0.01)))

quantile(car_price$boreratio, seq(0, 1, 0.01))
car_price$boreratio[car_price$boreratio < 2.91] <- 2.9

plot(quantile(car_price$boreratio, seq(0, 1, 0.01)))
plot(car_price$boreratio, car_price$price)


# 19. stroke
plot(car_price$stroke, car_price$price)
plot(quantile(car_price$stroke, seq(0, 1, 0.01)))

quantile(car_price$stroke, seq(0, 1, 0.01))
car_price$stroke[car_price$stroke < 2.64] <- 2.5

plot(quantile(car_price$stroke, seq(0, 1, 0.01)))
plot(car_price$stroke, car_price$price)


# 20. compressionratio
plot(car_price$compressionratio, car_price$price)
plot(quantile(car_price$compressionratio, seq(0, 1, 0.01)))

quantile(car_price$compressionratio, seq(0, 1, 0.01))
car_price$compressionratio[car_price$compressionratio > 10.9400] <- 11

plot(quantile(car_price$compressionratio, seq(0, 1, 0.01)))
plot(car_price$compressionratio, car_price$price)


# 21. horsepower
plot(car_price$horsepower, car_price$price)
plot(quantile(car_price$horsepower, seq(0, 1, 0.01)))

quantile(car_price$horsepower, seq(0, 1, 0.01))
car_price$horsepower[car_price$horsepower > 207] <- 210

plot(quantile(car_price$horsepower, seq(0, 1, 0.01)))
plot(car_price$horsepower, car_price$price)


# 22. peakrpm
plot(car_price$peakrpm, car_price$price)
plot(quantile(car_price$peakrpm, seq(0, 1, 0.01)))

quantile(car_price$peakrpm, seq(0, 1, 0.01))
car_price$peakrpm[car_price$peakrpm > 6000] <- 6100

plot(quantile(car_price$peakrpm, seq(0, 1, 0.01)))
plot(car_price$peakrpm, car_price$price)


# 23. citympg
plot(car_price$citympg, car_price$price)
plot(quantile(car_price$citympg, seq(0, 1, 0.01)))

quantile(car_price$citympg, seq(0, 1, 0.01))
car_price$citympg[car_price$citympg > 38] <- 40

plot(quantile(car_price$citympg, seq(0, 1, 0.01)))
plot(car_price$citympg, car_price$price)


# 24. highwaympg
plot(car_price$highwaympg, car_price$price)
plot(quantile(car_price$highwaympg, seq(0, 1, 0.01)))

quantile(car_price$highwaympg, seq(0, 1, 0.01))
car_price$highwaympg[car_price$highwaympg > 43] <- 44

plot(quantile(car_price$highwaympg, seq(0, 1, 0.01)))
plot(car_price$highwaympg, car_price$price)


# 25. Price
plot(sort(car_price$price))
plot(quantile(car_price$price, seq(0, 1, 0.01)))

# Let's not touch our target variable
str(car_price)


# It will be a good idea to separately analyse categorical and continuous variables
cat_vars <- car_price[ , sapply(car_price, is.character)]
num_vars <- car_price[ , sapply(car_price, is.numeric)]


# Categorical variables
summary(cat_vars)
cat_vars <- as.data.frame(sapply(cat_vars, as.factor))
summary(cat_vars)

require(dummies)
dummy.df <- dummy.data.frame(cat_vars)

# We have made dummy of Car_ID, which is wrong.

dummy.df <- dummy.data.frame(cat_vars[,-1])


# Continuous variables
summary(num_vars)

# Scale them
num_vars <- as.data.frame(sapply(num_vars, scale))
summary(num_vars)

cormat <- round(cor(num_vars), 2)
# require(corrplot)
# corrplot(cormat)

# We see heavy correlation among various variables.

# Final clean and ready dataset
final_data <- cbind(dummy.df, num_vars)

str(final_data)
# All categorical variables are converted to numeric dummy variables.

rm(car_brand, car_price, cat_vars, cormat, dummy.df, num_vars)


#### Model Building ####
set.seed(1234)
index <- sample(1:205, 0.70*205)

tr_data <- final_data[index, ]
ts_data <- final_data[-index, ]

# 1st model
lrm.1 <- lm(price ~ ., data = tr_data)
summary(lrm.1)

# Step-wise reduction
lrm.2 <- step(lrm.1, direction = 'both')

summary(lrm.2)
# Adjusted R-squared:  0.9568 

# Lets predict once here itself
pred_price <- predict(lrm.2, newdata = ts_data)

r = cor(pred_price, ts_data$price)
r^2
# There is considerable drop in Adjusted R-squared.

#################################################
#################################################

# Manually reducing variables
require(car)
sort(vif(lrm.2), decreasing = TRUE)

# enginesize. 2nd highest VIF and much higher p-value compared to curbweight
lrm.3 <- lm(formula = price ~ fueltypeDIESEL + doornumberFOUR + carbodyHATCHBACK + 
              enginelocationFRONT + enginetypeL + enginetypeOHC + enginetypeOHCV + 
              cylindernumberEIGHT + cylindernumberFIVE + cylindernumberFOUR + 
              cylindernumberSIX + fuelsystem1BBL + fuelsystem2BBL + fuelsystem4BBL + 
              fuelsystemMFI + `brandALFA-ROMERO` + brandAUDI + brandBMW + 
              brandBUICK + brandHONDA + brandJAGUAR + brandMAZDA + brandMITSUBISHI + 
              brandPLYMOUTH + brandPORSCHE + brandSAAB + brandVOLKSWAGEN + 
              wheelbase + carlength + carwidth + carheight + curbweight + 
              stroke + compressionratio + peakrpm + citympg, 
              data = tr_data)

summary(lrm.3)
# Adjusted R-squared:  0.9546. Negligible drop.
sort(vif(lrm.3), decreasing = TRUE)


# carlength. 2nd highest VIF and higher p-value
lrm.4 <- lm(formula = price ~ fueltypeDIESEL + doornumberFOUR + carbodyHATCHBACK + 
              enginelocationFRONT + enginetypeL + enginetypeOHC + enginetypeOHCV + 
              cylindernumberEIGHT + cylindernumberFIVE + cylindernumberFOUR + 
              cylindernumberSIX + fuelsystem1BBL + fuelsystem2BBL + fuelsystem4BBL + 
              fuelsystemMFI + `brandALFA-ROMERO` + brandAUDI + brandBMW + 
              brandBUICK + brandHONDA + brandJAGUAR + brandMAZDA + brandMITSUBISHI + 
              brandPLYMOUTH + brandPORSCHE + brandSAAB + brandVOLKSWAGEN + 
              wheelbase + carwidth + carheight + curbweight + 
              stroke + compressionratio + peakrpm + citympg, 
              data = tr_data)

summary(lrm.4)

sort(vif(lrm.4), decreasing = TRUE)


# cylindernumberSIX. 3rd highest VIF and higher p-value.
lrm.5 <- lm(formula = price ~ fueltypeDIESEL + doornumberFOUR + carbodyHATCHBACK + 
              enginelocationFRONT + enginetypeL + enginetypeOHC + enginetypeOHCV + 
              cylindernumberEIGHT + cylindernumberFIVE + cylindernumberFOUR + 
              fuelsystem1BBL + fuelsystem2BBL + fuelsystem4BBL + 
              fuelsystemMFI + `brandALFA-ROMERO` + brandAUDI + brandBMW + 
              brandBUICK + brandHONDA + brandJAGUAR + brandMAZDA + brandMITSUBISHI + 
              brandPLYMOUTH + brandPORSCHE + brandSAAB + brandVOLKSWAGEN + 
              wheelbase + carwidth + carheight + curbweight + stroke + 
              compressionratio + peakrpm + citympg, data = tr_data)

summary(lrm.5)

sort(vif(lrm.5), decreasing = TRUE)


# citympg. 2nd highest VIF and higher p-value.
lrm.6 <- lm(formula = price ~ fueltypeDIESEL + doornumberFOUR + carbodyHATCHBACK + 
              enginelocationFRONT + enginetypeL + enginetypeOHC + enginetypeOHCV + 
              cylindernumberEIGHT + cylindernumberFIVE + cylindernumberFOUR + 
              fuelsystem1BBL + fuelsystem2BBL + fuelsystem4BBL + 
              fuelsystemMFI + `brandALFA-ROMERO` + brandAUDI + brandBMW + 
              brandBUICK + brandHONDA + brandJAGUAR + brandMAZDA + brandMITSUBISHI + 
              brandPLYMOUTH + brandPORSCHE + brandSAAB + brandVOLKSWAGEN + 
              wheelbase + carwidth + carheight + curbweight + stroke + 
              compressionratio + peakrpm, data = tr_data)

summary(lrm.6)

sort(vif(lrm.6), decreasing = TRUE)


# fuelsystem1BBL. highest VIF and higher p-value than others with high VIF.
lrm.7 <- lm(formula = price ~ fueltypeDIESEL + doornumberFOUR + carbodyHATCHBACK + 
              enginelocationFRONT + enginetypeL + enginetypeOHC + enginetypeOHCV + 
              cylindernumberEIGHT + cylindernumberFIVE + cylindernumberFOUR + 
              fuelsystem2BBL + fuelsystem4BBL + 
              fuelsystemMFI + `brandALFA-ROMERO` + brandAUDI + brandBMW + 
              brandBUICK + brandHONDA + brandJAGUAR + brandMAZDA + brandMITSUBISHI + 
              brandPLYMOUTH + brandPORSCHE + brandSAAB + brandVOLKSWAGEN + 
              wheelbase + carwidth + carheight + curbweight + stroke + 
              compressionratio + peakrpm, data = tr_data)

summary(lrm.7)

sort(vif(lrm.7), decreasing = TRUE)


# wheelbase. 4th highest VIF and higher p-value than others with high VIF.
lrm.8 <- lm(formula = price ~ fueltypeDIESEL + doornumberFOUR + carbodyHATCHBACK + 
              enginelocationFRONT + enginetypeL + enginetypeOHC + enginetypeOHCV + 
              cylindernumberEIGHT + cylindernumberFIVE + cylindernumberFOUR + 
              fuelsystem2BBL + fuelsystem4BBL + fuelsystemMFI + `brandALFA-ROMERO` + 
              brandAUDI + brandBMW + brandBUICK + brandHONDA + brandJAGUAR + 
              brandMAZDA + brandMITSUBISHI + brandPLYMOUTH + brandPORSCHE + 
              brandSAAB + brandVOLKSWAGEN + carwidth + carheight + 
              curbweight + stroke + compressionratio + peakrpm, data = tr_data)

summary(lrm.8)
# Adjusted R-squared:  0.95

sort(vif(lrm.8), decreasing = TRUE)

# 3 highest VIF are all highly significant variables.
# Lets check their correlation with each other.

cor(tr_data$curbweight, tr_data$cylindernumberFIVE)
cor(tr_data$curbweight, tr_data$carwidth)
cor(tr_data$carwidth, tr_data$cylindernumberFIVE)

# Lets remove them one by one and check which causes least drop in R-squared.

# curbweight
lrm.9.1 <- lm(formula = price ~ fueltypeDIESEL + doornumberFOUR + carbodyHATCHBACK + 
                 enginelocationFRONT + enginetypeL + enginetypeOHC + enginetypeOHCV + 
                 cylindernumberEIGHT + cylindernumberFIVE + cylindernumberFOUR + 
                 fuelsystem2BBL + fuelsystem4BBL + fuelsystemMFI + `brandALFA-ROMERO` + 
                 brandAUDI + brandBMW + brandBUICK + brandHONDA + brandJAGUAR + 
                 brandMAZDA + brandMITSUBISHI + brandPLYMOUTH + brandPORSCHE + 
                 brandSAAB + brandVOLKSWAGEN + carwidth + carheight + 
                 stroke + compressionratio + peakrpm, data = tr_data)

summary(lrm.9.1)
# Adjusted R-squared:  0.9325 

# carwidth
lrm.9.2 <- lm(formula = price ~ fueltypeDIESEL + doornumberFOUR + carbodyHATCHBACK + 
                enginelocationFRONT + enginetypeL + enginetypeOHC + enginetypeOHCV + 
                cylindernumberEIGHT + cylindernumberFIVE + cylindernumberFOUR + 
                fuelsystem2BBL + fuelsystem4BBL + fuelsystemMFI + `brandALFA-ROMERO` + 
                brandAUDI + brandBMW + brandBUICK + brandHONDA + brandJAGUAR + 
                brandMAZDA + brandMITSUBISHI + brandPLYMOUTH + brandPORSCHE + 
                brandSAAB + brandVOLKSWAGEN + carheight + curbweight + 
                stroke + compressionratio + peakrpm, data = tr_data)

summary(lrm.9.2)
# Adjusted R-squared:  0.9288 

# cylindernumberFIVE
lrm.9.3 <- lm(formula = price ~ fueltypeDIESEL + doornumberFOUR + carbodyHATCHBACK + 
                enginelocationFRONT + enginetypeL + enginetypeOHC + enginetypeOHCV + 
                cylindernumberEIGHT + cylindernumberFOUR + 
                fuelsystem2BBL + fuelsystem4BBL + fuelsystemMFI + `brandALFA-ROMERO` + 
                brandAUDI + brandBMW + brandBUICK + brandHONDA + brandJAGUAR + 
                brandMAZDA + brandMITSUBISHI + brandPLYMOUTH + brandPORSCHE + 
                brandSAAB + brandVOLKSWAGEN + carwidth + carheight + curbweight + 
                stroke + compressionratio + peakrpm, data = tr_data)

summary(lrm.9.3)
# Adjusted R-squared:  0.9389 
# Thus least drop in Adjusted R-squared. So we drop cylindernumberFIVE in this iteration.

sort(vif(lrm.9.3), decreasing = TRUE)


# Now again, curbwieght and carwidth are with highest VIFs, and are highly significant too.
# Lets check one by one.

# dropping curbweight
lrm.10.1 <- lm(formula = price ~ fueltypeDIESEL + doornumberFOUR + carbodyHATCHBACK + 
               enginelocationFRONT + enginetypeL + enginetypeOHC + enginetypeOHCV + 
               cylindernumberEIGHT + cylindernumberFOUR + 
               fuelsystem2BBL + fuelsystem4BBL + fuelsystemMFI + `brandALFA-ROMERO` + 
               brandAUDI + brandBMW + brandBUICK + brandHONDA + brandJAGUAR + 
               brandMAZDA + brandMITSUBISHI + brandPLYMOUTH + brandPORSCHE + 
               brandSAAB + brandVOLKSWAGEN + carwidth + carheight + 
               stroke + compressionratio + peakrpm, data = tr_data)

summary(lrm.10.1)
# Drop in R^2 of ~0.02


# carwidth
lrm.10.2 <- lm(formula = price ~ fueltypeDIESEL + doornumberFOUR + carbodyHATCHBACK + 
                 enginelocationFRONT + enginetypeL + enginetypeOHC + enginetypeOHCV + 
                 cylindernumberEIGHT + cylindernumberFOUR + 
                 fuelsystem2BBL + fuelsystem4BBL + fuelsystemMFI + `brandALFA-ROMERO` + 
                 brandAUDI + brandBMW + brandBUICK + brandHONDA + brandJAGUAR + 
                 brandMAZDA + brandMITSUBISHI + brandPLYMOUTH + brandPORSCHE + 
                 brandSAAB + brandVOLKSWAGEN + carheight + curbweight + 
                 stroke + compressionratio + peakrpm, data = tr_data)

summary(lrm.10.2)
# Drop in R^2 of ~0.01
# So we drop carwidth.


# Lets check impact of dropping these high VIF variables ignoring the p-values.
# Lets predict again before proceeding.

pred_price <- predict(lrm.10.2, newdata = ts_data)

r = cor(pred_price, ts_data$price)
r^2

# r^2 of training nad prediction is almost same.
# Overfitting is gone !


# Lets proceed
sort(vif(lrm.10.2), decreasing = TRUE)

# dropping fueltypeDIESEL. 2nd highest VIF and insignificant as per p-value
lrm.11 <- lm(formula = price ~ doornumberFOUR + carbodyHATCHBACK + 
               enginelocationFRONT + enginetypeL + enginetypeOHC + enginetypeOHCV + 
               cylindernumberEIGHT + cylindernumberFOUR + fuelsystem2BBL + 
               fuelsystem4BBL + fuelsystemMFI + `brandALFA-ROMERO` + brandAUDI + 
               brandBMW + brandBUICK + brandHONDA + brandJAGUAR + brandMAZDA + 
               brandMITSUBISHI + brandPLYMOUTH + brandPORSCHE + brandSAAB + 
               brandVOLKSWAGEN + carheight + curbweight + stroke + compressionratio + 
               peakrpm, data = tr_data)

summary(lrm.11)
sort(vif(lrm.11), decreasing = TRUE)


# cylindernumberFOUR. 2nd highest VIF and insignificant as per p-value.
lrm.12 <- lm(formula = price ~ doornumberFOUR + carbodyHATCHBACK + 
               enginelocationFRONT + enginetypeL + enginetypeOHC + enginetypeOHCV + 
               cylindernumberEIGHT + fuelsystem2BBL + 
               fuelsystem4BBL + fuelsystemMFI + `brandALFA-ROMERO` + brandAUDI + 
               brandBMW + brandBUICK + brandHONDA + brandJAGUAR + brandMAZDA + 
               brandMITSUBISHI + brandPLYMOUTH + brandPORSCHE + brandSAAB + 
               brandVOLKSWAGEN + carheight + curbweight + stroke + compressionratio + 
               peakrpm, data = tr_data)

summary(lrm.12)
sort(vif(lrm.12), decreasing = TRUE)


# Now VIFs are controlled for all except curbweight.
# And curbweight is highly significant variable
# So lets remove variables by p-value only now.


# enginetypeOHCV
lrm.13 <- lm(formula = price ~ doornumberFOUR + carbodyHATCHBACK + enginelocationFRONT + 
               enginetypeL + enginetypeOHC + cylindernumberEIGHT + 
               fuelsystem2BBL + fuelsystem4BBL + fuelsystemMFI + `brandALFA-ROMERO` + 
               brandAUDI + brandBMW + brandBUICK + brandHONDA + brandJAGUAR + 
               brandMAZDA + brandMITSUBISHI + brandPLYMOUTH + brandPORSCHE + 
               brandSAAB + brandVOLKSWAGEN + carheight + curbweight + stroke + 
               compressionratio + peakrpm, data = tr_data)

summary(lrm.13)


# brandHONDA
lrm.14 <- lm(formula = price ~ doornumberFOUR + carbodyHATCHBACK + enginelocationFRONT + 
               enginetypeL + enginetypeOHC + cylindernumberEIGHT + 
               fuelsystem2BBL + fuelsystem4BBL + fuelsystemMFI + `brandALFA-ROMERO` + 
               brandAUDI + brandBMW + brandBUICK + brandJAGUAR + 
               brandMAZDA + brandMITSUBISHI + brandPLYMOUTH + brandPORSCHE + 
               brandSAAB + brandVOLKSWAGEN + carheight + curbweight + stroke + 
               compressionratio + peakrpm, data = tr_data)

summary(lrm.14)


# compressionratio
lrm.15 <- lm(formula = price ~ doornumberFOUR + carbodyHATCHBACK + enginelocationFRONT + 
               enginetypeL + enginetypeOHC + cylindernumberEIGHT + 
               fuelsystem2BBL + fuelsystem4BBL + fuelsystemMFI + `brandALFA-ROMERO` + 
               brandAUDI + brandBMW + brandBUICK + brandJAGUAR + 
               brandMAZDA + brandMITSUBISHI + brandPLYMOUTH + brandPORSCHE + 
               brandSAAB + brandVOLKSWAGEN + carheight + curbweight + stroke + 
               peakrpm, data = tr_data)

summary(lrm.15)


# fuelsystem2BBL
lrm.16 <- lm(formula = price ~ doornumberFOUR + carbodyHATCHBACK + enginelocationFRONT + 
               enginetypeL + enginetypeOHC + cylindernumberEIGHT + 
               fuelsystem4BBL + fuelsystemMFI + `brandALFA-ROMERO` + brandAUDI + 
               brandBMW + brandBUICK + brandJAGUAR + brandMAZDA + brandMITSUBISHI + 
               brandPLYMOUTH + brandPORSCHE + brandSAAB + brandVOLKSWAGEN + 
               carheight + curbweight + stroke + peakrpm, data = tr_data)

summary(lrm.16)


# doornumberFOUR
lrm.17 <- lm(formula = price ~ carbodyHATCHBACK + enginelocationFRONT + 
               enginetypeL + enginetypeOHC + cylindernumberEIGHT + 
               fuelsystem4BBL + fuelsystemMFI + `brandALFA-ROMERO` + brandAUDI + 
               brandBMW + brandBUICK + brandJAGUAR + brandMAZDA + brandMITSUBISHI + 
               brandPLYMOUTH + brandPORSCHE + brandSAAB + brandVOLKSWAGEN + 
               carheight + curbweight + stroke + peakrpm, data = tr_data)

summary(lrm.17)


# brandPLYMOUTH
lrm.18 <- lm(formula = price ~ carbodyHATCHBACK + enginelocationFRONT + 
               enginetypeL + enginetypeOHC + cylindernumberEIGHT + 
               fuelsystem4BBL + fuelsystemMFI + `brandALFA-ROMERO` + brandAUDI + 
               brandBMW + brandBUICK + brandJAGUAR + brandMAZDA + brandMITSUBISHI + 
               brandPORSCHE + brandSAAB + brandVOLKSWAGEN + 
               carheight + curbweight + stroke + peakrpm, data = tr_data)

summary(lrm.18)


# enginetypeL
lrm.19 <- lm(formula = price ~ carbodyHATCHBACK + enginelocationFRONT + 
               enginetypeOHC + cylindernumberEIGHT + 
               fuelsystem4BBL + fuelsystemMFI + `brandALFA-ROMERO` + brandAUDI + 
               brandBMW + brandBUICK + brandJAGUAR + brandMAZDA + brandMITSUBISHI + 
               brandPORSCHE + brandSAAB + brandVOLKSWAGEN + 
               carheight + curbweight + stroke + peakrpm, data = tr_data)

summary(lrm.19)


# fuelsystem4BBL
lrm.20 <- lm(formula = price ~ carbodyHATCHBACK + enginelocationFRONT + 
               enginetypeOHC + cylindernumberEIGHT + fuelsystemMFI + 
               `brandALFA-ROMERO` + brandAUDI + brandBMW + brandBUICK + 
               brandJAGUAR + brandMAZDA + brandMITSUBISHI + brandPORSCHE + 
               brandSAAB + brandVOLKSWAGEN + carheight + curbweight + stroke + 
               peakrpm, data = tr_data)

summary(lrm.20)


# brandSAAB
lrm.21 <- lm(formula = price ~ carbodyHATCHBACK + enginelocationFRONT + 
               enginetypeOHC + cylindernumberEIGHT + fuelsystemMFI + 
               `brandALFA-ROMERO` + brandAUDI + brandBMW + brandBUICK + 
               brandJAGUAR + brandMAZDA + brandMITSUBISHI + brandPORSCHE + 
               brandVOLKSWAGEN + carheight + curbweight + stroke + 
               peakrpm, data = tr_data)

summary(lrm.21)


# fuelsystemMFI
lrm.22 <- lm(formula = price ~ carbodyHATCHBACK + enginelocationFRONT + 
               enginetypeOHC + cylindernumberEIGHT + 
               `brandALFA-ROMERO` + brandAUDI + brandBMW + brandBUICK + 
               brandJAGUAR + brandMAZDA + brandMITSUBISHI + brandPORSCHE + 
               brandVOLKSWAGEN + carheight + curbweight + stroke + 
               peakrpm, data = tr_data)

summary(lrm.22)


# carbodyHATCHBACK
lrm.23 <- lm(formula = price ~ enginelocationFRONT + 
               enginetypeOHC + cylindernumberEIGHT + 
               `brandALFA-ROMERO` + brandAUDI + brandBMW + brandBUICK + 
               brandJAGUAR + brandMAZDA + brandMITSUBISHI + brandPORSCHE + 
               brandVOLKSWAGEN + carheight + curbweight + stroke + 
               peakrpm, data = tr_data)

summary(lrm.23)


# brandVOLKSWAGEN
lrm.24 <- lm(formula = price ~ enginelocationFRONT + enginetypeOHC + cylindernumberEIGHT + 
               `brandALFA-ROMERO` + brandAUDI + brandBMW + brandBUICK + 
               brandJAGUAR + brandMAZDA + brandMITSUBISHI + brandPORSCHE + 
               carheight + curbweight + stroke + peakrpm, 
               data = tr_data)

summary(lrm.24)


# stroke
lrm.25 <- lm(formula = price ~ enginelocationFRONT + enginetypeOHC + cylindernumberEIGHT + 
               `brandALFA-ROMERO` + brandAUDI + brandBMW + brandBUICK + 
               brandJAGUAR + brandMAZDA + brandMITSUBISHI + brandPORSCHE + 
               carheight + curbweight + peakrpm, data = tr_data)

summary(lrm.25)
# Now all variables are significant.

# Lets check VIF again.
sort(vif(lrm.25), decreasing = TRUE)
# VIF of curbweight is also controlled now.

# So this is our final model
fin_lrm <- lrm.25

rm(lrm.1,lrm.2,lrm.3,lrm.4,lrm.5,lrm.6,lrm.7,lrm.8,lrm.9.1,lrm.9.2,lrm.9.3,
   lrm.10.1,lrm.10.2,lrm.11,lrm.12,lrm.13,lrm.14,lrm.15,lrm.16,lrm.17,
   lrm.18,lrm.19,lrm.20,lrm.21,lrm.22,lrm.23,lrm.24,lrm.25)

dev.off()

#### ---- Predicting the results in test dataset ---- ####
pred_price <- predict(fin_lrm, ts_data)

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(ts_data$price, pred_price)

rsquared <- r^2

rsquared
summary(fin_lrm)

# Our model is predicting as good as it is trained.

################################################################
# Now, can we descibe to someone - 
# Top 5 independent factors which can describe the price of a car ???


#### Something for practice ----

# Brand as a variable ..... Really ???
# (Try without using brand variable)
################################################################

#### ---- Deriving metrics ---- ##### 

################################################################


# Derived metrics could  be

# 1. Parking area required by a car = carlength X carwidth

# 2. Mileage to Horse Power relation = mpg(city or highway) / horsepower

# 3. Car weight to engine size ratio = curbweight / enginesize

# 4. Difference between city & highway mileage = highwaympg - citympg

# Give it a try again with derived metrics !!!

################################################################
