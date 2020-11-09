cat("\014")

# USED CARS PRICE ESTIMATION #####################
# NATIONAL COLLEGE OF IRELAND 
# POSTGRAD IN SCIENCE IN DATA ANALYTICS
# Project for Data Mining and Machine Learning I
# by Jordi Batlle (x17133246)


# LOAD LIBRARIES ################################
# Reset outputs
rm(list = ls()) 
dev.off()

pacman::p_load(pacman, rio, tidyverse, dplyr, mice, corrplot, cowplot, caret, skimr, glmnet, randomForest)


# LOAD FILES ####################################
# used-cars-listing-from-ebay --> Germany
# secondhand-car-price-estimation --> Australia
# online-adds-of-used-cars --> Spain
# craigslist-carstrucks-data --> States

# Import CSV file
#setwd("/Users/jordi/Documents/_POSTGRAD/Data Mining and Machine Learning I/Project/x17133246 DMproject/")
Germany <- read_csv("used-cars-listing-from-ebay.csv")

# RETRIEVE SOME INFO ABOUT THE DATASET ##########
head(Germany)
glimpse(Germany)
summary(Germany)
# 335,111 rows
# 16 variables, many of them as character




# Remove obvious useless variables for the project objective:
# date_crawled,name,registration_month, ad_created, last_seen_online, postal_code
Germany <- subset(Germany, select=-c(date_crawled,name,registration_month, ad_created, last_seen_online,postal_code))

# Recode character variables into factor
cols <- c('vehicle_type','gearbox','model','fuel_type','brand','unrepaired_damage')
Germany[cols] <- lapply(Germany[cols], factor)
rm(cols)

# Reorder columns (target+numeric+factor)
Germany <- Germany[, c(1, 3, 7, 5, 2, 9, 6, 4, 8, 10)]

# Check
glimpse(Germany)


# CHECK MISSING VALUES #############################################
# Pct. of missing values per column
sort(sapply(Germany,function(x) round(sum(is.na(x))/nrow(Germany)*100,1)), decreasing=TRUE)

# VARIABLES LOOKUP##############################
# Price variable ###############################
summary(Germany$dollar_price)
# We can see the range is 500 - 245,000

# Lets check variable distribution
boxplot(Germany$dollar_price, 
        horizontal = T,  
        notch  = T,
        main   = "PRICE BOXPLOT",
        xlab   = "Used car price",
        col='lightsalmon'
)
hist(Germany$dollar_price, 
     breaks = 20, 
     main   = "PRICE COUNT",
     ylab   = "",
     xlab   = "Used car price",
     col='cadetblue'
)

# Lets check the mean and sd for price
(m <- mean(Germany$dollar_price))
(s <- sd(Germany$dollar_price))

# on the lower limit, leave 500 as in other datasetS
# on the upper limit we can set 15000 (approx. m+sd)
count(Germany[Germany$dollar_price > 12000, ])
Germany <- Germany[Germany$dollar_price <= 12000, ]

# Final distribution of price variable
summary(Germany$dollar_price)
boxplot(Germany$dollar_price, 
        horizontal = T,  
        notch  = T,
        main   = "CAR PRICE BOXPLOT",
        xlab   = "Used car prices",
        col='lightsalmon'
        )
hist(Germany$dollar_price, 
     breaks = 20, 
     main   = "CAR PRICES DISTRIBUTION",
     ylab   = "",
     xlab   = "Used car prices",
     col='cadetblue'
     )
# there are still some outliers but close enough to upper limit
# we see that prices taken from ebay are very cheap compared to dealers we've seen in other datasets


# Unrepaired_damage variable ###############################
summary(Germany$unrepaired_damage)

#lets remove damaged cars and NA as there is no other indicator to help to imput
Germany <- subset(Germany, unrepaired_damage != 'ja')
# then we can drop the variable
Germany <- subset(Germany, select=-c(unrepaired_damage))


# Registration_year variable ##############################
table(Germany$registration_year, useNA = "always")

boxplot(Germany$registration_year,        
        horizontal = T,  
        notch  = T,
        main   = "REGISTRATION YEAR BOXPLOT",
        xlab   = "Registration Year",
        col='lightsalmon'
)
hist(Germany$registration_year, 
     breaks = 20, 
     main   = "REGISTRATION YEAR COUNT",
     ylab   = "",
     xlab   = "Registration Year",
     col='cadetblue'
)

# There are some old cars, and we can see the count start to increase in 1988
# I will set the lower limit here: max car age 28 
count(Germany[Germany$registration_year < 1988, ])
Germany <- subset(Germany, registration_year >= 1988)

# On the other hand, dataset was collected 2016
# Remove the most recent cars
count(Germany[Germany$registration_year == 2016, ])
Germany <- Germany[Germany$registration_year != 2016, ]

# Final distribution of registration_year variable
table(Germany$registration_year, useNA = "always")
boxplot(Germany$registration_year,        
        horizontal = T,  
        notch  = T,
        main   = "REGISTRATION YEAR BOXPLOT",
        xlab   = "Registration Year",
        col='lightsalmon'
        )
hist(Germany$registration_year, 
     breaks = 20, 
     main   = "REGISTRATION YEAR COUNT",
     ylab   = "",
     xlab   = "Car Registration Year",
     col='cadetblue'
     )


# Kilometer variable ##############################
summary(Germany$kilometer)
# no missing values and range between 5,000 - 150,000 could work for the research
# But...
boxplot(Germany$kilometer,        
        horizontal = T,  
        notch  = T,
        main   = "KILOMETRES BOXLOT",
        xlab   = "Kilometres",
        col='lightsalmon'
)
hist(Germany$kilometer, 
     breaks = 20, 
     main   = "KILOMETRES COUNT",
     ylab   = "",
     xlab   = "Kilometres",
     col='cadetblue'
)

# Check the values over 100,000
samp <- Germany[Germany$kilometer > 100000, ]
hist(samp$kilometer, 
     breaks = 20, 
     main   = "KILOMETRES COUNT",
     ylab   = "",
     xlab   = "Kilometres",
     col='cadetblue'
)

# It looks like the values are not accurate
# I remove those rows 
# although it takes most of the dataset, a signficant sample remains
count(Germany[Germany$kilometer > 100000, ])
Germany <- subset(Germany, kilometer <= 100000)

# Final distribution of kilometres variable
summary(Germany$kilometer)
boxplot(Germany$kilometer,        
        horizontal = T,  
        notch  = T,
        main   = "KILOMETRES BOXLOT",
        xlab   = "Kilometres",
        col='lightsalmon'
)
hist(Germany$kilometer, 
     breaks = 10, 
     main   = "KILOMETRES COUNT",
     ylab   = "",
     xlab   = "Kilometres",
     col='cadetblue'
)


# Brand variable ###############################
summary(Germany$brand)
Germany %>%
  select(brand) %>%
  table() %>%
  sort(decreasing = T) %>%
  barplot(
    main   = "BRANDS COUNT",
    las=2,
    col='cadetblue'
  )
# German brands dominate!

# Model variable ###############################
# Missing values:
summary(Germany$model)
# Number of models per brand
samp <- aggregate(Germany[, 'model'], by=list(brand = Germany$brand), FUN=n_distinct)
samp[order(samp$model, decreasing = TRUE),]


# Fuel_type variable ###############################
summary(Germany$fuel_type)
# Recode NA to other
Germany$fuel_type[is.na(Germany$fuel_type)] <- 'other'

Germany %>%
  select(fuel_type) %>%
  table() %>%
  sort(decreasing = T) %>%
  barplot(
    main   = "FUEL TYPE COUNT",
    las=2,
    col='cadetblue'
  )


# Power_ps variable ###############################
summary(Germany$power_ps)
# Number of factors
n_distinct(Germany$power_ps)

Germany %>%
  select(power_ps) %>%
  table() %>%
  barplot(
    main   = "POWER PS COUNT",
    las=2,
    col='cadetblue'
  )
# Unfortunately this variable shows very strange figures
# which do not correspond to power ps values
# will be safer to drop it
Germany <- subset(Germany, select=-c(power_ps))


# Gearbox variable ###############################
# Missing values:
summary(Germany$gearbox)
Germany %>%
  select(gearbox) %>%
  table() %>%
  sort(decreasing = T) %>%
  barplot(
  main   = "GEARBOX COUNT",
  col='cadetblue'
  )


# Vehicle_type variable ###############################
# Missing values:
summary(Germany$vehicle_type)
# Remove bus type
Germany <- subset(Germany, vehicle_type != 'bus' | (is.na(vehicle_type)))

Germany %>%
  select(vehicle_type) %>%
  table() %>%
  sort(decreasing = T) %>%
  barplot(
    main   = "VEHICLE TYPE COUNT",
    las=2,
    col='cadetblue'
  )


# DROP UNUSED LEVELS IN THE DATASET #############################
Germany <- droplevels(Germany)   
rm(samp, m, s)


# IMPUT MISSING VALUES ##################################
# Pct. of missing values per column
sort(sapply(Germany,function(x) sum(is.na(x))))
# Check the distribution of the mising values
md.pattern(Germany, plot=FALSE)

# Lets remove rows with more than 1 missing value
Germany <- subset(Germany, rowSums(is.na(Germany)) < 2)
# And imput numeric variables with kNN
# first store price as we dont want to transform it
y <- Germany$dollar_price
set.seed(123)
Imput <- preProcess(Germany, method='knnImpute')
Germany <- predict(Imput, newdata=Germany)
Germany$dollar_price <- y
rm(Imput)
# Finally drop remaining rows with missing values in categorical variables
Germany <- na.omit(Germany)


# DATA VISUALISATION ###########################################

# Final variables overview
skimmed <- skim(Germany)
skimmed[, c(1:2,6:15)]
rm(skimmed)

# Average prices by variables
a <- ggplot(data=Germany, aes(x=reorder(brand,dollar_price), y=dollar_price)) +
  ggtitle("PRICE AVG by BRAND") +
  geom_bar(stat='summary',fill ='cadetblue') +
  theme(axis.title.x=element_blank(), axis.text.x=element_text(angle=90))

b <- ggplot(data=Germany, aes(x=reorder(vehicle_type,dollar_price), y=dollar_price)) +
  ggtitle("PRICE AVG by BODY") +
  geom_bar(stat='summary',fill ='cadetblue') +
  theme(axis.title.x=element_blank())

c <- ggplot(data=Germany, aes(x=reorder(gearbox,dollar_price), y=dollar_price)) +
  ggtitle("PRICE AVG by ENGINE") +
  geom_bar(stat='summary',fill ='cadetblue') +
  theme(axis.title.x=element_blank(), axis.text.x=element_text(angle=90))


theme_update(plot.title = element_text(hjust = 0.5))
plot_grid(a,b,c, ncol=2)
rm(a,b,c)

# Checking correlation between numeric variables
plot(aggregate(Germany[,c('dollar_price','kilometer')], by=list(year = Germany$registration_year), FUN=mean))
corrplot(cor(Germany[c('dollar_price', 'kilometer', 'registration_year')]), 
         method='color', addCoef.col = 'black',
         diag=FALSE, type='upper',
         tl.col='black', tl.srt=45, insig = 'blank')
# we can see correlation between the numeric variables, none too strong
# in particular, shows much weaker correlation year-kilometer than the other 2 datasets

# SAVE THE RESULTS OF PREPROCESSING
write.csv(Germany,file='Datasets/Preprocessed/Germany.csv',na="",row.names=FALSE)


# PREPARING FOR MODEL TECHNIQUES #################################
options(warn=-1)

# Set variables to Record results
DATASET <- vector()
ALGORITHM <- vector()
RMSE <- vector()
Rsquared <- vector()


# Split the data in train/test #################################
set.seed(123)
rows <- createDataPartition(Germany$dollar_price, p=0.7, list=FALSE)
traindf <-  Germany[rows,]
testdf <- Germany[-rows,]

# Predictor variables
x <- traindf[, 2:8]
x1 <- testdf[, 2:8]
# Outcome variable
y <- traindf$dollar_price
y1 <- testdf$dollar_price


# One-Hot Encoding ####################################
dummies <- dummyVars(dollar_price~., data=traindf)
traindf <- data.frame(predict(dummies, newdata = traindf))

dummies <- dummyVars(dollar_price~., data=testdf)
testdf <- data.frame(predict(dummies, newdata = testdf))


# Scale predictor variables to range 0-1
preProcess_range_model <- preProcess(traindf, method='range')
traindf <- predict(preProcess_range_model, newdata = traindf)
# Append the Y variable
traindf$dollar_price <- y
apply(traindf[, 1:143], 2, FUN=function(x){c('min'=min(x), 'max'=max(x))})

preProcess_range_model <- preProcess(testdf, method='range')
testdf <- predict(preProcess_range_model, newdata = testdf)
# Append the Y variable
testdf$dollar_price <- y1
apply(testdf[, 1:143], 2, FUN=function(x){c('min'=min(x), 'max'=max(x))})

rm(rows, dummies, preProcess_range_model)


# LINEAR REGRESSION MODELS ###############################

# Set lambda, reduces computational time
lambda <- 10^seq(-3, 3, length = 100)

# Set training control as 10 fold cross validation
tc <- trainControl("cv", number=5)

# Linear regression model ###########################################
set.seed(123)
LMfit <- train(
  dollar_price~., data=traindf, 
  method="lm",
  trControl = tc,
)

# LM summary
LMfit

# Make predictions on test file
predictions <- LMfit %>% predict(testdf)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, testdf$dollar_price),
  Rsquare = R2(predictions, testdf$dollar_price)
)

# Sort factors importance
(varimp <- varImp(LMfit))
a <- plot(varimp, top=10, main="TOP 10 VARIABLES IN LINEAR REGRESSION")

# Save the results
DATASET <- append(DATASET, 'Germany')
ALGORITHM <- append(ALGORITHM, 'Linear')
RMSE <- append(RMSE, RMSE(predictions, testdf$dollar_price))
Rsquared <- append(Rsquared, R2(predictions, testdf$dollar_price))


# Ridge model ###########################################
set.seed(123)
RIDGEfit <- train(
  dollar_price~., data=traindf, 
  method="glmnet",
  trControl = tc,
  tuneGrid = expand.grid(alpha=0, lambda=lambda)
)

# Ridge model coefficients
RIDGEfit

# Make predictions on test file
predictions <- RIDGEfit %>% predict(testdf)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, testdf$dollar_price),
  Rsquare = R2(predictions, testdf$dollar_price)
)

(varimp <- varImp(RIDGEfit))
b<-plot(varimp, top=10, main="TOP 10 VARIABLES IN RIDGE REGRESSION")

# Save the results
DATASET <- append(DATASET, 'Germany')
ALGORITHM <- append(ALGORITHM, 'Ridge')
RMSE <- append(RMSE, RMSE(predictions, testdf$dollar_price))
Rsquared <- append(Rsquared, R2(predictions, testdf$dollar_price))


# Lasso model ###########################################
set.seed(123)
LASSOfit <- train(
  dollar_price~., data=traindf, 
  method="glmnet",
  trControl = tc,
  tuneGrid = expand.grid(alpha=1, lambda=lambda)
)
# Lasso model coefficients
LASSOfit

# Make predictions on test file
predictions <- LASSOfit %>% predict(testdf)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, testdf$dollar_price),
  Rsquare = R2(predictions, testdf$dollar_price)
)

(varimp <- varImp(LASSOfit))
c<-plot(varimp, top=10, main="TOP 10 VARIABLES IN LASSO REGRESSION")

# Save the results
DATASET <- append(DATASET, 'Germany')
ALGORITHM <- append(ALGORITHM, 'Lasso')
RMSE <- append(RMSE, RMSE(predictions, testdf$dollar_price))
Rsquared <- append(Rsquared, R2(predictions, testdf$dollar_price))

# Plot variable importance for linear techniques
plot_grid(a,b,c, ncol=3)
rm(a,b,c)


# TREE BASED MODELS #############################

# Decision tree ##############################
TREEfit <- train(
  dollar_price~., data = traindf, method = "rpart",
  trControl = tc,
  tuneLength = 10
)

# Model overview
TREEfit
plot(TREEfit$finalModel)
text(TREEfit$finalModel, digits = 3)

# Make predictions
predictions <- TREEfit %>% predict(testdf)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, testdf$dollar_price)
)

(varimp <- varImp(TREEfit))
plot(varimp, top=10, main="TOP 10 VARIABLES IN DECISION TREE REGRESSION")

# Save the results
DATASET <- append(DATASET, 'Germany')
ALGORITHM <- append(ALGORITHM, 'Dec.Tree')
RMSE <- append(RMSE, RMSE(predictions, testdf$dollar_price))
Rsquared <- append(Rsquared, '')


# Random Forest ####################################
set.seed(123)
RFfit <- randomForest(dollar_price~., data=traindf, ntree=150, importance=TRUE)

# Model overview
RFfit
plot(RFfit, type="l", main='ERRORS vs. TREES IN RANDOM FOREST')
# Model was run for 150 trees, we can see that from 50th little improvement is done

# Make predictions
predictions <- RFfit %>% predict(testdf)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, testdf$dollar_price)
)

(varimp <- importance(RFfit))
varImpPlot(RFfit, n.var=min(10, nrow(RFfit$importance)), main="TOP 10 VARIABLE IN RANDOM FOREST REGRESSION")

# Save the results
DATASET <- append(DATASET, 'Germany')
ALGORITHM <- append(ALGORITHM, 'R. Forest')
RMSE <- append(RMSE, RMSE(predictions, testdf$dollar_price))
Rsquared <- append(Rsquared, '')



# PRINT THE RESULTS ################################
results <- data.frame(DATASET, ALGORITHM, RMSE, Rsquared)
names(results) <- c('DATASET', 'ALGORITHM', 'RMSE', 'Rsquared')
results <- results[order(results$RMSE),]
results
