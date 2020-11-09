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

pacman::p_load(  pacman, rio, tidyverse, dplyr, mice, corrplot, cowplot, caret, skimr, glmnet, randomForest)

# LOAD FILES ####################################
# used-cars-listing-from-ebay --> Germany
# secondhand-car-price-estimation --> Australia
# online-adds-of-used-cars --> Spain
# craigslist-carstrucks-data --> States

# Import CSV file
#setwd("/Users/jordi/Documents/_POSTGRAD/Data Mining and Machine Learning I/Project/x17133246 DMproject/")
Spain <- read_csv2("online-adds-of-used-cars.csv")

# RETRIEVE SOME INFO ABOUT THE DATASET ##########
head(Spain)
glimpse(Spain)
summary(Spain)
# 93,991 rows
# 12 variables, many of them as character

# Remove obvious useless variables for the project objective: id
Spain <- subset(Spain, select=c(2:12))

# Recode character variables into factor
cols <- c('make','model','version','sale_type','gear_type','fuel_type')
Spain[cols] <- lapply(Spain[cols], factor)
rm(cols)

# Reorder columns (target+numeric+factor)
#»Spain <- Spain[, c(1, 12, 5, 11, 6, 8, 2:4, 7, 9:10)]
Spain <- Spain[, c(11, 4, 10, 5, 7, 1:3, 6, 8:9)]

# Check
glimpse(Spain)

# CHECK MISSING VALUES #############################################
# Pct. of missing values per column
sort(sapply(Spain,function(x) round(sum(is.na(x))/nrow(Spain)*100,1)), decreasing=TRUE)

# num_owners could have been an interesting variable
# but with 75% missing values, we have to drop it
Spain <- subset(Spain, select=-c(num_owners))

# VARIABLES LOOKUP ##############################
# Price variable ###############################
summary(Spain$price)
# Lets check variable distribution
boxplot(Spain$price, 
        horizontal = T,  
        notch  = T,
        main   = "PRICE BOXPLOT",
        xlab   = "Used car price",
        col='lightsalmon'
)
hist(Spain$price, 
     breaks = 20, 
     main   = "PRICE COUNT",
     ylab   = "",
     xlab   = "Used car price",
     col='cadetblue'
)

# Lets check the mean and sd for price
(m <- mean(Spain$price))
(s <- sd(Spain$price))

# on the lower limit, limit 500 as in other datasetS
# on the upper limit we can set 30000 (approx. m+sd)
count(Spain[Spain$price < 500, ])
count(Spain[Spain$price > 30000, ])
Spain <- Spain[Spain$price <= 30000 & Spain$price >= 500, ]

# Final distribution of price variable
summary(Spain$price)
boxplot(Spain$price, 
        horizontal = T,  
        notch  = T,
        main   = "PRICE BOXPLOT",
        xlab   = "Used car price",
        col='lightsalmon'
)
hist(Spain$price, 
     breaks = 20, 
     main   = "PRICE COUNT",
     ylab   = "",
     xlab   = "Used car price",
     col='cadetblue'
)


# Months_old variable ##############################
table(Spain$months_old)

# Lets check variable distribution
boxplot(Spain$months_old,        
        horizontal = T,  
        notch  = T,
        main   = "MONTHS OLD BOXPLOT",
        xlab   = "Months",
        col='lightsalmon'
)
hist(Spain$months_old, 
     breaks = 20, 
     main   = "MONTHS OLD COUNT",
     ylab   = "",
     xlab   = "Months",
     col='cadetblue'
)

# There are some old cars, we can see the count start to increase around 252 (=21 years)
count(Spain[Spain$months_old > 252, ])
Spain <- subset(Spain, months_old <= 252 | is.na(months_old))

# as with the others, I removed newest cars (12 months)
count(Spain[Spain$months_old <= 12, ])
Spain <- subset(Spain, months_old > 12  | is.na(months_old))

# Final distribution of months variable
table(Spain$months_old, useNA = "always")
boxplot(Spain$months_old,        
        horizontal = T,  
        notch  = T,
        main   = "MONTHS OLD BOXPLOT",
        xlab   = "Months",
        col='lightsalmon'
)
hist(Spain$months_old, 
     breaks = 20, 
     main   = "MONTHS OLD COUNT",
     ylab   = "",
     xlab   = "Months",
     col='cadetblue'
)


# kms variable ##############################
# Let´s check the variable
summary(Spain$kms)

boxplot(Spain$kms,        
        horizontal = T,  
        notch  = T,
        main   = "KILOMETRES BOXLOT",
        xlab   = "Kilometres",
        col='lightsalmon'
)
hist(Spain$kms, 
     breaks = 20, 
     main   = "KILOMETRES COUNT",
     ylab   = "",
     xlab   = "Kilometres",
     col='cadetblue'
)

# Lets check the mean and sd for price
(m <- mean(Spain$kms, na.rm=TRUE))
(s <- sd(Spain$kms, na.rm=TRUE))

# we saw the data ranges from 1 as lower value, limit 500 as in other datasetS 
count(Spain[Spain$kms < 500, ])
Spain <- subset(Spain, kms  >= 500 | (is.na(kms)))
# on the upper limit we set 200,000 (approx. m+sd)
count(Spain[Spain$kms > 200000, ])
Spain <- subset(Spain, kms < 200000 | (is.na(kms)))


# Final distribution of price variable
summary(Spain$kms)
boxplot(Spain$kms,        
        horizontal = T,  
        notch  = T,
        main   = "KILOMETRES BOXLOT",
        xlab   = "Kilometres",
        col='lightsalmon'
)
hist(Spain$kms, 
     breaks = 20, 
     main   = "KILOMETRES COUNT",
     ylab   = "",
     xlab   = "Kilometres",
     col='cadetblue'
)


# Power variable ###############################
# No missing values
summary(Spain$power)
# Number of factors
n_distinct(Spain$power)
Spain %>%
  select(power) %>%
  table() %>%
  barplot(
    main   = "POWER COUNT",
    las=2,
    col='cadetblue'
  )
# Unfortunately this variable shows very strange figures
# will be safer to drop it
Spain <- subset(Spain, select=-c(power))


# Make variable ###############################
# No missing values
summary(Spain$make)
Spain %>%
  select(make) %>%
  table() %>%
  sort(decreasing = T) %>%
  barplot(
    main   = "MAKE COUNT",
    las=2,
    col='cadetblue'
  )


# Model variable ###############################
summary(Spain$model)
# Number of models per make
samp <- aggregate(Spain[, 'model'], by=list(make = Spain$make), FUN=n_distinct)
samp[order(samp$model, decreasing = TRUE),]


# Version variable ###############################
# Missing values
sum(is.na(Spain$version))
# Number of factors
n_distinct(Spain$version)
# as the version provides little information (having already make+model)
# and with so many factors
# I drop this variable
Spain <- subset(Spain, select=-c(version))

# Fuel_type variable ###############################
summary(Spain$fuel_type)
Spain %>%
  select(fuel_type) %>%
  table() %>%
  sort(decreasing = T) %>%
  barplot(
    main   = "FUEL TYPE COUNT",
    las=2,
    col='cadetblue'
  )


# Sale_type variable ###############################
summary(Spain$sale_type)
Spain %>%
  select(sale_type) %>%
  table() %>%
  sort(decreasing = T) %>%
  barplot(
    main   = "FUEL TYPE COUNT",
    las=2,
    col='cadetblue'
  )

# Remove new type
Spain <- subset(Spain, sale_type != 'new' | (is.na(sale_type)))


# Gear_type variable ###############################
# Missing values:
summary(Spain$gear_type)
Spain %>%
  select(gear_type) %>%
  table() %>%
  sort(decreasing = T) %>%
  barplot(
    main   = "GEARBOX COUNT",
    las=2,
    col='cadetblue'
  )


# DROP UNUSED LEVELS IN THE DATASET #############################
Spain <- droplevels(Spain)   
rm(samp, m, s)


# IMPUT MISSING VALES ##################################
# Pct. of missing values per column
sort(sapply(Spain,function(x) sum(is.na(x))))
# Check the distribution of the mising values
md.pattern(Spain, plot=FALSE)
# Lets remove rows with more than 1 missing value
Spain <- subset(Spain, rowSums(is.na(Spain)) < 2)

# And imput numeric variables with kNN
# first store price as we dont want to transform it
y <- Spain$price
set.seed(123)
Imput <- preProcess(Spain, method='knnImpute')
Spain <- predict(Imput, newdata=Spain)
Spain$price <- y
rm(Imput)

# Finally drop remaining rows with missing values in categorical variables
Spain <- na.omit(Spain)


# DATA VISUALISATION ###########################################

# Final variables overview
skimmed <- skim(Spain)
skimmed[, c(1:2,6:15)]
rm(skimmed)

# Average prices by variables
a <- ggplot(data=Spain, aes(x=reorder(make,price), y=price)) +
  ggtitle("PRICE AVG by MAKE") +
  geom_bar(stat='summary',fill ='cadetblue') +
  theme(axis.title.x=element_blank(), axis.text.x=element_text(angle=90))

b <- ggplot(data=Spain, aes(x=reorder(sale_type,price), y=price)) +
  ggtitle("PRICE AVG by SALE TYPE") +
  geom_bar(stat='summary',fill ='cadetblue') +
  theme(axis.title.x=element_blank(), axis.text.x=element_text(angle=90))

c <- ggplot(data=Spain, aes(x=reorder(gear_type,price), y=price)) +
  ggtitle("PRICE AVG by GEARBOX TYPE") +
  geom_bar(stat='summary',fill ='cadetblue') +
  theme(axis.title.x=element_blank())

d <- ggplot(data=Spain, aes(x=reorder(fuel_type,price), y=price)) +
  ggtitle("PRICE AVG by FUEL TYPE") +
  geom_bar(stat='summary',fill ='cadetblue') +
  theme(axis.title.x=element_blank())

theme_update(plot.title = element_text(hjust = 0.5))
plot_grid(a,b,c,d, ncol=2)
rm(a,b,c,d, Imput)

# Checking correlation between numeric variables
plot(aggregate(Spain[,c('price','kms')], by=list(months = Spain$months_old), FUN=mean))
corrplot(cor(Spain[c('price', 'kms', 'months_old')]), 
         method='color', addCoef.col = 'black',
         diag=FALSE, type='upper',
         tl.col='black', tl.srt=45, insig = 'blank')
# we can see correlation between the numeric variables
# in particular between months and kms, none too strong

# SAVE THE RESULTS OF PREPROCESSING
write.csv(Spain,file='Datasets/Preprocessed/Spain.csv',na="",row.names=FALSE)


# PREPARING FOR MODEL TECHNIQUES #################################
options(warn=-1)

# Set variables to Record results
DATASET <- vector()
ALGORITHM <- vector()
RMSE <- vector()
Rsquared <- vector()
FACTOR1 <- vector()
FACTOR2 <- vector()
FACTOR3 <- vector()


# Split the data in train/test #################################
set.seed(123)
rows <- createDataPartition(Spain$price, p=0.7, list=FALSE)
traindf <-  Spain[rows,]
testdf <- Spain[-rows,]

# Predictor variables
x <- traindf[, 2:8]
x1 <- testdf[, 2:8]
# Outcome variable
y <- traindf$price
y1 <- testdf$price


# One-Hot Encoding ####################################
dummies <- dummyVars(price~., data=traindf)
traindf <- data.frame(predict(dummies, newdata = traindf))

dummies <- dummyVars(price~., data=testdf)
testdf <- data.frame(predict(dummies, newdata = testdf))


# Scale predictor variables to range 0-1
preProcess_range_model <- preProcess(traindf, method='range')
traindf <- predict(preProcess_range_model, newdata = traindf)
# Append the Y variable
traindf$price <- y
apply(traindf[, 1:143], 2, FUN=function(x){c('min'=min(x), 'max'=max(x))})

preProcess_range_model <- preProcess(testdf, method='range')
testdf <- predict(preProcess_range_model, newdata = testdf)
# Append the Y variable
testdf$price <- y1
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
  price~., data=traindf, 
  method="lm",
  trControl = tc,
)

# LM summary
LMfit

# Make predictions on test file
predictions <- LMfit %>% predict(testdf)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, testdf$price),
  Rsquare = R2(predictions, testdf$price)
)

# Sort factors importance
(varimp <- varImp(LMfit))
a <- plot(varimp, top=10, main="TOP 10 VARIABLES IN LINEAR REGRESSION")

# Save the results
DATASET <- append(DATASET, 'Spain')
ALGORITHM <- append(ALGORITHM, 'Linear')
RMSE <- append(RMSE, RMSE(predictions, testdf$price))
Rsquared <- append(Rsquared, R2(predictions, testdf$price))


# Ridge model ###########################################
set.seed(123)
RIDGEfit <- train(
  price~., data=traindf, 
  method="glmnet",
  trControl = tc,
  tuneGrid = expand.grid(alpha=0, lambda=lambda)
)

# Ridge model summary
RIDGEfit

# Make predictions on test file
predictions <- RIDGEfit %>% predict(testdf)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, testdf$price),
  Rsquare = R2(predictions, testdf$price)
)

(varimp <- varImp(RIDGEfit))
b<-plot(varimp, top=10, main="TOP 10 VARIABLES IN RIDGE REGRESSION")

# Save the results
DATASET <- append(DATASET, 'Spain')
ALGORITHM <- append(ALGORITHM, 'Ridge')
RMSE <- append(RMSE, RMSE(predictions, testdf$price))
Rsquared <- append(Rsquared, R2(predictions, testdf$price))


# Lasso model ###########################################
set.seed(123)
LASSOfit <- train(
  price~., data=traindf, 
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
  RMSE = RMSE(predictions, testdf$price),
  Rsquare = R2(predictions, testdf$price)
)

(varimp <- varImp(LASSOfit))
c<-plot(varimp, top=10, main="TOP 10 VARIABLES IN LASSO REGRESSION")

# Save the results
DATASET <- append(DATASET, 'Spain')
ALGORITHM <- append(ALGORITHM, 'Lasso')
RMSE <- append(RMSE, RMSE(predictions, testdf$price))
Rsquared <- append(Rsquared, R2(predictions, testdf$price))

# Plot variable importance for linear techniques
plot_grid(a,b,c, ncol=3)
rm(a,b,c)


# TREE BASED MODELS #############################

# Decision tree ##############################
TREEfit <- train(
  price~., data = traindf, method = "rpart",
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
  RMSE = RMSE(predictions, testdf$price)
)

(varimp <- varImp(TREEfit))
plot(varimp, top=10, main="TOP 10 VARIABLES IN DECISION TREE REGRESSION")

# Save the results
DATASET <- append(DATASET, 'Spain')
ALGORITHM <- append(ALGORITHM, 'Dec.Tree')
RMSE <- append(RMSE, RMSE(predictions, testdf$price))
Rsquared <- append(Rsquared, '')


# Random Forest ####################################
set.seed(123)
RFfit <- randomForest(price~., data=traindf, ntree=150, importance=TRUE)

# Model overview
RFfit
plot(RFfit, type="l", main='ERRORS vs. TREES IN RANDOM FOREST')
# Model was run for 150 trees, we can see that from 50th little improvement is done

# Make predictions
predictions <- RFfit %>% predict(testdf)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, testdf$price)
)

(varimp <- importance(RFfit))
varImpPlot(RFfit, n.var=min(10, nrow(RFfit$importance)), main="TOP 10 VARIABLE IN RANDOM FOREST REGRESSION")

# Save the results
DATASET <- append(DATASET, 'Spain')
ALGORITHM <- append(ALGORITHM, 'R. Forest')
RMSE <- append(RMSE, RMSE(predictions, testdf$price))
Rsquared <- append(Rsquared, '')


# PRINT THE RESULTS ################################
results <- data.frame(DATASET, ALGORITHM, RMSE, Rsquared)
names(results) <- c('DATASET', 'ALGORITHM', 'RMSE', 'Rsquared')
results <- results[order(results$RMSE),]
results
