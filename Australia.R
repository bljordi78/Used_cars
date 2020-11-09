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
Australia <- read_csv("secondhand-car-price-estimation.csv")


# RETRIEVE SOME INFO ABOUT THE DATASET ##########
head(Australia)
glimpse(Australia)
summary(Australia)
# 55,870 rows
# 13 variables, many of them as character

# Remove obvious useless variables for the project objective: id, title
Australia <- subset(Australia, select=-c(id,title))

# Recode character variables into factor
cols <- c('brand','model','discount','body','transmission','engine','state','seller')
Australia[cols] <- lapply(Australia[cols], factor)
rm(cols)

# And move target variable to the 1st column
# Reorder columns (target+numeric+factor)
Australia <- Australia[, c(3, 11, 5, 1:2, 4, 6:10)]

# Check
glimpse(Australia)


# CHECK MISSING VALUES ##################################
# Pct. of missing values per column
sort(sapply(Australia,function(x) round(sum(is.na(x))/nrow(Australia)*100,1)), decreasing=TRUE)


# VARIABLES LOOKUP##############################
# Price variable ###################
summary(Australia$price)

# We can see the range is 250 - 399,000
# there are 544 missing values (1% as we saw previously)
# being a small amount and that it is the target variable: drop all missing values
Australia <- subset(Australia, !is.na(price))

# Lets check variable distribution
boxplot(Australia$price, 
        horizontal = T,  
        notch  = T,
        main   = "PRICE BOXPLOT",
        xlab   = "Used car price",
        col='lightsalmon'
)
hist(Australia$price, 
     breaks = 20, 
     main   = "PRICE COUNT",
     ylab   = "",
     xlab   = "Used car price",
     col='cadetblue'
)

# Lets check the mean and sd for price
(m <- mean(Australia$price))
(s <- sd(Australia$price))

# on the lower limit, set 500 as in other datasetS (as m-sd high value)
# on the upper limit we can set 40000 (approx. m+sd)
count(Australia[Australia$price < 500, ])
count(Australia[Australia$price > 40000, ])
# and remove values out of range
Australia <- Australia[Australia$price <= 40000 & Australia$price >= 500, ]

# Final distribution of price variable
summary(Australia$price)
boxplot(Australia$price, 
        horizontal = T,  
        notch  = T,
        main   = "PRICE BOXPLOT",
        xlab   = "Used car price",
        col='lightsalmon'
)
hist(Australia$price, 
     breaks = 20, 
     main   = "PRICE COUNT",
     ylab   = "",
     xlab   = "Used car price",
     col='cadetblue'
)


# Seller variable ###############################
summary(Australia$seller)
# Lets check variable distribution
Australia %>%
  select(seller) %>%
  table() %>%
  sort(decreasing = T) %>%
  barplot(
    main   = "SELLER TYPE COUNT",
    las=2,
    col='cadetblue'
  )

# for the purpose of the research, only need used cars and private seller
Australia <- Australia[Australia$seller == 'Private Seller Car' | Australia$seller == 'Used Car', ]


# Odometres variable ###############################
summary(Australia$odometres)

# We can see the minimum is 0 and max 999,999
# there are no missing values but 10 zero values
count(Australia[Australia$odometres == 0, ])

# Lets check variable distribution
boxplot(Australia$odometres, 
        horizontal = T,  
        notch  = T,
        main   = "ODOMETER BOXPLOT",
        xlab   = "Odometer",
        col='lightsalmon'
)
hist(Australia$odometres, 
     breaks = 20, 
     main   = "ODOMETER COUNT",
     ylab   = "",
     xlab   = "Odometer",
     col='cadetblue'
)

# Not only there are 0 values but also unusual very low values
# Lets check the mean and sd for odometres
(m <- mean(Australia$odometres))
(s <- sd(Australia$odometres))

# on the lower limit, we can set 500 as in other datasets (although far from m-sd)
# on the upper limit we can set 200,000 (close to m+sd)
count(Australia[Australia$odometres <= 500, ])
count(Australia[Australia$odometres >= 200000, ])
# and remove values out of range
Australia <- Australia[Australia$odometres <= 200000 & Australia$odometres >= 500, ]

# Final distribution of odometres variable
summary(Australia$odometres)
boxplot(Australia$odometres, 
        horizontal = T,  
        notch  = T,
        main   = "ODOMETER BOXPLOT",
        xlab   = "Odometer",
        col='lightsalmon'
)
hist(Australia$odometres, 
     breaks = 20, 
     main   = "ODOMETER COUNT",
     ylab   = "",
     xlab   = "Odometer",
     col='cadetblue'
)
rm(m,s)

# Brand variable #############################
summary(Australia$brand)
# Lets check variable distribution
Australia %>%
  select(brand) %>%
  table() %>%
  sort(decreasing = T) %>%
  barplot(
    main   = "BRANDS COUNT",
    las=2,
    col='cadetblue'
  )


# Model variable ###############################
# No missing values
sum(is.na(Australia$model))
# Number of models per brand
aggregate(Australia[, 'model'], by=list(brand = Australia$brand), FUN=n_distinct)


# State variable ###############################
summary(Australia$state)

# Lets check variable distribution
Australia %>%
  select(state) %>%
  table() %>%
  sort(decreasing = T) %>%
  barplot(
    main   = "STATE COUNT",
    las=2,
    col='cadetblue'
  )


# Body variable ###############################
summary(Australia$body)
# Lets check variable distribution
Australia %>%
  select(body) %>%
  table() %>%
  sort(decreasing = T) %>%
  barplot(
    main   = "BODY COUNT",
    las=2,
    col='cadetblue'
  )

# Remove bus, light truck and van
val = c('Cab Chassis','Convertible','Coupe','Hatch','People Mover','Sedan','SUV','Ute','Wagon')
Australia <- subset(Australia, subset = body %in% val)
rm(val)

# Final distribution of body variable
summary(Australia$body)
Australia %>%
  select(body) %>%
  table() %>%
  sort(decreasing = T) %>%
  barplot(
    main   = "BODY COUNT",
    las=2,
    col='cadetblue'
  )


# Transmission variable ###############################
summary(Australia$transmission)
# Lets check variable distribution
Australia %>%
  select(transmission) %>%
  table() %>%
  sort(decreasing = T) %>%
  barplot(
    main   = "TRANSMISSION COUNT",
    las=2,
    col='cadetblue'
  )


# Engine variable ###############################
summary(Australia$engine)
# Lets check variable distribution
Australia %>%
  select(engine) %>%
  table() %>%
  sort(decreasing = T) %>%
  barplot(
    main   = "ENGINE COUNT",
    las=2,
    col='cadetblue'
  )

# on the engine and transmission variables, we see some strange values probably scrapping errors
# but they show zero count after the rows deleted during the process


# Year variable ###############################
summary(Australia$year)
table(Australia$year)

# Lets check variable distribution
boxplot(Australia$year, 
        horizontal = T,  
        notch  = T,
        main   = "YEAR BOXPLOT",
        xlab   = "Year",
        col='lightsalmon'
)
hist(Australia$year, 
     breaks = 20, 
     main   = "YEAR COUNT",
     ylab   = "",
     xlab   = "Year",
     col='cadetblue'
)

# we see that count starts to rise in 1995 (23 years old cars)
# we limit as well the most recent cars (under 1 year)
count(Australia[Australia$year < 1995, ])
count(Australia[Australia$year == 2018, ])
Australia <- Australia[Australia$year != 2018 & Australia$year >= 1995, ]

# Final distribution of year variable
summary(Australia$year)
boxplot(Australia$year, 
        horizontal = T,  
        notch  = T,
        main   = "YEAR BOXPLOT",
        xlab   = "Year",
        col='lightsalmon'
)
hist(Australia$year, 
     breaks = 20, 
     main   = "YEAR COUNT",
     ylab   = "",
     xlab   = "Year",
     col='cadetblue'
)


# Discount variable #########################
# the data shows there is governemt charges excluded in some cases
table(Australia$discount)

# Recode as yes/no variable
Australia$discount <- as.character(Australia$discount)
Australia$discount[Australia$discount != 'Excl. Govt. Charges'] <- 0
Australia$discount[Australia$discount == 'Excl. Govt. Charges'] <- 1
Australia$discount <- as.factor(Australia$discount)

table(Australia$discount)

# the variable distribution
Australia %>%
  select(discount) %>%
  table() %>%
  sort(decreasing = T) %>%
  barplot(
    main  = "DISCOUNT COUNT",
    col='cadetblue'
  )


# DROP UNUSED LEVELS IN THE DATASET #############################
Australia <- droplevels(Australia)   


# CHECK MISSING VALUES #############################################
# Pct. of missing values per column
sort(sapply(Australia,function(x) round(sum(is.na(x))/nrow(Australia)*100,5)), decreasing=TRUE)
# after cleaning up the variables, all missing values were removed


# DATA VISUALISATION ###########################################

# Final variables overview
skimmed <- skim(Australia)
skimmed[, c(1:2,6:15)]
rm(skimmed)

# Average prices by variables
a1 <- ggplot(data=Australia, aes(x=reorder(brand,price), y=price)) +
  ggtitle("PRICE AVG by BRAND") +
  geom_bar(stat='summary',fill ='cadetblue') +
  theme(axis.title.x=element_blank())

a2 <- ggplot(data=Australia, aes(x=reorder(model,price), y=price)) +
  ggtitle("PRICE AVG by MODEL") +
  geom_bar(stat='summary',fill ='cadetblue') +
  theme(axis.title.x=element_blank(), axis.text.x=element_text(angle=90))

b1 <- ggplot(data=Australia, aes(x=reorder(body,price), y=price)) +
  ggtitle("PRICE AVG by BODY") +
  geom_bar(stat='summary',fill ='cadetblue') +
  theme(axis.title.x=element_blank())

b2 <- ggplot(data=Australia, aes(x=reorder(engine,price), y=price)) +
  ggtitle("PRICE AVG by ENGINE") +
  geom_bar(stat='summary',fill ='cadetblue') +
  theme(axis.title.x=element_blank(), axis.text.x=element_text(angle=90))

c1 <- ggplot(data=Australia, aes(x=reorder(transmission,price), y=price)) +
  ggtitle("PRICE AVG by TRANSMISSION") +
  geom_bar(stat='summary',fill ='cadetblue') +
  theme(axis.title.x=element_blank())

c2 <- ggplot(data=Australia, aes(x=reorder(discount,price), y=price)) +
  ggtitle("PRICE AVG by DISCOUNT") +
  geom_bar(stat='summary',fill ='cadetblue') +
  theme(axis.title.x=element_blank())

d1 <- ggplot(data=Australia, aes(x=reorder(state,price), y=price)) +
  ggtitle("PRICE AVG by STATE") +
  geom_bar(stat='summary',fill ='cadetblue') +
  theme(axis.title.x=element_blank())

d2 <- ggplot(data=Australia, aes(x=reorder(seller,price), y=price)) +
  ggtitle("PRICE AVG by SELLER") +
  geom_bar(stat='summary',fill ='cadetblue') +
  theme(axis.title.x=element_blank())

theme_update(plot.title = element_text(hjust = 0.5))
plot_grid(a1,a2,b1,b2, ncol=2)
plot_grid(c1,c2,d1,d2, ncol=2)
rm(a1,a2,b1,b2,c1,c2,d1,d2)

# Checking correlation between numeric variables
plot(aggregate(Australia[,c('price','odometres')], by=list(year = Australia$year), FUN=mean))
corrplot(cor(Australia[c('price', 'odometres', 'year')]), 
         method='color', addCoef.col = 'black',
         diag=FALSE, type='upper',
         tl.col='black', tl.srt=45, insig = 'blank')
# we can see correlation between the numeric variables, none too strong

# SAVE THE RESULTS OF PREPROCESSING
write.csv(Australia,file='Datasets/Preprocessed/Australia.csv',na="",row.names=FALSE)


# PREPARING FOR MODEL TECHNIQUES #################################
options(warn=-1)

# Set variables to Record results
DATASET <- vector()
ALGORITHM <- vector()
RMSE <- vector()
Rsquared <- vector()


# Split the data in train/test #################################
set.seed(123)
rows <- createDataPartition(Australia$price, p=0.7, list=FALSE)
traindf <-  Australia[rows,]
testdf <- Australia[-rows,]

# Predictor variables
x <- traindf[, 2:11]
x1 <- testdf[, 2:11]
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
DATASET <- append(DATASET, 'Australia')
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

# Ridge model coefficients
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
DATASET <- append(DATASET, 'Australia')
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
DATASET <- append(DATASET, 'Australia')
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
DATASET <- append(DATASET, 'Australia')
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
DATASET <- append(DATASET, 'Australia')
ALGORITHM <- append(ALGORITHM, 'R. Forest')
RMSE <- append(RMSE, RMSE(predictions, testdf$price))
Rsquared <- append(Rsquared, '')



# PRINT THE RESULTS ################################
results <- data.frame(DATASET, ALGORITHM, RMSE, Rsquared)
names(results) <- c('DATASET', 'ALGORITHM', 'RMSE', 'Rsquared')
results <- results[order(results$RMSE),]
results


