setwd("/Users/dill/Documents/Seminar in Business R folder")
# read in the csv

delaydf<-read.csv("Flight_Delay.csv")

#take a look at the data for interesting columns
View(delaydf)
names(delaydf)

#ARR_delay looks like what we want to model. 

# take a look at some descriptive statistics n checking for NAs
nas<-sum(is.na(delaydf))
summary(delaydf)
nas
#No NAs


#getting just the numeric columns so I can do a correlation to help inform variable selection

delay_num<-  subset(delaydf, select = -Carrier)
View(delay_num)

cdelay<-cor(delay_num)

View(cdelay)
# I'm going to start with baggage loading time,number of flights, and late arrival to start
# based on them being in a tier of their own in terms of correlation to Arr_delay

#get the df down to what we want
df_clean<- subset(delaydf, select = 
                    c(Number_of_flights, Baggage_loading_time, Late_Arrival_o, Arr_Delay))

View(df_clean)
#let's get some histograms and scatter plots 
#let's get some histograms and scatter plots 

#Histograms 
par(mfrow = c(2,2))
#Hist 1 Density curve idea from chatgpt
hist(df_clean$Number_of_flights, breaks = 20, col = "blue", probability = TRUE,
     main = "Number of Flights", xlab = "Data Values")
lines(density(df_clean$Number_of_flights), col = "red", lwd = 2)
# Hist 2
hist(df_clean$Baggage_loading_time, breaks = 20, col = "blue", probability = TRUE,
     main = "Baggage Loading Time", xlab = "Data Values")
lines(density(df_clean$Baggage_loading_time), col = "red", lwd = 2)
#Hist 3
hist(df_clean$Late_Arrival_o, breaks = 20, col = "blue", probability = TRUE,
     main = "Late Arrival", xlab = "Data Values")
lines(density(df_clean$Late_Arrival_o), col = "red", lwd = 2)
#Hist4
hist(df_clean$Arr_Delay, breaks = 20, col = "blue", probability = TRUE,
     main = "Delay", xlab = "Data Values")
lines(density(df_clean$Arr_Delay), col = "red", lwd = 2)



# Some of the value ranges are pretty narrow but everything looks basically normally distributed

#scatter plots
par(mfrow = c(2,2))
plot(df_clean$Number_of_flights, df_clean$Arr_delay)

plot(df_clean$Baggage_loading_time, df_clean$Arr_delay)

plot(df_clean$Late_Arrival_o, df_clean$Arr_delay)

#Scatter plots not saying much of use here, some scatter plots look strange because of 
#discrete, narrow ranges of values.


# Now to partition the data set
library(caTools)

set.seed(888) 

sample <- sample.split(df_clean$Arr_Delay,SplitRatio=0.70)
train<- subset(df_clean,sample==TRUE)
test<- subset(df_clean,sample==FALSE)
testf<-as.data.frame(test)
trainf<-as.data.frame(train)
#Modeling Arr_delay as a function of independent variables.
LRM<-lm(train$Arr_Delay ~ 
          train$Number_of_flights + train$Baggage_loading_time + train$Late_Arrival_o,data = train)
LRMcheck<- lm(Arr_Delay ~., data = trainf)

#broom package suggested by llm for more readable  model interpretation  I liked
install.packages("broom")
library(broom)
# for r^2
glance_model <- glance(LRM)
print(glance_model)
# for coefficients and p values
tidy_model<- tidy(LRM)
print(tidy_model)

# the P values are all well below .01
# an adjusted r^2 of .802
# we get an equation of 
# Arr_Delay = -542 + .00484*Number_of_flights + 15.2*Baggage_loading_time + 7.65* Late Arrivals
 

#fits and residuals
#predicting fitted values of train_data set

ptrain<- LRMcheck$fitted.values


ptrain1 <- data.frame(ptrain)

rtrain <- LRMcheck$residuals

rtrain1<-data.frame(rtrain)

ptest<- predict(LRMcheck,newdata = testf)
ptest1<- data.frame(ptest)

# Graphs for actual vs predicted 
plot (testf$Arr_Delay,col="red",type ="l",lty=1.8 )
lines(ptest1,col="blue",type ="l",lty=1.4)

plot(LRMcheck,which=1)
plot(LRMcheck,which=2)
plot(LRMcheck,which=3)

# Fit vs residual plots look pretty lined up
#Q-Q plot looks good, everything looking like its supposed to.

# durban watson test
install.packages("car")
# 
library(car)


# independence assumption

durbinWatsonTest(LRMcheck)
# results
# lag Autocorrelation D-W Statistic p-value
# 1     -0.02028526      2.039533   0.324
# Alternative hypothesis: rho != 0

# Homoscedasticity 

ncvTest(LRMcheck)
# results
# Non-constant Variance Score Test 
# Variance formula: ~ fitted.values 
# Chisquare = 0.1575692, Df = 1, p = 0.6914


#VIF Collinearity

vif(LRMcheck) 

# Number_of_flights Baggage_loading_time       Late_Arrival_o 
# 2.073025             1.966100             1.594569


