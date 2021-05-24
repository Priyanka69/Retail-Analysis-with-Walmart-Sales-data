##DataSet Description: The leading Retails store in US, Walmart wat to predict the sales and demand acuratlely.
## In given Data set "Walmart_Store_Retail" which have following fields:

#Store : the No. Of store
# Date : the date of sales
#Weekly Sales : Weekly sales for given stoore
#Holidays_Flag : week is holiday week= 1 or not holiday week =0
#Temperature : Temp on day of Sale
#Fuel_Price : Cost of fuel
#CPI : Prevailing Consumer price index
# Unemployment: Prevailing unemployment rate


## Data importing



getwd()
setwd("D:/New Project")  
getwd()
walmart_df <- read.csv("Walmart_Store_sales.csv", header = T)
walmart_df 
View(walmart_df)
str(walmart_df)

library(dplyr)
library(zoo)
str(walmart_df)
class(walmart_df$Date)
walmart_df$Date <- as.Date(walmart_df$Date, format="%d-%m-%Y")
class(walmart_df$Date)
# in given data set "Date"  was in Factor class but now "Date" have class Date
# as.Date : to change date in proper format

## To check if there is any missing value in data set or not

is.na(walmart_df)


## Analysis Tasks : 1) Basic Statistical Tasks
##  1. Which Store has maximum sales

Max_Sale_Store <-walmart_df %>% group_by(Store) %>% summarise(Total_Sale = sum(Weekly_Sales)) %>%
                   filter(Total_Sale == max(Total_Sale))

Max_Sale_Store

## 2. Which store has maximum Standard deviation i.e., the sales vary alot also find out the 
##  coefficient of mean to stndard deviation.

Max_Stand_Dev <-walmart_df %>% group_by(Store) %>% summarise(Stand_Dev = sd(Weekly_Sales)) %>%
                filter(Stand_Dev == max(Stand_Dev))

Max_Stand_Dev


Coeffi_Var <- walmart_df %>% group_by(Store) %>% summarise(CV = sd(Weekly_Sales)/mean(Weekly_Sales)) %>%
             filter(CV == max(CV))
Coeffi_Var


## 3. Which store/s has a good quarterly growth rate in Q3'2012
## Here first we need to add new Column in data set with name Year_Quarter to get quarters
## For this will use mutate() function
## then we need to calculate growth rate for Q3

str(walmart_df)
YQ = as.yearqtr(walmart_df$Date,format="%Y-%m-%d")
YQ
str(walmart_df)
walmart_df$Year_Quart <- YQ
View(walmart_df)

# As we want to calculate grwoth rte of Q3 2012, we need to use below formula
# Gro_Rate= (Weekly_Sale of 2012Q3 - Weekly_Sale of 2012Q2)/ Weekly_Sale 2012Q2
#1. First will get 2012 Q2 data

W_Sale_2012_Q2 <-walmart_df %>% group_by(Store) %>% filter(Date >= as.Date("2012-04-01") & Date <= as.Date("2012-06-30")) %>%
                                                             summarise(sum(Weekly_Sales))
W_Sale_2012_Q2

#W_Sale_2012_Q2_1 <-walmart_df %>% group_by(Store) %>% filter(Year_Quart == "2012 Q2") %>%
#summarise(sum(Weekly_Sales))
#W_Sale_2012_Q2_1

#2. Will get 2012 Q3 data

W_Sale_2012_Q3 <-walmart_df %>% group_by(Store) %>% filter(Date >= as.Date("2012-07-01") & Date <= as.Date("2012-09-30")) %>%
  summarise(sum(Weekly_Sales))

W_Sale_2012_Q3



# Will find Grwoth Rate
#Gro_Rate= (Weekly_Sale of 2012Q3 - Weekly_Sale of 2012Q2)/ Weekly_Sale 2012Q2

Gro_Rate_2012_Q3 <-mutate(W_Sale_2012_Q3,Performance = ((W_Sale_2012_Q3$`sum(Weekly_Sales)`
                                                        -W_Sale_2012_Q2$`sum(Weekly_Sales)`)
                                                        /W_Sale_2012_Q2$`sum(Weekly_Sales)`)
                                                        *100)
arrange(Gro_Rate_2012_Q3,desc(Performance))


# 4. Some Holidays have negative impact on sale. Find out holidays which have higher sales than 
# the mean sales in non holiday season for all stores together

##Holiday Events:
#Super Blow : 12-Feb-2010, 11-Feb-2011, 10-Feb-2012, 
#Labour Day : 10-Sep-2010, 09-Sep-2011, 07-Sep-2012, 
#Thanksgiving : 26-Nov-2010, 25-Nov-2011, 23-Nov-2012, 
#Christmas : 31-Dec-2010, 30-Dec-2011, 28-Dec-2012, 

# first we need to find out mean of non holiday sales and then
#we have to find out dates where weekly sale will be greater than mean of non holiday sales.


Mean_N_Holiday_Sale <-walmart_df %>% filter(Holiday_Flag == '0') %>%
                      summarise(Total_Non_Holiday_Sales =mean(Weekly_Sales))
Mean_N_Holiday_Sale

Holiday_Sales <- walmart_df %>% group_by(Date) %>% filter(Holiday_Flag == '1') %>%
                       summarise(Total_Holiday_Sales = sum(Weekly_Sales)) %>%
        mutate(Higher_Holiday_Sales_Than_Non_Holiday_Sales =Total_Holiday_Sales > Mean_N_Holiday_Sale)
Holiday_Sales


# 5. Provide a monthly and semester view of sales in units and give insights. 
# First we will find out Monthly View of sale
library(data.table)
install.packages("lubridate")
library(lubridate)
library(corrplot)

Monthly_View <- walmart_df %>% mutate(Month = month(Date)) %>%
               group_by(Month) %>% summarise(Weekly_Sales = sum(Weekly_Sales))

Monthly_View


Monthly_Yearly_View <- walmart_df %>% mutate(Month = month(Date), Year = year(Date)) %>%
  group_by(Month, Year) %>% summarise(Weekly_Sales = sum(Weekly_Sales)) %>% arrange(Year)


Monthly_Yearly_View


#Now will find Semester View of Sale

Semester_View <-walmart_df %>% mutate(Semester = semester(Date,2010)) %>% group_by(Semester)%>%
                summarise(Weekly_Sales_Semester = sum(Weekly_Sales))
Semester_View


## Coorelation :
#The sample correlation coefficient (r) is a measure of the closeness of association of 
#the points in a scatter plot to a linear regression line based on those points
# The corrplot package is required for graphical display of a correlation matrix, 
# confidence interval. 
# The correlation matrix can be reordered according to the correlation coefficient.
# This is important to identify the hidden structure and pattern in the matrix.

Wal_Subset <- subset(walmart_df, select = c('Weekly_Sales','Temperature','Fuel_Price','Unemployment','CPI'))
res <-cor(Wal_Subset)
head(res)


corrplot(res, type = 'upper',order = 'alphabet', tl.col ='Red', tl.str = 45)

corrplot.mixed(res, lower = "square", upper = "circle", tl.col = "Black")

corrplot.mixed(res, lower = "number", upper = "ellipse", tl.col = "Black")


## Statistical Model
# for Store 1 -Build prediction model to forcast demand
#Hypothesis Testing: Null Hypothesis - Hypothesis testing is carried out in order to test
#the validity of a claim or assumption that is made about the larger population.

#H0 : Null hypothesis : There is no any impact of Temperature, CPI, Fuel_Price, unemployment on Sales of store 1
#H1 : Hypothesis : There is impact of Temerature, CPI, Fuel_Price, unemployment on weekly_Sales of store 1


Store_1 <-filter(walmart_df, Store == 1)
head(Store_1)

#Linear Model (lm) : Linear regression is a regression model that uses a straight line to describe 
#the relationship between variables. It finds the line of best fit through  data by searching for 
#the value of the regression coefficient(s) that minimizes the total error of the model.

# Y=??0+??1X1+??2X2+???+??pXp+??   : ??0= intercept , ??1 =coefficient of X1, ??2 = coefficient of X2

model_Store_1 <- lm(formula = Weekly_Sales ~ Temperature + Fuel_Price + CPI + Unemployment, data = Store_1)
model_Store_1


#Coefficients:
#(Intercept)   Temperature    Fuel_Price      CPI      Unemployment
#-2727200         -2426        -31637         17872       90632 


#Weekly_Sales = (-2727200) + Temperature*(-2426) +Fuel_Price*(-31637) + CPI*17872 + Unemployment*90632
#Weekly_Sales
  
Weekly_Sales1 <- 2727200 + 38.51 * (-2426) + 2.572 * (-31637) + 211.0964 * (17872) + 8.106 * 90632
Weekly_Sales1 #7059782


Weekly_Sales2 <- 2727200 + 39.51 * (-2426) + 2.572 * (-31637) + 211.0964 * (17872) + 8.106 * 90632
Weekly_Sales2 #7057356

# Here we can see that, Temprerature increased by 1 degree, the weekly sales of store 1 get decreases by 2426

Weekly_Sales3 <- 2727200 + 38.51 * (-2426) + 2.572 * (-31637) + 211.0964 * (17872) + 8.106 * 90632
Weekly_Sales3 #7059782


Weekly_Sales4 <- 2727200 + 39.51 * (-2426) + 2.572 * (-31637) + 211.0964 * (17872) + 7.106 * 90632
Weekly_Sales4 # 6966724

# Here we can se that, Unemployment get decreased by 1 unit, the weekly sale of store get decreased by 93058

summary(model_Store_1)

#Call:
#  lm(formula = Weekly_Sales ~ Temperature + Fuel_Price + CPI + 
#       Unemployment, data = Store_1)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-316968  -85750  -15239   51482  844800 

#Coefficients:
#               Estimate Std. Error t value  Pr(>|t|)   

#(Intercept)  -2727200.0  1759518.7  -1.550  0.12344   
#Temperature     -2426.5      917.8  -2.644  0.00915 **
#Fuel_Price     -31637.1    47551.8  -0.665  0.50696   
#CPI             17872.1     6807.0   2.626  0.00963 **
#Unemployment    90632.0    58925.1   1.538  0.12632   
----------------------------------------------------------
#  Signif. codes:  
# 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 147700 on 138 degrees of freedom
#Multiple R-squared:  0.1291,	Adjusted R-squared:  0.1039 
#F-statistic: 5.114 on 4 and 138 DF,  p-value: 0.0007142
----------------------------------------------------------
# P-Value =  0.00915 < alpha(0.05) -> Reject H0, as tempreature has effect on sales
#P-Value = 0.50696 >alpha(0.05) -> Don't reject H0, Fuel price has no effect on slaes
#P-Value = 0.00963 <alpha(o.o5) -> Reject H0, CPI has effect on slaes
#P-Value = 0.12632 >alpha(0.05) -> Don't Reject H0, Unemployment has no efect on sales  
  

RSQD2 <- summary(model_Store_1)$r.squared
RSQD2


Predicted_Sales_Model_Store_1 <-predict(model_Store_1, Store_1)
Predicted_Sales_Model_Store_1[1:10]
summary(Predicted_Sales_Model_Store_1)

# Now will Calculate rmse value, The rmse() function available in Metrics package in R
#is used to calculate root mean square error between actual values and predicted values
install.packages("Metrics")
library(Metrics)
rmse(model_Store_1$coefficients, Predicted_Sales_Model_Store_1)
rmse(model_Store_1$residuals,Predicted_Sales_Model_Store_1)
rmse(model_Store_1$rank, Predicted_Sales_Model_Store_1)

## Linear Regression- Utilize variables like Date and restructre dates as 1 for 5 Feb 2010
#Hypothesis, if CPI, Fuel_Price, Unemployment have any impack on sales.
#Time series forecasting model-
# Hypothesis if data fit for time series analysis- checkfor white noise probablity test
#Make adjustment for historical data like holidays if applicable.
#Build ARIMA model  to forcast 6 months i.e., input utilize only till april 2012
#Predict next 6 months i.e., june to Oct 2010. Check for MAPE
# Select model which gives best accuracy


library(lubridate)

Date_Format <- as.Date("23/06/89", "%d%m%y")
weekdays(Date_Format)
quarters(Date_Format)
months(Date_Format)


x<- as.Date("2009-09-02")
yday(x)
mday(x)
wday(x)


str(Store_1)
Store_1$Date <- as.Date(Store_1$Date,format = "%d%m%Y")
dtd=yday(Store_1$Date)
dtd
str(Store_1)
Store_1$Date_TO_Days <- dtd
str(Store_1)
head(Store_1)


library(caret)
install.packages("mlbench")
library(mlbench)

View(Store_1)
str(Store_1)
dim(Store_1)


data(walmart_df)

Control <- trainControl(method = "repeatedcv", number =10, repeats = 3)

set.seed(7)
KNN_Model <- train(Weekly_Sales ~ Temperature + Fuel_Price + CPI + Unemployment,
                   data = Store_1, method = "knn", trControl = Control)


set.seed(7)
SVM_Model <-train(Weekly_Sales ~ Temperature + Fuel_Price + CPI + Unemployment,
                  data = Store_1, method = "svmRadial", trControl = Control)


Results_of_Models <- resamples(list(KNN=KNN_Model, SVM=SVM_Model))

summary(Results_of_Models)
bwplot(Results_of_Models)
dotplot(Results_of_Models)

