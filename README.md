# Time-Series-Forecasting-Global-Store
Time series Forcasting Global super Store

#/********************************************************************************/#
#/*     Objective : Global_Super store Time Series Forcasting                      #                           */                           */
#/*                                                                              */#
#/*     Prepared By : Rajendra Mehta                                               #
#*/     date        :15/04/17                                                    */#  
#/********************************************************************************/#
rm(list=(ls()))

library(RGtk2)
options(guiToolkit="RGtk2")
library(gWidgetsRGtk2)

w <- gwindow("Global SuperMarket",visible=T)
g <- ggroup(horizontal = F, container = w)

ga <- gframe("SELECT INPUT FILE/FOLDER PATH",pos=0.5,horizontal = F, container = g)

browse.file1 <- gfilebrowse("SELECT Input FILE",container = ga,quote=F)


button1 <- gbutton(text="OK- TO SUBMIT INPUT", container = gi,
                   handler = function(h, ...) {
                     CMBDB<<- svalue(browse.file1)
                   })
###################################################################################
Global_Super<-function(h,...){
library(plyr)
library(ggplot2)
library(sqldf)
library(caret)
library(mlr)
library(ggplot2)
library(forecast)
library(tseries)
library(reshape)
#install.packages("reshape",dependencies = T)
#install.packages("tseries",dependencies = T)
Data_Dictionary<-read.csv("C:/Users/rajemeht/Desktop/Rajendra/Data Dictionary.csv")
Global_Superstore<-read.csv("C:/Users/rajemeht/Desktop/Rajendra/Global Superstore.csv",header = T)
Master_File<-Global_Superstore
str(Master_File)
#summary(Master_File)
#sum(is.na(Master_File))
summarizeColumns(Master_File[,c("Sales","Quantity","Profit","Order.Date")])
#Imputing missing values using KNN.Also centering and scaling numerical columns
#sum(is.na(Master_File_train_processed))
#Outlier Treatment
plot(Sales ~ Order.Date, data=Master_File)
plot(Sales ~ Market, data=Master_File)
XY<-quantile(Master_File$Sales, probs = c(0.99)) # quartile
Master_File$Sales<-ifelse(Master_File$Sales>=XY,XY,Master_File$Sales)
max(Master_File$Sales)

plot(Quantity ~ Order.Date, data=Master_File)
plot(Quantity ~ Market, data=Master_File)
max(Master_File$Quantity)
XY_1<-quantile(Master_File$Quantity, probs = c(0.99)) # quartile
table(Master_File$Quantity)

plot(Profit ~ Order.Date, data=Master_File)
plot(Profit ~ Market, data=Master_File)
max(Master_File$Profit)
XY_2<-quantile(Master_File$Profit, probs = c(0.995)) # quartile
min(Master_File$Profit)
XY_3<-quantile(abs(Master_File$Profit), probs = c(0.995)) # quartile
Master_File$Profit<-ifelse(abs(Master_File$Profit)>XY_3,XY_3,Master_File$Profit)
max(Master_File$Profit)
min(Master_File$Profit)
table(Master_File$Profit)

#Segmenting the data in to segment and Market
Master_File_2<-ddply(Master_File, .(Segment, Market), numcolwise(sum))
Master_File_1_Sales<- ts(Master_File_2$Sales, start=c(2011, 1), end=c(2014, 12), frequency=12) 
Master_File_1_Profit<- ts(Master_File_2$Profit, start=c(2011, 1), end=c(2014, 12), frequency=12) 
#summary(Master_File_1$Sales)
plot(Master_File_1_Sales)
summary(Master_File_1_Profit)
plot(Master_File_1_Profit)
#Corelating with the Master File and prediction
Master_File_3<-Master_File
Master_File_3$year<-format(as.Date(Master_File$Order.Date, "%d-%m-%Y"), "%Y")
Master_File_3$Month<-format(as.Date(Master_File$Order.Date, "%d-%m-%Y"),"%B")
Master_File_3$Segmentation<-paste0(Master_File_3$Segment,"_",Master_File_3$Market)
Master_File_3_1<-ddply(Master_File_3, .(Segment, Market,year,Month), numcolwise(sum))
Master_File_3_1$Segmentation<-paste0(Master_File_3_1$Segment,"_",Master_File_3_1$Market)
Master_File_3_1$year_Month<-paste0(Master_File_3_1$year,"_",Master_File_3_1$Month)
#Master_File_3_2<-melt(Master_File_3_1,id=c("Segmentation","year_Month"))aggregate()
Master_File_3_2<-cast(Master_File_3_1[,c("Segmentation","year_Month","Sales")],Segmentation~year_Month,sum)
Master_File_3_2$CV<-apply(Master_File_3_2,1,function(x) sd(x)/mean(x))
Master_File_3_2$Rank<-rank(Master_File_3_2$CV)
Master_File_3_3<-subset(Master_File_3_2,Master_File_3_2$Rank>19)

Master_File_3_4<- subset(Master_File_3,Master_File_3$Segmentation==c("Corporate_Canada","Home Office_Canada"))
Master_File_3_5<- ts(round(Master_File_3_4$Sales,0), start=c(2011, 1), end=c(2014, 12), frequency=12) 
Master_File_3_4$Date_1 = as.Date(Master_File_3_4$Order.Date )
ggplot(Master_File_3_4, aes(Date_1, Sales)) + geom_line() + scale_x_date('month')  + ylab("Checkouts") +
  xlab("")

#Master_File_3_4$clean_cnt = tsclean(count_ts)
count_ts = ts(Master_File_3_4[, c('Sales')])
ggplot() +
  geom_line(data = Master_File_3_4, aes(x = Date_1, y = count_ts)) + ylab('Count')
#observing the time seriies output for two most profitable variable
class(Master_File_3_5)
plot(Master_File_3_5)
abline(reg=lm(Master_File_3_5~time(Master_File_3_5)))
adf.test(diff(log(Master_File_3_5)), alternative="stationary", k=0)
acf(log(Master_File_3_5))
acf(diff(log(Master_File_3_5)))
pacf(diff(log(Master_File_3_5)))

(fit <- arima(log(Master_File_3_5), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)))

pred_12_Month <- predict(fit, n.ahead = 1*12)

ts.plot(Master_File_3_5,2.718^pred$pred, log = "y", lty = c(1,3))
dispose(w)}
button10<-gbutton("RUN",cont=gj,handler=Global_Super)
###############################Thank You###########################################
