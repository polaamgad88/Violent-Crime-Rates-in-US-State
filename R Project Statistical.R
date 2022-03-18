library(rpart)
library(rpart.plot)
library(dplyr)
library(naivebayes)
library(ggplot2)
library(psych)
library(tree)
library(e1071)
library(caTools)
library(class)

ata = USArrests

#summary print mean, median , IQR , min ,max of data 
summary(data) 

# IQR of coulmns 
IQR(data$Assault)
IQR(data$Murder)
IQR(data$UrbanPop)
IQR(data$Rape)

# standard deviation
sd(data$Murder) 
sd(data$Assault) 
sd(data$UrbanPop) 
sd(data$Rape) 

# variance of each 
var(data$Murder) 
var(data$Assault) 
var(data$UrbanPop) 
var(data$Rape) 


#mode of data
#tab <- table(data) # number of occurrences for each unique value
#sort(tab, decreasing = TRUE) # sort highest to lowest


#histogram 
hist(data$Assault, col = "orange", main = "Histogram for  Assault arrests (per 100,000) in each states")
hist(data$Rape, col = "orange", main = "Histogram for  Rape arrests (per 100,000) in each states ")
hist(data$UrbanPop, col = "orange", main = "Histogram for  UrbanPop arrests (per 100,000) in each states ")
hist(data$Murder, col = "orange", main = "Histogram for  Murder arrests (per 100,000) in each states  ")



#box plot 
boxplot(data, col = c( "Red"), main = "Boxplot for Murder,Assault , UrbanPop , Rape arrests in each state")

#scatter plot 
scatter.smooth(x=data$Murder, y=data$UrbanPop, main="murder ~ UrbanPop")  # scatterplot
scatter.smooth(x=data$Murder, y=data$Assault, main="murder ~ Assult")  # scatterplot
scatter.smooth(x=data$Murder, y=data$Rape, main="murder ~ Rape")  # scatterplot

#pie chart
#pie(data$Murder)

#bar chart for only murder arrests
barplot(data$Murder, main="Bar chart
        for Murder arrests (per 100,000) in each states",
        col=c("darkblue","red"),
        legend = rownames(data$Murder))


#correlation between murder and rape arrests
cor(data$Murder,data$Rape)



# regression between murder and rape arrests
# y =  b0 + b1*x
Model <- lm(data$Murder ~ data$Rape, data=data)  # build linear regression model 2 attribute of on  data
print(Model)
summary(Model)





