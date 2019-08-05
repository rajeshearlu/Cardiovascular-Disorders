##Loading required libraries for EDA
library(tidyverse) #contains many useful packages such as dplyr and ggplot
library(ggthemes) #to create  plot themes
library(scales) # scale_y_continuous(label = percent)
library(corrplot) # Corelation Plots


##Loading & Cleaning data
setwd("C:/Users/rajes/Desktop/NYDS/NYDS-2/RDA/Shiny_Proj/")
df1 <- read.csv(file = "./data/heart.csv", header = TRUE, sep = ",", fileEncoding="UTF-8-BOM") 

class(df1)
str(df1) #Found some columns in numeric which are supposed to be categorical variables



# A correlation plot might give us a basic idea as to how those attributes are 
#related to heart disease in this dataset.


df1cor<- cor(df1) #the correlation graph is saved to 'heartcor'
Plot2 <- corrplot(df1cor, method = "pie", type = "lower")


#Conversion to categorical variables
#df2 = cbind(df1)

df1$sex = as.character(df1$sex)
df1$fbs = as.character(df1$fbs)
df1$restecg = as.character(df1$restecg)
df1$exang = as.character(df1$exang)
df1$slope = as.character(df1$slope)
df1$ca = as.character(df1$ca)
df1$thal = as.character(df1$thal)
df1$target = as.character(df1$target)




#Renaming categorical values to respective values
df1$sex[df1$sex == "0"] <- "Female"
df1$sex[df1$sex == "1"] <- "Male"
df1$cp[df1$cp == "0"] <- "Typical Angina"
df1$cp[df1$cp == "1"] <- "Atypical Angina"
df1$cp[df1$cp == "2"] <- "Non-Anginal Pain"
df1$cp[df1$cp == "3"] <- "Asymptomatic"
df1$fbs[df1$fbs == "1"] <- "True"
df1$fbs[df1$fbs == "0"] <- "False"
df1$exang[df1$exang == "0"] <- "No"
df1$exang[df1$exang =="1"] <- "Yes"
df1$slope[df1$slope == "0"] <- "Upsloping"
df1$slope[df1$slope == "1"] <- "Flat"
df1$slope[df1$slope == "2"] <- "Downsloping"



##Data Analysis using ggplot2

# 1. Target: Let's start by analyzing the target variable which is the diagnosis of 
#heart disease. The target variable is 1 if diagnosed, 0 if not.


round(table(df1$target)/nrow(df1), digits = 2)


Plot1 <- ggplot(df1, aes(x = as.factor(target), fill=as.factor(target))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = percent) +
  theme_economist()+
  scale_fill_economist()+
  guides(fill=F) +
  labs(x="Target", y="Percent")

# 2. Age: 


Plot2 <- ggplot(df1,aes(age,col=as.factor(target),fill=as.factor(target)))+
  geom_density(alpha=0.2)+
  theme_economist()+
  scale_colour_economist()+
  scale_fill_economist()+
  guides(col=F)+
  labs(fill="Target",x="Age")

Plot3 <- ggplot(df1, aes(as.factor(target), age, fill=as.factor(target))) + 
  geom_boxplot(alpha=0.2)+
  theme_economist()+
  scale_fill_economist()+
  labs(x="Target", y="Age", fill="Target")


#Gender
#=======

Plot4 <- ggplot(df1,aes(as.factor(sex),fill=as.factor(target)))+
  geom_bar(stat="count")+
  theme_economist()+
  scale_fill_economist()+
  labs(x="Sex",fill="Target")

Plot5 <- ggplot(df1,aes(as.factor(sex),fill=as.factor(target)))+
  geom_bar(stat="count",position="fill")+
  theme_economist()+
  scale_fill_economist()+
  labs(x="Sex",fill="Target",y="stacked Percent")




#Chest Pain
#============


Plot6 <- ggplot(df1,aes(as.factor(cp),fill=as.factor(target)))+
  geom_bar(stat="count")+
  theme_economist()+
  scale_fill_economist()+
  labs(x="Chest Pain Type",fill="Target")







