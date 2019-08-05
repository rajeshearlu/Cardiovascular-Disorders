

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






#Conversion to categorical variables
#df2 = rbind(df1)
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





##Server O/P
#=============


shinyServer(
  function(input, output){
    
    
    # #Corelation
    # #===========
    # 
    # # A correlation plot might give us a basic idea as to how those attributes are 
    # #related to heart disease in this dataset.
    # 
    # 
    # output$Corel = renderPlot({
    #   
    #   df1cor<- cor(df1) #the correlation graph is saved to 'heartcor'
    #   Plotcorl <- corrplot(df1cor, method = "pie", type = "lower")
    #   
    # })
    
   
    
    
    
    #Target
    #=======
    
    output$Target = renderPlot({
      
      ggplot(df1, aes(x = as.factor(df1$target), fill=as.factor(df1$target))) +
        geom_bar(aes(y = (..count..)/sum(..count..))) +
        scale_y_continuous(labels = percent) +
        theme_economist()+
        scale_fill_economist()+
        guides(fill=F) +
        labs(x="Target", y="Percent")
      
    })
      
      
      
    #Age
    #====

      output$Age = renderPlot({
        
        if (input$Plot1 == "1"){
          ggplot(df1,aes(age,col=as.factor(target),fill=as.factor(target)))+
            geom_density(alpha=0.2)+
            theme_economist()+
            scale_colour_economist()+
            scale_fill_economist()+
            guides(col=F)+
            labs(fill="Target",x="Age")
          
        } else
          ggplot(df1, aes(as.factor(target), age, fill=as.factor(target))) + 
          geom_boxplot(alpha=0.2)+
          theme_economist()+
          scale_fill_economist()+
          labs(x="Target", y="Age", fill="Target")
        
        
      })
      
     #Gender
     #======
      
      output$Gender = renderPlot({
        
        if (input$Plot2 == "3"){
          ggplot(df1,aes(as.factor(sex),fill=as.factor(target)))+
            geom_bar(stat="count")+
            theme_economist()+
            scale_fill_economist()+
            labs(x="Sex",fill="Target")
          
        }else
          ggplot(df1,aes(as.factor(sex),fill=as.factor(target)))+
          geom_bar(stat="count",position="fill")+
          theme_economist()+
          scale_fill_economist()+
          labs(x="Sex",fill="Target",y="stacked Percent")
        
        
      })
      
      
      
      #Chest Pain
      #============
        
      output$Chest_Pain = renderPlot({
        
        if (input$Plot3 == "5"){
          ggplot(df1,aes(as.factor(cp),fill=as.factor(target)))+
            geom_bar(stat="count")+
            theme_economist()+
            scale_fill_economist()+
            labs(x="Chest Pain Type",fill="Target")
          
        }else
          ggplot(df1,aes(as.factor(cp),fill=as.factor(target)))+
          geom_bar(stat="count",position="fill")+
          theme_economist()+
          scale_fill_economist()+
          labs(x="Chest Paint Type",fill="Target",y="Percent")
        
        
      })
      
      
      
      #Resting_BP
      #============
      
      output$Resting_BP = renderPlot({
        
        if (input$Plot4 == "7"){
          ggplot(df1,aes(trestbps,col=as.factor(target),fill=as.factor(target)))+
            geom_density(alpha=0.2)+
            theme_economist()+
            scale_colour_economist()+
            scale_fill_economist()+
            guides(col=F)+
            labs(fill="Target",x="Resting Blood Pressure")
          
          
        }else
          ggplot(df1,aes(as.factor(target),trestbps,fill=as.factor(target)))+
          geom_boxplot(alpha=0.2)+
          theme_economist()+
          scale_fill_economist()+
          labs(y="Resting Blood Pressure",x="Target",fill="Target")
        
        
        
      })
      
      
      
      #Cholestoral
      #============
      
      output$Cholestoral = renderPlot({
        
        if (input$Plot5 == "9"){
          ggplot(df1,aes(chol,col=as.factor(target),fill=as.factor(target)))+
            geom_density(alpha=0.2)+
            theme_economist()+
            scale_colour_economist()+
            scale_fill_economist()+
            guides(col=F)+
            labs(fill="Target",x="Serum cholestoral in mg/dl")
          
          
        }else
          ggplot(df1,aes(as.factor(target),chol,fill=as.factor(target)))+
          geom_boxplot(alpha=0.2)+
          theme_economist()+
          scale_fill_economist()+
          labs(y="Serum cholestoral in mg/dl",x="Target",fill="Target")
        
        
        
      })
      
      #Blood_Sugar
      #============
      
      output$Blood_Sugar = renderPlot({
        
        if (input$Plot6 == "11"){
          ggplot(df1,aes(as.factor(fbs),fill=as.factor(target)))+
            geom_bar(stat="count")+
            theme_economist()+
            scale_fill_economist()+
            labs(x="Fasting Blood Pressure",fill="Target")
          
          
        }else
          ggplot(df1,aes(as.factor(fbs),fill=as.factor(target)))+
          geom_bar(stat="count",position="fill")+
          theme_economist()+
          scale_fill_economist()+
          labs(x="Fasting Blood Pressure",fill="Target",y="stacked count")
        
        
        
      })
      
      
      
      #Restecg
      #============
      
      output$Restecg = renderPlot({
        
        if (input$Plot7 == "13"){
          ggplot(df1,aes(as.factor(restecg),fill=as.factor(target)))+
            geom_bar(stat="count")+
            theme_economist()+
            scale_fill_economist()+
            labs(x="Resting electrocardiographic results",fill="Target")
          
          
        }else
          ggplot(df1,aes(as.factor(restecg),fill=as.factor(target)))+
          geom_bar(stat="count",position="fill")+
          theme_economist()+
          scale_fill_economist()+
          labs(x="Resting electrocardiographic results",fill="Target",y="stacked count")
        
        
        
      })
      
      
      #Max_Heart_Rate
      #============
      
      output$Max_Heart_Rate = renderPlot({
        
        if (input$Plot8 == "15"){
          ggplot(df1,aes(thalach,col=as.factor(target),fill=as.factor(target)))+
            geom_density(alpha=0.2)+
            theme_economist()+
            scale_colour_economist()+
            scale_fill_economist()+
            guides(col=F)+
            labs(fill="Target",x="Maximum heart rate achieved")
          
          
          
        }else
          ggplot(df1,aes(as.factor(target),thalach,fill=as.factor(target)))+
          geom_boxplot(alpha=0.2)+
          theme_economist()+
          scale_fill_economist()+
          labs(y="Maximum Heart Rate Achieved",x="Target",fill="Target")
        
        
      })
      
    
      
      #Exang
      #============
      
      output$Exang = renderPlot({
        
        if (input$Plot9 == "17"){
          ggplot(df1,aes(as.factor(exang),fill=as.factor(target)))+
            geom_bar(stat="count")+
            theme_economist()+
            scale_fill_economist()+
            labs(x="Exercise induced angina",fill="Target")
          
          
          
        }else
          ggplot(df1,aes(as.factor(exang),fill=as.factor(target)))+
          geom_bar(stat="count",position="fill")+
          theme_economist()+
          scale_fill_economist()+
          labs(x="Exercise induced angina",fill="Target",y="stacked count")
        
        
      })
      
      
      #Oldpeak
      #============
      
      output$Oldpeak = renderPlot({
        
        if (input$Plot10 == "19"){
          ggplot(df1,aes(oldpeak,col=as.factor(target),fill=as.factor(target)))+
            geom_density(alpha=0.2)+
            theme_economist()+
            scale_colour_economist()+
            scale_fill_economist()+
            guides(col=F)+
            labs(fill="Target",x="ST depression induced by exercise relative to rest")
          
          
          
        }else
          ggplot(df1,aes(as.factor(target),thalach,fill=as.factor(target)))+
          geom_boxplot(alpha=0.2)+
          theme_economist()+
          scale_fill_economist()+
          labs(y="ST depression induced by exercise relative to rest",x="Target",fill="Target")
        
        
      })
      
      
      
      
      #Slope
      #============
      
      output$Slope = renderPlot({
        
        if (input$Plot11 == "21"){
          ggplot(df1,aes(as.factor(slope),fill=as.factor(target)))+
            geom_bar(stat="count")+
            theme_economist()+
            scale_fill_economist()+
            labs(x="The slope of the peak exercise ST segment",fill="Target")
          
          
        }else
          ggplot(df1,aes(as.factor(slope),fill=as.factor(target)))+
          geom_bar(stat="count",position="fill")+
          theme_economist()+
          scale_fill_economist()+
          labs(x="The slope of the peak exercise ST segment",fill="Target",y="stacked count")
        
        
      })
      
      
      #Thal
      #============
      
      output$Thal = renderPlot({
        
        if (input$Plot12 == "23"){
          ggplot(df1,aes(as.factor(thal),fill=as.factor(target)))+
            geom_bar(stat="count")+
            theme_economist()+
            scale_fill_economist()+
            labs(x="Thal",fill="Target")
          
          
        }else
          ggplot(df1,aes(as.factor(thal),fill=as.factor(target)))+
          geom_bar(stat="count",position="fill")+
          theme_economist()+
          scale_fill_economist()+
          labs(x="Thal",fill="Target",y="stacked count")
        
      })
      
      
      
      
      
})