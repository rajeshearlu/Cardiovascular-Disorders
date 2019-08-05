
library(shinydashboard)

shinyUI(dashboardPage(
  skin = 'blue',
  
  #1 Header
  dashboardHeader(title = "Cardiovascular Disorders",
                  titleWidth = 280),
  
  
  #2 Sidebar
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      menuItem("App Overview", tabName = "intro", icon = icon("info")),
      # br(),
      #menuItem("Target Corelation", tabName = "Corel", icon = icon("signal")),
      # br(),
      menuItem("Contributing Factors", tabName = 'Factors', icon = icon("th-list")),
      menuSubItem("Target", tabName = "Target"),
      menuSubItem("Age", tabName = 'Age'),
      menuSubItem("Gender", tabName = "Gender"),
      menuSubItem("Chest_Pain", tabName = "Chest_Pain"),
      menuSubItem("Resting_BP", tabName = "Resting_BP"),
      menuSubItem("Cholestoral", tabName = "Cholestoral"),
      menuSubItem("Blood_Sugar", tabName = "Blood_Sugar"),
      menuSubItem("Restecg", tabName = "Restecg"),
      menuSubItem("Max_Heart_Rate", tabName = "Max_Heart_Rate"),
      menuSubItem("Exang", tabName = "Exang"),
      menuSubItem("Oldpeak", tabName = "Oldpeak"),
      menuSubItem("Slope", tabName = "Slope"),
      menuSubItem("Thal", tabName = "Thal"),
      br(),
      menuItem("Data Table", tabName = "data", icon = icon("database")),
      br(),
      br(),
      br(),
      br(),
      menuItem('GitHub', icon = icon('github'),
               href = '')
      
    )
 
  ),
  
  
  
  ## Body
  
  dashboardBody(
    tabItems(
      
      
      #Intro
      #=======
      
      
      tabItem(tabName = 'intro',
              fluidRow(box(
                h1('Introduction', style = "font-family: 'times'; font-si16pt"),
                br(),
                h5("Analysing the heart-disease-uci from kaggle datasets. In particular,
                the Cleveland database is the only one that has been used by ML 
                researchers to this date. The target field refers to the presence of
                heart disease in the patient. It is integer valued from 0 (no presence)
                and 1 (presence) and it will get stored in the target column. Target 
                is the dependent variable and rests all the variable are the independent 
                variable. We are analysing the data to see the gender bifurcation for 
                the patient having the heart diseases. As we are analysing the clinical
                data set so, we are dealing with some of the clinical abbreviations.",
                   style = "font-family: 'times'; font-si16pt"),

                h5("Age: The person's age in years", style = "font-family: 'times'; 
                font-si16pt"),
                
                h5("Gender: The person's sex (1 = male, 0 = female)", style = 
                "font-family: 'times'; font-si16pt"),
                
                h5("cp: The chest pain experienced (Value 1: typical angina, Value 2: atypical angina, Value 3: non-anginal pain, Value 4: asymptomatic)",
                   style = "font-family: 'times'; font-si16pt"),
                
                h5("trestbps: The person's resting blood pressure (mm Hg on admission to the hospital)", style = "font-family: 'times'; font-si16pt"),
                
                h5("chol: The person's cholesterol measurement in mg/dl", style = "font-family: 'times'; font-si16pt"),
                h5("fbs: The person's fasting blood sugar (if > 120 mg/dl, 1 = true; 0 = false)", style = "font-family: 'times'; font-si16pt"),
                h5("restecg: Resting electrocardiographic measurement (0 = normal, 1 = having ST-T wave abnormality, 2 = showing probable or definite left ventricular hypertrophy by Estes' criteria)", style = "font-family: 'times'; font-si16pt"),
                h5("thalach: The person's maximum heart rate achieved", style = "font-family: 'times'; font-si16pt"),
                h5("exang: Exercise induced angina (1 = yes; 0 = no)", style = "font-family: 'times'; font-si16pt"),
                h5("oldpeak: ST depression induced by exercise relative to rest ('ST' relates to positions on the ECG plot)", style = "font-family: 'times'; font-si16pt"),
                h5("slope: the slope of the peak exercise ST segment (Value 1: upsloping, Value 2: flat, Value 3: downsloping)", style = "font-family: 'times'; font-si16pt"),
                h5("ca: The number of major vessels (0-3)", style = "font-family: 'times'; font-si16pt"),
                h5("thal: A blood disorder called thalassemia (1 = normal; 2 = fixed defect; 3 = reversable defect)", style = "font-family: 'times'; font-si16pt"),
                   # target: Heart disease (0 = no, 1 = yes)", style = "font-family: 'times'; font-si16pt"),
                   # 
                
              width = 10))
              ),
    
      
      
      
      
      #Corel Info
      #===============
      
      tabItem(tabName = 'Corel',
              fluidRow(
                box(print("SUMMARY: The number of people with heart disease is more
                          than with a number of people having no heart disease."), width = 12, length =30),
                br(),
                br(),
                box(plotOutput("Corel"),  width = 12, length = 30))),
      
      
      #Contributing Factors
      #=====================
      
      #Target
      
      tabItem(tabName = 'Target',
              fluidRow(
                box(print("SUMMARY: The number of people with heart disease is more
                          than with a number of people having no heart disease."), width = 12, length =30),
                br(),
                br(),
                box(plotOutput("Target"),  width = 12, length = 30))),
      
      
      #Age
      
      tabItem(tabName = 'Age',
              fluidRow(box(background = 'aqua',
                           radioButtons('Plot1',label = h5('Select  Plot'),
                                       choices = list("Density" = "1", "BoxPlot" = "2"),
                                       inline = TRUE, selected = 1, width = 180))),
              print("SUMMARY: There are some differences in the age distributions 
                        of those with heart disease and those without."), 
              box(plotOutput("Age"),  width = 12, length = 30)),
              
      #Gender        
      
      tabItem(tabName = 'Gender',
              fluidRow(box(background = 'aqua',
                           radioButtons('Plot2',label = h5('Select  Plot'),
                                        choices = list("Count" = "3", "Percent" = "4"),
                                        inline = TRUE, selected = 3, width = 180))),
              print("SUMMARY: Males have a much higher rate of heart disease than the females."),
              box(plotOutput("Gender"),  width = 12, length = 30)),
      
      
      
      #Chest_Pain
      
      tabItem(tabName = 'Chest_Pain',
              fluidRow(box(background = 'aqua',
                           radioButtons('Plot3',label = h5('Select  Plot'),
                                        choices = list("Count" = "5", "Percent" = "6"),
                                        inline = TRUE, selected = 5, width = 180))),
              print("SUMMARY: Patient with typical angina pain has the least chances 
                    of heart disease, which is the most common type of cheast pain. 
                    Rest all other type have a equal chances of having a heart disease."),
              box(plotOutput("Chest_Pain"),  width = 12, length = 30)),
      
      
      #Resting_BP
      
      tabItem(tabName = 'Resting_BP',
              fluidRow(box(background = 'aqua',
                           radioButtons('Plot4',label = h5('Select  Plot'),
                                        choices = list("Density" = "7", "Box Plot" = "8"),
                                        inline = TRUE, selected = 7, width = 180))),
              print(" SUMMARY: Patients who are most likely to not suffer from the disease have
                    a slighly greater blood pressure than the patients who have heart 
                    diseases."),
              box(plotOutput("Resting_BP"),  width = 12, length = 30)),
      
      
      
      #Cholestoral
      
      tabItem(tabName = 'Cholestoral',
              fluidRow(box(background = 'aqua',
                           radioButtons('Plot5',label = h5('Select  Plot'),
                                        choices = list("Density" = "9", "Box Plot" = "10"),
                                        inline = TRUE, selected = 9, width = 180))),
              print("SUMMARY: Patients likely to suffer from heart diseases are having
                    higher cholestrol levels in comparison to the patients with target
                    0(likely to not suffer from the heart diseases."),
              
              box(plotOutput("Cholestoral"),  width = 12, length = 30)),
      
      
      
      
      #Blood_Sugar
      
      tabItem(tabName = 'Blood_Sugar',
              fluidRow(box(background = 'aqua',
                           radioButtons('Plot6',label = h5('Select  Plot'),
                                        choices = list("Count" = "11", "Percent" = "12"),
                                        inline = TRUE, selected = 11, width = 180))),
              print(" SUMMARY: People having fbs < 120 have more chance of having Heart Disease than people havnig fbs >120"),
              br(),
              box(plotOutput("Blood_Sugar"),  width = 12, length = 30)),
      
      
      
      #Restecg
      
      tabItem(tabName = 'Restecg',
              fluidRow(box(background = 'aqua',
                           radioButtons('Plot7',label = h5('Select  Plot'),
                                        choices = list("Count" = "13", "Percent" = "14"),
                                        inline = TRUE, selected = 13, width = 180))),
              print("SUMMARY: Resting electrocardiographic is 1 then person have more chances of suffering from Heart Disease"),
              br(),
              box(plotOutput("Restecg"),  width = 12, length = 30)),
      
      
      
      #Max_Heart_Rate
      
      tabItem(tabName = 'Max_Heart_Rate',
              fluidRow(box(background = 'aqua',
                           radioButtons('Plot8',label = h5('Select  Plot'),
                                        choices = list("Density" = "15", "Box Plot" = "16"),
                                        inline = TRUE, selected = 15, width = 180))),
              print("SUMMARY: Person with more heart rate is prone to heart diseases."),
              br(),
              box(plotOutput("Max_Heart_Rate"),  width = 12, length = 30)),
      
      
      
      
      #Exang
      
      tabItem(tabName = 'Exang',
              fluidRow(box(background = 'aqua',
                           radioButtons('Plot9',label = h5('Select  Plot'),
                                        choices = list("Count" = "17", "Percent" = "18"),
                                        inline = TRUE, selected = 17, width = 180))),
              print("This is it!"),
              br(),
              box(plotOutput("Exang"),  width = 12, length = 30)),
      
      
      
      #Oldpeak
      
      tabItem(tabName = 'Oldpeak',
              fluidRow(box(background = 'aqua',
                           radioButtons('Plot10',label = h5('Select  Plot'),
                                        choices = list("Density" = "19", "Box Plot" = "20"),
                                        inline = TRUE, selected = 19, width = 180))),
              print("SUMMARY: The value oldpeak increases the chances of heart disease decreases."),
              
              box(plotOutput("Oldpeak"),  width = 12, length = 30)),
      
      
      #Slope
      
      tabItem(tabName = 'Slope',
              fluidRow(box(background = 'aqua',
                           radioButtons('Plot11',label = h5('Select  Plot'),
                                        choices = list("Count" = "21", "Percent" = "22"),
                                        inline = TRUE, selected = 21, width = 180))),
              print("SUMMARY: Paients with downsloping slope have higher chances of 
                    having a heart disease as compared to flat slope."),
              
              box(plotOutput("Slope"),  width = 12, length = 30)),
      
      
      #Thal
      
      tabItem(tabName = 'Thal',
              fluidRow(box(background = 'aqua',
                           radioButtons('Plot12',label = h5('Select  Plot'),
                                        choices = list("Count" = "23", "Percent" = "24"),
                                        inline = TRUE, selected = 23, width = 180))),
              print("SUMMARY: We can observe that heart diseases highly influenced by hereditary diseases"),
              
              box(plotOutput("Thal"),  width = 12, length = 30))
      
      
      
    )
      
      
            
  )
  
  
  
))


