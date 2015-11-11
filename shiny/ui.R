shinyUI(pageWithSidebar(
  
  headerPanel('Predicting your Income'),
  
  
  sidebarPanel(
    numericInput('st', 'State', 30,
                 min = 1, max = 56), 
    
    
    selectInput('sex', 'Sex', c("Male", "Female"),
                selected="Male"),
    
    selectInput('education', 'Highest Education Attained', 
                c("No School", "pre school", "Kindergarden",
                  "Grade 1", "Grade 2", "Grade 3",
                  "Grade 4", "Grade 5", "Grade 6",
                  "Grade 7", "Grade 8", "Grade 9",
                  "Grade 10", "Grade 11", "Grade 12 - no diploma",
                  "high school", "GED", "college less than 1 year",
                  "college > 1 year, no degree", "Associate's degree", "Bachelor's degree",
                  "Master's degree", "Professional degree beyond a bachelor's degree", "Doctorate degree"
                  ),
                selected="Bachelor's degree"),
    
    selectInput('mar', 'Marriage Status', c("Married","Widowed", "Divorced", "Seperated", "Single"),
                selected="Single"),
    
    selectInput('SOCP', 'Occupations', c(  "Managers" , "Human Resource" , "IT" , "Engineers" , 
                                           "Scientist" , "Social Workers" , "Educators" , "Entertainers" , 
                                           "Doctors" , "Medical Assistant", "Officers", "Food Industry Workers", # doctors, cooks
                                           "JANITORS", "Service Workers", "Salesperson", "Office Workers", # janitors, sales
                                           "Forest Workers", "Mainance Worker", "Technicians", "Factory Worker", # social worker, 231 is laywer
                                           "Transportion Operator", "Military", "Unemployed") , 
                selected = "Engineers"),
    
    
    selectInput('oc', 'Has Children', c("yes", "no"),
                 selected = "no"), 
    
    numericInput('age', 'Age', 30,
                 min = 10, max = 70), 
    
    selectInput('esr', 'Employment status', c("Civilian employed, at work","Civilian employed, with job but not at work",
                                              "Unemployed", "Armed forces, at work", "Armed forces, with a job but not at work", "Not in labor force"),
                selected = "Civilian employed, at work"),
    
    numericInput('race', 'Race', 1,
                 min = 1, max = 9)
  ),
  
  mainPanel(
    div(tableOutput("values"), style = "font-size:130%"),
    div(textOutput("summary") , style = "font-size:200%"),
    div(textOutput("summary2"), style = "font-size:200%"),
    plotOutput('plot')
  )
  
  
))