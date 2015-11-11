palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

library("dplyr")
library("data.table")


load(file = "./fit.Rdata")
load(file = "./percentile.Rdata")

shinyServer(function(input, output, session) {
  
  # Combine the selected variables into a new data frame

  
  personal_Values <- reactive({
  
    # Compose data frame
    data.frame(
      Name = c("State", 
               "Age",
               "Married",
               "Education",
               "Sex",
               "Employment status",
               "Has children",
               "Race"),
      Value = as.character(c(input$st, 
                             input$age,
                             input$mar,
                             input$education,
                             input$sex,
                             input$esr,
                             input$oc,
                             input$race)
                             ), 
      stringsAsFactors=FALSE)
  }) 
  
  output$values <- renderTable({
    personal_Values()
  })
  
  predict_income <- reactive({
    
    sex_map = c( 'Male' = 1,  'Female' = 2)
    mar_map = c( "Married" = 1, "Widowed" = 2, "Divorced" = 3, "Seperated" = 4,  "Single" = 5)
    deg_map = c("No School" = 1, "pre school" = 2, "Kindergarden" = 3,
                "Grade 1" = 4, "Grade 2" = 5, "Grade 3" = 6,
                "Grade 4" = 7, "Grade 5"=8, "Grade 6"=9,
                "Grade 7"=10, "Grade 8"=11, "Grade 9"=12,
                "Grade 10"=13, "Grade 11"=14, "Grade 12 - no diploma"=15,
                "high school"=16, "GED"=17, "college less than 1 year"=18,
                "college > 1 year, no degree"=19, "Associate's degree"=20, "Bachelor's degree"=21,
                "Master's degree"=22, "Professional degree beyond a bachelor's degree"=23, "Doctorate degree"=24
    )
    
    occupation_map = c(  "Managers" = 11, "Human Resource" = 13, "IT" = 15, "Engineers" = 17, # managers and engineers
                         "Scientist" = 19, "Social Workers" = 21, "Educators" = 25, "Entertainers" = 27, # scientist, social worker, educator
                         "Doctors" = 29, "Medical Assistant" = 31, "Officers" = 33, "Food Industry Workers" = 35, # doctors, cooks
                         "JANITORS" = 37, "Service Workers" = 39, "Salesperson" = 41, "Office Workers" = 43, # janitors, sales
                         "Forest Workers" = 45, "Mainance Worker" = 47, "Technicians" = 49, "Factory Worker" = 51, # social worker, 231 is laywer
                         "Transportion Operator" = 53, "Military" = 55, "Unemployed" = 99) # drivers
    
    esr_map = c("Civilian employed, at work" = 1,"Civilian employed, with job but not at work" = 2,
                "Unemployed" = 3, "Armed forces, at work" = 4, "Armed forces, with a job but not at work" =5,
                "Not in labor force" = 6)
    oc_map = c("yes" = 2, "no" = 1)
    
    sample <- c(input$st, input$age, mar_map[input$mar] , deg_map[input$education], sex_map[input$sex], esr_map[input$esr], oc_map[input$oc], input$race, occupation_map[input$SOCP])
    names(sample) <- c("ST", "AGEP","MAR", "SCHL","SEX", "ESR", "OC", "RAC1P", "SOCP")
    
    sample <- tbl_df(data.frame(t(sample)))
    test_sample <- model.matrix(~(.)^2 + (.)^3 , data = sample )
    test_sample <- tbl_df(data.frame(test_sample))
    
    x <- sum( coef * c(1, test_sample) )
    
    
    
  })
  
  output$percentiles <- renderText({
    
    income_percentile
  })
  
  output$summary <- renderText({
    paste("The predicted Income is: $", formatC(as.numeric(predict_income()), format = "f", digits = 2 ) )
  })
  
  output$summary2 <- renderText({
    paste("Your predicted income is greater than ", sum(income_percentile< predict_income()), "% of all U.S. populations.")
  })
  
  
  output$plot <- renderPlot({
    n <- sum(income_percentile< predict_income())
    color= cbind(t(rep("red", n)), t(rep("blue", 100-n )))
    barplot(income_percentile, ylim=c(0,100000), col = color)
  })

  
})