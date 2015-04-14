################################################################################
### Title: server.R                                                          ###
###                                                                          ###
### Project: Coursera Data Science Capstone Project                          ###
###                                                                          ###
### Version: 0.1 - 04/14/2015                                                ###
###                                                                          ###
### Description: Server Script for Text Prediction UI in Shiny               ###
###                                                                          ###
### Authors: Christopher Stewart <stewart.christophermichael@gmail.com>      ###
###                                                                          ###
### Maintainer: Christopher Stewart <stewart.christophermichael@gmail.com>   ###
###                                                                          ###
### Versions:                                                                ###
###     > 0.1 - 04/14/2015 - 14:36:33:                                       ###
###         creation                                                         ###
###                                                                          ###
################################################################################
###

#rm(list = ls()); gc(reset = TRUE)

require(shiny); require(stringr); 

shinyServer(function(input, output) {
  values <- reactiveValues()
  # calculate scores
  observe({
    input$texttype
    values$text.type <- isolate ({
      (input$extraverted + 
        recode(input$reserved, "1 = '7'; 2 = '6'; 3 = '5'; 4 = '4'; 5 = '3'; 6 = '2'; 7 = '1'"))/(2)
    })
    values$agr <- isolate ({
      (input$sympathetic + 
           recode(input$critical, "1 = '7'; 2 = '6'; 3 = '5'; 4 = '4'; 5 = '3'; 6 = '2'; 7 = '1'"))/(2)
    })
    values$consc <- isolate ({
      (input$dependable + 
          recode(input$disorganized, "1 = '7'; 2 = '6'; 3 = '5'; 4 = '4'; 5 = '3'; 6 = '2'; 7 = '1'"))/(2)
    })
    values$emot <- isolate ({
      (input$calm + 
         recode(input$anxious, "1 = '7'; 2 = '6'; 3 = '5'; 4 = '4'; 5 = '3'; 6 = '2'; 7 = '1'"))/(2)
    })
    values$open <- isolate ({
      (input$open + 
         recode(input$conventional, "1 = '7'; 2 = '6'; 3 = '5'; 4 = '4'; 5 = '3'; 6 = '2'; 7 = '1'"))/(2)  
    })
  output$extroversion <- renderText({
      paste("Your score on the Extroversion scale is: ", values$ext)
    })
  output$agreeableness <- renderText({
      paste("Your score on the Agreeableness scale is: ", values$agr)
    })
  output$conscientiousness <- renderText({
      paste("Your score on the Conscientiousness scale is: ", values$consc)
  })
  output$emotionalstability <- renderText({
    paste("Your score on the Emotional Stability scale is: ", values$emot)
  })
  output$openness <- renderText({
    paste("Your score on the Openness scale is: ", values$open)
  })
})})