library(shiny)

options(shiny.trace = TRUE)

server <- function(input, output, session) {
  
  fruit <- read.csv("Fruit.csv")
  
  values <- reactiveValues(result = NULL)
  
  # assessing data
  observeEvent(input$run_chapter_7_code1, {
    code <- input$chapter_7_code1
    
    temp_env <- new.env()
    assign("fruit", fruit, envir = temp_env)
    
    result <- tryCatch({
      eval(parse(text = code), envir = temp_env)
    }, error = function(e) {
      e$message
    })
    
    values$result <- result

    output$chapter_7_code_1_result <- renderPrint({
      values$result
    })
    
    output$t_test_code1 <- renderUI({
      if (is.data.frame(values$result)) {
        div(class = "success-box",
            "\U1F44D That's our data! Looks good!")
      } else if (is.matrix(values$result)) {
        div(class = "success-box",
            "\U1F44D Great! Now have a look at what the summary tells us about our data")
      } else {
        div(class = "error-box",
            "\U0001F937\u200D\u2640\uFE0F Did you mean to do that? Totally fine if it was... If that was your intention, then please, carry on.")
      }
    })
  })
  
  #homogeneity of variances
  observeEvent(input$run_chapter_7_code_boxplot, {
    code <- input$chapter_7_code_boxplot
    
    temp_env <- new.env()
    assign("fruit", fruit, envir = temp_env)
    
    plot_result <- NULL
    result <- tryCatch({
      eval(parse(text = code), envir = temp_env)
      
      # Capture the plot if generated
      plot_result <- recordPlot()
      
      NULL  # return NULL to indicate success
    }, error = function(e) {
      e$message  # return error message
    })
    
    values$result <- result
    values$plot_result <- plot_result
    
    # Check if the code used the boxplot function
    boxplot_detected <- grepl("boxplot", code, ignore.case = TRUE) || 
      grepl("geom_boxplot", code, ignore.case = TRUE)
    
    # Display the result as text (errors, warnings, etc.)
    output$chapter_7_code_boxplot_result <- renderPrint({
      values$result
    })
    
    # Render the captured plot
    output$t_test_code_boxplot <- renderPlot({
      if (!is.null(values$plot_result)) {
        replayPlot(values$plot_result)
      } else {
        plot.new()  # clear the plot area
        text(0.5, 0.5, "\U0001F937\u200D\u2640\uFE0F Oops! Why don't you try again?", cex = 1.5)
      }
    })
    
    # Display the custom message if a boxplot was detected
    output$boxplot_text_output <- renderUI({
      if (boxplot_detected && is.null(result)) {
        radioButtons("variance_question", 
                     label = HTML('<div class="success-box">\U1F64C Great Job! Do you think that these variances are equal?</div>'), 
                     choices = list("Yes" = "option1", 
                                    "No" = "option2", 
                                    "Can't tell from this" = "option3"),
                     selected =  character(0))
      } else {
        NULL
      }
    })
    
    observeEvent(input$variance_question, {
      feedback <- if (input$variance_question == "option1") {
        div(class = "success-box", "\U1F64C Correct!")
      } else {
        div(class = "error-box", "\U1F914 Not quite - try again!")
      }
      
      output$boxplot_quiz1 <- renderUI({
        feedback
      })
    })
  })
  
  #testing for normality
  observeEvent(input$run_chapter_7_code_normality, {
    code <- input$chapter_7_code_normality
    
    temp_env <- new.env()
    assign("fruit", fruit, envir = temp_env)
    
    # Run the user code and check for errors
    result <- tryCatch({
      eval(parse(text = code), envir = temp_env)
      NULL  # No error, return NULL
    }, error = function(e) {
      e$message  # Return the error message
    })
    
    # Store the result
    values$result <- result
    
    # Detect if a histogram was created
    histogram_detected <- grepl("hist\\(", code, ignore.case = TRUE) || grepl("geom_histogram", code, ignore.case = TRUE)
    
    # Render the result of the code execution as text
    output$chapter_7_code_normality_result <- renderPrint({
      values$result
    })
    
    # Render the plot directly from the environment
    output$t_test_code_normality <- renderPlot({
      # Check if the plot is available in the active device
      if (is.null(result)) {  # Ensure no errors occurred
        eval(parse(text = code), envir = temp_env)  # Re-evaluate the code to ensure the plot is created
      } else {
        plot.new()  # Clear the plot area if there is an error
        text(0.5, 0.5, "\U0001F937\u200D\u2640\uFE0F Oops! Why don't you try again?", cex = 1.5)
      }
    })
    
    # Display the custom message if a histogram was detected
    output$normality_text_output <- renderUI({
      if (histogram_detected && is.null(result)) {
        radioButtons("normality_question", 
                     label = HTML('<div class="success-box">\U1F64C Great Job! Do you think that our data is normally distributed?</div>'), 
                     choices = list("Yes" = "option1", 
                                    "No" = "option2", 
                                    "Can\'t tell from this" = "option3"),
                     selected = character(0))
      } else {
        NULL
      }
    })
    
    # Handle quiz responses
    observeEvent(input$normality_question, {
      feedback <- if (input$normality_question == "option1") {
        div(class = "success-box", "\U1F64C Correct!")
      } else {
        div(class = "error-box", "\U1F914 Not quite - try again!")
      }
      
      output$normality_quiz1 <- renderUI({
        feedback
      })
    })
  })
  
  
  #running the t-test
  observeEvent(input$run_chapter_7_code2, {
    code <- input$chapter_7_code2
    
    temp_env <- new.env()
    assign("fruit", fruit, envir = temp_env)
    
    result <- tryCatch({
      eval(parse(text = code), envir = temp_env)
    }, error = function(e) {
      e$message
    })
    
    values$result <- result
 
    output$chapter_7_code_2_result <- renderPrint({
      values$result
    })
    
    output$t_test_code2 <- renderUI({
      if (inherits(values$result, "htest")) {
        div(class = "success-box",
            "\U1F64C Great Job! You have just executed your first t-test!")
      } else {
        div(class = "error-box",
            "\U0001F937\u200D\u2640\uFE0F Did you mean to do that? Totally fine if it was... but it looks like you are trying something that isn't a t-test. If that was your intention, then please, carry on.")
      }
  })
})

  #t-test quizzes
  observeEvent(input$quiz1_test, {
    output$quiz1_message <- renderUI({
      if (input$quiz1_test == "option1"){
        div(class = "success-box", "\U1F64C Correct!")
      } else {
        div(class = "error-box", "\U1F914 Not quite - try again!")
      }
    })
  })
  
  observeEvent(input$quiz2_test, {
    output$quiz2_message <- renderUI({
      if (input$quiz2_test == "option2"){
        div(class = "success-box", "\U1F64C Correct!")
      } else {
        div(class = "error-box", "\U1F914 Not quite - try again!")
      }
    })
  })
  
  observeEvent(input$quiz3_test, {
    output$quiz3_message <- renderUI({
      if (input$quiz3_test == "option2"){
        div(class = "success-box", "\U1F64C Correct!")
      } else {
        div(class = "error-box", "\U1F914 Not quite - try again!")
      }
    })
  })

  observeEvent(input$t_test_interpretation_quiz_submit1, {
      user_answer <- as.numeric(input$t_test_interpretation_quiz1)
      output$t_test_interpretation_quiz_result1 <- renderUI({
      if (!is.na(user_answer) && user_answer >= 11 && user_answer <= 12) {
        div(class = "success-box", "\U1F64C Correct!")
      } else {
        div(class = "error-box", "\U1F914 Not quite - try again!")
      }
    })
  })

  observeEvent(input$t_test_interpretation_quiz_submit2, {
    user_answer2 <- as.numeric(input$t_test_interpretation_quiz2)
    output$t_test_interpretation_quiz_result2 <- renderUI({
      if (!is.na(user_answer2) && user_answer2 >= 15 && user_answer2 <= 15.1) {
        div(class = "success-box", "\U1F64C Correct!")
      } else {
        div(class = "error-box", "\U1F914 Not quite - try again!")
      }
    })
  })
  
  observeEvent(input$t_test_interpretation_quiz3, {
    output$t_test_interpretation_quiz_result3 <- renderUI({
      if (input$t_test_interpretation_quiz3 == "option1"){
        div(class = "success-box", "\U1F64C Correct!")
      } else {
        div(class = "error-box", "\U1F914 Not quite - try again!")
      }
    })
  })
  
  observeEvent(input$t_test_interpretation_quiz4, {
    output$t_test_interpretation_quiz_result4 <- renderUI({
      if (input$t_test_interpretation_quiz4 == "option1"){
        div(class = "success-box", "\U1F64C Correct!")
      } else {
        div(class = "error-box", "\U1F914 Not quite - try again!")
      }
    })
  })
  
  #conditonal output for the correct answer to quiz4
  output$conditional_explanation_1 <- renderUI({
    if (!is.null(input$t_test_interpretation_quiz4) && input$t_test_interpretation_quiz4 == "option1") {
      HTML("<p> Here's why: </p>
              <ol> 
              <li> 95% Confidence Interval: The range does not include 0, which implies that the difference is significantly different from 0. </li>
              <li> p-value: Our p-value of 0.0028 is less than 0.05, our threshold, meaning we can reject our null-hypothesis and conclude that we have enough evidence to believe that the weights of apples and oranges are significantly different from one-another.</li>
           </ol>")
    } else {
      NULL
    }
  })
  
  observeEvent(input$t_test_free_write_submit, {
    output$t_test_free_write_result <- renderUI({
      well_done_message <- div(class = "success-box", 
                               "\U1F64C Well done! Now, you can compare it against what we wrote and see if it is similar!")
      
      comparison_text <- HTML("<p>
      Our samples indicate that, on average, apples (mean mass +/- SE: 11.9g +/- 0.3) and oranges (15.1 +/- 0.7) differ in mass by (95% CI) 1.43 – 4.86 g, which differs significantly from zero (2-sample t-test; t = 4.24, df = 8, p = 0.0028).
    </p>")
      
      tagList(well_done_message, comparison_text)
    })
  })
  
  
}
