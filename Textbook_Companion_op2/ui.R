library(shiny)
library(shinyAce)
library(bslib)
library(htmltools)
library(markdown)
library(stats)

js <- "
Shiny.addCustomMessageHandler('resetFileInput', function(elementId) {
  var el = document.getElementById(elementId);
  if (el) {
    el.value = '';
  }
});
"

ui <- 
  tagList(
  tags$head(tags$script(HTML(js))),
  page_navbar(
    includeCSS("www/styles.css"),
    title = "Chapter 7",
    bg = "#05457d",
    inverse = TRUE,
    collapsible = TRUE,
    lang = "en",
    fillable = TRUE,
    fillable_mobile = TRUE,
    underline = TRUE,
    nav_panel(
      title = "Chapter 7: Introduction",
      fluidPage(strong("Comparing averages with two (or one) groups"),
        fluidRow(
          column(12,
                 accordion(
                   accordion_panel(
                     "Introduction",
                     markdown("**This chapter explores how to compare the average of a group to something else for simple experimental designs.**

                    For instance, one of the most common questions that researchers ask is, “Do we have evidence that the average value of a trait in one group differs from the average for anther group?”.
                    
                    Alternatively, a researcher might wish to test whether the mean value of a group differs from a specified value (e.g., Does the mean temperature of a group of subjects in a cold room differ from 37 degrees Celsius?). This chapter explores t-tests as a way to answer these questions.
                    
                    Note that the videos in this chapter discuss results of t-tests in terms of statistical significance (we will update these videos, in this respect). In this light, we particularly draw your attention to the Practice Problems and Answers for this chapter because these materials demonstrate a more modern interpretation of t-tests by focusing on effect size.
                    
                    The Practice Problems and Answers also follow the advice in the next Chapter, by abandoning the concept of statistical significance, and show how to test assumptions."),
                     ),
                   accordion_panel(
                     "A Note on Non-Parametric Tests",
                     markdown("This website does not consider non-parametric tests to compare measures of central tendency (e.g., median value or average value) between groups for two reasons. First, non-parametric tests involve (often unappreciated) assumptions that hinder analyses. For example, researchers often use a Mann-Whitney U test to analyse data that fail to meet the assumptions of a t-test; here, researchers aim to evaluate evidence that the median values differ between the groups.

                    This approach can be problematic, however, because Mann-Whitney U tests provide evidence for whether two distributions differ in general (i.e., including shape), not for whether median values differ between two distributions, specifically. Therefore, if two distributions being compared differ in shape, then a small p-value might arise from a Mann-Whitney U test (at least in part) due to shape differences, and not due differences in median values.
                    
                    In other words, in order to use a Mann-Whitney U test to evaluate evidence for different median values between groups, the researcher must be confident that the two distributions have similar shapes. Such unappreciated assumptions make non-parametric tests less desirable. Second, non-parametric methods cannot provide meaningful estimates of effect size with appropriate uncertainty.
                    
                    As analyses of effect size offer more insight than those that focus on p-values (see Chapter 8: Abandon statistical significance, non-parametric tests have less to offer than alternative methods, such as computational approaches (randomization / permutation tests, bootstrapping).")
                     )
                  )
            )
      )
    )
    ),
    
    nav_panel(
      title = "T-Tests: Two Samples",
      fluidPage(
        withMathJax(),
        fluidRow(
          column(12)
        ),
        fluidRow(
          column(12,
                 accordion(
                   accordion_panel(
                     "Introduction: 2 Sample T-Tests",
                     markdown("How can we test the hypothesis that the average values from two different groups are different?
                              For simple experimental designs, researchers most commonly address this question with a 2-sample t-test, the subject of this video.
                              We discuss when 2-sample t-tests are useful, how they work, and how to perform one in R."),
                     HTML('<iframe 
                          src="https://media.ed.ac.uk/media/1_dcuvndfl" 
                          width="560" 
                          height="315" 
                          frameborder="0" 
                          allow="accelerometer; autoplay; gyroscope; picture-in-picture" 
                          allowfullscreen>
                        </iframe>'),
                   ),
                  
                   accordion_panel(
                     "What can they do?",
                     markdown("**2-sample T-tests allow you to compare the averages between two groups.**"),
                     markdown("1. How big is the difference between these two groups (and how much uncertainty accompanies this estimate?) *[effect size]*
                               2. Do we have evidence whether the difference differs from zero? *[p-value]*"),
                   ),
                   accordion_panel(
                     "How do they work?",
                    
                     markdown("**Example: Average Weight of Apples \U1F34E vs. Average Weight of Oranges \U1F34A**"),
                     markdown("In this example, we are comparing the average weight of a group of apples and the average weight of a group of oranges using a two-sample T-Test."),
                     markdown("A two-sample T-test works by calculating a test statistic, called 't'"),
                     helpText("$$t = \\frac{\\overline{X}_1 - \\overline{X}_2}{S_p}$$"),
                     p("\\(\\overline{X}_1\\) = mean value of group 1 (\U1F34E)"),
                     p("\\(\\overline{X}_2\\) = mean value of group 2 (\U1F34A)"),
                     p("\\(\\overline{X}_1 - \\overline{X}_2\\) represents the difference between these two averages"),
                     p("\\(S_p\\) = standard error (SE) in the difference between the means; this represents the amount of uncertainty in the difference between means"),
                     markdown("**The t-value estimates the difference between two means, corrected for uncertainty in the difference.**"),
                    
                     wellPanel(
                     radioButtons(
                       inputId = "quiz1_test",
                       label = "How does the numerator (\\(\\overline{X}_1 - \\overline{X}_2\\)) influence t?",
                       choices = list("Large Numerator = Large t" = "option1", "Small Numerator = Large t" = "option2"),
                       inline = TRUE,
                       selected = character(0)
                     ),
                     uiOutput("quiz1_message"),
                     radioButtons(
                       inputId = "quiz2_test",
                       label = "What does high Sp indicate?",
                       choices = list("Low uncertainty" = "option1", "High uncertainty" = "option2"),
                       inline = TRUE,
                       selected = character(0)
                     ),
                     uiOutput("quiz2_message"),
                     radioButtons(
                       inputId = "quiz3_test",
                       label = "How does the denominator influence t?",
                       choices = list("Large Denominator = Large t" = "option1", "Small Denominator = Large t" = "option2"),
                       inline = TRUE,
                       selected = character(0)
                     ),
                     uiOutput("quiz3_message"),
                     markdown("**So, overall - the t-value is dependent on the differences in means and on the error of this difference.**")
                     ),
                     ),
                   accordion_panel(
                     "Standard Assumptions",
                     HTML('<div class="yellow-box">
                        <p>These are standard assumptions for parametric tests, and are listed in decreasing order of importance.</p>
                        <ol>
                          <li>Random sampling: samples are randomly sampled from a population to prevent bias.</li>
                          <li>Independence: Independently obtained to avoid pseudo-replication.</li>
                          <li>Homogeneity of Variances (no outliers): is the variation amongst the groups within the same range?</li>
                          <li>Normality within each group: do the groups fall in a normal distribution, or is the data skewed?</li>
                        </ol>
                      </div>'),
                     
                     markdown("Using our apples and oranges example:
                                
                        1. Apples/oranges picked from each tree at random, not just from one tree or not just from big trees.
                        2. Re-set the scales after each measurement, avoid some apples being accidentally weighed together with oranges.
                        3. Check Homogeneity of Variances by plotting the data.
                        4. Check Normality within each group by plotting the data.
                      "),
                     
                     HTML('<div class="purple-box">
                          <p>So, let\'s get plotting!</p>
                          </div>')
                   ),
                   
                   accordion_panel(
                     "Step 1: Preparing and Viewing the Data",
                     wellPanel(
                       markdown("First, let's confirm the data is here. Type in `head(fruit)` and click 'Run Code'."),
                     markdown("Next let's take a look at a summary of this dataset with `summary(fruit)`."),
                     markdown("This output tells us a little more about the dataset."),
                     aceEditor("chapter_7_code1",
                               mode = "r",
                               theme = "solarized_dark",
                               value = "#type your code in here!",
                               height = "200px",
                               fontSize = 14,
                               showLineNumbers = TRUE,
                               autoComplete = "live",
                               autoCompleters = c("rlang", "text", "snippet", "keyword"),
                               showPrintMargin = FALSE,
                               setBehavioursEnabled = TRUE,
                     ),
                     actionButton("run_chapter_7_code1", "Run Code"),
                     verbatimTextOutput("chapter_7_code_1_result"),
                     uiOutput("t_test_code1")
                     ),
                   ),
                   accordion_panel(
                     "Testing for Homogeneity of Variances",
                     fluidRow(
                       column(6,
                              markdown("**What does 'homogeneity of variances' even mean?**"),
                              markdown("*For a more detailed examination of variance, go back to chapter X*"),
                              markdown("Briefly, homogeneity of variances describes the assumption that the variance across samples is equal. While there are calculations you can perform to quantify this, the best approach is to visually inspect your data."),
                              markdown("So, let's do just that!"),
                              wellPanel(
                              markdown("Using the code editor below, type in (or copy/paste) the following code:
                                       ````
                                        boxplot(Size ~ Fruit, data = fruit)
                                        stripchart(Size ~ Fruit, data = fruit,
                                                  add = TRUE, vertical = TRUE, method = \"jitter\", 
                                                  pch = 21, col = \"maroon\", bg = \"bisque\")
                                       ````
                                       "),
                              aceEditor("chapter_7_code_boxplot",
                                        mode = "r",
                                        theme = "solarized_dark",
                                        value = "#type your code in here!",
                                        height = "200px",
                                        fontSize = 14,
                                        showLineNumbers = TRUE,
                                        autoComplete = "live",
                                        autoCompleters = c("rlang", "text", "snippet", "keyword"),
                                        showPrintMargin = FALSE,
                                        setBehavioursEnabled = TRUE,
                              ),
                              actionButton("run_chapter_7_code_boxplot", "Run Code")
                              ),
                       ),
                       column(6,
                              plotOutput("t_test_code_boxplot", width = "100%", height = "600px"),
                              uiOutput("boxplot_text_output"),
                              uiOutput("boxplot_quiz1")
                       )
                     )
                   ),
                   accordion_panel(
                     "Testing for Normality",
                     fluidRow(
                       column(6,
                              markdown("**What does 'Normality' mean?**"),
                              wellPanel(
                                markdown("Using the code editor below, type in (or copy/paste) the following code:
                                       ````
                                        hist(fruit$Size)
                                       ````
                                       "),
                                aceEditor("chapter_7_code_normality",
                                          mode = "r",
                                          theme = "solarized_dark",
                                          value = "#type your code in here!",
                                          height = "200px",
                                          fontSize = 14,
                                          showLineNumbers = TRUE,
                                          autoComplete = "live",
                                          autoCompleters = c("rlang", "text", "snippet", "keyword"),
                                          showPrintMargin = FALSE,
                                          setBehavioursEnabled = TRUE,
                                ),
                                actionButton("run_chapter_7_code_normality", "Run Code")
                            ),
                       ),
                      column(6,
                             plotOutput("t_test_code_normality", width = "100%", height = "600px"),
                             uiOutput("normality_text_output"),
                             uiOutput("normality_quiz1")
                             ),
                     )
                   ),
                   accordion_panel(
                     "Step 2: Running the t-test in R",
                     markdown("In R, we type the following code:"),
                     markdown("```
                              t.test(Size ~ Fruit, var.equal = TRUE, data = fruit)
                              ```"),
                     markdown("`Size ~ Fruit` defines the columns used for the analysis. The first step is to decide the order of the column variables for this model. Generally, best practice is to do `numerical~categorical`. Alternatively, you can think of it as `dependent variable ~ independent variable`."),
                     markdown("The next parameter in the code is `var.equal = TRUE`, which tells the function that the variances are indeed equal. We'll come back to this in the next section ‘Welch's t-test'."),
                     markdown("Finally, we need to tell this function which data to use, so we point it at `data = fruit`."),
                     wellPanel(markdown("**Now it's your turn!** Type or copy in the above code for the t-test:"),
                     aceEditor("chapter_7_code2",
                               mode = "r",
                               theme = "solarized_dark",
                               value = "#type your code in here!",
                               height = "200px",
                               fontSize = 14,
                               showLineNumbers = TRUE,
                               autoComplete = "live",
                               autoCompleters = c("rlang", "text", "snippet", "keyword"),
                               showPrintMargin = FALSE,
                               setBehavioursEnabled = TRUE,
                               ),
                     actionButton("run_chapter_7_code2", "Run Code"),
                     verbatimTextOutput("chapter_7_code_2_result"),
                     uiOutput("t_test_code2")),
                   ),
                   accordion_panel(
                     "Understanding the t-test result",
                     wellPanel( markdown("You should have gotten this output:
                  ````
                  Two Sample t-test
                  
                  data:  Size by Fruit
                  t = -4.2374, df = 8, p-value = 0.002848
                  alternative hypothesis: true difference in means is not equal to 0
                  95 percent confidence interval:
                   -4.858271 -1.433977
                  sample estimates:
                   mean in group Apple mean in group Orange 
                              11.94325             15.08938 
                  ````
                  "),),
                     markdown("**Let's interpret this together.**"),
                     markdown("First, try and interpret the average size of the apples and the oranges:"),
                     textInput(
                       inputId = "t_test_interpretation_quiz1",
                       label = "Average apple \U1F34E size?",
                       placeholder = "Type your answer here"
                     ),
                     actionButton(inputId = "t_test_interpretation_quiz_submit1",
                                  label = "Submit your Answer"),
                     uiOutput("t_test_interpretation_quiz_result1"),
                     textInput(
                       inputId = "t_test_interpretation_quiz2",
                       label = "Average orange \U1F34A size?",
                       placeholder = "Type your answer here"
                     ),
                     markdown("**Now, let's discuss the t-test results**"),
                     actionButton(inputId = "t_test_interpretation_quiz_submit2",
                                  label = "Submit your Answer"),
                     uiOutput("t_test_interpretation_quiz_result2"),
                     radioButtons(
                       inputId = "t_test_interpretation_quiz3",
                       label = "Where can we assess the difference between the two groups?",
                       choices = list("95% Confidence Interval" = "option1", "df" = "option2", "p-value" = "option3"),
                       inline = TRUE,
                       selected = character(0)
                     ),
                     uiOutput("t_test_interpretation_quiz_result3"),
                     radioButtons(
                       inputId = "t_test_interpretation_quiz4",
                       label = "Do our data suggest that apples and oranges differ in weight, on average?",
                       choices = list("Yes" = "option1", "No" = "option2", "We don't know" = "option3"),
                       inline = TRUE,
                       selected = character(0)
                     ),
                     uiOutput("t_test_interpretation_quiz_result4"),
                     uiOutput("conditional_explanation_1")
                   ),
                   accordion_panel(
                     "Final Remarks",
                  
                     markdown("You should now have a good understanding of how to use R to apply a t-test in order to compare two sample averages and decide whether the differences are statistically significant."),
                     wellPanel(
                       markdown("**How would you write this up?**"),
                     markdown("Why don't you give it a go in the textbox below (1 sentence):"),
                     markdown("*hint: remember to include the relevant information such as the means, the SE, the t-test results, and the 95% CI!*"),
                     textAreaInput(
                       inputId = "t_test_free_write",
                       label = "Your interpretation:",
                       placeholder = "The results show that...",
                       width = 400,
                       height = 200,
                       resize = "both"
                     ),
                     actionButton("t_test_free_write_submit", label = "Submit"),
                     uiOutput("t_test_free_write_result")
                     
                     )
                   )
                 )
          )
        )
      )
    ),
    
    nav_panel(
      title = "T-Test: 2 Sample (Welchs)",
      fluidPage(
           accordion(
             accordion_panel(
               "Introduction",
               ),
             accordion_panel(
               "Title1",
               ),
             accordion_panel(
               "Title",
               ),
           )
        ),
      ),
    nav_panel(
      title = "T-Test: 1 Sample",
      fluidPage(
        accordion(
          accordion_panel(
            "Introduction",
          ),
          accordion_panel(
            "Title1",
          ),
          accordion_panel(
            "Title",
          ),
        )
      ),
    ),
    nav_panel(
      title = "T-Test: Paired",
      fluidPage(
        accordion(
          accordion_panel(
            "Introduction",
          ),
          accordion_panel(
            "Title1",
          ),
          accordion_panel(
            "Title",
          ),
        )
      ),
    ),
    nav_panel(
      title = "Bootstrapping",
      fluidPage(
        accordion(
          accordion_panel(
            "Introduction",
          ),
          accordion_panel(
            "Title1",
          ),
          accordion_panel(
            "Title",
          ),
        )
      ),
    ),
    nav_panel(
      title = "Practice Problems",
      fluidPage(
        accordion(
          accordion_panel(
            "Introduction",
          ),
          accordion_panel(
            "Title1",
          ),
          accordion_panel(
            "Title",
          ),
        )
      ),
    ),
    nav_panel(
      title = "Recommended Reading",
      fluidPage(
        accordion(
          accordion_panel(
            "Introduction",
          ),
          accordion_panel(
            "Title1",
          ),
          accordion_panel(
            "Title",
          ),
        )
      ),
    ),
    nav_spacer(),
    nav_menu(
      title = "Links",
      align = "right",
      nav_item(tags$a("Posit", href = "https://posit.co")),
      nav_item(tags$a("Shiny", href = "https://shiny.posit.co"))
    )
  )
)

