library(shiny)
library(shinyjs)

# data struct packages
library(collections, warn.conflicts = FALSE)

# data storage
library(googlesheets4)

# init global vars
source("data_store.R")
source("plot.R")

# Create the UI to display data
ui <- fluidPage(
  useShinyjs(),
  fluidRow(
    column(12,
      # TODO: use lapply to flatten
      tabsetPanel(id=tabset_id, selected="page0",
        tabPanel("Question_1",
                 uiOutput("page0")
        ),
        tabPanel("Present_0",
                 uiOutput("page1")
        ),
        tabPanel("Present_1", 
                 uiOutput("page2")
        ),
        tabPanel("Present_2",
                 uiOutput("page3")
        ),
        tabPanel("Present_3",
                 uiOutput("page4")
        ),
        tabPanel("Question_2",
                 uiOutput("page5")
        ),
        tabPanel("Question_3", 
                 uiOutput("page6")
        ),
        tabPanel("Question_4",
                 uiOutput("page7")
        ),
        tabPanel("Question_5",
                 uiOutput("page8")
        ),
        tabPanel("Present_4", 
                 uiOutput("page9")
        ),
        tabPanel("Present_5",
                 uiOutput("page10")
        ),
        tabPanel("Present_6",
                 uiOutput("page11")
        ),
        tabPanel("Question_6",
                 uiOutput("page12")
        ),
        tabPanel("Question_7",
                 uiOutput("page13")
        ),
        tabPanel("Question_8",
                 uiOutput("page14")
        ),
        tabPanel("Question_9",
                 uiOutput("page15")
        ),
        tabPanel("Present_7", 
                 uiOutput("page16")
        ),
        tabPanel("Present_8",
                 uiOutput("page17")
        ),
        tabPanel("Present_9",
                 uiOutput("page18")
        ),
        tabPanel("Question_10",
                 uiOutput("page19")
        ),
        tabPanel("Question_11",
                 uiOutput("page20")
        ),
        tabPanel("Question_12",
                 uiOutput("page21")
        ),
        tabPanel("Question_13",
                 uiOutput("page22")
        ),
        tabPanel("Present_10", 
                 uiOutput("page23")
        ),
        tabPanel("Present_11",
                 uiOutput("page24")
        ),
        tabPanel("Present_12",
                 uiOutput("page25")
        ),
        tabPanel("Question_14",
                 uiOutput("page26")
        ),
        tabPanel("Question_15",
                 uiOutput("page27")
        ),
        tabPanel("Question_16",
                 uiOutput("page28")
        ),
        tabPanel("Question_17",
                 uiOutput("page29")
        ),
        tabPanel("Done", 
                 uiOutput("page30")
        )
      )
    )
  ),
  fluidRow(
    column(12,id="plot_reveal",
      plotOutput("render_line",click="plot_click")
    )
  )
)

# Define the server to update data
server <- function(input, output, session) {
  queryList <- reactiveValues()
  queryList$values <- c()
  queryList$index <- 1
  queryList$key_val <- "vec1"
  
  persist_data <- c()
  
  # hide all tabs when first entering page
  hideTab(tabset_id, "Present_0")
  hideTab(tabset_id, "Present_1")
  hideTab(tabset_id, "Present_2")
  hideTab(tabset_id, "Present_3")
  hideTab(tabset_id, "Question_2")
  hideTab(tabset_id, "Question_3")
  hideTab(tabset_id, "Question_4")
  hideTab(tabset_id, "Question_5")
  hideTab(tabset_id, "Present_4")
  hideTab(tabset_id, "Present_5")
  hideTab(tabset_id, "Present_6")
  hideTab(tabset_id, "Question_6")
  hideTab(tabset_id, "Question_7")
  hideTab(tabset_id, "Question_8")
  hideTab(tabset_id, "Question_9")
  hideTab(tabset_id, "Present_7")
  hideTab(tabset_id, "Present_8")
  hideTab(tabset_id, "Present_9")
  hideTab(tabset_id, "Question_10")
  hideTab(tabset_id, "Question_11")
  hideTab(tabset_id, "Question_12")
  hideTab(tabset_id, "Question_13")
  hideTab(tabset_id, "Present_10")
  hideTab(tabset_id, "Present_11")
  hideTab(tabset_id, "Present_12")
  hideTab(tabset_id, "Question_14")
  hideTab(tabset_id, "Question_15")
  hideTab(tabset_id, "Question_16")
  hideTab(tabset_id, "Question_17")
  hideTab(tabset_id, "Done")
  
  # Create clickable display that records input
  output$render_line <- renderPlot({
    interact_line(queryList$values, queryList$key_val)
  })
  observeEvent(input$plot_click, {
    queryList$values[queryList$index] <- input$plot_click$x
  })
  shinyjs::hide(id="plot_reveal")

  # Make UI for page0 "How old are you?"
  output$page0 <- renderUI({
   fluidRow(
     column(8, align="center", offset = 2,
            h3("How old are you?"),
            textInput("ageInput", "", placeholder="Age"),
            actionButton("page0Complete", "Next")
     )
   )
  })
  observeEvent(input$page0Complete, {
    hideTab(tabset_id, "Question_1")
    showTab(tabset_id, "Present_0")
  })
  
  ## 
  # Make UI for page1 "How old are you?"
  output$page1 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
             h3("This is a timeline. It shows when different things happen. 
                The line starts in the past and goes to the future.")),
             actionButton("page1Complete", "Next"),
      renderPlot({
        line()
      }))
  })
  observeEvent(input$page1Complete, {
    hideTab(tabset_id, "Present_0")
    showTab(tabset_id, "Present_1")
  })
  
  # Make UI for yesterday
  output$page2 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
             h3("So it goes from when you were a baby"),
             img(src="baby-emoji.png", width="5%")),actionButton("page2Complete", "Next"),
      renderPlot({
        past_line()
      }))
  })
  observeEvent(input$page2Complete, {
    hideTab(tabset_id, "Present_1")
    showTab(tabset_id, "Present_2")
  })
  
  # Make UI for tomorrow
  output$page3 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
        h3("to when you're going to be a grown up."),
        img(src="person-emoji.png", width="5%")),actionButton("page3Complete", "Next"),
      renderPlot({
        future_line()
      }))
  })
  observeEvent(input$page3Complete, {
    hideTab(tabset_id, "Present_2")
    showTab(tabset_id, "Present_3")
  })
  
  # Make UI for today
  output$page4 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
        h3("And here in the middle is right now."),
        img(src="kid-emoji.png", width="5%")),actionButton("page4Complete", "Next"),
      renderPlot({
        today_line()
      }))
  })
  observeEvent(input$page4Complete, {
    hideTab(tabset_id, "Present_3")
    showTab(tabset_id, "Question_2")
    shinyjs::show(id="plot_reveal")
  })
  
  # Make UI for Question 2
  output$page5 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
        h3("When did you eat breakfast today? Think about when you ate breakfast today. 
           Draw a line for when you ate breakfast today.")),
      actionButton("page5Complete", "Next"))
  })
  observeEvent(input$page5Complete, {
    hideTab(tabset_id, "Question_2")
    showTab(tabset_id, "Question_3")
    queryList$index <- queryList$index + 1
  })
  
  # Make UI for Question 3
  ##AK question: how can we add age+1 to the text?
  output$page6 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
        h3(paste0("When are you going to turn ",as.numeric(input$ageInput) + 1, "? Think about 
        when you are going to turn ", as.numeric(input$ageInput) + 1, ". Draw a line for 
                  when you are going to turn ", as.numeric(input$ageInput) + 1, "."))),
      actionButton("page6Complete", "Next"))
  })
  observeEvent(input$page6Complete, {
    hideTab(tabset_id, "Question_3")
    showTab(tabset_id, "Question_4")
    queryList$index <- queryList$index + 1
  })
  
  # Make UI for Question 4
  output$page7 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
        h3("When are you going to eat dinner today? Think about when you are 
           going to eat dinner today. Draw a line when for you are going to 
           eat dinner today.")),actionButton("page7Complete", "Next"))
  })
  observeEvent(input$page7Complete, {
    hideTab(tabset_id, "Question_4")
    showTab(tabset_id, "Question_5")
    queryList$index <- queryList$index + 1
  })
  
  # Make UI for Question 5
  ##AK question: how can we add age-1 to the text?
  output$page8 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
             h3(paste0("When did you have your birthday and turn ",
             as.numeric(input$ageInput) - 1, "? Think about when you 
             turned ", as.numeric(input$ageInput) - 1, ". Draw a line for when you turned ", 
             as.numeric(input$ageInput) - 1), ".")),
    actionButton("page8Complete", "Next"))
  })
  observeEvent(input$page8Complete, {
    hideTab(tabset_id, "Question_5")
    showTab(tabset_id, "Present_4")
    
    shinyjs::hide(id="plot_reveal")
    
    # save data to persistent store
    persist_data <- c(persist_data, queryList$values)
    
    # Reset clicks
    queryList$values <- c()
    queryList$key_val <- "vec2"
    queryList$index <- 1
  })
  
  ##AK question: how can we add text to empty slide?
  ##text: This line also shows when things happen.
  
  ##AK question: can we hide the previous timeline? right now it shows up below the current one...
  
  # Make UI for yesterday
  output$page9 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
             h3(" Remember, it goes from the time when you were a baby"),
             img(src="baby-emoji.png", width="5%")),actionButton("page9Complete", "Next"),
      renderPlot({
        past_line()
      }))
  })
  observeEvent(input$page9Complete, {
    hideTab(tabset_id, "Present_4")
    showTab(tabset_id, "Present_5")
  })
  
  # Make UI for tomorrow
  output$page10 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
             h3("to the time when you are going to be a grown-up."),
             img(src="person-emoji.png", width="5%")),actionButton("page10Complete", "Next"),
      renderPlot({
        future_line()
      }))
  })
  observeEvent(input$page10Complete, {
    hideTab(tabset_id, "Present_5")
    showTab(tabset_id, "Present_6")
  })
  
  # Make UI for today
  output$page11 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
             h3("And right now goes here."),
             img(src="kid-emoji.png", width="5%")),actionButton("page11Complete", "Next"),
      renderPlot({
        today_line()
      }))
  })
  observeEvent(input$page11Complete, {
    hideTab(tabset_id, "Present_6")
    showTab(tabset_id, "Question_6")
    shinyjs::show(id="plot_reveal")
  })
  
  ##AK question: how can we add an extra line of text: "You’re going to draw lines to show me when some other things happen."
  
  # Make UI for Question 6
  output$page12 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
             h3("Now you’re going to show me where last week goes. Where does last week go? Can you draw a line for last week?")),actionButton("page12Complete", "Next"))
  })
  observeEvent(input$page12Complete, {
    hideTab(tabset_id, "Question_6")
    showTab(tabset_id, "Question_7")
    queryList$index <- queryList$index + 1
  })
  
  # Make UI for Question 7
  output$page13 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
             h3("Now you’re going to show me where one day from now goes. Where does one day from now go? Can you draw a line for tomorrow?")),actionButton("page13Complete", "Next"))
  })
  observeEvent(input$page13Complete, {
    hideTab(tabset_id, "Question_7")
    showTab(tabset_id, "Question_8")
    queryList$index <- queryList$index + 1
  })
  
  # Make UI for Question 8
  output$page14 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
             h3("Now you’re going to show me where tonight goes. Where does tonight go? Can you draw a line for tonight? ")),actionButton("page14Complete", "Next"))
  })
  observeEvent(input$page14Complete, {
    hideTab(tabset_id, "Question_8")
    showTab(tabset_id, "Question_9")
    queryList$index <- queryList$index + 1
  })
  
  # Make UI for Question 9
  output$page15 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
             h3("Now you’re going to show me where two days ago goes. Where does two days ago go? Can you draw a line for two days ago? ")),actionButton("page15Complete", "Next"))
  })
  observeEvent(input$page15Complete, {
    hideTab(tabset_id, "Question_9")
    showTab(tabset_id, "Present_7")
    
    shinyjs::hide(id="plot_reveal")
    
    # save data to persistent store
    persist_data <- c(persist_data, queryList$values)

    # Reset clicks, but store in final_data
    queryList$values <- c()
    queryList$key_val <- "vec3"
    queryList$index <- 1
  })
  
  ##AK question: how can we add text to empty slide
  ##text: This line also shows when things happen.
  
  
  # Make UI for yesterday
  output$page16 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
             h3("Remember, it goes from the time when you were a baby"),
             img(src="baby-emoji.png", width="5%")),actionButton("page16Complete", "Next"),
      renderPlot({
        past_line()
      }))
  })
  observeEvent(input$page16Complete, {
    hideTab(tabset_id, "Present_7")
    showTab(tabset_id, "Present_8")
  })
  
  # Make UI for tomorrow
  output$page17 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
             h3("to the time when you are going to be a grown-up."),
             img(src="person-emoji.png", width="5%")),actionButton("page17Complete", "Next"),
      renderPlot({
        future_line()
      }))
  })
  observeEvent(input$page17Complete, {
    hideTab(tabset_id, "Present_8")
    showTab(tabset_id, "Present_9")
  })
  
  ##AK question: how can we add an extra line of text: "You’re going to draw lines to show me when some other things happen."
  
  
  # Make UI for today
  output$page18 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
             h3("And right now goes here"),
             img(src="kid-emoji.png", width="5%")),actionButton("page18Complete", "Next"),
      renderPlot({
        today_line()
      }))
  })
  observeEvent(input$page18Complete, {
    hideTab(tabset_id, "Present_9")
    showTab(tabset_id, "Question_10")
    shinyjs::show(id="plot_reveal")
  })
  
  ## AK question: how can we add text: You’re going to draw lines to show me when some other things happen. 
  
  # Make UI for Question 10
  output$page19 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
             h3("Now you’re going to show me where next week goes. Where does next week go? Can you draw a line for next week?")),actionButton("page19Complete", "Next"))
  })
  observeEvent(input$page19Complete, {
    hideTab(tabset_id, "Question_10")
    showTab(tabset_id, "Question_11")
    queryList$index <- queryList$index + 1
  })
  
  # Make UI for Question 11
  output$page20 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
             h3("Now you’re going to show me where a day ago goes. Where does a day ago go? Can you draw a line for a day ago?")),actionButton("page20Complete", "Next"))
  })
  observeEvent(input$page20Complete, {
    hideTab(tabset_id, "Question_11")
    showTab(tabset_id, "Question_12")
    queryList$index <- queryList$index + 1
  })
  
  # Make UI for Question 12
  output$page21 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
             h3("Where does two days from now go? Can you draw a line for two days from now?")),actionButton("page21Complete", "Next"))
  })
  observeEvent(input$page21Complete, {
    hideTab(tabset_id, "Question_12")
    showTab(tabset_id, "Question_13")
    queryList$index <- queryList$index + 1
  })
  
  # Make UI for Question 13
  output$page22 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
             h3("Where does last night go? Can you draw a line for last night?")),actionButton("page25Complete", "Next"))
  })
  observeEvent(input$page25Complete, {
    hideTab(tabset_id, "Question_13")
    showTab(tabset_id, "Present_10")
    
    shinyjs::hide(id="plot_reveal")
    
    # save data to persistent store
    persist_data <- c(persist_data, queryList$values)
    
    # Reset clicks, but store in final_data
    queryList$values <- c()
    queryList$key_val <- "vec4"
    queryList$index <- 1
  })
  
  
  ## AK question: how can we add empty line with text
  ## text: This is the last line! Just like the others, it shows when things happen,
  
  # Make UI for yesterday
  output$page23 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
             h3("from when you were a baby"),
             img(src="baby-emoji.png", width="5%")),actionButton("page22Complete", "Next"),
      renderPlot({
        past_line()
      }))
  })
  observeEvent(input$page22Complete, {
    hideTab(tabset_id, "Present_10")
    showTab(tabset_id, "Present_11")
  })
  
  # Make UI for tomorrow
  output$page24 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
             h3("to when you will be a grown-up."),
             img(src="person-emoji.png", width="5%")),actionButton("page23Complete", "Next"),
      renderPlot({
        future_line()
      }))
  })
  observeEvent(input$page23Complete, {
    hideTab(tabset_id, "Present_11")
    showTab(tabset_id, "Present_12")
  })
  
  # Make UI for today
  output$page25 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
             h3("And right now goes here."),
             img(src="kid-emoji.png", width="5%")),actionButton("page24Complete", "Next"),
      renderPlot({
        today_line()
      }))
  })
  observeEvent(input$page24Complete, {
    hideTab(tabset_id, "Present_12")
    showTab(tabset_id, "Question_14")
    shinyjs::show(id="plot_reveal")
  })
  
  ## AK question: how can we add text: You’re going to draw lines to show me when some other things happen. Okay?
  
  # Make UI for Question 14
  output$page26 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
             h3("Now you’re going to show me where tomorrow goes. Where does tomorrow go? Can you draw a line for tomorrow?")),actionButton("page26Complete", "Next"))
  })
  observeEvent(input$page26Complete, {
    hideTab(tabset_id, "Question_14")
    showTab(tabset_id, "Question_15")
    queryList$index <- queryList$index + 1
  })
  
  # Make UI for Question 15
  output$page27 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
             h3("Now you’re going to show me where yesterday goes. Where does yesterday go? Can you draw a line for yesterday? ")),actionButton("page27Complete", "Next"))
  })
  observeEvent(input$page27Complete, {
    hideTab(tabset_id, "Question_15")
    showTab(tabset_id, "Question_16")
    queryList$index <- queryList$index + 1
  })
  
  # Make UI for Question 16
  output$page28 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
             h3("Where does they day after tomorrow go? Can you draw a line for the day after tomorrow?")),actionButton("page28Complete", "Next"))
  })
  observeEvent(input$page28Complete, {
    hideTab(tabset_id, "Question_16")
    showTab(tabset_id, "Question_17")
    queryList$index <- queryList$index + 1
  })
  
  # Make UI for Question 17
  output$page29 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
             h3("Where does the day before yesterday go? Can you draw a line for the day before yesterday? ")),actionButton("page29Complete", "Next"))
  })
  observeEvent(input$page29Complete, {
    hideTab(tabset_id, "Question_17")
    showTab(tabset_id, "Done")
    shinyjs::hide(id="plot_reveal")
  })
  
  # Make UI for page 10
  output$page30 <- renderUI({
    fluidRow(
      fluidRow(
        column(8, align="center", offset = 2,
               h3("Thank you for your time!"),
               img(src = "/thank-you-youre-pawsome.jpg", width = "50%")),
        fluidRow(
          column(8, align="center", offset = 2,
                 actionButton("close", "Close")))
      ))
  })
  observeEvent(input$close, {
    stopApp()
  })
}


# Start the app
shinyApp(ui = ui, server = server)
