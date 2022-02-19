library(shiny)
library(shinyjs)

# Data struct packages
library(broom)
library(collections, warn.conflicts = FALSE)

# time slider
library(leaflet.extras2)

# init global vars
source("data_store.R")
source("plot.R")

# Create the UI to display data
ui <- fluidPage(
  fluidRow(
    column(12,
      # TODO: use lapply to flatten
      tabsetPanel(id=tabset_id, selected="page1",
        tabPanel("Question_1",
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
        tabPanel("Question_6",
                 uiOutput("page9")
        ),
        tabPanel("Done", 
                 uiOutput("page10")
        )
      )
    )
  ),
  fluidRow(
    column(12,
      plotOutput("render_line",click="plot_click")
    )
  )
)

# Define the server to update data
server <- function(input, output, session) {
  # hide all tabs when first entering page
  hideTab(tabset_id, "Present_1")
  hideTab(tabset_id, "Present_2")
  hideTab(tabset_id, "Present_3")
  hideTab(tabset_id, "Question_2")
  hideTab(tabset_id, "Question_3")
  hideTab(tabset_id, "Question_4")
  hideTab(tabset_id, "Question_5")
  hideTab(tabset_id, "Question_6")
  hideTab(tabset_id, "Done")
  
  # Create various display lines
  output$render_line <- renderPlot({
    req(input$page1Complete)
    select_line(yesterday_marker(), tomorrow_marker())
  })

  # Make UI for page1 "How old are you?"
  output$page1 <- renderUI({
   fluidRow(
     column(8, align="center", offset = 2,
            h3("How old are you?"),
            textInput("ageInput", "", placeholder="Age"),
            actionButton("page1Complete", "Next")
     )
   )
  })
  observeEvent(input$page1Complete, {
    # play next pages
    hideTab(tabset_id, "Question_1")
    showTab(tabset_id, "Present_1")
  })
  
  # Make UI for page 2
  output$page2 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
        h3("This is today.")),actionButton("page2Complete", "Next"))
  })
  observeEvent(input$page2Complete, {
    yesterday_marker <<- TRUE
    hideTab(tabset_id, "Present_1")
    showTab(tabset_id, "Present_2")
  })
  
  # Make UI for page 3
  output$page3 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
        h3("This is yesterday.")),actionButton("page3Complete", "Next"))
  })
  observeEvent(input$page3Complete, {
    tomorrow_marker <<- TRUE
    hideTab(tabset_id, "Present_2")
    showTab(tabset_id, "Present_3")
  })
  
  # Make UI for page 4
  output$page4 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
        h3("This is tomorrow.")),actionButton("page4Complete", "Next"))
  })
  observeEvent(input$page4Complete, {
    hideTab(tabset_id, "Present_3")
    showTab(tabset_id, "Question_2")
  })
  
  # Make UI for page 5
  output$page5 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
        h3("Question2")),actionButton("page5Complete", "Next"))
  })
  observeEvent(input$page5Complete, {
    hideTab(tabset_id, "Question_2")
    showTab(tabset_id, "Question_3")
  })
  
  # Make UI for page 6
  output$page6 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
        h3("Question3")),actionButton("page6Complete", "Next"))
  })
  observeEvent(input$page6Complete, {
    hideTab(tabset_id, "Question_3")
    showTab(tabset_id, "Question_4")
  })
  
  # Make UI for page 7
  output$page7 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
        h3("Question4")),actionButton("page7Complete", "Next"))
  })
  observeEvent(input$page7Complete, {
    hideTab(tabset_id, "Question_4")
    showTab(tabset_id, "Question_5")
  })
  
  # Make UI for page 8
  output$page8 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
             h3("Question5")),actionButton("page8Complete", "Next"))
  })
  observeEvent(input$page8Complete, {
    hideTab(tabset_id, "Question_5")
    showTab(tabset_id, "Question_6")
  })
  
  # Make UI for page 9
  output$page9 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
             h3("Question6")),actionButton("page9Complete", "Next"))
  })
  observeEvent(input$page9Complete, {
    hideTab(tabset_id, "Question_6")
    showTab(tabset_id, "Done")
  })
  
  # Make UI for page 10
  output$page10 <- renderUI({
    fluidRow(
      fluidRow(
        column(8, align="center", offset = 2,
               h3("Thank you for your time!"),
               tags$img(src = "/thank-you-youre-pawsome.jpg", width = "50%")),
        fluidRow(
          column(8, align="center", offset = 2,
                 actionButton("close", "Close")))
      ))
  })
  observeEvent(input$close, {
    hideTab(tabset_id, "Done")
  })
}


# Start the app
shinyApp(ui = ui, server = server)
