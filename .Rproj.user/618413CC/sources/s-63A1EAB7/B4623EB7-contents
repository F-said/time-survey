library(shiny)
library(shinyjs)


# Data struct packages
library(broom)
library(collections, warn.conflicts = FALSE)

# time slider
library(leaflet.extras2)

# init global vars
source("constants.R")

# Create the UI to display data
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
      '.irs-from, .irs-to, .irs-min, .irs-max {
            visibility: hidden !important;
      }
      .irs-bar {
            width: 100% !important; 
      }
      irs-bar-edge {
            border-radius: 0px !important;
      }'
      )
    )
  ),
  useShinyjs(),
  # TODO: use lapply to flatten
  tabsetPanel(id=tabset_id, selected="page1",
    tabPanel("Question_1", uiOutput("page1")),
    tabPanel("Question_2", uiOutput("page2")),
    tabPanel("Question_3", uiOutput("page3")),
    tabPanel("Question_4", uiOutput("page4")),
    tabPanel("Question_5", uiOutput("page5")),
    tabPanel("Question_6", uiOutput("page6")),
    tabPanel("Question_7", uiOutput("page7")),
    tabPanel("Done", uiOutput("page8")),
  )
)

# Define the server to update data
server <- function(input, output, session) {
  # hide all tabs when first entering page
  hideTab(tabset_id, "Question_2")
  hideTab(tabset_id, "Question_3")
  hideTab(tabset_id, "Question_4")
  hideTab(tabset_id, "Question_5")
  hideTab(tabset_id, "Question_6")
  hideTab(tabset_id, "Question_7")
  hideTab(tabset_id, "Done")

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
    hideTab(tabset_id, "Question_1")
    showTab(tabset_id, "Question_2")
  })
  
  # Make UI for page 2
  output$page2 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
        h3("Question2"),
        actionButton("toggle.slider2.button", "Toggle Slider"),
        div(id="slider2", 
            sliderInput("integer", "",
                        min = -2, max = 2,
                        value = 0, ticks = FALSE)
        ) %>% shinyjs::hidden(),
        actionButton("page2Complete", "Next")))
  })
  observeEvent(input$page2Complete, {
    hideTab(tabset_id, "Question_2")
    showTab(tabset_id, "Question_3")
  })
  
  # Make UI for page 3
  output$page3 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
        h3("Question3"),
        actionButton("toggle.slider2.button", "Toggle Slider"),
        div(id="slider2", 
            sliderInput("integer", "",
                        min = -2, max = 2,
                        value = 0, ticks = FALSE)
        ) %>% shinyjs::hidden(),
        actionButton("page3Complete", "Next")))
  })
  observeEvent(input$page3Complete, {
    hideTab(tabset_id, "Question_3")
    showTab(tabset_id, "Question_4")
  })
  
  # Make UI for page 4
  output$page4 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
        h3("Question4"),
        actionButton("toggle.slider2.button", "Toggle Slider"),
        div(id="slider2", 
            sliderInput("integer", "",
                        min = -2, max = 2,
                        value = 0, ticks = FALSE)
        ) %>% shinyjs::hidden(),
        actionButton("page4Complete", "Next")))
  })
  observeEvent(input$page4Complete, {
    hideTab(tabset_id, "Question_4")
    showTab(tabset_id, "Question_5")
  })
  
  # Make UI for page 5
  output$page5 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
        h3("Question5"),
        actionButton("toggle.slider2.button", "Toggle Slider"),
        div(id="slider2", 
            sliderInput("integer", "",
                        min = -2, max = 2,
                        value = 0, ticks = FALSE)
        ) %>% shinyjs::hidden(),
        actionButton("page5Complete", "Next")))
  })
  observeEvent(input$page5Complete, {
    hideTab(tabset_id, "Question_5")
    showTab(tabset_id, "Question_6")
  })
  
  # Make UI for page 6
  output$page6 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
        h3("Question6"),
        actionButton("toggle.slider2.button", "Toggle Slider"),
        div(id="slider2", 
            sliderInput("integer", "",
                        min = -2, max = 2,
                        value = 0, ticks = FALSE)
        ) %>% shinyjs::hidden(),
        actionButton("page6Complete", "Next")))
  })
  observeEvent(input$page6Complete, {
    hideTab(tabset_id, "Question_6")
    showTab(tabset_id, "Question_7")
  })
  
  # Make UI for page 7
  output$page7 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
        h3("Question7"),
        actionButton("toggle.slider2.button", "Toggle Slider"),
        div(id="slider2", 
            sliderInput("integer", "",
                        min = -2, max = 2,
                        value = 0, ticks = FALSE)
        ) %>% shinyjs::hidden(),
        actionButton("page7Complete", "Next")))
  })
  observeEvent(input$page7Complete, {
    hideTab(tabset_id, "Question_7")
    showTab(tabset_id, "Done")
  })
  
  # Make UI for page 8, done page
  output$page8 <- renderUI({
    fluidRow(
      column(8, align="center", offset = 2,
        h3("Thank you for your time!"),
        tags$img(src = "/thank-you-youre-pawsome.jpg", width = "50%"),
        ))
  })
  observeEvent(input$close, {
    hideTab(tabset_id, "Done")
  })
  
  # Toggling visibility of sliders on button click.
  observeEvent(input$toggle.slider1.button, {
    shinyjs::toggle("slider1")
  })
  observeEvent(input$toggle.slider2.button, {
    shinyjs::toggle("slider2")
  })
  observeEvent(input$toggle.slider3.button, {
    shinyjs::toggle("slider3")
  })
  
}


# Start the app
shinyApp(ui = ui, server = server)
