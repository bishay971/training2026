# First Input + First Output
# Goal: Add a dropdown. Show selected value as text.
# New:  selectInput()  textOutput()  renderText()
#
# NAMING RULE — write this on the board:
#   inputId  in selectInput  →  input$inputId   in server
#   outputId in textOutput   →  output$outputId in server

library(shiny)
library(pharmaverseadam)
# adsl <- read_sas("adsl.sas7bdat")
ui <- fluidPage(
  titlePanel("My Second App"),

  # TODO 1: add selectInput (dropdown) for treatment arm
  selectInput(
    inputId = "arm", label = "Treatment Arm",
    choices = unique(adsl$ARM),
    selected = ""
  ),

  # TODO 2: add textOutput — outputId must be "selected_arm"
  textOutput(outputId = "selected_arm")
)

server <- function(input, output) {
  # TODO 3: add renderText that reads input$arm
  output$selected_arm <- renderText({
    paste0("You have selected: ", input$arm)
  })
}

shinyApp(ui, server)


# ══════════════════════════════════════════════════════════════════
# TODO GUIDE
# ══════════════════════════════════════════════════════════════════
#
# TODO 1 (line 15): dropdown — add inside fluidPage after titlePanel
#   selectInput(
#     inputId = "arm",
#     label   = "Select Treatment Arm",
#     choices = c("Placebo", "Drug A", "Drug B")
#   )
#
# TODO 2 (line 18): output slot — add after selectInput
#   textOutput("selected_arm")
#
# TODO 3 (line 23): server output — add inside server function
#   output$selected_arm <- renderText({
#     paste("You selected:", input$arm)
#   })
#
# TODO 4 (bonus): add second selectInput for SEX
#   choices = c("M", "F")
#   Update renderText: paste("ARM:", input$arm, "| SEX:", input$sex)
