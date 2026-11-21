# Filter a Real Dataset
# Goal: Connect ARM dropdown to ADSL. Show filtered table.
# New:  sidebarLayout()  sliderInput()  tableOutput()  renderTable()
#
# sliderInput two handles:
#   value = c(min, max)     → two handles
#   input$age[1] = lower,   input$age[2] = upper

library(shiny)
library(dplyr)
library(pharmaverseadam)

ui <- fluidPage(

  titlePanel("ADSL Patient Explorer"),

  sidebarLayout(

    sidebarPanel(

      # TODO 1: add selectInput for ARM (read choices from adsl)
        selectInput(
          inputId = "arm",
          label   = "Treatment Arm",
          choices = adsl |> distinct(ARM) |> pull()
        ),

      # TODO 2: add sliderInput for age range
      
        sliderInput(
          inputId = "age",
          label   = "Age Range",
          min     = floor(min(adsl$AGE, na.rm = TRUE)),
          max     = ceiling(max(adsl$AGE, na.rm = TRUE)),
          value   = c(floor(min(adsl$AGE, na.rm = TRUE)),
                      ceiling(max(adsl$AGE, na.rm = TRUE)))
        )

    ),

    mainPanel(

      # TODO 3: add textOutput — outputId = "count"
      textOutput("count"),

      br(),

      # TODO 4: add tableOutput — outputId = "pt_table"
      tableOutput("pt_table")

    )
  )
)

server <- function(input, output) {

  # TODO 5: write output$count and output$pt_table
  #         both filter adsl by ARM and age range
  
  
  
  output$count <- renderText({
        n <- adsl |>
          filter(ARM == input$arm,
                 AGE >= input$age[1],
                 AGE <= input$age[2]) |>
          nrow()
        paste("Patients found:", n)
      })
  
    output$pt_table <- renderTable({
      adsl |>
        filter(ARM == input$arm,
               AGE >= input$age[1],
               AGE <= input$age[2]) |>
        select(USUBJID, AGE, SEX, RACE, COUNTRY)
    })

  

}

shinyApp(ui, server)

# Note: same filter written twice — reactive() fixes this (next exercise)


# ══════════════════════════════════════════════════════════════════
# TODO GUIDE
# ══════════════════════════════════════════════════════════════════
#
# TODO 1 (line 20): ARM dropdown — choices read live from data
#   selectInput(
#     inputId = "arm",
#     label   = "Treatment Arm",
#     choices = adsl |> distinct(ARM) |> pull()
#   )
#
# TODO 2 (line 23): two-handle age slider
#   sliderInput(
#     inputId = "age",
#     label   = "Age Range",
#     min     = floor(min(adsl$AGE, na.rm = TRUE)),
#     max     = ceiling(max(adsl$AGE, na.rm = TRUE)),
#     value   = c(floor(min(adsl$AGE, na.rm = TRUE)),
#                 ceiling(max(adsl$AGE, na.rm = TRUE)))
#   )
#
# TODO 3 (line 30): patient count slot
#   textOutput("count")
#
# TODO 4 (line 34): patient table slot
#   tableOutput("pt_table")
#
# TODO 5 (line 39): both server outputs
#   output$count <- renderText({
#     n <- adsl |>
#       filter(ARM == input$arm,
#              AGE >= input$age[1],
#              AGE <= input$age[2]) |>
#       nrow()
#     paste("Patients found:", n)
#   })
#
#   output$pt_table <- renderTable({
#     adsl |>
#       filter(ARM == input$arm,
#              AGE >= input$age[1],
#              AGE <= input$age[2]) |>
#       select(USUBJID, AGE, SEX, RACE, COUNTRY)
#   })
