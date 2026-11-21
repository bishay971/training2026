# reactive() — Write the Filter Once
# Goal: Understand reactive(). Eliminate repeated filter code.
# New:  reactive({ })
#
# reactive({ }) = named block that:
#   - runs when any input inside changes
#   - returns a value (dataset, number, anything)
#   - shared by all outputs — computed only once
#
# CRITICAL: always call result with ()
#   filtered()   ← correct (returns the data frame)
#   filtered     ← wrong  (returns the reactive object itself)

library(shiny)
library(dplyr)
library(pharmaverseadam)

ui <- fluidPage(

  titlePanel("ADSL Explorer — reactive()"),

  sidebarLayout(
    sidebarPanel(
      selectInput("arm", "Treatment Arm",
                  choices = adsl |> distinct(ARM) |> pull()),
      sliderInput("age", "Age Range",
                  min   = floor(min(adsl$AGE, na.rm = TRUE)),
                  max   = ceiling(max(adsl$AGE, na.rm = TRUE)),
                  value = c(floor(min(adsl$AGE, na.rm = TRUE)),
                             ceiling(max(adsl$AGE, na.rm = TRUE))))
    ),
    mainPanel(
      textOutput("count"),
      br(),
      textOutput("avg_age"),
      br(),
      tableOutput("pt_table")
    )
  )
)

server <- function(input, output) {

  # TODO 1: create reactive() named "filtered"
  #         filter adsl by input$arm and input$age[1] / input$age[2]
  
    filtered <- reactive({
      adsl |>
        filter(ARM == input$arm,
               AGE >= input$age[1],
               AGE <= input$age[2])
    })


    
  output$count <- renderText({
    # TODO 2: paste("Patients:", nrow(filtered()))
    
    paste("Patients:", nrow(filtered()))
  })

  output$avg_age <- renderText({
    # TODO 3: paste("Average age:", round(mean(filtered()$AGE, na.rm = TRUE), 1))
    
    paste("Average age:", round(mean(filtered()$AGE, na.rm = TRUE), 1))
  })

  output$pt_table <- renderTable({
    # TODO 4: filtered() |> select(USUBJID, AGE, SEX, RACE, COUNTRY)
    filtered() |> select(USUBJID, AGE, SEX, RACE, COUNTRY)
  })

  # TODO 5 (bonus): 4th output — count of unique countries
  # Also add textOutput("countries") in mainPanel above

}

shinyApp(ui, server)


# ══════════════════════════════════════════════════════════════════
# TODO GUIDE
# ══════════════════════════════════════════════════════════════════
#
# TODO 1 (line 41): reactive block — define filtered()
#   filtered <- reactive({
#     adsl |>
#       filter(ARM == input$arm,
#              AGE >= input$age[1],
#              AGE <= input$age[2])
#   })
#
# TODO 2 (line 44): replace comment with:
#   paste("Patients:", nrow(filtered()))
#
# TODO 3 (line 48): replace comment with:
#   paste("Average age:", round(mean(filtered()$AGE, na.rm = TRUE), 1))
#
# TODO 4 (line 52): replace comment with:
#   filtered() |> select(USUBJID, AGE, SEX, RACE, COUNTRY)
#
# TODO 5 (bonus): countries output — add inside server after pt_table
#   output$countries <- renderText({
#     n <- filtered() |> distinct(COUNTRY) |> nrow()
#     paste("Countries represented:", n)
#   })
#   Add textOutput("countries") in mainPanel too
