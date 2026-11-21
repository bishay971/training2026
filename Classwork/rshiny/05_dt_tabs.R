# DT Tables + Multi-Tab Layout
# Goal: Replace basic table with interactive DT. Add tabs.
# New:  tabsetPanel()  tabPanel()  DTOutput()  renderDT()  checkboxGroupInput()
#
# checkboxGroupInput: input$sex returns a vector — c("M","F") or c("M")
#   filter with:  filter(SEX %in% input$sex)
#
# datatable(df, filter = "top") → adds column-level search boxes

library(shiny)
library(dplyr)
library(DT)
library(pharmaverseadam)

ui <- fluidPage(
  titlePanel("Clinical Explorer — Tabs + DT"),
  sidebarLayout(
    sidebarPanel(
      selectInput("arm", "Treatment Arm",
        choices  = c("All", adsl |> distinct(ARM) |> pull()),
        selected = "All"
      ),

      # TODO 1: add checkboxGroupInput for SEX
      #         choices = c("M", "F"),  selected = c("M", "F")


      checkboxGroupInput("sex", "Sex", choices = c("M", "F"), selected = c("M", "F"))
    ),
    mainPanel(
      # TODO 2: wrap outputs in tabsetPanel
      #         Tab 1: "Demographics" with DTOutput("dm_tbl")
      #         Tab 2: "Adverse Events" with DTOutput("ae_tbl")
      tabsetPanel(
        tabPanel(
          "Demographics",
          DTOutput("dm_tbl")
        ),
        tabPanel(
          "Adverse Events",
          DTOutput("ae_tbl")
        )
      )
    )
  )
)

server <- function(input, output) {
  adsl_f <- reactive({
    df <- adsl
    if (input$arm != "All") df <- df |> filter(ARM == input$arm)
    df |> filter(SEX %in% input$sex)
  })

  adae_f <- reactive({
    adae |> filter(USUBJID %in% adsl_f()$USUBJID)
  })

  # TODO 3: renderDT for demographics — output id: dm_tbl
  #         columns: USUBJID, ARM, AGE, SEX, RACE, COUNTRY
  
  output$dm_tbl <- renderDT(
    adsl_f() |> select(USUBJID, ARM, AGE, SEX, RACE, COUNTRY) |> 
      datatable(filter = "top", options = list(pageLength = 20, scrollX = TRUE))
  )

  # TODO 4: renderDT for adverse events — output id: ae_tbl
  #         columns: any_of(c("USUBJID","ARM","AEDECOD","AEBODSYS","AESEV"))
  
  output$ae_tbl <- renderDT(
   adae_f() |> select(any_of(c("USUBJID","ARM","AEDECOD","AEBODSYS","AESEV"))) |> 
     datatable(filter = "top", options = list(pageLength = 20, scrollX = TRUE))
  )
  
    
}

shinyApp(ui, server)

# Filter ARM in sidebar → AE tab also updates (adae_f uses adsl_f()$USUBJID)


# ══════════════════════════════════════════════════════════════════
# TODO GUIDE
# ══════════════════════════════════════════════════════════════════
#
# TODO 1 (line 25): sex checkbox — add inside sidebarPanel after selectInput
#   checkboxGroupInput(
#     inputId  = "sex",
#     label    = "Sex",
#     choices  = c("M", "F"),
#     selected = c("M", "F")
#   )
#
# TODO 2 (line 31): tabbed layout — add inside mainPanel
#   tabsetPanel(
#     tabPanel("Demographics",
#       br(),
#       DTOutput("dm_tbl")
#     ),
#     tabPanel("Adverse Events",
#       br(),
#       DTOutput("ae_tbl")
#     )
#   )
#
# TODO 3 (line 47): demographics DT output
#   output$dm_tbl <- renderDT({
#     adsl_f() |>
#       select(USUBJID, ARM, AGE, SEX, RACE, COUNTRY) |>
#       datatable(filter = "top", options = list(pageLength = 10, scrollX = TRUE))
#   })
#
# TODO 4 (line 51): adverse events DT output
#   output$ae_tbl <- renderDT({
#     adae_f() |>
#       select(any_of(c("USUBJID", "ARM", "AEDECOD", "AEBODSYS", "AESEV"))) |>
#       datatable(filter = "top", options = list(pageLength = 10, scrollX = TRUE))
#   })
