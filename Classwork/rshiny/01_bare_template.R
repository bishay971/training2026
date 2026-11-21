# The Bare Template
# Goal: Run the smallest valid Shiny app. Understand structure.
# Run: Click "Run App" in RStudio
#
# ui     = what the user SEES   (layout, controls, output slots)
# server = what the app DOES    (reads inputs, creates outputs)

library(shiny)

ui <- fluidPage(

  # TODO 1: add titlePanel here
  titlePanel("My First Shiny App")

)

server <- function(input, output) {

}

shinyApp(ui = ui, server = server)


# ══════════════════════════════════════════════════════════════════
# TODO GUIDE
# ══════════════════════════════════════════════════════════════════
#
# TODO 1 (line 12): add a page title inside fluidPage
#   titlePanel("My First Shiny App")
#
# Expected: white page with "My First Shiny App" as heading.
# That is a complete, working Shiny app.
