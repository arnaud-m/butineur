# http://www.ats.ucla.edu/stat/r/faq/barplotplus.htm

library(shiny)
## library(plotly)

## TODO http://deanattali.com/blog/advanced-shiny-tips/
## http://zevross.com/blog/2016/04/19/r-powered-web-applications-with-shiny-a-tutorial-and-cheat-sheet-with-40-example-apps/
## https://github.com/aoles/shinyURL

MakeAccueilPanel<- function() {
  tabPanel(
    "Accueil",
    includeMarkdown(file.path("markdown", "home.md"))
  )
}

source(file.path("R", "uiMin.R"), local = TRUE)

source(file.path("R", "uiRaw.R"), local = TRUE)


## Define UI
navbarPage(
  "Le butineur (en développement)!",
  theme = "bootstrap.min.css",
  MakeAccueilPanel(),
  tabPanel(
    "Données Ministère 2013",
    tabsetPanel(
      MakeMinAccueilPanel(),
      MakeMinResultatsPanel()
    )
  ),
  tabPanel(
    "Données brutes 2014",
    h4("Appliquer un filtre:"),
    MakeSelectionRow(),
    ## MakeDebugRow(), ##DEBUG
    tabsetPanel(
      MakeRawAccueilPanel(),
      MakeResultatslPanel(),
      MakePopulationPanel(),
      MakeSalairePanel(),
      MakeEmploiPanel(),
      MakeCloudPanel()
    )
  )
)

