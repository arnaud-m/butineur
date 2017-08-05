# http://www.ats.ucla.edu/stat/r/faq/barplotplus.htm

library(shiny)
## library(plotly)

## TODO http://deanattali.com/blog/advanced-shiny-tips/
## http://zevross.com/blog/2016/04/19/r-powered-web-applications-with-shiny-a-tutorial-and-cheat-sheet-with-40-example-apps/
## https://github.com/aoles/shinyURL

MakeAccueilPanel<- function() {
  tabPanel(
    title = "Accueil", value="homePanel",
    includeMarkdown(file.path("markdown", "home.md"))
  )
}

source(file.path("R", "uiMin.R"), local = TRUE)

source(file.path("R", "uiRaw.R"), local = TRUE)


## Define UI
navbarPage(
  title = "Le butineur (en développement)!",
  id = "navPage",
  theme = "butineur.css",
  MakeAccueilPanel(),
  tabPanel(
    title = "Données Ministère 2013",
    value = "minTabPanel",
    tabsetPanel(
      id = "minTabSetPanel",
      MakeMinAccueilPanel(),
      MinIndicatorsUI("licence", title = "Licence Pro par domaine", value = "minLPPanel"),
      MinIndicatorsUI("master", title = "Master par domaine", value = "minMPanel"),
      MakeMinResultatsPanel()
    )
  ),
  tabPanel(
    title = "Données brutes 2014",
    value = "rawTabPanel",
    MakeSelectionRow(),
    ## MakeDebugRow(), ##DEBUG
    tabsetPanel(
      id = "rawTabSetPanel",
      MakeRawAccueilPanel(),
      MakeResultatslPanel(),
      MakePopulationPanel(),
      MakeEmploiPanel(),
      MakeSalairePanel(),
      MakeCloudPanel()
    )
  ),
  footer = tags$footer(HTML("&copy; 2017 Université Nice Sophia Antipolis."))
)

