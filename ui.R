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
fluidPage(
  theme = "bootstrap.min.css",
  titlePanel("Le butineur de l'OVE (en développement)"),
  fluidRow(
    tabsetPanel(
      MakeAccueilPanel(),
      tabPanel(
        "Données Ministère 2013",
        tabsetPanel(
          MakeMinAccueilPanel(),
          MakeMinDomPanel(),
          MakeMinDiscPanel()
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
  ),
  h2("The End !")
)

