# http://www.ats.ucla.edu/stat/r/faq/barplotplus.htm

library(shiny)
## library(plotly)

## http://deanattali.com/blog/advanced-shiny-tips/
## http://zevross.com/blog/2016/04/19/r-powered-web-applications-with-shiny-a-tutorial-and-cheat-sheet-with-40-example-apps/

MakeAccueilPanel<- function() {
  tabPanel(
    title = "Accueil", value="homePanel",
    includeMarkdown(file.path("markdown", "home.md"))
  )
}

source(file.path("R", "uiMin.R"), local = TRUE)

source(file.path("R", "uiRaw.R"), local = TRUE)

## ###########################
## Action button in the navbar
## https://github.com/daattali/advanced-shiny/blob/master/navbar-add-text/app.R

## Call this function with an input (such as `textInput("text", NULL, "Search")`) if you
## want to add an input to the navbar
navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-form navbar-right", inputs)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form)
  navbar
}

## ################
## Define UI
function(request) {
  navbarPageWithInputs(
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
      MinIndicatorsUI("master", title = "Master par domaine", value = "minMPanel")
    )
  ),
  tabPanel(
    title = "Données brutes 2014",
    value = "rawTabPanel",
    MakeSelectionRow(),
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
  footer = tags$footer(HTML("&copy; 2017 Université Nice Sophia Antipolis.")),
  inputs = bookmarkButton(class = "btn-primary")
)
}
