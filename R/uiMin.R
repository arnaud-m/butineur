
require(shiny)

MakeMinAccueilPanel<- function() {
  tabPanel(
    title = "Notice",
    value = "minHomePanel",
    includeMarkdown(file.path("markdown", "min", "home.md"))
  )
}

MakeMinSelectionRow <- function() {
  fluidRow(
    column(8,includeMarkdown(file.path("markdown", "min", "resultats.md"))),
    column(2, uiOutput("checkboxGradeMin")),
    column(2, radioButtons("isMinPerDomain", label = "Regroupement",
    choices = list("Domaine" = TRUE, "Discipline" = FALSE), 
    selected = TRUE))
  )
}

MakeMinDebugRow <- function() {
  fluidRow(
    column(3, verbatimTextOutput("gradeMin")),
    column(5, verbatimTextOutput("isMinPerDomain"))
  )
}

MakeMinLPPanel<- function() {
  tabPanel(
    "LP par domaine",
    h2("Foo")
  )
}


MakeMinMasterPanel<- function() {
  tabPanel(
    "Master par domaine",
    h2("Foo")
  )
}


MakeMinResultatsPanel<- function() {
  tabPanel(
    "Résultats",
    MakeMinSelectionRow(),
    MakeMinDebugRow(),
    h2("Caractéristiques socio-démographiques (ensemble des répondants)"),
    fluidRow(
      column(6, plotOutput("diplomeMin")),
      column(6, plotOutput("insertionMin"))
    )
  )
}

