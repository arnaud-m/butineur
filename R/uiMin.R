
require(shiny)

MakeMinAccueilPanel<- function() {
  tabPanel(
    title = "Notice",
    value = "minHomePanel",
    includeMarkdown(file.path("markdown", "min", "home.md"))
  )
}



MakeMinLPPanel<- function() {
  tabPanel(
    title = "LP par domaine",
    value = "minLPPanel",
    h3("Caractéristiques socio-démographiques"),
    fluidRow(
      column(4, plotOutput("minDiplomeLP")),
      column(4, plotOutput("minReponsesLP")),
      column(4, plotOutput("minFemmesLP"))
    ),
    h3("Conditions d'emploi"),
    fluidRow(
      column(6, plotOutput("minInsertionLP")),
      column(6, plotOutput("minEmploiLP")),
      column(6, plotOutput("minSalaireLP"))
    )
  )
}


MakeMinMasterPanel<- function() {
  tabPanel(
    title = "Master par domaine",
    value = "minMPanel",
    h2("Foo")
  )
}


MakeMinResultatsPanel<- function() {
  tabPanel(
    title = "Par discipline(s)",
    value = "minDiscPanel",
    h2("TODO")
    ## fluidRow(
    ##   column(6, plotOutput("diplomeMin")),
    ##   column(6, plotOutput("insertionMin"))
    ## )
  )
}

