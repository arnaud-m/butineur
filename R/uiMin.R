
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
    "Par discipline",
    h2("TODO")
    ## fluidRow(
    ##   column(6, plotOutput("diplomeMin")),
    ##   column(6, plotOutput("insertionMin"))
    ## )
  )
}

