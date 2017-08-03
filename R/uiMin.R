require(shiny)

MakeMinAccueilPanel<- function() {
  tabPanel(
    title = "Notice",
    value = "minHomePanel",
    includeMarkdown(file.path("markdown", "min", "home.md"))
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

