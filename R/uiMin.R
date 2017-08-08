require(shiny)

MakeMinAccueilPanel<- function() {
  tabPanel(
    title = "Notice",
    value = "minHomePanel",
    includeMarkdown(file.path("markdown", "min", "home.md"))
  )
}

