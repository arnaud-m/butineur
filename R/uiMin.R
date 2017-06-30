
require(shiny)

MakeMinAccueilPanel<- function() {
  tabPanel(
    "Notice",
    includeMarkdown(file.path("markdown", "min", "home.md"))
  )
}

MakeMinDomPanel<- function() {
  tabPanel(
    "Par domaine",
    h2("Caractéristiques socio-démographiques (ensemble des répondants)")
  )
}

MakeMinDiscPanel<- function() {
  tabPanel(
    "Par Discipline",
    h2("Caractéristiques socio-démographiques (ensemble des répondants)")
  )
}
