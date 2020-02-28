require(shiny)

MakeSelectionRow <- function() {
  ## The creation of a checkbox avoids restoration issue when restoring a bookmark
  ## Indeed, the call to renderUI is delayed until the UI get the focus. It can happen long after restoration.
  fluidRow(
    column(3, checkboxGroupInput("annee", label = "Années(s)", choices = NULL, selected = NULL, inline = TRUE)),
    column(1, checkboxGroupInput("grade", label = "Grade(s)", choices = NULL, selected = NULL)),
    column(6, selectizeInput(
                'diplome',
                label = 'Sélectionner un ou plusieurs domaines, mentions, spécialités ou codes SISE : ', 
                choices = NULL, selected = NULL, multiple = TRUE, 
                options = list(
                  placeholder = "Taper la sélection ici."
                ), width = "800px")
           ),
    column(1, checkboxGroupInput("sexe", label = "Sexe(s)", choices = c("Femme", "Homme"), selected = c("Femme", "Homme")))
  )
}

MakeRawAccueilPanel<- function() {
  tabPanel(
    title = "Notice",
    value = "rawHomePanel",
    includeMarkdown(file.path("markdown", "raw", "home.md"))
  )
}


MakeResultatslPanel<- function() {
  tabPanel(
    title = "Résultats",
    value = "rawResultPanel",
    fluidRow(
      h2("Résultats de l'enquête"),
      column(
        6,
        column(6, h4("Taux de réponse"), tableOutput("recapReponse")),
        column(6, h4("Statut des réponses"),tableOutput("statutReponse")),
        column(12, includeMarkdown(file.path("markdown", "raw", "resultats.md")))        
        ),
      column(6, plotOutput("statutReponsePlot"))
    )
  )
}


MakePopulationPanel<- function() {
  tabPanel(
    title = "Population",
    value = "rawPopPanel",
    h2(textOutput("populationHeader")),
    fluidRow(
      column(6, h3("Genre"), tableOutput("populationGenre")),
      column(6, h3("Bourse"),tableOutput("populationBourse"))
    ),
    h3("Baccalauréat"),
    fluidRow(
      column(6, plotOutput("serieBac")),
      column(6, plotOutput("regionBac"))
    )
  )
}

MakeInsertionPanel<- function() {
  tabPanel(
    title = "Insertion",
    value = "rawInsPanel",
    h2(textOutput("insertionHeader")),
    h3("Évolution de l'insertion des diplômé.e.s à 18 et 30 mois"),
    fluidRow(
      column(
        6,
        includeMarkdown(file.path("markdown", "raw", "insertion.md")),
        tableOutput("insertionMNESR")
      ),
      column(
        6,
        tableOutput("etudeInsertionTaux"),
        plotOutput("insertionTaux")
      )
    ),
    h3("Évolution de la situation des diplômé.e.s à 18 et 30 mois (en %)"),
    fluidRow(
      column(6, plotOutput("situationDiplomeN18")),
      column(6, plotOutput("situationDiplomeN30"))
    )
  )
}


MakeEmploiPanel<- function() {
  tabPanel(
    title = "Emploi",
    value = "rawJobPanel",
    h2(textOutput("emploiHeader")),
    fluidRow(
      column(6, plotOutput("niveauEmploi"), includeMarkdown(file.path("markdown", "raw", "niveauEmploi.md"))),
      column(6, plotOutput("typeEmployeur"), includeMarkdown(file.path("markdown", "raw", "typeEmployeur.md"))),
      column(6, plotOutput("statutEmploi")),
      column(6, plotOutput("regionEmploi")),
      column(6, plotOutput("activiteEcoEmployeur"))
    ))
}

MakeSalairePanel<- function() {
  tabPanel(
    title = "Salaire",
    value = "rawSalPanel",
    h2(textOutput("salaireHeader")),
    fluidRow(
      column(
        6,
        includeMarkdown(file.path("markdown", "raw", "salaire.md")),
        tableOutput("salaireParSexe")
      ),
      column(6, plotOutput("salaire"))
    )
  )
}


MakeCloudPanel<- function() {
  tabPanel(
    title = "Nuage d'emplois",
    value = "rawCloudPanel",
    h2(textOutput("cloudHeader")),
    plotOutput("nuageEmploi")
  )
}
