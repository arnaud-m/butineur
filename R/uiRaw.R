require(shiny)

MakeSelectionRow <- function() {
  fluidRow(
    column(1, uiOutput("checkboxAnnee")),
    column(1, uiOutput("checkboxGrade")),
    column(6, uiOutput("selectizeDiplome")),
    column(1, checkboxGroupInput("sexe", "Sexe(s)", c("Femme", "Homme"), c("Femme", "Homme")))
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
        plotOutput("statutReponsePlot")
      ),
      column(6, includeMarkdown(file.path("markdown", "raw", "resultats.md")))
    )
  )
}


MakePopulationPanel<- function() {
  tabPanel(
    title = "Population",
    value = "rawPopPanel",
    h2("Caractéristiques socio-démographiques (ensemble des diplômés)"),
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
    h2("Insertion professionnelle des diplômés"),
    fluidRow(
      column(
        6,
        h3("Taux de poursuite d’études et évolution du taux d’insertion (en %)"),
        tableOutput("etudeInsertionTaux")
      ),
      column(
        6,
        h3("Progression des conditions d’emploi (en %)"),
        plotOutput("insertionTaux")
      )
    ),
    h2("Évolution de la situation des diplomé.e.s à 18 et 30 mois (en %)"),
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
    h2("Diplômés en emploi"),
    fluidRow(
      column(6, textOutput("nbEmployes"))
    ),
    fluidRow(
      column(6, includeMarkdown(file.path("markdown", "raw", "emploi.md"))),
      column(6, plotOutput("regionEmploi"))
    ),
    fluidRow(
      column(6,plotOutput("niveauEmploi")),
      column(6,plotOutput("typeEmployeur"))
    ),
    fluidRow(
      column(6,plotOutput("statutEmploi")),
      column(6,plotOutput("activiteEcoEmployeur"))
    ))
}

MakeSalairePanel<- function() {
  tabPanel(
    title = "Salaire",
    value = "rawSalPanel",
    h2("Diplômés en emploi à temps plein"),
    fluidRow(
      column(
        6,
        textOutput("nbSalaries"),
        includeMarkdown(file.path("markdown", "raw", "salaire.md")),
        column(6, tableOutput("salaireParSexe"))
      ),
      fluidRow(
      column(6, plotOutput("salaire"))
    )
    )
  )
}


MakeCloudPanel<- function() {
  tabPanel(
    title = "Nuage d'emplois",
    value = "rawCloudPanel",
    plotOutput("nuageEmploi")
  )
}
