require(shiny)

MakeSelectionRow <- function() {
  fluidRow( 
    column(1, uiOutput("checkboxAnnee")),
    column(1, uiOutput("checkboxGrade")),
    column(4, uiOutput("selectizeDiplome")),
    column(1, checkboxGroupInput("sexe", "Sexe(s)", c("Femme", "Homme"), c("Femme", "Homme"))),
    column(2, actionButton("copyButton", "Copier l'URL dans le presse-papier."), align="center")
    ##style = "background-color:#f5f5f5; border:1px solid #e3e3e3; box-shadow:0 1px 1px rgba(0, 0, 0, 0.05) inset"
   )
}

MakeDebugRow <- function() {
  fluidRow(
    column(1, verbatimTextOutput("annee")),
    column(2, verbatimTextOutput("grade")),
    column(4, verbatimTextOutput("diplome")),
    column(2, verbatimTextOutput("sexe")),
    column(3, verbatimTextOutput("url"), align="left")
  )
}

MakeRawAccueilPanel<- function() {
  tabPanel(
    "Notice",
    includeMarkdown(file.path("markdown", "raw", "home.md"))
  )
}


MakeResultatslPanel<- function() {
  tabPanel(
    "Résultats",
    fluidRow(
      column(
        6,
        h3("Résultats de l'enquête"),
        column(6, h4("Taux de réponse"), tableOutput("recapReponse")),
        column(6, h4("Statut des réponses"),tableOutput("statutReponse")),
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


MakePopulationPanel<- function() {
  tabPanel(
    "Population",
    h2("Caractéristiques socio-démographiques (ensemble des diplômés)"),
    fluidRow(
      column(6, h3("Tableau des effectifs"), tableOutput("populationEffectifs")),
      column(6, h3("En pourcentage"),tableOutput("populationPourcents"))
    ),
    h3("Baccalauréat"),
    fluidRow(
      column(6, plotOutput("serieBac")),
      column(6, plotOutput("regionBac"))
    )
  )
}


MakeSalairePanel<- function() {
  tabPanel(
    "Salaire",
    h2("Diplômés en emploi"),
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


MakeEmploiPanel<- function() {
  tabPanel(
    "Emploi",
    h2("Diplômés en emploi"),
    fluidRow(
      column(6, textOutput("nbEmployes"))
    ),
    fluidRow(
      column(6, plotOutput("regionEmploi"))
    ),
    tags$p("Emploi stable : contrat en CDI, fonctionnaire ou travailleur indépendant."),
    fluidRow(
      column(6,plotOutput("statutEmploi")),
      column(6,plotOutput("niveauEmploi"))
    ),
    fluidRow(
      column(8,plotOutput("niveauEmploi2"))
    ),
    fluidRow(
      column(6,plotOutput("typeEmployeur")),
      column(6,plotOutput("activiteEcoEmployeur"))
    ))
}

MakeCloudPanel<- function() {
  tabPanel("Nuage d'emplois", plotOutput("nuageEmploi"))
}
