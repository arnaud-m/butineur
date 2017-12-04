## options(shiny.reactlog=TRUE)
## Run once when the app is launched
library(shiny)
library(ggplot2)
library(ggthemes)
library(plyr)
library(reshape)
## TODO Use interactive charts ?
## library(plotly)

##########################
## Source local R files
source(file.path("R", "barPlotUtil.R"), local = TRUE)
source(file.path("R", "srvSelect.R"), local = TRUE)
source(file.path("R", "srvOutput.R"), local = TRUE)

##########################
## Load local data files

## Raw data
data <- readRDS(file.path("data", "all-uns-insertion_professionnelle.rda"))
data$mobiliteEmploi <- data$regionEmploi == "Étranger" | data$regionEmploi == "Hors PACA"


## Min. data
ReadMinIP <- function(file) {
  df <- read.table(file = file, header=TRUE, row.names=NULL, sep=';', quote="", na.strings=c("", NA, "ns", "nd"))
  df$Nombre.de.diplômés <- round(df$Nombre.de.réponses * 100 /  df$Taux.de.réponse)
  return(df)
}

FilterDomains <- function(dataMin) {
  df <- aggregate(dataMin$Domaine, by = list(code = dataMin$Code.du.domaine), length)
  domains <- subset(df$code, df$x == 2)
  subset(dataMin, grepl("^Ensemble ", dataMin$Discipline) | dataMin$Code.du.domaine %in% domains)
}

FilterDisciplines <- function(dataMin) {
  subset(dataMin, !grepl("^Ensemble ", dataMin$Discipline))
}


dataMinM <- ReadMinIP(file = file.path("data", "fr-esr-insertion_professionnelle-master.csv"))
dataMinDomM <- FilterDomains(dataMinM)
## TODO dataMinDiscM <- FilterDisciplines(dataMinM)

dataMinLP <- ReadMinIP(file = file.path("data", "fr-esr-insertion_professionnelle-lp.csv"))
dataMinDomLP <- FilterDomains(dataMinLP)
## TODO dataMinDiscLP <- FilterDisciplines(dataMinLP)



########################
## Define server logic 
function(input, output, session) {
  ## Run once each time a user visits the app

  ## ######################
  ## Page: Ministere 
  
  callModule(
    MinIndicators, "licence", dataMinDomLP
  )
  
  callModule(
    MinIndicators, "master", dataMinDomM
  )

  ## ######################
  ## Page: Brut 

  rdiplomes <- MakeReactiveDiplomes(session, input, output, data)
  
  ## #######################
  ## Ensemble des diplômés

  MakeResultatsOutput(output, rdiplomes)
  
  rpopulation <- reactive({
    x <- rdiplomes()
    subset(x, is.na(x$statutReponse) | x$statutReponse!= "Deuxième diplôme\n(double diplôme)")
  })
  
  MakePopulationOutput(output, rpopulation)
  MakeBaccalaureatOutput(output, rpopulation)

  ## #######################
  ## Ensemble des répondants
  rrepondants <- reactive({
    x <- rpopulation()
    subset(x, x$repondant)
  })

  output$insertionHeader <- renderText(paste0("Insertion professionnelle des diplômés (", nrow(rrepondants())," répondants)"))
  MakeInsertionOutput(output, rrepondants)
  MakeSituationOutput(output, rrepondants)

  ## #####################
  ## Diplômés en emploi
  remployes <- reactive({
    x <- rrepondants()
    subset(x, x$insertionN30)
  })

  output$emploiHeader <- renderText(paste0("Caractéristiques des emplois (", nrow(remployes())," diplomés en emploi)"))
  MakeEmploiOutput(output, remployes)

  output$cloudHeader <- renderText(paste0("Nuage d'emplois (", nrow(remployes())," diplomés en emploi)"))
  MakeCloudOutput(output, remployes)

  ## ###############################
  ## Diplômés en emploi à temps plein
  output$salaireHeader <- renderText(paste0("Distribution des salaires (", sum(remployes()$emploiPleinN30, na.rm=TRUE)," diplomés en emploi à temps plein)"))
  MakeSalaireOutput(output, remployes)
  
  
  
  ## #####################
  ## DEBUG Set active panel
  ## updateNavbarPage(session, "navPage", selected = "minTabPanel")
  ## updateTabsetPanel(session, "minTabSetPanel", selected = "minLPPanel")
  
  ## updateNavbarPage(session, "navPage", selected = "rawTabPanel")
  ## updateTabsetPanel(session, "rawTabSetPanel", selected = "rawInsPanel")
  
  ## #########################################################
  ## Automatically stop a Shiny app when closing the browser tab
  ## session$onSessionEnded(stopApp)
  
  
  }







