##  trace the execution of reactives at runtime
## options(shiny.reactlog=TRUE)
## Run once when the app is launched
library(shiny)
library(ggplot2)
library(ggthemes)
library(plyr)
library(reshape)
## TODO Use interactive charts ?
## library(plotly)

## print all inputs
## print(str(reactiveValuesToList(input)))

##########################
## Source local R files
source(file.path("R", "barPlotUtil.R"), local = TRUE)
source(file.path("R", "srvSelect.R"), local = TRUE)
source(file.path("R", "srvOutput.R"), local = TRUE)

##########################
## Load local data files (rda)
data <- readRDS(file.path("data", "all-uns-insertion_professionnelle.rda"))
dataMinM <-  readRDS(file.path("data", "fr-esr-insertion_professionnelle-master.rda"))
dataMinLP <- readRDS(file.path("data", "fr-esr-insertion_professionnelle-lp.rda"))

########################
## Define server logic 
function(input, output, session) {
  ## Run once each time a user visits the app

  ## ######################
  ## Page: Ministere 

  callModule(
    MinIndicators, "licence", dataMinLP
  )
  
  callModule(
    MinIndicators, "master", dataMinM
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







