## options(shiny.reactlog=TRUE)
## Run once when the app is launched
library(shiny)
library(ggplot2)
library(ggthemes)
library(plyr)
library(reshape)
## TODO Use interactive charts ?
## library(plotly)

## Read and source local files
source(file.path("R", "barPlotUtil.R"), local = TRUE)

## data <- read.csv(file.path("data", "all-uns-insertion_professionnelle.csv"), header=TRUE)
data <- readRDS(file.path("data", "all-uns-insertion_professionnelle.rda"))
data$mobiliteEmploi <- data$regionEmploi == "Étranger" | data$regionEmploi == "Hors PACA"

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




MakeChoiceLists<- function(data) {
  su <- function(x) sort(unique(x))
  list(
    annee=su(data$annee),
    grade=su(data$libdip1),
    diplome=list("Domaine"= su(data$libdom), "Mention" = su(data$libdip2), "Spécialité" = su(data$libdip3), "Code SISE" = su(data$code_diplome))
  )
}

choices <- MakeChoiceLists(data)

MakeSelectionOutput <- function(input, output, choices) {
  output$checkboxAnnee <- renderUI( {
    checkboxGroupInput("annee", "Année(s)", choices$annee, choices$annee)
  })
  
  output$checkboxGrade <- renderUI( {
    checkboxGroupInput("grade", "Grade(s)", choices$grade, choices$grade)
  })
    
  output$selectizeDiplome <- renderUI( {
    selectizeInput(
      'diplome', 'Sélectionner un ou plusieurs domaines, mentions, spécialités ou codes SISE : ', choices$diplome, selected = NULL, multiple = TRUE, 
      options = list(
        placeholder = "Taper la sélection ici."
      ), width = "800px"
    )
  })
}

## https://stackoverflow.com/questions/38653903/r-shiny-repetitive-evaluation-of-the-reactive-expression
## https://shiny.rstudio.com/articles/action-buttons.html
MakeReactiveData <- function(input, data, choices) {
  reactive({
    logInd <- rep(TRUE, nrow(data))
    if( length(input$annee) < length(choices$annee) ) {
      ## Sélection active : certains grades ne sont pas sélectionnés.
      logInd <- logInd & (data$annee %in% input$annee)
    } 
    if( length(input$grade) < length(choices$grade) ) {
      ## Sélection active : certains grades ne sont pas sélectionnés.
      logInd <- logInd & (data$libdip1 %in% input$grade)
      }
    if( length(input$sexe) < 2) {
      ## Sélection active : un seul genre est sélectionné.
      logInd <- logInd & (data$sexe %in% input$sexe)
    }
    if(! is.null(input$diplome) ) {
      logInd <- logInd & (data$libdom %in% input$diplome | data$libdip2 %in% input$diplome | data$libdip3 %in% input$diplome | data$code_diplome %in% input$diplome)
    }
    subset(data, logInd)
  })
}


MakeResultatsOutput <- function(output, rdiplomes) {
  output$recapReponse <- renderTable( {
    df <- rdiplomes()
    n <- nrow(df)
    m <- n - sum(df$statutReponse == "Deuxième diplôme\n(double diplôme)", na.rm = TRUE)
    p <- sum(df$repondant)
    data.frame(
      c("Nombre de diplômes", "Nombre de diplômés", "Questionnaires exploités", "Taux de réponse"),
      c(n, m, p, sprintf("%.1f%%", 100*p/m))
    )
  }, colnames = FALSE, striped = TRUE,  spacing = 'l'
  )
  
  output$statutReponse <- renderTable({
    table(rdiplomes()[,"statutReponse"][drop=TRUE], useNA = "ifany")
  }, colnames = FALSE, striped = TRUE,  spacing = 'l')

  output$statutReponsePlot <- renderPlot({
    BarPlotRaw(rdiplomes()$statutReponse) + ggtitle("Statut des réponses") + labs(x="Statut de la réponse", y="Effectifs") 
  })

}

MakePopulationOutput <- function(output, rpopulation) {
  output$populationHeader <- renderText(paste0("Caractéristiques socio-démographiques (", nrow(rpopulation())," diplômés)"))
  population <- reactive({
    as.matrix(ftable(rpopulation()[, c("sexe", "boursier")], exclude = NULL))
  })
  
  SummarySD <- function(x, margin) {
    popcount <- sum(x)
    FuncSD <- function(y) {
      eff <- sum(y)
      return(c("En effectifs"=as.character(eff), "En pourcentages"= sprintf("%.1f",100*eff/popcount)))
    }
    apply(x, margin, FuncSD)
  }
  
  output$populationGenre <- renderTable(SummarySD(population(), 1), rownames = TRUE, spacing = 'l')
  output$populationBourse <- renderTable(SummarySD(population(), 2), rownames = TRUE, spacing = 'l')
}

MakeBaccalaureatOutput <- function(output, rpopulation) {
  output$serieBac <- renderPlot(
    BarPlotRaw(rpopulation()$serieBac) + ggtitle("Bac obtenu") + labs(x="Bac obtenu", y="Effectifs") 
  )

  output$regionBac <- renderPlot(
    BarPlotRaw(rpopulation()$regionBac) + ggtitle("Région d'obtention du bac") + labs(x="Région d'obtention du bac", y="Effectifs") 
  )
}

MakeInsertionOutput <- function(output, rpopulation) {
  output$etudeInsertionTaux <- renderTable({
    x <- rpopulation()
    x <- aggregate( 100*x[,c("poursuiteEtude", "insertionN18","insertionN30")], by = list(x$libdip1), mean, na.rm=TRUE)
    colnames(x) <- c("Grade", "Poursuite d'étude", "Insertion à 18 mois", "Insertion à 30 mois")
    x
  }, digits = 1, spacing = 'l')

  output$insertionMNESR <- renderTable({
    x <- rpopulation()
   ## browser()
    y <- x[,c("poursuiteEtude", "mobiliteEmploi", 
              "insertionN18","insertionN30",
              "emploiStableN18", "emploiStableN30",
              "emploiPleinN18", "emploiPleinN30",
              "emploiSupIntN18", "emploiSupIntN30")]
    y <- 100*sapply(y, mean, na.rm = TRUE)
    df <- data.frame(
      c(
        y["insertionN18"],
        NA,
        y[c("emploiStableN18", "emploiPleinN18", "emploiSupIntN18")],
        NA,
        mean(x$salaireEmploiN18[x$emploiPleinN18], na.rm=TRUE)
      ),
      c(
        y[c("insertionN30", "poursuiteEtude",
            "emploiStableN30", "emploiPleinN30", "emploiSupIntN30",
            "mobiliteEmploi")],
        mean(x$salaireEmploiN30[x$emploiPleinN30], na.rm=TRUE)
      ),
      row.names = c("Taux d'insertion", "Poursuite d'étude", 
                    "Part des emplois stables",
                    "Part des emplois à temps plein",
                    "Part des emplois de niveau cadre ou profession intermédiaire",
                    "Part des emplois situés en dehors de la région de l'établissement (y compris à l'étranger)",
                    "Salaire net mensuel médian des emplois à temps plein (en euros)"
                    )
    )
    colnames(df) <- c("N+18", "N+30")
    df
  }, digits = 1, rownames= TRUE, spacing = 'l', striped = TRUE)

  
  
  output$insertionTaux <- renderPlot({
    x <- rpopulation()
    indicateurs <- c("emploiStable", "emploiPlein", "emploiSupInt")
    labels <- c("emploi stable", "emploi à temps plein", "emploi cadre ou prof. interm.")
    y <- aggregate( 100*x[,c(paste0(indicateurs, "N18"), paste0(indicateurs, "N30"))], by = list(grade = x$libdip1), mean, na.rm=TRUE)
    ## print(y)
    colnames(y) <- c("grade", labels, labels)
    
    z <- rbind(
      cbind(date = "N+18", melt(y[,1:4], "grade")),
      cbind(date = "N+30", melt(y[,c(1,5:7)], "grade"))
    )
    ## print(z)
    ggplot(z, aes(x = date, y = value, fill = variable)) + 
      geom_bar(position = "dodge", stat = "identity") +
      facet_wrap( ~ grade) + 
      theme_gdocs() + scale_fill_ptol() +
      ggtitle("Progression des conditions d'emploi (en %)") +
      labs(x="Situation à N+M mois", y="Taux (%)") +
      theme(legend.position="bottom", legend.direction="horizontal") 
      
  }
  )
}
  
MakeSituationOutput <- function(output, rpopulation) {
  output$situationDiplomeN30 <- renderPlot({
    BarStackedPlotRaw(rpopulation(), "situationProN30", "etudeN30", "Poursuite d'étude") + ggtitle("Situation des diplômés à N + 30 mois") + labs(x="Situation professionnelle", y="Effectifs")
  })
  
  output$situationDiplomeN18 <- renderPlot({
    BarStackedPlotRaw(rpopulation(), "situationProN18", "etudeN18", "Poursuite d'étude") + ggtitle("Situation des diplômés à N + 18 mois") + labs(x="Situation professionnelle", y="Effectifs")
  }
  )
}

MakeEmploiOutput <- function(output, remployes) {
  output$regionEmploi <- renderPlot({
      tauxMobilite <- 100*mean(remployes()$mobiliteEmploi, na.rm = TRUE)
      BarStackedPlotRaw(remployes(), "regionEmploi", "regionBac", "Région d'obtention du bac") +
        labs(x="Région d'emploi", y="Effectifs") +
        ggtitle("Localisation de l'emploi et mobilité des diplomés", subtitle = sprintf("Le taux de mobilité des diplomés est de %.1f%%", tauxMobilite))
    })

    output$niveauEmploi <- renderPlot({
      BarPlotRaw(remployes()$niveauEmploiN30) + ggtitle("Niveau de l'emploi") + labs(x="Niveau de l'emploi", y="Effectifs") 
    })
     
    output$typeEmployeur <- renderPlot({
      BarPlotRaw(remployes()$typeEmployeur) + ggtitle("Type d'employeur") + labs(x="Type d'employeur", y="Effectifs") 
    })

    output$statutEmploi <- renderPlot({
      BarStackedPlotRaw(remployes(), "statutEmploiN30","niveauEmploiN30", "Niveau de l'emploi") + ggtitle("Statut de l'emploi") + labs(x="Statut de l'emploi", y="Effectifs") 
    })
   
    output$activiteEcoEmployeur <- renderPlot({
      BarPlotRaw(remployes()$activiteEcoEmployeur) + ggtitle("Activité économique de l'entreprise") + labs(x="Secteur d'activité", y="Effectifs") 
    })
}

MakeSalaireOutput <- function(output, remployes) {
  remployesTP <- reactive({
    subset(remployes()[,c("salaireEmploiN30", "sexe")], remployes()$emploiPleinN30)
  })

  output$salaireParSexe <- renderTable( {
    SummaryWithNAs <- function(x) {
      s <- summary(x)[c(-1, -6)]
      if(is.na(s["NA's"])) {
        s["NA's"] <- 0
      }
      return(s)
    }
    df <- rbind(
      "Femme/Homme"=SummaryWithNAs(remployesTP()$salaireEmploiN30),
      "Femme"=SummaryWithNAs( subset(remployesTP()$salaireEmploiN30, remployesTP()$sexe == "Femme")),
      "Homme"=SummaryWithNAs( subset(remployesTP()$salaireEmploiN30, remployesTP()$sexe == "Homme"))
    )
    colnames(df) <- c("1er Qu.", "Médiane", "Moyenne", "3ème Qu.", "NA's")
    df
  }, rownames = TRUE, digits = 0, striped = TRUE,  spacing = 'l', width = "80%")
    
  output$salaire <- renderPlot( {
    salary <- remployesTP()$salaireEmploiN30
    salary <- subset(salary, salary < 10000) 
    ggplot() + aes(salary) + geom_histogram(binwidth = 250,  fill = ptol_pal()(1)) + ggtitle("Niveau de rémunération (salaire mensuel net avec primes)") +  theme_gdocs() + labs(x="Salaire", y="Effectifs")
  })
}



MakeCloudOutput <- function(output, remployes) {
  ## Generate job word cloud
  ## http://shiny.rstudio.com/gallery/word-cloud.html
  ## Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  output$nuageEmploi <- renderPlot({
    v <- withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(as.character(remployes()$intituleEmploi))
    })
    wordcloud_rep(names(v), v, scale=c(3.5,0.5),
                  min.freq = 3, max.words=100,
                  colors=brewer.pal(8, "Dark2"))
  })  
}

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

  MakeSelectionOutput(input, output, choices)


  ## #######################
  ## Ensemble des diplômés
  rdiplomes <- MakeReactiveData(input, data, choices)
  
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







