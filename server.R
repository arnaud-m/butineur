## options(shiny.reactlog=TRUE)

library(shiny)
library(ggplot2)
library(ggthemes)
library(plyr)
library(reshape)
## TODO Use interactive charts ?
## library(plotly)

## Run once when the app is launched

data <- read.csv(file.path("data", "all-uns-insertion_professionnelle.csv"), header=TRUE)

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
dataMinDiscM <- FilterDisciplines(dataMinM)

dataMinLP <- ReadMinIP(file = file.path("data", "fr-esr-insertion_professionnelle-lp.csv"))
dataMinDomLP <- FilterDomains(dataMinLP)
dataMinDiscLP <- FilterDisciplines(dataMinLP)

## Compute percentage labels of bar plots
GetPercentLabels <- function(x, threshold = 1, digits = 1) {
  ind <- x >= threshold
  r <- rep("", length(x))
  r[ind] <- sprintf(paste0("%.", digits, "f%%"), x[ind])
  return(r)
}

## http://stackoverflow.com/questions/21236229/stacked-bar-chart
## http://rstudio-pubs-static.s3.amazonaws.com/4305_8df3611f69fa48c2ba6bbca9a8367895.html
## http://www.sthda.com/french/wiki/ggplot2-barplots-guide-de-demarrage-rapide-logiciel-r-et-visualisation-de-donnees
BarPlotRaw <- function(x, threshold = 1, digits = 0) {
  x <- as.data.frame(table(x[drop=TRUE], useNA = "ifany"))
  label <- GetPercentLabels(100*x$Freq/sum(x$Freq), threshold = threshold, digits = digits)
  pos <- x$Freq / 2
  ggplot(x, aes(x = Var1, y = Freq)) + geom_bar(stat="identity", position="dodge", fill = ptol_pal()(1)) +
    geom_text(aes(y = pos, label=label), color = "white", size=8, fontface = 2) +
    coord_flip() + theme_gdocs() 
}

BarStackedPlotRaw <- function(df, aesX, aesF, legend.title = NULL, labelX = TRUE, labelF = TRUE) {
  x <- as.data.frame(ftable(df[ , c(aesX, aesF), drop=TRUE]))
  totFreq <- sum(x$Freq)
  ## Percentage labels
  x$percentage <- 100 * x$Freq / totFreq
  x$percentage <-  GetPercentLabels(x$percentage, threshold = 2, digits = 0)
  x <- ddply(x, aesX, transform, pos = sum(Freq)-cumsum(Freq) + (0.5 * Freq), top = cumsum(Freq))
  x$toplab <-  GetPercentLabels(100 * x$top / totFreq, threshold = 1, digits = 0)
  m <- length(unique(x[,aesF]))
  ## exploit recycling
  x$toplab[ append(rep(TRUE,m-1), FALSE) ] <- "" 
  ## print(x)
  p <- ggplot(x, aes_string(x = aesX, y = "Freq", fill = aesF)) + geom_bar(stat="identity") + coord_flip() + theme_gdocs()
  
  if(labelF) {
    ## FIXME Stopped working when changing the fluid page into a navbar page
    ## commit cba64eb use a navbar page instead of a fluid page
    p <- p + geom_text(aes(y = x$pos, label = x$percentage, size = 6), show.legend = FALSE)
  }
  
  if(labelX) {
    p <- p + geom_text(aes(y = x$top, label = x$toplab, size = 8, hjust = -0.25, vjust = -0.5, fontface = 2), show.legend = FALSE) + expand_limits( y = c(0,round(max(x$top)*1.1)))
  }
  
  if(is.null(legend.title)) {
    p <- p + scale_fill_ptol()
  } else {
    p <- p + scale_fill_ptol(name=legend.title) 
  }
  p <- p + theme(legend.position="bottom", legend.direction="horizontal")
  return(p)
}


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


MakeResultatsOutput <- function(output, rpopulation) {
  output$recapReponse <- renderTable( {
    df <- rpopulation()
    n <- nrow(df)
    q <- sum(df$repondant)
    data.frame(
      c("Nombre de diplômés", "Questionnaires exploités", "Taux de réponse"),
      c(n, q, sprintf("%.1f%%", 100*q/n))
    )
  }, colnames = FALSE
  )
  
  output$statutReponse <- renderTable({
    table(rpopulation()[,"statutReponse"][drop=TRUE], useNA = "ifany")
  }, colnames = FALSE)

  output$statutReponsePlot <- renderPlot({
    BarPlotRaw(rpopulation()$statutReponse) + ggtitle("Statut des réponses") + labs(x="Statut de la réponse", y="Effectifs") 
  })

}

MakeSituationOutput <- function(output, rpopulation) {
  output$situationDiplomeN30 <- renderPlot({
    ## if(nrow(rdata()) > 0) {     ## no on the first tab anymore
    ## Avoid to run Before loading of UI and to cause Null parameters
    ## no on the first tab anymore
    BarStackedPlotRaw(rpopulation(), "situationProN30", "etudeN30", "Poursuite d'étude") + ggtitle("Situation des diplômés à N + 30 mois") + labs(x="Situation professionnelle", y="Effectifs")
    ## }
  })
  
  output$situationDiplomeN18 <- renderPlot({
    ## if(nrow(rdata()) > 0) {     ## no on the first tab anymore
    ## Avoid to run Before loading of UI and to cause Null parameters
    BarStackedPlotRaw(rpopulation(), "situationProN18", "etudeN18", "Poursuite d'étude") + ggtitle("Situation des diplômés à N + 18 mois") + labs(x="Situation professionnelle", y="Effectifs")
  }
  )
}

MakeInsertionOutput <- function(output, rpopulation) {
  output$etudeInsertionTaux <- renderTable({
    ##if(nrow(rdata()) > 0) {     ## no on the first tab anymore
    x <- rpopulation()
    x <- subset (x, x$repondant)
    x <- aggregate( 100*x[,c("poursuiteEtude", "insertionN18","insertionN30")], by = list(x$libdip1), mean, na.rm=TRUE)
    colnames(x) <- c("Grade", "Poursuite d'étude", "IP à 18 mois", "IP\nà 30 mois")
    x
    ## }
  }, digits = 1)
  
  output$insertionTaux <- renderPlot({
    x <- rpopulation()
    indicateurs <- c("insertion", "emploiStable", "emploiSupInt")
    labels <- c("insertion", "emploi stable", "emploi cadre ou prof. interm.")
    y <- aggregate( 100*x[,c(paste0(indicateurs, "N18"), paste0(indicateurs, "N30"))], by = list(grade = x$libdip1), mean, na.rm=TRUE)
    ## print(y)
    ## colnames(y) <- gsub("(N18|N30)", "", colnames(y))
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
      labs(x="Situation à N+M mois", y="Taux (%)") +
      theme(legend.position="bottom", legend.direction="horizontal") 
  }
  )
}

MakePopulationOutput <- function(output, rpopulation) {
  population <- reactive({
    as.matrix(ftable(rpopulation()[, c("sexe", "boursier")], exclude = NULL))
  })
  
  SummarySD <- function(x, margin) {
    popcount <- sum(x)
    FuncSD <- function(y) {
      eff <- sum(y)
      return(c("En effectifs"=eff, "En pourcentages"= 100*eff/popcount))
    }
    apply(x, margin, FuncSD)
  }
  
  output$populationGenre <- renderTable(SummarySD(population(), 1), rownames = TRUE, digits = 1)
  output$populationBourse <- renderTable(SummarySD(population(), 2), rownames = TRUE, digits = 1)
}

MakeBaccalaureatOutput <- function(output, rpopulation) {
  output$serieBac <- renderPlot(
    BarPlotRaw(rpopulation()$serieBac) + ggtitle("Bac obtenu") + labs(x="Bac obtenu", y="Effectifs") 
  )

  output$regionBac <- renderPlot(
    BarPlotRaw(rpopulation()$regionBac) + ggtitle("Région d'obtention du bac") + labs(x="Région d'obtention du bac", y="Effectifs") 
  )
}

MakeSalaireOutput <- function(output, remploye) {
  remployeTP <- reactive({
    subset(remploye()[,c("salaireEmploiN30", "sexe")], remploye()$tempsPleinN30)
  })

  output$salaireParSexe <- renderTable( {
    SummaryWithNAs <- function(x) {
      s <- summary(x)[c(-1, -6)]
      if(is.na(s["NA's"])) {
        s["NA's"] <- 0
      }
      return(s)
    }
    ## FIXME problem with Nas in only one gender !
    rbind(
      "Femme/Homme"=SummaryWithNAs(remployeTP()$salaireEmploiN30),
      "Femme"=SummaryWithNAs( subset(remployeTP()$salaireEmploiN30, remployeTP()$sexe == "Femme")),
      "Homme"=SummaryWithNAs( subset(remployeTP()$salaireEmploiN30, remployeTP()$sexe == "Homme"))
      )
  }, rownames = TRUE, digits = 0)
    
  output$salaire <- renderPlot( {
    salary <- remployeTP()$salaireEmploiN30
    salary <- subset(salary, salary < 10000) 
    ggplot() + aes(salary) + geom_histogram(binwidth = 250,  fill = ptol_pal()(1)) + ggtitle("Niveau de rémunération (salaire mensuel net hors primes)") +  theme_gdocs() + labs(x="Salaire", y="Effectifs")
  })
}

MakeEmploiOutput <- function(output, remploye) {
    output$regionEmploi <- renderPlot({
      BarStackedPlotRaw(remploye(), "regionEmploi", "regionBac", "Région d'obtention du bac") +
        labs(x="Région d'emploi", y="Effectifs") +
        ggtitle("Localisation de l'emploi et mobilité des diplomés")
    })

    output$niveauEmploi <- renderPlot({
      BarPlotRaw(remploye()$niveauEmploiN30) + ggtitle("Niveau de l'emploi") + labs(x="Niveau de l'emploi", y="Effectifs") 
    })
     
    output$typeEmployeur <- renderPlot({
      BarPlotRaw(remploye()$typeEmployeur) + ggtitle("Type d'employeur") + labs(x="Type d'employeur", y="Effectifs") 
    })
    ## output$typeEmployeur <- renderPlot({
    ##   BarStackedPlotRaw(remploye(), "typeEmployeur", "effectifsEmployeur", legend.title = "Effectifs de l'employeur") +
    ##     labs(x="Type d'employeur", y="Effectifs") +
    ##     ggtitle("Type d'employeur")
    ## })

    output$statutEmploi <- renderPlot({
      BarStackedPlotRaw(remploye(), "statutEmploiN30","niveauEmploiN30", "Niveau de l'emploi") + ggtitle("Statut de l'emploi") + labs(x="Statut de l'emploi", y="Effectifs") 
    })
   
    output$activiteEcoEmployeur <- renderPlot({
      BarPlotRaw(remploye()$activiteEcoEmployeur) + ggtitle("Activité économique de l'entreprise") + labs(x="Secteur d'activité", y="Effectifs") 
    })
}

MakeCloudOutput <- function(output, rrepondants) {
  ## Generate job word cloud
  ## http://shiny.rstudio.com/gallery/word-cloud.html
  ## Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
    
  output$nuageEmploi <- renderPlot({
    v <- withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(as.character(rrepondants()$intituleEmploi))
    })
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = 2, max.words=100,
                  colors=brewer.pal(8, "Dark2"))
  })  
}

########################
## Define server logic 
function(input, output, session) {
  ## Run once each time a user visits the app
    
  callModule(
    MinIndicators, "licence", dataMinDomLP
  )
  
  callModule(
    MinIndicators, "master", dataMinDomM
  )
  
  MakeSelectionOutput(input, output, choices)
  rpopulation <- MakeReactiveData(input, data, choices)
  MakeResultatsOutput(output, rpopulation)
  
  rrepondants <- reactive({
    x <- rpopulation()
    subset(x, x$repondant)
  })
  
  MakeInsertionOutput(output, rrepondants)
  MakeSituationOutput(output, rrepondants)
  
  remploye <- reactive({
    x <- rrepondants()
    subset(x, x$employe)
  })
  
  MakeCloudOutput(output, rrepondants)
  
  ## ###############################################################
  ## Caractéristiques socio-démographiques (ensemble des diplômés)
  
  MakePopulationOutput(output, rpopulation)
  MakeBaccalaureatOutput(output, rpopulation)
  
  ## #####################
  ## Diplômés en emploi
  
  MakeSalaireOutput(output, remploye)
  MakeEmploiOutput(output, remploye)
  
  output$nbEmployes <- renderText(paste("Il y a", nrow(remploye()), "répondants en emploi"))
  output$nbSalaries <- renderText(paste("Il y a", sum(remploye()$tempsPleinN30, na.rm=TRUE), "répondants en emploi à temps plein"))
  
  ## #####################
  ## DEBUG Set active panel
  ## updateNavbarPage(session, "navPage", selected = "minTabPanel")
  ## updateTabsetPanel(session, "minTabSetPanel", selected = "minLPPanel")
  
  ## updateNavbarPage(session, "navPage", selected = "rawTabPanel")
  ## updateTabsetPanel(session, "rawTabSetPanel", selected = "rawSalPanel")
  
  ## #########################################################
  ## Automatically stop a Shiny app when closing the browser tab
  ## session$onSessionEnded(stopApp)
  
  
  }







