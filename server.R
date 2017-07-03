## options(shiny.reactlog=TRUE)

###########################
## New MNESR Data analysis
## x <- read.table(file = 'fr-esr-insertion_professionnelle-master.csv', header=TRUE, row.names=NULL, sep=';', quote="", na.strings=c("", NA, "ns", "nd"))
## col.names <- c("domaine", "discipline", "situation", "emplois_cadre_ou_professions_intermediaires", "emplois_stables", "emplois_a_temps_plein")
## col.times <- tail(col.names, -3)
## row.domains <- !grepl("^Ensemble ", x$discipline)
## y <- x[row.domains,col.names]
## y <- subset(y, apply(y[,col.times], 1, function(x) !all(is.na(x))))
## z <- reshape(y, idvar=head(col.names, 3), varying=list(col.times), direction="long", v.names="value", timevar="characteristic", times=col.times)
## library(ggplot2)
## ggplot(z, aes(x = situation, y = value, fill = characteristic)) + 
##       geom_bar(position = "dodge", stat = "identity") +
##   facet_wrap( ~ discipline)

## x$nombre_de_diplômés <- x$nombre_de_reponses * 100 /  x$taux_de_reponse
## u <- x[row.domains,]
## u <- subset(u, u$situation == "30 mois après le diplôme")
## ggplot(u, aes(x = "", y = nombre_de_diplômés, fill = domaine)) + 
##       geom_bar(stat = "identity") +  coord_polar("y", start=0) 

## u <- x[row.domains,]
## u <- subset(u, u$situation == "30 mois après le diplôme")
## ggplot(u, aes(x = "", y = nombre_de_reponses, fill = discipline)) + 
##       geom_bar(stat = "identity") +  coord_polar("y", start=0) 

## u <- x[row.domains,]
## u <- subset(u, !is.na(u$taux_dinsertion))
## ggplot(u, aes(x = domaine, y = taux_dinsertion, fill = situation)) + 
##   geom_bar(position = "dodge", stat = "identity")

## u <- x[row.domains,]
## u <- subset(u, !is.na(u$salaire_net_median_des_emplois_a_temps_plein))
## ggplot(u, aes(x = domaine, y = salaire_net_median_des_emplois_a_temps_plein, fill = situation)) +
##   geom_bar(position = "dodge", stat = "identity")
###########################

library(shiny)
library(ggplot2)
library(ggthemes)
library(plyr)
## TODO Use interactive charts ?
## library(plotly)

## n = 100000
## m = 50
## x <- matrix(runif(n = n*m), nrow=n)
## k <- 5
## thresh <- runif(k) 
## sel1 <- function(x, thresh) {
##   y <- x
##   for(i in 1:k) {
##     y <- subset(x, x[,i] <= thresh[i] )
##   }
##   y
## }

## sel2 <- function(x, thresh) {
##   subset(x, all(x[,1:k] <= thresh) )
## }

## system.time(sel1(x, thresh))
## system.time(sel2(x, thresh))
## system.time(replicate(k, sel2(x, thresh)))

## Run once when the app is launched

## Load database IP
#source(file.path("R", "ReadIP.R'"), local = TRUE)
#data <- ReadIP(file.path("data", "raw_data.csv"))
#write.csv(data, file.path("data", "int-uns-insertion_professionnelle-master.csv"), row.names=FALSE)
data <- read.csv(file.path("data", "all-uns-insertion_professionnelle-master.csv"), header=TRUE)

dataMin <- read.table(file = file.path("data", "fr-esr-insertion_professionnelle.csv"), header=TRUE, row.names=NULL, sep=';', quote="", na.strings=c("", NA, "ns", "nd"))
dataMin$Nombre.de.diplômés <- round(dataMin$Nombre.de.réponses * 100 /  dataMin$Taux.de.réponse)

filterSituation <-function(dataMin, after=30) {
  subset(
    dataMin,
    dataMin$situation == paste0(after, " mois après le diplôme")
  )
}

GetIndicateurs <- function(data) {
  population <- nrow(data)
  indics <- c("population"= nrow(data))
}

## Rename sum function for addmargins
Total <- function(x) sum(x)

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
BarPlot <- function(x, threshold = 5, digits = 0) {
  x <- as.data.frame(table(x[drop=TRUE], useNA = "ifany"))
  x$label <- GetPercentLabels(100*x$Freq/sum(x$Freq), threshold, digits)
  pos <- x$Freq / 2
  ggplot(x, aes(x = Var1, y = Freq)) + geom_bar(stat="identity", position="dodge", fill = ptol_pal()(1)) + coord_flip() + geom_text(aes(y = pos, label=label), color = "white", size=6) +  theme_gdocs() 
}

BarStackedPlot2 <- function(df, aesX, aesF, legend.title = NULL, labelX = TRUE, labelF = TRUE) {
  x <- as.data.frame(ftable(df[ , c(aesX, aesF), drop=TRUE]))
  totFreq <- sum(x$Freq)
  ## Percentage labels
  ## TODO bug with the percentage labels
  x$percentage <- 100 * x$Freq / totFreq
  x$percentage <-  GetPercentLabels(x$percentage, threshold = 2, digits = 0)
  x <- ddply(x, aesX, transform, pos = sum(Freq)-cumsum(Freq) + (0.5 * Freq), top = cumsum(Freq))
  x$toplab <-  GetPercentLabels(100 * x$top / totFreq, threshold = 0)
  m <- length(unique(x[,aesF]))
  ## exploit recycling
  x$toplab[ append(rep(TRUE,m-1), FALSE) ] <- "" 

  p <- ggplot(x, aes_string(x = aesX, y = "Freq", fill = aesF)) + geom_bar(stat="identity") + coord_flip() + theme_gdocs()
  
  if(labelF) {
    p <- p + geom_text(aes(y = pos, label = x$percentage, size = 6, position = "stack"), show.legend = FALSE)
  }
  
  if(labelX) {
    p <- p + geom_text(aes(y = x$top, label = x$toplab, size = 6, hjust = -0.25, vjust = -0.5, position = "stack", fontface = 2), show.legend = FALSE) + expand_limits( y = c(0,round(max(x$top)*1.05)))
  }
  
  if(is.null(legend.title)) {
    p <- p + scale_fill_ptol()
  } else {
    p <- p + scale_fill_ptol(name=legend.title) 
  }
  p <- p + theme(legend.position="bottom", legend.direction="horizontal")
  return(p)
}



## TODO Add Domain/Discipline
## Droit-Economie-Gestion (DEG) 
## Lettres-Langues-Arts (LLA)
## Sciences Humaines et sociales (SHS) 
## Sciences - Technologies-Santé (STS)
MakeChoiceLists<- function(data, dataMin) {
  su <- function(x) sort(unique(x)) 
  list(
    annee=su(data$annee),
    grade=su(data$libdip1),
    diplome=list("Mention" = su(data$libdip2), "Spécialité" = su(data$libdip3), "Code SISE" = su(data$code_diplome)),
    gradeMin=su(dataMin$Diplôme)
  )
}

choices <- MakeChoiceLists(data, dataMin)

## TODO ? https://rstudio.github.io/shinythemes/

MakeSelectionOutput <- function(input, output, choices) {
  output$checkboxAnnee <- renderUI( {
    checkboxGroupInput("annee", "Année(s)", choices$annee, choices$annee)
  })
  
  output$checkboxGrade <- renderUI( {
    checkboxGroupInput("grade", "Grade(s)", choices$grade, choices$grade)
  })
    
  output$selectizeDiplome <- renderUI( {
    selectizeInput(
      'diplome', 'Sélectionner une ou plusieurs mentions, spécialités ou codes SISE : ', choices$diplome, multiple = TRUE, 
      options = list(
        placeholder = "Taper la sélection ici.",
        onInitialize = I('function() { this.setValue(""); }')
      ), width = "800px"
    )
  })
}

MakeMinSelectionOutput <- function(input, output, choices) {
  output$checkboxGradeMin <- renderUI( {
    checkboxGroupInput("gradeMin", "Grade(s)", choices$gradeMin, choices$gradeMin)
  })
}

MakeMinDebugOutput <- function(input, output, choices) {
  output$gradeMin <- renderPrint(input$gradeMin)
  output$isMinPerDomain <- renderPrint(input$isMinPerDomain)
}


MakeDebugOutput <- function(input, output, choices) {
  output$annee <- renderPrint(input$annee)
  output$grade <- renderPrint(input$grade)
  output$diplome <- renderPrint(input$diplome)
  output$sexe <- renderPrint(input$sexe)
}



  ## https://stackoverflow.com/questions/38653903/r-shiny-repetitive-evaluation-of-the-reactive-expression
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
      logInd <- logInd & (data$libdip2 %in% input$diplome | data$libdip3 %in% input$diplome | data$code_diplome %in% input$diplome)
    }
    subset(data, logInd)
  })
}

MakeMinReactiveData <- function(input, dataMin, choices) {
  reactive({
    logInd <- rep(TRUE, nrow(dataMin))
    if( length(input$gradeMin) < length(choices$gradeMin) ) {
      ## Sélection active : certains grades ne sont pas sélectionnés.
      logInd <- logInd & (dataMin$Diplôme %in% input$gradeMin)
    }
    if(input$isMinPerDomain) {
      logInd <- logInd & ( grepl("^Ensemble ", dataMin$Discipline) | dataMin$Domaine %in% c("Masters enseignement", "Lettres, langues, arts" ))
    } else {
      logInd <- logInd & !grepl("^Ensemble ", dataMin$Discipline)
    }
    subset(dataMin, logInd)
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
    ##table(rdata()[,"statutReponse"][drop=TRUE], useNA = "ifany", dnn= "Réponse")
    table(rpopulation()[,"statutReponse"][drop=TRUE], useNA = "ifany")
  }, colnames = FALSE)
}

MakeSituationOutput <- function(output, rpopulation) {
  output$situationDiplomeN30 <- renderPlot({
    ## if(nrow(rdata()) > 0) {     ## no on the first tab anymore
    ## Avoid to run Before loading of UI and to cause Null parameters
    ## no on the first tab anymore
    BarStackedPlot2(rpopulation(), "situationProN30", "etudeN30", "Poursuite d'étude") + ggtitle("Situation des diplômés à N + 30 mois") + labs(x="Situation professionnelle", y="Effectifs")
    ## }
  })
  
  output$situationDiplomeN18 <- renderPlot({
    ## if(nrow(rdata()) > 0) {     ## no on the first tab anymore
    ## Avoid to run Before loading of UI and to cause Null parameters
    BarStackedPlot2(rpopulation(), "situationProN18", "etudeN18", "Poursuite d'étude") + ggtitle("Situation des diplômés à N + 18 mois") + labs(x="Situation professionnelle", y="Effectifs")
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
    ## df=data.frame(
    ##   year=rep(c("2010","2011"),each=4),
    ##   treatment=rep(c("Impact","Control")),
    ##   type=rep(c("Phylum1","Phylum2"),each=2),
    ##   total=sample(1:100,8))
    ## ggplot(df, aes(x = year, y = total, fill = type)) +
    ##     geom_bar(position = "stack", stat = "identity") +
    ##   facet_wrap( ~ treatment)
    x <- rpopulation()
    y <- aggregate( 100*x[,c("insertionN18","insertionN30", "emploiStableN18", "emploiStableN30", "emploiSupIntN18", "emploiSupIntN30")], by = list(x$libdip1), mean, na.rm=TRUE)
    ## TODO Use melt instead
    y <- data.frame(rep(c("Insertion", "Stabilité", "Cadre ou Interm."), each = 4), rep(c("N+18", "N+30"),each=4), y$Group.1, as.numeric(c(as.matrix(y)[,-1])))
    colnames(y) <- c("Indicateur", "Grade", "NM", "Taux")
    
    ggplot(y, aes(x = Grade, y = Taux, fill = Indicateur)) + 
      geom_bar(position = "dodge", stat = "identity") +
      facet_wrap( ~ NM)
  }
  )
}

MakePopulationOutput <- function(output, rpopulation) {
  
}

MakeBaccalaureatOutput <- function(output, rpopulation) {
  output$serieBac2 <- renderPlot(
    BarStackedPlot2(rpopulation(), "serieBac", "regionBac", "Région d'obtention du bac") + ggtitle("Bac obtenu") + labs(x="Bac obtenu", y="Effectifs")
  )
  output$serieBac <- renderPlot(
    BarPlot(rpopulation()$serieBac) + ggtitle("Bac obtenu") + labs(x="Bac obtenu", y="Effectifs") 
  )

  output$regionBac <- renderPlot(
    BarPlot(rpopulation()$regionBac) + ggtitle("Région d'obtention du bac") + labs(x="Région d'obtention du bac", y="Effectifs") 
  )
}

MakeSalaireOutput <- function(output, remploye) {
  output$salaireParSexe <- renderTable( {
    x <- subset(remploye()[,c("salaireEmploiN30", "sexe")], remploye()$tempsPleinN30)
    x <- as.matrix(aggregate(x$salaireEmploiN30, list(x$sexe), summary))
    colnames(x) <- c( "Sexe", substring(colnames(x)[-1], 3))
    x
  })
    
  output$salaire <- renderPlot( {
    salary <- subset(remploye()$salaireEmploiN30, remploye()$tempsPleinN30)
    salary <- subset(salary, salary < 10000) 
    ggplot() + aes(salary) + geom_histogram(binwidth = 250,  fill = ptol_pal()(1)) + ggtitle("Niveau de rémunération (salaire mensuel net hors primes)") +  theme_gdocs() + labs(x="Salaire", y="Effectifs")
  })
}

MakeEmploiOutput <- function(output, remploye) {
    output$regionEmploi <- renderPlot({
      BarStackedPlot2(remploye(), "regionEmploi", "regionBac", "Région d'obtention du bac") +
        labs(x="Région d'emploi", y="Effectifs") +
        ggtitle("Localisation de l'emploi et mobilité des diplomés")
    })

    
    output$niveauEmploi2 <- renderPlot({
      BarStackedPlot2(remploye(), "statutEmploiN30","niveauEmploiN30", "Niveau de l'emploi") + ggtitle("Statut de l'emploi") + labs(x="Niveau de l'emploi", y="Effectifs") 
    })
    output$statutEmploi <- renderPlot({
      BarPlot(remploye()$statutEmploiN30) + ggtitle("Statut de l'emploi") + labs(x="Statut de l'emploi", y="Effectifs") 
    })
    
    output$niveauEmploi <- renderPlot({
      BarPlot(remploye()$niveauEmploiN30) + ggtitle("Niveau de l'emploi") + labs(x="Niveau de l'emploi", y="Effectifs") 
    })

    output$typeEmployeur <- renderPlot({
      BarPlot(remploye()$typeEmployeur) + ggtitle("Type d'employeur") + labs(x="Type d'employeur", y="Effectifs") 
    })
    ## output$typeEmployeur <- renderPlot({
    ##   BarStackedPlot2(remploye(), "typeEmployeur", "effectifsEmployeur", legend.title = "Effectifs de l'employeur") +
    ##     labs(x="Type d'employeur", y="Effectifs") +
    ##     ggtitle("Type d'employeur")
    ## })
    output$activiteEcoEmployeur <- renderPlot({
      BarPlot(remploye()$activiteEcoEmployeur) + ggtitle("Activité économique de l'entreprise") + labs(x="Secteur d'activité", y="Effectifs") 
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

shinyServer(
  ## Define server logic 
  function(input, output, session) {
    ## Run once each time a user visits the app

    MakeMinSelectionOutput(input, output, choices)
    MakeMinDebugOutput(input, output) ##DEBUG
    rpopulationMin <- MakeMinReactiveData(input, dataMin, choices)

    output$diplomeMin <- renderPlot({
      x <- rpopulationMin()
      x <- filterSituation(x)
      if(input$isMinPerDomain) {
        p <- ggplot(x, aes(x = "", y = Nombre.de.diplômés, fill = Domaine))
      } else {
        p <- ggplot(x, aes(x = "", y = Nombre.de.diplômés, fill = Discipline))
      }
      p + geom_bar(stat = "identity") +  coord_polar("y", start=0) + scale_fill_ptol() +  theme_gdocs() + ggtitle("Nombre de diplômés") +
        theme(axis.line=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
          
    })

    output$insertionMin <- renderPlot({
      x <- rpopulationMin()
      x <- subset(x, !is.na(x$Taux.d.insertion))
      ggplot(x, aes(x = Domaine, y = Taux.d.insertion, fill = situation)) + geom_bar(position = "dodge", stat = "identity") +
        theme_gdocs() + scale_fill_ptol() +
        theme(legend.position="bottom", legend.direction="horizontal")
      })


    ## ggplot(y, aes(x = Discipline, y = Taux.de.réponse)) + geom_bar(stat="identity", position="dodge", fill = ptol_pal()(1)) + coord_flip() +  geom_text(aes(y = y$Taux.de.réponse/2, label=y$Taux.de.réponse), color = "white", size=10) +  theme_gdocs()


    ## y <- subset(y, !is.na(y$Salaire.net.médian.des.emplois.à.temps.plein))
## >   ggplot(y, aes(x = Discipline, y = Salaire.net.médian.des.emplois.à.temps.plein)) + geom_bar(stat="identity", position="dodge", fill = ptol_pal()(1)) + coord_flip() +  theme_gdocs()
    
    
    MakeSelectionOutput(input, output, choices)
    ## MakeDebugOutput(input, output) ##DEBUG
    rpopulation <- MakeReactiveData(input, data, choices)
        
    ## TODO rpopulation -> rrepondant

      
    MakeResultatsOutput(output, rpopulation)
    MakeInsertionOutput(output, rpopulation)
    MakeSituationOutput(output, rpopulation)

    rrepondants <- reactive({
      x <- rpopulation()
      subset(x, x$repondant)
    })
    
    remploye <- reactive({
      x <- rrepondants()
      subset(x, x$employe)
    })
    
    MakeCloudOutput(output, rrepondants)
    
    
    ## ###############################################################
    ## Caractéristiques socio-démographiques (ensemble des diplômés)

    population <- reactive({
      as.matrix(ftable(rpopulation()[, c("sexe", "boursier")], exclude = NULL))
    })
    
    output$populationEffectifs <- renderTable(addmargins(population(), FUN = Total, quiet = TRUE), rownames = TRUE, digits = 0)
    output$populationPourcents <- renderTable({
      x <- population()
      x <- 100*x /sum(x)
      addmargins(x, FUN = Total, quiet = TRUE)
    }, rownames = TRUE, digits = 1)
    
    MakeBaccalaureatOutput(output, rpopulation)

    

    ## #####################
    ## Diplômés en emploi
    
    MakeSalaireOutput(output, remploye)
    MakeEmploiOutput(output, remploye)

    output$nbEmployes <- renderText(paste("Il y a", nrow(remploye()), "répondants en emploi"))
    output$nbSalaries <- renderText(paste("Il y a", sum(remploye()$tempsPleinN30, na.rm=TRUE), "répondants en emploi à temps plein"))
    

    ## #########################################################
    ## Automatically stop a Shiny app when closing the browser tab
    ## session$onSessionEnded(stopApp)
    
    ## #########################################
    ## pass parameters to a shiny app via URL
    ## http://stackoverflow.com/questions/32872222/how-do-you-pass-parameters-to-a-shiny-app-via-url
    updateFromURL <- function(session, query, nameval, uifunc = updateCheckboxGroupInput) {
      valuetoupdate <- query[[nameval]]
      if (!is.null(query[[nameval]])) {
        valuetoupdate <- unlist(strsplit(valuetoupdate, ","))
        uifunc(session, nameval, selected = valuetoupdate)
      }
    }
    observe({
      query <- parseQueryString(session$clientData$url_search)
      updateFromURL(session, query, "annee")
      updateFromURL(session, query, "grade")
      updateFromURL(session, query, "diplome", updateSelectizeInput)
      updateFromURL(session, query, "sexe")
      ## TODO Go to the right place into the UI 
    })

    sprintQuery <- function(nameval, x, maxlen) {
      if( !is.null(x) && length(x) < maxlen) return(paste0(nameval,"=", paste0(x, collapse = ",")))
      else return(NULL)
    }
    
    ## generate the URL for the current selection
    url <- reactive({
      ##url <- "http://127.0.0.1:3141/?"
      query <- c(
        sprintQuery("annee", input$annee, length(choices$annee)),
        sprintQuery("grade", input$grade, length(choices$grade)),
        sprintQuery("diplome", input$diplome, Inf),
        sprintQuery("sexe", input$sexe, 2)
      )
      url <- "http://unicepro-ove.shinyapps.io/oveshinyip/"
      if(length(query) > 0) {
        url <- paste0(url, "?", paste0(query, collapse="&"))
      }
      url
    })
    
    output$url <- renderText(url())
    ## A reactive data source
    observeEvent(input$copyButton, {
      clipr::write_clip(url())
    })
  }
)







