## options(shiny.reactlog=TRUE)

library(shiny)
library(ggplot2)
library(ggthemes)
library(plyr)
## TODO Use interactive charts ?
## library(plotly)

## Run once when the app is launched

## Load database IP
#source(file.path("R", "ReadIP.R'"), local = TRUE)
#data <- ReadIP(file.path("data", "raw_data.csv"))
#write.csv(data, file.path("data", "int-uns-insertion_professionnelle-master.csv"), row.names=FALSE)
data <- read.csv(file.path("data", "all-uns-insertion_professionnelle-master.csv"), header=TRUE)


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


FilterSituation <-function(dataMin, after=30) {
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
BarPlotRaw <- function(x, threshold = 5, digits = 0) {
  x <- as.data.frame(table(x[drop=TRUE], useNA = "ifany"))
  label <- GetPercentLabels(100*x$Freq/sum(x$Freq))
  pos <- x$Freq / 2
  ggplot(x, aes(x = Var1, y = Freq)) + geom_bar(stat="identity", position="dodge", fill = ptol_pal()(1)) +
    geom_text(aes(y = pos, label=label), color = "white", size=8, fontface = 2) +
    coord_flip() + theme_gdocs() 
}

BarPlotMin <- function(df, aesX, aesY, labelYPercent = FALSE) {
  df <- subset(df, !is.na( df[, aesY]))
  p <- ggplot(df, aes_string(x = aesX, y = aesY)) + geom_bar(stat="identity", position="dodge", fill = ptol_pal()(1)) + theme_gdocs() 
  if(labelYPercent) {
    labels <- GetPercentLabels(df[, aesY], digits=0)
  } else {
    labels <- as.character(df[, aesY])
  }
  p <- p + geom_text(aes(y = df[, aesY], label=labels), color = "white", vjust=1.25, size=8, fontface = 2)
  return(p)
}


BarDodgedPlotMin <- function(df, aesX, aesY, aesF = "situation", labelYPercent = FALSE) {
  x <- subset(df, !is.na(df[, aesY]))
  if(labelYPercent) {
    labels <- GetPercentLabels(x[, aesY], digits=0)
  } else {
    labels <- as.character(x[, aesY])
  }
  ggplot(x, aes_string(x = aesX, y = aesY, fill = aesF)) + geom_bar(position = "dodge", stat = "identity") +
    geom_text(aes(y = x[, aesY], label = labels), position = position_dodge(width = 1), size = 8, fontface = 2, vjust=1.25, color = "white") +
    theme_gdocs() + scale_fill_ptol() +
    theme(legend.position="bottom", legend.direction="horizontal") 
}


BarFacetPlotMin <- function(df, aesFacet) {
  col.names <- c("Domaine", "situation", "X..emplois.cadre.ou.professions.intermédiaires", "X..emplois.à.temps.plein", "X..emplois.stables")
  col.times <- tail(col.names, -2)
  x <- subset(df, apply(df[,col.times], 1, function(x) !all(is.na(x))))
  x <- reshape(x, idvar=head(col.names, 3), varying=list(col.times), direction="long", v.names="valeur", timevar="indicateur", times=col.times)
  x$indicateur <- factor(x$indicateur, levels = c("X..emplois.stables", "X..emplois.à.temps.plein", "X..emplois.cadre.ou.professions.intermédiaires"), labels =  c("% emplois stables", "% emplois à temps plein", "% emplois cadre ou professions intermédiaires"))
  
  ggplot(x, aes(x = situation, y = valeur, fill = indicateur)) + geom_bar(position = "dodge", stat = "identity") + facet_wrap(aesFacet) +
    theme_gdocs() + scale_fill_ptol() + theme(legend.position="bottom", legend.direction="horizontal") 
}



BarStackedPlotRaw <- function(df, aesX, aesF, legend.title = NULL, labelX = TRUE, labelF = TRUE) {
  x <- as.data.frame(ftable(df[ , c(aesX, aesF), drop=TRUE]))
  totFreq <- sum(x$Freq)
  ## Percentage labels
  x$percentage <- 100 * x$Freq / totFreq
  x$percentage <-  GetPercentLabels(x$percentage, threshold = 2, digits = 0)
  x <- ddply(x, aesX, transform, pos = sum(Freq)-cumsum(Freq) + (0.5 * Freq), top = cumsum(Freq))
  x$toplab <-  GetPercentLabels(100 * x$top / totFreq, threshold = 0)
  m <- length(unique(x[,aesF]))
  ## exploit recycling
  x$toplab[ append(rep(TRUE,m-1), FALSE) ] <- "" 

  p <- ggplot(x, aes_string(x = aesX, y = "Freq", fill = aesF)) + geom_bar(stat="identity") + coord_flip() + theme_gdocs()
  
  if(labelF) {
    ## FIXME Stopped working when changing the fluid page into a navbar page
    ## commit cba64eb use a navbar page instead of a fluid page
    p <- p + geom_text(aes(y = pos, label = x$percentage, size = 6, position = "stack"), show.legend = FALSE)
  }
  
  if(labelX) {
    p <- p + geom_text(aes(y = x$top, label = x$toplab, size = 8, hjust = -0.25, vjust = -0.5, position = "stack", fontface = 2), show.legend = FALSE) + expand_limits( y = c(0,round(max(x$top)*1.05)))
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
    diplome=list("Mention" = su(data$libdip2), "Spécialité" = su(data$libdip3), "Code SISE" = su(data$code_diplome))
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
## TODO https://shiny.rstudio.com/articles/action-buttons.html
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
    BarStackedPlotRaw(rpopulation(), "serieBac", "regionBac", "Région d'obtention du bac") + ggtitle("Bac obtenu") + labs(x="Bac obtenu", y="Effectifs")
  )
  output$serieBac <- renderPlot(
    BarPlotRaw(rpopulation()$serieBac) + ggtitle("Bac obtenu") + labs(x="Bac obtenu", y="Effectifs") 
  )

  output$regionBac <- renderPlot(
    BarPlotRaw(rpopulation()$regionBac) + ggtitle("Région d'obtention du bac") + labs(x="Région d'obtention du bac", y="Effectifs") 
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
      BarStackedPlotRaw(remploye(), "regionEmploi", "regionBac", "Région d'obtention du bac") +
        labs(x="Région d'emploi", y="Effectifs") +
        ggtitle("Localisation de l'emploi et mobilité des diplomés")
    })

    
    output$niveauEmploi2 <- renderPlot({
      BarStackedPlotRaw(remploye(), "statutEmploiN30","niveauEmploiN30", "Niveau de l'emploi") + ggtitle("Statut de l'emploi") + labs(x="Niveau de l'emploi", y="Effectifs") 
    })
    output$statutEmploi <- renderPlot({
      BarPlotRaw(remploye()$statutEmploiN30) + ggtitle("Statut de l'emploi") + labs(x="Statut de l'emploi", y="Effectifs") 
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


shinyServer(
  ## Define server logic 
  function(input, output, session) {
    ## Run once each time a user visits the app

    MakeMinSelectionOutput(input, output, choices)
    MakeMinDebugOutput(input, output) ##DEBUG

    output$minDiplomeLP <- renderPlot({
      x <- dataMinDomLP
      x <- FilterSituation(x)
      ggplot(x, aes(x = "", y = Nombre.de.diplômés, fill = Domaine)) + geom_bar(stat = "identity") +  coord_polar("y", start=0) + scale_fill_ptol() +  theme_gdocs() + ggtitle("Nombre de diplômés") +
        theme(
          axis.line=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank()
        )
    })

    output$minInsertionLP <- renderPlot({
      BarDodgedPlotMin(dataMinDomLP, aesX = "Domaine", aesY = "Taux.d.insertion", labelYPercent = TRUE) +
        ggtitle("Évolution du taux d'insertion des diplômés")
    })
    
    
    output$minEmploiLP <- renderPlot({
      BarFacetPlotMin(dataMinDomLP, "Domaine") + ggtitle("Progression des conditions d'emploi des diplômés en emploi (en %)") 
    })
    
    output$minSalaireLP <- renderPlot({
      BarDodgedPlotMin(dataMinDomLP, aesX = "Domaine", aesY = "Salaire.net.médian.des.emplois.à.temps.plein", labelYPercent = FALSE) +
        ggtitle("Progression du salaire net mensuel médian à temps plein")
      })
    

    output$minFemmesLP <- renderPlot({
      x <- dataMinDomLP
      x <- FilterSituation(x)
      BarPlotMin(x, "Domaine", "X..femmes", labelYPercent = TRUE) + ggtitle("Pourcentage de femmes")
    })
    
    output$minReponsesLP <- renderPlot({
      x <- dataMinDomLP
      x <- FilterSituation(x)
      BarPlotMin(x, "Domaine", "Taux.de.réponse", labelYPercent = TRUE) + ggtitle("Taux de réponse")
     })
    
    
    MakeSelectionOutput(input, output, choices)
    ## MakeDebugOutput(input, output) ##DEBUG
    rpopulation <- MakeReactiveData(input, data, choices)
        
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

    ## x <- as.data.frame(table(data[, c("sexe","boursier")]))
    ## ggplot(x, aes(x = "", y = sexe, fill = boursier)) + geom_bar(stat = "identity") +  coord_polar("y", start=0) + scale_fill_ptol() +  theme_gdocs()
    
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
    

    updateNavbarPage(session, "navPage", selected = "minTabPanel")
    updateTabsetPanel(session, "minTabSetPanel", selected = "minLPPanel")
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
    observeEvent(input$copyButton, {
      clipr::write_clip(url())
    })
  }
)







