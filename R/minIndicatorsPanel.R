
FilterSituation <-function(dataMin, after=30) {
  subset(
    dataMin,
    dataMin$situation == paste0(after, " mois après le diplôme")
  )
}

MakeReactiveMinData <- function(input, data) {
  reactive({
    validate(
      need(input$domaine, 'Choisir un domaine.'),
      need(input$annee, 'Choisir une année.')
    )
    domaine <- input$domaine
    domaines <- levels(data$Code.du.domaine)
    if(domaine %in% domaines) {
      ## All disciplines of the given domain
      domInd <- data$Code.du.domaine == domaine
    } else {
      ## All domains
      ensInd <- grepl("^Ensemble ", data$Discipline)
      missInd <- data$Code.du.domaine %in% setdiff(domaines, data$Code.du.domaine[ensInd])
      domInd <- ensInd | missInd
    }
    droplevels(
      subset(
        data,
        data$Annee == input$annee & domInd
      )
    )
  })
}


GetPercentLabels <- function(x, threshold = 1, digits = 0) {
  ind <- x >= threshold
  r <- rep("", length(x))
  r[ind] <- sprintf(paste0("%.", digits, "f%%"), x[ind])
  return(r)
}

BarPlotMin <- function(df, aesX, aesY, labelYPercent = FALSE) {
  df <- subset(df, !is.na( df[, aesY]))
  p <- ggplot(df, aes_string(x = aesX, y = aesY)) + geom_bar(stat="identity", position="dodge", fill = ptol_pal()(1)) + theme_gdocs() 
  if(labelYPercent) {
    labels <- GetPercentLabels(df[, aesY])
  } else {
    labels <- as.character(df[, aesY])
  }
  p <- p + geom_text(aes(y = df[, aesY], label=labels), color = "white", vjust=1.25, size=7, fontface = 2)
  return(p)
}

BarDodgedPlotMin <- function(df, aesX, aesY, aesF = "situation", labelYPercent = FALSE) {
  x <- subset(df, !is.na(df[, aesY]))
  if(labelYPercent) {
    labels <- GetPercentLabels(x[, aesY])
  } else {
    labels <- as.character(x[, aesY])
  }
  ggplot(x, aes_string(x = aesX, y = aesY, fill = aesF)) + geom_bar(position = "dodge", stat = "identity") +
    geom_text(aes(y = x[, aesY], label = labels), position = position_dodge(width = 1), size = 7, fontface = 2, vjust=1.25, color = "white") +
    theme_gdocs() + scale_fill_ptol() +
    theme(legend.position="bottom", legend.direction="horizontal") 
}


BarFacetPlotMin <- function(df, aesFacet) {
  col.names <- c(aesFacet, "situation", "X..emplois.cadre.ou.professions.intermédiaires", "X..emplois.à.temps.plein", "X..emplois.stables")
  col.times <- tail(col.names, -2)
  x <- subset(df, apply(df[,col.times], 1, function(x) !all(is.na(x))))
  x <- reshape(x, idvar=head(col.names, 3), varying=list(col.times), direction="long", v.names="valeur", timevar="indicateur", times=col.times)
  x$indicateur <- factor(x$indicateur, levels = c("X..emplois.stables", "X..emplois.à.temps.plein", "X..emplois.cadre.ou.professions.intermédiaires"), labels =  c("% emplois stables", "% emplois à temps plein", "% emplois cadre ou professions intermédiaires"))
  
  ggplot(x, aes(x = situation, y = valeur, fill = indicateur)) + geom_bar(position = "dodge", stat = "identity") + facet_wrap(aesFacet) +
    theme_gdocs() + scale_fill_ptol() + theme(legend.position="bottom", legend.direction="horizontal") + labs(y = "Taux (%)")
}


MinIndicatorsUI <- function(id, title, value) {
  ns <- NS(id)
  tabPanel(
    title = title,
    value = value,
    h3(textOutput(ns("minHeader"))),
    fluidRow(
      column(3, radioButtons(ns("annee"), "Année : ", choices = c(""), inline=TRUE)),
      column(3, uiOutput(ns("selectizeDomaine")))
    ),
    fluidRow(
      column(3, tableOutput(ns("domaineCodes"))),
      column(3, plotOutput(ns("tauxReponse"))),
      column(3, plotOutput(ns("diplomes"))),
      column(3, plotOutput(ns("pourcentFemmes")))
    ),
    h3("Conditions d'emploi"),
    fluidRow(
      column(6, plotOutput(ns("tauxInsertion"))),
      column(6, plotOutput(ns("conditionEmploi")))
      ),
    h3("Conditions salariales"),
    fluidRow(
      column(6, plotOutput(ns("salaireNetMensuel"))),
      column(6, plotOutput(ns("salaireBrutAnnuel")))
    ),
    h3("Mobilité"),
    fluidRow(
      column(6, plotOutput(ns("tauxMobilite")))
    )
  )
}

MinIndicators <- function(input, output, session, data) {

  ## Radio buttons
  choices <- sort(unique(data$Annee))
  updateRadioButtons(
    session, "annee",
    choices = choices,
    selected = max(choices),
    inline=TRUE
  )


  ensInd <- grepl("^Ensemble ", data$Discipline)
  domainChoices <- levels(data$Code.du.domaine[ensInd, drop = TRUE])
  names(domainChoices) <- levels(data$Domaine[ensInd, drop = TRUE])
  domainChoices <- c(
    "Regroupement par domaine"="ALL.DOM",
    domainChoices
  )

  ## bookmarking currently does not seem to support accented characters 
  ## https://stackoverflow.com/q/47743120
  ## Workaround : use domain code instead of domain as value
  output$selectizeDomaine <- renderUI( {
    ## Using renderUI within modules
    ns <- session$ns
    selectizeInput(
      ns("domaine"), "Choisir un domaine pour zoomer sur ses disciplines : ",
      domainChoices, multiple = FALSE, 
      options = list(
        placeholder = "Taper la sélection ici."
      ), width = "350px"
    )
  })

  onRestored(function(state) {
    ##FIXME Two calls ! can validate ?
    print(state$input$domaine)
    print(state$input$annee)
    updateSelectizeInput(session, 'domaine', choices = domainChoices, selected = state$input$domaine)
    updateRadioButtons(
      session, "annee",
      choices = choices,
      selected = state$input$annee,
      inline=TRUE
  )
  })
  
  ## Reactive data
  rdata <- MakeReactiveMinData(input, data)
  ## Keep only data at N+30 months
  rdata30 <- reactive({
    ## print(rdata())
    FilterSituation(rdata())
  })

  isByDomain <- reactive({
    input$domaine == "ALL.DOM"
  })
  
  rcolBy <- reactive ({
    ifelse(isByDomain(), "Domaine", "Discipline")
  })

  raesBy <- reactive ({
    ifelse(isByDomain(), "Code.du.domaine", "Code.de.la.discipline")
  })
  
  ## Header
  ## FIXME twice the good count
  output$minHeader <- renderText({
    data <- rdata30()
    nbDiplomes <- ifelse(
      isByDomain(),
      sum(data$Nombre.de.diplômés),
      ## The row "Ensemble" exists, otherwise the domain could not be selected 
      data$Nombre.de.diplômés[grepl("^Ensemble ", data$Discipline)]
    )

    paste0("Résultats et caractéristiques socio-démographiques (", nbDiplomes," diplômés)")
  })
  
  ## ######################
  ## Tableau des domaines
  output$domaineCodes <- renderTable({
    x <- unique(rdata30()[, c(rcolBy(), raesBy(), "Nombre.de.diplômés")])
    colnames(x) <- c(rcolBy(), "Code", "Diplômés")
    x[ order(x$Code), ]
  }, rownames = FALSE, colnames = TRUE, digits = 0, striped = TRUE,  spacing = 'l')
  
  ## ######################
  ## Ensemble des diplômés
  output$diplomes <- renderPlot({
    data <- rdata30()
    if(!isByDomain()) {
      data <- subset(data, !grepl("^Ensemble ", data$Discipline))
    }
    ## browser()
    ggplot(data, aes_string(x= NA, y = "Nombre.de.diplômés", fill = raesBy())) + geom_bar(stat = "identity") +  coord_polar("y", start=0) + scale_fill_ptol(name=rcolBy()) +  theme_gdocs() + ggtitle("Nombre de diplômés") + labs(x = rcolBy()) + 
      theme(
        axis.line=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="bottom",
        legend.direction="horizontal") 
  })

  output$tauxReponse <- renderPlot({
    BarPlotMin(rdata30(), raesBy(), "Taux.de.réponse", labelYPercent = TRUE) + ggtitle("Taux de réponse") + labs(x = rcolBy(), y = "Taux de réponse (%)")
  })
  
  output$pourcentFemmes <- renderPlot({
    BarPlotMin(rdata30(), raesBy(), "X..femmes", labelYPercent = TRUE) + ggtitle("Pourcentage de femmes") + labs(x = rcolBy(), y = "Taux de femmes (%)")
  })

  ## ######################
  ## Diplômés en emploi
  output$tauxInsertion <- renderPlot({
    BarDodgedPlotMin(rdata(), aesX = raesBy(), aesY = "Taux.d.insertion", labelYPercent = TRUE) +
      ggtitle("Évolution du taux d'insertion des diplômés", subtitle = sprintf("Le taux de chomage régional est de %.1f%%", rdata()[1, "Taux.de.chômage.régional"])) + labs(x = rcolBy(), y = "Taux d'insertion (%)")
  })
  
  output$conditionEmploi <- renderPlot({
    BarFacetPlotMin(rdata(), rcolBy()) + ggtitle("Progression des conditions d'emploi des diplômés en emploi") 
  })
  
  output$salaireNetMensuel <- renderPlot({
    BarDodgedPlotMin(rdata(), aesX = raesBy(), aesY = "Salaire.net.médian.des.emplois.à.temps.plein", labelYPercent = FALSE) +
      ggtitle("Progression du salaire net mensuel médian à temps plein", subtitle = sprintf("Le salaire net mensuel médian régional est de %d euros.",rdata()[1, "Salaire.net.mensuel.médian.régional"])) +labs(x = rcolBy(), y = "euros") 
  })

    output$salaireBrutAnnuel <- renderPlot({
    BarDodgedPlotMin(rdata(), aesX = raesBy(), aesY = "Salaire.brut.annuel.estimé", labelYPercent = FALSE) +
      ggtitle("Progression de l'estimation du salaire net brut annuel médian à temps plein") +labs(x = rcolBy(), y = "euros") 
  })

  output$tauxMobilite <- renderPlot({
    BarPlotMin(rdata30(), raesBy(), "X..emplois.extérieurs.à.la.région.de.l.université", labelYPercent = TRUE) + ggtitle("Taux de mobilité") + labs(x = rcolBy(), y = "% emplois extérieurs à la région de l'université")
  }) 
}

