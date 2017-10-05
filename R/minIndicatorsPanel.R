
FilterSituation <-function(dataMin, after=30) {
  subset(
    dataMin,
    dataMin$situation == paste0(after, " mois après le diplôme")
  )
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
  col.names <- c("Domaine", "situation", "X..emplois.cadre.ou.professions.intermédiaires", "X..emplois.à.temps.plein", "X..emplois.stables")
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
    h3("Caractéristiques socio-démographiques"),
    fluidRow(
      column(4, plotOutput(ns("diplomes"))),
      column(4, plotOutput(ns("tauxReponse"))),
      column(4, plotOutput(ns("pourcentFemmes")))
    ),
    h3("Conditions d'emploi"),
    fluidRow(
      column(6, plotOutput(ns("tauxInsertion"))),
      column(6, plotOutput(ns("conditionEmploi"))),
      column(6, plotOutput(ns("salaireMedian"))),
      column(6, plotOutput(ns("tauxMobilite")))
    )
  )
}

MinIndicators <- function(input, output, session, data) {

  ## Keep only data at N+30 months
  fdata <- FilterSituation(data)
  
  ## ######################
  ## Ensemble des diplômés
  output$diplomes <- renderPlot({
    ggplot(fdata, aes(x = "", y = Nombre.de.diplômés, fill = Domaine)) + geom_bar(stat = "identity") +  coord_polar("y", start=0) + scale_fill_ptol() +  theme_gdocs() + ggtitle("Nombre de diplômés") +
      theme(
        axis.line=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()
      )
  })

  output$tauxReponse <- renderPlot({
    BarPlotMin(fdata, "Code.du.domaine", "Taux.de.réponse", labelYPercent = TRUE) + ggtitle("Taux de réponse") + labs(x = "Domaine", y = "Taux de réponse (%)")
  })
  
  output$pourcentFemmes <- renderPlot({
    BarPlotMin(fdata, "Code.du.domaine", "X..femmes", labelYPercent = TRUE) + ggtitle("Pourcentage de femmes") + labs(x = "Domaine", y = "Taux de femmes (%)")
  })

  ## ######################
  ## Diplômés en emploi
  output$tauxInsertion <- renderPlot({
    BarDodgedPlotMin(data, aesX = "Code.du.domaine", aesY = "Taux.d.insertion", labelYPercent = TRUE) +
      ggtitle("Évolution du taux d'insertion des diplômés", subtitle = sprintf("Le taux de chomage régional est de %.1f%%", data[1, "Taux.de.chômage.régional"])) + labs(x = "Domaine", y = "Taux d'insertion (%)")
  })
  
  output$conditionEmploi <- renderPlot({
    BarFacetPlotMin(data, "Domaine") + ggtitle("Progression des conditions d'emploi des diplômés en emploi (en %)") 
  })
  
  output$salaireMedian <- renderPlot({
    BarDodgedPlotMin(data, aesX = "Code.du.domaine", aesY = "Salaire.net.médian.des.emplois.à.temps.plein", labelYPercent = FALSE) +
      ggtitle("Progression du salaire net mensuel médian à temps plein", subtitle = sprintf("Le salaire net mensuel médian régional est de %d euros.",data[1, "Salaire.net.mensuel.médian.régional"])) +labs(x = "Domaine", y = "euros") 
  })
  
  output$tauxMobilite <- renderPlot({
    BarPlotMin(fdata, "Code.du.domaine", "X..emplois.extérieurs.à.la.région.de.l.université", labelYPercent = TRUE) + ggtitle("Taux de mobilité") + labs(x = "Domaine", y = "% emplois extérieurs à la région de l’université")
  }) 
}

