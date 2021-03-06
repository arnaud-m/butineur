
MakeResultatsOutput <- function(output, rdiplomes) {
  output$recapReponse <- renderTable( {
    df <- rdiplomes()
    n <- nrow(df)
    m <- n - sum(df$doubleDiplome)
    p <- sum(df$repondant)
    data.frame(
      c("Nombre de diplômes", "Nombre de diplômés", "Questionnaires exploités", "Taux de réponse"),
      c(n, m, p, sprintf("%.1f%%", 100*p/m))
    )
  }, colnames = FALSE, striped = TRUE,  spacing = 'l'
  )
  
  output$statutReponse <- renderTable({
    table(rdiplomes()[,"statutReponse"][drop=TRUE], useNA = "no")
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
  output$etudesInsertionTaux <- renderTable({
    x <- rpopulation()
    x <- aggregate( 100*x[,c("poursuiteEtudes", "insertionN18","insertionN30")], by = list(x$libdip1), mean, na.rm=TRUE)
    colnames(x) <- c("Grade", "Poursuite d'études", "Insertion à 18 mois", "Insertion à 30 mois")
    x
  }, digits = 1, spacing = 'l')

  output$insertionMNESR <- renderTable({
    x <- rpopulation()
   ## browser()
    y <- x[,c("poursuiteEtudes", "mobiliteEmploi", 
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
        median(x$salaireEmploiN18[x$emploiPleinN18], na.rm=TRUE),
        mean(x$salaireEmploiN18[x$emploiPleinN18], na.rm=TRUE)
      ),
      c(
        y[c("insertionN30", "poursuiteEtudes",
            "emploiStableN30", "emploiPleinN30", "emploiSupIntN30",
            "mobiliteEmploi")],
        median(x$salaireEmploiN30[x$emploiPleinN30], na.rm=TRUE),
        mean(x$salaireEmploiN30[x$emploiPleinN30], na.rm=TRUE)
      ),
      row.names = c("Taux d'insertion", "Poursuite d'études", 
                    "Part des emplois stables",
                    "Part des emplois à temps plein",
                    "Part des emplois de niveau cadre ou profession intermédiaire",
                    "Part des emplois situés en dehors de la région de l'établissement (y compris à l'étranger)",
                    "Salaire net mensuel médian des emplois à temps plein (en euros)",
                    "Salaire net mensuel moyen des emplois à temps plein (en euros)"
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
    colnames(y) <- c("grade", labels, labels)
    
    z <- rbind(
      cbind(date = "N+18", melt(y[,1:4], "grade")),
      cbind(date = "N+30", melt(y[,c(1,5:7)], "grade"))
    )
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
    BarStackedPlotRaw(rpopulation(), "situationProN30", "etudesN30", "En études") + ggtitle("Situation des diplômés à N + 30 mois", subtitle="Analyse croisée des cas mixtes, emploi ou recherche d’emploi et études.") + labs(x="Situation professionnelle", y="Effectifs")
  })
  
  output$situationDiplomeN18 <- renderPlot({
    BarStackedPlotRaw(rpopulation(), "situationProN18", "etudesN18", "En études") + ggtitle("Situation des diplômés à N + 18 mois", subtitle="Analyse croisée des cas mixtes, emploi ou recherche d’emploi et études.") + labs(x="Situation professionnelle", y="Effectifs")
  }
  )
}

MakeEmploiOutput <- function(output, remployes) {
  output$regionEmploi <- renderPlot({
      tauxMobilite <- 100*mean(remployes()$mobiliteEmploi, na.rm = TRUE)
      BarStackedPlotRaw(remployes(), "regionEmploi", "regionBac", "Région d'obtention du bac") +
        labs(x="Région d'emploi", y="Effectifs") +
        ggtitle("Localisation de l'emploi et mobilité des diplômés", subtitle = sprintf("Le taux de mobilité des diplômés est de %.1f%%", tauxMobilite))
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
    wordcloud_rep(names(v), v, scale=c(3.5,0.75), ##rot.per = 0.15,
                  min.freq = 3, max.words=80,
                  colors=brewer.pal(8, "Dark2"))
  })  
}
