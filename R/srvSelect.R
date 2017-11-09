MakeCoreChoices <- function(data) {
  list(
    annee=sort(unique(data$annee)),
    grade=levels(data$libdip1)
  )
}



MakeCoreSelectionOutput <- function(output, choices) {
  output$checkboxAnnee <- renderUI( {
    checkboxGroupInput("annee", "Année(s)", choices$annee, choices$annee)
  })
  
  output$checkboxGrade <- renderUI( {
    checkboxGroupInput("grade", "Grade(s)", choices$grade, choices$grade)
  })
}


## https://stackoverflow.com/questions/38653903/r-shiny-repetitive-evaluation-of-the-reactive-expression
## https://shiny.rstudio.com/articles/action-buttons.html
MakeReactiveCoreData <- function(input, data, choices) {
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
    subset(data, logInd)
  })
}


MakeReactiveSelectizeOutput <- function(input, output, coreData) {
  output$selectizeDiplome <- renderUI( {
    data <- coreData()
    diplomes = list(
      "Domaine"= levels(data$libdom[drop=TRUE]),
      "Mention" = levels(data$libdip2[drop=TRUE]),
      "Spécialité" = levels(data$libdip3[drop=TRUE]),
      "Code SISE" = sort(unique(data$code_diplome))
    )
    selectizeInput(
      'diplome', 'Sélectionner un ou plusieurs domaines, mentions, spécialités ou codes SISE : ',
      diplomes, selected = input$diplome, multiple = TRUE, 
      options = list(
        placeholder = "Taper la sélection ici."
      ), width = "800px"
    )
  })
}


## https://stackoverflow.com/questions/38653903/r-shiny-repetitive-evaluation-of-the-reactive-expression
## https://shiny.rstudio.com/articles/action-buttons.html
MakeReactiveData <- function(input, coreData) {
  reactive({
    data <- coreData()
    logInd <- rep(TRUE, nrow(data))
    if( length(input$sexe) < 2) {
      ## Sélection active : un seul genre est sélectionné.
      logInd <- logInd & (data$sexe %in% input$sexe)
    }
    
    if(! is.null(input$diplome) ) {
      ## Sélection active : un sous-ensemble de diplômes est sélectionné.
      logInd <- logInd & (data$libdom %in% input$diplome | data$libdip2 %in% input$diplome | data$libdip3 %in% input$diplome | data$code_diplome %in% input$diplome)
    }
    subset(data, logInd)
  })
}
