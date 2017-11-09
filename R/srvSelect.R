

MakeCheckboxOutput <- function(output, choices) {
  output$checkboxAnnee <- renderUI( {
    checkboxGroupInput("annee", "Année(s)", choices$annee, choices$annee)
  })
  
  output$checkboxGrade <- renderUI( {
    checkboxGroupInput("grade", "Grade(s)", choices$grade, choices$grade)
  })
}


## https://stackoverflow.com/questions/38653903/r-shiny-repetitive-evaluation-of-the-reactive-expression
## https://shiny.rstudio.com/articles/action-buttons.html
MakeCheckboxReactiveIndices <- function(input, data, choices) {
  isSelected <- function(column) length(input[[column]]) < length(choices[[column]])
  reactive({
    if( isSelected("annee")) {
      if(isSelected("grade")) {
        logInd <- (data$annee %in% input$annee) & (data$libdip1 %in% input$grade)
      } else {
        logInd <- (data$annee %in% input$annee)
      }
    } else if(isSelected("grade")) {
      logInd <- (data$libdip1 %in% input$grade)
    } else {
      logInd <- rep(TRUE, nrow(data))
    }
    logInd
  })
}


MakeReactiveSelectizeOutput <- function(input, output, data, rindices) {
  output$selectizeDiplome <- renderUI( {
    df <- droplevels(data[ rindices(), c("libdom", "libdip2", "libdip3", "code_diplome")])
    ##df <- data
    diplomes = list(
      "Domaine"= levels(df$libdom),
      "Mention" = levels(df$libdip2),
      "Spécialité" = levels(df$libdip3),
      "Code SISE" = sort(unique(df$code_diplome))
    )
    
    selectizeInput(
      'diplome', 'Sélectionner un ou plusieurs domaines, mentions, spécialités ou codes SISE : ',
      diplomes, selected = isolate(input$diplome), multiple = TRUE, 
      options = list(
        placeholder = "Taper la sélection ici."
      ), width = "800px"
    )
  })
}


## https://stackoverflow.com/questions/38653903/r-shiny-repetitive-evaluation-of-the-reactive-expression
## https://shiny.rstudio.com/articles/action-buttons.html
MakeReactiveData <- function(input, data, rindices) {
  reactive({
    data <- subset(data, rindices())
    GetDiplomaIndices <- function() data$libdom %in% input$diplome | data$libdip2 %in% input$diplome | data$libdip3 %in% input$diplome | data$code_diplome %in% input$diplome
    GetSexeIndices <- function() data$sexe %in% input$sexe
    if( length(input$sexe) < 2) {
      ## Sélection d'un sous-ensemble de genres.
      if(!is.null(input$diplome) ) {
        ## Sélection d'un sous-ensemble de diplômes.
        data <- subset(data, GetSexeIndices() & GetDiplomaIndices())
      } else {
        data <- subset(data, GetSexeIndices())
      }
    } else if(!is.null(input$diplome) ) {
      ## Sélection d'un sous-ensemble de diplômes seulement.
      data <- subset(data, GetDiplomaIndices())
    }
    data
  })
}


MakeReactiveDiplomes <- function(input, output, data) {
  choices <- list(
    annee=sort(unique(data$annee)),
    grade=levels(data$libdip1)
  )
  MakeCheckboxOutput(output, choices)
  rindices <- MakeCheckboxReactiveIndices(input, data, choices)
  MakeReactiveSelectizeOutput(input, output, data, rindices)
  MakeReactiveData(input, data, rindices)
}
