


## https://stackoverflow.com/questions/38653903/r-shiny-repetitive-evaluation-of-the-reactive-expression
## https://shiny.rstudio.com/articles/action-buttons.html
MakeCheckboxReactiveIndices <- function(input, data, choices) {
  isSelected <- function(column) length(input[[column]]) < length(choices[[column]])
  reactive({
    validate(
      need(input$annee, 'Choisir une ou plusieurs années.'),
      need(input$grade, 'Choisir un ou plusieurs grades.')
    )
    ## print("update indices")
    ## print(input$grade)
    ## print(input$annee)
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


MakeReactiveDiplomeChoices <- function(data, rindices) {
  reactive({
    #print("update diplomas")
    df <- droplevels(data[ rindices(), c("libdom", "libdip2", "libdip3", "code_diplome")])
    ## df <- data
    diplomes = list(
      "Domaine"= levels(df$libdom),
      "Mention" = levels(df$libdip2),
      "Spécialité" = levels(df$libdip3),
      "Code SISE" = sort(unique(df$code_diplome))
    )
  })
}

MakeSelectizeOutput <- function(input, output) {
  output$selectizeDiplome <- renderUI( {    
    selectizeInput(
      'diplome', 'Sélectionner un ou plusieurs domaines, mentions, spécialités ou codes SISE : ',
      NULL, multiple = TRUE, 
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


MakeReactiveDiplomes <- function(session, input, output, data) {
  ## Choose years and grades
  choices <- list(
    annee=sort(unique(data$annee)),
    grade=levels(data$libdip1)
  )

  ## TODO Avoid loading default when restoring
  updateCheckboxGroupInput(session, 'annee', choices = choices$annee, selected = choices$annee)
  updateCheckboxGroupInput(session, 'grade', choices = choices$grade, selected = choices$grade)
 

  rindices <- MakeCheckboxReactiveIndices(input, data, choices)

  ## Selectize Diplomas
  ## MakeSelectizeOutput(input, output)
  ## Reactive diplomas List
  diplomeChoices <- MakeReactiveDiplomeChoices(data, rindices)
  ## Default selected diplomas
  stateInputDiplome <- NULL
  ## stateInputGrade <- NULL
  ## stateInputAnnee <- NULL
  
  ## Update selectize when the diplomas list has changed
  observe({
    #print("observer")
    #print(input$diplome)
    #print(stateInputDiplome)
    #updateCheckboxGroupInput(session, 'grade', choices = choices$grade, selected = "Master")
    #print(input$grade)
    if(is.null(isolate(input$diplome))) {
      updateSelectizeInput(session, 'diplome', choices = diplomeChoices(), selected = stateInputDiplome)
      ## Restore the state only once
      ## stateInputDiplome <<- NULL
    } else {
      updateSelectizeInput(session, 'diplome', choices = diplomeChoices(), selected = isolate(input$diplome))
    }
  })
  
  ## Update the default selected diplomas when restoring a bookmark 
  onRestored(function(state) {
    #print("onRestored")
    ## update default selected diplomes in the parent env for the observer
    stateInputDiplome <<- state$input$diplome
    ## These two checkboxes are not restored whereas the 'sexe' one is.
    #print(input$grade)
    updateCheckboxGroupInput(session, 'grade', choices = choices$grade, selected = state$input$grade)
    updateCheckboxGroupInput(session, 'annee', choices = choices$annee, selected = state$input$annee)
    ## Selectize will be updated by the observer
    ## updateSelectizeInput(session, 'diplome', choices = diplomeChoices(), selected = stateInputDiplome)
  })
  ## Make reactive graduates list
  MakeReactiveData(input, data, rindices)
}
