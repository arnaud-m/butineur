ReadIP <- function(file) {
  ## Read the IP database and preprocess the data.
  ## Columns must be given as MNESR codes.
  ## Visualization uses mostly columns created by this function.
  ## The survey model and charts model are separated.
  df <- read.csv(file = file, row.names = NULL, strip.white = TRUE, na.strings = c("NA", "#N/A", ""))

  ## FIXME Remove VAEs
  df <- subset(df, df[,"attribute_8..profil.étudiant_BO."] != "dem VAE")

  ##res <- data.frame(matrix(, nrow=nrow(df), ncol=0))
  res <- df[,c("Année", "Diplôme", "code_diplome")] 
  
  colnames(res) <- c("annee", "libdip1", "code_diplome") ##, "libdip3", "libdip2", "composante_lib_BO")

  ## res$libdom  <- factor(gsub('[[:blank:]]*-.*$','', df$attribute_27_dom_mention_SISE_BO)) ## domaine
  res$libdom  <- df$DOMAINE ## domaine
  ## LP has only a 'mention' 
  ## res$libdom[res$libdip1=="LP"] <- NA

  ## res$libdip2 <- factor(gsub('^[^-]*-[[:blank:]]','', df$attribute_27_dom_mention_SISE_BO)) ## mention
  res$libdip2 <- df$Mention ## Mention
  res$libdip3 <- df$Spécialité ## spécialité
  
  ColToFactor <- function(fromCol, toCol, labels, levels = seq_along(labels)) {
    res[,toCol] <<- factor(df[, fromCol], levels = levels, labels = labels)
  }

  ## ColToFactor("attribute_10_sexe_BO", "sexe", c("Femme", "Homme"), c("F", "M"))
  res$sexe <- df$Sexe.BO
  ## Detailed classification
  bacs <- read.csv(file = file.path("data", "baccalaureats.csv"), na.strings = c("NA", "N/A", ""))
  bacs <- aggregate(bacs$bac, by = list(bacs$categorie), paste)
  x <- bacs[,2]
  names(x) <- bacs[,1]
  bacs <- x
  ##FIXME bad format
  res$serieBac <- df[, "X.attribute_34..code.bac_BO."]
  levels(res$serieBac) <- bacs
  
  GetRegion <- function(x) {
    y <- c(rep(3, 98), 4)
    y[6] <- 1
    y[c(4,5,13,83,84)] <- 2
    z <- factor(y[x], levels = 1:4, labels = c("Alpes-Maritimes", "PACA hors\nAlpes-Maritimes", "Hors PACA", "Étranger"))
  }
  res$regionEmploi <- GetRegion(df$q6_14a)
  res$mobiliteEmploi <- res$regionEmploi == "Étranger" | res$regionEmploi == "Hors PACA"
  ## ##res$regionBac <- GetRegion(df$region_bac)
  ## FIXME bad format
  ## res$regionBac <- GetRegion(df[ ,"X.attribute_32..dpt.obtention.bac_code_BO."])
  res$regionBac <- "FIXME"
  res$repondant <- df$Eq.statut_reponse %in% 4:6


  
  ##   ColToFactor(
  ##   "statut_reponse", "statutReponse",
  ##   c(
  ##     "Deuxième diplôme dans le cas d'un double diplôme",
  ##     "Décédé",
  ##     "Erreur sur le diplôme :\n diplômé n'ayant pas validé le diplôme",
  ##     "téléphone",
  ##     "internet",
  ##     "papier",
  ##     "Diplômé issu d'une formation délocalisée\n(pour les diplômés étrangers uniquement)",
  ##     "Autre"
  ##   )
  ## )
   ColToFactor(
    "Eq.statut_reponse", "statutReponse",
    c(
      "Deuxième diplôme\n(double diplôme)",
      "Décédé",
      "Erreur : non validé",
      "téléphone",
      "internet",
      "papier",
      "Formation délocalisée",
      "Autre"
    )
   )

  
 
  ## ## ColToFactor("q2_2", "boursier", c("Oui sur critères sociaux", "Oui sur d'autres critères", "Non"))
  res$boursier <- as.factor(df$Eq.q2_2)
  levels(res$boursier) <- list(Boursier=1:2, "Non boursier"=3)
  
  poursuiteEtude <- c(
    "En doctorat (Master) / en Master (LP)",
    "Dans une autre formation",
    "Non"
  )
  
  ColToFactor("Eq.q3_1_1", "etudeN6", poursuiteEtude)
  ColToFactor("Eq.q3_1_2", "etudeN18", poursuiteEtude)
  ColToFactor("Eq.q4_1", "etudeN30", poursuiteEtude)
  ## poursuivent des études dans les deux ans.
  res$poursuiteEtude <- (df$Eq.q3_1_1 != 3) | (df$Eq.q3_1_2 != 3)

  ## ## situationPro <- c(
  ## ##   "Vous avez un emploi ",
  ## ##   "Vous n'avez pas d'emploi et\n vous recherchez du travail ou\n vous êtes en attente d'un contrat",
  ## ##   "Vous n'avez pas d'emploi et\n vous ne cherchez pas de travail"
  ## ## )
  situationPro <- c(
    "En emploi",
    "En recherche d'emploi",
    "Ne recherche pas d'emploi"
  )
  ColToFactor("Eq.q4_3r", "situationProN30", situationPro)
  ColToFactor("Eq.q7_1", "situationProN18", situationPro)

  ColToFactor("Eq.q4_3r", "situationProN30r", c(situationPro, "En études"))
  res$situationProN30r[ df$Eq.q4_1 != 3 & df$eq.q4_2r == 1 & df$Eq.COD.q6_5 != 10 ] <- "En études"
  res$situationProN30r[ df$Eq.q4_1 != 3 & df$Eq.q4_3 == 1 & df$Eq.COD.q6_5 == 9 ] <- "En études"
  res$situationProN30r[ df$Eq.q4_1 != 3 & df$Eq.q4_3 == 1 & df$Eq.COD.q6_5 == 10 ] <- "En emploi"

  ColToFactor("Eq.q7_1", "situationProN18r", c(situationPro, "En études"))
  ## ## Pas de question sur l'activite principale à N+18
  res$situationProN18r[ df$Eq.q3_1_2 != 3 & df$Eq.q7_1 == 1 & df$Eq.q8_1 == 9 ] <- "En études"
  res$situationProN18r[ df$Eq.q3_1_2 != 3 & df$Eq.q7_1 == 1 & df$Eq.q8_1 == 10 ] <- "En emploi"

  res$insertionN30 <- res$situationProN30r == "En emploi"
  res$insertionN30[ res$situationProN30r %in% c("Ne recherche pas d'emploi","En études") ] <- NA

  res$insertionN18 <- res$situationProN18r == "En emploi"
  res$insertionN18[ res$situationProN18r %in% c("Ne recherche pas d'emploi","En études") ] <- NA
  
  ## ## statutEmploi <- c( 
  ## ##   "Prof. libérale, indépendant,\n chef d'entreprise, auto-entrepreneur",
  ## ##   "Fonctionnaire\n(y compris fonctionnaire stagiaire ou élève fonctionnaire)",
  ## ##   "CDI",
  ## ##   "Contrat spécifique au doctorat\n(contrat doctoral, allocation recherche, CIFRE….)",
  ## ##   "CDD (hors contrat spécifique au doctorat et \ny compris contractuel de la fonction publique...)",
  ## ##   "Vacataire",
  ## ##   "Intérimaire",
  ## ##   "Intermittent du spectacle, pigiste",
  ## ##   "Contrat d'apprentissage",
  ## ##   "Contrat de professionnalisation",
  ## ##   "Emplois aidés (Contrat Initiative Emploi…)",
  ## ##   "Volontariat international",
  ## ##   "Service civique")
  
  statutEmploi <- c( 
    "Prof. libérale, indépendant,\nchef d'entreprise, auto-entrepreneur",
    "Fonctionnaire",
    "CDI",
    "Contrat spécifique au doctorat",
    "CDD",
    "Vacataire",
    "Intérimaire",
    "Intermittent du spectacle, pigiste",
    "Contrat d'apprentissage",
    "Contrat de professionnalisation",
    "Emplois aidés",
    "Volontariat international",
    "Service civique")

  ColToFactor("Eq.COD.q6_5", "statutEmploiN30", statutEmploi)
  ColToFactor("Eq.q8_1", "statutEmploiN18", statutEmploi)

  ## ## Diplômés en emploi
  emploiN30 <- res$insertionN30 %in% TRUE
  emploiN18 <- res$insertionN18 %in% TRUE
  
  ## ## L'emploi stable correspond à la part des diplômés en emploi sous contrat de CDI, sous statut de la Fonction publique ou en qualité de travailleur indépendant.
  res$emploiStableN30 <- NA
  res$emploiStableN30[emploiN30] <- df$Eq.COD.q6_5[emploiN30] <= 3
  res$emploiStableN18 <- NA
  res$emploiStableN18[emploiN18] <- df$Eq.q8_1[emploiN18] <= 3 

  res$emploiPleinN30 <- NA
  res$emploiPleinN30[emploiN30] <- df$Eq.q6_7[emploiN30] == 1
  res$emploiPleinN18 <- NA
  res$emploiPleinN18[emploiN18] <- df$Eq.q8_3[emploiN18] == 1


  ## ## niveauEmploi <- c(
  ## ##   "personnel de catégorie A de la fonction publique",
  ## ##   "ingénieur, cadre, professions libérales, professions intellectuelles supérieures",
  ## ##   "personnel de catégorie B de la fonction publique",
  ## ##   "emploi de niveau intermédiaire : technicien, agent de maîtrise, maîtrise administrative et commerciale, VRP",
  ## ##   "personnel de catégorie C de la fonction publique",
  ## ##   "manœuvre, ouvrier",
  ## ##   "employé de bureau, de commerce, personnel de service"
  ## ## )
  ## ## ColToFactor("q8_2r", "niveauEmploiN18", niveauEmploi)
  ## ## ColToFactor("q6_6r", "niveauEmploiN30", niveauEmploi)

  res$niveauEmploiN18 <- factor(df$Eq.q8_2)
  levels(res$niveauEmploiN18) <- list('ingénieur ou cadre /cat. A'=1:2, 'technicien ou agent de maîtrise / cat. B'=3:4, 'ouvrier ou employé / cat. C'=5:7)

  res$niveauEmploiN30 <- factor(df$Eq.q6_6) 
  levels(res$niveauEmploiN30) <- list('ingénieur ou cadre /cat. A'=1:2, 'technicien ou agent de maîtrise / cat. B'=3:4, 'ouvrier ou employé / cat. C'=5:7)
  
  ## ## https://fr.wikipedia.org/wiki/Professions_et_cat%C3%A9gories_socioprofessionnelles_en_France
  res$emploiSupIntN30 <- NA
  res$emploiSupIntN30[emploiN30] <- df$Eq.q6_6[emploiN30] <= 4
  res$emploiSupIntN18 <- NA
  res$emploiSupIntN18[emploiN18] <- df$Eq.q8_2[emploiN18] <= 4

  ## ## Salaire mensuel net avec primes
  GetSalaireEmploi <- function(salaires,primes) {
    salaires <- as.numeric(salaires)
    primes <- as.numeric(primes)
    ## Si le montant des primes n'est pas précisée : on considère qu'il est nul.
    primes[is.na(primes)] <- 0
    return(salaires + primes/12)
  }
  
  res$salaireEmploiN18 <- GetSalaireEmploi(df$q8_5, df$q8_7) 
  res$salaireEmploiN30 <- GetSalaireEmploi(df$q6_9, df$q6_11)
  
  ## ## typeEmployeur <- c(
  ## ##   "vous-même",
  ## ##   "la fonction publique\n (d'Etat, territoriale ou hospitalière)",
  ## ##   "une entreprise publique",
  ## ##   "une entreprise privée",
  ## ##   "un organisme à but non lucratif ou\n une association",
  ## ##   "une personne exerçant une profession libérale ou\n un indépendant (cabinet, étude notariale…)",
  ## ##   "un particulier"
  ## ## )
  ## ## ColToFactor("q6_12", "typeEmployeur", typeEmployeur)

  ## ## Employeurs privés : cette catégorie regroupe les entreprises (privées et publiques), les indépendant.e.s et les professions libérales 
  res$typeEmployeur <- factor(df$Eq.COD.q6_12)
  levels(res$typeEmployeur) <- list('Employeurs privés'=c(1,3,4,6,7), 'Fonction Publique'=2, 'Associations'=5)


  activiteEcoEmployeur <- c( 
    "Agriculture, sylviculture et pêche",
    "Industries (manufacturières, extractives et autres)",
    "Construction",
    "Commerce, transports, hébergement et restauration",
    "Information et communication",
    "Activités financières et d'assurance",
    "Activités spécialisées, scientifiques et techniques",
    "Activités de services administratifs et de soutien",
    "Enseignement",
    "Administration publique (hors enseignement)",
    "Santé humaine et action sociale",
    "Arts, spectacles et activités récréatives",
    "Autres activités de service"
  )
  ColToFactor("Eq.q6_13", "activiteEcoEmployeur", activiteEcoEmployeur)

  res$intituleEmploi <- as.character(df$q6_4)
  
  return(res)
}

GenerateShinyRawDb <- function( filename = file.path("data", "raw_data.csv")) {
  data <- ReadIP(filename)
  saveRDS(data, file.path("data", "all-uns-insertion_professionnelle.rda"))
}

ReadMIN <- function(filename) {
  ## Read a min database and preprocess the data.
   df <-  df <- read.table(file = filename, header=TRUE, row.names=NULL, sep=';', quote="", na.strings=c("", NA, "ns", "nd"))
   df$Nombre.de.diplômés <- round(df$Nombre.de.réponses * 100 /  df$Taux.de.réponse)
   df[,"Abbrev.de.la.discipline"] <- factor(
     df[, "Code.de.la.discipline"],
     levels =  c("disc01", "disc02", "disc03", "disc04", "disc05",
                 "disc06", "disc07", "disc08", "disc09", "disc10",
                 "disc11", "disc12", "disc13", "disc14", "disc15",
                 "disc16", "disc17", "disc18", "disc19", "disc20"),
     labels =   c("Ens. DEG", "Droit", "Éco", "Gestion", "Autres DEG",
                  "LLA", "Ens. SHS", "Histoire Géo", "Psycho", "Info. Com.",
                  "Autres SHS", "Ens. STS", "SVT", "Sc. Fonda", "Sc. Ing.",
                  "Info", "Autres STS", "MENS", "MENS 1", "MENS 2")
   )
   return(df)
}

GenerateShinyMinDb <- function(filename) {
  data <- ReadMIN(filename)
  saveRDS(data, sub(".[^.]*$", ".rda", filename))
}

