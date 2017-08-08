ReadIP <- function(file) {
  ## Read the IP database and preprocess the data.
  ## Columns must be given as MNESR codes.
  ## Visualization uses mostly columns created by this function.
  ## The survey model and charts model are separated.
  df <- read.csv(file = file, row.names = NULL, na.strings = c("NA", ""))
  ##res <- data.frame(matrix(, nrow=nrow(df), ncol=0))
  ## TODO Add attribute_27_dom_mention_SISE_BO instead attribute_6_diplôme_lib_BO
  res <- df[,c("annee", "Diplôme", "code_dip")] ## "attribute_5_specialite_SISE_BO", "attribute_6_diplôme_lib_BO", "attribute_25_composante_lib_BO")]
  
  colnames(res) <- c("annee", "libdip1", "code_diplome") ##, "libdip3", "libdip2", "composante_lib_BO")
  res$libdom  <- factor(gsub('[[:blank:]]*-.*$','', df$attribute_27_dom_mention_SISE_BO)) ## domaine
  res$libdip2 <- factor(gsub('^[^-]*-[[:blank:]]','', df$attribute_27_dom_mention_SISE_BO)) ## mention
  res$libdip3 <- df$attribute_5_specialite_SISE_BO ## spécialité
  ColToFactor <- function(fromCol, toCol, labels, levels = seq_along(labels)) {
    res[,toCol] <<- factor(df[, fromCol], levels = levels, labels = labels)
  }

  ColToFactor("attribute_10_sexe_BO", "sexe", c("Femme", "Homme"), c("F", "M"))
  ## Detailed classification

  bacs <- read.csv(file = file.path("data", "baccalaureats.csv"), na.strings = c("NA", "N/A", ""))
  bacs <- aggregate(bacs$bac, by = list(bacs$categorie), paste)
  x <- bacs[,2]
  names(x) <- bacs[,1]
  bacs <- x
  res$serieBac <- df$attribute_34_code_bac_BO
  levels(res$serieBac) <- bacs
  
  GetRegion <- function(x) {
    y <- c(rep(3, 98), 4)
    y[6] <- 1
    y[c(4,5,13,83,84)] <- 2
    z <- factor(y[x], levels = 1:4, labels = c("Alpes-Maritimes", "PACA hors\nAlpes-Maritimes", "Hors PACA", "Étranger"))
  }
  res$regionEmploi <- GetRegion(df$q6_14a)

  ##res$regionBac <- GetRegion(df$region_bac)
  res$regionBac <- df$region_obtention_bac
  
  res$repondant <- df$statut_reponse %in% 4:6

  ## Rename 
  res$employe <- res$repondant & df$q4_3 == 1

  
  res$intituleEmploi <- as.character(df$q6_4)
  res$salaireEmploiN30 <- as.numeric(df$q6_9)
  res$salaireEmploiN18 <- as.numeric(df$q8_5)
  

  ## TODO Trouver la formule pour le taux d'insertion
  ## Le taux d’insertion est défini comme le taux net d’emploi c’est-à-dire la part des diplômés occupant un emploi, quel qu’il soit,
  ## sur l’ensemble des diplômés présents sur le marché du travail (en emploi ou au chômage).
  res$insertionN30 <- df$q4_3 == 1
  res$tempsPleinN30 <- df$q6_7 == 1
  
  res$insertionN18 <- df$q7_1 == 1
  res$tempsPleinN18 <- df$q8_3 == 1
  
  ColToFactor(
    "statut_reponse", "statutReponse",
    c(
      "Deuxième diplôme dans le cas d'un double diplôme",
      "Décédé",
      "Erreur sur le diplôme :\n diplômé n’ayant pas validé le diplôme",
      "téléphone",
      "internet",
      "papier",
      "Diplômé issu d'une formation délocalisée\n(pour les diplômés étrangers uniquement)",
      "Autre"
    )
  )

  ##   ColToFactor(
  ##   "statut_reponse", "statutReponse",
  ##   c(
  ##     "Deuxième diplôme dans le cas d'un double diplôme",
  ##     "Décédé",
  ##     "Erreur sur le diplôme :\n diplômé n’ayant pas validé le diplôme",
  ##     "téléphone",
  ##     "internet",
  ##     "papier",
  ##     "Diplômé issu d'une formation délocalisée\n(pour les diplômés étrangers uniquement)",
  ##     "Autre"
  ##   )
  ## )
   ColToFactor(
    "statut_reponse", "statutReponse",
    c(
      "Deuxième diplôme (double diplôme)",
      "Décédé",
      "Erreur : non validé",
      "téléphone",
      "internet",
      "papier",
      "Formation délocalisée",
      "Autre"
    )
  )
  ## TODO add parameter
  ## ColToFactor("q2_2", "boursier", c("Oui sur critères sociaux", "Oui sur d’autres critères", "Non"))
  res$boursier <- as.factor(df$q2_2)
  levels(res$boursier) <- list(Boursier=1:2, Non=3)
  
  poursuiteEtude <- c(
    "En doctorat (Master) / en Master (LP)",
    "Dans une autre formation",
    "Non"
  )
  
  ColToFactor("q3_1_1", "etudeN6", poursuiteEtude)
  ColToFactor("q3_1_2", "etudeN18", poursuiteEtude)
  ColToFactor("q4_1", "etudeN30", poursuiteEtude)
  ## poursuivent des études dans les deux ans.
  res$poursuiteEtude <- (df$q3_1_1 != 3) | (df$q3_1_2 != 3)

  ## situationPro <- c(
  ##   "Vous avez un emploi ",
  ##   "Vous n’avez pas d’emploi et\n vous recherchez du travail ou\n vous êtes en attente d’un contrat",
  ##   "Vous n’avez pas d’emploi et\n vous ne cherchez pas de travail"
  ## )
  situationPro <- c(
    "En emploi ",
    "En recherche d'emploi",
    "Ne recherche pas d'emploi"
  )
  ColToFactor("q4_3", "situationProN30", situationPro)
  ColToFactor("q7_1", "situationProN18", situationPro)

  
  ## statutEmploi <- c( 
  ##   "Prof. libérale, indépendant,\n chef d’entreprise, auto-entrepreneur",
  ##   "Fonctionnaire\n(y compris fonctionnaire stagiaire ou élève fonctionnaire)",
  ##   "CDI",
  ##   "Contrat spécifique au doctorat\n(contrat doctoral, allocation recherche, CIFRE….)",
  ##   "CDD (hors contrat spécifique au doctorat et \ny compris contractuel de la fonction publique...)",
  ##   "Vacataire",
  ##   "Intérimaire",
  ##   "Intermittent du spectacle, pigiste",
  ##   "Contrat d’apprentissage",
  ##   "Contrat de professionnalisation",
  ##   "Emplois aidés (Contrat Initiative Emploi…)",
  ##   "Volontariat international",
  ##   "Service civique")
  statutEmploi <- c( 
    "Prof. libérale, indépendant,\n chef d’entreprise, auto-entrepreneur",
    "Fonctionnaire",
    "CDI",
    "Contrat spécifique au doctorat",
    "CDD",
    "Vacataire",
    "Intérimaire",
    "Intermittent du spectacle, pigiste",
    "Contrat d’apprentissage",
    "Contrat de professionnalisation",
    "Emplois aidés",
    "Volontariat international",
    "Service civique")

  ColToFactor("q6_5", "statutEmploiN30", statutEmploi)
  ColToFactor("q8_1", "statutEmploiN18", statutEmploi)

  ## L'emploi stable correspond à la part des diplômés en emploi sous contrat de CDI, sous statut de la Fonction publique ou en qualité de travailleur indépendant.
  res$emploiStableN30 <- df$q6_5 <= 3
  res$emploiStableN18 <- df$q8_1 <= 3

 
  ## niveauEmploi <- c(
  ##   "personnel de catégorie A de la fonction publique",
  ##   "ingénieur, cadre, professions libérales, professions intellectuelles supérieures",
  ##   "personnel de catégorie B de la fonction publique",
  ##   "emploi de niveau intermédiaire : technicien, agent de maîtrise, maîtrise administrative et commerciale, VRP",
  ##   "personnel de catégorie C de la fonction publique",
  ##   "manœuvre, ouvrier",
  ##   "employé de bureau, de commerce, personnel de service"
  ## )
  ## ColToFactor("q8_2r", "niveauEmploiN18", niveauEmploi)
  ## ColToFactor("q6_6r", "niveauEmploiN30", niveauEmploi)

  res$niveauEmploiN18 <- factor(df$q8_2r)
  levels(res$niveauEmploiN18) <- list('ingénieur ou cadre /cat. A'=1:2, 'technicien ou agent de maîtrise / cat. B'=3:4, 'ouvrier ou employé / cat. C'=5:7)

  res$niveauEmploiN30 <- factor(df$q6_6r)
  levels(res$niveauEmploiN30) <- list('ingénieur ou cadre /cat. A'=1:2, 'technicien ou agent de maîtrise / cat. B'=3:4, 'ouvrier ou employé / cat. C'=5:7)
  
  ## https://fr.wikipedia.org/wiki/Professions_et_cat%C3%A9gories_socioprofessionnelles_en_France
  res$emploiSupIntN30 <- df$q6_6r <= 4
  res$emploiSupIntN18 <- df$q8_2r <= 4

  ##TODO Create a second column
  ## typeEmployeur <- c(
  ##   "vous-même",
  ##   "la fonction publique\n (d'Etat, territoriale ou hospitalière)",
  ##   "une entreprise publique",
  ##   "une entreprise privée",
  ##   "un organisme à but non lucratif ou\n une association",
  ##   "une personne exerçant une profession libérale ou\n un indépendant (cabinet, étude notariale…)",
  ##   "un particulier"
  ## )
  ## ColToFactor("q6_12", "typeEmployeur", typeEmployeur)
  
  ## Employeurs privés : cette catégorie regroupe les entreprises (privées et publiques), les indépendant.e.s et les professions libérales 
  res$typeEmployeur <- factor(df$q6_12)
  levels(res$typeEmployeur) <- list('Employeurs privés'=c(1,3,4,6,7), 'Fonction Publique'=2, 'Associations'=5)


  activiteEcoEmployeur <- c( 
    "Agriculture, sylviculture et pêche",
    "Industries (manufacturières, extractives et autres)",
    "Construction",
    "Commerce, transports, hébergement et restauration",
    "Information et communication",
    "Activités financières et d’assurance",
    "Activités spécialisées, scientifiques et techniques",
    "Activités de services administratifs et de soutien",
    "Enseignement",
    "Administration publique (hors enseignement)",
    "Santé humaine et action sociale",
    "Arts, spectacles et activités récréatives",
    "Autres activités de service"
  )
  ColToFactor("q6_13", "activiteEcoEmployeur", activiteEcoEmployeur)
  
  return(res)
}

GenerateShinyRawDb <- function( file = file.path("data", "raw_data.csv")) {
  data <- ReadIP(file.path("data", "raw_data.csv"))
  write.csv(data, file.path("data", "all-uns-insertion_professionnelle.csv"), row.names=FALSE)
}
