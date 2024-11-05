# Charger les bibliothèques nécessaires
library(dplyr)        # Manipulation de données
library(tidyr)        # Manipulation de données
library(ggplot2)      # Visualisation de données
library(ggpubr)       # Visualisation avancée
library(shiny)        # Création d'applications interactives
library(shinythemes)  # Thèmes pour Shiny
library(e1071)        # Pour utiliser le modèle SVM pour la classification
library(DT)           # Pour afficher les données sous forme de table interactive


# Fixer une limite de taille pour les fichiers téléversés par l'utilisateur
options(shiny.maxRequestSize = 50*1024^2)  # Limite à 50 Mo

# Charger les données de référence RNASeq et le modèle SVM pré-entraîné
ref_data <- readRDS("data/2_Data_RNASeq_Cannabis_Sex.rds")  # Données d'expression génique
svm_model <- readRDS("model/svm_model.RDS")  # Modèle SVM pour prédire le sexe des plantes



# Fonction pour préparer et transformer les données de référence
prepare_reference_data <- function(data) {
  # Renommer les identifiants de gènes pour les rendre plus compréhensibles
  data$X <- gsub("LOC115699937", "REM16", data$X)
  data$X <- gsub("LOC115696989", "FT1", data$X)
  
  # Extraction et transformation des données d'expression pour chaque gène en format long
  gene1_data <- data %>% filter(X == "REM16") %>% select(-X)
  gene1_long <- data.frame(t(gene1_data))
  colnames(gene1_long) <- "expression"
  gene1_long$sample <- rownames(gene1_long)
  gene1_long$sex <- substr(gene1_long$sample, nchar(gene1_long$sample) - 1, nchar(gene1_long$sample))
  gene1_long$gene <- "REM16"
  
  gene2_data <- data %>% filter(X == "FT1") %>% select(-X)
  gene2_long <- data.frame(t(gene2_data))
  colnames(gene2_long) <- "expression"
  gene2_long$sample <- rownames(gene2_long)
  gene2_long$sex <- substr(gene2_long$sample, nchar(gene2_long$sample) - 1, nchar(gene2_long$sample))
  gene2_long$gene <- "FT1"
  
  # Combiner les données des deux gènes dans une seule liste pour un accès simplifié
  combined_data <- bind_rows(gene1_long, gene2_long)
  list(rem16 = gene1_long, ft1 = gene2_long, combined = combined_data)
}

# Préparer les données de référence à l'avance
ref_data_list <- prepare_reference_data(ref_data)


ui <- navbarPage(
  # Ajoutez le lien Font Awesome, le JavaScript et le CSS personnalisé
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"),
    tags$script(HTML("
      $(document).on('shiny:inputchanged', function(event) {
        if (event.name === 'user_data') {
          setTimeout(function() {
            $('div.progress-bar').text('Téléchargement terminé');
          }, 10);
        }
      });
    ")),
    tags$style(HTML("
      /* Style pour les onglets non sélectionnés */
      .navbar-nav > li > a {
        color: #666666 !important;  /* Gris clair pour les onglets non sélectionnés */
        font-weight: normal !important;
      }
      
      /* Style pour l'onglet sélectionné */
      .navbar-nav > li.active > a {
        color: #333333 !important;  /* Plus foncé pour l'onglet sélectionné */
        font-weight: bold !important;
      }
    "))
  ),
  
  title = div(
    "SexCannalyzer",
    style = "font-size: 20px; font-weight: bold; color: #333;"
  ),
  theme = shinytheme("lumen"),
  
  # Onglet pour visualiser les graphiques avec icône de graphique
  tabPanel(
    HTML("<i class='fas fa-chart-line'></i> Graphiques des résultats"),
    sidebarLayout(
      sidebarPanel(
        fileInput("user_data", "Télécharger un fichier CSV pour les individus à analyser", accept = ".csv", 
                  buttonLabel = "Parcourir", placeholder = "Aucun fichier sélectionné"),
        textInput("user_gene1_id", "ID du gène pour REM16", value = "LOC115699937"),
        textInput("user_gene2_id", "ID du gène pour FT1", value = "LOC115696989"),
        actionButton("analyze", "Analyser"),
        hr(),
        selectInput("column_selector", "Sélectionner un individu (colonne) :", choices = NULL),
        textInput("search_column", "Ou entrer un nom d'individu (colonne) :"),
        actionButton("search_column_button", "Chercher"),
        width = 3
      ),
      mainPanel(
        fluidRow(
          column(3, offset = 9, selectInput("graph_choice", "Choisir le graphique :", choices = c("REM16", "FT1", "REM16+FT1"), selected = "REM16"))
        ),
        plotOutput("expressionPlot", height = "600px")
      )
    )
  ),
  
  # Onglet pour afficher les résultats dans un tableau interactif avec icône de tableau
  tabPanel(
    HTML("<i class='fas fa-table'></i> Tableau des résultats"),
    DT::dataTableOutput("result_table")
  )
)




# Serveur de l'application
server <- function(input, output, session) {
  
  # Fonction pour charger et préparer les données utilisateur en réponse à un clic sur le bouton "analyze"
  user_data <- eventReactive(input$analyze, {
    # Vérifier que l'utilisateur a bien téléversé un fichier
    req(input$user_data)
    # Lire le fichier CSV téléversé
    user_df <- read.csv(input$user_data$datapath)
    # Remplacer les identifiants de gènes par des noms plus lisibles ("REM16" et "FT1")
    user_df$X <- gsub(input$user_gene1_id, "REM16", user_df$X)
    user_df$X <- gsub(input$user_gene2_id, "FT1", user_df$X)
    # Mettre à jour la liste des choix dans le menu déroulant pour sélectionner une colonne spécifique
    updateSelectInput(session, "column_selector", choices = colnames(user_df)[-1], selected = colnames(user_df)[2])
    return(user_df)
  })
  
  # Fonction réactive pour obtenir la colonne sélectionnée par l'utilisateur dans le menu déroulant
  selected_column <- reactive({
    # Vérifier que les données utilisateur sont chargées
    req(user_data())
    # Renvoyer la colonne actuellement sélectionnée
    input$column_selector
  })
  
  # Observer l'événement de recherche de colonne pour permettre à l'utilisateur de trouver et de sélectionner une colonne spécifique
  observeEvent(input$search_column_button, {
    # Vérifier que les données utilisateur sont chargées
    req(user_data())
    # Obtenir les noms des colonnes, en excluant la première colonne (identifiant des gènes)
    col_names <- colnames(user_data())[-1]
    # Convertir le nom de colonne recherché en minuscule pour éviter les problèmes de casse
    search_name <- tolower(input$search_column)
    # Trouver la colonne correspondant exactement au nom saisi par l'utilisateur
    match <- col_names[which(tolower(col_names) == search_name)]
    
    if (length(match) == 1) {
      # Si une correspondance exacte est trouvée, mettre à jour le menu déroulant pour sélectionner cette colonne
      updateSelectInput(session, "column_selector", selected = match)
    } else {
      # Afficher un message d'erreur si aucune correspondance n'est trouvée
      showModal(modalDialog(
        title = "Erreur",
        "Le nom de colonne saisi n'est pas dans la liste.",
        easyClose = TRUE,
        footer = modalButton("Fermer")
      ))
    }
  })
  
  # Générer le graphique d'expression génique en fonction du choix de l'utilisateur
  output$expressionPlot <- renderPlot({
    # Vérifier que les données utilisateur et la colonne sélectionnée sont disponibles
    req(user_data(), selected_column())
    user_column <- selected_column()
    
    # Choix du graphique en fonction de l'option sélectionnée par l'utilisateur
    if (input$graph_choice == "REM16") {
      # Graphique pour le gène REM16
      user_rem16 <- user_data() %>% filter(X == "REM16")  # Filtrer les données pour le gène REM16
      user_expression_rem16 <- as.numeric(user_rem16[[user_column]])  # Extraire les valeurs d'expression pour la colonne sélectionnée
      
      # Génération du graphique pour REM16
      ggplot(ref_data_list$rem16, aes(x = sex, y = expression, color = sex)) +
        geom_boxplot(outlier.shape = NA) +  # Ajouter un boxplot sans les points atypiques
        labs(
          title = paste("Expression du gène REM16 chez ", user_column, "et données de référence"),
          x = "Individu",
          y = "Niveau d'expression"
        ) +
        theme_minimal() +
        theme(
          legend.position = "none",
          plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)
        ) +
        scale_x_discrete(labels = c(user_column, "Ref. XX", "Ref. XY")) +
        geom_point(aes(x = user_column, y = user_expression_rem16), color = "black", size = 3)  # Point utilisateur en noir
      
    } else if (input$graph_choice == "FT1") {
      # Graphique pour le gène FT1
      user_ft1 <- user_data() %>% filter(X == "FT1")  # Filtrer les données pour le gène FT1
      user_expression_ft1 <- as.numeric(user_ft1[[user_column]])  # Extraire les valeurs d'expression pour la colonne sélectionnée
      
      # Génération du graphique pour FT1
      ggplot(ref_data_list$ft1, aes(x = sex, y = expression, color = sex)) +
        geom_boxplot(outlier.shape = NA) +
        labs(
          title = paste("Expression du gène FT1 chez ", user_column, "et données de référence"),
          x = "Individu",
          y = "Niveau d'expression"
        ) +
        theme_minimal() +
        theme(
          legend.position = "none",
          plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)
        ) +
        scale_x_discrete(labels = c(user_column, "Ref. XX", "Ref. XY")) +
        geom_point(aes(x = user_column, y = user_expression_ft1), color = "black", size = 3)  # Point utilisateur en noir
    } else {
      # Graphique combiné pour les gènes REM16 et FT1
      combined_data <- ref_data_list$combined  # Récupérer les données combinées pour REM16 et FT1
      user_rem16 <- user_data() %>% filter(X == "REM16")  # Filtrer les données pour REM16
      user_ft1 <- user_data() %>% filter(X == "FT1")  # Filtrer les données pour FT1
      
      # Combiner les données utilisateur pour les deux gènes en un seul dataframe avec des étiquettes distinctes
      user_data_combined <- data.frame(
        sex_gene = c("Individu.REM16", "Individu.FT1"),  # REM16 avant FT1
        expression = c(as.numeric(user_rem16[[user_column]]), as.numeric(user_ft1[[user_column]])),
        gene = c("REM16", "FT1")
      )
      
      # Forcer l'ordre des niveaux avec REM16 avant FT1
      combined_data$sex_gene <- factor(
        interaction(combined_data$sex, combined_data$gene),
        levels = c("Individu.REM16", "Individu.FT1", "XX.REM16", "XY.REM16", "XX.FT1", "XY.FT1")
      )
      user_data_combined$sex_gene <- factor(
        user_data_combined$sex_gene,
        levels = c("Individu.REM16", "Individu.FT1", "XX.REM16", "XY.REM16", "XX.FT1", "XY.FT1")
      )
      
      # Labels personnalisés pour chaque catégorie, avec le vrai nom de colonne pour l'individu étudié
      x_labels <- c(user_column, user_column, "Ref.XX", "Ref.XY", "Ref.XX", "Ref.XY")
      
      # Génération du graphique combiné pour REM16 et FT1
      ggplot(combined_data, aes(x = sex_gene, y = expression, color = gene)) +
        geom_boxplot(outlier.shape = NA) +  # Ajouter un boxplot sans les points atypiques
        labs(title = "Niveau d'expression des gènes REM16 et FT1 en fonction du sexe", x = "Individu", y = "Niveau d'expression") +
        theme_minimal() +
        theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 12)) +  # Appliquer un thème minimal pour une meilleure lisibilité
        geom_point(data = user_data_combined, aes(x = sex_gene, y = expression, color = gene), size = 3) +  # Ajouter les points utilisateur pour chaque gène avec la couleur appropriée
        scale_x_discrete(limits = levels(user_data_combined$sex_gene), labels = x_labels)  # Utiliser limits pour forcer l'ordre avec le vrai nom de colonne
    }
  })
  
  
  # Prédire le sexe des plantes avec le modèle SVM et afficher les résultats dans un tableau
  output$result_table <- DT::renderDataTable({
    
    req(user_data()) # Vérifier que les données utilisateur sont chargées
    
    # Extraire les données d'expression pour REM16 et les organiser dans un format approprié
    rem16_values <- user_data() %>% filter(X == "REM16") %>% select(-X) %>% t() %>% as.data.frame()
    colnames(rem16_values) <- "REM16"
    rem16_values$Sample <- rownames(rem16_values)
    
    # Extraire les données d'expression pour FT1 et les organiser
    ft1_values <- user_data() %>% filter(X == "FT1") %>% select(-X) %>% t() %>% as.data.frame()
    colnames(ft1_values) <- "FT1"
    ft1_values$Sample <- rownames(ft1_values)
    
    # Combiner les deux jeux de données par échantillon (Sample)
    prediction_data <- merge(rem16_values, ft1_values, by = "Sample")
    
    # Convertir les colonnes d'expression en numérique pour le modèle SVM
    prediction_data$REM16 <- as.numeric(prediction_data$REM16)
    prediction_data$FT1 <- as.numeric(prediction_data$FT1)
    
    # Utiliser le modèle SVM pour prédire le sexe des plantes en fonction des niveaux d'expression des gènes REM16 et FT1
    prediction_data$Sexe <- predict(svm_model, prediction_data[, c("REM16", "FT1")])
    
    # Afficher les résultats finaux dans un tableau (échantillon et sexe prédit)
    prediction_data %>% select(Sample, Sexe)
  })
}
  


# Lancer l'application Shiny
shinyApp(ui, server)



