library(shiny)
library(colourpicker)
library(shinydashboard)
library(rAmCharts)
library(DT)
library(tidyverse)
library(leaflet)
library(viridis)
library(corrplot)
library(stargazer)
library(plotly)
library(hrbrthemes)
library(car)
library(FactoMineR)
library(lmtest)

shinyServer(function(input, output, session) {
  
  # Imporation jeux de donnees
  # Nettoyage + fusion 
  
  # Ouverture premiere BDD relative à l'année 2021
  donnees_2021 <- read.csv("world-happiness-report-2021.csv")
  donnees_2021 <- donnees_2021[,c(1:3,7:12)]
  colnames(donnees_2021) <- c("Pays","Region","Indice_du_bonheur","log_PIB_par_hab","Soutien_social","Esperance_de_vie_en_bonne_sante","Liberte_de_faire_des_choix","Generosite","Perception_de_corruption")
  donnees_2021$Annees <- 2021
  
  # Ouverture de la deuxième BDD (années antérieures à 2021)
  donnees_annees <- read.csv("world-happiness-report.csv")
  donnees_annees <- donnees_annees[,1:9]
  colnames(donnees_annees) <- c("Pays","Annees","Indice_du_bonheur","log_PIB_par_hab","Soutien_social","Esperance_de_vie_en_bonne_sante","Liberte_de_faire_des_choix","Generosite","Perception_de_corruption")
  
  # Ajout de la colonne Region à donnees_annees
  tmp <- merge(data.frame(Pays = donnees_2021$Pays, Region = donnees_2021$Region), 
               data.frame(Pays = donnees_annees$Pays),
               all.y = TRUE)
  donnees_annees$Region <- tmp$Region
  donnees <- rbind(donnees_2021, donnees_annees)
  
  # Ajout zone géographique pour les pays n'en n'ayant pas
  donnees$Region[donnees$Pays == "Angola"] <- "Sub-Saharan Africa"                
  donnees$Region[donnees$Pays == "Belize"] <- "Latin America and Caribbean"
  donnees$Region[donnees$Pays == "Bhutan"] <- "South Asia"
  donnees$Region[donnees$Pays == "Central African Republic"] <- "Sub-Saharan Africa"                
  donnees$Region[donnees$Pays == "Congo (Kinshasa)"] <- "Sub-Saharan Africa" 
  donnees$Region[donnees$Pays == "Cuba"] <- "Latin America and Caribbean"
  donnees$Region[donnees$Pays == "Djibouti"] <- "Sub-Saharan Africa"                
  donnees$Region[donnees$Pays == "Guyana"] <- "Latin America and Caribbean"
  donnees$Region[donnees$Pays == "Oman"] <- "Middle East and North Africa"
  donnees$Region[donnees$Pays == "Qatar"] <- "Middle East and North Africa"                
  donnees$Region[donnees$Pays == "Somalia"] <- "Middle East and North Africa" #ou Afrique
  donnees$Region[donnees$Pays == "Somaliland region"] <- "Middle East and North Africa" #ou Afrique
  donnees$Region[donnees$Pays == "South Sudan"] <- "Sub-Saharan Africa"                
  donnees$Region[donnees$Pays == "Sudan"] <- "Sub-Saharan Africa"
  donnees$Region[donnees$Pays == "Suriname"] <- "Latin America and Caribbean"
  donnees$Region[donnees$Pays == "Syria"] <- "Middle East and North Africa"                
  donnees$Region[donnees$Pays == "Trinidad and Tobago"] <- "Latin America and Caribbean"
  
  
  # Données carte
  # On télécharge le nom des pays associé à leur code ISO (3 lettres)
  df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')
  # On supprime la colonne GDP qui était un exemple
  df <- df[,-2]
  
  
  ##########################
  #Parties de l'application#
  ##########################
  
  
  # Onglet data
  
  # Affichage de la table
  output$table <- renderDT({
    
    datatable(donnees, options = list(
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    ))
  })
  
  # Affichage du résumé
  output$summary <- renderPrint({
    summary(donnees)
  })
  
  
  
  
  # Onglet Visualisation
  
  # Sous partie visualisation
  
  # nombre de classes
  output$n_bins <- renderText({
    input$go_graph
    isolate({
      paste("Nombre de classes : ", input$bins)
    })
  })
  
  observeEvent(input$go_graph,{
    updateTabsetPanel(session,inputId = "viz",selected="Histogramme")
  })
  
  # Histogramme de la variable sélectionnée + choix année
  # Graphique "interactif"
  # Titre du graphique modifiable par l'utilisateur + choix de la couleur + choix du nbre de bins
  output$distPlot <- renderPlotly({
    input$go_graph
    isolate({
      # selection du bon jeu de donnees avec les filtres de l'utilisateur
      df <- data.frame(x=donnees[donnees$Annees == input$choix_annee_hist, input$choix_colonne])
      
      # creation de l'histogramme
      pl <- ggplot(data = df) +
        aes(x = x, y = ..density..) +
        geom_histogram(bins=input$bins, fill=input$color) +
        theme(legend.position="none") + 
        ggtitle(input$titre) +
        labs(x = paste(gsub("_", " ", input$choix_colonne))) + 
        theme(plot.title = element_text(size=14, face="bold", hjust=0.5))+
        geom_vline(aes(xintercept = mean(df$x, na.rm=TRUE)), colour="green", linetype = "longdash")
      
      # transformation en interactif
      ggplotly(pl)%>%
        highlight("plotly_selected")
      })
  })
  
  # Boxplot de la variable + choix année
  output$distPlot1 <- renderPlotly({
    df <- data.frame(x=donnees[donnees$Annees == input$choix_annee_hist, input$choix_colonne])
    
    pl <- ggplot(data = df) +
      aes(x = colnames(df), y = x) +
      geom_boxplot(fill=input$color) +
      ggtitle(paste("Boxplot", gsub("_", " ", input$choix_colonne))) +
      theme(plot.title = element_text(size=14, face="bold", hjust=0.5),
            axis.title.x = element_blank())
    
    ggplotly(pl)%>%
      highlight("plotly_selected")
  })
  
  # sous partie ACP
  
  # Creation du graphique des individus et des variables en fonction de l'année sélectionnée.
  output$acpind <- renderPlot({
    donnees <- donnees[donnees$Annees == input$choix_annee_ACP,3:9]
    plot(PCA(donnees),choix="ind")
  })
  
  output$acpvar <- renderPlot({
    donnees <- donnees[donnees$Annees == input$choix_annee_ACP,3:9]
    PCA(donnees)
  })
  
  
  
  # Onglet Comparaisons temporelles
  
  # sous partie : Entre régions
  
  output$comp_regions <- renderPlotly({
    # Calculons la moyenne d'une variable numérique par région
    moy_region <- aggregate(donnees[,input$choix_colonne_ST], list(donnees$Region,donnees$Annees), mean, na.action = na.omit)
    colnames(moy_region) <- c("Region", "Annees", "Moyenne")
    
    une_region <- moy_region[moy_region$Region %in% input$choix_zone_comparaison,]
    
    # On représente l'évolution de cette variable par année en comparant les continents
    p <- ggplot(une_region,
                aes(Annees, Moyenne, 
                    group = Region, 
                    color = factor(Region))) +
      geom_line() +
      geom_point() +
      ggtitle(paste("Comparaison de l'évolution de", gsub("_", " ", input$choix_colonne_ST), "entre les régions")) +
      labs(x = "Année", y = paste("Moyenne", gsub("_", " ", input$choix_colonne_ST))) +
      theme_bw() +
      theme(plot.title = element_text(size=15, face="bold", hjust = 0.5),
            legend.position = "bottom",
            legend.title=element_blank()) 
    
    ggplotly(p, source = une_region$Region)%>%
      highlight("plotly_selected") 
  })
  
  # sous partie : Entre pays
  
  output$comp_pays <- renderPlotly({
    
    donnees_pays = donnees[donnees$Pays %in% c(input$choix_pays1, input$choix_pays2),c(input$choix_colonne_comparaison,"Pays","Annees")]
    
    p <- ggplot(donnees_pays)+
      aes(x = Annees, y = donnees_pays[,1], col = Pays) +
      geom_line() +
      geom_point() +
      xlab("Année") +
      ylab(paste(gsub("_", " ", input$choix_colonne_comparaison))) +
      theme_bw() +
      ggtitle(paste("Evolution de",gsub("_", " ", input$choix_colonne_comparaison), 'entre', input$choix_pays1, 'et', input$choix_pays2)) +
      theme(plot.title = element_text(size=15, face="bold", hjust = 0.5))
    
    ggplotly(p, source = donnees_pays$Pays)%>%
      highlight("plotly_selected")
  })
  

  
  # Onglet cartographie
  
  # carte
  output$map = renderPlotly({
    input$go_graph_carte
    isolate({
      # On choisit l'année
      donnees_carte <- filter(donnees, Annees == input$choix_annees)
      # On choisit la variable que l'on souhaite représenter
      variable <- input$choix_colonne_carte
      index <- which(colnames(donnees_carte) == variable)
      # On ajoute la colonne que l'on souhaite représenter
      df <- full_join(data.frame(COUNTRY = donnees_carte$Pays, Var_num = donnees_carte[,index]), df, by="COUNTRY")
      # Frontières gris clair
      l <- list(color = toRGB("grey"), width = 0.5)
      # Spécifier la projection/options de la carte
      g <- list(
        scope = "world",
        showland = TRUE,
        landcolor = toRGB("gray85"),
        showocean = TRUE,
        oceancolor = "#a5c7ff",
        showframe = FALSE,
        showcoastlines = FALSE, 
        projection = list(type = 'Mercator')
      )
      fig <- plot_geo(df)
      fig <- fig %>% add_trace(
        z = ~Var_num, color = ~Var_num, colors = 'YlOrRd',
        text = ~COUNTRY, locations = ~CODE, marker = list(line = l)
      )
      fig <- fig %>% colorbar(title = gsub("_", " ", input$choix_colonne_carte), tickprefix = '')
      fig <- fig %>% layout(
        title = paste(gsub("_", " ", input$choix_colonne_carte), 'en', input$choix_annees),
        geo = g
      )
      fig
    })
  })
  
  
  # Onglet Modeles
  
  #correlogramme
  output$correlogramme <- renderPlot({
    input$zone
    donnees <- donnees %>%
      filter(Annees == input$choix_annee_cor & Region %in% input$choix_zone_cor) %>%
      drop_na()
    matrixcorr<- cbind(donnees$Indice_du_bonheur, donnees$log_PIB_par_hab, donnees$Soutien_social, donnees$Esperance_de_vie_en_bonne_sante,
                       donnees$Liberte_de_faire_des_choix,donnees$Generosite, donnees$Perception_de_corruption)
    colnames(matrixcorr) <-c("IDB","Log PIB/h", "SS","EDV",
                             "LDC", "G", "PDC")
    mcor<-cor(matrixcorr)
    corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45, title = "Correlogramme de nos variables", mar = c(0,0,1,0))
  })
  
  # Regression simple
  
  # Graphique / nuage de points
  output$reg_2var_graph <- renderPlotly({
    donnees <- donnees[donnees$Annees == input$choix_annee_reg_2var & donnees$Region %in% input$choix_zone_reg_2var,c(input$choix_colonne_reg_2var,"Indice_du_bonheur","Pays","Region")]  
    donnees <- donnees %>% drop_na()
    pl <- ggplot(data = donnees) +
      aes(x = donnees[,1], y = Indice_du_bonheur) +
      geom_point(aes(text = donnees$Pays)) +
      geom_smooth(method=lm) +
      xlab(paste(gsub("_", " ", input$choix_colonne_reg_2var), 'en', input$choix_annee_reg_2var)) +
      ylab("Indice du bonheur") +
      ggtitle(paste("Indice du bonheur en fonction de",gsub("_", " ", input$choix_colonne_reg_2var), 'en', input$choix_annee_reg_2var)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(pl, source = donnees$Pays)%>%
      highlight("plotly_selected")
  })
  
  # resume de la regression simple
  output$reg_2var_summary <- renderPrint({
    donnees <- donnees[donnees$Annees == input$choix_annee_reg_2var & donnees$Region %in% input$choix_zone_reg_2var,c(input$choix_colonne_reg_2var,"Indice_du_bonheur")] 
    donnees <- donnees %>% drop_na()
    reg_2var <- lm(Indice_du_bonheur~., data = donnees)
    summary(reg_2var)
    
    stargazer(reg_2var, type="text", title="Régression simple")
  })
  
  # ACF des residus
  output$eval_residus_reg_2var <- renderPlot({
    donnees <- donnees[donnees$Annees == input$choix_annee_reg_2var & donnees$Region %in% input$choix_zone_reg_2var,c(input$choix_colonne_reg_2var,"Indice_du_bonheur")] 
    donnees <- donnees %>% drop_na()
    reg_2var <- lm(Indice_du_bonheur~., data = donnees)
    acf(residuals(reg_2var), main="Evaluation de l'hypothèse d'indépendance des résidus") 
  })
  
  
  # Regression multiple
  # resultat de la regression multiple
  output$reg_mult <- renderPrint({
    donnees <- donnees[donnees$Annees == input$choix_annee_reg_mul & donnees$Region %in% input$choix_zone_reg_mul,c(input$choix_colonnes_reg_mul,"Indice_du_bonheur")] 
    donnees <- donnees %>% drop_na()
    
    reg_mul <- lm(Indice_du_bonheur~., data=donnees)
    summary(reg_mul)
    
    stargazer(reg_mul, type="text", title="Régression multiple")
  })
  
  # Test de Ramsey
  output$reg_mult_ramsey <- renderPrint({
    donnees <- donnees[donnees$Annees == input$choix_annee_reg_mul & donnees$Region %in% input$choix_zone_reg_mul,c(input$choix_colonnes_reg_mul,"Indice_du_bonheur")] 
    donnees <- donnees %>% drop_na()
    
    reg_mul <- lm(Indice_du_bonheur~., data=donnees)
    resettest(reg_mul)
  })
  
  # analyse des residus
  output$residus_mult <- renderPlot({
    donnees <- donnees[donnees$Annees == input$choix_annee_reg_mul & donnees$Region %in% input$choix_zone_reg_mul,c(input$choix_colonnes_reg_mul,"Indice_du_bonheur")] 
    donnees <- donnees %>% drop_na()
    
    reg_mul <- lm(Indice_du_bonheur~., data=donnees)
    resid = residuals(reg_mul)
    resid2 = resid^2
    Fitted = fitted(reg_mul)
    
    plot(resid2~Fitted, data=donnees, main="Analyse de l' hétéroscédasticité éventuelle, selon les résidus associés \n aux prévisions de l'indice du Bonheur" ,xlab="Prévisions indice du Bonheur", ylab="Résidus carrés")
  })
  
  # analyse de la normalite des residus
  output$resume_mult <- renderPlot({
    donnees <- donnees[donnees$Annees == input$choix_annee_reg_mul & donnees$Region %in% input$choix_zone_reg_mul,c(input$choix_colonnes_reg_mul,"Indice_du_bonheur")] 
    donnees <- donnees %>% drop_na()
    
    reg_mul <- lm(Indice_du_bonheur~., data=donnees)
    
    res = rstandard(reg_mul)
    
    qqnorm(res, 
           ylab="Standardized Residuals", 
           xlab="Normal Scores", 
           main="Normal Q-Q") 
    qqline(res)
  })
})