library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  # Imporation jeux de donnees
  # Nettoyage + fusion 
  
  # Ouverture premiere BDD relative à l'annee 2021
  donnees <- read.csv("world-happiness-report-2021.csv")
  donnees <- donnees[,c(1:3,7:12)]
  colnames(donnees) <- c("Pays","Region","Indice_du_bonheur","log_PIB_par_hab","Soutien_social","Esperance_de_vie_en_bonne_sante","Liberte_de_faire_des_choix","Generosite","Perception_de_corruption")
  donnees$Annees <- 2021
  donnees$Pays[donnees$Pays == "Congo (Brazzaville)"] = 'Republic of Congo'
  donnees$Pays[donnees$Pays == "United States"] = 'USA'
  
  # Ouverture de la deuxième BDD 
  donnees_annees <- read.csv("world-happiness-report.csv")
  donnees_annees <- donnees_annees[,1:9]
  colnames(donnees_annees) <- c("Pays","Annees","Indice_du_bonheur","log_PIB_par_hab","Soutien_social","Esperance_de_vie_en_bonne_sante","Liberte_de_faire_des_choix","Generosite","Perception_de_corruption")
  donnees_annees <- merge(donnees[,c("Region","Pays")], donnees_annees, by="Pays") #,"long","lat","group","order","subregion"
  
  # Jeu de donnees final pour notre etude : 1 ligne par année (si dispo) et par pays 
  donnees <- full_join(donnees, donnees_annees)
  
  # données pour carto
  carte.monde <- map_data("world")
  colnames(carte.monde)[5] <- "Pays"
  donnees_carto <- left_join(carte.monde, donnees, by="Pays")
  
  # Onglet donnees
  # summary
  output$summary <- renderPrint({
    summary(donnees)
  })
  
  # table
  output$table <- renderDT({
    donnees
  })
  
  # nombre de classe
  output$n_bins <- renderText({
    input$go_graph
    isolate({
      paste("Nombre de classes : ", input$bins)
    })
  })
  
  observeEvent(input$go_graph,{
    updateTabsetPanel(session,inputId = "viz",selected="Histogramme")
  })
  
  # Onglet Visualisation
  output$distPlot <-renderAmCharts({
    input$go_graph
    isolate({
      x <- donnees[donnees$Annees == input$choix_annee_hist, input$choix_colonne] 
      bins <-round(seq(min(x,na.rm=TRUE),max(x,na.rm=TRUE), length.out = input$bins+1), 2) 
      # use amHist
      amHist(x = x,col = input$color, main = input$titre,export = TRUE, zoom = TRUE) #control_hist =list(breaks = bins)
    })
  })
  
  output$distPlot1 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- donnees[, input$choix_colonne] 
    
    # draw the histogram with the specified number of bins
    boxplot(x,col = input$color)
  })
  
  # Onglet cartographie
  # carte
  output$map = renderPlot({
    input$go_graph_carte
    isolate({
      donnees_bis <- donnees_carto %>%
        filter(Annees==input$choix_annees)
      
      ggplot(donnees_bis, aes(x=long, y=lat,group=group,fill= input$choix_colonne_carte))+
        geom_polygon()+
        scale_fill_viridis_d(option = "inferno")+
        ggtitle(label = "Carte du monde pour l'indice de bonheur")+
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5), panel.background = element_blank(), panel.grid.major = element_line(colour = "grey"))+
        ylab("Latitude")+
        xlab("Longitude")
    })
  })
  
  # Onglet Modeles
  
  #correlogramme
  output$correlogramme <- renderPlot({
    input$zone
    donnees <- donnees %>%
      filter(Annees == input$choix_annee_cor & Region == input$choix_zone_cor) %>%
      drop_na()
    matrixcorr<- cbind(donnees$Indice_du_bonheur, donnees$log_PIB_par_hab, donnees$Soutien_social, donnees$Esperance_de_vie_en_bonne_sante,
                       donnees$Liberte_de_faire_des_choix,donnees$Generosite, donnees$Perception_de_corruption)
    colnames(matrixcorr) <-c("IDB","Log PIB/h", "SS","EDV",
                             "LDC", "G", "PDC")
    mcor<-cor(matrixcorr)
    corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45, title = "Correlogramme de nos variables", mar = c(0,0,1,0))
  })
  
  # observeEvent(input$go_graph_carte,{
  #   updateTabsetPanel(session,inputId = "modele",selected="choix_zone_cor")
  # })
  
  # Regression simple
  output$reg_2var_graph <- renderPlot({
    donnees <- donnees[donnees$Annees == input$choix_annee_reg_2var,c(input$choix_colonne_reg_2var,"Indice_du_bonheur")] 
    donnees <- donnees %>% drop_na()
    ggplot(data = donnees) + 
      aes(x = input$choix_colonne_reg_2var , y = Indice_du_bonheur) + 
      geom_point() + 
      geom_smooth(method=lm)
  })
  
  output$reg_2var_summary <- renderPrint({
    donnees <- donnees[donnees$Annees == input$choix_annee_reg_2var,c(input$choix_colonne_reg_2var,"Indice_du_bonheur")] 
    donnees <- donnees %>% drop_na()
    reg_2var <- lm(Indice_du_bonheur~., data = donnees)
    summary(reg_2var)
    
    stargazer(reg_2var, type="text", title="Régression simple")
  })
  
  # Regression multiple
  output$reg_mult <- renderPrint({
    donnees <- donnees[donnees$Annees == input$choix_annee_reg_mul,] 
    donnees <- donnees %>% drop_na()
    
    reg_mul <- lm(Indice_du_bonheur~input$choix_colonnes_reg_mul, data=donnees)
    summary(reg_mul)
    
    stargazer(reg_mul, type="text", title="Régression simple")
  })
})
