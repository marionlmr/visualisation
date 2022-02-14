library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  # Imporation jeux de données
  # Nettoyage + fusion 
  
  donnees <- read.csv("world-happiness-report-2021.csv")
  donnees <- donnees[,c(1:3,7:12)]
  colnames(donnees) <- c("Pays","Région","Indice_du_bonheur","log_PIB_par_hab","Soutien_social","Espérance_de_vie_en_bonne_santé","Liberté_de_faire_des_choix","Générosité","Perception_de_corruption")
  donnees$Années <- 2021
  
  donnees_annees <- read.csv("world-happiness-report.csv")
  donnees_annees <- donnees_annees[,1:9]
  colnames(donnees_annees) <- c("Pays","Années","Indice_du_bonheur","log_PIB_par_hab","Soutien_social","Espérance_de_vie_en_bonne_santé","Liberté_de_faire_des_choix","Générosité","Perception_de_corruption")
  donnees_annees <- merge(donnees[,c("Région","Pays")], donnees_annees, by="Pays")
  
  # Jeu de données final pour notre étude
  donnees <- full_join(donnees, donnees_annees)
  
  
  output$distPlot <-renderAmCharts({
    input$go_graph
    isolate({
      x <- donnees[, input$choix_colonne]
      bins <-round(seq(min(x,na.rm=TRUE),max(xna.rm=TRUE), length.out = input$bins+1), 2) 
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
  
  # summary
  output$summary <- renderPrint({
    summary(donnees)
  })
  
  # table
  output$table <- renderDT({ #DataTable
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
  
})
