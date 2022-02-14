library(leaflet)
create_data=function(n){
  rand_lng <-  rnorm(n, 3, 1)
  rand_lat <-  rnorm(n, 47, 1)
  categories <- LETTERS
  data.frame(
    lat = rand_lat, lng = rand_lng, size = runif(n, 5, 20),
    category = factor(sample(categories, n, replace = TRUE), levels = categories),
    value = rnorm(n),id=1:n
  )}

df=create_data(100)
m <- leaflet(df) %>% addTiles()
m <- m %>% addMarkers(~lng, ~lat,popup = ~category,layerId = ~id,clusterOptions = markerClusterOptions(),clusterId = "points")
m


server <- function(input,output){
  output$map=renderLeaflet(m)
}
ui <- fluidPage(
  leafletOutput("map")
)
leaflet_app=shinyApp(ui=ui,server=server)
runApp(leaflet_app)



mygeocode <- function(adresses){
  # adresses est un vecteur contenant toutes les adresses sous forme de chaine de caracteres
  nominatim_osm <- function(address = NULL){
    ## details: http://wiki.openstreetmap.org/wiki/Nominatim
    ## fonction nominatim_osm proposée par D.Kisler
    if(suppressWarnings(is.null(address)))  return(data.frame())
    tryCatch(
      d <- jsonlite::fromJSON(
        gsub('\\@addr\\@', gsub('\\s+', '\\%20', address),
             'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
      ), error = function(c) return(data.frame())
    )
    if(length(d) == 0) return(data.frame())
    return(c(as.numeric(d$lon), as.numeric(d$lat)))
  }
  tableau <- t(sapply(adresses,nominatim_osm))
  colnames(tableau) <- c("lon","lat")
  return(tableau)
}



donnees <- read.csv("world-happiness-report-2021.csv")
donnees <- donnees[,c(1:3,7:12)]
colnames(donnees) <- c("Pays","Région","Indice_du_bonheur","log_PIB_par_hab","Soutien_social","Espérance_de_vie_en_bonne_santé","Liberté_de_faire_des_choix","Générosité","Perception_de_corruption")
donnees$Années <- 2021
donnees$geo <- mygeocode(donnees$Pays)

donnees_annees <- read.csv("world-happiness-report.csv")
donnees_annees <- donnees_annees[,1:9]
colnames(donnees_annees) <- c("Pays","Années","Indice_du_bonheur","log_PIB_par_hab","Soutien_social","Espérance_de_vie_en_bonne_santé","Liberté_de_faire_des_choix","Générosité","Perception_de_corruption")
donnees_annees <- merge(donnees[,c("Région","Pays","geo")], donnees_annees, by="Pays")

# Jeu de données final pour notre étude
donnees <- full_join(donnees, donnees_annees)


carte <- leaflet(donnees) %>% addTiles()
carte <- carte %>% addMarkers(~lng, ~lat,popup = ~Indice_du_bonheur,layerId = ~Pays,clusterOptions = markerClusterOptions(),clusterId = "points")
carte

