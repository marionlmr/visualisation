library(shiny)
library(colourpicker)
library(shinydashboard)
library(rAmCharts)
library(DT)
library(tidyverse)
library(leaflet)
library(viridis)

doc <- tags$html(
  tags$head(
    tags$title('Bonheur')
  ),
  tags$body(
    h1('Indice du bonheur dans le monde', style = "text-align: center; background-color: #C70039; color: white;"),
    p("Notre base de données est issue du site “kaggle”. Elle se nomme ", strong("“Le bonheur dans le monde”."), 
      " Elle comporte 149 pays pour lesquels nous disposons de leur zone géographique, ainsi que des 
indicateurs comme l’indice de bonheur, le PIB par habitant (log), le soutien social, l’espérance de vie
en bonne santé, la liberté de faire des choix, les perceptions de corruption ou la générosité.", style = "text-align: justify;"),
    p("Les zones géographiques présentes dans notre base de données sont : Europe centrale et orientale, 
Europe occidentale, Asie de l’Est, Asie du Sud, Asie du Sud-Est, Afrique subsaharienne, Moyen-Orient 
et Afrique du Nord, Amérique latine et Caraïbes, Amérique du Nord et Australie et Nouvelle Zélande 
et la Communauté des Etats indépendants (Commonwealth of Independent States).", style = "text-align: justify;"),
    p("Dans notre base de données est présent différentes années, allant de 2005 à 2021, concernant
      l'indice de bonheur dans le monde. Grâce à cette variable nous allons pouvoir comparer l'évolution des
      différents indicateurs présents dans notre base de données.", style = "text-align: justify;"),
    p("Le score du ", strong("World Happiness Index"),", que nous appellerons indice de bonheur, constitue la 
variable expliquée de notre analyse économétrique. Il est obtenu selon la méthode de l'échelle de 
Cantril qui consiste à demander auprès d’un échantillon représentatif de la population nationale de 
penser à une échelle allant de 0 (la pire vie possible imaginable) à 10 (la meilleure vie possible). Les 
enquêtés sont ensuite invités à évaluer leur situation personnelle actuelle, puis de lui attribuer une 
note comprise entre 0 et 10 sur cette même échelle. Le rapport établit alors une corrélation entre les 
résultats de l’évaluation de la vie et divers indicateurs statistiques relatifs au bonheur pour un pays 
donné.", style = "text-align: justify;"),
    p("Nous allons désormais présenter les six indicateurs qui constituent les variables explicatives de notre 
analyse économétrique. ", style = "text-align: justify;"),
    p("    ● Le ", strong("PIB par habitant"), " est exprimé en dollars à parité du pouvoir d’achat. Dans notre base de 
données, cette variable est déjà exprimée en logarithme. Cette transformation est très utile : la pente 
de la série transformée correspond ainsi au taux de croissance, et de plus cela nous libère des unités 
de mesure et introduit des unités de pourcentage qui sont relatives.", style = "text-align: justify;"),
    p("    ● Le ", strong("soutien social"), " est un indice créé dans le but de montrer le développement humain d’un 
pays. Cet indice est basé sur trois axes principaux incluant 52 indicateurs : les besoins humains 
fondamentaux (alimentation, santé, logement, …), les fondements du bien-être (alphabétisation, 
éducation, obésité, pollution, …) et les opportunités (droits politiques, de propriété, la corruption, ...).
      Dans notre base de données, il est compris entre 0 (absence d’aide sociale) et 1 (aide sociale absolue),
mais il est possible de l’exprimer en pourcentage", style = "text-align: justify;"),
    p("    ● Selon l’INSEE, ", strong("l'espérance de vie en bonne santé"), " est la durée de vie moyenne en bonne 
santé - c'est-à-dire sans limitation irréversible d'activité dans la vie quotidienne ni incapacités - d'une 
génération fictive soumise aux conditions de mortalité et de morbidité de l'année. Elle caractérise la 
mortalité et la morbidité indépendamment de la structure par âge.", style = "text-align: justify;"),
    p("    ● La ", strong("liberté de faire des choix"), " est un indice qui permet de décrire la possibilité et l’autonomie 
d’une personne d’effectuer une action choisie parmi au moins deux options disponibles, sans 
contraintes par des parties externes. L’indicateur de liberté (de 0 : absence totale de liberté à 1 :
jouissance totale de liberté) proposé par Freedom House caractérise globalement le respect des droits 
et des libertés.", style = "text-align: justify;"),
    p("    ● La ", strong("perception de corruption"), " correspond au pourcentage de la population déclarant que le 
secteur des affaires et le gouvernement sont corrompus (dans notre base de données, il s’agit d’un 
indice compris entre 0 et 1 : plus il y a proche de 0, moins la population pense que la corruption est 
répandue dans son pays, et inversement). Les résultats sont basés sur des entretiens en face à face et 
par téléphone avec environ 1000 adultes par pays, âgés de 15 ans et plus. Toutefois, notons que les 
questions sur la corruption sont si délicates dans certains pays que même si Gallup est autorisé à les 
poser, les résultats peuvent refléter la réticence des résidents à critiquer leur gouvernement. Cela est 
particulièrement vrai dans les pays où la liberté des médias est restreinte.", style = "text-align: justify;"),
    p("    ● La variable ", strong("générosité (World Giving Index)"), " est un indice permettant de donner un aperçu 
de la portée et de la nature du don dans le monde. Dans la plupart des pays étudiés, 1 000 
questionnaires sont remplis par un échantillon représentatif d'individus vivant dans tout le pays. 
Gallup a interrogé un échantillon d’individus pour savoir lesquels des trois actes de bienfaisance 
suivants ils avaient entrepris au cours du mois précédent : aider un étranger, ou quelqu'un qu'ils ne 
connaissaient pas et qui avait besoin d’aide ; fait un don d'argent à un organisme de bienfaisance ; 
donner de leur temps à une organisation. ", style = "text-align: justify;")),
  tags$footer(
    p("Application réalisée par 3 étudiantes de master 1 MAS, Science des données, IA à Rennes 2.", style = "text-align: center;"),
    img(src = "https://upload.wikimedia.org/wikipedia/fr/2/23/Logo_univ-rennes2-2016.svg", width = "100px", height = "100px"),
    p("Lou-Anne Guillotel, Marion Lemer & Floriane Mézirard", style = "text-align: center;"),
    p("Date de création mars 2022", style = "text-align: center;"),
    style = "background-color: #C70039; color: white;"
    
    #   div(id='myDiv', class='simpleDiv',
    #       'Here is a div with some attributes.')
  )
)

# Define UI for application that draws a histogram
shinyUI(
  # navbarPage
  navbarPage("Evolution du bonheur dans le monde",
             
             tabPanel("Présentation",
                      doc),
             
             # premier onglet Data
             tabPanel("Data", 
                      navlistPanel("Sélection de la visualisation pour le jeu de données",
                                   tabPanel("Tableau", h1("Données", style = "color: #C70039;"),DTOutput("table")),
                                   tabPanel("Résumé", verbatimTextOutput("summary")))
                      
             ), 
             
             # second onglet Visualisation
             tabPanel("Visualisation", 
                      fluidRow(column(width = 3,wellPanel(sliderInput("bins",
                                                                      "Number of bins:",
                                                                      min = 1,
                                                                      max = 20,
                                                                      value = 10),
                                                          
                                                          # input pour la couleur
                                                          colourInput(inputId = "color", label = "Couleur :", value = "#C70039"),
                                                          
                                                          # titre du graphique
                                                          textInput(inputId = "titre", label = "Titre :", value = "Histogramme"),
                                                          
                                                          # selection de la colonne
                                                          radioButtons(inputId = "choix_colonne", label = "Variables : ", choices = colnames(donnees)[8:14]),
                                                          actionButton("go_graph", "Update !")
                      )),
                      column(width = 9, 
                             tabsetPanel(id="viz",
                                         tabPanel("Histogramme", amChartsOutput("distPlot"), 
                                                  div(textOutput("n_bins"), align = "center")),
                                         tabPanel("Boxplot",plotOutput("distPlot1"))
                             )
                      )
             )),
             
             # Troisième onglet Cartographie
             tabPanel("Cartographie",
             fluidRow(column(width = 3,wellPanel(# selection de la colonne
                                                 radioButtons(inputId = "choix_colonne_carte", label = "Variables : ", choices = colnames(donnees)[8:14]),
                                                 radioButtons(inputId = "choix_annees", label = "Années : ", choices = sort(unique(donnees$Annees))),
                                                 actionButton("go_graph_carte", "Update !")
             )),
             column(width = 9, 
                    tabsetPanel(id="viz",
                                tabPanel("Cartographie", plotOutput("map"))
                    )
             )
  )),
             
             # Quatième onglet Modèle : Régression
             tabPanel("Modèles", 
                      navlistPanel("Sélection des variables explicatives")
                      
             ),
             
             # Cinquième onglet : comparaison des pays et comparaison des années
             tabPanel("Comparaisons",
                      navlistPanel("Sélection des variables : années, pays")
             )
  )
)