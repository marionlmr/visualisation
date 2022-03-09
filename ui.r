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
library(FactoMineR)
library(shinythemes)
library(lmtest)

# Creation de texte, sous forme html pour la premiere page de l'application
doc <- tags$html(
  tags$head(
    tags$title('Bonheur')
  ),
  
  tags$body(
    h1('Indice du bonheur dans le monde', style = "text-align: center; background-color: #C70039; color: white;"),
    
    br(),
    br(),
    
    p("Notre base de données est issue du site “kaggle”. Elle se nomme ", strong("“Le bonheur dans le monde”."), 
      " Elle comporte 149 pays pour lesquels nous disposons de leur zone géographique, ainsi que des 
indicateurs comme l’indice de bonheur, le PIB par habitant (log), le soutien social, l’espérance de vie
en bonne santé, la liberté de faire des choix, les perceptions de corruption ou la générosité.", style = "text-align: justify;"),
    
    p("Les zones géographiques présentes dans notre base de données sont : Europe centrale et orientale, 
Europe occidentale, Asie de l’Est, Asie du Sud, Asie du Sud-Est, Afrique subsaharienne, Moyen-Orient 
et Afrique du Nord, Amérique latine et Caraïbes, Amérique du Nord et Australie et Nouvelle Zélande 
et la Communauté des Etats indépendants (Commonwealth of Independent States).", style = "text-align: justify;"),
    
    p("Nous avons à notre disposition des données relatives à l'indice du bonheur dans le monde pour les années
    allant de 2005 à 2021. Grâce à cette variable, nous allons pouvoir comparer l'évolution des
      différents indicateurs présents dans notre base de données.", style = "text-align: justify;"),
    
    p("Le score du ", strong("World Happiness Index"),", que nous appellerons indice de bonheur, constitue la 
variable expliquée de notre analyse économétrique. Il est obtenu selon la méthode de l'échelle de 
Cantril qui consiste à demander auprès d’un échantillon représentatif de la population nationale de 
penser à une échelle allant de 0 (la pire vie possible imaginable) à 10 (la meilleure vie possible). Les 
enquêtés sont ensuite invités à évaluer leur situation personnelle actuelle, puis de lui attribuer une 
note comprise entre 0 et 10 sur cette même échelle. Le rapport établit alors une corrélation entre les 
résultats de l’évaluation de la vie et divers indicateurs statistiques relatifs au bonheur pour un pays 
donné.", style = "text-align: justify;"),
    
    img(src = "https://th.bing.com/th/id/R.b3946abe13f8d66924f6b4623020bdc0?rik=0fK1gfU3YKPiog&pid=ImgRaw&r=0", width = "300px",style="display: block; margin-left: auto; margin-right: auto;"),
    
    br(),
    br(),
    
    p("Nous allons désormais présenter les six indicateurs qui constituent les variables explicatives de notre 
analyse économétrique. ", style = "text-align: justify;"),
    
    p("● Le ", strong("PIB par habitant"), " est exprimé en dollars à parité du pouvoir d’achat. Dans notre base de 
données, cette variable est déjà exprimée en logarithme. Cette transformation est très utile : la pente 
de la série transformée correspond ainsi au taux de croissance, et de plus cela nous libère des unités 
de mesure et introduit des unités de pourcentage qui sont relatives.", style = "text-align: justify;"),
    
    p("● Le ", strong("soutien social"), " est un indice créé dans le but de montrer le développement humain d’un 
pays. Cet indice est basé sur trois axes principaux incluant 52 indicateurs : les besoins humains 
fondamentaux (alimentation, santé, logement, …), les fondements du bien-être (alphabétisation, 
éducation, obésité, pollution, …) et les opportunités (droits politiques, de propriété, la corruption, ...).
      Dans notre base de données, il est compris entre 0 (absence d’aide sociale) et 1 (aide sociale absolue),
mais il est possible de l’exprimer en pourcentage", style = "text-align: justify;"),
    
    p("● Selon l’INSEE, ", strong("l'espérance de vie en bonne santé"), " est la durée de vie moyenne en bonne 
santé - c'est-à-dire sans limitation irréversible d'activité dans la vie quotidienne ni incapacités - d'une 
génération fictive soumise aux conditions de mortalité et de morbidité de l'année. Elle caractérise la 
mortalité et la morbidité indépendamment de la structure par âge.", style = "text-align: justify;"),
    
    p("● La ", strong("liberté de faire des choix"), " est un indice qui permet de décrire la possibilité et l’autonomie 
d’une personne d’effectuer une action choisie parmi au moins deux options disponibles, sans 
contraintes par des parties externes. L’indicateur de liberté (de 0 : absence totale de liberté à 1 :
jouissance totale de liberté) proposé par Freedom House caractérise globalement le respect des droits 
et des libertés.", style = "text-align: justify;"),
    
    p("● La ", strong("perception de corruption"), " correspond au pourcentage de la population déclarant que le 
secteur des affaires et le gouvernement sont corrompus (dans notre base de données, il s’agit d’un 
indice compris entre 0 et 1 : plus il y a proche de 0, moins la population pense que la corruption est 
répandue dans son pays, et inversement). Les résultats sont basés sur des entretiens en face à face et 
par téléphone avec environ 1000 adultes par pays, âgés de 15 ans et plus. Toutefois, notons que les 
questions sur la corruption sont si délicates dans certains pays que même si Gallup est autorisé à les 
poser, les résultats peuvent refléter la réticence des résidents à critiquer leur gouvernement. Cela est 
particulièrement vrai dans les pays où la liberté des médias est restreinte.", style = "text-align: justify;"),
    
    p("● La variable ", strong("générosité (World Giving Index)"), " est un indice permettant de donner un aperçu 
de la portée et de la nature du don dans le monde. Dans la plupart des pays étudiés, 1 000 
questionnaires sont remplis par un échantillon représentatif d'individus vivant dans tout le pays. 
Gallup a interrogé un échantillon d’individus pour savoir lesquels des trois actes de bienfaisance 
suivants ils avaient entrepris au cours du mois précédent : aider un étranger, ou quelqu'un qu'ils ne 
connaissaient pas et qui avait besoin d’aide ; fait un don d'argent à un organisme de bienfaisance ; 
donner de leur temps à une organisation. ", style = "text-align: justify;")),
  br(),
  br(),
  
  tags$footer(
    p(em("Application réalisée par 3 étudiantes de Master 1 Mathématiques Appliquées, Statistique - Science des données, IA"), style = "text-align: center;"),
    
    img(src = "https://static.univ-rennes2.fr/img/logo.svg", width = "100px", height = "100px",style="display: block; margin-left: auto; margin-right: auto;"),
    br(),
    p("Lou-Anne Guillotel, Marion Lemer & Floriane Mézirard", style="text-align:center; font-family: times"),
    
    p(em("Date de création - Mars 2022"), style = "text-align: center;"),
    
    style = "background-color: #C70039; color: white;"
  )
)



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
donnees$Region[donnees$Pays == "Somalia"] <- "Middle East and North Africa" # ou Afrique
donnees$Region[donnees$Pays == "Somaliland region"] <- "Middle East and North Africa" # ou Afrique
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


# Define UI for application that draws a histogram
shinyUI( 
  # navbarPage
  navbarPage(theme = shinytheme("slate"),
             "Evolution du bonheur dans le monde",
             
             # Premier onglet : Présentation
             tabPanel("Présentation",
                      doc),
             
             
             # Second onglet : Data
             tabPanel("Data", 
                      navlistPanel("Jeu de données", widths = c(2,10), 
                                   tabPanel("Tableau", h1("Données", style = "color: #C70039; text-align: center"),DTOutput("table")),
                                   tabPanel("Résumé", verbatimTextOutput("summary")))
                      
                    ), 
             
             
             # Troisième onglet : Visualisation
             tabPanel("Visualisation", 
                      tabsetPanel(
                        tabPanel("Visualisation",
                                 tabsetPanel(fluidRow(column(width = 3, wellPanel(sliderInput(inputId = "bins",
                                                                                              label = "Nombre de classes :",
                                                                                              min = 1,
                                                                                              max = 20,
                                                                                              value = 10),
                                                                                  
                                                                                  # input pour la couleur
                                                                                  colourInput(inputId = "color", label = "Couleur :", value = "#C70039"),
                                                                                  
                                                                                  # titre du graphique
                                                                                  textInput(inputId = "titre", label = "Titre :", value = "Histogramme"),
                                                                                  
                                                                                  # selection de la colonne
                                                                                  radioButtons(inputId = "choix_colonne", label = "Variables : ", choiceValues = colnames(donnees)[3:9], 
                                                                                               choiceNames = c("Indice du bonheur", "log PIB par habitant", "Soutien social", "Espérance de vie en bonne santé",
                                                                                                               "Liberté de faire des choix", "Générosité", "Perception de corruption")),
                                                                                  # filtre sur les lignes - choix de l'année
                                                                                  selectInput(inputId = "choix_annee_hist", label = "Années : ", choices = sort(unique(donnees$Annees),decreasing = TRUE), selected = 2021),
                                                                                  actionButton("go_graph", "Update !")
                                                                                  )
                                                          ),
                                                    column(width = 9, 
                                                          tabsetPanel(id="viz",
                                                                      tabPanel("Histogramme", plotlyOutput("distPlot"),
                                                                      br(),
                                                                      div(textOutput("n_bins"), align = "center")),
                                                                      tabPanel("Boxplot",plotlyOutput("distPlot1"))
                                                                      )
                                                          )
                                                    )
                                          )
                                ),
                        tabPanel("ACP",
                                 tabsetPanel(fluidRow(column(width = 2, 
                                                             wellPanel(# filtre sur les lignes - choix de l'année
                                                               selectInput(inputId = "choix_annee_ACP", label = "Années : ", choices = sort(unique(donnees$Annees),decreasing = TRUE), selected = 2021)
                                                                      )
                                                              ),
                                                      column(width = 5,
                                                            plotOutput("acpind")
                                                              ),
                                                      column(width = 5,
                                                            plotOutput("acpvar"),
                                                            br(),
                                                            p("Voici le constat général : Le soutien social, l'espérance de vie en bonne
                                                              santé, le log du PIB par habitant et l'indice du bonheur sont fortement corrélés positivement."),
                                                            p("La liberté de faire des choix et la perception de corruption sont fortement corrélées négativement."),
                                                            p("L'indice du bonheur et la générosité ne sont pas corrélés."),
                                                            p("Remarque : On retrouvera le même constat dans l'onglet Modèles.")
                                                            )
                                                    )
                                            )
                                )
                      )
             ),
             
             
             # Quatrième onglet : Comparaisons temporelles
             tabPanel("Comparaisons temporelles", 
                      tabsetPanel(id="comparaisons",
                                  tabPanel("Comparaisons entre régions", 
                                           fluidRow(column(width = 2, 
                                                           wellPanel(# filtre sur les lignes - choix de l'année
                                                             radioButtons(inputId = "choix_colonne_ST", label = "Variables : ", choiceValues = colnames(donnees)[3:9],
                                                                          choiceNames = c("Indice du bonheur", "log PIB par habitant", "Soutien social", "Espérance de vie en bonne santé",
                                                                                          "Liberté de faire des choix", "Générosité", "Perception de corruption")),
                                                                          checkboxGroupInput(inputId = "choix_zone_comparaison", 
                                                                                             label = "Zones géographiques : ", 
                                                                                             choices = sort(unique(donnees$Region)), 
                                                                                             selected = unique(donnees$Region)
                                                                                             )
                                                                    ),
                                                           
                                                           ),
                                                    column(width = 10,
                                                           plotlyOutput("comp_regions")
                                                          )
                                                    )
                                          ),
                                  
                                  tabPanel("Comparaisons entre pays", 
                                           fluidRow(column(width = 2, 
                                                           wellPanel(# filtre sur les lignes - choix de l'année
                                                             radioButtons(inputId = "choix_colonne_comparaison", label = "Variables : ", choiceValues = colnames(donnees)[3:9],
                                                                          choiceNames = c("Indice du bonheur", "log PIB par habitant", "Soutien social", "Espérance de vie en bonne santé",
                                                                                          "Liberté de faire des choix", "Générosité", "Perception de corruption")),
                                                             selectInput(inputId = "choix_pays1", label = "Pays n°1 : ", choices = sort(unique(donnees$Pays)), selected = "France"),
                                                             selectInput(inputId = "choix_pays2", label = "Pays n°2 : ", choices = sort(unique(donnees$Pays)), selected = "Finland")
                                                                    )
                                                          ),
                                                    column(width = 10,
                                                          plotlyOutput("comp_pays")
                                                          )
                                                    )
                                          )
                                )
             ),
             
             # Cinquième onglet : Cartographie
             tabPanel("Cartographie",
                      fluidRow(column(width = 3,wellPanel(# selection de la colonne
                        radioButtons(inputId = "choix_colonne_carte", label = "Variables : ", choiceValues = colnames(donnees)[3:9],
                                     choiceNames = c("Indice du bonheur", "log PIB par habitant", "Soutien social", "Espérance de vie en bonne santé",
                                                     "Liberté de faire des choix", "Générosité", "Perception de corruption")),
                        selectInput(inputId = "choix_annees", label = "Années : ", choices = sort(unique(donnees$Annees),decreasing = TRUE), selected = 2021),
                        actionButton("go_graph_carte", "Update !")
                      )),
                              column(width = 9,
                                    plotlyOutput("map")
                                    )
                              )
                      ),
             
             # Sizième onglet : Modèle - Régression
             tabPanel("Modèles", 
                      tabsetPanel(id="modele",
                                  tabPanel("Corrélation", 
                                           fluidRow(column(width = 3,
                                                           wellPanel(
                                                             # filtre sur les lignes - choix de l'année
                                                             selectInput(inputId = "choix_annee_cor", label = "Années : ", choices = 2021:2006, selected = 2021),
                                                             # filtre sur les régions - choix des zones géographiques
                                                             checkboxGroupInput(inputId = "choix_zone_cor", label = "Zones géographiques : ", choices = sort(unique(donnees$Region)), selected = unique(donnees$Region))
                                                                    )
                                                          ),
                                           
                                           column(width = 9,
                                                  plotOutput("correlogramme"))
                                                  )
                                           ),
                                  tabPanel("Régression linéaire simple",
                                           fluidRow(column(width = 3,
                                                           wellPanel(
                                                             # filtre sur les lignes - choix de l'année
                                                             selectInput(inputId = "choix_annee_reg_2var", label = "Années : ", choices = 2021:2006, selected = 2021),
                                                             # sélection colonne 
                                                             radioButtons(inputId = "choix_colonne_reg_2var", label = "Variable explicative : ", choiceValues = colnames(donnees)[4:9],
                                                                          choiceNames = c("log PIB par habitant", "Soutien social", "Espérance de vie en bonne santé",
                                                                                          "Liberté de faire des choix", "Générosité", "Perception de corruption")),
                                                             checkboxGroupInput(inputId = "choix_zone_reg_2var", label = "Zones géographiques : ", choices = sort(unique(donnees$Region)), selected = unique(donnees$Region))
                                                           )
                                           ),
                                           
                                           column(width = 6,
                                                  plotlyOutput("reg_2var_graph")),
                                           column(width = 3,
                                                  verbatimTextOutput("reg_2var_summary")),
                                           column(
                                             width = 9,
                                             plotOutput("eval_residus_reg_2var"),
                                             p("Les pointillées horizontaux sont les intervalles de confiance du coefficient 
                                               de corrélation égal à 0. Les traits verticaux représentent les coefficients de corrélation 
                                               entre les résidus de chaque point et ceux des points de la ligne suivante (lag=1), 
                                               ou ceux séparés de deux lignes (lag=2) etc…",style = "text-align: justify;")
                                           ))
                                  ),
                                  tabPanel("Régression linéaire multiple",
                                           fluidRow(column(width = 3,
                                                           wellPanel(
                                                             # filtre sur les lignes - choix de l'année
                                                             selectInput(inputId = "choix_annee_reg_mul", label = "Années : ", choices = 2021:2006, selected = 2021),
                                                             # sélection colonne 
                                                             checkboxGroupInput(inputId = "choix_colonnes_reg_mul", label = "Variables explicatives : ", choiceValues = colnames(donnees)[4:9],
                                                                                choiceNames = c("log PIB par habitant", "Soutien social", "Espérance de vie en bonne santé",
                                                                                                "Liberté de faire des choix", "Générosité", "Perception de corruption"), selected = colnames(donnees)[4]),
                                                             checkboxGroupInput(inputId = "choix_zone_reg_mul", label = "Zones géographiques : ", choices = sort(unique(donnees$Region)), selected = unique(donnees$Region))
                                                           )),
                                                    column(
                                                      width = 4,
                                                      verbatimTextOutput("reg_mult")),
                                                    column(width = 4,
                                                           p(strong("Test de Ramsey"),"- L'objectif est de vérifier que le modèle est bien spécifié."),
                                                           p("H0 : Le modèle est bien spécifié vs H1 : Le modèle est mal spécifié."),
                                                           textOutput("Ramsey"),
                                                           verbatimTextOutput("reg_mult_ramsey"),
                                                           p("Si pvalue < 5%, on rejette H0 (le modèle est mal spécifié).
                                                            Sinon, on ne rejette pas H0 (on ne dispose pas d'éléments suffisants remettant en cause la bonne spécification du modèle)
                                                            ." , style = "text-align: justify;"),
                                                           br()),
                                                    
                                                    column(width = 5,
                                                           plotOutput("residus_mult"),
                                                           p("Si les résidus semblent se répartir de manière aléatoire, 
                                                             alors on peut penser que l'hypothèse d'homoscédasticité est vérifiée.
                                                             En revanche, si les résidus ont une forme en trompette, il est 
                                                             possible que ces derniers soient hétéroscédastiques.")),
                                                    column(width = 7),     
                                                    column(width = 4,
                                                           plotOutput("resume_mult"),
                                                           p("Si les résidus semblent se répartir le long de la bissectrice,
                                                             alors cela nous conforte sur l'hypothèse de normalité des bruits."))
                                                    )
                                          )
                              ) 
                    )
          )
)