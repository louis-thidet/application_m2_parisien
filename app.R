library(shiny)
library(dplyr)
library(sf)
library(mapsf)
library(base64enc) # sert à charger l'image de fond
library(RColorBrewer) # sert aux palettes de couleurs
library(DT)

################ CHARGEMENT DES DONNEES ###########################################

source("pgm/fonctions.R") # chargement des fonctions

don <- preparation_donnees("data/dvfclean_V3.RDS") # chargement et préparation des données
tabref <- agregation_donnees(2, don, ann, type, x, x, x) # agrégation par année et type de logement
tabcom <- agregation_donnees(5, don, ann, type, code, nom, dep) # agrégation par année, type de logement, commune et département
iris<-readRDS("geom/map_iris.RDS") # chargement du fond de carte des iris
dep<-readRDS("geom/map_dep.RDS") # chargement du fond de carte des départements
com<-readRDS("geom/map_com.RDS") # chargement du fond de carte des communes

## chargement de l'image de fond de l'application
image_path <- "img/fond.jpg"
image_data <- readBin(image_path, "raw", file.info(image_path)$size)
image_fond <- base64encode(image_data)
## chargement des images de fond des boutons
image_path <- "img/visu_nbventes_oui.PNG" # bouton pour activer la visualisation des ventes sur la carte
image_data <- readBin(image_path, "raw", file.info(image_path)$size)
visu_nbventes_oui <- base64encode(image_data)
image_path <- "img/visu_nbventes_non.PNG" # bouton pour désactiver la visualisation des ventes sur la carte
image_data <- readBin(image_path, "raw", file.info(image_path)$size)
visu_nbventes_non <- base64encode(image_data)

################ PARTIE INTERFACE #################################################

## ============ BARRE DE NAVIGATION ============ 
ui <- navbarPage("Les ventes de logement dans le Grand Paris (2014-2021)",
  ## CREATION DES STYLES CSS CONFIGURANT LES IMAGES DE FOND
  tags$style
  (
    HTML
    ("
      .image_fond {
        position: fixed;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        z-index: -1;
        background-image: linear-gradient(to bottom, rgba(255, 255, 255, 0) 0%, rgba(255, 255, 255, 1) 110%), url('data:image/png;base64,", image_fond, "');
        background-repeat: no-repeat;
        background-size: cover;
      }
    .style_btn_oui {
      background-image: url('data:image/png;base64,", visu_nbventes_oui, "')!important;
      padding:20px;
    }
    .style_btn_non {
      background-image: url('data:image/png;base64,", visu_nbventes_non, "')!important;
      padding:20px;
    }
    "),
  ## INTEGRATION DES AUTRES STYLES CSS
  includeCSS("styles.css"),
  ),
  div(class = "image_fond"),
  ## =================================================== CARTE INTERACTIVE =================================================== 
  tabPanel
  ("Carte interactive",
   fluidPage
   (
     ## ============ PARAMETRES DE LA PAGE ============
     sidebarLayout
     (
       sidebarPanel
       (
         div(
           paste("Paramètres des données "), class = "case1"
         ),
         ## SELECTION DU TYPE DE LOGEMENT
         selectInput(
           inputId = "sel_log",
           label = "Type de logement",
           choices = c("Appartement" = "Appartement",
                       "Maison" = "Maison",
                       "Tout type confondu" = "tousleslogements"),
           selected = "Appartement"
         ),
         ## AFFICHER LE GRAND PARIS OU UN DEPARTEMENT
         selectInput(
           inputId = "selection_echelle",
           label = "Sélection de l'échelle",
           choices = c("Grand Paris" = "grand_paris",
                       "Département" = "departement"),
         ),
         # SELECTION DU DEPARTEMENT A AFFICHER
         conditionalPanel(
           condition = "input.selection_echelle == 'departement' || input.selection_echelle == 'commune'",
           selectInput(
             inputId = "departement",
             label = "Sélection du département",
             choices = c("75" = "75",
                         "92" = "92",
                         "93" = "93",
                         "94" = "94"),
             selected = "75"
           )
         ),
         ## SELECTION DE L'ANNEE
         sliderInput(
           inputId = "sel_ann",
           label = "Année de vente",
           min = 2014,
           max = 2021,
           value = 2019
         ),
         div(
           paste("Paramètres de la carte "), class = "case1"
         ),
         ## AFFICHER OU NON LE NOMBRE DE VENTES DE LOGEMENTS
         div(paste("Afficher les ventes : "),
             id = "ventesBouton",
             actionButton(inputId = "bouton_oui", label = "", class = "style_btn_oui"),
             actionButton(inputId = "bouton_non", label = "", class = "style_btn_non"), style = "margin-bottom:30px;"
         ),
         tags$input(type = "text", id = "affichage_ventes", value = "non", style = "pointer-events: none;"), # input gérant l'affichage du nombre de ventes
         ## SELECTION DE LA METHODE DE CLASSE
         selectInput(
           inputId = "methode",
           label = "Type de classes",
           choices = c("Effectifs égaux" = "quantile",
                       "Amplitudes égales" = "equal",
                       "Jenks" = "jenks"),
           selected = "quantile"
         ),
         ## SELECTION DU NOMBRE DE CLASSES
         sliderInput(
           inputId = "nb_classes",
           label = "Nombres de classes",
           min = 2,
           max = 8,
           value = 5
         ),
         ## SELECTION DE LA COULEUR
         div(paste("Couleur de la légende : "),
             id = "couleurBouton",
             actionButton(inputId = "bouton_orange", label = "", class = " style_btn_orange"),
             actionButton(inputId = "bouton_rouge", label = "", class = "style_btn_rouge"),
             actionButton(inputId = "bouton_vert", label = "", class = "style_btn_vert"),
             actionButton(inputId = "bouton_bleu", label = "", class = "style_btn_bleu"), style = "margin-bottom:30px;"
         ),
         tags$input(type = "text", id = "couleur_legende", value = "Oranges"), # input de la couleur de la légende
         tags$input(type = "text", id = "couleur_contour", value = "#4298f5"), # input de la couleur des départements
         
         ## ============ AFFICHAGE DU PRIX MEDIAN TOUTES COMMUNES CONFONDU ============
         div(
           htmlOutput("prix_m2_median"),class = "case2"
           ),
         div(HTML(paste("<span style= 'font-weight:bold;'> Remarque </span> : il s'agit du prix médian tout logement confondu, 
         et non de la valeur médiane entre tous les prix médians
               des communes")), class = "remarque"),
         style = "margin-left:30px;"
       ),
       ## ============ INTEGRATION DE LA CARTE ET DES GRAPHIQUES DE LA PAGE ============             
       mainPanel
       (
         class = "custom-main-panel",
         plotOutput("mapPlot",height = "550px", width = "95%"),
         plotOutput("histwhiskersPlot", height = "300px", width = "95%"),
         downloadButton("telechargement_donnees_parametrees", "Données paramétrées", class = "telechargement_donnees"),
         downloadButton("telechargement_donnees_communes", "Données par commune", class = "telechargement_donnees"),
         downloadButton("telechargement_donnees_ref", "Données du Grand Paris", class = "telechargement_donnees"),
         downloadButton("telechargement_donnees_brutes", "Données brutes", class = "telechargement_donnees")
       )
     )
   ),
  ),
    ## =================================================== TABLEAUX DE DONNEES =================================================== 
  tabPanel("Tableaux de données",
    tabsetPanel(
     id = 'dataset',
     tabPanel("Données paramétrées", DT::dataTableOutput("map_sortie"), style = "margin-top:20px; padding:15px; background-color:white;"),
     tabPanel("Prix du m² médian par commune", DT::dataTableOutput("tabcom_sortie"), style = "margin-top:20px; padding:15px; background-color:white;"),
     tabPanel("Prix du m² médian dans le Grand Paris", DT::dataTableOutput("tabref_sortie"), style = "margin-top:20px; padding:15px; background-color:white;"),
     tabPanel("Données brutes", DT::dataTableOutput("don_sortie"), style = "margin-top:20px; padding:15px; background-color:white;")
    )
  ),
  ## =================================================== A PROPOS =================================================== 
  tabPanel("A propos",
           div(
             paste("A propos "), class = "a_propos_titre"
           ),
           div(
             paste("Cette application Shiny a été réalisée par Louis THIDET dans le cadre du cours Datamining du master PISE
                               de l'Université Paris-Cité, dirigé par Claude GRASLAND et Camille SIGNORETTO"), class ="a_propos"
           ),
           div(
             HTML(paste("Contact : <a href='mailto: louis.thidet@gmail.com'>louis.thidet@gmail.com</a>")), class ="a_propos"
           ),
  )
)

################ PARTIE SERVEUR ###################################################
server <- function(input, output, session) {
  
  ## ============ CREATION DE L'HISTOGRAMME ET DE LA BOITE A MOUSTACHE ============
  output$histwhiskersPlot <- renderPlot({

    if(input$selection_echelle == "departement") # Si l'échelle sélectionnée est le département, alors...
    {
      tabcom<- tabcom %>% filter(dep == input$departement) # on filtre sur le département sélectionné
      label_sel_echelle = paste("département", input$departement)
    }
    else
    {
      label_sel_echelle = "Grand Paris"
    }

    if(input$sel_log != "tousleslogements") # Si on ne choisit pas de considérer tous les types de logements, alors...
    {
      tabcom<-tabcom %>% filter(type == input$sel_log) # on filtre sur l'année et le type de logement paramétrés
    }
    tabcom<-tabcom %>% filter(ann==input$sel_ann) # filtrage sur l'année choisie
    tabcom<-tabcom$medprixm2
    
    palette <- brewer.pal(name = input$couleur_legende, n = input$nb_classes) ## création d'une palette de couleur en fonction du nombre de classes
    mybreaks<-mf_get_breaks(tabcom, nbreaks= input$nb_classes, breaks=input$methode) ## les barres de l'hist sont faites en conséquence des classes et de leur méthode
    
    ## modifier le titre du graphique en conséquence
    if(input$sel_log == "Appartement")
    {
      label_sel_log = "pour les appartements"
    }
    else if(input$sel_log == "Maison")
    {
      label_sel_log = "pour les maisons"
    }
    else
    {
      label_sel_log = "tout type de logement confondu"
    }
    
    ## HISTOGRAMME
    par(bg = "#f2f9df", lwd =2) # Remarque : le fond vert ne s'affiche pas avec le package Cairo
    hist(tabcom, 
         breaks=mybreaks, 
         col = palette, 
         border = input$couleur_contour, 
         main = paste("Distribution du prix médian du m² par commune dans le", label_sel_echelle ,"en \n", input$sel_ann, label_sel_log),
         xlab = "Prix du m² dans la commune",
         ylab = "Densité de probabilité")
    mybw<-2*sd(tabcom,na.rm=T)/input$nb_classes
    lines(density(tabcom,bw=mybw,na.rm=T),col="red",lwd=2)
    
    ## BOITE A MOUSTACHE
    par(bg = "#f2f9df", lwd =2, new = TRUE) # Remarque : le fond vert ne s'affiche pas avec le package Cairo
    boxplot(tabcom,
            main = "",
            xlab = "",
            ylab = "",
            col = rgb(0, 0.8, 1, alpha = 0.8),
            border = "brown",
            horizontal = TRUE,
            notch = FALSE)
    box()
  })
  
  ## ======== BOUTONS POUR AFFICHER OU CACHER LES VENTES ========
  observeEvent(input$bouton_oui, {
    updateSelectInput(inputId = "affichage_ventes", selected = "oui")

  })
  observeEvent(input$bouton_non, {
    updateSelectInput(inputId = "affichage_ventes", selected = "non")
  })
  
  ## ============ BOUTONS DE SELECTION DE LA COULEUR ============
  observeEvent(input$bouton_orange, {
    updateSelectInput(inputId = "couleur_legende", selected = "Oranges")
    updateSelectInput(inputId = "couleur_contour", selected = "#4298f5")
  })
  observeEvent(input$bouton_rouge, {
    updateSelectInput(inputId = "couleur_legende", selected = "Reds")
    updateSelectInput(inputId = "couleur_contour", selected = "#31a354")
  })
  observeEvent(input$bouton_vert, {
    updateSelectInput(inputId = "couleur_legende", selected = "Greens")
    updateSelectInput(inputId = "couleur_contour", selected = "Red")
  })
  observeEvent(input$bouton_bleu, {
    updateSelectInput(inputId = "couleur_legende", selected = "Blues")
    updateSelectInput(inputId = "couleur_contour", selected = "Orange")
  })
  output$selectedColor <- renderPrint({ ## MISE A JOUR DES SORTIES EN FONCTION DE L'ACTION EFFECTUEE
    input$couleur_legende
  })

  ## ============ CREATION DE LA CARTE ============
  output$mapPlot <-renderPlot({

    ## SERT A CONFIGURER LE TITRE
    if(input$sel_log == "Appartement")
    {
      label_sel_log = "pour les appartements"
    }
    else if(input$sel_log == "Maison")
    {
      label_sel_log = "pour les maisons"
    }
    else
    {
      label_sel_log = "tout type de logement confondu"
    }
    
    ## PREPARATION EN FONCTION DE LA SELECTION DE L'ECHELLE
    if(input$selection_echelle == "departement")
    {
      tabcom<- tabcom %>% filter(dep == input$departement)
      dep <- dep[dep$DEPT == input$departement, ]
      com$dep <- substr(com$INSEE_COM, 1, 2)
      com <- com %>% filter(dep == input$departement)
      label_sel_echelle = paste("le département", input$departement)
    }
    else
    {
      label_sel_echelle = "le Grand Paris"
    }
    
    ## FILTRE SUR LE DU TYPE DE LOGEMENT
    if(input$sel_log != "tousleslogements") ## Si on ne considère pas tous les logements, alors...
    {
      tabcom<-tabcom %>% filter(type == input$sel_log) ## on filtre sur le type de logement choisi
    }
    
    ## FILTRAGE SUR L'ANNEE SELECTIONNE
    tabcom<-tabcom %>% filter(ann==input$sel_ann) %>% # filtrage sur l'année choisie
      mutate(INSEE_COM = code) # uniformisation de la variable du code INSEE
    
    ## CREATION D'UNE PALETTE POUR LA COUCHE CHOROPLETE DE LA CARTE
    palette <- brewer.pal(name = input$couleur_legende, n = input$nb_classes) ## création d'une palette de couleur pour les classes
    map <-left_join(com,tabcom) # jointure du fond de carte des communes sur le tableau des données
    
    ## INITIALISATION DE LA CARTE ET AJOUT DE L'OMBRE DE LA CARTE
    mf_init(map, theme = "agolalight") # initialise la carte et définit son thème
    mf_shadow(map, col = "grey50", cex = 3) # crée une ombre sous la carte
    
    ## COUCHE CHOROPLETE
    {
      mf_map(map,
             leg_title = "Prix du m² par commune",
             var = "medprixm2",
             type = "choro",
             nbreaks = input$nb_classes,
             breaks = input$methode,
             pal = palette,
             leg_pos = "bottomleft2",
             leg_title_cex = 1.3,
             leg_val_cex = 0.8,
             add = TRUE)
    }
    
    mf_layout(title = paste("Prix du m² par commune dans", label_sel_echelle ,"en", input$sel_ann, label_sel_log), ## titre de la carte
              arrow = FALSE, scale = FALSE, credits= "")
    mf_credits(
      txt = "Source : Base DVF (Direction générale des finances publiques)",
      pos = "bottomleft",
      cex = 1,
      font = 3,
      bg = NA
    )
    
    ## COUCHE CONTOUR DES DEPARTEMENTS
    mf_map(dep, 
           type = "base", 
           col = NA,
           border=input$couleur_contour,
           lwd=2,
           add = TRUE)
    
    ## COUCHE DU NOMBRE DE VENTES PAR COMMUNES
    if(input$affichage_ventes == "oui")
    {
      if(input$sel_log != "tousleslogements")
      {
        mf_map(map,
               leg_title = "Nombre de ventes de la commune",
               var = "nbvent",
               type = "prop",
               col = input$couleur_contour,
               border = "black",
               leg_pos = "topleft",
               leg_title_cex = 1.3,
               leg_val_cex = 0.9,
               add = TRUE)
      }
      else
      {
        mf_map(map,
               leg_title = "Nombre de ventes de la commune",
               var = c("nbvent", "type"),
               type = "prop_typo",
               border = "black",
               leg_pos = c("topleft", "hide"),
               leg_title_cex = 1.3,
               leg_val_cex = 1,
               add = TRUE)
        
        mf_legend(
          type = "typo", pos = "topleft2", val = c("Appartements", "Maisons"),
          pal = hcl.colors(2, "Dynamic"), cex = 1.5, title = "", val_cex = 1
        )
      }
    }
    
  })

  ## ============ CALCUL DU PRIX MEDIAN DU M2 DANS LE GRAND PARIS, TOUT LOGEMENTS CONFONDUES ============
  output$prix_m2_median <- renderText({
    
    if(input$sel_log != "tousleslogements") # Si on ne choisit pas de considérer tous les types de logements, alors...
    {
      prix_m2_median <- tabref %>%
        filter(ann == input$sel_ann, type == input$sel_log) # on filtre sur l'année et le type de logement paramétrés
    }
    else # sinon...
    {
      prix_m2_median <- tabref %>%
        group_by(ann) %>% #                            On regroupe les prix médians des deux types de logement
        summarize(medprixm2 = mean(medprixm2)) %>% #   On fait la moyenne de ces deux prix médians
        filter(ann == input$sel_ann) #                 On filtre sur l'année paramétrée
    }

    palette <- brewer.pal(name = input$couleur_legende, n = input$nb_classes) # On récupère la palette générée pour la légende
    classe_milieu <- floor(input$nb_classes / 2)                              # On calcul l'indice de la couleur de la classe médiane
    couleur <- palette[classe_milieu]                                         # On récupère la couleur de la classe médiane
    
    HTML(paste("Prix médian du m² dans le <br> Grand Paris en ", input$sel_ann, " : <br>",
            "<span style='color:", couleur, "; font-size: 40px;'>", round(prix_m2_median$medprixm2,0), "€</span>"))
    ## la couleur de la classe médiane est appliquée au prix médian
  })
  
  ## ============ BOUTON DE TELECHARGEMENT DES DONNEES DE LA CARTE ACTUELLE ============
  map <- reactive({
    if(input$selection_echelle == "departement")
    {
      tabcom<- tabcom %>% filter(dep == input$departement) 
      dep <- dep[dep$DEPT == input$departement, ]
      com$dep <- substr(com$INSEE_COM, 1, 2)
      com <- com %>% filter(dep == input$departement)
    }
    ## FILTRE SUR LE DU TYPE DE LOGEMENT
    if(input$sel_log != "tousleslogements") ## Si on ne considère pas tous les logements, alors...
    {
      tabcom<-tabcom %>% filter(type == input$sel_log) ## on filtre sur le type de logement choisi
    }
    ## FILTRAGE SUR L'ANNEE SELECTIONNE
    tabcom<-tabcom %>% filter(ann==input$sel_ann) %>% # filtrage sur l'année choisie
      mutate(INSEE_COM = code) # uniformisation de la variable du code INSEE
    map <-left_join(com,tabcom) # jointure du fond de carte des communes sur le tableau des données
    return(map)
  })
  
  output$telechargement_donnees_parametrees <- downloadHandler(
    filename = function() {
      paste("donnees_parametrees-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(map(), file)
    }
  )
  ## ============ BOUTON DE TELECHARGEMENT DES DONNEES DES COMMUNES ============
  output$telechargement_donnees_communes <- downloadHandler(
    filename = function() {
      paste("donnees_communes-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(tabcom, file)
    }
  )
    ## ============ BOUTON DE TELECHARGEMENT DES DONNEES DANS LE GRAND PARIS ============
  output$telechargement_donnees_ref <- downloadHandler(
    filename = function() {
      paste("donnees_grand_paris-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(tabref, file)
    }
  )
  ## ============ BOUTON DE TELECHARGEMENT DES DONNEES BRUTES ============
  output$telechargement_donnees_brutes <- downloadHandler(
    filename = function() {
      paste("donnees_brutes-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(don, file)
    }
  )
  ## ============ AFFICHAGE DU TABLEAU DES DONNEES PARAMETREES PAR L'UTILISATEUR ============
  output$map_sortie = DT::renderDataTable({
    map()
  })
  ## ============ AFFICHAGE DU TABLEAU DES DONNEES PAR COMMUNE ============
  output$tabcom_sortie = DT::renderDataTable({
    tabcom
  })
  ## ============ AFFICHAGE DU TABLEAU DES DONNEES DU GRAND PARIS ============
  output$tabref_sortie = DT::renderDataTable({
    tabref
  })
  ## ============ AFFICHAGE DU TABLEAU DES DONNEES BRUTES ============
  output$don_sortie = DT::renderDataTable({
    don
  })
}

## =================================================== LANCEMENT DE L'APPLICATION ===================================================
shinyApp(ui = ui, server = server)