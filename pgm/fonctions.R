## FONCTION DE PREPARATION DES DONNEES
preparation_donnees <- function(chemin_fichier)
{
  df <- readRDS(chemin_fichier) # chargement des données
  df <- df %>%
    mutate(prixm2 = replace(prixm2, prixm2 < 2000, NA)) # passage en NA des prix au m2 inférieurs à 2000 euros
  df <- na.omit(df) # sortie des lignes avec des NA
}

## FONCTION D'AGREGATION DES DONNEES
agregation_donnees <- function(nombre_var, df, var1, var2, var3, var4, var5) # jusqu'à 5 variables sélectionnables
{
  if(nombre_var == 1) # Si le nombre de variables sélectionné est 1, alors...
  {
    tab<-df %>% group_by({{var1}}) %>% # agrégation par la variable
      summarize(nbvent = n(),
                medprixm2 = median(prixm2)) %>% 
      ungroup() %>%
      as.data.frame()
  }
  else if(nombre_var == 2) # Sinon, si le nombre de variables sélectionné est 2, alors...
  {
    tab<-df %>% group_by({{var1}}, {{var2}}) %>% # agrégation sur les deux variables
      summarize(nbvent = n(),
                medprixm2 = median(prixm2)) %>% 
      ungroup() %>%
      as.data.frame()
  }
  else if(nombre_var == 3) # etc.
  {
    tab<-df %>% group_by({{var1}}, {{var2}}, {{var3}}) %>%
      summarize(nbvent = n(),
                medprixm2 = median(prixm2)) %>% 
      ungroup() %>%
      as.data.frame()
  }
  else if(nombre_var == 4)
  {
    tab<-df %>% group_by({{var1}}, {{var2}}, {{var3}}, {{var4}}) %>%
      summarize(nbvent = n(),
                medprixm2 = median(prixm2)) %>% 
      ungroup() %>%
      as.data.frame()
  }
  else
  {
    tab<-df %>% group_by({{var1}}, {{var2}}, {{var3}}, {{var4}}, {{var5}}) %>%
      summarize(nbvent = n(),
                medprixm2 = median(prixm2)) %>% 
      ungroup() %>%
      as.data.frame()
  }
}