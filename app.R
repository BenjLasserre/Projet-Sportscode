## L'application étant assez complexe l'utilisation de Shiny n'était pas suffisante, j'ai donc dû faire appel
## au package Shiny Dashboard, plus élaboré sur certains points.
## Son utilisation est très proche de celle de Shiny, c'est en fait le même environnement avec un UI et un Server.
## La différence c'est le format à l'intérieur de chaque scripte. Pas de page simple ou de navbar, le dashboard impose
## un format en trois entités : une barre de titre (Header) en haut, une zone de sélection des onglets sur la gauche (Sidebar)
## et enfin le corps principal de la page (Body).
## Pour être dynamique (par rapport à la Sidebar principalement) les éléments doivent être créés à partir du Server
## et appelés dans le UI.


## Chargement des librairies nécessaires pour le code (les packages doivent être installés au préalable)
library(shiny)
library(shinydashboard)
library(readr)
library(stringr)
library(lubridate)
library(dplyr)
library(tidyr)
library(highcharter)
library(data.table)
library(shinyjs)
library(stringi)
library(varhandle)
library(DT)
library(readxl)


## Plus tard, nous aurons besoin de fonds et de thèmes particuliers pour les graphiques de représentations des frappes.
## Puisqu'il s'agit plus ou moins de fonctions, nous pouvons les créer ici et les utiliser plus bas.

# Tout d'abord le thème surface, qui utilise en fond l'image du dernier tiers du terrain sur le thème de base tufte, très sobre.
# On trouve assez facilement sur internet des exemples de création d'un tel modèle. Le point important c'est surtout le plotBackgroundImage.
# Le lien va ensuite chercher l'image que j'ai, après modification paint.net, ajoutée à mon github (qui est en public)
surface <- hc_theme_merge(
  hc_theme_tufte(),
  hc_theme(title = list(style = list(color = "black")),
           chart = list(backgroundColor = "transparent",
                        plotBackgroundImage = "https://github.com/BenjLasserre/Projet-Sportscode/raw/master/surface2.jpg",
                        style = list(fontFamily = "Century Gothic")
           )
  )
)

# Cette fois, il s'agit de l'image d'une cage, également adaptée grâce à paint.net, que l'on va aussi chercher sur mon github.
but <- hc_theme_merge(
  hc_theme_tufte(),
  hc_theme(title = list(style = list(color = "black")),
           chart = list(backgroundColor = "transparent",
                        plotBackgroundImage = "https://github.com/BenjLasserre/Projet-Sportscode/raw/master/cage2.png",
                        style = list(fontFamily = "Century Gothic")
           )
  )
)


### On fait la même chose aussi avec un terrain complet sur lequel on placera ensuite le onze type de la saison
terrain <- hc_theme_merge(
  hc_theme_tufte(),
  hc_theme(
    title = list(
      style = list(
        color = "black"
      )
    ),
    chart = list(
      backgroundColor = "transparent",
      plotBackgroundImage = "http://wallpaper.pickywallpapers.com/1920x1080/schematic-green-soccer-field.jpg",
      style = list(fontFamily = "Century Gothic")
    )
  )
)


## Chargement des données Sportscode. À cause du format xml et plus particulièrement des accents, de nombreuses étapes préalables étaient à faire.
# Ouvrir un Excel vide, faire ouvrir un fichier, ouvrir le xml avec l'option par défaut. Ensuite enregistrer ce Excel en csv.
# Ensuite, ne surtout pas ouvrir ce csv avec Excel, l'ouvrir en premier avec un éditeur de texte, puis enregistrer en txt après avoir supprimé la dernière ligne.
# On en a profité pour changer les é en e avant le code. On aurait pu le faire ici, mais ils posaient problème dans un premier temps.
stats_barca = read.table("Stats_Barca2.txt", header = TRUE,sep = ";", dec=",", na.strings = "",
                         colClasses = c("character","numeric","numeric","character","character","character"))

## Import du fichier des données OPTA sur un match, rempli à la main donc directement dans un format adapté.
# Simplement un travail sur la date de naissance des joueurs, à l'aide de la fonction format (package lubridate) pour imposer une écriture plus usuelle
Barca_Eibar <- read_excel("Barca_Eibar.xlsx")
Barca_Eibar$Date_naissance = format(Barca_Eibar$Date_naissance,format="%d/%m/%Y")


## Il est maintenant temps de faire du datamanagement pour retravailler à notre guise les données Sportscode
# On va commencer par traiter les journées une à une, car c'est plus simple, puis les regrouper ensuite. D'où la boucle sur 15 positions (15 journées)
for(i in 1:15){
  # On récupère tous les ID qui ont une correspondance avec la journée i, pour récupérer toutes les lignes de la journée
  temp = stats_barca[which(stats_barca$ID %in% stats_barca$ID[which(stats_barca$text==paste0('J',i))]),]
  
  # On nettoie un peu les chaines de caractères qui vont devenir des noms de variables et on supprime les lignes inutiles
  temp$group = tolower(temp$group)
  temp$group = gsub(' ','_',temp$group)
  temp = temp[-which(temp$text=="All"),]
  
  # Les joueurs ne sont pas forcément écrits dans le même ordre, il peuvent être différents et surtout 
  # il peut y en avoir un nombre différent en cas d'expulsion ou une autre variable peut s'être glissée au milieu.
  # Ici, je suis donc passé par une seconde table temporaire, qui isole les lignes décrivant l'effectif (group vaut NA) 
  # et les places dans la nouvelle base et leur attribuant un compteur par ID. (utilisation de la fonction mutate aidée par Google "create a counter")
  # Ce compteur est ensuite collé à la chaine 'joueur_' dans la variable group.
  temp2 = temp %>% filter(is.na(group)) %>%  group_by(ID) %>% mutate(counter = ave(text, ID, FUN = seq_along))
  temp2$group[is.na(temp2$group)] = 'joueur_' 
  for(j in 1:nrow(temp2)){
    if(temp2$group[j] == 'joueur_'){
      temp2$group[j] = paste0(temp2$group[j],temp2$counter[j])
    }
  } 
  # En refusionant cette table avec la première, temp, on obtient alors 2 variables group, mais jamais pleine en même temps,
  # en les superposant (unite), tout en supprimant les NA devenue une chaine de caractères, on obtient une variable group complète,
  # prête à être transposées en colonnes et à faire figure de noms de variables.
  temp2 = temp2[,c(1,5,6)]
  temp = merge(temp,temp2,by=c("ID","text"),all.x = TRUE) %>% unite_('group',c('group.x','group.y'),sep="")
  temp$group = gsub('NA','',temp$group,fixed = TRUE)
  
  # Une dernière modification pour avoir exactement les variables désirées en sortie.
  for(j in 1:nrow(temp)){
    if(temp$text[j] == 'Passes clees'){
      temp$group[j] = 'qualite_passe'
    }
  }
  
  # On peut maintenant transposer la base, en passant les valeurs de group en variables et celles de text en valeurs. Les autres variables restent.
  # Or, ces autres variables peuvent être les mêmes, bien que les lignes étant différentes via group et text.
  # La création très temporaire d'une variable row sert à rendre le pivot unique pour réaliser avec succès cette opération.
  temp = temp %>% mutate(row = row_number(rownames(temp))) %>% spread(group, text) %>% select(-row)
  
  # On se retrouve avec une matrice assez creuse, puisque une seule cellule par ligne est remplie (hors variables pivot).
  # Pour retrouver une seule ligne par ID qui soit complète, on va utiliser la fonction max, qui fonctionne aussi sur les chaines de caractères
  # Toutefois la fonction max renvoie toujours NA s'il y en a un, on remplace donc temporairement les NA par -- toujours inférieur à un autre caractère
  temp[is.na(temp)] = "--"
  temp = temp %>% group_by(ID) %>% summarise_all(funs(max))
  temp[temp=="--"] = NA
  
  # Au départ nous n'avions que la notion de dernière passe et de passé clée, on va ajouter une dimension de passe décisive manuellement, en repérant les buts
  temp$passe_dec = NA
  temp$qualite_passe_dec = NA
  for (j in 1:nrow(temp)){
    if(!is.na(temp$finalite[j]) & temp$finalite[j] == "But"){
      temp$passe_dec[j] = temp$dp[j]
      temp$qualite_passe_dec[j] = temp$qualite_passe[j]
    }
  }
  
  # Enfin, on créé une table par journée pour les fusionner plus tard et on supprime les tables temporaires
  assign(paste0('actions_j',i),temp)
  remove(list=c('temp','temp2'))
}

# Chaque base par journée à la même forme, on utilise donc rbind
actions_tot = rbind(actions_j1,actions_j2,actions_j3,actions_j4,actions_j5,actions_j6,
                    actions_j7,actions_j8,actions_j9,actions_j10,actions_j11,actions_j12,
                    actions_j13,actions_j14,actions_j15)


# Pour chaque ID il y a un start et un end en seconde depuis le début de saison. Lorsque la variable code est différente de Temps,
# il s'agit d'une action de jeu, on peut en récupérer la durée. 
# Lorsque la variable code vaut Temps, la différence entre start et end est le temps durant lequel l'effectif afférent est resté le même,
# et dans la même mi-temps. On peut donc récupérer cette durée qui en la sommant nous donnera le temps de jeu.
actions_tot$duree_action = NA
actions_tot$duree_action[which(actions_tot$code!="Temps")] = round(actions_tot$end - actions_tot$start,1)[which(actions_tot$code!="Temps")] 

actions_tot$duree_jeu = NA
actions_tot$duree_jeu[which(actions_tot$code=="Temps")] = round(actions_tot$end - actions_tot$start,1)[which(actions_tot$code=="Temps")] 


# On sépare en deux bases pour alléger un peu les calculs, l'effectif et les actions
actions_effectif = filter(actions_tot,code=='Temps') %>% select(c(-code,-duree_action,-av_av_dp,-av_dp,-dp,-finalite,-fnt,
                                                                  -periode,-qualite_passe,-score,-type_phase,-passe_dec,
                                                                  -qualite_passe_dec))

actions_jeu = filter(actions_tot,code!='Temps') %>% select(c(ID,start,end,duree_action,av_av_dp,av_dp,dp,finalite,fnt,
                                                             periode,qualite_passe,score,type_phase,passe_dec,
                                                             qualite_passe_dec,competition,journee,lieu))



## Je ne sais pas exactement pourquoi, mais la table est de classe tbl et pas dataframe, je le change donc pour travailler avec facilement
actions_effectif = as.data.frame(actions_effectif)

# Liste des joueurs ayant participé à au moins un match, elle servira régulièrement
# Il y a un NA qui correspond à un joueur exclu
joueurs = unique(c(actions_effectif$joueur_1,actions_effectif$joueur_2,actions_effectif$joueur_3,
                   actions_effectif$joueur_4,actions_effectif$joueur_5,actions_effectif$joueur_6,
                   actions_effectif$joueur_7,actions_effectif$joueur_8,actions_effectif$joueur_9,
                   actions_effectif$joueur_10))

# Table contenant l'effectif débutant chaque journée
titu = actions_effectif %>% group_by(journee) %>% summarise_all(funs(first))

## Cette table tableau_temps est celle qui servira à sortir le 11 type et à afficher le tableau récapitulatif du temps de jeu dans l'onglet équipe
tableau_temps = data.frame()
# Pour chaque joueur, NA compris, ayant joué au moins un bout de match
for(i in 1:length(joueurs)){
  # On traite d'abord le cas du NA en précisant que c'est un joueur exclu
  if(is.na(joueurs[i])){
    tableau_temps[i,1] = "Joueur(s) exclu(s)"
    # actions_effectif comporte 60 lignes, which lit la table colonne par colonne en additionnant les numéros de ligne,
    # le numéro réel de la ligne est donc le reste de la division euclidienne par 60.
    # En pensant à modifier le 0 qui correspond en fait à la ligne 60
    indic = which(is.na(actions_effectif[,c('joueur_1','joueur_2','joueur_3','joueur_4',
                                            'joueur_5','joueur_6','joueur_7','joueur_8',
                                            'joueur_9','joueur_10')])) %% nrow(actions_effectif)
    for(j in 1:length(indic)){if(indic[j]==0){indic[j]=nrow(actions_effectif)}} 
    # Temps de jeu total, ou ici d'inferiorité numérique, en minutes, d'où la division par 60
    tableau_temps[i,2] = round(sum(actions_effectif$duree_jeu[indic])/60)
    # Pourcentage par rapport au temps de jeu total sur 15 journées
    tableau_temps[i,3] = round(sum(actions_effectif$duree_jeu[indic])/actions_effectif$end[nrow(actions_effectif)],3)*100
    # Nombre de matchs joués, ici avec une exclusion
    tableau_temps[i,4] = length(unique(actions_effectif$journee[indic]))
    # Temps de jeu moyen par match joué, ici le temps moyen d'inferiorité numérique
    tableau_temps[i,5] = round(tableau_temps[i,2]/tableau_temps[i,4])
    # Pas de titularisation pour les cartons rouges
    tableau_temps[i,6] = NA
    
    # Puis on passe aux joueurs, avec exactement les mêmes colonnes donc on ne détaille que la dernière
  } else {
    tableau_temps[i,1] = joueurs[i]
    indic = which(joueurs[i] == actions_effectif[,c('joueur_1','joueur_2','joueur_3','joueur_4',
                                                    'joueur_5','joueur_6','joueur_7','joueur_8',
                                                    'joueur_9','joueur_10')]) %% nrow(actions_effectif)
    for(j in 1:length(indic)){if(indic[j]==0){indic[j]=60}} 
    tableau_temps[i,2] = round(sum(actions_effectif$duree_jeu[indic])/60)
    tableau_temps[i,3] = round(sum(actions_effectif$duree_jeu[indic])/actions_effectif$end[nrow(actions_effectif)],3)*100
    tableau_temps[i,4] = length(unique(actions_effectif$journee[indic]))
    tableau_temps[i,5] = round(tableau_temps[i,2]/tableau_temps[i,4])
    # Pour compter le nombre de titularisation, on utilise la table titu créée juste au-dessus et on va utiliser le même procédé que 
    # pour le temps de jeu, mais avec des divisions euclidiennes par 15 cette fois (nombre de lignes)
    indic = which(joueurs[i] == titu[,c('joueur_1','joueur_2','joueur_3','joueur_4',
                                        'joueur_5','joueur_6','joueur_7','joueur_8',
                                        'joueur_9','joueur_10')]) %% nrow(titu)
    # On exclu le NA du vecteur joueurs de cette condition car on ne voudrait pas qu'il récupère la ligne 15
    if(length(indic)!=0){for(j in 1:length(indic)){if(!is.na(indic[j]) & indic[j]==0){indic[j]=nrow(titu)}}} 
    tableau_temps[i,6] = length(unique(titu$journee[indic]))
    
  }
  remove(indic)
}

# On nomme les colonnes de façon claire
colnames(tableau_temps) = c("Joueur","Temps de jeu total","Ratio équipe (%)","Nombre de match(s)",
                            "Temps de jeu moyen (mn/match)","Nombre de titularisation(s)")

# On ajoute son poste majoritaire à chaque joueur sur le terrain
tableau_temps$Poste = NA
for(i in 1:nrow(tableau_temps)){
  if(tableau_temps$Joueur[i]=='Alba'){
    tableau_temps$Poste[i]='DG'
  }else if(tableau_temps$Joueur[i]=='Mascherano'){
    tableau_temps$Poste[i]='DCG'
  }else if(tableau_temps$Joueur[i]=='Roberto'){
    tableau_temps$Poste[i]='AD'
  }else if(tableau_temps$Joueur[i]=='Vermaelen'){
    tableau_temps$Poste[i]='DCD'
  }else if(tableau_temps$Joueur[i]=='Mathieu'){
    tableau_temps$Poste[i]='DCG'
  }else if(tableau_temps$Joueur[i]=='Busquets'){
    tableau_temps$Poste[i]='MC'
  }else if(tableau_temps$Joueur[i]=='Bartra'){
    tableau_temps$Poste[i]='DD'
  }else if(tableau_temps$Joueur[i]=='Suarez'){
    tableau_temps$Poste[i]='AC'
  }else if(tableau_temps$Joueur[i]=='Neymar'){
    tableau_temps$Poste[i]='AG'
  }else if(tableau_temps$Joueur[i]=='Rakitic'){
    tableau_temps$Poste[i]='MD'
  }else if(tableau_temps$Joueur[i]=='Pique'){
    tableau_temps$Poste[i]='DCD'
  }else if(tableau_temps$Joueur[i]=='Alves'){
    tableau_temps$Poste[i]='DD'
  }else if(tableau_temps$Joueur[i]=='Iniesta'){
    tableau_temps$Poste[i]='MG'
  }else if(tableau_temps$Joueur[i]=='Messi'){
    tableau_temps$Poste[i]='AD'
  }else if(tableau_temps$Joueur[i]=='El Haddadi'){
    tableau_temps$Poste[i]='AD'
  }else if(tableau_temps$Joueur[i]=='Ramirez'){
    tableau_temps$Poste[i]='AG'
  }else if(tableau_temps$Joueur[i]=='Adriano'){
    tableau_temps$Poste[i]='DG'
  }else if(tableau_temps$Joueur[i]=='Rafinha'){
    tableau_temps$Poste[i]='MG'
  }else if(tableau_temps$Joueur[i]=='Gumbau'){
    tableau_temps$Poste[i]='MC'
  }else if(tableau_temps$Joueur[i]=='Douglas'){
    tableau_temps$Poste[i]='DD'
  }else {
    tableau_temps$Poste[i]='ZZZ'
  }
}

# Ensuite pour placer convenablement les joueurs sur le terrain pour le onze type, on associe à chaque poste une valeur de x et de y
tableau_temps$x = NA
tableau_temps$y = NA
for(i in 1:nrow(tableau_temps)){
  if(tableau_temps$Poste[i]=='DD'){
    tableau_temps$x[i]=1
    tableau_temps$y[i]=1
  }else if(tableau_temps$Poste[i]=='DCD'){
    tableau_temps$x[i]=1
    tableau_temps$y[i]=2
  }else if(tableau_temps$Poste[i]=='DCG'){
    tableau_temps$x[i]=1
    tableau_temps$y[i]=3
  }else if(tableau_temps$Poste[i]=='DG'){
    tableau_temps$x[i]=1
    tableau_temps$y[i]=4
  }else if(tableau_temps$Poste[i]=='MD'){
    tableau_temps$x[i]=2
    tableau_temps$y[i]=1.5
  }else if(tableau_temps$Poste[i]=='MC'){
    tableau_temps$x[i]=2
    tableau_temps$y[i]=2.5
  }else if(tableau_temps$Poste[i]=='MG'){
    tableau_temps$x[i]=2
    tableau_temps$y[i]=3.5
  }else if(tableau_temps$Poste[i]=='AD'){
    tableau_temps$x[i]=3
    tableau_temps$y[i]=1.5
  }else if(tableau_temps$Poste[i]=='AC'){
    tableau_temps$x[i]=3
    tableau_temps$y[i]=2.5
  }else if(tableau_temps$Poste[i]=='AG'){
    tableau_temps$x[i]=3
    tableau_temps$y[i]=3.5
  }
}

# Et enfin on donne comme nom de ligne le numéro du joueur associé
rownames(tableau_temps) = c('18','14','20','23','24','5','15','9','3','8','17','11','4','6','10','19','21','28','2','12','--')


## On passe ensuite aux actions de jeu, tirs, passes, occasions etc...
# Comme pour action effectif il faut changer la classe de actions_jeu pour un data.frame
actions_jeu = as.data.frame(actions_jeu)

# Une cellule de finalité n'était pas renseignée, pour ne pas trop perdre de sens, on suppose que c'est l'action la plus faible, une demi-occasion
# (on voit avec l'évolution du score que ce n'est pas un but)
actions_jeu$finalite[is.na(actions_jeu$finalite)] = '1/2 occasions'

## Dans la base de départ on a juste l'information But du Barça et l'écart au score, on va en déduire le score à tout moment et surtout à la fin des matchs
# On va avoir besoin des buts inscrits dans une variable et de décaler l'écart au score pour qu'il change sur la même ligne que le but inscrit
actions_jeu = actions_jeu %>% group_by(journee) %>%
  mutate(but_pour = cumsum(ifelse(finalite=='But',1,0)),score2 = lead(score))

# On gère les cas particuliers
actions_jeu$score2 = as.numeric(actions_jeu$score2)
for(i in 1:nrow(actions_jeu)){
  if(is.na(actions_jeu$score2[i])){
    actions_jeu$score2[i]=actions_jeu$score2[i-1]
  }
}

# On calcule les buts contres grâce à l'écart décalé et aux buts pour
actions_jeu$but_contre = actions_jeu$but_pour - actions_jeu$score2

# On en déduit en fin le score total qui nous servira ensuite pour le parcours en championnat
actions_jeu = actions_jeu %>% select(-score2) %>% mutate(score_match = paste0(but_pour,'-',but_contre))

## Que ce soit pour les graphiques qui zooment ou le radar on a besoin d'avoir toutes les infos au niveau joueur,
## comme les occasions, les buts, les passes clées, les passées décisives, l'éfficacité ou encore reprendre les informations 
## sur le temps de jeu.
# Pour cela on va largement utilisé le fonction table, en la mettant en data.frame et en adaptant sa forme avec spread
occaz_but = spread(as.data.frame(table(actions_jeu$fnt,actions_jeu$finalite)),Var2,Freq)
colnames(occaz_but)[1] = "Joueur"
occaz_but$Efficacite = round(occaz_but$But/(occaz_but$But+occaz_but$Occasions)*100)

passes_d = as.data.frame(table(actions_jeu$passe_dec))
colnames(passes_d) = c("Joueur","passes_decisives")

passes_clees_dp = as.data.frame(table(actions_jeu$dp[which(actions_jeu$qualite_passe=='Passes clees')]))
colnames(passes_clees_dp) = c('Joueur','passes_clees_dp')
passes_clees_av_dp = as.data.frame(table(actions_jeu$av_dp[which(actions_jeu$qualite_passe=='Passes clees')]))
colnames(passes_clees_av_dp) = c('Joueur','passes_clees_av_dp')
passes_clees_av_av_dp = as.data.frame(table(actions_jeu$av_av_dp[which(actions_jeu$qualite_passe=='Passes clees')]))
colnames(passes_clees_av_av_dp) = c('Joueur','passes_clees_av_av_dp')

passes_clees = merge(passes_clees_dp,passes_clees_av_dp,by=c('Joueur'),all=TRUE)
passes_clees = merge(passes_clees,passes_clees_av_av_dp,by=c('Joueur'),all=TRUE)
passes_clees$passes_clees = rowSums(passes_clees[,-1],na.rm = TRUE)

## Enfin on fusionne toutes ces tables pour avoir une base avec toutes ces infos
base_finale = merge(passes_clees[,c(1,5)],passes_d,by=c('Joueur'),all=TRUE)
base_finale = merge(occaz_but[,c(1,3,5)],base_finale,by=c('Joueur'),all=TRUE)
base_finale = merge(tableau_temps[,c(1,5,6)],base_finale,by=c('Joueur'),all=TRUE)

base_finale = base_finale[-which(base_finale$Joueur=="Joueur(s) exclu(s)"),]

## On norme les valeurs, en pourcentage, pour le radar plus tard
base_finale$`Temps de jeu moyen (mn/match)` = round(base_finale$`Temps de jeu moyen (mn/match)`*100/94)
base_finale$`Nombre de titularisation(s)` = round(base_finale$`Nombre de titularisation(s)`/15*100)
base_finale$But = round(base_finale$But/max(base_finale$But,na.rm = T)*100)
base_finale$passes_clees = round(base_finale$passes_clees/max(base_finale$passes_clees,na.rm = T)*100)
base_finale$passes_decisives = round(base_finale$passes_decisives/max(base_finale$passes_decisives,na.rm = T)*100)

## On a pour l'instant une colonne par type d'action, mais pour les graphiques on aura besoin d'une colonne par joueur donc 
## on pivote et on fait les petites modifs qui vont bien 
base_finale = t(base_finale)
colnames(base_finale) = base_finale[1,]
base_finale = as.data.frame(base_finale[-1,])
rownames(base_finale) = NULL
# La fonction unfactor, du package zoo, est super pratique quand R choisit le type facteur par défaut
for(i in 1:ncol(base_finale)){base_finale[,i]= unfactor(base_finale[,i])}
base_finale[is.na(base_finale)] = 0


## Enfin on créé une table avec l'adversaire, le lieu, la compétition etc. pour l'utiliser dans le menu 
match = actions_tot %>% select(competition,journee,lieu) %>% group_by(journee) %>% summarise_all(funs(first))
# Méthode de clodo pour remettre les matchs dans le bon ordre, qui n'est pas l'alphabétique
match = as.data.frame(match[c(1,8:15,2:7),])

match$adversaire = NA
for(i in 1:nrow(match)){
  if(match$journee[i]=='J1'){
    match$adversaire[i] = 'Athletic Bilbao'
  } else if(match$journee[i]=='J2'){
    match$adversaire[i] = 'Malaga'
  } else if(match$journee[i]=='J3'){
    match$adversaire[i] = 'Atletico Madrid'
  } else if(match$journee[i]=='J4'){
    match$adversaire[i] = 'Levante'
  } else if(match$journee[i]=='J5'){
    match$adversaire[i] = 'Celta Vigo'
  } else if(match$journee[i]=='J6'){
    match$adversaire[i] = 'Las Palmas'
  } else if(match$journee[i]=='J7'){
    match$adversaire[i] = 'Séville'
  } else if(match$journee[i]=='J8'){
    match$adversaire[i] = 'Rayo Vallecano'
  } else if(match$journee[i]=='J9'){
    match$adversaire[i] = 'SD Eibar'
  } else if(match$journee[i]=='J10'){
    match$adversaire[i] = 'Getafe'
  } else if(match$journee[i]=='J11'){
    match$adversaire[i] = 'Villarreal'
  } else if(match$journee[i]=='J12'){
    match$adversaire[i] = 'Real Madrid'
  } else if(match$journee[i]=='J13'){
    match$adversaire[i] = 'Real Sociedad'
  } else if(match$journee[i]=='J14'){
    match$adversaire[i] = 'FC Valence'
  } else if(match$journee[i]=='J15'){
    match$adversaire[i] = 'La Corogne'
  } else {
    match$adversaire[i] = 'Error'
  }
}


## On va associer 3 points à la victoire et 1 point aux matchs nuls pour suivre l'évolution du nombre de points en championnat
score_final = actions_jeu %>% group_by(journee) %>% summarise_all(funs(last)) %>%
  select(journee,but_pour,but_contre,score_match,start,lieu)

# Cette fois, en gardant temporairement la variable start, qui est numérique, ça permet de trier les journées dans l'ordre, plus proprement
score_final = merge(score_final,match,by='journee') %>% arrange(start) %>% 
# Les points pour le classement selon le résultat du match
  mutate(points = ifelse(but_pour>but_contre,3,
                         ifelse(but_pour==but_contre,1,0)),
         # Le résultat en lettres pour l'afficher dans le tooltip (l'étiquette) 
         resultat = ifelse(but_pour>but_contre,"Victoire",
                           ifelse(but_pour==but_contre,"Nul","Défaite")),
         # Le nom 'y' est important pour le graphique car highcharts quand on donne les bons noms au variables peut comprendre des choses automatiquement
         y = cumsum(points),
         # Idem pour le x, qui n'a pas de sens ici, mais avec un x numérique on fera ce qu'on veut dans le graphique
         x = as.numeric(0:(nrow(score_final)-1)),
         # On prépare ce qui sera affiché dans le tooltip
         info = paste0(journee,' (',substr(score_final$lieu,1,3),') ',adversaire,' : ',resultat,' ',score_match)) %>%
  select(x,y,journee,info)


#### Dessin de l'interface
## Comme précisé plus haut dans un dashboard on a une structure en 3 éléments
## On retrouve bien ici le dashboardHeader, le dashboardSidebar avec les différents onglets et items (choix du match 
## et vue synthétique de l'équipe). Les petites icônes sont là pour égayer un peu le menu, on en trouve un 
## large choix en cherchant sur internet. Et enfin le dashboardBody qui fait appel aux onglet du menu et diffuse 
## les onglets créés dans le server.


ui <- dashboardPage(
  ### Pour être tout à fait honnête je ne comprend pas vraiment le fonctionnement des tags, j'ai vu sur Internet qu'il en fallait,
  ### ceux-là me donnent ce que je veux, mais je ne sais pas du tout pourquoi il en faut.
  ### En tout cas il y a le $a pour ajouter des éléments au titre, le $img pour mettre le fanion du Barça et le $li pour modifier
  ### le style et la taille de l'écriture. Je ne sais pas non plus pourquoi la classe dropdown, mais ça fonctionne.
  ## Partie en haut à gauche
  dashboardHeader(title = tags$a(href='https://fr.wikipedia.org/wiki/Saison_2015-2016_du_FC_Barcelone',
                                 tags$img(src='fanion.png',height="120px")),
                  titleWidth = 324,
                  ## Partie en haut à droite
                  # Pour le style, valeurs trouvées en tatonnant
                  tags$li(a("Saison 2015-2016",style = "font-size: 28px; padding-top:62px; padding-right:5% ;"),
                          align='right',
                          class="dropdown")),
  ## Création du menu sur la gauche, on trouve beaucoup d'exemples sur internet en général et sur le tuto Shiny Dashboard
  dashboardSidebar(width = 324,
                   sidebarMenu(id = "sidebar_menu",
                               menuItem("Vue d'ensemble de l'équipe", tabName = "equipe", icon = icon("dashboard")),
                               menuItem("Choix du match", icon = icon("calendar"),tabName = "journee"),
                               conditionalPanel(
                                 # Le choix de la journée n'apparait que si l'utilisateur a cliqué sur "Choix du match"
                                 condition = "input.sidebar_menu == 'journee'",
                                 selectInput(inputId = "journee_match",
                                             # C'est ici qu'on utilise les informations compilées dans la table 'match'
                                             # pour permettre la sélection du match (qui pour l'instant n'est qu'apparence
                                             # dans notre appli)
                                             choices=list('Domicile'=paste(match$competition,match$journee,":",match$adversaire,sep=" ")[which(match$lieu=='Domicile')],
                                                          'Extérieur'=paste(match$competition,match$journee,":",match$adversaire,sep=" ")[which(match$lieu=='Exterieur')]),
                                             selected = "CPN J9 : SD Eibar",
                                             label = "Journée :"
                                 )
                               )
                   )
  ),
  
  ### Création du corps de la page Web.
  ## Tout d'abord, le useShinyjs et les tags qui sont en dessous servent à appeler le fichier css présent dans le dossier www
  ## Ce dossier permet de changer le fond de la page, la taille et les fonds des head et side bar ou encore la disposition d'une box plus tard
  ## Il y a ensuite plusieurs méthodes de définition du corps de la page. Pour l'onglet de l'équipe, seul le clic sur le menu
  ## change le corps de la page, une fois qu'on est dessus rien ne change, on peut donc définir le contenu dans le server,
  ## dans un renderUI qu'on appelle ici simplement dans le UI.
  ## Pour l'onglet, ou tabItem de comparaison de 2 joueurs sur un match on choisit les joueurs à l'intérieur, il doit 
  ## donc être réactif. Pour cela, chaque partie, ou box, doit être définie ici. En utilisant tout de même des éléments
  ## render (UI pour les profils, ou Highchart) créés dans le server.
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    fluidRow(
      tabItems(
        tabItem(tabName = "equipe", uiOutput("body_equipe")),
        tabItem(tabName = "journee", 
                # C'est très clair dans le tutoriel Shiny Dashboard, pour pouvoir gérer la disposition en ligne et en colonne
                # il faut utiliser les fonctions fluidRow et column, en imbriquant les colonnes dans les lignes
                # Ensuite on peut écrire du texte en HTML, intégrer des widgets Shiny (selectInput) et des objets render du server
                # pour ces derniers, on appelle un renderName avec nameOutput, en utilisant le name adapté bien sûr
                list(
                  # En css, le corps de la page et divisé en 12, et il est impossible de mettre des virgules, on doit donc choisir 
                  # la taille des colonnes parmi 12 nombres entiers. Le offset défini l'écart avec le bord ou l'élément de gauche
                  fluidRow(column(width = 6, offset = 0,
                                  box(width=NULL,
                                      title = "Profil Joueur 1", status = "primary",
                                      selectInput(inputId = "choix_j1",
                                                  choices=Barca_Eibar$Joueur[-nrow(Barca_Eibar)],
                                                  selected = "Neymar",
                                                  label = "Choix du premier joueur à comparer :"
                                      ),
                                      uiOutput("infos_j1")
                                  )
                                  
                  ),
                  column(width = 6, offset = 0,
                         box(width=NULL,
                             title = "Profil Joueur 2", status = "danger",
                             selectInput(inputId = "choix_j2",
                                         choices=Barca_Eibar$Joueur[-nrow(Barca_Eibar)],
                                         selected = "Suarez",
                                         label = "Choix du second joueur à comparer :"
                             ),
                             uiOutput("infos_j2")
                         )
                         
                  )
                  ),
                  fluidRow(column(width = 12, offset = 0,
                                  # À la place de box, pour ne pas avoir de contour, il est aussi possible de créer un div
                                  # On peut en changer la couleur et la transparence dans style, ainsi que la place que prendra 
                                  # le graphique dedans avec le pourcentage de width 
                                  div(
                                    style = 'background-color: rgba(255, 255, 255, 1); width: 100%; display: inline-block;',
                                    highchartOutput(outputId = 'spider_plot_j1_j2')
                                  )
                                  
                  )
                  ),
                  fluidRow(column(width = 12, offset = 0,
                                  div(
                                    style = 'background-color: rgba(255, 255, 255, 1); width: 100%; display: inline-block;',
                                    highchartOutput(outputId = 'attaque_pyramid_plot')
                                  )
                                  
                  )
                  ),
                  fluidRow(column(width = 12, offset = 0,
                                  div(
                                    style = 'background-color: rgba(255, 255, 255, 1); width: 100%; display: inline-block;',
                                    highchartOutput(outputId = 'defense_pyramid_plot')
                                  )
                                  
                  )
                  ),
                  fluidRow(column(width = 12, offset = 0,
                                  div(
                                    style = 'background-color: rgba(255, 255, 255, 1); width: 100%; display: inline-block;',
                                    highchartOutput(outputId = 'global_pyramid_plot')
                                  )
                                  
                  )
                  ),
                  fluidRow(column(width = 6, offset = 0,
                                  div(
                                    style = 'background-color: rgba(255, 255, 255, 1); width: 100%; display: inline-block;',
                                    highchartOutput(outputId = 'frappes_surface_j1')
                                  )
                                  
                  ),
                  column(width = 6, offset = 0,
                         div(
                           style = 'background-color: rgba(255, 255, 255, 1); width: 100%; display: inline-block;',
                           highchartOutput(outputId = 'frappes_surface_j2')
                         )
                         
                  )
                  ),
                  fluidRow(column(width = 6, offset = 0,
                                  div(
                                    style = 'background-color: rgba(255, 255, 255, 1); width: 100%; display: inline-block;',
                                    highchartOutput(outputId = 'frappes_cage_j1')
                                  )
                                  
                  ),
                  column(width = 6, offset = 0,
                         div(
                           style = 'background-color: rgba(255, 255, 255, 1); width: 100%; display: inline-block;',
                           highchartOutput(outputId = 'frappes_cage_j2')
                         )
                         
                  )
                  )
                ))
      )
    )
  )
)


#### Objets R à intégrer
### Création dans le server de tous les body, UI ou Highcharts qui sont, ou qui peuvent être, appelés dans le UI
server <- function(input, output, session) {
  
  ## Dans la box de sélection des joueurs on met un selectInput pour choisir les joueurs et en fonction on affiche leur profil
  ## Après avoir affiché la photo, le nom et le numéro, on souhaite diviser la box en 3, avec un drapeau à gauche,
  ## les infos au milieu et le fanion du Barça à droite. Pour cela on utilise les classes définies dans le fichier css
  ## du dossier www. Pour être honnête je n'en comprend pas exactement le mécanisme, mais en mettant bien les classes
  ## dans cet ordre et en gérant la taille dans le fichier css, on arrive à ce qu'on voulait !
  ## Par ailleurs, on va utiliser les fonctions HTML typiques de Shiny comme span, strong ou br pour écrire les infos 
  ## comme on le souhaite (voir tuto Shiny Lesson 2 si besoin).
  output$infos_j1 <- renderUI({
    list(
      p(img(src=Barca_Eibar$Photo[which(Barca_Eibar$Joueur==input$choix_j1)],width=120),align='center'),
      p(span(Barca_Eibar$Nom_complet[which(Barca_Eibar$Joueur==input$choix_j1)],style = "font-size: 28px;"), br(),
        span(paste("#",Barca_Eibar$Numero[which(Barca_Eibar$Joueur==input$choix_j1)]),style = "font-size: 24px; color:darkblue"),align='center'),
      div(class = 'niveau1_profil',
          div(class = 'niveau2_profil left', br(),p(img(src=Barca_Eibar$Drapeau[which(Barca_Eibar$Joueur==input$choix_j1)],width=130),align='center')), 
          div(class = 'niveau2_profil right', br(),p(img(src="https://pbs.twimg.com/media/DGk2O8jXcAExSgM.png",width=160),align='center')), 
          div(class = 'niveau2_profil center', p(span("Poste : "),strong(Barca_Eibar$Poste[which(Barca_Eibar$Joueur==input$choix_j1)]),
                                                 br(),
                                                 span("Nationalité : "),strong(Barca_Eibar$Nationalite[which(Barca_Eibar$Joueur==input$choix_j1)]),
                                                 br(),
                                                 span("Date de naissance : "),strong(Barca_Eibar$Date_naissance[which(Barca_Eibar$Joueur==input$choix_j1)]),
                                                 br(),
                                                 span("Taille : "),strong(Barca_Eibar$Taille[which(Barca_Eibar$Joueur==input$choix_j1)]),
                                                 br(),
                                                 span("Meilleur pied : "),strong(Barca_Eibar$Pied[which(Barca_Eibar$Joueur==input$choix_j1)]),align='center',style = "font-size: 16px;"))
      )
    )
  })
  
  output$infos_j2 <- renderUI({
    list(
      p(img(src=Barca_Eibar$Photo[which(Barca_Eibar$Joueur==input$choix_j2)],width=120),align='center'),
      p(span(Barca_Eibar$Nom_complet[which(Barca_Eibar$Joueur==input$choix_j2)],style = "font-size: 28px;"), br(),
        span(paste("#",Barca_Eibar$Numero[which(Barca_Eibar$Joueur==input$choix_j2)]),style = "font-size: 24px; color:darkred"),align='center'),
      div(class = 'niveau1_profil',
          div(class = 'niveau2_profil left', br(),p(img(src=Barca_Eibar$Drapeau[which(Barca_Eibar$Joueur==input$choix_j2)],width=130),align='center')), 
          div(class = 'niveau2_profil right', br(),p(img(src="https://pbs.twimg.com/media/DGk2O8jXcAExSgM.png",width=160),align='center')), 
          div(class = 'niveau2_profil center', p(span("Poste : "),strong(Barca_Eibar$Poste[which(Barca_Eibar$Joueur==input$choix_j2)]),
                                                 br(),
                                                 span("Nationalité : "),strong(Barca_Eibar$Nationalite[which(Barca_Eibar$Joueur==input$choix_j2)]),
                                                 br(),
                                                 span("Date de naissance : "),strong(Barca_Eibar$Date_naissance[which(Barca_Eibar$Joueur==input$choix_j2)]),
                                                 br(),
                                                 span("Taille : "),strong(Barca_Eibar$Taille[which(Barca_Eibar$Joueur==input$choix_j2)]),
                                                 br(),
                                                 span("Meilleur pied : "),strong(Barca_Eibar$Pied[which(Barca_Eibar$Joueur==input$choix_j2)]),align='center',style = "font-size: 16px;"))
      )
    )
  })
  
  
  ## Pour comparer deux joueurs, le radar est très adapté. On présente sur celui-ci des données défensives, de jeu,
  ## de passes et de buts. On commence par récupérer les variables dont on a besoin et ce pour tous les joueurs, 
  ## car on a besoin de récupérer la valeur maximum de l'équipe pour calculer le pourcentage qui sera affiché.
  ## Ensuite on filtre selon le joueur choisi dans le selectInput approprié (un pour j1, un autre pour j2).
  ## Enfin on transpose les données car pour un radar il faut une colonne et pas une ligne, on donne un nom de colonne 
  ## et on supprime les noms de ligne.
  #### Attention : Cette portion de code doit être écrite à l'intérieur d'une fonction reactive({}) pour que le
  #### contenu soit modifié à chaque fois que l'utilisateur change le joueur sélectionné
  spider_j1 = reactive({
    temp = Barca_Eibar[-nrow(Barca_Eibar),c('Buts','Tirs','Tirs_cadres','Passes_decisives','Passes_cles','Dribles_reussis',
                                            'Ballons_touches','Duel_aeriens','Tacles_reussis','Interceptions','Fautes')]
    for (j in 1:ncol(temp)) {
      temp[,j] = round(temp[,j]/max(temp[,j],na.rm = T)*100,1)
    }
    temp[is.na(temp)] = 0
    temp = temp[which(Barca_Eibar$Joueur==input$choix_j1),]
    temp = as.data.frame(t(temp))
    colnames(temp) = "Stats"
    rownames(temp) = NULL
    temp
  })
  
  
  spider_j2 = reactive({
    temp = Barca_Eibar[-nrow(Barca_Eibar),c('Buts','Tirs','Tirs_cadres','Passes_decisives','Passes_cles','Dribles_reussis',
                                            'Ballons_touches','Duel_aeriens','Tacles_reussis','Interceptions','Fautes')]
    for (j in 1:ncol(temp)) {
      temp[,j] = round(temp[,j]/max(temp[,j],na.rm = T)*100,1)
    }
    temp[is.na(temp)] = 0
    temp = temp[which(Barca_Eibar$Joueur==input$choix_j2),]
    temp = as.data.frame(t(temp))
    colnames(temp) = "Stats"
    rownames(temp) = NULL
    temp
  })
  
  
  ## On a construit deux bases, se mettant à jour, avec les données relatives aux deux joueurs sélectionnés
  ## On peut donc les utiliser pour tracer notre radar, ou spiderplot. Ce sera un graphique Highchart,
  ## il s'agit de graphiques codés en Javascript, adaptés à R via le package Highcharter.
  ## Le graphique doit être dynammique, puisque changeant avec les joueurs, donc on le met dans un renderHighchart
  output$spider_plot_j1_j2 = renderHighchart({  
    hc <- highchart() %>%
      hc_title(text="Influence dans le collectif",align='center',style=list(color="black")) %>%
      hc_subtitle(text="En pourcentage de la valeur maximale dans l'équipe",align='center') %>%
      # Un radar c'est finalement un graphique en ligne ou en aire (ici) avec des coordonnées polaires
      # On trouve facilement des exemples sur internet, c'est là que j'ai récupéré les options comme tickmarkPlacement
      hc_chart(type = "area", polar = TRUE) %>%
      hc_xAxis(categories = c('Buts','Tirs','Tirs cadrés','Passes décisives','Passes clés','Dribles réussis',
                              'Ballons touchés','Duel aériens','Tacles réussis','Interceptions','Fautes'),
               tickmarkPlacement = 'on',
               lineWidth = 0) %>%
      # Le tour peut être un cercle ou un polygone, avec beaucoup de variables comme ici, le polygone tend vers le cercle
      hc_yAxis(gridLineInterpolation = 'polygon',lineWidth = 0,min = 0,max=100) %>%
      # Tout n'est pas possible en R, pour le tooltip (ou étiquette) on retrouve donc du HTML pour lui donner
      # la forme souhaitée. Ici on fait ça pour pouvoir rajouter un % à la suite de la valeur du point.
      # On trouve facilement des exemples pour rajouter une unité sur internet pour récupérer une fonction comme ça
      hc_tooltip(headerFormat='<span style="font-size:11px">{point.x}</span><br>',
                 pointFormat='<span style="color:{series.color}">{series.name}</span>: <b>{point.y}%<br/>') %>%
      # On ajoute ici enfin les séries de données, en récupérant le nom du joueur concerné pour le nom de la série
      # et en mettant les couleurs qui vont bien.
      ## Attention : Pour pouvoir utiliser les données d'un reactive, il faut ajouter des () après le nom de l'objet réactif
      hc_add_series(name = Barca_Eibar$Nom_complet[which(Barca_Eibar$Joueur==input$choix_j1)], 
                    data = spider_j1()$Stats, pointPlacement = 'on',color = "#00529F") %>%
      hc_add_series(name = Barca_Eibar$Nom_complet[which(Barca_Eibar$Joueur==input$choix_j2)], 
                    data = spider_j2()$Stats, pointPlacement = 'on',color = "#93111d")
    
    hc})
  
  
  ### Ensuite pour comparer les deux joueurs, on va faire comme des pyramides des âges avec d'un côté les barres d'un joueur
  ### et en négatif celles de l'autre. Chacun dans sa couleur.
  ### Pour un soucis de sens et d'échelle on peut faire une pyramide pour l'attaque, une pour la défense
  ### et une pour le jeu en général (ballons touchés, passes réussies...)
  
  ## Un peu comme pour le radar on va construire des tables réactives avec les bonnes variables et en mettant
  ## des valeurs négatives pour le joueur 1 qui sera sur la gauche du graphique. Pour ainsi avoir le 0 au milieu
  pyramide_attaque_j1 = reactive({
    temp = Barca_Eibar[-nrow(Barca_Eibar),c('Tirs','Tirs_cadres','Buts','Passes_decisives','Dribles','Dribles_reussis','Hors_jeu','Erreurs')]
    temp[is.na(temp)] = 0
    temp = -temp[which(Barca_Eibar$Joueur==input$choix_j1),]
    temp = as.data.frame(t(temp))
    colnames(temp) = "Stats"
    rownames(temp) = NULL
    temp
  })
  
  pyramide_attaque_j2 = reactive({
    temp = Barca_Eibar[-nrow(Barca_Eibar),c('Tirs','Tirs_cadres','Buts','Passes_decisives','Dribles','Dribles_reussis','Hors_jeu','Erreurs')]
    temp[is.na(temp)] = 0
    temp = temp[which(Barca_Eibar$Joueur==input$choix_j2),]
    temp = as.data.frame(t(temp))
    colnames(temp) = "Stats"
    rownames(temp) = NULL
    temp
  })
  
  ## Encore une fois, de la même façon, on va utiliser les données pour tracer le graphique avec une série par joueur
  output$attaque_pyramid_plot = renderHighchart({  
    hc <- highchart() %>%
      hc_title(text="Jeu Offensif",align='center',style=list(color="black")) %>%
      # Il s'agit d'un barplot particulier, mais bar quand même
      hc_chart(type = "bar") %>%
      # Même si elles sont identiques de chaque côté, pour avoir la forme escomptée il faut répété deux fois les catégories
      # à afficher, dont une fois avec le opposite=T pour avoir l'axe des deux côtés
      hc_xAxis(list(categories = c('Tirs','Tirs cadrés','Buts','Passes décisives','Dribles','Dribles réussis','Hors jeu','Erreurs')),
               list(categories = c('Tirs','Tirs cadrés','Buts','Passes décisives','Dribles','Dribles réussis','Hors jeu','Erreurs'),
                    opposite=T,linkedTo =0)) %>%
      # Pour que les barres s'opposent on a mis négatives les valeurs du premier joueur, mais c'est positif en réalité
      # Donc valeur absolue sur l'axe. Et pour cela il faut utiliser une fonction Javascript. On peut l'intégrer
      # grâce à la fonction JS.
      hc_yAxis(title = list(text = NULL),labels = list(formatter = JS("function () {
          return Math.abs(this.value); }")),
               # Le spider c'était entre 0 et 100, ici on doit bien gérer le min et le max du graphique pour qu'il 
               # soit bien centré autour du 0. On prend donc le max de toutes les valeurs en positif et en négatif.
               # On ajoute le +1 ou -1 car c'est plus joli si la barre la plus longue ne touche pas le bout.
               max=max(abs(pyramide_attaque_j1()$Stats),pyramide_attaque_j2()$Stats)+1,
               min=-max(abs(pyramide_attaque_j1()$Stats),pyramide_attaque_j2()$Stats)-1) %>%
      # Option pour que les barres des deux joueurs s'opposent bien aux mêmes niveaux
      hc_plotOptions(series=list(stacking='normal')) %>%
      # Idem pour le tooltip, il faut une fonction JS pour mettre la valeur absolue
      hc_tooltip(formatter = JS("function () {
              return '<b>' + this.point.category + '</b><br/>' +
      this.series.name + ' :  '+ Math.abs(this.point.y);
      }")) %>%
      hc_add_series(name = Barca_Eibar$Nom_complet[which(Barca_Eibar$Joueur==input$choix_j1)], data = pyramide_attaque_j1()$Stats,color = "#00529F") %>%
      hc_add_series(name = Barca_Eibar$Nom_complet[which(Barca_Eibar$Joueur==input$choix_j2)], data = pyramide_attaque_j2()$Stats,color = "#93111d")
    
    hc})
  
  ### Idem pour les autres pyramides
  
  pyramide_defense_j1 = reactive({
    temp = Barca_Eibar[-nrow(Barca_Eibar),c('Tacles','Tacles_reussis','Interceptions','Degagements','Duel_aeriens','Fautes')]
    temp[is.na(temp)] = 0
    temp = -temp[which(Barca_Eibar$Joueur==input$choix_j1),]
    temp = as.data.frame(t(temp))
    colnames(temp) = "Stats"
    rownames(temp) = NULL
    temp
  })
  
  pyramide_defense_j2 = reactive({
    temp = Barca_Eibar[-nrow(Barca_Eibar),c('Tacles','Tacles_reussis','Interceptions','Degagements','Duel_aeriens','Fautes')]
    temp[is.na(temp)] = 0
    temp = temp[which(Barca_Eibar$Joueur==input$choix_j2),]
    temp = as.data.frame(t(temp))
    colnames(temp) = "Stats"
    rownames(temp) = NULL
    temp
  })
  
  
  output$defense_pyramid_plot = renderHighchart({  
    hc <- highchart() %>%
      hc_title(text="Jeu Défensif",align='center',style=list(color="black")) %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(list(categories = c('Tacles','Tacles réussis','Interceptions','Dégagements','Duel aériens','Fautes')),
               list(categories = c('Tacles','Tacles réussis','Interceptions','Dégagements','Duel aériens','Fautes'),opposite=T,linkedTo =0)) %>%
      hc_yAxis(title = list(text = NULL),labels = list(formatter = JS("function () {
                                                                      return Math.abs(this.value); }")),
               max=max(abs(pyramide_defense_j1()$Stats),pyramide_defense_j2()$Stats)+1,
               min=-max(abs(pyramide_defense_j1()$Stats),pyramide_defense_j2()$Stats)-1) %>%
      hc_plotOptions(series=list(stacking='normal')) %>%
      hc_tooltip(formatter = JS("function () {
                                return '<b>' + this.point.category + '</b><br/>' +
                                this.series.name + ' :  '+ Math.abs(this.point.y);
                                }")) %>%
      hc_add_series(name = Barca_Eibar$Nom_complet[which(Barca_Eibar$Joueur==input$choix_j1)], data = pyramide_defense_j1()$Stats,color = "#00529F") %>%
      hc_add_series(name = Barca_Eibar$Nom_complet[which(Barca_Eibar$Joueur==input$choix_j2)], data = pyramide_defense_j2()$Stats,color = "#93111d")
    
    hc})
  
  
  pyramide_global_j1 = reactive({
    temp = Barca_Eibar[-nrow(Barca_Eibar),c('Minutes','Ballons_touches','Passes','Passes_cible_30m_off','Passes_cles')]
    temp[is.na(temp)] = 0
    temp = -temp[which(Barca_Eibar$Joueur==input$choix_j1),]
    temp = as.data.frame(t(temp))
    colnames(temp) = "Stats"
    rownames(temp) = NULL
    temp
  })
  
  pyramide_global_j2 = reactive({
    temp = Barca_Eibar[-nrow(Barca_Eibar),c('Minutes','Ballons_touches','Passes','Passes_cible_30m_off','Passes_cles')]
    temp[is.na(temp)] = 0
    temp = temp[which(Barca_Eibar$Joueur==input$choix_j2),]
    temp = as.data.frame(t(temp))
    colnames(temp) = "Stats"
    rownames(temp) = NULL
    temp
  })
  
  
  output$global_pyramid_plot = renderHighchart({  
    hc <- highchart() %>%
      hc_title(text="Jeu Collectif",align='center',style=list(color="black")) %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(list(categories = c('Minutes','Ballons Touchés','Passes','Passes dans les 30 derniers mètres','Passes clés')),
               list(categories = c('Minutes','Ballons Touchés','Passes','Passes dans les 30 derniers mètres','Passes clés'),opposite=T,linkedTo =0)) %>%
      hc_yAxis(title = list(text = NULL),labels = list(formatter = JS("function () {
                                                                      return Math.abs(this.value); }")),
               # Juste ici on adapte le min et le max pour que ce soit : soit 99+1, soit le nombre de ballons touchés +1
               max=max(99,abs(pyramide_global_j1()$Stats),pyramide_global_j2()$Stats)+1,
               min=-max(99,abs(pyramide_global_j1()$Stats),pyramide_global_j2()$Stats)-1) %>%
      hc_plotOptions(series=list(stacking='normal')) %>%
      hc_tooltip(formatter = JS("function () {
                                return '<b>' + this.point.category + '</b><br/>' +
                                this.series.name + ' :  '+ Math.abs(this.point.y);
}")) %>%
      hc_add_series(name = Barca_Eibar$Nom_complet[which(Barca_Eibar$Joueur==input$choix_j1)], data = pyramide_global_j1()$Stats,color = "#00529F") %>%
      hc_add_series(name = Barca_Eibar$Nom_complet[which(Barca_Eibar$Joueur==input$choix_j2)], data = pyramide_global_j2()$Stats,color = "#93111d")
    
    hc})
  
  
  ### Ensuite viennent les graphiques des différentes zones de tirs (départ du tir et cadrage à l'arrivée)
  ### Pour cela on suit le même fonctionnement des bases de données réactives.
  ### Respectivement pour les frappes hors cadres, les frapppes cadrées et les buts on va créer une base en filtrant
  ### sur le joueur sélectionné. 
  ### Dans la base importée on a déjà toutes les variables donnant le nombre de tirs par finalité et par zone
  ### on va donc conserver les variables adéquates par graphique parmis celles-là et surtout on va associer
  ### une valeur de x et une de y à chaque zone pour bien placer le point sur le graphique résultant.
  
  buts_surface_j1 = reactive({
    temp = Barca_Eibar[which(Barca_Eibar$Joueur==input$choix_j1),] %>% select(Tirs_6_metres_B,Tirs_surface_B,Tirs_exterieur_B)
    temp = as.data.frame(t(temp))
    colnames(temp) = 'name'
    temp$x = c(6,6,6)
    temp$y = c(7.2,5,1.1)
    temp$x[which(is.na(temp$name))] = NA
    temp$y[which(is.na(temp$name))] = NA
    rownames(temp) = NULL
    temp
  })  
  
  buts_surface_j2 = reactive({
    temp = Barca_Eibar[which(Barca_Eibar$Joueur==input$choix_j2),] %>% select(Tirs_6_metres_B,Tirs_surface_B,Tirs_exterieur_B)
    temp = as.data.frame(t(temp))
    colnames(temp) = 'name'
    temp$x = c(6,6,6)
    temp$y = c(7.2,5,1.1)
    temp$x[which(is.na(temp$name))] = NA
    temp$y[which(is.na(temp$name))] = NA
    rownames(temp) = NULL
    temp
  })
  
  
  cadres_surface_j1 = reactive({
    temp = Barca_Eibar[which(Barca_Eibar$Joueur==input$choix_j1),] %>% select(Tirs_6_metres_C,Tirs_surface_C,Tirs_exterieur_C)
    temp = as.data.frame(t(temp))
    colnames(temp) = 'name'
    temp$x = c(5,5,5)
    temp$y = c(7.2,5,1.1)
    temp$x[which(is.na(temp$name))] = NA
    temp$y[which(is.na(temp$name))] = NA
    rownames(temp) = NULL
    temp
  })  
  
  cadres_surface_j2 = reactive({
    temp = Barca_Eibar[which(Barca_Eibar$Joueur==input$choix_j2),] %>% select(Tirs_6_metres_C,Tirs_surface_C,Tirs_exterieur_C)
    temp = as.data.frame(t(temp))
    colnames(temp) = 'name'
    temp$x = c(5,5,5)
    temp$y = c(7.2,5,1.1)
    temp$x[which(is.na(temp$name))] = NA
    temp$y[which(is.na(temp$name))] = NA
    rownames(temp) = NULL
    temp
  })
  
  
  non_cadres_surface_j1 = reactive({
    temp = Barca_Eibar[which(Barca_Eibar$Joueur==input$choix_j1),] %>% select(Tirs_6_metres_NC,Tirs_surface_NC,Tirs_exterieur_NC)
    temp = as.data.frame(t(temp))
    colnames(temp) = 'name'
    temp$x = c(4,4,4)
    temp$y = c(7.2,5,1.1)
    temp$x[which(is.na(temp$name))] = NA
    temp$y[which(is.na(temp$name))] = NA
    rownames(temp) = NULL
    temp
  })  
  
  non_cadres_surface_j2 = reactive({
    temp = Barca_Eibar[which(Barca_Eibar$Joueur==input$choix_j2),] %>% select(Tirs_6_metres_NC,Tirs_surface_NC,Tirs_exterieur_NC)
    temp = as.data.frame(t(temp))
    colnames(temp) = 'name'
    temp$x = c(4,4,4)
    temp$y = c(7.2,5,1.1)
    temp$x[which(is.na(temp$name))] = NA
    temp$y[which(is.na(temp$name))] = NA
    rownames(temp) = NULL
    temp
  })
  
  
  ## Cette fois on aura un graphique par joueur, puisque les différentes séries sont déjà les différentes finalités de tirs
  ## et que c'est facile de comparer les deux joueurs en ayant les deux graphique l'un à côté de l'autre.
  ## Donc ici, pour le joueur 1 on va utiliser les trois bases de données réactives pour créer les 3 séries (buts, 
  ## frappes cadrées et frappes hors cadre).
  ## On va utiliser les thèmes qu'on a créé au début début de cadre pour la représentation du dernier tiers du terrain (surface)
  ## et ensuite la représentation de la cage et ses alentours (but).
  ## On créé un graphique scatterplot sur lequel les points seront placés grâce aux variables x et y suscitées.
  ## Enfin pour que ce soit plus joli, et plus clair, les points seront remplacés par des ballons de différentes couleurs
  ## qui ont été fait sous paint.net et placés sur mon github.
  output$frappes_surface_j1 = renderHighchart({
    highchart() %>% 
      hc_add_theme(surface) %>%
      hc_chart(type = "scatter") %>%
      hc_xAxis(visible=F,min=0,max=10) %>%
      hc_yAxis(visible=F,min = 0,max=10) %>%
      hc_add_series(name = "Frappes hors cadre",data = non_cadres_surface_j1(),
                    # Changement du symbole du marker par le lien vers un ballon
                    marker = list(symbol = 'url(https://github.com/BenjLasserre/Projet-Sportscode/raw/master/ballon_non_cadre2.png)')) %>% 
      hc_add_series(name = "Frappes cadrées",data = cadres_surface_j1(),
                    marker = list(symbol = 'url(https://github.com/BenjLasserre/Projet-Sportscode/raw/master/ballon_cadre2.png)')) %>% 
      hc_add_series(name = "Buts",data = buts_surface_j1,
                    marker = list(symbol = 'url(https://github.com/BenjLasserre/Projet-Sportscode/raw/master/ballon_but2.png)')) %>% 
      hc_title(text = "Zones de tirs") %>% 
      hc_subtitle(text=input$choix_j1,align='center') %>% 
      # Pour que ce soit plus clair on écrit en blanc le nombre de tirs dans la zone, qu'on a mis comme noms de variables
      hc_plotOptions(
        scatter = list(
          dataLabels = list(
            enabled = TRUE,
            format = '<span style="font-size:14px">{point.name}</span><br>',
            color = "white"
          ))) %>%
      hc_tooltip(enabled = FALSE) %>%
      hc_legend(enabled = T)
  })
  
  
  output$frappes_surface_j2 = renderHighchart({
    highchart() %>% 
      hc_add_theme(surface) %>%
      hc_chart(type = "scatter") %>%
      hc_xAxis(visible=F,min=0,max=10) %>%
      hc_yAxis(visible=F,min = 0,max=10) %>%
      hc_add_series(name = "Frappes hors cadre",data = non_cadres_surface_j2(),
                    marker = list(symbol = 'url(https://github.com/BenjLasserre/Projet-Sportscode/raw/master/ballon_non_cadre2.png)')) %>% 
      hc_add_series(name = "Frappes cadrées",data = cadres_surface_j2(),
                    marker = list(symbol = 'url(https://github.com/BenjLasserre/Projet-Sportscode/raw/master/ballon_cadre2.png)')) %>% 
      hc_add_series(name = "Buts",data = buts_surface_j2(),
                    marker = list(symbol = 'url(https://github.com/BenjLasserre/Projet-Sportscode/raw/master/ballon_but2.png)')) %>% 
      hc_title(text = "Zones de tirs") %>% 
      hc_subtitle(text=input$choix_j2,align='center') %>% 
      hc_plotOptions(
        scatter = list(
          dataLabels = list(
            enabled = TRUE,
            format = '<span style="font-size:14px">{point.name}</span><br>',
            color = "white"
          ))) %>%
      hc_tooltip(enabled = FALSE) %>%
      hc_legend(enabled = T)
  })
  
  
  ### Exactement la même méthode pour la cage sauf qu'on utilise le thème but
  buts_j1 = reactive({
    temp = Barca_Eibar[which(Barca_Eibar$Joueur==input$choix_j1),] %>% select(Tirs_BBG:Tirs_BHD)
    temp = as.data.frame(t(temp))
    colnames(temp) = 'name'
    temp$x = c(3.1,5.3,7.5,3.1,5.3,7.5)
    temp$y = c(3.6,3.6,3.6,5.6,5.6,5.6)
    temp$x[which(is.na(temp$name))] = NA
    temp$y[which(is.na(temp$name))] = NA
    rownames(temp) = NULL
    temp
  })  
  
  buts_j2 = reactive({
    temp = Barca_Eibar[which(Barca_Eibar$Joueur==input$choix_j2),] %>% select(Tirs_BBG:Tirs_BHD)
    temp = as.data.frame(t(temp))
    colnames(temp) = 'name'
    temp$x = c(3.1,5.3,7.5,3.1,5.3,7.5)
    temp$y = c(3.6,3.6,3.6,5.6,5.6,5.6)
    temp$x[which(is.na(temp$name))] = NA
    temp$y[which(is.na(temp$name))] = NA
    rownames(temp) = NULL
    temp
  })
  
  
  cadres_j1 = reactive({
    temp = Barca_Eibar[which(Barca_Eibar$Joueur==input$choix_j1),] %>% select(Tirs_CBG:Tirs_CHD)
    temp = as.data.frame(t(temp))
    colnames(temp) = 'name'
    temp$x = c(2.3,4.5,6.7,2.3,4.5,6.7)
    temp$y = c(3.6,3.6,3.6,5.6,5.6,5.6)
    temp$x[which(is.na(temp$name))] = NA
    temp$y[which(is.na(temp$name))] = NA
    rownames(temp) = NULL
    temp
  })  
  
  cadres_j2 = reactive({
    temp = Barca_Eibar[which(Barca_Eibar$Joueur==input$choix_j2),] %>% select(Tirs_CBG:Tirs_CHD)
    temp = as.data.frame(t(temp))
    colnames(temp) = 'name'
    temp$x = c(2.3,4.5,6.7,2.3,4.5,6.7)
    temp$y = c(3.6,3.6,3.6,5.6,5.6,5.6)
    temp$x[which(is.na(temp$name))] = NA
    temp$y[which(is.na(temp$name))] = NA
    rownames(temp) = NULL
    temp
  })
  
  
  non_cadres_j1 = reactive({
    temp = Barca_Eibar[which(Barca_Eibar$Joueur==input$choix_j1),] %>% select(Tirs_NCG:Tirs_NCHD)
    temp = as.data.frame(t(temp))
    colnames(temp) = 'name'
    temp$x = c(0.9,9.1,5,0.9,9.1)
    temp$y = c(5,5,8.1,8.1,8.1)
    temp$x[which(is.na(temp$name))] = NA
    temp$y[which(is.na(temp$name))] = NA
    rownames(temp) = NULL
    temp
  })  
  
  non_cadres_j2 = reactive({
    temp = Barca_Eibar[which(Barca_Eibar$Joueur==input$choix_j2),] %>% select(Tirs_NCG:Tirs_NCHD)
    temp = as.data.frame(t(temp))
    colnames(temp) = 'name'
    temp$x = c(0.9,9.1,5,0.9,9.1)
    temp$y = c(5,5,8.1,8.1,8.1)
    temp$x[which(is.na(temp$name))] = NA
    temp$y[which(is.na(temp$name))] = NA
    rownames(temp) = NULL
    temp
  })
  
  
  output$frappes_cage_j1 = renderHighchart({
    highchart() %>% 
      hc_add_theme(but) %>%
      hc_chart(type = "scatter") %>%
      hc_xAxis(visible=F,min=0,max=10) %>%
      hc_yAxis(visible=F,min = 0,max=10) %>%
      hc_add_series(name = "Frappes hors cadre",data = non_cadres_j1(),
                    marker = list(symbol = 'url(https://github.com/BenjLasserre/Projet-Sportscode/raw/master/ballon_non_cadre2.png)')) %>% 
      hc_add_series(name = "Frappes cadrées",data = cadres_j1(),
                    marker = list(symbol = 'url(https://github.com/BenjLasserre/Projet-Sportscode/raw/master/ballon_cadre2.png)')) %>% 
      hc_add_series(name = "Buts",data = buts_j1(),
                    marker = list(symbol = 'url(https://github.com/BenjLasserre/Projet-Sportscode/raw/master/ballon_but2.png)')) %>% 
      hc_title(text = "Zones de tirs") %>% 
      hc_subtitle(text=input$choix_j1,align='center') %>% 
      hc_plotOptions(
        scatter = list(
          dataLabels = list(
            enabled = TRUE,
            format = '<span style="font-size:14px">{point.name}</span><br>',
            color = "white"
          ))) %>%
      hc_tooltip(enabled = FALSE) %>%
      hc_legend(enabled = T)
  })
  
  
  output$frappes_cage_j2 = renderHighchart({
    highchart() %>% 
      hc_add_theme(but) %>%
      hc_chart(type = "scatter") %>%
      hc_xAxis(visible=F,min=0,max=10) %>%
      hc_yAxis(visible=F,min = 0,max=10) %>%
      hc_add_series(name = "Frappes hors cadre",data = non_cadres_j2(),
                    marker = list(symbol = 'url(https://github.com/BenjLasserre/Projet-Sportscode/raw/master/ballon_non_cadre2.png)')) %>% 
      hc_add_series(name = "Frappes cadrées",data = cadres_j2(),
                    marker = list(symbol = 'url(https://github.com/BenjLasserre/Projet-Sportscode/raw/master/ballon_cadre2.png)')) %>% 
      hc_add_series(name = "Buts",data = buts_j2(),
                    marker = list(symbol = 'url(https://github.com/BenjLasserre/Projet-Sportscode/raw/master/ballon_but2.png)')) %>% 
      hc_title(text = "Zones de tirs") %>% 
      hc_subtitle(text=input$choix_j2,align='center') %>%
      hc_plotOptions(
        scatter = list(
          dataLabels = list(
            enabled = TRUE,
            format = '<span style="font-size:14px">{point.name}</span><br>',
            color = "white"
          ))) %>%
      hc_tooltip(enabled = FALSE) %>%
      hc_legend(enabled = T)
  })
  
  
  
  ### Parmi plusieurs méthodes de définition du corps de la page, pour l'onglet de l'équipe, seul le clic sur le menu
  ### change le corps de la page, une fois qu'on est dessus rien ne change, on peut donc définir le contenu dans le server,
  ### dans un renderUI qu'on appelle simplement dans le UI.  
  output$body_equipe <- renderUI({
    
    ### Dans cette partie là, pas de reactive puisque pas de choix à faire. On utilise principalement les bases
    ### construites dans l'environnement et déjà commentées.
    
    ## A l'intérieur du renderUI il faut tout de même utiliser les autres render, comme renderHighchart
    output$points_equipe = renderHighchart({ 
      hc = highchart() %>%
        hc_title(text="Le parcours en championnat",align='center',style=list(color="black")) %>%
        hc_chart(type = "line") %>%
        hc_xAxis(title = "Journee",categories = score_final$journee) %>%
        hc_yAxis(title="Nombre de points") %>%
        ## Dans ce graphique on veut un affichage très particulier, avec une valeur différente entre l'axe des x
        ## et le tooltip. Pour reprendre la base score_final dont on a déjà commenté la construction.
        ## Highcharts va prendre automatiquement les x et les y comme valeurs pour l'axe, sauf qu'il va afficher les
        ## valeurs de journee comme catégories à la place. Puis dans une fonction JS on va utiliser directement
        ## les autres variables avec this.point.variable pour écrire ce que l'on veut
        hc_tooltip(formatter = JS(
          'function(){
    var out = this.point.info + "<b>" + "<br>" + this.point.y + " points" + "</b> "
    return out ;
    }'
        )) %>%
        ## Pour pouvoir mettre juste data = base, il faut avoir donné les bons noms de variable, x et y ici
        hc_add_series(name="Points",data = score_final, pointPlacement = 'on',color = "#00529F")
      
      hc })
    
    
    ## Pour créer un graphique drilldown la base de départ doit avoir une forme très particulière.
    ## Pour le niveau source il faut une liste avec les variables name, y et drilldown :
    # name contient les catégories à afficher, y les valeurs, et drilldown les valeurs qui 
    # seront appelées par les graphiques de niveau 2.
    ## C'est ce qui est fait ici avant de mettre le résultat sous forme de liste_parse, lui aussi nécessaire
    niv_1 = occaz_but %>% dplyr::select(Joueur,But) %>%
      dplyr::mutate(drilldown=Joueur) %>%
      dplyr::rename(name=Joueur,y=But)
    
    to_plot_niv1 <- highcharter::list_parse(niv_1)
    names(to_plot_niv1) <- NULL
    
    # Il faut ensuite créer des listes avec la catégories et la valeur à afficher (sans nom obligatoire cette fois)
    # pour chaque graphique de niveau 2 (un par drilldown du niveau 1).
    # Fonction pour optimiser ce traitement sur tous les joueurs.
    to_plot_niv2 = function(joueur){
      temp = occaz_but %>% dplyr::select(Joueur,But,`1/2 occasions`,Occasions) %>% dplyr::filter(Joueur == joueur) %>% 
        tidyr::gather(type,nombre,c(`1/2 occasions`,But,Occasions)) %>% dplyr::select(-Joueur) %>%
        highcharter::list_parse2()
      temp
    }
    
    for(i in 1:length(joueurs)){
      assign(paste0('to_plot_niv2_',gsub(' ','',joueurs[i])),to_plot_niv2(joueurs[i]))
    }
    
    
    ## On trace ensuite le graphique en drilldown. Encore une fois on trouve facilement des exemples sur internet
    output$buts_equipe = renderHighchart({  
      hc <- highchart()
      # Le type est celui des graphiques de niveau 2
      hc <- hc_chart(hc,type = "pie")
      hc <- hc_title(hc,text = "Buts et occasions des joueurs")
      hc <- hc_subtitle(hc,text = "Cliquer sur un joueur pour voir le détail de ses occasions")
      # L'axe de type catégorie indique juste que chaque valeur doit être un label sur l'axe
      hc <- hc_xAxis(hc,type = "category")
      hc <- hc_legend(hc,enabled = FALSE)
      # Dessin général des camemberts (niveau 2), le innersize est là pour les creuser (comme des donuts)
      hc <- hc_plotOptions(hc,series = list(borderWidth = 0),pie = list(size="71%",innerSize = '71%',colors=list("#00529F","#e5bd2b","#93111d"),
                                                                        dataLabels = list(enabled = TRUE, format = '<b>{point.name}</b>: {point.y}')))
      # Série pour le niveau 1
      hc <- hc_add_series(hc,name = "Buts",colorByPoint = FALSE,data = to_plot_niv1,color="#00529F",type = "column",
                          dataLabels = list(enabled = TRUE))
      # Toutes les séries pour le niveau 2, mais dans une fonction drilldown pour que le clic fonctionne
      hc <- hc_drilldown(hc,allowPointDrilldown = TRUE,activeDataLabelStyle=list(textDecoration='none',fontStyle='none'),
                         drillUpButton=list(relativeTo='spacingBox'),
                         series = list(list(id = "Adriano",data = to_plot_niv2_Adriano,name="Adriano"),
                                       list(id = "Alba",data = to_plot_niv2_Alba,name="Alba"),
                                       list(id = "Alves",data = to_plot_niv2_Alves,name="Alves"),
                                       list(id = "Bartra",data = to_plot_niv2_Bartra,name="Bartra"),
                                       list(id = "Busquets",data = to_plot_niv2_Busquets,name="Busquets"),
                                       list(id = "Douglas",data = to_plot_niv2_Douglas,name="Douglas"),
                                       list(id = "El Haddadi",data = to_plot_niv2_ElHaddadi,name="El Haddadi"),
                                       list(id = "Gumbau",data = to_plot_niv2_Gumbau,name="Gumbau"),
                                       list(id = "Iniesta",data = to_plot_niv2_Iniesta,name="Iniesta"),
                                       list(id = "Mascherano",data = to_plot_niv2_Mascherano,name="Mascherano"),
                                       list(id = "Mathieu",data = to_plot_niv2_Mathieu,name="Mathieu"),
                                       list(id = "Messi",data = to_plot_niv2_Messi,name="Messi"),
                                       list(id = "Neymar",data = to_plot_niv2_Neymar,name="Neymar"),
                                       list(id = "Pique",data = to_plot_niv2_Pique,name="Pique"),
                                       list(id = "Rafinha",data = to_plot_niv2_Rafinha,name="Rafinha"),
                                       list(id = "Rakitic",data = to_plot_niv2_Rakitic,name="Rakitic"),
                                       list(id = "Ramirez",data = to_plot_niv2_Ramirez,name="Ramirez"),
                                       list(id = "Roberto",data = to_plot_niv2_Roberto,name="Roberto"),
                                       list(id = "Suarez",data = to_plot_niv2_Suarez,name="Suarez"),
                                       list(id = "Vermaelen",data = to_plot_niv2_Vermaelen,name="Vermaelen")))
      ## Une fois les séries ajoutées, l'idée est de mettre la photo de chaque joueur au milieu du drilldown
      ## La solution que j'ai trouvé c'est de mettre une image de fond qui change en fonction du clic et d'en adapter la taille
      ## C'est une nouvelle fois une solution que j'ai trouvé et dont certaines parties m'échappent un peu
      ## La première partie avec les if est bien compréhensible, pour chaque joueur on met un lien avec des images types.
      ## C'est la dernière ligne qui devient plus complexe, on fait d'abord référence au nom Shiny de l'objet, but_equipe,
      ## tout comme dans le drillup. Puis je ne comprend pas vraiment les autres parenthèses mais j'ai joué sur les 50, 69 et 10%
      ## pour ajuster la taille. Fixer le 10%, en pourcentage, permet à ce que ça s'adapte à différent format d'écran.
      hc = hc_chart(
        hc,
        backgroundColor = NULL,
        events = list(
          drilldown = JS(
            "function(e){
          console.log(e.point.name);
          var image = 'none';
          if(e.point.name == 'Adriano') {
          image = 'http://img.uefa.com/imgml/TP/players/9/2015/324x324/98593.jpg';
          } else if (e.point.name == 'Alba') {
          image = 'http://img.uefa.com/imgml/TP/players/9/2015/324x324/250000036.jpg';
          } else if (e.point.name == 'Alves') {
          image = 'http://img.uefa.com/imgml/TP/players/9/2015/324x324/95724.jpg'
          } else if (e.point.name == 'Bartra') {
          image = 'http://thetopforward.com/uploads/0/Marc%20Bartra.jpg'
          } else if (e.point.name == 'Busquets') {
          image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/250002704.jpg'
          } else if (e.point.name == 'Douglas') {
          image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/250075674.jpg'
          } else if (e.point.name == 'El Haddadi') {
          image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/250063866.jpg'
          } else if (e.point.name == 'Gumbau') {
          image = 'http://img.uefa.com/imgml/TP/players/1/2016/324x324/250086317.jpg'
          } else if (e.point.name == 'Iniesta') {
          image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/58031.jpg'
          } else if (e.point.name == 'Mascherano') {
          image = 'http://www.thesportsdb.com/images/media/player/thumb/qrxpyw1431636012.jpg'
          } else if (e.point.name == 'Mathieu') {
          image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/65012.jpg'
          } else if (e.point.name == 'Messi') {
          image = 'https://ucl.truevisions.tv/images/topscore/Lionel%20Messi.jpg'
          } else if (e.point.name == 'Neymar') {
          image = 'http://media.baogiaothong.vn/files/Baogiay/2015/04/01/145-0716.jpg'
          } else if (e.point.name == 'Pique') {
          image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/93148.jpg'
          } else if (e.point.name == 'Rafinha') {
          image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/250013895.jpg'
          } else if (e.point.name == 'Rakitic') {
          image = 'http://img.uefa.com/imgml/TP/players/9/2015/324x324/97744.jpg'
          } else if (e.point.name == 'Ramirez') {
          image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/250042431.jpg'
          } else if (e.point.name == 'Roberto') {
          image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/250006092.jpg'
          } else if (e.point.name == 'Suarez') {
          image = 'http://media.footalist.com/compos/uploads/y.jpg'
          } else if (e.point.name == 'Vermaelen') {
          image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/71982.jpg'
          }
          $('#buts_equipe').css('background', 'url(\"'+image+'\") no-repeat 50% 69%').css({'background-size':'10%'});
  }"
          ),
          drillup = JS(
            "function(e){ console.log(e); $('#buts_equipe').css('background-image', 'none'); }"
          )
        ))
      hc })
    
    
    ### On reproduit exactement le même schéma pour les passes que pour les buts précédemment
    niv_1_passes = passes_clees %>% dplyr::select(Joueur,passes_clees) %>%
      dplyr::mutate(drilldown=Joueur) %>%
      dplyr::rename(name=Joueur,y=passes_clees)
    
    to_plot_niv1_passes <- highcharter::list_parse(niv_1_passes)
    names(to_plot_niv1_passes) <- NULL
    
    
    
    to_plot_niv2_passes = function(joueur){
      temp = passes_clees %>% dplyr::select(Joueur,passes_clees_dp,passes_clees_av_dp,passes_clees_av_av_dp) %>% 
        dplyr::filter(Joueur == joueur) %>% dplyr::rename(`Derniere passe`=passes_clees_dp,`Avant derniere passe`=passes_clees_av_dp,`Avant avant derniere passe`=passes_clees_av_av_dp) %>%
        tidyr::gather(type,nombre,c(`Avant avant derniere passe`,`Derniere passe`,`Avant derniere passe`)) %>% 
        dplyr::select(-Joueur) %>%
        highcharter::list_parse2()
      temp
    }
    
    for(i in 1:length(joueurs)){
      assign(paste0('to_plot_niv2_',gsub(' ','',joueurs[i]),'_passes'),to_plot_niv2_passes(joueurs[i]))
    }
    
    
    output$passes_equipe = renderHighchart({  
      hc <- highchart()
      hc <- hc_chart(hc,type = "pie")
      hc <- hc_title(hc,text = "Passes clées des joueurs")
      hc <- hc_subtitle(hc,text = "Cliquer sur un joueur pour voir le détail de ses passes clées")
      hc <- hc_xAxis(hc,type = "category")
      hc <- hc_legend(hc,enabled = FALSE)
      hc <- hc_plotOptions(hc,series = list(borderWidth = 0),pie = list(size="71%",innerSize = '71%',colors=list("#00529F","#e5bd2b","#93111d"),
                                                                        dataLabels = list(enabled = TRUE, format = '<b>{point.name}</b>: {point.y}')))
      hc <- hc_add_series(hc,name = "Passes Clées",colorByPoint = FALSE,data = to_plot_niv1_passes,color="#00529F",type = "column",
                          dataLabels = list(enabled = TRUE))
      hc <- hc_drilldown(hc,allowPointDrilldown = TRUE,activeDataLabelStyle=list(textDecoration='none',fontStyle='none'),
                         drillUpButton=list(relativeTo='spacingBox'),
                         series = list(list(id = "Adriano",data = to_plot_niv2_Adriano_passes,name="Adriano"),
                                       list(id = "Alba",data = to_plot_niv2_Alba_passes,name="Alba"),
                                       list(id = "Alves",data = to_plot_niv2_Alves_passes,name="Alves"),
                                       list(id = "Bartra",data = to_plot_niv2_Bartra_passes,name="Bartra"),
                                       list(id = "Busquets",data = to_plot_niv2_Busquets_passes,name="Busquets"),
                                       list(id = "Douglas",data = to_plot_niv2_Douglas_passes,name="Douglas"),
                                       list(id = "El Haddadi",data = to_plot_niv2_ElHaddadi_passes,name="El Haddadi"),
                                       list(id = "Gumbau",data = to_plot_niv2_Gumbau_passes,name="Gumbau"),
                                       list(id = "Iniesta",data = to_plot_niv2_Iniesta_passes,name="Iniesta"),
                                       list(id = "Mascherano",data = to_plot_niv2_Mascherano_passes,name="Mascherano"),
                                       list(id = "Mathieu",data = to_plot_niv2_Mathieu_passes,name="Mathieu"),
                                       list(id = "Messi",data = to_plot_niv2_Messi_passes,name="Messi"),
                                       list(id = "Neymar",data = to_plot_niv2_Neymar_passes,name="Neymar"),
                                       list(id = "Pique",data = to_plot_niv2_Pique_passes,name="Pique"),
                                       list(id = "Rafinha",data = to_plot_niv2_Rafinha_passes,name="Rafinha"),
                                       list(id = "Rakitic",data = to_plot_niv2_Rakitic_passes,name="Rakitic"),
                                       list(id = "Ramirez",data = to_plot_niv2_Ramirez_passes,name="Ramirez"),
                                       list(id = "Roberto",data = to_plot_niv2_Roberto_passes,name="Roberto"),
                                       list(id = "Suarez",data = to_plot_niv2_Suarez_passes,name="Suarez"),
                                       list(id = "Vermaelen",data = to_plot_niv2_Vermaelen_passes,name="Vermaelen")))
      hc = hc_chart(
        hc,
        backgroundColor = NULL,
        events = list(
          drilldown = JS(
            "function(e){
            console.log(e.point.name);
            var image = 'none';
            if(e.point.name == 'Adriano') {
            image = 'http://img.uefa.com/imgml/TP/players/9/2015/324x324/98593.jpg';
            } else if (e.point.name == 'Alba') {
            image = 'http://img.uefa.com/imgml/TP/players/9/2015/324x324/250000036.jpg';
            } else if (e.point.name == 'Alves') {
            image = 'http://img.uefa.com/imgml/TP/players/9/2015/324x324/95724.jpg'
            } else if (e.point.name == 'Bartra') {
            image = 'http://thetopforward.com/uploads/0/Marc%20Bartra.jpg'
            } else if (e.point.name == 'Busquets') {
            image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/250002704.jpg'
            } else if (e.point.name == 'Douglas') {
            image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/250075674.jpg'
            } else if (e.point.name == 'El Haddadi') {
            image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/250063866.jpg'
            } else if (e.point.name == 'Gumbau') {
            image = 'http://img.uefa.com/imgml/TP/players/1/2016/324x324/250086317.jpg'
            } else if (e.point.name == 'Iniesta') {
            image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/58031.jpg'
            } else if (e.point.name == 'Mascherano') {
            image = 'http://www.thesportsdb.com/images/media/player/thumb/qrxpyw1431636012.jpg'
            } else if (e.point.name == 'Mathieu') {
            image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/65012.jpg'
            } else if (e.point.name == 'Messi') {
            image = 'https://ucl.truevisions.tv/images/topscore/Lionel%20Messi.jpg'
            } else if (e.point.name == 'Neymar') {
            image = 'http://media.baogiaothong.vn/files/Baogiay/2015/04/01/145-0716.jpg'
            } else if (e.point.name == 'Pique') {
            image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/93148.jpg'
            } else if (e.point.name == 'Rafinha') {
            image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/250013895.jpg'
            } else if (e.point.name == 'Rakitic') {
            image = 'http://img.uefa.com/imgml/TP/players/9/2015/324x324/97744.jpg'
            } else if (e.point.name == 'Ramirez') {
            image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/250042431.jpg'
            } else if (e.point.name == 'Roberto') {
            image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/250006092.jpg'
            } else if (e.point.name == 'Suarez') {
            image = 'http://media.footalist.com/compos/uploads/y.jpg'
            } else if (e.point.name == 'Vermaelen') {
            image = 'http://img.uefa.com/imgml/TP/players/1/2015/324x324/71982.jpg'
            }
            $('#passes_equipe').css('background', 'url(\"'+image+'\") no-repeat 50% 69%').css({'background-size':'10%'});
    }"
          ),
          drillup = JS(
            "function(e){ console.log(e); $('#passes_equipe').css('background-image', 'none'); }"
          )
        ))
      hc })
    
    
    ### Pour les passes décisives c'est plus simple car pas de drilldown donc un graphique très classique en colonnes
    output$passes_d = renderHighchart({  
      hc <- highchart() %>%
        hc_title(text="Nombre de passes décisives") %>%
        hc_chart(type = "column") %>%
        hc_yAxis(title = list(text = "Nombre de passes décisives"), labels = list(format = '{value}')) %>%
        hc_xAxis(categories=passes_d$Joueur) %>%
        hc_add_series(name = "Passes Décisives",colorByPoint = FALSE,data = passes_d$passes_decisives,color="#00529F",
                      dataLabels = list(enabled = TRUE))
      
      hc})
    
    
    
    ## Pour ce radar plot on procède exactement comme celui qui compare 2 joueurs plus haut sauf qu'on en met 20
    ## On ajoute donc 20 séries, une par joueur, et on ne va faire apparaitre dans un premier temps que Messi, Neymar et Suarez
    ## Ensuite on peut librement sélectionner ou déselectionner un joueur dans l'affichage.
    ## On va choisir 4 couleurs (les trois du barça déjà utilisé et le gris anthracite foncé de la sidebar) et 5 symboles
    ## afin que chacun des 20 joueurs et une combinaison différente et soit ainsi différencié (en plus de l'étiquette)
    output$spider_plot = renderHighchart({  
      hc <- highchart() %>%
        hc_title(text="Comparaison des joueurs",align='left',style=list(color="black")) %>%
        hc_subtitle(text="En pourcentage de la valeur maximale",align='left') %>%
        hc_chart(type = "line", polar = TRUE) %>%
        hc_xAxis(categories = c("Temps de jeu moyen","Titularisations","Buts","Efficacité","Passes clées","Passes décisives"),
                 tickmarkPlacement = 'on',
                 lineWidth = 0) %>%
        hc_yAxis(gridLineInterpolation = 'polygon',lineWidth = 0,min = 0) %>%
        hc_legend(align ='right', verticalAlign ='top', layout='vertical') %>%
        hc_tooltip(headerFormat='<span style="font-size:11px">{point.x}</span><br>',
                   pointFormat='<span style="color:{series.color}">{series.name}</span>: <b>{point.y}%<br/>') %>%
        hc_add_series(name = 'Lionel Messi', data = base_finale$Messi, pointPlacement = 'on',color="#00529F") %>%
        hc_add_series(name = 'Luis Suarez', data = base_finale$Suarez, pointPlacement = 'on',color="#e5bd2b") %>%
        hc_add_series(name = 'Neymar JR', data = base_finale$Neymar, pointPlacement = 'on',color="#93111d") %>%
        hc_add_series(name = 'Sandro Ramirez', data = base_finale$Ramirez, pointPlacement = 'on',visible=F,color="#222d32") %>%
        hc_add_series(name = 'Munir El Haddadi', data = base_finale$`El Haddadi`, pointPlacement = 'on',visible=F,color="#00529F") %>%
        hc_add_series(name = 'Andres Iniesta', data = base_finale$Iniesta, pointPlacement = 'on',visible=F,color="#e5bd2b") %>%
        hc_add_series(name = 'Ivan Rakitic', data = base_finale$Rakitic, pointPlacement = 'on',visible=F,color="#93111d") %>%
        hc_add_series(name = 'Sergi Roberto', data = base_finale$Roberto, pointPlacement = 'on',visible=F,color="#222d32") %>%
        hc_add_series(name = 'Rafinha', data = base_finale$Rafinha, pointPlacement = 'on',visible=F,color="#00529F") %>%
        hc_add_series(name = 'Sergio Busquets', data = base_finale$Busquets, pointPlacement = 'on',visible=F,color="#e5bd2b") %>%
        hc_add_series(name = 'Gerard Gumbau', data = base_finale$Gumbau, pointPlacement = 'on',visible=F,color="#93111d") %>%
        hc_add_series(name = 'Jordi Alba', data = base_finale$Alba, pointPlacement = 'on',visible=F,color="#222d32") %>%
        hc_add_series(name = 'Dani Alves', data = base_finale$Alves, pointPlacement = 'on',visible=F,color="#00529F") %>%
        hc_add_series(name = 'Gerard Pique', data = base_finale$Pique, pointPlacement = 'on',visible=F,color="#e5bd2b") %>%
        hc_add_series(name = 'Marc Bartra', data = base_finale$Bartra, pointPlacement = 'on',visible=F,color="#93111d") %>%
        hc_add_series(name = 'Javier Mascherano', data = base_finale$Mascherano, pointPlacement = 'on',visible=F,color="#222d32") %>%
        hc_add_series(name = 'Adriano', data = base_finale$Adriano, pointPlacement = 'on',visible=F,color="#00529F") %>%
        hc_add_series(name = 'Douglas', data = base_finale$Douglas, pointPlacement = 'on',visible=F,color="#e5bd2b") %>%
        hc_add_series(name = 'Jeremy Mathieu', data = base_finale$Mathieu, pointPlacement = 'on',visible=F,color="#93111d") %>%
        hc_add_series(name = 'Thomas Vermaelen', data = base_finale$Vermaelen, pointPlacement = 'on',visible=F,color="#222d32") 
      
      hc})
    
    


    ## Afin de placer le 11 type sur un terrain, on va choisir par poste les 10 joueurs qui ont joué le plus (hors gardien)
    ## Puis on pourra les placer grâce aux valeurs de x et y par poste définies plus haut, sur un scatterplot,
    ## comme pour les zones de tirs. On fait alors appel au thème terrain défini tout en haut du code
    onze_type = tableau_temps %>% dplyr::rename(name=Joueur) %>% arrange(Poste,desc(`Temps de jeu total`)) %>%
      group_by(Poste) %>% summarise_all(funs(first)) %>% dplyr::slice(1:10) %>% dplyr::select(name,x,y)
    onze_type = as.data.frame(onze_type)
    
    output$onze_saison = renderHighchart({  
      hc <- highchart() %>% 
        hc_add_theme(terrain) %>% 
        hc_chart(type = "scatter") %>%
        hc_xAxis(type = "category",visible=F) %>%
        hc_yAxis(visible=F) %>%
        hc_add_series(data = onze_type, color = "#00529F") %>% 
        hc_title(text = "Le 11 type") %>% 
        hc_plotOptions(
          scatter = list(
            dataLabels = list(
              enabled = TRUE,
              format = '<span style="font-size:14px">{point.name}</span><br>',
              color = "white"
            ),
            marker = list(radius=8)
          )
        ) %>%
        hc_tooltip(enabled = FALSE)
      
      hc})
    
    
    
    ## Enfin on va afficher le tableau_temps en format DT (avec DT::renderDataTable) pour avoir un récap de ces infos
    ## de temps de jeu directement dans l'appli. Les rownames sont les numéros des joueurs si on se souvient bien.
    ## Ensuite on demande un affichage de 10 lignes qui correspondra bien au dessin du onze type au niveau de la taille.
    ## Puis au lieu d'avoir 2 pages et de devoir cliquer avec paging = FALSE et scrollY, on aura une seule page sur laquelle
    ## on pourra scroller. Je ne sais pas pourquoi le 300 fonctionne mais c'est un exemple que j'ai trouvé et ça marche bien.
    output$temps_jeu <- DT::renderDataTable({
      tableau_temps[,1:6]
    },
    options = list(scrollY = 300, ordering = TRUE, pageLength = 10, paging = FALSE),
    rownames = TRUE
    )
    
    
    #### Ensuite on va habiller le corps de la page avec tous les éléments créés exactement comme on l'a fait plus haut
    #### simplement ici, on le fait à l'intérieur du renderUI et ça fonctionnera en l'appelant
    list(
      
      fluidRow(column(width = 8, offset = 2,
                      h3("Depuis le début de saison",align='center',style = "color:white"),
                      br(),
                      div(
                        style = 'background-color: rgba(255, 255, 255, 1); width: 100%; display: inline-block;',
                        highchartOutput(outputId = 'points_equipe')
                      )
      )),
      fluidRow(column(width = 12, offset = 0,
                      tabBox(width = 12,
                             tabPanel("Buts",highchartOutput(outputId = "buts_equipe",height = '13.5cm')),
                             tabPanel("Passes Clées",highchartOutput(outputId = "passes_equipe",height = '13.5cm')),
                             tabPanel("Passes Décisives",highchartOutput(outputId = "passes_d",height = '13.5cm'))
                      )
                      
      )
      ),
      fluidRow(column(width = 8, offset = 2,
                      div(
                        style = 'background-color: rgba(255, 255, 255, 1); width: 100%; display: inline-block;',
                        highchartOutput(outputId = 'spider_plot')
                      )
      )),
      fluidRow(column(width = 6, offset = 0,
                      br(),
                      box(DT::dataTableOutput(outputId = 'temps_jeu'),width = NULL,height = "11cm",style = "font-size: 84%; width: 84%%;")
      ),
      column(width = 6, offset = 0,
             br(),
             div(
               style = 'background-color: rgba(255, 255, 255, 1); width: 100%; display: inline-block;',
               highchartOutput(outputId = 'onze_saison',height = "11cm")
             )
      ))
      
    )
    
  })
  
  
  
  ## Quand on utlise seul et, peut-être, sur le Server plus tard, il est mieux que l'application se ferme 
  ## à la fermeture de la page Web. C'est le rôle de cette fonction, mise en commentaire pour l'utilisation actuelle.
  #session$onSessionEnded(stopApp)
  
  #### Pour partager l'application on a choisi de la déployer sur Shinyapps.io, pour cela rien de sorcier, c'est 
  #### vraiment bien expliqué dans le tutoriel Shiny. Il suffit d'avoir un compte shinyapps.io, de le connecter
  #### (donc pas de proxy, attention) et de cliquer sur le symbole à côté de runApp.

}

shinyApp(ui, server)