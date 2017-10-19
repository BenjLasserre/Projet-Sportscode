## L'application Captain Dash étant très complexe l'utilisation de Shiny n'était pas suffisante, j'ai donc dû faire appel
## au package Shiny Dashboard, plus élaboré sur certains points.
## Son utilisation est très proche de celle de Shuny, c'est en fait le même environnement avec un UI et un Server.
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


stats_barca = read.table("Stats_Barca2.txt", header = TRUE,sep = ";", dec=",", na.strings = "",
                         colClasses = c("character","numeric","numeric","character","character","character"))
#load("stats_barca.RData")

Barca_Eibar <- read_excel("Barca_Eibar.xlsx")
Barca_Eibar$Date_naissance = format(Barca_Eibar$Date_naissance,format="%d/%m/%Y")

surface <- hc_theme_merge(
  hc_theme_tufte(),
  hc_theme(
    title = list(
      style = list(
        color = "black"
      )
    ),
    chart = list(
      backgroundColor = "transparent",
      plotBackgroundImage = "https://github.com/MathieuMarauri/shinyApps/raw/master/data/surface.jpg",
      style = list(fontFamily = "Century Gothic")
    )
  )
)

highchart() %>% 
  hc_add_theme(surface)


but <- hc_theme_merge(
  hc_theme_tufte(),
  hc_theme(
    title = list(
      style = list(
        color = "black"
      )
    ),
    chart = list(
      backgroundColor = "transparent",
      plotBackgroundImage = "https://github.com/MathieuMarauri/shinyApps/raw/master/data/cage.jpg",
      style = list(fontFamily = "Century Gothic")
    )
  )
)

highchart() %>% 
  hc_add_theme(but) %>%
  hc_chart(type = "scatter") %>%
  hc_xAxis(visible=T,min=0,max=10) %>%
  hc_yAxis(visible=T,min = 0) %>%
  hc_add_series(data = list(
    list(
      name = 3,
      x = 1.7,
      y = 3.5
    ),
    list(
      name = "Messi",
      x = 3,
      y = 4.15
    ),
    list(
      name = "Messi",
      x = 4,
      y = 3
    ),
    list(
      name = "Messi",
      x = 5,
      y = 3
    ),
    list(
      name = "Messi",
      x = 3,
      y = 5
    )
  ), color = "#00529F") %>% 
  hc_title(text = "Le 11 type") %>% 
  hc_plotOptions(
    scatter = list(
      dataLabels = list(
        enabled = TRUE,
        format = '<span style="font-size:14px">{point.name}</span><br>',
        color = "white"
      ),
      marker = list(symbol = 'url(https://www.highcharts.com/samples/graphics/sun.png)')
    )
  ) %>%
  hc_tooltip(enabled = FALSE)



for(i in 1:15){
  temp = stats_barca[which(stats_barca$ID %in% stats_barca$ID[which(stats_barca$text==paste0('J',i))]),]
  temp$group = tolower(temp$group)
  temp$group = gsub(' ','_',temp$group)
  #temp$group = iconv(temp$group, to="ASCII//TRANSLIT//IGNORE")
  #temp$group = gsub('é','e',temp$group)
  #temp$text = gsub('é','e',temp$text)
  temp = temp[-which(temp$text=="All"),]
  temp2 = temp %>% filter(is.na(group)) %>%  group_by(ID) %>% mutate(counter = ave(text, ID, FUN = seq_along))
  temp2$group[is.na(temp2$group)] = 'joueur_' 
  for(j in 1:nrow(temp2)){
    if(temp2$group[j] == 'joueur_'){
      temp2$group[j] = paste0(temp2$group[j],temp2$counter[j])
    }
  }
  temp2 = temp2[,c(1,5,6)]
  temp = merge(temp,temp2,by=c("ID","text"),all.x = TRUE) %>% unite_('group',c('group.x','group.y'),sep="")
  temp$group = gsub('NA','',temp$group,fixed = TRUE)
  
  
  for(j in 1:nrow(temp)){
    if(temp$text[j] == 'Passes clees'){
      temp$group[j] = 'qualite_passe'
    }
  }
  
  temp = temp %>% mutate(row = row_number(rownames(temp))) %>% spread(group, text) %>% select(-row)
  temp[is.na(temp)] = "--"
  temp = temp %>% group_by(ID) %>% summarise_all(funs(max))
  temp[temp=="--"] = NA
  temp$passe_dec = NA
  temp$qualite_passe_dec = NA
  for (j in 1:nrow(temp)){
    if(!is.na(temp$finalite[j]) & temp$finalite[j] == "But"){
      temp$passe_dec[j] = temp$dp[j]
      temp$qualite_passe_dec[j] = temp$qualite_passe[j]
    }
  }
  assign(paste0('actions_j',i),temp)
  remove(list=c('temp','temp2'))
}

actions_tot = rbind(actions_j1,actions_j2,actions_j3,actions_j4,actions_j5,actions_j6,
                    actions_j7,actions_j8,actions_j9,actions_j10,actions_j11,actions_j12,
                    actions_j13,actions_j14,actions_j15)


actions_tot$duree_action = NA
actions_tot$duree_action[which(actions_tot$code!="Temps")] = round(actions_tot$end - actions_tot$start,1)[which(actions_tot$code!="Temps")] 

actions_tot$duree_jeu = NA
actions_tot$duree_jeu[which(actions_tot$code=="Temps")] = round(actions_tot$end - actions_tot$start,1)[which(actions_tot$code=="Temps")] 


actions_effectif = filter(actions_tot,code=='Temps') %>% select(c(-code,-duree_action,-av_av_dp,-av_dp,-dp,-finalite,-fnt,
                                                                  -periode,-qualite_passe,-score,-type_phase,-passe_dec,
                                                                  -qualite_passe_dec))

actions_jeu = filter(actions_tot,code!='Temps') %>% select(c(ID,start,end,duree_action,av_av_dp,av_dp,dp,finalite,fnt,
                                                             periode,qualite_passe,score,type_phase,passe_dec,
                                                             qualite_passe_dec,competition,journee,lieu))




actions_effectif = as.data.frame(actions_effectif)

joueurs = unique(c(actions_effectif$joueur_1,actions_effectif$joueur_2,actions_effectif$joueur_3,
                   actions_effectif$joueur_4,actions_effectif$joueur_5,actions_effectif$joueur_6,
                   actions_effectif$joueur_7,actions_effectif$joueur_8,actions_effectif$joueur_9,
                   actions_effectif$joueur_10))

titu = actions_effectif %>% group_by(journee) %>% summarise_all(funs(first))

tableau_temps = data.frame()
for(i in 1:length(joueurs)){
  if(is.na(joueurs[i])){
    tableau_temps[i,1] = "Joueur(s) exclu(s)"
    indic = which(is.na(actions_effectif[,c('joueur_1','joueur_2','joueur_3','joueur_4',
                                            'joueur_5','joueur_6','joueur_7','joueur_8',
                                            'joueur_9','joueur_10')])) %% 60
    for(j in 1:length(indic)){if(indic[j]==0){indic[j]=60}} 
    tableau_temps[i,2] = round(sum(actions_effectif$duree_jeu[indic])/60)
    tableau_temps[i,3] = round(sum(actions_effectif$duree_jeu[indic])/actions_effectif$end[nrow(actions_effectif)],3)*100
    tableau_temps[i,4] = length(unique(actions_effectif$journee[indic]))
    tableau_temps[i,5] = round(tableau_temps[i,2]/tableau_temps[i,4])
    tableau_temps[i,6] = NA
    
  } else {
    tableau_temps[i,1] = joueurs[i]
    indic = which(joueurs[i] == actions_effectif[,c('joueur_1','joueur_2','joueur_3','joueur_4',
                                                    'joueur_5','joueur_6','joueur_7','joueur_8',
                                                    'joueur_9','joueur_10')]) %% 60
    for(j in 1:length(indic)){if(indic[j]==0){indic[j]=60}} 
    tableau_temps[i,2] = round(sum(actions_effectif$duree_jeu[indic])/60)
    tableau_temps[i,3] = round(sum(actions_effectif$duree_jeu[indic])/actions_effectif$end[nrow(actions_effectif)],3)*100
    tableau_temps[i,4] = length(unique(actions_effectif$journee[indic]))
    tableau_temps[i,5] = round(tableau_temps[i,2]/tableau_temps[i,4])
    indic = which(joueurs[i] == titu[,c('joueur_1','joueur_2','joueur_3','joueur_4',
                                        'joueur_5','joueur_6','joueur_7','joueur_8',
                                        'joueur_9','joueur_10')]) %% 15
    if(length(indic)!=0){for(j in 1:length(indic)){if(!is.na(indic[j]) & indic[j]==0){indic[j]=15}}} 
    tableau_temps[i,6] = length(unique(titu$journee[indic]))
    
  }
  remove(indic)
}

colnames(tableau_temps) = c("Joueur","Temps de jeu total","Ratio équipe (%)","Nombre de match(s)",
                            "Temps de jeu moyen (mn)","Nombre de titularisation(s)")

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

rownames(tableau_temps) = c('18','14','20','23','24','5','15','9','3','8','17','11','4','6','10','19','21','28','2','12','--')

##### Recherche des cartons rouges

# x=actions_effectif[(which(is.na(actions_effectif[,c('joueur_1','joueur_2','joueur_3','joueur_4',
#                                                     'joueur_5','joueur_6','joueur_7','joueur_8',
#                                                     'joueur_9','joueur_10')])) %% 60)-1,5:14]
# 
# y=actions_effectif[which(is.na(actions_effectif[,c('joueur_1','joueur_2','joueur_3','joueur_4',
#                                                    'joueur_5','joueur_6','joueur_7','joueur_8',
#                                                    'joueur_9','joueur_10')])) %% 60,5:14]
# for (i in 1:nrow(x)){
#   exclus = x[i,!(x[i,] %in% y[i,])]
# }



actions_jeu = as.data.frame(actions_jeu)

actions_jeu$finalite[is.na(actions_jeu$finalite)] = '1/2 occasions'

actions_jeu = actions_jeu %>% group_by(journee) %>%
  mutate(but_pour = cumsum(ifelse(finalite=='But',1,0)),score2 = lead(score))

actions_jeu$score2 = as.numeric(actions_jeu$score2)
for(i in 1:nrow(actions_jeu)){if(is.na(actions_jeu$score2[i])){actions_jeu$score2[i]=actions_jeu$score2[i-1]}}

actions_jeu$but_contre = actions_jeu$but_pour - actions_jeu$score2

actions_jeu = actions_jeu %>% select(-score2) %>% mutate(score_match = paste0(but_pour,'-',but_contre))


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
# colnames(passes_clees)

base_finale = merge(passes_clees[,c(1,5)],passes_d,by=c('Joueur'),all=TRUE)
base_finale = merge(occaz_but[,c(1,3,5)],base_finale,by=c('Joueur'),all=TRUE)
base_finale = merge(tableau_temps[,c(1,5,6)],base_finale,by=c('Joueur'),all=TRUE)

base_finale = base_finale[-which(base_finale$Joueur=="Joueur(s) exclu(s)"),]

base_finale$`Temps de jeu moyen (mn)` = round(base_finale$`Temps de jeu moyen (mn)`*100/94)
base_finale$`Nombre de titularisation(s)` = round(base_finale$`Nombre de titularisation(s)`/15*100)
base_finale$But = round(base_finale$But/max(base_finale$But,na.rm = T)*100)
base_finale$passes_clees = round(base_finale$passes_clees/max(base_finale$passes_clees,na.rm = T)*100)
base_finale$passes_decisives = round(base_finale$passes_decisives/max(base_finale$passes_decisives,na.rm = T)*100)


base_finale = t(base_finale)
colnames(base_finale) = base_finale[1,]
base_finale = as.data.frame(base_finale[-1,])
rownames(base_finale) = NULL
for(i in 1:ncol(base_finale)){base_finale[,i]= unfactor(base_finale[,i])}



match = actions_tot %>% select(competition,journee,lieu) %>% group_by(journee) %>% summarise_all(funs(first))
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


# selon_moment = spread(as.data.frame(table(actions_jeu$periode,actions_jeu$finalite)),Var2,Freq)
# 
# selon_score = spread(as.data.frame(table(actions_jeu$score,actions_jeu$finalite)),Var2,Freq)
# 
# selon_phase = spread(as.data.frame(table(actions_jeu$type_phase,actions_jeu$finalite)),Var2,Freq)




#### Dessin de l'interface
## Comme précisé plus haut dans un dashboard on a une structure en 3 éléments
## On retrouve bien ici le dashboardHeader, le dashboardSidebar avec les différents onglets et items (choix de date 
## et de la gamme pour les résultats par gamme). Les petites icônes sont là pour égayer un peu le menu, on en trouve un 
## large choix en cherchant sur internet. Et enfin le dashboardBody qui fait appel aux onglet du menu et diffuse 
## les onglets créés dans le server.


ui <- dashboardPage(
  ## La tags$li sert à imposer un format aux boutons de téléchargement, ici un fond noir.
  ## Il était nécessaire de le faire car dans cette interface de dashbord l'écriture dans la sidebar est gris clair par défaut
  # donc on lisait très difficilement les boutons en gris clair sur blanc.
  ## Je ne sais pas vraiment expliquer pourquoi li, pourquoi le dropdown et pourquoi le style, j'ai trouvé la solution 
  # en cherchant sur internet et ça fonctionne bien.
  dashboardHeader(title = tags$a(href='https://fr.wikipedia.org/wiki/Saison_2015-2016_du_FC_Barcelone',
                                 tags$img(src='fanion.png',height="120px")),
                  titleWidth = 324,
                  tags$li(a("Saison 2015-2016",style = "font-size: 28px; padding-top:62px; padding-right:5% ;"),
                          align='center',
                          class="dropdown")),
  dashboardSidebar(width = 324,
                   sidebarMenu(id = "sidebar_menu",
                               menuItem("Choix du match", icon = icon("calendar"),tabName = "journee"),
                               conditionalPanel(
                                 condition = "input.sidebar_menu == 'journee'",
                                 selectInput(inputId = "journee_match",
                                             choices=list('Domicile'=paste(match$competition,match$journee,":",match$adversaire,sep=" ")[which(match$lieu=='Domicile')],
                                                          'Extérieur'=paste(match$competition,match$journee,":",match$adversaire,sep=" ")[which(match$lieu=='Exterieur')]),
                                             selected = "CPN J9 : SD Eibar",
                                             label = "Journée :"
                                 )
                               ),
                               menuItem("Vue d'ensemble de l'équipe", tabName = "equipe", icon = icon("dashboard"))
                   )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    fluidRow(
      tabItems(
        tabItem(tabName = "journee", 
                list(
                  
                  fluidRow(column(width = 6, offset = 0,
                                  box(width=NULL,
                                      title = "Profil Joueur 1", status = "primary",
                                      selectInput(inputId = "choix_j1",
                                                  choices=Barca_Eibar$Joueur,
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
                                         choices=Barca_Eibar$Joueur,
                                         selected = "Suarez",
                                         label = "Choix du second joueur à comparer :"
                             ),
                             uiOutput("infos_j2")
                         )
                         
                  )
                  ),
                  fluidRow(column(width = 12, offset = 0,
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
                  )
                )),
        tabItem(tabName = "equipe", uiOutput("body_equipe"))
      )
    )
  )
)


#### Objets R à intégrer
### Création dans le server de tous les body qui sont, ou qui peuvent être, appelés dans le UI
### Ils doivent être entiérement créés dans des fonctions renderUi pour avoir le dynamisme du choix de l'onglet
server <- function(input, output, session) {
  
  ### Onglet Vue Synthétique  
  output$infos_j1 <- renderUI({
    list(
      p(img(src=Barca_Eibar$Photo[which(Barca_Eibar$Joueur==input$choix_j1)],width=120),align='center'),
      h4(Barca_Eibar$Nom_complet[which(Barca_Eibar$Joueur==input$choix_j1)],align='center'),
      paste0("Numéro : ",Barca_Eibar$Numero[which(Barca_Eibar$Joueur==input$choix_j1)]),
      br(),
      paste0("Poste : ",Barca_Eibar$Poste[which(Barca_Eibar$Joueur==input$choix_j1)]),
      br(),
      paste0("Nationalité : ",Barca_Eibar$Nationalite[which(Barca_Eibar$Joueur==input$choix_j1)]),
      br(),
      paste0("Date de naissance : ",Barca_Eibar$Date_naissance[which(Barca_Eibar$Joueur==input$choix_j1)]),
      br(),
      paste0("Taille : ",Barca_Eibar$Taille[which(Barca_Eibar$Joueur==input$choix_j1)]),
      br(),
      paste0("Meilleur pied : ",Barca_Eibar$Pied[which(Barca_Eibar$Joueur==input$choix_j1)])
    )
  })
  
  output$infos_j2 <- renderUI({
    list(
      p(img(src=Barca_Eibar$Photo[which(Barca_Eibar$Joueur==input$choix_j2)],width=120),align='center'),
      h4(Barca_Eibar$Nom_complet[which(Barca_Eibar$Joueur==input$choix_j2)],align='center'),
      paste0("Numéro : ",Barca_Eibar$Numero[which(Barca_Eibar$Joueur==input$choix_j2)]),
      br(),
      paste0("Poste : ",Barca_Eibar$Poste[which(Barca_Eibar$Joueur==input$choix_j2)]),
      br(),
      paste0("Nationalité : ",Barca_Eibar$Nationalite[which(Barca_Eibar$Joueur==input$choix_j2)]),
      br(),
      paste0("Date de naissance : ",Barca_Eibar$Date_naissance[which(Barca_Eibar$Joueur==input$choix_j2)]),
      br(),
      paste0("Taille : ",Barca_Eibar$Taille[which(Barca_Eibar$Joueur==input$choix_j2)]),
      br(),
      paste0("Meilleur pied : ",Barca_Eibar$Pied[which(Barca_Eibar$Joueur==input$choix_j2)])
    )
  })
  
  
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
  
  
  output$spider_plot_j1_j2 = renderHighchart({  
    hc <- highchart() %>%
      hc_title(text="Le match",align='center',style=list(color="black")) %>%
      hc_subtitle(text="En pourcentage de la valeur maximale dans l'équipe",align='center') %>%
      hc_chart(type = "area", polar = TRUE) %>%
      hc_xAxis(categories = c('Buts','Tirs','Tirs cadrés','Passes décisives','Passes clés','Dribles réussis',
                              'Ballons touchés','Duel aériens','Tacles réussis','Interceptions','Fautes'),
               tickmarkPlacement = 'on',
               lineWidth = 0) %>%
      hc_yAxis(gridLineInterpolation = 'polygon',lineWidth = 0,min = 0,max=100) %>%
      #hc_legend(align ='right', verticalAlign ='top', layout='vertical') %>%
      hc_tooltip(headerFormat='<span style="font-size:11px">{point.x}</span><br>',
                 pointFormat='<span style="color:{series.color}">{series.name}</span>: <b>{point.y}%<br/>') %>%
      hc_add_series(name = Barca_Eibar$Nom_complet[which(Barca_Eibar$Joueur==input$choix_j1)], 
                    data = spider_j1()$Stats, pointPlacement = 'on',color = "#00529F") %>%
      hc_add_series(name = Barca_Eibar$Nom_complet[which(Barca_Eibar$Joueur==input$choix_j2)], 
                    data = spider_j2()$Stats, pointPlacement = 'on',color = "#93111d")
    
    hc})
  
  
  
  pyramide_attaque_j1 = reactive({
    temp = Barca_Eibar[-nrow(Barca_Eibar),c('Tirs','Tirs_cadres','Buts','Dribles','Dribles_reussis','Hors_jeu','Erreurs')]
    temp[is.na(temp)] = 0
    temp = -temp[which(Barca_Eibar$Joueur==input$choix_j1),]
    temp = as.data.frame(t(temp))
    colnames(temp) = "Stats"
    rownames(temp) = NULL
    temp
  })
  
  pyramide_attaque_j2 = reactive({
    temp = Barca_Eibar[-nrow(Barca_Eibar),c('Tirs','Tirs_cadres','Buts','Dribles','Dribles_reussis','Hors_jeu','Erreurs')]
    temp[is.na(temp)] = 0
    temp = temp[which(Barca_Eibar$Joueur==input$choix_j2),]
    temp = as.data.frame(t(temp))
    colnames(temp) = "Stats"
    rownames(temp) = NULL
    temp
  })
  
  
  output$attaque_pyramid_plot = renderHighchart({  
    hc <- highchart() %>%
      hc_title(text="Le jeu offensif",align='center',style=list(color="black")) %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(list(categories = c('Tirs','Tirs cadrés','Buts','Dribles','Dribles réussis','Hors jeu','Erreurs')),
               list(categories = c('Tirs','Tirs cadrés','Buts','Dribles','Dribles réussis','Hors jeu','Erreurs'),opposite=T,linkedTo =0)) %>%
      hc_yAxis(title = list(text = NULL),labels = list(formatter = JS("function () {
          return Math.abs(this.value); }")),
               max=max(abs(pyramide_attaque_j1()$Stats),pyramide_attaque_j2()$Stats),
               min=-max(abs(pyramide_attaque_j1()$Stats),pyramide_attaque_j2()$Stats)) %>%
      hc_plotOptions(series=list(stacking='normal')) %>%
      #hc_tooltip(headerFormat='<span style="font-size:11px">{point.x}</span><br>',
      #           pointFormat='<span style="color:{series.color}">{series.name}</span>: <b>{point.y}%<br/>') %>%
      hc_tooltip(formatter = JS("function () {
              return '<b>' + this.point.category + '</b><br/>' +
      this.series.name + ' :  '+ Math.abs(this.point.y);
      }")) %>%
      hc_add_series(name = Barca_Eibar$Nom_complet[which(Barca_Eibar$Joueur==input$choix_j1)], data = pyramide_attaque_j1()$Stats,color = "#00529F") %>%
      hc_add_series(name = Barca_Eibar$Nom_complet[which(Barca_Eibar$Joueur==input$choix_j2)], data = pyramide_attaque_j2()$Stats,color = "#93111d")
    
    hc})
  
  
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
      hc_title(text="Le jeu défensif",align='center',style=list(color="black")) %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(list(categories = c('Tacles','Tacles réussis','Interceptions','Dégagements','Duel aériens','Fautes')),
               list(categories = c('Tacles','Tacles réussis','Interceptions','Dégagements','Duel aériens','Fautes'),opposite=T,linkedTo =0)) %>%
      hc_yAxis(title = list(text = NULL),labels = list(formatter = JS("function () {
                                                                      return Math.abs(this.value); }")),
               max=max(abs(pyramide_defense_j1()$Stats),pyramide_defense_j2()$Stats),
               min=-max(abs(pyramide_defense_j1()$Stats),pyramide_defense_j2()$Stats)) %>%
      hc_plotOptions(series=list(stacking='normal')) %>%
      #hc_tooltip(headerFormat='<span style="font-size:11px">{point.x}</span><br>',
      #           pointFormat='<span style="color:{series.color}">{series.name}</span>: <b>{point.y}%<br/>') %>%
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
      hc_title(text="Le informations générales de jeu",align='center',style=list(color="black")) %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(list(categories = c('Minutes','Ballons Touchés','Passes','Passes dans les 30 derniers mètres','Passes clés')),
               list(categories = c('Minutes','Ballons Touchés','Passes','Passes dans les 30 derniers mètres','Passes clés'),opposite=T,linkedTo =0)) %>%
      hc_yAxis(title = list(text = NULL),labels = list(formatter = JS("function () {
                                                                      return Math.abs(this.value); }")),
               max=max(abs(pyramide_global_j1()$Stats),pyramide_global_j2()$Stats),
               min=-max(abs(pyramide_global_j1()$Stats),pyramide_global_j2()$Stats)) %>%
      hc_plotOptions(series=list(stacking='normal')) %>%
      #hc_tooltip(headerFormat='<span style="font-size:11px">{point.x}</span><br>',
      #           pointFormat='<span style="color:{series.color}">{series.name}</span>: <b>{point.y}%<br/>') %>%
      hc_tooltip(formatter = JS("function () {
                                return '<b>' + this.point.category + '</b><br/>' +
                                this.series.name + ' :  '+ Math.abs(this.point.y);
}")) %>%
      hc_add_series(name = Barca_Eibar$Nom_complet[which(Barca_Eibar$Joueur==input$choix_j1)], data = pyramide_global_j1()$Stats,color = "#00529F") %>%
      hc_add_series(name = Barca_Eibar$Nom_complet[which(Barca_Eibar$Joueur==input$choix_j2)], data = pyramide_global_j2()$Stats,color = "#93111d")
    
    hc})
  
  
  ### Pour l'onglet des gammes de jeu, le corps change selon la gamme de jeu (hasard ou précision) choisie
  ### Pour cela un body par choix sera créé et l'affichage s'adaptera grâce à la fonction observe qui suit le choix de l'utilisateur
  
  output$body_equipe <- renderUI({
    
    
    niv_1 = occaz_but %>% dplyr::select(Joueur,But) %>%
      dplyr::mutate(drilldown=Joueur) %>%
      dplyr::rename(name=Joueur,y=But)
    
    to_plot_niv1 <- highcharter::list_parse(niv_1)
    names(to_plot_niv1) <- NULL
    
    # Il faut ensuite créer des listes avec la catégories et la valeur à afficher (sans nom obligatoire cette fois)
    # pour chaque graphique de niveau 2 (un par drilldown du niveau 1).
    # Fonction pour optimiser ce traitement sur les 5 catégories.
    to_plot_niv2 = function(joueur){
      temp = occaz_but %>% dplyr::select(Joueur,But,`1/2 occasions`,Occasions) %>% dplyr::filter(Joueur == joueur) %>% 
        tidyr::gather(type,nombre,c(`1/2 occasions`,But,Occasions)) %>% dplyr::select(-Joueur) %>%
        highcharter::list_parse2()
      temp
    }
    
    for(i in 1:length(joueurs)){
      assign(paste0('to_plot_niv2_',gsub(' ','',joueurs[i])),to_plot_niv2(joueurs[i]))
    }
    
    
    output$buts_equipe = renderHighchart({  
      hc <- highchart()
      # Le type est celui des graphiques de niveau 2
      hc <- hc_chart(hc,type = "pie")
      hc <- hc_title(hc,text = "Buts et occasions des joueurs")
      hc <- hc_subtitle(hc,text = "Cliquer sur un joueur pour voir le détail de ses occasions")
      # L'axe de type catégorie indique juste que chaque valeur doit être un label sur l'axe
      hc <- hc_xAxis(hc,type = "category")
      hc <- hc_legend(hc,enabled = FALSE)
      # Le y_axis apparait dans les 2 niveaux uniquement si le type du graphique le demande
      # hc <- hc_yAxis(hc,title = list(text = "Nombre de buts"), labels = list(format = '{value}'))
      hc <- hc_plotOptions(hc,series = list(borderWidth = 0),pie = list(size="71%",innerSize = '71%',colors=list("#00529F","#e5bd2b","#93111d"),
                                                                        dataLabels = list(enabled = TRUE, format = '<b>{point.name}</b>: {point.y}')))
      # Dessin du camembert, le innersize est là pour le creuser (comme un donut)
      hc <- hc_add_series(hc,name = "Les buts inscrits",colorByPoint = FALSE,data = to_plot_niv1,color="#00529F",type = "column",
                          dataLabels = list(enabled = TRUE))
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
    
    
    
    niv_1_passes = passes_clees %>% dplyr::select(Joueur,passes_clees) %>%
      dplyr::mutate(drilldown=Joueur) %>%
      dplyr::rename(name=Joueur,y=passes_clees)
    
    to_plot_niv1_passes <- highcharter::list_parse(niv_1_passes)
    names(to_plot_niv1_passes) <- NULL
    
    # Il faut ensuite créer des listes avec la catégories et la valeur à afficher (sans nom obligatoire cette fois)
    # pour chaque graphique de niveau 2 (un par drilldown du niveau 1).
    # Fonction pour optimiser ce traitement sur les 5 catégories.
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
      # Le type est celui des graphiques de niveau 2
      hc <- hc_chart(hc,type = "pie")
      hc <- hc_title(hc,text = "Passes clées des joueurs")
      hc <- hc_subtitle(hc,text = "Cliquer sur un joueur pour voir le détail de ses passes clées")
      # L'axe de type catégorie indique juste que chaque valeur doit être un label sur l'axe
      hc <- hc_xAxis(hc,type = "category")
      hc <- hc_legend(hc,enabled = FALSE)
      # Le y_axis apparait dans les 2 niveaux uniquement si le type du graphique le demande
      # hc <- hc_yAxis(hc,title = list(text = "Nombre de buts"), labels = list(format = '{value}'))
      hc <- hc_plotOptions(hc,series = list(borderWidth = 0),pie = list(size="71%",innerSize = '71%',colors=list("#00529F","#e5bd2b","#93111d"),
                                                                        dataLabels = list(enabled = TRUE, format = '<b>{point.name}</b>: {point.y}')))
      # Dessin du camembert, le innersize est là pour le creuser (comme un donut)
      hc <- hc_add_series(hc,name = "Les passes clées",colorByPoint = FALSE,data = to_plot_niv1_passes,color="#00529F",type = "column",
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
    
    
    
    
    output$spider_plot = renderHighchart({  
      hc <- highchart() %>%
        hc_title(text="Comparaison des joueurs",align='left',style=list(color="black")) %>%
        hc_subtitle(text="En pourcentage de la valeur maximale",align='left') %>%
        hc_chart(type = "line", polar = TRUE) %>%
        hc_xAxis(categories = c("Temps de jeu moyen","Titularisations","Buts","Efficacite","Passes clees","Passes decisives"),
                 tickmarkPlacement = 'on',
                 lineWidth = 0) %>%
        hc_yAxis(gridLineInterpolation = 'polygon',lineWidth = 0,min = 0) %>%
        hc_legend(align ='right', verticalAlign ='top', layout='vertical') %>%
        hc_tooltip(headerFormat='<span style="font-size:11px">{point.x}</span><br>',
                   pointFormat='<span style="color:{series.color}">{series.name}</span>: <b>{point.y}%<br/>') %>%
        hc_add_series(name = 'Lionel Messi', data = base_finale$Messi, pointPlacement = 'on') %>%
        hc_add_series(name = 'Luis Suarez', data = base_finale$Suarez, pointPlacement = 'on') %>%
        hc_add_series(name = 'Neymar JR', data = base_finale$Neymar, pointPlacement = 'on') %>%
        hc_add_series(name = 'Sandro Ramirez', data = base_finale$Ramirez, pointPlacement = 'on',visible=F) %>%
        hc_add_series(name = 'Munir El Haddadi', data = base_finale$`El Haddadi`, pointPlacement = 'on',visible=F) %>%
        hc_add_series(name = 'Andres Iniesta', data = base_finale$Iniesta, pointPlacement = 'on',visible=F) %>%
        hc_add_series(name = 'Ivan Rakitic', data = base_finale$Rakitic, pointPlacement = 'on',visible=F) %>%
        hc_add_series(name = 'Sergi Roberto', data = base_finale$Roberto, pointPlacement = 'on',visible=F) %>%
        hc_add_series(name = 'Rafinha', data = base_finale$Rafinha, pointPlacement = 'on',visible=F) %>%
        hc_add_series(name = 'Sergio Busquets', data = base_finale$Busquets, pointPlacement = 'on',visible=F) %>%
        hc_add_series(name = 'Gerard Gumbau', data = base_finale$Gumbau, pointPlacement = 'on',visible=F) %>%
        hc_add_series(name = 'Jordi Alba', data = base_finale$Alba, pointPlacement = 'on',visible=F) %>%
        hc_add_series(name = 'Dani Alves', data = base_finale$Alves, pointPlacement = 'on',visible=F) %>%
        hc_add_series(name = 'Gerard Pique', data = base_finale$Pique, pointPlacement = 'on',visible=F) %>%
        hc_add_series(name = 'Marc Bartra', data = base_finale$Bartra, pointPlacement = 'on',visible=F) %>%
        hc_add_series(name = 'Javier Mascherano', data = base_finale$Mascherano, pointPlacement = 'on',visible=F) %>%
        hc_add_series(name = 'Adriano', data = base_finale$Adriano, pointPlacement = 'on',visible=F) %>%
        hc_add_series(name = 'Douglas', data = base_finale$Douglas, pointPlacement = 'on',visible=F) %>%
        hc_add_series(name = 'Jeremy Mathieu', data = base_finale$Mathieu, pointPlacement = 'on',visible=F) %>%
        hc_add_series(name = 'Thomas Vermaelen', data = base_finale$Vermaelen, pointPlacement = 'on',visible=F) 
      
      hc})
    
    
    output$passes_d = renderHighchart({  
      hc <- highchart() %>%
        hc_title(text="Nombre de passes décisives") %>%
        hc_chart(type = "column") %>%
        hc_yAxis(title = list(text = "Nombre de passes"), labels = list(format = '{value}')) %>%
        hc_xAxis(categories=passes_d$Joueur) %>%
        hc_add_series(name = "Les passes",colorByPoint = FALSE,data = passes_d$passes_decisives,color="#00529F",
                      dataLabels = list(enabled = TRUE))
      
      hc})
    
    
    
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
    
    # chart
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
    
    output$temps_jeu <- DT::renderDataTable({
      tableau_temps[,1:6]
    },
    options = list(scrollY = 300, ordering = TRUE, pageLength = 10, paging = FALSE),
    rownames = TRUE
    )
    
    
    
    list(
      
      
      fluidRow(column(width = 12, offset = 0,
                      h3("Depuis le début de saison",align='center',style = "color:white"),
                      br(),
                      tabBox(width = 12,
                             tabPanel("Buts",highchartOutput(outputId = "buts_equipe",height = '13.5cm')),
                             tabPanel("Passes clées",highchartOutput(outputId = "passes_equipe",height = '13.5cm')),
                             tabPanel("Passes décisives",highchartOutput(outputId = "passes_d",height = '13.5cm'))
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
  session$onSessionEnded(stopApp)
  
  ## Justement, dans la méthode de partage actuelle, il faut lancer le bout de code ci-dessous pour pouvoir partager
  ## la page Web une fois l'application lancée.
  
  # runApp('C:/Users/intercontrat/Desktop/Benjamin/Captain_Dash/Shiny',host = "164.7.85.117", port = 5052)
}

shinyApp(ui, server)