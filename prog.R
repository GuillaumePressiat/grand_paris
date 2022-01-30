
libs <- c('rgdal','rgeos','classInt','RColorBrewer','maptools','readr', 'ggplot2', 'dplyr', 'plotly','reshape','reshape2','ggthemes', 'knitr', 'rmarkdown')
sapply(libs, require, character.only = TRUE)

# Grand paris
# a partir des communes et territoires ici :
# http://www.apur.org/sites/default/files/documents/etablissements_publics_territoriaux_MGP_carte_chiffres_cles.pdf

# setwd('~/Documents/Documents/grand_paris/Grand Paris')
require(readr)
require(dplyr)
un<-read_delim('base_officielle_codes_postaux_-_09102015-2.csv', delim=';')
# names(un)
# View(un)
deux<- un %>% filter(substr(INSEE_COM,1,2) %in% c('75', '92', '93', '94') |
                       
                       INSEE_COM %in% c('95018', '91479', '91027','91326',
                                        '91589','91432','91687'))

# 95018 argenteuil
# 91479 paray vieille poste
# 91027 athis mons
# 91326 juvisy
# 91589 savigny
# 91432 morangis
# viry chatillon
deux$Territoire<-rep('000',nrow(deux))

T1<- (substr(deux$INSEE_COM,1,2)=='75')

T2<- (deux$INSEE_COM %in% c('92046','92049','92020','92007','92023','92032','92014','92071',
                            '92060','92019','92002'))
# malakoff
# montrouge
# chatillon
# bagneux
# clamart
# fontenay aux roses
# bourg la reine
# sceaux
# le plessis robinson
# chatenay malabry
# antony

T3<- (deux$INSEE_COM %in% c('92012','92040','92075','92072','92048','92022','92077','92047'))

# boulogne billancourt
# vanves
# sevres
# meudon
# chaville
# ville d'avray
# marnes la coquette 


T4<- (deux$INSEE_COM %in% c('92064','92033','92076','92063','92073','92050','92062','92051','92044',
                            '92026','92035'))

# saint cloud
# garches
# vaucresson
# rueil malmaison
# suresnes
# nanterre
# puteaux
# neuilly sur seine
# levallois perret
# courbevoie
# la garenne colombes

T5<- (deux$INSEE_COM %in% c('95018','92025','92009','92004','92024','92036','95018','92078'))

# argenteuil
# colombes
# bois colombes
# asnieres sur seine
# clichy
# gennevilliers
# argenteuil
# villeneuve la garenne

T6<- (deux$INSEE_COM %in% c('93031','93039','93070','93066','93079','93059','93072','93027',
                            '93001'))

# epinay sur seine
# ile st denis
# st ouen
# st denis
# villetaneuse
# pierrefitte sur seine
# stains
# la courneuve
# aubervilliers

T7<- (deux$INSEE_COM %in% c('93030','93013','93029','93007','93005','93071','93078','93073'))

# Dugny
# Le Bourget
# Drancy
# Le Blanc mesnil
# Aulnay sous bois
# Sevran
# Villepinte
# tremblay en france

T8<- (deux$INSEE_COM %in% c('93055','93008','93061','93045','93006','93048','93063','93053','93010'))

# pantin
# bobigny
# pre st gervais
# les lilas
# bagnolet
# montreuil
# romainville
# noisy le sec
# bondy

T9<- (deux$INSEE_COM %in% c('93064','93049','93077','93050','93051','93033',
                            '93032','93062','93047','93014','93057','93046','93015','93074'))


# rosny sous bois
# neuilly plaisance
# villemomble
# neuilly sur marne
# noisy le grand  
# gournay sur marne
# gagny
# le raincy
# montfermeil
# clichy sous bois
# les pavillons sous bois
# livry gargan
# coubron
# vaujours

T10<- (deux$INSEE_COM %in% c('94080','94033','94052','94042','94067','94058',
                             '94015','94079','94017','94068','94046','94069','94018'))

# vincennes
# fontenay sous bois
# nogent sur marne
# joinville le pont
# saint mande
# le perreux sur marne
# bry sur marne
# villiers sur marne
# champigny sur marne
# saint maur des fosses
# maisons alfort
# saint maurice
# charenton le pont

T11<- (deux$INSEE_COM %in% c('94002','94028','94011','94044','94004','94075','94047',
                             '94056','94070','94048',
                             '94071','94053','94055','94019','94059','94060'
                             ))

# Alfortville
# Creteil
# bonneuil sur marne
# limeil brevannes
# boissy st leger
# villecresnes
# mandres les roses
# perigny
# santeny
# marolles en brie
# sucy en brie
# noiseau
# ormesson sur marne
# chennevieres sur marne
# le plessis trevise
# la queue en brie

T12<- (deux$INSEE_COM %in% c( '91479', '91027','91326',
                             '91589','91432','91687',
                             '94037','94043','94041','94003',
                             '94016','94076','94081',
                             '94038','94021','94073','94022','94074','94078',
                             '94077','94054','94034','94065','94001'))


# 91479 paray vieille poste
# 91027 athis mons
# 91326 juvisy
# 91589 savigny
# 91432 morangis
# viry chatillon


# gentilly
# kremlin bicetre
# ivry sur seine
# arcueil
# cachan
# villejuif
# vitry sur seine
# l'hay les roses
# chevilly larue
# thiais
# choisy le roi
# valenton
# villeneuve st georges
# villeneuve le roi
# orly
# fresnes 
# rungis
# ablon sur seine


deux[T1,]$Territoire<-"T1"
deux[T2,]$Territoire<-"T2"
deux[T3,]$Territoire<-"T3"
deux[T4,]$Territoire<-"T4"
deux[T5,]$Territoire<-"T5"
deux[T6,]$Territoire<-"T6"
deux[T7,]$Territoire<-"T7"
deux[T8,]$Territoire<-"T8"
deux[T9,]$Territoire<-"T9"
deux[T10,]$Territoire<-"T10"
deux[T11,]$Territoire<-"T11"
deux[T12,]$Territoire<-"T12"
# sum(deux$Territoire!='000')/nrow(deux)
# View(deux[deux$Territoire=='000',])

#write_csv(deux,"Grand Paris T.csv")

require(rgdal)
# fdc2 is a shapefile for Ile de France communes 
com<-readOGR(dsn='shp','fdc2')
# deux<-deux %>% distinct(INSEE_COM)
require(rgeos)

myColors <- c("#3399ff","#6699ff","#66ccff","#99ffcc","#009933","#ccff66","#999966",
              "#ffff99","#666633","#33cccc","#9966ff","#99ff66")

deux$Territoire=factor(deux$Territoire,levels= c('T1','T2','T3','T4','T5','T6','T7','T8',
                                                 'T9','T10','T11','T12'))
names(myColors) <- levels(deux$Territoire)
colScale <- scale_colour_manual(name = "12 territoires",values = myColors)

b<-fortify(com,region="INSEE_COM")

theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         plot.background = element_rect(fill="white"),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         plot.title = element_text(size=28),
                         legend.text=element_text(size=10),
                         legend.title=element_text(size=20)))

b<-inner_join(b,deux,by=c("id"="INSEE_COM"))
b$Territoire=factor(b$Territoire,levels= c('T1','T2','T3','T4','T5','T6','T7','T8',
                                                 'T9','T10','T11','T12'))

ggplot() + geom_polygon(data=b,aes(long,lat,group=group,fill=Territoire),col='grey10') + 
  scale_fill_manual(name = "12 territoires",values = myColors) + 
  coord_fixed() + 
  theme_opts


# a<-fortify(map2,region=="Territoire")
# ggplot() + geom_polygon(data=a,aes(long,lat, group=group))

# reunir par territoire
dd <- full_join(com@data, deux, "INSEE_COM"="INSEE_COM")
dd<-dd[!is.na(dd$Territoire),]

dd <- merge(com, deux %>% distinct(INSEE_COM, .keep_all = TRUE), by="INSEE_COM")
dd<-dd[!is.na(dd@data$Territoire),]
d2<-unionSpatialPolygons(dd, dd$Territoire)
d2$Te<-unique(dd$Territoire)
d3<-fortify(d2,region="Te")

ggplot() + geom_polygon(data=b,aes(long,lat,group=group,fill=Territoire),col='grey10') + 
  geom_polygon(data=d3,aes(long,lat,group=group),fill=NA,col='grey10',lwd=1.5) +
  geom_polygon(data=d3,aes(long,lat,group=group),fill=NA,col='grey50',lwd=1)+
  scale_fill_manual(name = "",values = myColors, guide = guide_legend(title.position = NULL)) + 
  coord_fixed() + 
  theme_opts + theme(legend.position="top") + ggtitle("Les 12 territoires du Grand Paris")


u <- as.data.frame(gCentroid(d2, byid=T))

lab<-c('T1 Paris', 'T2 Sud Hauts-de-Seine','T3 GPSO',
       'T4 La Défense','T5 Boucle Nord 92','T6 Plaine Commune','T7 Territoire des aéroports',
       'T8 Est Ensemble','T9 Grand-Paris Est','T10 ACTEP','T11 Plaine Centrale - Haut Val-de-Marne - Plateau Briand',
       'T12 Val de Bièvres - Seine Amont - Grand Orly')
names(myColors) <- levels(deux$Territoire)

ggplot() + geom_polygon(data=b,aes(long,lat,group=group,fill=Territoire),col='grey10',lwd=0.1) + 
  geom_polygon(data=d3,aes(long,lat,group=group),fill=NA,col='grey10',lwd=1.5) +
  geom_polygon(data=d3,aes(long,lat,group=group),fill=NA,col='grey50',lwd=1)+
  scale_fill_manual(name = "",values = myColors, labels=lab, guide = guide_legend(title.position = NULL)) + 
  geom_text(data=u,aes(x=x,y=y,label=row.names(u)),size=7,col='black')+
  coord_fixed() + ggtitle("Les 12 territoires du Grand Paris")+
  theme_opts + theme(legend.position="top")


v<- cbind(as.data.frame(gCentroid(com, byid=T)),i=com@data$INSEE_COM)
v<-right_join(v,b %>% distinct(id, .keep_all = TRUE) %>% select(id,Nom_de_la_commune),by=c('i'='id'))


ggplot() + geom_polygon(data=b,aes(long,lat,group=group,fill=Territoire),col='grey10',lwd=0.1) + 
  geom_polygon(data=d3,aes(long,lat,group=group),fill=NA,col='grey10',lwd=1.5) +
  geom_polygon(data=d3,aes(long,lat,group=group),fill=NA,col='grey50',lwd=1)+
  scale_fill_manual(name = "",values = myColors, labels=lab, guide = guide_legend(title.position = NULL)) + 
  geom_text(data=u,aes(x=x,y=y,label=row.names(u)),size=7,col='black')+
  geom_text(data=v,aes(x=x,y=y,label=Nom_de_la_commune),size=1,col='black')+
  coord_fixed() + ggtitle("Les 12 territoires du Grand Paris")+
  theme_opts + theme(legend.position="top")

require('ggrepel')
theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         plot.background = element_rect(fill="white"),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         plot.title = element_text(size=28),
                         legend.text=element_text(size=8),
                         legend.title=element_text(size=20)))

ggplot() + geom_polygon(data=b,aes(long,lat,group=group,fill=Territoire),col='grey10',lwd=0.1) + 
  geom_polygon(data=d3,aes(long,lat,group=group),fill=NA,col='grey10',lwd=1.5) +
  geom_polygon(data=d3,aes(long,lat,group=group),fill=NA,col='grey50',lwd=1)+
  scale_fill_manual(name = "",values = myColors, labels=lab, guide = guide_legend(title.position = NULL)) + 
  geom_label_repel(data=u,
                   aes(x, y, label = rownames(u)),
                   fontface = 'bold', color = 'white',
                   box.padding = unit(0.25, "lines"),fill='grey20',alpha=0.5,size=7,segment.size = 0,
                   segment.color = NA
  ) +
  geom_text(data=v,
            aes(x, y, label = Nom_de_la_commune),
            fontface = 'bold', color = 'grey10',
            #label.padding=unit(0.1, "lines"),fill=NA,
            #box.padding = unit(0.25, "lines"),
            alpha=0.7,size=0.8
            # segment.size = 0,
            # segment.color = NA
  )+
  #geom_text(data=v,aes(x=x,y=y,label=Nom_de_la_commune),size=1,col='black')+
  coord_fixed() + ggtitle("Les 12 territoires du Grand Paris")+
  theme_opts + theme(legend.position="top")


comp<-fortify(com,region="INSEE_COM")
comp<-anti_join(comp,b, by=c('id'='id'))

# replacer ile saint denis
v[v$i=='93039',]$y<-v[v$i=='93039',]$y-400
v[v$i=='93039',]$x<-v[v$i=='93039',]$x+600

# 650661.3
# 6871030

u <- as.data.frame(gCentroid(d2, byid=T))

lab<-c('T1 Paris', 'T2 Sud Hauts-de-Seine','T3 GPSO',
       'T4 La Défense','T5 Boucle Nord 92','T6 Plaine Commune','T7 Territoire des aéroports',
       'T8 Est Ensemble','T9 Grand-Paris Est','T10 ACTEP','T11 Plaine Centrale - Haut Val-de-Marne - Plateau Briand',
       'T12 Val de Bièvres - Seine Amont - Grand Orly')
names(myColors) <- levels(deux$Territoire)

u[rownames(u)=='T4',]<- c(u[rownames(u)=='T4',]$x-400,u[rownames(u)=='T4',]$y+1000)
u[rownames(u)=='T2',]<- c(u[rownames(u)=='T2',]$x,u[rownames(u)=='T2',]$y-800)
u[rownames(u)=='T8',]<- c(u[rownames(u)=='T8',]$x,u[rownames(u)=='T8',]$y+200)
u[rownames(u)=='T7',]<- c(u[rownames(u)=='T7',]$x-600,u[rownames(u)=='T7',]$y-600)
u[rownames(u)=='T6',]<- c(u[rownames(u)=='T6',]$x,u[rownames(u)=='T6',]$y+300)

theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background =  element_blank(),
                         plot.background = element_rect(fill = "grey95"),
                         legend.background = element_rect(fill = NA),
                         #panel.border = element_blank(),
                         panel.border = element_rect(colour = "black", fill=NA, size=2),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         plot.title = element_text(size=28),
                         legend.text=element_text(face="bold", size=8),
                         legend.title=element_text(size=20),
                         legend.position = c(0.23, .25)))

gp1 <- ggplot() + 
  geom_polygon(data=comp,aes(long,lat,group=group),fill='grey95',alpha=0.8,col='grey70')  +
  geom_polygon(data=b,aes(long,lat,group=group,fill=Territoire),col='grey10',lwd=0.1) + 
  geom_polygon(data=d3,aes(long,lat,group=group),fill=NA,col='grey10',lwd=0.9) +
  geom_polygon(data=d3,aes(long,lat,group=group),fill=NA,col='grey50',lwd=0.6)+
  scale_fill_manual(name = "Grand Paris\n12 territoires",values = myColors, labels=lab) +
  #guide = guide_legend(title.position = NULL)) + 
  
  coord_fixed(xlim=c(min(b$long)-13000,max(b$long)+200),
              ylim=c(min(b$lat)-200,max(b$lat)+200)) + 
  theme_opts + # theme(legend.position="left") + 
  geom_text(data=v,
            aes(x, y, label = Nom_de_la_commune),
            fontface = 'bold', color = 'grey10',
            #label.padding=unit(0.1, "lines"),fill=NA,
            #box.padding = unit(0.25, "lines"),
            alpha=0.7,size=0.8
            # segment.size = 0,
            # segment.color = NA
  )


gp1

library(sf)
library(rgdal)
# shp from IGN
dep <- readOGR('shp', 'DEPARTEMENT')
dep <- dep[dep@data$CODE_DEPT %in% c('75', '77', '78', '91', '92', '93', '94', '95'),]

dep_f <- fortify(dep, region = "CODE_DEPT")

gp_carbon <- ggplot() + 
  geom_polygon(data=comp,aes(long,lat,group=group),fill='grey75',alpha=0.8,col='white', lwd = 0.2)  +
  geom_polygon(data=b,aes(long,lat,group=group), fill = 'grey20', col='grey40',lwd=0.0003) + 
  geom_path(data=dep_f, aes(long,lat,group=group), alpha = 0.34, col='white', lwd=2.0) + 
  geom_path(data=dep_f, aes(long,lat,group=group), alpha = 0.33, col='red4', lwd=2.1) + 
  coord_fixed(xlim=c(min(b$long)-48800,max(b$long)+1500),
              ylim=c(min(b$lat)-2000,max(b$lat)+10000)) + 
  theme_opts #+ # theme(legend.position="left") + 

gp_carbon

# https://opendata.apur.org/datasets/plan-eau/explore?location=48.843002%2C2.423601%2C11.16
water_ <- readOGR('shp', 'PLAN_EAU')

# View(water_@data)
seine <- water_[water_@data$L_EAU %in% c('Seine', 'Port de Gennevilliers', 'Marne'),]

seine_f <- fortify(seine)

canaux <- water_[water_@data$L_EAU %in% c('Seine', 'Canal Saint-Denis', 'Canal Saint-Martin', 
                                          "Bassin de l'Arsenal", "Bassin de la Villette",
                                          "Canal de l'Ourcq"),]

canaux_f <- fortify(canaux)

gp_carbon + 
  geom_polygon(data = seine_f, aes(x = long, y = lat, group = group), color = 'cornflowerblue', fill = 'grey35', 
               alpha = 0.8, lwd = 0.1) +
  geom_path(data = seine_f, aes(x = long, y = lat, group = group), color = 'cornflowerblue',
            alpha = 0.4, lwd = 0.1) +
  geom_path(data = seine_f, aes(x = long, y = lat, group = group), color = 'cornflowerblue', lwd = 0.01) +
  geom_polygon(data = seine_f, aes(x = long, y = lat, group = group), color = 'cornflowerblue', fill = 'cornflowerblue',
               alpha = 0.8, lwd = 0.01) +
  geom_polygon(data = canaux_f, aes(x = long, y = lat, group = group), color = 'cornflowerblue', fill = 'grey35', 
               alpha = 0.02, lwd = 0.0002) +
  geom_path(data = canaux_f, aes(x = long, y = lat, group = group), color = 'cornflowerblue',
            alpha = 0.01, lwd = 0.0002) +
  geom_path(data = canaux_f, aes(x = long, y = lat, group = group), color = 'cornflowerblue', lwd = 0.002, alpha = 0.01) +
  geom_polygon(data = canaux_f, aes(x = long, y = lat, group = group), color = 'cornflowerblue', fill = 'cornflowerblue',
               alpha = 0.05, lwd = 0.0001) +
  # ggtitle('Grand Paris') +
  geom_text(aes(x = mean(b$long) - 0.4e4, y = mean(b$lat) + 2e4, label = 'Grand Paris'), col = 'black', size = 10) +
  theme(panel.border = element_rect(colour = "grey35", fill=NA, size=4.5),
        panel.background = element_rect(fill = "grey85"))

ggsave('test_gp_seine_carbon2.pdf', width = 29.7, height = 21, units = "cm")


gp1 + 
  geom_polygon(data = seine_f, aes(x = long, y = lat, group = group), color = '#000080', fill = 'grey35', 
               alpha = 0.8, lwd = 0.1) +
  geom_path(data = seine_f, aes(x = long, y = lat, group = group), color = '#000080',
            alpha = 0.4, lwd = 0.1) +
  geom_path(data = seine_f, aes(x = long, y = lat, group = group), color = '#000080', lwd = 0.01) +
  geom_polygon(data = seine_f, aes(x = long, y = lat, group = group), color = '#000080', fill = '#000080',
               alpha = 0.8, lwd = 0.01) +
  # geom_text(aes(x = mean(b$long) - 0.4e4, y = mean(b$lat) + 2e4, label = 'Grand Paris'), col = 'black', size = 10) +
  theme(panel.border = element_rect(colour = "grey35", fill=NA, size=4.5),
        panel.background = element_rect(fill = "grey85"))  +
  geom_label_repel(data=u,
                   aes(x, y, label = rownames(u)),
                   fontface = 'bold', color = 'white',
                   box.padding = unit(0, "lines"),fill='black',alpha=1,size=4,segment.size = 0,
                   segment.color = NA,force=1
  )

ggsave('test_gp_apur_like.pdf', width = 29.7, height = 21, units = "cm")

