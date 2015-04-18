pdf("rapport/rapport.pdf")

#MOY_TU

#seg1
vec = c(-2,0,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8,2,30);
info_complet_for_pdf (telecom_2$MOY_TU, vec, "MOY_TU seg1")

#seg2
vec = c(-2,0,0.6,1,30);
info_complet_for_pdf (telecom_2$MOY_TU, vec, "MOY_TU seg2")

#seg3 --> proche des summary
vd = cut(telecom_2$MOY_TU, breaks=c(-2,0,0.6,0.9,30))
info_for_pdf (vd,telecom_2$FLAG_RESIL, "MOY_TU seg3")
# Choix de ces indicateurs pour MOY_TU

#TU_M1

vd = cut(telecom_2$TU_M1, breaks=c(-2,0,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8,2,30))
info_for_pdf (vd,telecom_2$FLAG_RESIL, "TU_M1 seg1")


#On choisit celle là car on ne voit aucune différence entre chaque taux d'utilisation par mois
vd = cut(telecom_2$TU_M2, breaks=c(-2,0,0.68,1,30))
info_for_pdf (vd,telecom_2$FLAG_RESIL, "TU_M1 seg2")

#Abandon des indicateurs TU_M1, TU_M2, TU_M3 car très 

#Etude de VAR et de POVAR
#seg1 - initial avec le summary
vec = sort(union (c(-200,200), seq(-2.5,2.5,by=0.5)))
vd = cut(telecom_2$VAR/1000, breaks=vec)
info_for_pdf (vd,telecom_2$FLAG_RESIL, "VAR seg1")

#seg2 - Création de 4 classe en regroupant
vec = c(-200, -2.5,-0.5,0,2.5,200)
vd = cut(telecom_2$VAR/1000, breaks=vec)
info_for_pdf (vd,telecom_2$FLAG_RESIL, "VAR seg2")

#Etude de POVAR
#seg 1
vec = sort(union(c(-100, 100),seq(-2, 2,by=0.5)))
vd  = cut(telecom_2$POVAR, breaks=vec)
info_for_pdf (vd,telecom_2$FLAG_RESIL, "POVAR seg1")

#seg 2
vec = c(-100,-2,-1,0,1,2, 100)
vd  = cut(telecom_2$POVAR, breaks=vec)
info_for_pdf (vd,telecom_2$FLAG_RESIL, "POVAR seg2")

#seg 3
#Fusion de [1,2] avec [2,100] car même prob et petite frequence
vec = c(-100,-1,0,1, 100)
vd  = cut(telecom_2$POVAR, breaks=vec)
info_for_pdf (vd,telecom_2$FLAG_RESIL, "POVAR seg3")

#seg 4
#[-1;0] et [0;1] trop grand je veux voir si on les sépare si je ne peux pas obtenir des probabilités différentes (avec un centile par exemple)
vec = union(c(-100,100), seq(-1,1,by=0.2))
vd  = cut(telecom_2$POVAR, breaks=vec)
info_for_pdf (vd,telecom_2$FLAG_RESIL, "POVAR seg4")

#Je pense qu'il vaut mieux utiliser le segmentation 3 - la segmentation 4 montre peu de différence entre -1 et 1. Il ya juste une forte variation de la fréquence

#Analyse de la variable TOTAL_APPEL a t'elle une incidence sur la résiliation?
vec = union(c(0,500), seq(0,50,by=5))
vd  = cut(telecom_2$TOTAL_APPEL/1000, breaks=vec)
info_for_pdf (vd,telecom_2$FLAG_RESIL, "TOTAL_APPEL seg1")

#En observant les tableaux générés, on remarque: 
#- Que plus le nombre d'appel est faible, et plus il y a des chances que le consomateur résilie (passage de 0.40 à 0.1)
#- Que le palier [0;10] totalise à eux seule plus de 30 à 40 % des résiliations alors qu'il ne représente que 2 000 individus
#- Qu'il existe une autre classe de [10;35], situe entre 20 et 10 % des résiliés
#- Le nombre d'appel élevé (inférieur à 10%)

vec = union(c(0,500), seq(0,10,by=1))
t = subset(telecom_2, TOTAL_APPEL <= 10000)
vd  = cut(t$TOTAL_APPEL/1000, breaks=vec)
info_for_pdf (vd,t$FLAG_RESIL, "TOTAL_APPEL [0;10] seg2")

#Entre [0;10], rien n'est significatif, il ya moins de 300 individus dans chaque colonne

vec = c(0,10,35,500)
vd  = cut(telecom_2$TOTAL_APPEL/1000, breaks=vec)
info_for_pdf (vd,telecom_2$FLAG_RESIL, "TOTAL_APPEL seg3")

#Segmentation TOTAL_HORS_FFT seg1
vec = union(c(-1,500), seq(0,50,by=5))
vd  = cut(telecom_2$TOTAL_HORS_FFT/1000, breaks=vec)
info_for_pdf (vd,telecom_2$FLAG_RESIL, "TOTAL_APPEL_FFT seg1")

#En observant les tableaux générés, on remarque:
#- Plus de 6000 individus compris entre 0;10
#- On remarque l'apparition de trois classe [0] [0;10][10;500]

#Segmentation TOTAL_HORS_FFT seg2 [0;10] car plus de 6000 individu compris [0;10]
vec = c(-1,0,seq(1,10,by=1))
tab = subset (telecom_2, TOTAL_HORS_FFT < 10000);
vd  = cut(tab$TOTAL_HORS_FFT/1000, breaks=vec)
info_for_pdf (vd,tab$FLAG_RESIL, "TOTAL_APPEL_FFT [0;10] seg2")

#Segmentation TOTAL_HORS_FFT seg3 plus fine
vec = sort(union(c(-1,500), seq(0,50,by=2)))
vd  = cut(telecom_2$TOTAL_HORS_FFT/1000, breaks=vec)
info_for_pdf (vd,telecom_2$FLAG_RESIL, "TOTAL_APPEL_FFT seg3")

#En observant les tableaux générés, on remarque:
# une classe TOTAL_HORS_FORFAIT == 0
# une classe [0;2]
# une classe [4;6]
# une classe [6;+++] mais en regardant on se rend compte que [4;6] = [6;+++]
vec = c(-1,0,2,4,100)
vd  = cut(telecom_2$TOTAL_HORS_FFT/1000, breaks=vec)
info_for_pdf (vd,telecom_2$FLAG_RESIL, "TOTAL_APPEL_FFT seg4")

#Etude SD_DS_FFT
vec = sort(union(c(-1,500), seq(0,60,by=6)))
vd  = cut(telecom_2$SD_DS_FR/1000, breaks=vec)
info_for_pdf (vd,telecom_2$FLAG_RESIL, "SD_DS_FR seg1")

#On remarque:
#la valeur 0 compte 1000 individus avec 0.20 de 0.4 prob
#la valeur [0;6] compte 4000 individus avec 0.23 prob
#la valeur [6;30] compte environs 2000 individus avec une prob de 0.10 et 0.20
#le reste moins de probabilité

vec = c(-1, 0,6,30,500)
vd  = cut(telecom_2$SD_DS_FR/1000, breaks=vec)
info_for_pdf (vd,telecom_2$FLAG_RESIL, "SD_DS_FR seg2")

#Etude 2 (fusion de [6;30] avec [30;500] car [30;500] très petit
vec = c(-1, 0,6,500)
vd  = cut(telecom_2$SD_DS_FR/1000, breaks=vec)
info_for_pdf (vd,telecom_2$FLAG_RESIL, "SD_DS_FR seg3")

#Etude 3 [0;6] représente 4000 individus je voudrais voir si on ne peut pas le diviser en 2 pour voir s'il représente quelque chose
vec = c(-1, 0,3,6,500)
vd  = cut(telecom_2$SD_DS_FR/1000, breaks=vec)
info_for_pdf (vd,telecom_2$FLAG_RESIL, "SD_DS_FR seg4")

#On peut fusionner [3;6] avec [6;500]
#Etude de 0;3 représente quand même 3500 individus
vec = c(-1, 0,1,2,3,500)
vd  = cut(telecom_2$SD_DS_FR/1000, breaks=vec)
info_for_pdf (vd,telecom_2$FLAG_RESIL, "SD_DS_FR seg5")

#On peut fusionner [3;6] avec [6;500]
#Segmentation finale
vec = c(-1, 0,2,500)
vd  = cut(telecom_2$SD_DS_FR/1000, breaks=vec)
info_for_pdf (vd,telecom_2$FLAG_RESIL, "SD_DS_FR seg5")


dev.off()
