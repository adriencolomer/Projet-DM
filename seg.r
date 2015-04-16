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


#On choisit celle l� car on ne voit aucune diff�rence entre chaque taux d'utilisation par mois
vd = cut(telecom_2$TU_M2, breaks=c(-2,0,0.68,1,30))
info_for_pdf (vd,telecom_2$FLAG_RESIL, "TU_M1 seg2")

#Abandon des indicateurs TU_M1, TU_M2, TU_M3 car tr�s 

#Etude de VAR et de POVAR
#seg1 - initial avec le summary
vec = sort(union (c(-200,200), seq(-2.5,2.5,by=0.5)))
vd = cut(telecom_2$VAR/1000, breaks=vec)
info_for_pdf (vd,telecom_2$FLAG_RESIL, "VAR seg1")

#seg2 - Cr�ation de 4 classe en regroupant
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
#Fusion de [1,2] avec [2,100] car m�me prob et petite frequence
vec = c(-100,-1,0,1, 100)
vd  = cut(telecom_2$POVAR, breaks=vec)
info_for_pdf (vd,telecom_2$FLAG_RESIL, "POVAR seg3")

#seg 4
#[-1;0] et [0;1] trop grand je veux voir si on les s�pare si je ne peux pas obtenir des probabilit�s diff�rentes (avec un centile par exemple)
vec = union(c(-100,100), seq(-1,1,by=0.2))
vd  = cut(telecom_2$POVAR, breaks=vec)
info_for_pdf (vd,telecom_2$FLAG_RESIL, "POVAR seg4")

#Je pense qu'il vaut mieux utiliser le segmentation 3 - la segmentation 4 montre peu de diff�rence entre -1 et 1. Il ya juste une forte variation de la fr�quence
dev.off()
