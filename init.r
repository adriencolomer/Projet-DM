#chargement des fichiers
library(rpart)
library(lattice)
library(FactoMineR)

dirdonnee = "donnee";
#chargement de toutes les données (automatique à chaque chargement du fichier)
base <- read.table(paste(dirdonnee,"telecom.csv",sep="\\"), header = TRUE,sep=";");
telecom = data.frame(base);

base2 <- read.table(paste(dirdonnee,"telecom_2.csv",sep="\\"), header = TRUE,sep=";");
telecom_2 = data.frame(base2);

base3 <- read.table(paste(dirdonnee,"telecom_3.csv",sep="\\"), header = TRUE,sep=";");
telecom_3 = data.frame(base3);

#ID_CLIENT;FLAG_RESIL;DATE_ACTIVATION;DATE_NAISSANCE;OFFRE;NB_SERVICES;
#APPELS_DS_FFT_M1;APPELS_DS_FFT_M2;APPELS_DS_FFT_M3;
#APPELS_DEP_M1;APPELS_DEP_M2;APPELS_DEP_M3;
#APPELS_HORS_FFT_M1;APPELS_HORS_FFT_M2;APPELS_HORS_FFT_M3

#Construction du fichier telecom_2.csv avec les données du fichier telecom.csv, on ajoute que quelque indicateurs
create_indicateur <- function()
{
	#FLAG_RESIL
	print('Transformation de flag_resil en variable qualitatif');
	telecom$FLAG_RESIL = ifelse(telecom$FLAG_RESIL==1,'RESIL_OUI','RESIL_NON')
	
	#MOY
	print('Construction des MOY');
	telecom$MOY_DS_FR   = (telecom$APPELS_DS_FFT_M1   + telecom$APPELS_DS_FFT_M2   + telecom$APPELS_DS_FFT_M3  ) / 3;
	telecom$MOY_DEP_FR  = (telecom$APPELS_DEP_M1      + telecom$APPELS_DEP_M2      + telecom$APPELS_DEP_M3     ) / 3;
	telecom$MOY_HORS_FR = (telecom$APPELS_HORS_FFT_M1 + telecom$APPELS_HORS_FFT_M2 + telecom$APPELS_HORS_FFT_M3) / 3;
	
	#AGE & ANC
	print ('Construction de AGE & ANC');
	telecom$AGE = as.numeric(floor(difftime (as.Date('2010-12-31'),as.Date(telecom$DATE_NAISSANCE, '%d/%m/%Y'), units="auto")/365.25));
	telecom$ANC = as.numeric(floor(difftime (as.Date('2010-12-31'),as.Date(telecom$DATE_ACTIVATION, '%d/%m/%Y'), units="auto")/365.25));
	
	#P_APPELS_DS_FFT
	print ('Construction de P_APPELS_DS_FFT');
	telecom$P_APPELS_DS_FFT = telecom$MOY_DS_FR / (telecom$MOY_DS_FR+ telecom$MOY_DEP_FR + telecom$MOY_HORS_FR);
	
	#VAR & POVAR
	print('Construction de VAR & POVAR');
	telecom$VAR    = (telecom$APPELS_DS_FFT_M3 +telecom$APPELS_DEP_M3 ) - (telecom$APPELS_DS_FFT_M1 +telecom$APPELS_DEP_M1);
	telecom$POVAR  = (telecom$VAR) / (telecom$APPELS_DS_FFT_M1+telecom$APPELS_DEP_M1);
	
	#VAR_M1M2 VAR_M2M3 VAR M1M3
	print('Construction de VAR_M1M2, de VAR_M2M3, de VAR_M1M3 et de VAR_MOY')
	telecom$VAR_M1M2 = (telecom$APPELS_DS_FFT_M2 +telecom$APPELS_DEP_M2 ) - (telecom$APPELS_DS_FFT_M1 +telecom$APPELS_DEP_M1);
	telecom$VAR_M2M3 = (telecom$APPELS_DS_FFT_M3 +telecom$APPELS_DEP_M3 ) - (telecom$APPELS_DS_FFT_M2 +telecom$APPELS_DEP_M2);
	telecom$VAR_M1M3 = (telecom$APPELS_DS_FFT_M3 +telecom$APPELS_DEP_M3 ) - (telecom$APPELS_DS_FFT_M1 +telecom$APPELS_DEP_M1);
	telecom$VAR_MOY  = (telecom$VAR_M1M2 + telecom$VAR_M2M3 + telecom$VAR_M1M3)/3;
	
	#Calcul TU
	print('Construction des TU')
	OFFRES         = as.numeric(lapply(telecom$OFFRE,getNBAppelWithOffre));
	telecom$TU_M1  = (telecom$APPELS_DS_FFT_M1+telecom$APPELS_DEP_M1 )/ OFFRES;
	telecom$TU_M2  = (telecom$APPELS_DS_FFT_M2+telecom$APPELS_DEP_M2 )/ OFFRES;
	telecom$TU_M3  = (telecom$APPELS_DS_FFT_M3+telecom$APPELS_DEP_M3 )/ OFFRES;
	telecom$MOY_TU = (telecom$TU_M1 + telecom$TU_M2 + telecom$TU_M3) / 3;
	
	#Construction OFFRE IDEALE
	print('Construction OFFRE_IDEAL');
	telecom$OFFRE_IDEAL = as.character(lapply(telecom$MOY_DS_FR + telecom$MOY_DEP_FR, getOffreWithNBAppel));
	telecom$CHOIX_OFFRE = ifelse (telecom$OFFRE_IDEAL == telecom$OFFRE,'CHOIX_OUI','CHOIX_NON');
	
	#Construction REPARTITION ENTRE DS_FFT, DEP et hors FFT PAR MOIS
	print('Construction REPARTITION ENTRE DS_FFT, DEP et Hors FFT PAR MOIS');
	telecom$REP_DS_FFT_M1  = telecom$APPELS_DS_FFT_M1/(telecom$APPELS_DS_FFT_M1 + telecom$APPELS_DEP_M1 + telecom$APPELS_HORS_FFT_M1);
	telecom$REP_DS_FFT_M2  = telecom$APPELS_DS_FFT_M2/(telecom$APPELS_DS_FFT_M2 + telecom$APPELS_DEP_M2 + telecom$APPELS_HORS_FFT_M2);
	telecom$REP_DS_FFT_M3  = telecom$APPELS_DS_FFT_M3/(telecom$APPELS_DS_FFT_M3 + telecom$APPELS_DEP_M3 + telecom$APPELS_HORS_FFT_M3);
	telecom$REP_DS_FFT_MOY = (telecom$REP_DS_FFT_M1 + telecom$REP_DS_FFT_M2 + telecom$REP_DS_FFT_M3) /3;
	
	telecom$REP_DEP_M1  = telecom$APPELS_DEP_M1/(telecom$APPELS_DS_FFT_M1 + telecom$APPELS_DEP_M1 + telecom$APPELS_HORS_FFT_M1);
	telecom$REP_DEP_M2  = telecom$APPELS_DEP_M2/(telecom$APPELS_DS_FFT_M2 + telecom$APPELS_DEP_M2 + telecom$APPELS_HORS_FFT_M2);
	telecom$REP_DEP_M3  = telecom$APPELS_DEP_M3/(telecom$APPELS_DS_FFT_M3 + telecom$APPELS_DEP_M3 + telecom$APPELS_HORS_FFT_M3);
	telecom$REP_DEP_MOY = (telecom$REP_DEP_M1 + telecom$REP_DEP_M2 + telecom$REP_DEP_M3) /3;
	
	telecom$REP_HORS_FFT_M1  = telecom$APPELS_HORS_FFT_M1/(telecom$APPELS_DS_FFT_M1 + telecom$APPELS_DEP_M1 + telecom$APPELS_HORS_FFT_M1);
	telecom$REP_HORS_FFT_M2  = telecom$APPELS_HORS_FFT_M2/(telecom$APPELS_DS_FFT_M2 + telecom$APPELS_DEP_M2 + telecom$APPELS_HORS_FFT_M2);
	telecom$REP_HORS_FFT_M3  = telecom$APPELS_HORS_FFT_M3/(telecom$APPELS_DS_FFT_M3 + telecom$APPELS_DEP_M3 + telecom$APPELS_HORS_FFT_M3);
	telecom$REP_HORS_FFT_MOY = (telecom$REP_HORS_FFT_M1 + telecom$REP_HORS_FFT_M2 + telecom$REP_HORS_FFT_M3) /3;
	
	print('Construction TOTAL_APPEL, TOTAL_HORS_FFT');
	telecom$TOTAL_APPEL    = telecom$APPELS_DS_FFT_M1 + telecom$APPELS_DEP_M1 + telecom$APPELS_DS_FFT_M2 + telecom$APPELS_DEP_M2 + telecom$APPELS_DS_FFT_M3 + telecom$APPELS_DEP_M3;	
	telecom$TOTAL_HORS_FFT = telecom$APPELS_HORS_FFT_M1 + telecom$APPELS_HORS_FFT_M2 + telecom$APPELS_HORS_FFT_M3;
	
	print('Construction REPARTITION PAR MOIS');
	telecom$REP_APPEL_BYMONTH_M1 = (telecom$APPELS_DS_FFT_M1 + telecom$APPELS_DEP_M1) / (telecom$TOTAL_APPEL);
	telecom$REP_APPEL_BYMONTH_M2 = (telecom$APPELS_DS_FFT_M2 + telecom$APPELS_DEP_M2) / (telecom$TOTAL_APPEL);
	telecom$REP_APPEL_BYMONTH_M3 = (telecom$APPELS_DS_FFT_M3 + telecom$APPELS_DEP_M3) / (telecom$TOTAL_APPEL);
	
	print('debut boucle');
	
	for (i in 1:nrow(telecom))
	{	
		if (telecom$OFFRE[i] == 'O7')
		{
			telecom$TU_M1[i]  = -2;
			telecom$TU_M2[i]  = -2;
			telecom$TU_M3[i]  = -2;
			telecom$MOY_TU[i] = -2;
		}
		#VAR
		telecom$VAR_DS_FR[i]   = var (c(telecom$APPELS_DS_FFT_M1[i], telecom$APPELS_DS_FFT_M2[i], telecom$APPELS_DS_FFT_M3[i]));
		telecom$VAR_DEP_FR[i]  = var (c(telecom$APPELS_DEP_M1[i], telecom$APPELS_DEP_M2[i], telecom$APPELS_DEP_M3[i]));
		telecom$VAR_HORS_FR[i] = var (c(telecom$APPELS_HORS_FFT_M1[i], telecom$APPELS_HORS_FFT_M2[i], telecom$APPELS_HORS_FFT_M3[i]));
		
		#SD
		telecom$SD_DS_FR[i]   = sd (c(telecom$APPELS_DS_FFT_M1[i], telecom$APPELS_DS_FFT_M2[i], telecom$APPELS_DS_FFT_M3[i]));
		telecom$SD_DEP_FR[i]  = sd (c(telecom$APPELS_DEP_M1[i], telecom$APPELS_DEP_M2[i], telecom$APPELS_DEP_M3[i]));
		telecom$SD_HORS_FR[i] = sd (c(telecom$APPELS_HORS_FFT_M1[i], telecom$APPELS_HORS_FFT_M2[i], telecom$APPELS_HORS_FFT_M3[i]));
			
	 }
	telecom = subset(telecom, 15 <= AGE & AGE <= 79 & difftime (as.Date('2010-12-31'),as.Date(telecom$DATE_ACTIVATION, '%d/%m/%Y'), units="auto") > 90);
	
	#print ("NB LIGNE : ",nrow(telecom))
	write.table(telecom,file=paste(dirdonnee,"telecom_2.csv",sep="\\"),,row.names=TRUE,col.names=TRUE,sep=";");
	source("init.r");
}

#Construction de classe Plus simple pour voir si je peux apprendre plus de chosev| idem que la fonction suivante sauf que c'est d'autre indicateur
create_class2 <- function(base)
{
	#CLASS_AGE
	#resultat=data.frame(ID_CLIENT = base$ID_CLIENT;
	resultat=data.frame(FLAG_RESIL = base$FLAG_RESIL);
	
	print("Construction CLASS_AGE 2");
	resultat$CLASS_AGE = as.character(lapply(base$AGE,function(x){
		class_age = "";
		if 		(x <= 50) {class_age = '[0:50]';}
		else 			  {class_age = '[51:110]';}
	}));
	
	print("Construction CLASS_OFFRE");
	resultat$CLASS_OFFRE = as.character(lapply(base$OFFRE,function(x){
		class_offre = "";
		if 		(x == 'O1' || x == 'O2') {class_offre = 'O1O2';}
		else if (x == 'O3' || x == 'O4') {class_offre = 'O3O4';}
		else if (x == 'O5' || x == 'O6') {class_offre = 'O5O6';}
		else 			  				 {class_offre = 'O7';}
		return (class_offre);
	}));
	
	print("Construction CLASS_DEP 2");
	resultat$CLASS_DEP = as.character(lapply(base$MOY_DEP_FR,function(x){
		classe = "";
		if (is.na (x))      {classe = NA;}
		else if (x == 0)    {classe = 'NO_DEP';}
		else			    {classe = 'DEP';}
		return (classe);
	}));
	
	print("Construction CLASS_TU");
	resultat$CLASS_TU = as.character(lapply(base$MOY_TU,function(x){
		classe = "";
		if      (is.na (x)) {classe = NA;}
		else if (x <= 1)	{classe = 'UTIL_NORMAL';}
		else			    {classe = 'UTIL_EXCE';}
		return (classe);
	}));
	
	print("Construction CLASS_HORS_FFT");
	resultat$CLASS_HORS_FFT = as.character(lapply(base$MOY_HORS_FR,function(x){
		classe = "";
		if      (is.na(x)) {classe = NA;}
		else if (x == 0)   {classe = 'HORS_FFT_NON';}
		else			   {classe = 'HORS_FFT_OUI';}
		return (classe);
	}));
	
	print("Construction CLASS_TYP_VAR");
	resultat$CLASS_TYP_VAR = as.character(lapply(base$VAR_MOY,function(x){
		classe = "";
		if      (is.na(x))               {classe = NA;}
		else if (-1000 < x & x < 1000)   {classe = 'PEU_VAR';}
		else if (-2500 < x & x < 2500)   {classe = 'MOY_VAR';}
		else			   				 {classe = 'GRANDE_VAR';}
		return (classe);
	}));
	
	resultat_2 = subset(resultat, CLASS_TU != 'NA' & CLASS_DEP != 'NA');
	
	write.table(resultat_2,file=paste(dirdonnee,"telecom_3.csv",sep="\\"),,row.names=TRUE,col.names=TRUE,sep=";");
	source("init.r");
}

#Ecriture du fichier telecom_3.csv en remplaçant tout les indicateurs par des classes
create_class <- function(base)
{
	#CLASS_AGE
	#resultat=data.frame(ID_CLIENT = base$ID_CLIENT;
	resultat=data.frame(FLAG_RESIL = base$FLAG_RESIL);
	#resultat$FLAG_RESIL = base$FLAG_RESIL;
	print("Construction CLASS_AGE");
	resultat$CLASS_AGE = as.character(lapply(base$AGE,function(x){
		class_age = "";
		if 		(x <= 25) {class_age = '[0:25]';}
		else if (x <= 40) {class_age = '[26:40]';}
		else if (x <= 60) {class_age = '[41:60]';}
		else 			  {class_age = '[61:110]';}
	}));
	
	print("Construction CLASS_ANC");
	resultat$CLASS_ANC = as.character(lapply(base$ANC,function(x){
		class_age = "";
		if 		(x <= 1) {class_age = 'ANC1';}
		else if (x <= 4) {class_age = 'ANC2';}
		else if (x >= 5) {class_age = 'ANC3';}
		else 			 {class_age = 'ANC4';}
	}));
	
	# print("Construction CLASS_AGE 2");
	# resultat$CLASS_AGE = as.character(lapply(base$AGE,function(x){
		# class_age = "";
		# if 		(x <= 50) {class_age = '[0:50]';}
		# else 			  {class_age = '[51:110]';}
	# }));
	
	print("Construction CLASS_OFFRE");
	resultat$CLASS_OFFRE = as.character(lapply(base$OFFRE,function(x){
		class_offre = "";
		if 		(x == 'O1' || x == 'O2') {class_offre = 'O1O2';}
		else if (x == 'O3' || x == 'O4') {class_offre = 'O3O4';}
		else if (x == 'O5' || x == 'O6') {class_offre = 'O5O6';}
		else 			  				 {class_offre = 'O7';}
		return (class_offre);
	}));
	
	print("Construction CLASS_DEP");
	resultat$CLASS_DEP = as.character(lapply(base$MOY_DEP_FR,function(x){
		classe = "";
		if (is.na (x))      {classe = NA;}
		else if (x == 0)    {classe = 'NO_DEP';}
		else if (x <= 2000) {classe = 'DEP_3600';}
		else			    {classe = 'BIG_DEP';}
		return (classe);
	}));
	
	print("Construction CLASS_DEP 2");
	resultat$CLASS_DEP = as.character(lapply(base$MOY_DEP_FR,function(x){
		classe = "";
		if (is.na (x))      {classe = NA;}
		else if (x == 0)    {classe = 'NO_DEP';}
		else			    {classe = 'DEP';}
		return (classe);
	}));
	
	print ("Construction CLASS_APPELS")
	resultat$CLASS_APPELS = as.character(lapply(base$MOY_DS_FR + base$MOY_DEP_FR + base$MOY_HORS_FR,function (x){
		classe = '';
		if      (is.na(x))  {classe = 'APPNA';}
		else if (x <= 0)	{classe = 'APP0';}
		else if	(x < 3600)  {classe = 'APP1';}
		else if (x < 7200)  {classe = 'APP2';}
		else if (x < 18000) {classe = 'APP3';}
		else    	        {classe = 'APP4';}
	}));
	
	
	print("Construction CLASS_P_APPELS_DS_FFT")
	resultat$CLASS_P_APPELS_DS_FFT = as.character(lapply(base$P_APPELS_DS_FFT,function (x){
		classe = '';
		if      (is.na(x))  {classe = 'Autres';}
		else if (x < 0.5)	{classe = 'PAP1';}
		else if	(x <= 0.9)  {classe = 'PAP2';}
		else                {classe = 'PAP3';}
	}));

	print("Construction CLASS_VAR");
	resultat$CLASS_VAR = as.character(lapply(base$VAR_MOY,function(x){
		classe = "";
		if (is.na (x))  {classe = NA;}
		else if (x > 0) {classe = 'CROISSANCE';}
		else if (x < 0) {classe = 'DECROISSANCE';}
		else			{classe = 'PAS_VARIATION';}
		return (classe);
	}));
	
	print("Construction CLASS_TU");
	resultat$CLASS_TU = as.character(lapply(base$MOY_TU,function(x){
		classe = "";
		if      (is.na (x)) {classe = NA;}
		else if (x <  0)    {classe = 'UTIL_ILIM';}
		else if (x <= 0.5)  {classe = 'UTIL_BAS';}
		else if (x <= 0.8)  {classe = 'UTIL_NORMAL';}
		else if (x <= 1)	{classe = 'UTIL_HAUTE';}
		else			    {classe = 'UTIL_EXCE';}
		return (classe);
	}));
	
	print("Construction CLASS_HORS_FFT");
	resultat$CLASS_HORS_FFT = as.character(lapply(base$MOY_HORS_FR,function(x){
		classe = "";
		if      (is.na(x)) {classe = NA;}
		else if (x == 0)   {classe = 'HORS_FFT_NON';}
		else			   {classe = 'HORS_FFT_OUI';}
		return (classe);
	}));

	print("Construction CLASS_QT_M1");
	resultat$CLASS_QT_M1 = as.character(lapply(base$APPELS_DS_FFT_M1,function(x){
		classe = "";
		if      (is.na(x)) {classe = NA;}
		else if (x == 0)   {classe = 'PAS_APPEL_M1';}
		else			   {classe = 'APPEL_M1';}
		return (classe);
	}));

	print("Construction CLASS_QT_M2");
	resultat$CLASS_QT_M2 = as.character(lapply(base$APPELS_DS_FFT_M2,function(x){
		classe = "";
		if      (is.na(x)) {classe = NA;}
		else if (x == 0)   {classe = 'PAS_APPEL_M2';}
		else			   {classe = 'APPEL_M2';}
		return (classe);
	}));
	
	print("Construction CLASS_QT_M3");
	resultat$CLASS_QT_M3 = as.character(lapply(base$APPELS_DS_FFT_M3,function(x){
		classe = "";
		if      (is.na(x)) {classe = NA;}
		else if (x == 0)   {classe = 'PAS_APPEL_M3';}
		else			   {classe = 'APPEL_M3';}
		return (classe);
	}));
	
	print("Construction CLASS_TYP_VAR");
	resultat$CLASS_TYP_VAR = as.character(lapply(base$VAR_MOY,function(x){
		classe = "";
		if      (is.na(x))               {classe = NA;}
		else if (-1000 < x & x < 1000)   {classe = 'PEU_VAR';}
		else if (-2500 < x & x < 2500)   {classe = 'MOY_VAR';}
		else			   				 {classe = 'GRANDE_VAR';}
		return (classe);
	}));
	
	print("Construction CLASS_SERVICE");
	resultat$CLASS_SERVICES = as.character(lapply(base$NB_SERVICES,function(x){
		classe = "";
		if      (is.na(x)) {classe = 'SERNA';}
		else if (x <= 1)   {classe = 'SER1';}
		else if (x == 2)   {classe = 'SER2';}
		else if (x == 3)   {classe = 'SER3';}
		else if (x >= 4)   {classe = 'SER4';}
		else			   {classe = 'SERNA';}
		return (classe);
	}));
	
	# print("Construction CLASS_TYP_VAR");
	# resultat$CLASS_TYP_VAR = as.character(lapply(base$VAR_MOY,function(x){
		# classe = "";
		# if      (is.na(x))               {classe = NA;}
		# else if (x < -2500)              {classe = 'GRANDE_VAR_NEG';}
		# else if (x < -1000)              {classe = 'MOY_VAR_NEG'}
		# else if (x < 0)                  {classe = 'PEU_VAR_NEG'}
		# else if (x < 1000)               {classe = 'PEU_VAR_POS'}
		# else if (x < 2500)               {classe = 'MOY_VAR_POS';}
		# else			   				 {classe = 'GRANDE_VAR_POS';}
		# return (classe);
	# }));
	
	resultat$CHOIX_OFFRE = base$CHOIX_OFFRE;
	
	resultat_2 = subset(resultat, CLASS_TU != 'NA' & CLASS_DEP != 'NA' & CLASS_VAR != 'NA');
	
	write.table(resultat_2,file=paste(dirdonnee,"telecom_3.csv",sep="\\"),,row.names=TRUE,col.names=TRUE,sep=";");
	source("init.r");
}

#
#Exemple pour utiliser arbre, Ne pas utiliser cette fonction
#
generate_arbre <- function(base)
{
	y <- 2*nrow(base)/10
	base_appr <- base[y:nrow(base),]
	base_test <- base[1:y-1,]

	arbre <<- rpart (FLAG_RESIL ~ ., data=base_appr,method="class")
	predi <<- predict (arbre, newdata=base_test,type="class")
	
	mc <<- table(base_test$FLAG_RESIL,predi)
	erreur <<- (mc[2,1]+mc[1,2])/sum(mc)
	print(erreur)
}

#
#permet  d'obtenir le nombre de seconde sans dépassement pour l'offre x
#
#Exemple d'utilisation:
#getNBAppelWithOffre ('O1') => renvoie 1800 (car l'offre O1 donne droit à une demi-heure d'utilisation)
#
getNBAppelWithOffre <- function(x)
{
	NB_APPELS = 1;
	if     (x == 'O1'){NB_APPELS = 1800;}
	else if(x == 'O2'){NB_APPELS = 3600;}
	else if(x == 'O3'){NB_APPELS = 7200;}
	else if(x == 'O4'){NB_APPELS = 18000;}
	else if(x == 'O5'){NB_APPELS = 36000;}
	else if(x == 'O6'){NB_APPELS = 54000;}
	else 			  {NB_APPELS = -1;}
	return (NB_APPELS);
}

#
#renvoie l'offre la plus adapté pour le nombre de secondes x
#
#Exemple d'utilisation:
#getOffreWithNBAppel (1800) => renvoie O1 car x ne dépasse pas le maximum autorisé par l'O1
#
getOffreWithNBAppel <- function (x)
{
	OFFRE_IDEAL = '';
	if      (is.na(x))   {OFFRE_IDEAL = NA;}    
	else if (x <= 1800)  {OFFRE_IDEAL = 'O1';}
	else if (x <= 3600)  {OFFRE_IDEAL = 'O2';}
	else if (x <= 7200)  {OFFRE_IDEAL = 'O3';}
	else if (x <= 18000) {OFFRE_IDEAL = 'O4';}
	else if (x <= 36000) {OFFRE_IDEAL = 'O5';}
	else if (x <= 54000) {OFFRE_IDEAL = 'O6';}
	else 				 {OFFRE_IDEAL = 'O7';}
	return (OFFRE_IDEAL);
}

#permet de calculer la distance de la modalisation x et de la modalité  y en utilisant les données de coord
#
#
#Exemple d'utilisation:
#mca = MCA(telecom_3)
#calcul_distance (mca$var$coord,1,2) ==> calcule la distance entre modalité RESIL_OUI et RESIL_NON
#
calcul_distance <- function(coord,x,y)
{
	return ( sum((coord[x,]-coord[y,])*(coord[x,]-coord[y,])));
}

#
#permet de calculer les distance de la modalité d'index x avec toutes les autres modalités
#
#Exemple d'utilisation:
#mca = MCA (telecom_3,35)
#distance_par_modal(mca$var$coord,1); ==> calcule la distance entre RESIL_OUI avec toutes les modalités
#
distance_par_modal <- function(base,x)
{
	resultat = 1:nrow(base);
	for (i in 1:nrow(base))
	{
		resultat[i] = calcul_distance(base,x,i);
	}
	return (resultat);
}

#
#permet de calculer les distance de la modalité d'index x avec toutes les autres modalités
#
#Exemple d'utilisation:
#mca = MCA (telecom_3,35)
#distance_par_modal(mca$var$coord,1); ==> calcule la distance entre RESIL_OUI avec toutes les modalités
#
distance_par_modal2 <- function(base,x)
{
	resultat = 1:nrow(base);
	for (i in 1:nrow(base))
	{
		resultat[rownames(base)[i]] = calcul_distance(base,x,i);
	}
	return (resultat);
}

#
#permet de compléter les informations d'un table. Au lieu de marquer la fréquence, on la remplace par le calcul d'un pourcentage
#
#Exemple d'utilisation:
#pocol(table(telecom_3$CLASS_AGE, telecom_3$FLAG_RESIL)) ==> affiche le pourcentage colonne de la table pour chaque cellule
#
pocol <- function(base)
{
	for (i in 1:nrow(base))
	{
		base[i,]=base[i,]/sum(base[i,]);
	}
	return (base);
}

#
#permet de compléter les informations d'un table. Au lieu de marquer la fréquence, on la remplace par le calcul d'un pourcentage
#
#Exemple d'utilisation:
#pocol(table(telecom_3$CLASS_AGE, telecom_3$FLAG_RESIL)) ==> affiche le pourcentage colonne de la table pour chaque cellule
#
sumcol <- function(base)
{
	for (i in 1:nrow(base))
	{
		base[i,]=sum(base[i,]);
	}
	return (base);
}

#
#permet de compléter les informations d'un table. Au lieu de marquer la fréquence, on la remplace par le calcul d'un pourcentage
#
#Exemple d'utilisation:
#pocol(table(telecom_3$FLAG_RESIL, telecom_3$CLASS_AGE)) ==> affiche le pourcentage ligne de la table pour chaque cellule
#
polig <- function(base)
{
	for (i in 1:ncol(base))
	{
		base[,i]=base[,i]/sum(base[,i]);
	}
	return (base);
}

reload <- function ()
{
	source('init.r');
	create_indicateur();
	create_class(telecom_2);
}

info <- function(liste_A,liste_B, titre)
{
	t = table (liste_A, liste_B)
	x11()
	barplot(t[,1]+t[,2], main=paste('Tableau en barre des frequences de', titre))
	x11();
	t = pocol(t)
	barplot(t[,2], main=paste('Tableau en barre des pourcentage de', titre))
}

info_for_pdf <- function(liste_A,liste_B, titre)
{
	t = table (liste_A, liste_B)
	barplot(t[,1]+t[,2], main=paste('Tableau en barre des frequences de', titre))
	t = pocol(t)
	barplot(t[,2], main=paste('Tableau en barre des pourcentage de', titre))
}

info_complet_for_pdf <- function(liste_A, vecteur, titre)
{
	vd = cut(telecom_2$MOY_TU, breaks=vecteur)
	t = table (vd, telecom_2$FLAG_RESIL)
	barplot(t[,1]+t[,2], main=paste('Tableau en barre des frequences de', titre))
	t = pocol(t)
	barplot(t[,2], main=paste('Tableau en barre des pourcentage de', titre))
}

load_class <- function (nom_fichier)
{
	source (paste("class\\",nom_fichier,".r",sep=""))
	base <- read.table(paste("class\\",nom_fichier,".csv",sep=""), header = TRUE,sep=";");
	resultat = data.frame(base);
	return (resultat)
}

info_quanti <- function (ressource,titre)
{
	pdf (paste('class\\', titre,'.pdf',sep=''))
	for (i in 1:ncol(ressource))
	{
		colname = colnames(ressource)[i];
		print (colname)
		t = table(ressource[,colname], ressource$FLAG_RESIL)
		print(t)
		barplot(t[,1]+t[,2], main=paste('Tableau en barre des frequences de', colname))
		print(pocol(t))
		barplot(pocol(t)[,2], main=paste('Tableau en barre des pourcentage de', colname))
		print('')
		print(summary(table(ressource[,colname], ressource$FLAG_RESIL)))
		print('')
	}
	dev.off()
}

#
#renvoie les modalités qui ont une distance à "#indice_mod" inférieur à "#distance" 
#
#Paramètres:
#mca        : MCA du tableau de données (exemple : mca(telecom_3)
#indice_mod : indique l'indice pour lequel calculer la distance
#distance   : Distance entière
getModalites <- function (mca, indice_mod, distance)
{
	return (rownames(mca$var$coord)[which(distance_par_modal(mca$var$coord, indice_mod) < distance)])
}