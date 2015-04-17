
#CLASS_AGE
resultat=data.frame(FLAG_RESIL = telecom_2$FLAG_RESIL);

print("Construction CLASS_AGE");
resultat$CLASS_AGE = as.character(lapply(telecom_2$AGE,function(x){
	class_age = "";
	if 		(x <= 25) {class_age = '[0:25]';}
	else if (x <= 40) {class_age = '[26:40]';}
	else if (x <= 60) {class_age = '[41:60]';}
	else 			  {class_age = '[61:110]';}
}));

print("Construction CLASS_ANC");
resultat$CLASS_ANC = as.character(lapply(telecom_2$ANC,function(x){
	class_age = "";
	if 		(x <= 1) {class_age = 'ANC1';}
	else if (x <= 4) {class_age = 'ANC2';}
	else if (x <= 5) {class_age = 'ANC3';}
	else 			 {class_age = 'ANC4';}
}));

print("Construction CLASS_OFFRE");
resultat$CLASS_OFFRE = as.character(lapply(telecom_2$OFFRE,function(x){
	class_offre = "";
	if 		(x == 'O1' || x == 'O2') {class_offre = 'O1O2';}
	else if (x == 'O3' || x == 'O4') {class_offre = 'O3O4';}
	else if (x == 'O5' || x == 'O6') {class_offre = 'O5O6';}
	else 			  				 {class_offre = 'O7';}
	return (class_offre);
}));

print("Construction CLASS_DEP");
resultat$CLASS_DEP = as.character(lapply(telecom_2$MOY_DEP_FR,function(x){
	classe = "";
	if (is.na (x))      {classe = NA;}
	else if (x == 0)    {classe = 'NO_DEP';}
	else			    {classe = 'DEP';}
	return (classe);
}));

print ("Construction CLASS_APPELS")
resultat$CLASS_APPELS = as.character(lapply(telecom_2$MOY_DS_FR + telecom_2$MOY_DEP_FR + telecom_2$MOY_HORS_FR,function (x){
	classe = '';
	if      (is.na(x))  {classe = 'APPNA';}
	else if (x <= 0)	{classe = 'APP0';}
	else if	(x < 3600)  {classe = 'APP1';}
	else if (x < 7200)  {classe = 'APP2';}
	else if (x < 18000) {classe = 'APP3';}
	else    	        {classe = 'APP4';}
}));


print("Construction CLASS_P_APPELS_DS_FFT")
resultat$CLASS_P_APPELS_DS_FFT = as.character(lapply(telecom_2$P_APPELS_DS_FFT,function (x){
	classe = '';
	if      (is.na(x))  {classe = 'Autres';}
	else if (x < 0.5)	{classe = 'PAP1';}
	else if	(x <= 0.9)  {classe = 'PAP2';}
	else                {classe = 'PAP3';}
}));

print("Construction CLASS_HORS_FFT");
resultat$CLASS_HORS_FFT = as.character(lapply(telecom_2$MOY_HORS_FR,function(x){
	classe = "";
	if      (is.na(x)) {classe = NA;}
	else if (x == 0)   {classe = 'HORS_FFT_NON';}
	else			   {classe = 'HORS_FFT_OUI';}
	return (classe);
}));

print("Construction CLASS_SERVICE");
resultat$CLASS_SERVICES = as.character(lapply(telecom_2$NB_SERVICES,function(x){
	classe = "";
	if      (is.na(x)) {classe = 'SERNA';}
	else if (x <= 1)   {classe = 'SER1';}
	else if (x == 2)   {classe = 'SER2';}
	else if (x == 3)   {classe = 'SER3';}
	else if (x >= 4)   {classe = 'SER4';}
	else			   {classe = 'SERNA';}
	return (classe);
}));

#c(-2,0,0.6,0.9,30)
print("Construction CLASS_TU");
resultat$CLASS_TU = as.character(lapply(telecom_2$MOY_TU,function(x){
	classe = "";
	if      (is.na(x)) {classe   = 'TUNA';}
	else if (x <= 0)   {classe   = 'TU1';}
	else if (x <= 0.6) {classe   = 'TU2';}
	else if (x <= 0.9) {classe   = 'TU3';}
	else               {classe   = 'TU4';}
	return (classe);
}));

print("Construction CLASS_NBAPPELS")
resultat$CLASS_NBAPPELS = as.character(lapply(telecom_2$TOTAL_APPEL,function(x){
	classe = "";
	if      (is.na(x))   {classe   = 'NBNA';}
	else if (x <= 10000) {classe   = 'NBA1';}
	else if (x <= 35000) {classe   = 'NBA2';}
	else                 {classe   = 'NBA3';}
	return (classe);
}));

print("Construction CLASS_NBAPPELS_HF")
resultat$CLASS_NBAPPELS = as.character(lapply(telecom_2$TOTAL_HORS_FFT,function(x){
	classe = "";
	if      (is.na(x))  {classe   = 'NB_HF_NA';}
	else if (x == 0)    {classe   = 'NB_HF_0';}
	else if (x <= 2000) {classe   = 'NB_HF_1';}
	else if (x <= 4000) {classe   = 'NB_HF_2';}
	else                {classe   = 'NB_HF_3';}
	return (classe);
}));

write.table(resultat,file="class/class_1.csv",row.names=TRUE,col.names=TRUE,sep=";");