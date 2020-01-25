#Définissez un répertoire de travail « C:/RTravail/RetinolM1 (commande 
getwd()
setwd("C:/RTravail/RetinolM1")
getwd()

#Chargez les packages nécessaires (prettyR, car)
library(prettyR)
library(car)

#Importez les 3 fichiers de données constituant la base : importez le fichier 
#« M1BB_TPDataMag_DataECF.csv » dans un objet nommé « dataecf » ; 
#importez le fichier « M1BB_TPDataMag_DataECM.csv » dans un objet nommé 
#« dataecm » ; importez le fichier « M1BB_TPDataMag_DataPlamsaAll.csv » 
#dans un objet nommé « dataplasma ».

dataecf <- read.csv2("M1BB_TPDataMag_DataECF.csv", header=TRUE)
head(dataecf)
dataecm <- read.csv2("M1BB_TPDataMag_DataECM.csv", header=TRUE)
head(dataecm)
dataplasma <- read.csv2("M1BB_TPDataMag_DataPlasmaAll.csv", header=TRUE)
head(dataplasma)

#Réalisez la fusion en lignes des objets « dataecf » et « dataecm » dans un nouvel 
#objet nommé « dataecall »
dataecall <- rbind(dataecf, dataecm)
head(dataecall)

#Vérifiez que votre objet « dataecall » contient bien 314 lignes et 14 colonnes, si 
#ca n'est pas le cas vous avez mal fait la fusion, reprenez alors la fusion
dim(dataecall)

#Réalisez la fusion en colonnes de « dataecall » et de « dataplasma » dans un 
#nouvel objet « dataall » 
dataall <- merge(dataecall, dataplasma, by="id")

#Effectuez une description rapide des modalités des variables catégorielles et des 
#valeurs minimum et maximum des variables quantitatives
tmpDataall <- dataall[,sapply(dataall,is.numeric)] 
describe(tmpDataall)
str(dataall)
sapply(dataall, summary, na.rm=TRUE) 

#Sexe : des erreurs de saisie semblent s'être glissées dans le fichier : il existe une 
#modalité « 2 » pour le sexe, après discussion avec l'équipe responsable du projet, 
#vous apprenez que cela provient d'un ancien accord sur le codage ou 2 
#correspondait initialement aux femmes. Recodez correctement les 2 dans la 
#bonne modalité. Reportez dans votre fichier de report les id des sujets pour 
#lesquels vous avez dû effectuer la correction et le code utilisé
dataall[dataall$sexe==2, "id"]
dataall$sexe <- recode(dataall$sexe, "2=1")

#Bmi : vous constatez des valeurs à 3 chiffres maximales aberrantes. Après 
#discussion, il s'agit d'une erreur de saisie, le « 1 » des centaines est en trop, mais 
#le reste de la valeur est correct. Recodez correctement les valeurs aberrantes. 
#Reportez dans votre fichier de report les id de sujets pour lesquels vous avez dû 
#effectuer la correction, la valeur initiale (aberrante) et finale (correcte) et le code 
#utilisé

bar <- subset(dataall, dataall$bmi>=100)
bar$id

bar$bmi <- substring(bar$bmi, 2)
bar
dataall$bmi[29] <- bar$bmi[1] #Change le bmi pour id 29
dataall$bmi[191] <- bar$bmi[2] #change le bmi pour id 191

#Renommez « beta1 » en « betadiet », « ret1 » en « retdiet », 
#« beta2 » en « betaplasma » et « ret2 » en « retplasma ». Indiquez dans votre 
#fichier de report de résultat le code utilisé
names(dataall)[13] <- "betadiet"
names(dataall)[14] <- "retdiet"
names(dataall)[15] <- "betaplasma"
names(dataall)[16] <- "retplasma"

#Renommez « beta1 » en « betadiet », « ret1 » en « retdiet », 
#« beta2 » en « betaplasma » et « ret2 » en « retplasma ». Indiquez dans votre 
#fichier de report de résultat le code utilisé

dataall$age <- as.numeric(difftime(as.Date(dataall$ddp, format="%d/%m/%Y"), as.Date(dataall$ddn, format="%d/%m/%Y"), units="days")/365.25)

#Créez une variable BMI en catégories « bmicat » à partir de la variable « bmi » 
#quantitative. Le codage doit être le suivant : 0 = < 18,5 ; 1 entre 18,5 et 25 exclu, 
#2 entre 25 et 30 exclu, 3 à partir de 30 et plus
dataall$bmicat <- recode(dataall$bmi, "0:18.49999=0 ; 18.5:24.99999=1 ; 25:29.99999=2 ; 30:99=3")

#Créez une variable BMI en catégories « bmicat » à partir de la variable « bmi » 
#quantitative. Le codage doit être le suivant : 0 = < 18,5 ; 1 entre 18,5 et 25 exclu, 
#2 entre 25 et 30 exclu, 3 à partir de 30 et plus
mean(dataall$retplasma) # = 603.7006
dataall$retplasmabin <- ifelse(dataall$retplasma < mean(dataall$retplasma), 0, 1)

#A partir de l'objet « dataall », créez un fichier csv 
#« M1BB_TPDataMag_BaseGelee.csv » qui sauvegardera en dur une base de 
#données prête à être analysée ; utilisez les options : quote=FALSE, 
#row.names=FALSE, na="" (pour que les données manquantes soient bien 
#codées par des espaces vides dans le fichier csv)
write.csv(dataall,"M1BB_TPDataMag_BaseGelee.csv", quote=FALSE, row.names=FALSE, na="")

#Définissez les fonctions nécessaires à l'analyse des données (Quartile25, 
#Quartile75, SommeNa, Sumfunct)
Quartile25 <- function(x, na.rm=TRUE) {
  if(na.rm) x <- x[!is.na(x)]
  quantile(x, probs=0.25)
}

Quartile75 <- function(x, na.rm=TRUE) {
  if(na.rm) x <- x[!is.na(x)]
  quantile(x, probs=0.75)
}

SommeNa <- function(x, na.rm=FALSE) {
  sum(is.na(x))
}

#On initialise la fonction pour nous faire un full
Sumfunct <- function(x) {
  c(Mean=round(mean(x, na.rm=TRUE), digits=3),
    ET=round(sd(x, na.rm=TRUE), digits=3), Q25=Quartile25(x, na.rm=TRUE),
    Median=median(x, na.rm=TRUE), Q75=Quartile75(x, na.rm=TRUE),
    Min=min(x, na.rm=TRUE), Max=max(x, na.rm=TRUE),
    SommeNA=SommeNa(x))
}

#Importez le fichier « M1BB_TPDataMag_BaseGelee.csv » dans un objet 
#nommé « donnees »
donnees <- read.csv("M1BB_TPDataMag_BaseGelee.csv", sep=",", dec=".")
head(donnees)
#Définissez correctement la typologie des variables de la base (factor, numeric, 
#Date)
donnees$sexe <- as.factor(donnees$sexe)
donnees$tabac <- as.factor(donnees$tabac)
donnees$vitamine <- as.factor(donnees$vitamine)
donnees$bmicat <- as.factor(donnees$bmicat)
donnees$retplasmabin <- as.factor(donnees$retplasmabin)
str(donnees)
# Date : ddn, ddp
donnees$ddn <- as.Date(donnees$ddn, format="%Y-%m-%d")
donnees$ddp <- as.Date(donnees$ddp, format="%Y-%m-%d")
str(donnees)

#Réalisez l'analyse descriptive des variables quantitatives de la base et reportez 
#les résultats dans le fichier de report de résultats
Sumfunct(donnees$age)
#Moyenne : 50.56200, Ecart-type : 14.57200, 25Q : 39.46064, MÃ©diane : 47.99580, 75Q : 62.72895, Min : 19.40041, Max : 83.66324, NA : 0

Sumfunct(donnees$bmi)
#Moyenne : 26.16600, Ecart-type : 6.02100, 25Q : 21.79413, MÃ©diane : 24.73935, 75Q : 28.90161, Min : 16.33114, Max : 50.40333, NA : 0

Sumfunct(donnees$calories)
#Moyenne : 1781.159, Ecart-type : 623.279, 25Q : 1335.900, MÃ©diane : 1665.050, 75Q : 2092.525, Min : 445.200, Max : 4373.600, NA : 0

Sumfunct(donnees$graisses)
#Moyenne : 76.755, Ecart-type : 33.521, 25Q : 53.925, MÃ©diane : 72.900, 75Q : 95.175, Min : 14.400, Max : 235.900, NA : 0

Sumfunct(donnees$fibres)
#Moyenne : 12.793, Ecart-type : 5.338, 25Q : 9.125, MÃ©diane : 12.100, 75Q : 15.600, Min : 3.100, Max : 36.800, NA : 0

Sumfunct(donnees$alcool)
#Moyenne : 2.643, Ecart-type : 4.949, 25Q : 0, MÃ©diane : 0.300, 75Q : 3.200, Min : 0, Max : 35, NA : 0

Sumfunct(donnees$cholesterol)
#Moyenne : 242.312, Ecart-type : 130.617, 25Q : 154.950, MÃ©diane : 206.200, 75Q : 308.225, Min : 37.700, Max : 900.700, NA : 0

Sumfunct(donnees$betadiet)
#Moyenne : 2183.350, Ecart-type : 1475.696, 25Q : 1115.00, MÃ©diane : 1795.00, 75Q : 2803.500, Min : 214.00, Max : 9642.00, NA : 0

Sumfunct(donnees$retdiet)
#Moyenne : 831.022, Ecart-type : 589.463, 25Q : 479.500, MÃ©diane : 707.000, 75Q : 1026.750, Min : 30.00, Max : 6901.00, NA : 0

Sumfunct(donnees$betaplasma)
#Moyenne : 190.191, Ecart-type : 183.216, 25Q : 89.500, MÃ©diane : 140.00, 75Q : 230.500, Min : 0, Max : 1415.00, NA : 0

Sumfunct(donnees$retplasma)
#Moyenne : 603.701, Ecart-type : 208.602, 25Q : 467.00, MÃ©diane : 566.00, 75Q : 717.500, Min : 179.00, Max : 1727.00, NA : 0

#Faîtes l'histogramme de la variable bmi de l'ensemble de l'échantillon et 
#reportez le graphique dans le fichier de report de résultats
hist(donnees$bmi, 
     main="Histogramme de l'IMC des sujets de l'Ã©chantillon", 
     xlab="IMC", 
     xlim=c(10,60),
     ylab="Sujets", 
     las=1,
     col="grey")

#Réalisez l'analyse descriptive des variables catégorielles de la base et reportez 
#les résultats dans le fichier de report de résultats (Attention : pour chaque 
#variable, le pourcentage estimé dans chaque catégorie doit être rapportée à 
#l'effectif disponible (sans données manquantes) et non au total de l'échantillon. 
#Le pourcentage de données manquantes doit lui bien évidemment être rapporté 
#au total de l'échantillon)
test <- table(donnees$sexe, useNA="always") # 0: 41, 1: 273
test
prop.table(test) # 0: 13%, 1: 87%

test <- table(donnees$tabac, useNA="always") # 0: 157, 1: 115, 2: 42
test
prop.table(test) # 0: 50%, 1: 37%, 2: 13%

test <- table(donnees$vitamine, useNA="no") # 0: 116, 1: 76, 2: 104, NA: 18
test
prop.table(test) # 0: 39%, 1: 26%, 2 : 35% | NA : 6%

test <- table(donnees$bmicat, useNA="always") # 0: 4, 1: 159, 2: 89, 3: 62
test
prop.table(test) # 0: 1%, 1: 51%, 2: 28%, 3: 20%

test <- table(donnees$retplasmabin, useNA="always") # 0: 181, 1: 133
test
prop.table(test) # 0: 58%, 1: 42%


#Faîtes le diagramme en bâtons (barplot) de la variable « vitamine »
barplot(table(donnees$vitamine), 
        main= "Consommation de supplÃ©ments vitaminique", 
        xlab="Habitude d'utilisation des supplÃ©ments", 
        ylab="Sujets", 
        ylim=c(0,125), 
        las=1,
        names.arg=c("0: Jamais", "1: Parfois", "2: Souvent"), 
        col=c("white", "grey", "black"),
)

#Réalisez l'analyse statistique de comparaison des variables quantitatives et 
#catégorielles selon la variable binaire « retplasmabin ». Réalisez les tests 
#statistiques appropriés pour comparer la moyenne des variables quantitatives 
#selon le groupe (en dessous ou au-dessus de la moyenne de rétinol plasmatique), 
#catégorielles selon le groupe
head(donnees)
nums <- unlist(lapply(donnees, is.numeric))  
nums
fact <- unlist(lapply(donnees, is.factor))
fact

num1 <- data.frame(donnees[ , nums], donnees$retplasmabin)

dim(num1)
 
newpi <- function(x) {
  t <- pairwise.t.test(x, num1$donnees.retplasmabin,conf.level=.95, p.adj="none",paired=FALSE, alternative="two.sided", pool.sd=F)
  return(t$p.value)
}
#Get p.value quickly
result <- apply(num1[,-13], 2, newpi)
result

#5. RÃ©alisez lâanalyse statistique de comparaison des variables quantitatives et catÃ©gorielles selon la variable binaire Â« retplasmabin Â».
ret1 <- data.frame(donnees[donnees$retplasmabin == "1", ])
ret0 <- data.frame(donnees[donnees$retplasmabin == "0", ])
#Variables quantitatives
Sumfunct(ret0$age)
t.test(x=ret0$age,y=ret1$age, paired=F,alternative="two.sided")
Sumfunct(ret0$bmi)
t.test(x=ret0$bmi,y=ret1$bmi, paired=F,alternative="two.sided")
Sumfunct(ret0$calories)
t.test(x=ret0$calories,y=ret1$calories, paired=F,alternative="two.sided")
Sumfunct(ret0$graisses)
t.test(x=ret0$graisses,y=ret1$graisses, paired=F,alternative="two.sided")
Sumfunct(ret0$fibres)
t.test(x=ret0$fibres,y=ret1$fibres, paired=F,alternative="two.sided")
Sumfunct(ret0$alcool)
t.test(x=ret0$alcool,y=ret1$alcool, paired=F,alternative="two.sided")
Sumfunct(ret0$cholesterol)
t.test(x=ret0$cholesterol,y=ret1$cholesterol, paired=F,alternative="two.sided")
Sumfunct(ret0$betadiet)
t.test(x=ret0$betadiet,y=ret1$betadiet, paired=F,alternative="two.sided")
Sumfunct(ret0$retdiet)
t.test(x=ret0$retdiet,y=ret1$retdiet, paired=F,alternative="two.sided")
Sumfunct(ret0$betaplasma)
t.test(x=ret0$betaplasma,y=ret1$betaplasma, paired=F,alternative="two.sided")
Sumfunct(ret1$age)
Sumfunct(ret1$bmi)
Sumfunct(ret1$calories)
Sumfunct(ret1$graisses)
Sumfunct(ret1$fibres)
Sumfunct(ret1$alcool)
Sumfunct(ret1$cholesterol)
Sumfunct(ret1$betadiet)
Sumfunct(ret1$retdiet)
Sumfunct(ret1$betaplasma)

#Variables qualitatives

ret1 <- data.frame(donnees[donnees$retplasmabin == "1", ])
ret0 <- data.frame(donnees[donnees$retplasmabin == "0", ])
table(ret0$sexe)
round(prop.table(table(ret0$sexe, useNA="always"))*100,1)
table(ret0$tabac)
round(prop.table(table(ret0$tabac, useNA="always"))*100,1)
table(ret0$vitamine)
round(prop.table(table(ret0$vitamine, useNA="always"))*100,1)
SommeNa(ret0$vitamine)
round((SommeNa(ret0$vitamine)/173)*100,1)
table(ret0$bmicat)
round(prop.table(table(ret0$bmicat, useNA="no"))*100,1)
table(ret0$retplasmabin)
round(prop.table(table(ret0$retplasmabin, useNA="no"))*100,1)

table(ret1$sexe)
round(prop.table(table(ret1$sexe, useNA="no"))*100,1)
table(ret1$tabac)
round(prop.table(table(ret1$tabac, useNA="no"))*100,1)
table(ret1$vitamine)
round(prop.table(table(ret1$vitamine, useNA="no"))*100,1)
SommeNa(ret1$vitamine)
round((SommeNa(ret1$vitamine)/123)*100,1)
table(ret1$bmicat)
round(prop.table(table(ret1$bmicat, useNA="no"))*100,1)
table(ret1$retplasmabin)
round(prop.table(table(ret1$retplasmabin, useNA="no"))*100,1)

#Variable Sexe
hommes = c(17,24)
femmes = c(164,109)
tableau = matrix(c(hommes, femmes),2,2,byrow=T)
khi_test = chisq.test(tableau)
khi_test
#Variable Tabac
nf = c(94,63)
af = c(59,56)
fa = c(28,14)
tableau = matrix(c(nf, af, fa),3,2,byrow=T)
khi_test = chisq.test(tableau)
khi_test
#Variable Vitamine
jamais = c(64,52)
parfois = c(47,29)
souvent = c(62,42)
tableau = matrix(c(jamais, parfois, souvent),3,2,byrow=T)
khi_test = chisq.test(tableau)
khi_test
#Variable BMI
bmi0 = c(3,1)
bmi1 = c(95,64)
bmi2 = c(47,42)
bmi3 = c(36,26)
tableau = matrix(c(bmi0, bmi1, bmi2, bmi3),4,2,byrow=T)
khi_test = chisq.test(tableau)
khi_test

#En sélectionnant les variables associées à une forte concentration de rétinol 
#plasmatique (au-dessus de la moyenne) à un seuil alpha = 0,10 (selon les 
#résultats des tests statistiques précédents), fittez le modèle de régression 
#logistique expliquant la variable « retplasmabin » (n'utilisez, si besoin, que la 
#variable BMI catégrielle). Récupérez les coefficients, Odds-Ratio et Intervalles 
#de Confiance des OR et reportez le dans le fichier de report de résultats
regression <-  glm(formula = retplasmabin ~ sexe + age + alcool,data=donnees, family=binomial)
summary(regression)

#Au final, au risque alpha = 5%, quels est(sont) le(s) facteur(s) associés à une 
#concentration de rétinol plasmatique supérieure à la moyenne de l'échantillon ?
#Exprimer verbalement les résultats de la régression logistique obtenue (force 
#de(s) associations retrouvée(s), significativité).
exp(coefficients(regression)) #OR
exp(confint(regression)) #Intervalle de confiances
