getwd()

#Part 1 - NA manipulation

#1
num <- c(-10, 0, 10, 20, NA, 30, 40)
str <- c("M1", "M2", ".", NA, "NA")

num==NA #NA for every values
str==NA #NA for every values
is.na(num) #Only the real NA
is.na(str) #Only the real NA
#is.na doesn't take NA string in account
!is.na(num) #Now we're looking for the reverse
!is.na(str) #Now we're looking for the reverse

#2
str[5] <- NA #Replace the 5th value of str vector by NA
is.na(str)

#3
which(is.na(num)) #id : 5 ; Which rank contains a NA
which(is.na(str)) #id : 4 5 ; Which rank contains a NA

#4
factor(num) #NA isen't in levels part
factor(num, exclude=NULL) #Now it is
table(num) #No NA taking in count
table(num, exclude=NULL) #NA taking but not in level part
summary(num) 
mean(num) #NA needs to be handled
mean(num, na.rm=TRUE) 
sd(num) 
sd(num, na.rm=TRUE)
sum(num) 
sum(num, na.rm=TRUE)

#Part 2 - HTA data

#1 Read csv data
library(readr)
init <- read.csv2("donneesinit.csv", sep=";", dec=",")

#2 Calculate bmi
init$poids <- as.numeric(as.character(init$poids))
str(init)
init$taille <- init$taille/100
init$bmi <- init$poids/(init$taille^2)

init$pas.M0 <- as.numeric(as.character(init$pas.M0))
init$pad.M0 <- as.numeric(as.character(init$pad.M0))

init$hta.M0 <- ifelse(init$pas.M0 >= 140 | init$pad.M0 >= 90, 1, 0)
init$hta.M0

init$pas.M6 <- as.numeric(as.character(init$pas.M6))
init$pad.M6 <- as.numeric(as.character(init$pad.M6))

init$hta.M6 <- ifelse(init$pas.M6 >= 140 | init$pad.M6 >= 90, 1, 0)
init$hta.M6

#3 Delete id
which(init$hta.M0==0) #id : 190

init$age <- as.numeric(as.character(init$age))
which(init$age>80) #id : 260

init <- init[which(init$age<80 & init$hta.M0==1),]
# equal to
# init <- init[-which(init$age>80 & init$hta.M0==0),]

#4 Percentage of missing value
str(init)
init$pas.M3 <- as.numeric(as.character(init$pas.M3))
init$pad.M3 <- as.numeric(as.character(init$pad.M3))

NAFree.M3 <- 100 * sum(!is.na(init$pas.M3))/328
NAFree.M3
NAFree.M3 <- sum(!is.na(init$pad.M3))/328 * 100
NAFree.M3

NAFree.M6 <- 100 * sum(!is.na(init$pas.M6))/328
NAFree.M6
NAFree.M6 <- sum(!is.na(init$pad.M6))/328 * 100
NAFree.M6
  
#5 NA shared between M3 and M6
sum(is.na(init$pas.M3) & !is.na(init$pas.M6)) 
#1

sum(is.na(init$pad.M3) & !is.na(init$pad.M6))

which(is.na(init$pas.M0) | is.na(init$pas.M3))
which(is.na(init$pad.M0) | is.na(init$pad.M3))
#cause <- table(init$premat.cause, exclude = NULL)
#cause

#6 Recode NA by 1,variables that code missing values
init$pas.M6.missing <- ifelse(is.na(init$pas.M6), 1, 0)

init$pad.M6.missing <- ifelse(is.na(init$pad.M6), 1, 0)

init$hta.M6.missing <- ifelse(is.na(init$hta.M6), 1, 0)

#7 Difference between variables
init$diffpasM3 <- (init$pas.M3-init$pas.M0)
init$diffpadM3 <- (init$pad.M3-init$pad.M0)

init$diffpasM6 <- (init$pas.M6-init$pas.M0)
init$diffpadM6 <- (init$pad.M6-init$pad.M0)
 
#8 NA logistics regression by PAS values
summary(glm(init$pas.M6.missing~init$pas.M0, family=binomial))
#p-value : 0.578
#H0 not rejected (alpha:5%)
# MCAR (NA randomly spreaded)

#NA logistics regression by PAS values
summary(glm(init$pad.M6.missing~init$pad.M0, family=binomial))
#p-value : 0.805
#H0 not rejected (alpha:5%)
# MCAR (NA randomly spreaded)

#9 M0, M3 and M6 means
moyPAS.M0 <- mean(init$pas.M0, na.rm=TRUE)
moyPAS.M0 #159.5229

moyPAS.M3 <- mean(init$pas.M3, na.rm=TRUE)
moyPAS.M3 #155.3548

moyPAS.M6 <- mean(init$pas.M6, na.rm=TRUE)
moyPAS.M6 #152.9554

# Confidence interval
IC95.M0inf <- moyPAS.M0 - 1.96 * sqrt(var(init$pas.M0)/328)
IC95.M0inf #158
IC95.M0sup <- moyPAS.M0 + 1.96 * sqrt(var(init$pas.M0)/328)
IC95.M0sup #161

IC95.M3inf <- moyPAS.M3 - 1.96 * sqrt(var(init$pas.M3)/328)
IC95.M3inf #158
IC95.M3sup <- moyPAS.M3 + 1.96 * sqrt(var(init$pas.M3)/328)
IC95.M3sup

IC95.M6inf <- moyPAS.M6 - 1.96 * sqrt(var(init$pas.M6)/328)
IC95.M6inf #158
IC95.M6sup <- moyPAS.M6 + 1.96 * sqrt(var(init$pas.M6)/328)
IC95.M6sup
