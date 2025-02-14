library(readxl)
library(dplyr)
cmps_getat <- read_excel("C:/Users/sagla/Downloads/cmps_getat.xlsx")
View(cmps_getat)

attach(cmps_getat)
re_cmps_getat = cmps_getat[,c(-10,-12,-27,-28,-30)]

################################################
#question 1 ya� vs destek
# H0= there is no relation between age and participants support level. linear regression is used and no relation is found
yasdestekreg = lm( age ~destektoplam)
summary(yasdestekreg)

result_regression <- function (regression) {
  if (class(regression) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(regression)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p) }

func.regression =(
  if(result_regression(yasdestekreg) <= 0.05){
    print("variables are related")
  }  else { 
    print("variables are independent")})
k = result_regression(yasdestekreg)
func.regression
plot(age,destektoplam,
     xlab='age', ylab='destektoplam')
##############################################
#question 2 cinsiyet vs destek
# H0= both genders have similar support level/ two sample t test is used and no remarkable difference is found
female = destektoplam[gender == "Kad�n/Female"]
male =   destektoplam[gender == "Erkek/male"]
result_ttest = (t.test(female,male))$p.value
t.test(female,male)
func.ttest =(
  if(result_ttest <= 0.05){
    print("there is a remarkable difference")
  }  else { 
    print("difference could be by chance")} 
)
func.ttest

##################################################
#question 3 din vs destek
# H0= muslims and non muslims have similar support level/ two sample t test is used and no remarkable difference is found
muslim = destektoplam[religion == "islam"]
religion =   destektoplam[religion != "islam"]
result_ttestq3 = (t.test(muslim,religion))$p.value
t.test(muslim,religion)
func.ttestq3 =(
  if(result_ttestq3 <= 0.05){
    print("there is a remarkable difference")
  }  else { 
    print("difference could be by chance")} 
)
func.ttestq3

##################################################
#question 4 gelir vs destek (anova)
# H0= three groups that indicates different income levels have similar support level. ANOVA is used and no remarkable difference is found
anovaq4 = aov(destektoplam ~ income)
value_anovaq4 = summary(anovaq4)[[1]][[1,"F value"]]
critic_valueq4
critic_valueq4 = qf(0.95,length(destektoplam)-1,2)
func.anovaq4 = (
  if(value_anovaq4>critic_valueq4){
    print("mean difference is remarkable")
  }  else { 
    print("they are almost same")})
func.anovaq4

####################################################
#Q5 destek vs kullan�m
#H0= participants' support level and usage levels are irrelevant/ linear regression is used and no relation is found
destekkullan�mreg = lm( destektoplam ~ kullan�mtoplam)
summary(destekkullan�mreg)
result_regressionq5 <- function (regression) {
  if (class(regression) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(regression)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p) }

func.regressionq5 =(
  if(result_regressionq5(destekkullan�mreg) <= 0.05){
    print("variables are related")
  }  else { 
    print("variables are independent")})

func.regressionq5
plot(destektoplam,kullan�mtoplam,
     xlab='destektoplam', ylab='kullan�mtoplam')


#################################################
#Q6 bran� se�imi vs destek
# H0= participants' thoughts about branch choices in medicine makes no difference in their support level/ ANOVA is used and no difference is found
anovaq6 = aov(destektoplam ~ branch)
value_anovaq6 = summary(anovaq6)[[1]][[1,"F value"]]
critic_valueq6 = qf(0.95,length(destektoplam)-1,2)
critic_valueq6 
func.anovaq6 = (
  if(value_anovaq6>critic_valueq6){
    print("mean difference is remarkable")
  }  else { 
    print("they are almost same")})
func.anovaq6
####################################################
#Q7 ba��ml�l�k vs kullanma
# H0= participants' encountering hard health conditions have no effect on their use/ linear regression is used and no relation is found
hastakullanmareg = lm( hastaderece ~ kullan�mtoplam)
summary(hastakullanmareg)
result_regressionq7 <- function (regression) {
  if (class(regression) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(regression)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p) }

func.regressionq7 =(
  if(result_regressionq7(hastakullanmareg) <= 0.05){
    print("variables are related")
  }  else { 
    print("variables are independent")})

func.regressionq7
plot(hastaderece,kullan�mtoplam,
     xlab='hastaderece', ylab='kullan�mtoplam')

####################################################
#Q8 s�nnet m�sl�man
# what is the percentile of the muslims that thinks that methods are a part of religion/ accept as sunnah
#H0= people who think methods are part of religion and who has counter idea has similar use of methods

#step1, muslim to sunnah
freqq8 = (length(age[cmps_getat[,5] == "islam"& cmps_getat[,22]== "1x"])/length(age[cmps_getat[,5] == "islam"]))
#step2, if there is an effect
sunnah = kullan�mtoplam[cmps_getat[,22] == "1x"]
idontcare = kullan�mtoplam[cmps_getat[,22] == "0x"]
result_ttestq8 = (t.test(sunnah,idontcare))$p.value
t.test(sunnah,idontcare)
func.ttestq8 =(
  if(result_ttestq8 <= 0.05){
    print("there is a remarkable difference")
  }  else { 
    print("difference could be by chance")} 
)
func.ttestq8

###########################################
#Q9ya� kullan�m
# H0= there is no relation between age and participants usage levels. linear regression is used and no relation is found
yaskullan�mreg = lm( age ~kullan�mtoplam)
summary(yaskullan�mreg)
result_regressionq9 <- function (regression) {
  if (class(regression) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(regression)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p) }

func.regressionq9 =(
  if(result_regression(yaskullan�mreg) <= 0.05){
    print("variables are related")
  }  else { 
    print("variables are independent")})

func.regressionq9
plot(age, kullan�mtoplam,
     xlab='age', ylab='kullan�mtoplam')

################################################
#Q10 Cinsiyet vs kullan�m
#2.	H0= both genders have similar usage levels/ two sample t test is used and no remarkable difference is found
kfemale = kullan�mtoplam[gender == "Kad�n/Female"]
kmale =   kullan�mtoplam[gender == "Erkek/male"]
result_ttestq10 = (t.test(kfemale,kmale))$p.value
t.test(kfemale,kmale)
func.ttestq10 =(
  if(result_ttestq10 <= 0.05){
    print("there is a remarkable difference")
  }  else { 
    print("difference could be by chance")} 
)
func.ttestq10

#######################################################
#Q11 Din vs kullan�m
#11.	H0= muslims and non muslims have similar usage levels/ two sample t test is used and no remarkable difference is found
kmuslim = kullan�mtoplam[cmps_getat[,5] == "islam"]
kreligion = kullan�mtoplam[cmps_getat[,5] != "islam"]
result_ttest = (t.test(kmuslim,kreligion))$p.value
t.test(kmuslim,kreligion)
func.ttestq11 =(
  if(result_ttest <= 0.05){
    print("there is a remarkable difference")
  }  else { 
    print("difference could be by chance")} 
)
func.ttestq11

#######################################################
#Q12 gelir vs kullan�m
#H0= three groups that indicates different income levels have similar usage levels. ANOVA is used and no remarkable difference is found

anovaq12 = aov(kullan�mtoplam ~ income)
value_anovaq12 = summary(anovaq12)[[1]][[1,"F value"]]
critic_valueq12 = qf(0.95,length(kullan�mtoplam),2)
critic_valueq12
func.anovaq12 = (
  if(value_anovaq12>critic_valueq12){
    print("mean difference is remarkable")
  }  else { 
    print("they are almost same")})
func.anovaq12

################################################Pie chart
a1= destektoplam[destektoplam==1]
a2= destektoplam[destektoplam==2]
a3= destektoplam[destektoplam==3]
a4= destektoplam[destektoplam==4]
a5= destektoplam[destektoplam==5]
a6= destektoplam[destektoplam==6]
a7= destektoplam[destektoplam==7]
a8= destektoplam[destektoplam==8]
a9= destektoplam[destektoplam==9]
a10= destektoplam[destektoplam==10]
a11= destektoplam[destektoplam==11]
a1+a2+a3+a4+a5+a6+a7+a8+a9+a10+a11

perc = function(x){
  y = length(x)
  z = paste(x[1],"=%",round(y/60, digits = 2))
  return(z)
}  
  
pie(c(length(a1),length(a2),length(a3),
      length(a4),length(a5),length(a6),
      length(a7),length(a8),length(a9),
      length(a10),length(a11)),
    c(perc(a1),perc(a2),perc(a3),
      perc(a4),perc(a5),perc(a6),perc(a7),
      perc(a8),perc(a9),perc(a10),perc(a11)),main = "destek puan� y�zdeleri")

perc(a3)
length(a2)
##########################################################################
?qf
