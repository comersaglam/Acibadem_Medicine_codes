datamain = Child_marriage
summary(datamain)
attach(datamain)
class(Language)
cont = as.factor(Continental)
lang = as.factor(Language)
levels(lang)
mean(female_15[lang=="turkish"])
mean(female_15[lang=="spanish"])
length(cont[cont=="europe"])


#question 1


datamain3 = Child_marriage_total
attach(datamain3)
detach(datamain2)
detach(datamain)
boxplot(percentage~continental_a)##########
anova_test2 = (aov(percentage~continental_a))
#h0= mean difference is not important
summary(anova_test2)
attributes(anova_test2)
Fvalue_anova2 = summary(anova_test2)[[1]][[1,"F value"]]
levels(cont)
critic_value = qf(0.95,5,120) # if F>critical value, then reject H0
func.anova2 = (
  if(Fvalue_anova2>critic_value){
    print("mean difference is remarkable")
  }  else { 
    print("they are almost same")})
func.anova2


#question 2


datamain2 = na.omit(datamain)
detach(datamain)
attach(datamain2)
detach(datamain3)
length(male_18)
length(female_18)
plot(female_18,male_18) ###########
regression = lm(female_18 ~ male_18)
#H0= two variables are not related to each other alpha= 0,01
summary(regression)#bu tablodan regression pvalue<0,01 çýkmasý normal mi?
attributes(regression)
#result_regression = (regression = lm(female_18 ~ male_18))$p.value#???bunu nasýl çýkartacaðýz???
result_regression <- function (regression) {
  if (class(regression) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(regression)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p) }

func.regression =(
  if(result_regression(regression) <= 0.01){
    print("variables are related")
  }  else { 
    print("variables are independent")})

func.regression


# guestion 3


detach(datamain2)
attach(datamain3)
detach(datamain)
boxplot(percentage ~ language_a) ############
anova_test = (aov(percentage ~language_a))
summary(anova_test)
attributes(anova_test)

#alpha=0,05. h0= mean difference is not important
result_anova = summary(anova_test)[[1]][[1,"Pr(>F)"]]
func.anova =(
  if(result_anova <= 0.05){
    print("mean difference is remarkable")
  }  else { 
    print("they are almost same")})
tuk = TukeyHSD(anova_test)
dimnames(tuk[[1]])
rownames(tuk[[1]])
tuk
plot(tuk)############
print("only 1 comparison out of 21 has a differantual mean difference(other-french)")


#question 4


detach(datamain3)
detach(datamain2)
attach(datamain)
length(year_f)
year_f1 = sort(year_f)
median_f = (year_f1[63] + year_f1[64])/2

year_mr = na.omit(year_m)
length(year_mr)
year_m1 = sort(year_mr)
median_m = (year_m1[47])
male_18_r = na.omit(male_18)

a1 = (female_15[year_f < median_f])
a2 = (female_18[year_f < median_f])
a3 = (male_18_r[year_m1 < median_m])
a  = c(a1,a2,a3)

b1 = (female_15[year_f > median_f])
b2 = (female_18[year_f > median_f])
b3 = (male_18_r[year_m1 > median_m])
b  = c(b1,b2,b3)

early_year = a
late_year = b
improvement = mean(early_year)-mean(late_year)# >0 , so there is an improvement in years                               
#H0= there is no important difference alpha=0.05
result_ttest = (t.test(early_year,late_year))$p.value
attributes(t.test(early_year,late_year))
func.ttest = (
  if(improvement>0){
    if(result_ttest <= 0.05){
      print("there is a remarkable improvement")
    }  else { 
      print("change could be by chance")} 
  } else {
    print("there is no improvement")
  })

func.ttest

