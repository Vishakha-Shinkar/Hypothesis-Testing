#Q1
#1. Business problem/Action: Whether to do maintenance of machine or not? or Whether to buy new machine or not.
#2. Randomly selected samples.
#Y= diameter of Cutlets= Continuous
#X= 2 samples = Discrete

data=read.csv("C:/Users/Shinkar/Desktop/vss 2020/Excelr/Assignments/Hypothesis/Cutlets.csv")
data
attach(data)
#Case 1: H0: Data is normal, No action.
#       Ha: Data is not normal, Take action.
ad.test(Unit.A)
# As p-value for Unit.A= 0.2866 > 0.05, Data of Unit.A is normal.
ad.test(Unit.B)
# As p-value for Unit.B= 0.6869 > 0.05, Data of Unit.B is normal.
#Case 2: H0: Var of Unit.A= var of Unit.B
#        Ha: var of unit.A != var of Unit.B
var.test(Unit.A,Unit.B)
#p-value= 0.3136 > 0.05, We fail to reject H0 i.e. Var of Unit.A = Var of Unit.B
# We use 2 sample t-test for equal variance here.
#Case 3: H0: There is significant difference in the diameter of the cutlet between two units i.e.mu1=mu2
#        Ha: There is no significant difference in the diameter of the cutlet between two units i.e. mu1 != mu2 

t.test(Unit.A,Unit.B,alternative = "two.sided",conf.level = 0.95, var.equal = T)
#p-value = 0.4722 > 0.05, We fail to reject H0.
#Hence, there is significant difference in the diameter of the cutlet between two units.

#Q2
# Y= TAT= Continuous
# X= 4 laboratories= Discrete
data2= read.csv("C:/Users/Shinkar/Desktop/vss 2020/Excelr/Assignments/Hypothesis/LabTAT.csv")
data2
attach(data2)
#Case1: H0: Data is normal, No action.
#       Ha: Data is not normal, Take action.
Stacked_Data2<-stack(data2)
Stacked_Data2
ad.test(Stacked_Data2$values)
#p-value= 0.05072 > 0.05, We fail to reject H0 i.e. Data is normal.
#var test
#H0: Variances are equal.
#Ha: Variances are not equal.
leveneTest(values~ ind, data = Stacked_Data2)
#p-value=0.05161 > 0.05. Hence, variances are equal.
# ANOVA test:
#H0: There is difference in the average Turn Around Time (TAT) of reports of the laboratories.
#Ha: There is no difference in the average Turn Around Time (TAT) of reports of the laboratories.
Anova_results <- aov(values~ind,data = Stacked_Data2)
summary(Anova_results)
#p-value<0.05. We reject H0. Hence, there is no difference in the average Turn Around Time (TAT) of reports of the laboratories.

#Q3
#H0: All proportions are equal.
#Ha: Not all proportions are equal.
data3=read.csv("C:/Users/Shinkar/Desktop/vss 2020/Excelr/Assignments/Hypothesis/BuyerRatio.csv")
data3
attach(data3)
D=c('East','West','North','South')
M=c(50,142,131,70)
F=c(435,1523,1356,750)
d=data.frame(D,M,F)
d
chisq.test(d$M,d$F)
#p-value > 0.05. Hence, we fail to reject H0. All proportions are equal.


#Q4
#data4=read.csv("C:/Users/Shinkar/Desktop/vss 2020/Excelr/Assignments/Hypothesis/Costomer+OrderForm.csv")
#data4
#attach(data4)
#d=data.frame(Phillippines,Indonesia,Malta,India)
#d
#levels(Phillippines) <- c(1,0)
#d
#levels(Indonesia) <- c(1,0)
#d
#levels(Malta) <- c(1,0)
#d
#levels(India) <- c(1,0)
#d
#d=data.frame(Phillippines,Indonesia,Malta,India)
#d
#Stacked_Data4<-stack(d)
#Stacked_Data4
#typeof(d)


#Q5
#H0: Proportion of females in weekdays = proportion of males in weekdays
#Ha: Proportion of females in weekdays != proportion of males in weekdays
fantaloons=read.csv("C:/Users/Shinkar/Desktop/vss 2020/Excelr/Assignments/Hypothesis/Faltoons.csv")
fantaloons
attach(fantaloons)
table(Weekdays)
table(Weekend) 
prop.test(x=c(287,113),n=c(400,400),conf.level = 0.95,correct = TRUE,alternative = "two.sided")
#p-value < 0.05. Hence, We reject H0.Proportion of females in weekdays != proportion of males in weekdays.
#H0: Proportion of females < Proportion of males in weekdays
#Ha: Proportion of females > Proportion of males in weekdays
prop.test(x=c(287,113),n=c(400,400),conf.level = 0.95,correct = TRUE,alternative = "greater")
#p-value < 0.05. Hence, We reject H0. Proportion of females > Proportion of males in weekdays.
#H0: Proportion of females in weekend = proportion of males in weekend
#Ha: Proportion of females in weekend != proportion of males in weekend
prop.test(x=c(233,167),n=c(400,400),conf.level = 0.95,correct = TRUE,alternative = "two.sided")
#p-value < 0.05. Hence, We reject H0.Proportion of females in weekend != proportion of males in weekend.
#H0: Proportion of females < Proportion of males in weekend
#Ha: Proportion of females > Proportion of males in weekend
prop.test(x=c(233,167),n=c(400,400),conf.level = 0.95,correct = TRUE,alternative = "greater")
# p-value < 0.05. Hence, We reject H0. Proportion of females > Proportion of males in weekend.