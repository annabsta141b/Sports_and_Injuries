

######################### part 1 blood #########################
dat1 = read.csv("~/LocalDocuments/GitHub/Sports_and_Injuries/data/btype.csv")
str(dat1)
########### graphs ###############


library(ggplot2)
library(viridis)
ggplot(data=dat1,aes(x=dat1$group,fill=dat1$group))+
  geom_bar()+xlab("Blood Type")+
  ylab("Number of Subjects")+
  geom_text(stat='count', aes(label=..count..), vjust=-0)+
  ggtitle("Sample Blood Types")+
  scale_fill_viridis_d(begin=.25, name = "Blood Types")+
  theme_bw()+
  ggsave("blood summary.png", height = 4,width = 5)

############### table with proportions ###############
table1= table(dat1)
n1 = sum(table1)
prop1 = table1/n1
prop1

############### HT & CI'S ##########################

prop.mu = c(0.25,0.11,0.2,0.44)
expected = n1 * prop.mu
names(expected) = names(table1)
expected

test.1 = chisq.test(table1,p=prop.mu)
test.1

chi.breakdown = (table1 - expected)^2/expected  
chi.breakdown

y1 = table1[1]
y2 = table1[2]
y3 = table1[3]
y4 = table1[4]
n = sum(table1)
g = 4
alpha = 0.05
CI1 = prop.test(y1+2,n+4,conf.level = 1-alpha/(g),correct = FALSE)$conf.int
CI2 = prop.test(y2+2,n+4,conf.level = 1-alpha/(g),correct = FALSE)$conf.int
CI3 = prop.test(y3+2,n+4,conf.level = 1-alpha/(g),correct = FALSE)$conf.int
CI4 = prop.test(y4+2,n+4,conf.level = 1-alpha/(g),correct = FALSE)$conf.int
results = rbind(CI1,CI2,CI3,CI4)
colnames(results) = c("Lower","Upper")
rownames(results) = names(table1)
results

########## part 2 sports and injury ########################
dat2 = read.csv("~/LocalDocuments/GitHub/Sports_and_Injuries/data/compare.csv")
str(dat2)
table2 = table(dat2$sport,dat2$injury)

#######	Injury Incident by Sport Graph ##############
library(reshape2)
library(ggplot2)
table.long = melt(table2,vars="sport")
table.long
ourplot = ggplot(data=table.long,aes(x=Var2,y=value,fill=factor(Var1)))+geom_bar(stat="identity",position="dodge")+
  theme_bw()+scale_fill_viridis_d(begin=.25)+xlab("Type of Injury")+ylab("Number of Study Participants")+
  ggtitle("Injury Incidence by Sport")
ourplot$labels$fill= "Sport"
ourplot

##### residuals table ###
test.2$expected
test.2$residuals
x=c(-1.8732,-1.1520,1.6924,0.5987,1.9278,1.1856,-1.7417,-0.6161)
x=abs(x)
probs = 1-pnorm(x)
probs

######################## HT and Cis ##################   

test.2 = chisq.test(table2)
test.2
test.2$expected
test.2$residuals


find.odd.CI = function(y1,n1,y2,n2, conf.level = x){
  odds1 = (y1/n1)/((n1-y1)/n1)
  odds2 = (y2/n2)/((n2-y2)/n2)
  OR = odds1/odds2
  Za = qnorm((1-conf.level)/2 , lower.tail = FALSE)
  ln.CI = log(OR) + c(-1,1)*Za*sqrt(1/y1 + 1/(n1-y1) + 1/y2 + 1/(n2-y2))
  CI = exp(ln.CI)
  return(CI)
}
#
table2
rowSums(table2)
colSums(table2)
#Four CI's:
g=4
alpha = (1-(.05/g))  #don't use 2g, the function is already correcting for tails.

n1 = sum(table2[1,]) #summing the first row to get the total number of martial artists
n2 = sum(table2[2,]) #summing the second row to get total number of soccer players

#Broken Bone
bb1 = table2[1,1] #number of broken bones for martial artists
bb2 = table2[2,1] #number of broken bones for soccer players

find.odd.CI(bb1,n1,bb2,n2, alpha) #does not include 1

#Concussions
c1 = table2[1,2] #number of concussions for martial artists
c2 = table2[2,2] #number of concussions for soccer players

find.odd.CI(c1,n1,c2,n2, alpha)  #includes 1

#Muscular
m1 = table2[1,3] #number of muscular injuries for martial artists
m2 = table2[2,3] #number of muscular injuries for soccer players

find.odd.CI(m1,n1,m2,n2, alpha)  #does not include 1

#None
na1 = table2[1,4] #number of no injuries for martial artists
na2 = table2[2,4] #number of no injuries for soccer players

find.odd.CI(na1,n1,na2,n2, alpha)  #includes 1





 