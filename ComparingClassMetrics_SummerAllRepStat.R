###################################################################################################################################################
## Su All vs Repstat with betweenness###
##################################################################################################################################################
##Initial values to load if not already done for previous analyses
Group<-"herd"
library(tnet)
P<-5000 #Number of permutations
Summer2015_All <- read.csv(file="Summer2015_All.csv", header=TRUE, sep=",")
net<-as.tnet(Summer2015_All, type="weighted one-mode tnet")
AttributeSummer2015Metrics <- read.csv(file="AttributeSummer2015Metrics.csv", header=TRUE, sep=",")
attribute<-AttributeSummer2015Metrics

##load dunn.test and kruskall.wallis package
library(dunn.test)

#create grouping vector
groupvec<-attribute$RepStatNum
metricvec<-attribute$Su15_All_between

#Run permutations with weight permutation
#calculate observed test stat 
H_obs<-dunn.test(metricvec,groupvec,table=FALSE)$chi2
obspairs<-dunn.test(metricvec,groupvec,table=FALSE)$Z
paircomp<-dunn.test(metricvec,groupvec,table=FALSE)$comparisons
observedpairs<-data.frame(paircomp,obspairs)

#Create output matrix of pairwise comparisons
#c is number of pairs being compared as gathered from observedpairs file
c<-21
pairsmatrix<-matrix(0,nrow=c,ncol=P)
rownames(pairsmatrix)<-paircomp
random2<-rep(1:P)

#Runn Dunn's test for random networks
for (i in c(1:P)) {
  rand_df<-attribute[c("NumberID","ID")]
  node<-rand_df$NumberID
  rand_df<-cbind(rand_df,node)
  randnet<-rg_reshuffling_w(net,option="weights")
  metricrand<-betweenness_w(randnet, alpha=1.5)
  metricrand2<-data.frame(metricrand)
  rand_df<-merge(rand_df,metricrand2,by="node",all=TRUE)
  rand_df[is.na(rand_df)]<-0
  Randmetricvec<-metricrand2$betweenness
  random2[i]<-dunn.test(Randmetricvec,groupvec,table=FALSE)$chi2
  pairsmatrix[,i]<-dunn.test(Randmetricvec,groupvec,table=FALSE)$Z}

creat.random.testvalues <- function(randomNetwork) {
    statements
    return(something)
}

NAomit<-sum(is.nan(random2))
random3<-na.omit(random2)
pairsmatrix2<-t(na.omit(t(pairsmatrix)))
upperp<-sum(H_obs<random3)/P
lowerp<-sum(H_obs>random3)/P
absp<-sum(abs(H_obs)<abs(random3))/P
randmean<-mean(random3)
randmedian<-median(random3)
randsd<-sd(random3)

pairpvalue<-rep(1:c)
for(i in c(1:c)) {
  pairpvalue[i]<-sum(abs(obspairs[i])<abs(pairsmatrix2[i,]))/P}
pairmean<-rep(1:c)
for(i in c(1:c)) {
  pairmean[i]<-mean(pairsmatrix2[i,])}
pairmedian<-rep(1:c)
for(i in c(1:c)) {
  pairmedian[i]<-median(pairsmatrix2[i,])}
pairsd<-rep(1:c)
for(i in c(1:c)) {
  pairsd[i]<-sd(pairsmatrix2[i,])}

#creating main test stat output file
Repstat1<-attribute[which(attribute$RepStatNum==1),]
Repstat1_median<-median(Repstat1$Su15_All_between)
Repstat1_mean<-mean(Repstat1$Su15_All_between)
Repstat1_sd<-sd(Repstat1$Su15_All_between)
Repstat1_cv<-(Repstat1_sd/Repstat1_mean)
Repstat2<-attribute[which(attribute$RepStatNum==2),]
Repstat2_median<-median(Repstat2$Su15_All_between)
Repstat2_mean<-mean(Repstat2$Su15_All_between)
Repstat2_sd<-sd(Repstat2$Su15_All_between)
Repstat2_cv<-(Repstat2_sd/Repstat2_mean)
Repstat3<-attribute[which(attribute$RepStatNum==3),]
Repstat3_median<-median(Repstat3$Su15_All_between)
Repstat3_mean<-mean(Repstat3$Su15_All_between)
Repstat3_sd<-sd(Repstat3$Su15_All_between)
Repstat3_cv<-(Repstat3_sd/Repstat3_mean)
Repstat4<-attribute[which(attribute$RepStatNum==4),]
Repstat4_median<-median(Repstat4$Su15_All_between)
Repstat4_mean<-mean(Repstat4$Su15_All_between)
Repstat4_sd<-sd(Repstat4$Su15_All_between)
Repstat4_cv<-(Repstat4_sd/Repstat4_mean)
Repstat5<-attribute[which(attribute$RepStatNum==5),]
Repstat5_median<-median(Repstat5$Su15_All_between)
Repstat5_mean<-mean(Repstat5$Su15_All_between)
Repstat5_sd<-sd(Repstat5$Su15_All_between)
Repstat5_cv<-(Repstat5_sd/Repstat5_mean)
Repstat6<-attribute[which(attribute$RepStatNum==6),]
Repstat6_median<-median(Repstat6$Su15_All_between)
Repstat6_mean<-mean(Repstat6$Su15_All_between)
Repstat6_sd<-sd(Repstat6$Su15_All_between)
Repstat6_cv<-(Repstat6_sd/Repstat6_mean)
Repstat7<-attribute[which(attribute$RepStatNum==7),]
Repstat7_median<-median(Repstat7$Su15_All_between)
Repstat7_mean<-mean(Repstat7$Su15_All_between)
Repstat7_sd<-sd(Repstat7$Su15_All_between)
Repstat7_cv<-(Repstat7_sd/Repstat7_mean)
Result_9b<-data.frame(group=Group,network="SuAll",attribute="RepStat",metric="betweenness",random="weights",teststat=H_obs,lowerp=lowerp,upperp=upperp,absp=absp,
                      NaNomit=NAomit,testrandmean=randmean,testrandmedian=randmedian,testrandsd=randsd,
                      Class1="stallion_1",Class1median=Repstat1_median,Class1mean=Repstat1_mean,Class1sd=Repstat1_sd,Class1cv=Repstat1_cv,
                      Class2="repmare_2",Class2median=Repstat2_median,Class2mean=Repstat2_mean,Class2sd=Repstat2_sd,Class2cv=Repstat2_cv,
                      Class3="nonrepmare_3",Class3median=Repstat3_median,Class3mean=Repstat3_mean,Class3sd=Repstat3_sd,Class3cv=Repstat3_cv,
                      Class4="colt_4",Class4median=Repstat4_median,Class4mean=Repstat4_mean,Class4sd=Repstat4_sd,Class4cv=Repstat4_cv,
                      Class5="filly_5",Class5median=Repstat5_median,Class5mean=Repstat5_mean,Class5sd=Repstat5_sd,Class5cv=Repstat5_cv,
                      Class6="foal_colt_6",Class6median=Repstat6_median,Class6mean=Repstat6_mean,Class6sd=Repstat6_sd,Class6cv=Repstat6_cv,
                      Class7="foal_filly_7",Class7median=Repstat7_median,Class7mean=Repstat7_mean,Class7sd=Repstat7_sd,Class7cv=Repstat7_cv)
write.csv(Result_9b,file="Result_9b.csv")

#Creating pairwise output file
observedpairs$pairpvalue<-pairpvalue
observedpairs$teststatmean<-pairmean
observedpairs$teststatmedian<-pairmedian
observedpairs$teststatsd<-pairsd
permmethod<-rep("weights",times=c)
obsnetwork<-rep("SuAll",times=c)
obsattribute<-rep("RepStat",times=c)
obsmetric<-rep("betweenness",times=c)
observedpairs_result9b<-cbind(obsnetwork,obsattribute,obsmetric,permmethod,observedpairs)
observedpairs_result9b
write.table(observedpairs_result9b,"observedpairs_result9b.txt")

#Run permutations with link permutation
#calculate observed test stat 
H_obs<-dunn.test(metricvec,groupvec,table=FALSE)$chi2
obspairs<-dunn.test(metricvec,groupvec,table=FALSE)$Z
paircomp<-dunn.test(metricvec,groupvec,table=FALSE)$comparisons
observedpairs<-data.frame(paircomp,obspairs)

#Create output matrix of pairwise comparisons
#c is number of pairs being compared as gathered from observedpairs file
c<-21
pairsmatrix<-matrix(0,nrow=c,ncol=P)
rownames(pairsmatrix)<-paircomp
random2<-rep(1:P)

#Runn Dunn's test for random networks
for (i in c(1:P)) {
  rand_df<-attribute[c("NumberID","ID")]
  node<-rand_df$NumberID
  rand_df<-cbind(rand_df,node)
  randnet<-rg_reshuffling_w(net,option="links")
  metricrand<-betweenness_w(randnet, alpha=1.5)
  metricrand2<-data.frame(metricrand)
  rand_df<-merge(rand_df,metricrand2,by="node",all=TRUE)
  rand_df[is.na(rand_df)]<-0
  Randmetricvec<-metricrand2$betweenness
  random2[i]<-dunn.test(Randmetricvec,groupvec,table=FALSE)$chi2
  pairsmatrix[,i]<-dunn.test(Randmetricvec,groupvec,table=FALSE)$Z}

NAomit<-sum(is.nan(random2))
random3<-na.omit(random2)
pairsmatrix2<-t(na.omit(t(pairsmatrix)))
upperp<-sum(H_obs<random3)/P
lowerp<-sum(H_obs>random3)/P
absp<-sum(abs(H_obs)<abs(random3))/P
randmean<-mean(random3)
randmedian<-median(random3)
randsd<-sd(random3)

pairpvalue<-rep(1:c)
for(i in c(1:c)) {
  pairpvalue[i]<-sum(abs(obspairs[i])<abs(pairsmatrix2[i,]))/P}
pairmean<-rep(1:c)
for(i in c(1:c)) {
  pairmean[i]<-mean(pairsmatrix2[i,])}
pairmedian<-rep(1:c)
for(i in c(1:c)) {
  pairmedian[i]<-median(pairsmatrix2[i,])}
pairsd<-rep(1:c)
for(i in c(1:c)) {
  pairsd[i]<-sd(pairsmatrix2[i,])}

Result10<-data.frame(group=Group,network="SuAll",attribute="RepStat",metric="betweenness",random="links",teststat=H_obs,lowerp=lowerp,upperp=upperp,absp=absp,
                     NaNomit=NAomit,testrandmean=randmean,testrandmedian=randmedian,testrandsd=randsd,
                     Class1="stallion_1",Class1median=Repstat1_median,Class1mean=Repstat1_mean,Class1sd=Repstat1_sd,Class1cv=Repstat1_cv,
                     Class2="repmare_2",Class2median=Repstat2_median,Class2mean=Repstat2_mean,Class2sd=Repstat2_sd,Class2cv=Repstat2_cv,
                     Class3="nonrepmare_3",Class3median=Repstat3_median,Class3mean=Repstat3_mean,Class3sd=Repstat3_sd,Class3cv=Repstat3_cv,
                     Class4="colt_4",Class4median=Repstat4_median,Class4mean=Repstat4_mean,Class4sd=Repstat4_sd,Class4cv=Repstat4_cv,
                     Class5="filly_5",Class5median=Repstat5_median,Class5mean=Repstat5_mean,Class5sd=Repstat5_sd,Class5cv=Repstat5_cv,
                     Class6="foal_colt_6",Class6median=Repstat6_median,Class6mean=Repstat6_mean,Class6sd=Repstat6_sd,Class6cv=Repstat6_cv,
                     Class7="foal_filly_7",Class7median=Repstat7_median,Class7mean=Repstat7_mean,Class7sd=Repstat7_sd,Class7cv=Repstat7_cv)
write.csv(Result10,file="Result10.csv")

#Creating pairwise output file
observedpairs$pairpvalue<-pairpvalue
observedpairs$teststatmean<-pairmean
observedpairs$teststatmedian<-pairmedian
observedpairs$teststatsd<-pairsd
permmethod<-rep("links",times=c)
obsnetwork<-rep("SuAll",times=c)
obsattribute<-rep("RepStat",times=c)
obsmetric<-rep("betweenness",times=c)
observedpairs_result10<-cbind(obsnetwork,obsattribute,obsmetric,permmethod,observedpairs)
observedpairs_result10
write.table(observedpairs_result10,"observedpairs_result10.txt")

#Run permutations with node permutation
#calculate observed test stat 
H_obs<-dunn.test(metricvec,groupvec,table=FALSE)$chi2
obspairs<-dunn.test(metricvec,groupvec,table=FALSE)$Z
paircomp<-dunn.test(metricvec,groupvec,table=FALSE)$comparisons
observedpairs<-data.frame(paircomp,obspairs)

#Create output matrix of pairwise comparisons
#c is number of pairs being compared as gathered from observedpairs file
c<-21
pairsmatrix<-matrix(0,nrow=c,ncol=P)
rownames(pairsmatrix)<-paircomp
random2<-rep(1:P)

#create sna matrix for randomization
library(sna)
snanet <- as.matrix(net)
N <- max(c(snanet[,"i"],snanet[,"j"]))
g <- matrix(data=0, nrow=N, ncol=N)
g[snanet[,c("i","j")]] <- snanet[,"w"]
snanet2<- as.sociomatrix.sna(g)

#Runn Dunn's test for random networks
for (i in c(1:P)) {
  rand_df<-attribute[c("NumberID","ID")]
  node<-rand_df$NumberID
  rand_df<-cbind(rand_df,node)
  randnet<-rmperm(snanet2)
  metricrand<-betweenness_w(randnet, alpha=1.5)
  metricrand2<-data.frame(metricrand)
  rand_df<-merge(rand_df,metricrand2,by="node",all=TRUE)
  rand_df[is.na(rand_df)]<-0
  Randmetricvec<-metricrand2$betweenness
  random2[i]<-dunn.test(Randmetricvec,groupvec,table=FALSE)$chi2
  pairsmatrix[,i]<-dunn.test(Randmetricvec,groupvec,table=FALSE)$Z}

NAomit<-sum(is.nan(random2))
random3<-na.omit(random2)
pairsmatrix2<-t(na.omit(t(pairsmatrix)))
upperp<-sum(H_obs<random3)/P
lowerp<-sum(H_obs>random3)/P
absp<-sum(abs(H_obs)<abs(random3))/P
randmean<-mean(random3)
randmedian<-median(random3)
randsd<-sd(random3)

pairpvalue<-rep(1:c)
for(i in c(1:c)) {
  pairpvalue[i]<-sum(abs(obspairs[i])<abs(pairsmatrix2[i,]))/P}
pairmean<-rep(1:c)
for(i in c(1:c)) {
  pairmean[i]<-mean(pairsmatrix2[i,])}
pairmedian<-rep(1:c)
for(i in c(1:c)) {
  pairmedian[i]<-median(pairsmatrix2[i,])}
pairsd<-rep(1:c)
for(i in c(1:c)) {
  pairsd[i]<-sd(pairsmatrix2[i,])}

detach("package:sna")

Result11<-data.frame(group=Group,network="SuAll",attribute="RepStat",metric="betweenness",random="nodeperm",teststat=H_obs,lowerp=lowerp,upperp=upperp,absp=absp,
                     NaNomit=NAomit,testrandmean=randmean,testrandmedian=randmedian,testrandsd=randsd,
                     Class1="stallion_1",Class1median=Repstat1_median,Class1mean=Repstat1_mean,Class1sd=Repstat1_sd,Class1cv=Repstat1_cv,
                     Class2="repmare_2",Class2median=Repstat2_median,Class2mean=Repstat2_mean,Class2sd=Repstat2_sd,Class2cv=Repstat2_cv,
                     Class3="nonrepmare_3",Class3median=Repstat3_median,Class3mean=Repstat3_mean,Class3sd=Repstat3_sd,Class3cv=Repstat3_cv,
                     Class4="colt_4",Class4median=Repstat4_median,Class4mean=Repstat4_mean,Class4sd=Repstat4_sd,Class4cv=Repstat4_cv,
                     Class5="filly_5",Class5median=Repstat5_median,Class5mean=Repstat5_mean,Class5sd=Repstat5_sd,Class5cv=Repstat5_cv,
                     Class6="foal_colt_6",Class6median=Repstat6_median,Class6mean=Repstat6_mean,Class6sd=Repstat6_sd,Class6cv=Repstat6_cv,
                     Class7="foal_filly_7",Class7median=Repstat7_median,Class7mean=Repstat7_mean,Class7sd=Repstat7_sd,Class7cv=Repstat7_cv)
write.csv(Result11,file="Result11.csv")

#Creating pairwise output file
observedpairs$pairpvalue<-pairpvalue
observedpairs$teststatmean<-pairmean
observedpairs$teststatmedian<-pairmedian
observedpairs$teststatsd<-pairsd
permmethod<-rep("nodeperm",times=c)
obsnetwork<-rep("SuAll",times=c)
obsattribute<-rep("RepStat",times=c)
obsmetric<-rep("betweenness",times=c)
observedpairs_result11<-cbind(obsnetwork,obsattribute,obsmetric,permmethod,observedpairs)
observedpairs_result11
write.table(observedpairs_result11,"observedpairs_result11.txt")


###########################################################################################
#Su All vs Repstat with strength
#########################################################################################

#create grouping vector
groupvec<-attribute$RepStatNum
metricvec<-attribute$Su15_All_strength

#Run permutations with weight permutation
#calculate observed test stat 
H_obs<-dunn.test(metricvec,groupvec,table=FALSE)$chi2
obspairs<-dunn.test(metricvec,groupvec,table=FALSE)$Z
paircomp<-dunn.test(metricvec,groupvec,table=FALSE)$comparisons
observedpairs<-data.frame(paircomp,obspairs)

#Create output matrix of pairwise comparisons
#c is number of pairs being compared as gathered from observedpairs file
c<-21
pairsmatrix<-matrix(0,nrow=c,ncol=P)
rownames(pairsmatrix)<-paircomp
random2<-rep(1:P)

#Runn Dunn's test for random networks
for (i in c(1:P)) {
  rand_df<-attribute[c("NumberID","ID")]
  node<-rand_df$NumberID
  rand_df<-cbind(rand_df,node)
  randnet<-rg_reshuffling_w(net,option="weights")
  metricrand<-degree_w(randnet, measure=c("degree","output","alpha"), alpha=0.5)
  metricrand2<-data.frame(metricrand)
  rand_df<-merge(rand_df,metricrand2,by="node",all=TRUE)
  rand_df[is.na(rand_df)]<-0
  Randmetricvec<-metricrand2$output
  random2[i]<-dunn.test(Randmetricvec,groupvec,table=FALSE)$chi2
  pairsmatrix[,i]<-dunn.test(Randmetricvec,groupvec,table=FALSE)$Z}

NAomit<-sum(is.nan(random2))
random3<-na.omit(random2)
pairsmatrix2<-t(na.omit(t(pairsmatrix)))
upperp<-sum(H_obs<random3)/P
lowerp<-sum(H_obs>random3)/P
absp<-sum(abs(H_obs)<abs(random3))/P
randmean<-mean(random3)
randmedian<-median(random3)
randsd<-sd(random3)

pairpvalue<-rep(1:c)
for(i in c(1:c)) {
  pairpvalue[i]<-sum(abs(obspairs[i])<abs(pairsmatrix2[i,]))/P}
pairmean<-rep(1:c)
for(i in c(1:c)) {
  pairmean[i]<-mean(pairsmatrix2[i,])}
pairmedian<-rep(1:c)
for(i in c(1:c)) {
  pairmedian[i]<-median(pairsmatrix2[i,])}
pairsd<-rep(1:c)
for(i in c(1:c)) {
  pairsd[i]<-sd(pairsmatrix2[i,])}

#creating main test stat output file
Repstat1<-attribute[which(attribute$RepStatNum==1),]
Repstat1_median<-median(Repstat1$Su15_All_strength)
Repstat1_mean<-mean(Repstat1$Su15_All_strength)
Repstat1_sd<-sd(Repstat1$Su15_All_strength)
Repstat1_cv<-(Repstat1_sd/Repstat1_mean)
Repstat2<-attribute[which(attribute$RepStatNum==2),]
Repstat2_median<-median(Repstat2$Su15_All_strength)
Repstat2_mean<-mean(Repstat2$Su15_All_strength)
Repstat2_sd<-sd(Repstat2$Su15_All_strength)
Repstat2_cv<-(Repstat2_sd/Repstat2_mean)
Repstat3<-attribute[which(attribute$RepStatNum==3),]
Repstat3_median<-median(Repstat3$Su15_All_strength)
Repstat3_mean<-mean(Repstat3$Su15_All_strength)
Repstat3_sd<-sd(Repstat3$Su15_All_strength)
Repstat3_cv<-(Repstat3_sd/Repstat3_mean)
Repstat4<-attribute[which(attribute$RepStatNum==4),]
Repstat4_median<-median(Repstat4$Su15_All_strength)
Repstat4_mean<-mean(Repstat4$Su15_All_strength)
Repstat4_sd<-sd(Repstat4$Su15_All_strength)
Repstat4_cv<-(Repstat4_sd/Repstat4_mean)
Repstat5<-attribute[which(attribute$RepStatNum==5),]
Repstat5_median<-median(Repstat5$Su15_All_strength)
Repstat5_mean<-mean(Repstat5$Su15_All_strength)
Repstat5_sd<-sd(Repstat5$Su15_All_strength)
Repstat5_cv<-(Repstat5_sd/Repstat5_mean)
Repstat6<-attribute[which(attribute$RepStatNum==6),]
Repstat6_median<-median(Repstat6$Su15_All_strength)
Repstat6_mean<-mean(Repstat6$Su15_All_strength)
Repstat6_sd<-sd(Repstat6$Su15_All_strength)
Repstat6_cv<-(Repstat6_sd/Repstat6_mean)
Repstat7<-attribute[which(attribute$RepStatNum==7),]
Repstat7_median<-median(Repstat7$Su15_All_strength)
Repstat7_mean<-mean(Repstat7$Su15_All_strength)
Repstat7_sd<-sd(Repstat7$Su15_All_strength)
Repstat7_cv<-(Repstat7_sd/Repstat7_mean)
Result12<-data.frame(group=Group,network="SuAll",attribute="RepStat",metric="strength",random="weights",teststat=H_obs,lowerp=lowerp,upperp=upperp,absp=absp,
                     NaNomit=NAomit,testrandmean=randmean,testrandmedian=randmedian,testrandsd=randsd,
                     Class1="stallion_1",Class1median=Repstat1_median,Class1mean=Repstat1_mean,Class1sd=Repstat1_sd,Class1cv=Repstat1_cv,
                     Class2="repmare_2",Class2median=Repstat2_median,Class2mean=Repstat2_mean,Class2sd=Repstat2_sd,Class2cv=Repstat2_cv,
                     Class3="nonrepmare_3",Class3median=Repstat3_median,Class3mean=Repstat3_mean,Class3sd=Repstat3_sd,Class3cv=Repstat3_cv,
                     Class4="colt_4",Class4median=Repstat4_median,Class4mean=Repstat4_mean,Class4sd=Repstat4_sd,Class4cv=Repstat4_cv,
                     Class5="filly_5",Class5median=Repstat5_median,Class5mean=Repstat5_mean,Class5sd=Repstat5_sd,Class5cv=Repstat5_cv,
                     Class6="foal_colt_6",Class6median=Repstat6_median,Class6mean=Repstat6_mean,Class6sd=Repstat6_sd,Class6cv=Repstat6_cv,
                     Class7="foal_filly_7",Class7median=Repstat7_median,Class7mean=Repstat7_mean,Class7sd=Repstat7_sd,Class7cv=Repstat7_cv)
write.csv(Result12,file="Result12.csv")

#Creating pairwise output file
observedpairs$pairpvalue<-pairpvalue
observedpairs$teststatmean<-pairmean
observedpairs$teststatmedian<-pairmedian
observedpairs$teststatsd<-pairsd
permmethod<-rep("weights",times=c)
obsnetwork<-rep("SuAll",times=c)
obsattribute<-rep("RepStat",times=c)
obsmetric<-rep("strength",times=c)
observedpairs_result12<-cbind(obsnetwork,obsattribute,obsmetric,permmethod,observedpairs)
observedpairs_result12
write.table(observedpairs_result12,"observedpairs_result12.txt")

#Run strengthpermutations with link permutation
#calculate observed test stat 
H_obs<-dunn.test(metricvec,groupvec,table=FALSE)$chi2
obspairs<-dunn.test(metricvec,groupvec,table=FALSE)$Z
paircomp<-dunn.test(metricvec,groupvec,table=FALSE)$comparisons
observedpairs<-data.frame(paircomp,obspairs)

#Create output matrix of pairwise comparisons
#c is number of pairs being compared as gathered from observedpairs file
c<-21
pairsmatrix<-matrix(0,nrow=c,ncol=P)
rownames(pairsmatrix)<-paircomp
random2<-rep(1:P)

#Runn Dunn's test for random networks
for (i in c(1:P)) {
  rand_df<-attribute[c("NumberID","ID")]
  node<-rand_df$NumberID
  rand_df<-cbind(rand_df,node)
  randnet<-rg_reshuffling_w(net,option="links")
  metricrand<-degree_w(randnet, measure=c("degree","output","alpha"), alpha=0.5)
  metricrand2<-data.frame(metricrand)
  rand_df<-merge(rand_df,metricrand2,by="node",all=TRUE)
  rand_df[is.na(rand_df)]<-0
  Randmetricvec<-metricrand2$output
  random2[i]<-dunn.test(Randmetricvec,groupvec,table=FALSE)$chi2
  pairsmatrix[,i]<-dunn.test(Randmetricvec,groupvec,table=FALSE)$Z}

NAomit<-sum(is.nan(random2))
random3<-na.omit(random2)
pairsmatrix2<-t(na.omit(t(pairsmatrix)))
upperp<-sum(H_obs<random3)/P
lowerp<-sum(H_obs>random3)/P
absp<-sum(abs(H_obs)<abs(random3))/P
randmean<-mean(random3)
randmedian<-median(random3)
randsd<-sd(random3)

pairpvalue<-rep(1:c)
for(i in c(1:c)) {
  pairpvalue[i]<-sum(abs(obspairs[i])<abs(pairsmatrix2[i,]))/P}
pairmean<-rep(1:c)
for(i in c(1:c)) {
  pairmean[i]<-mean(pairsmatrix2[i,])}
pairmedian<-rep(1:c)
for(i in c(1:c)) {
  pairmedian[i]<-median(pairsmatrix2[i,])}
pairsd<-rep(1:c)
for(i in c(1:c)) {
  pairsd[i]<-sd(pairsmatrix2[i,])}

Result13<-data.frame(group=Group,network="SuAll",attribute="RepStat",metric="strength",random="links",teststat=H_obs,lowerp=lowerp,upperp=upperp,absp=absp,
                     NaNomit=NAomit,testrandmean=randmean,testrandmedian=randmedian,testrandsd=randsd,
                     Class1="stallion_1",Class1median=Repstat1_median,Class1mean=Repstat1_mean,Class1sd=Repstat1_sd,Class1cv=Repstat1_cv,
                     Class2="repmare_2",Class2median=Repstat2_median,Class2mean=Repstat2_mean,Class2sd=Repstat2_sd,Class2cv=Repstat2_cv,
                     Class3="nonrepmare_3",Class3median=Repstat3_median,Class3mean=Repstat3_mean,Class3sd=Repstat3_sd,Class3cv=Repstat3_cv,
                     Class4="colt_4",Class4median=Repstat4_median,Class4mean=Repstat4_mean,Class4sd=Repstat4_sd,Class4cv=Repstat4_cv,
                     Class5="filly_5",Class5median=Repstat5_median,Class5mean=Repstat5_mean,Class5sd=Repstat5_sd,Class5cv=Repstat5_cv,
                     Class6="foal_colt_6",Class6median=Repstat6_median,Class6mean=Repstat6_mean,Class6sd=Repstat6_sd,Class6cv=Repstat6_cv,
                     Class7="foal_filly_7",Class7median=Repstat7_median,Class7mean=Repstat7_mean,Class7sd=Repstat7_sd,Class7cv=Repstat7_cv)
write.csv(Result13,file="Result13.csv")

#Creating pairwise output file
observedpairs$pairpvalue<-pairpvalue
observedpairs$teststatmean<-pairmean
observedpairs$teststatmedian<-pairmedian
observedpairs$teststatsd<-pairsd
permmethod<-rep("links",times=c)
obsnetwork<-rep("SuAll",times=c)
obsattribute<-rep("RepStat",times=c)
obsmetric<-rep("strength",times=c)
observedpairs_result13<-cbind(obsnetwork,obsattribute,obsmetric,permmethod,observedpairs)
observedpairs_result13
write.table(observedpairs_result13,"observedpairs_result13.txt")

#Run permutations with node permutation
#calculate observed test stat 
H_obs<-dunn.test(metricvec,groupvec,table=FALSE)$chi2
obspairs<-dunn.test(metricvec,groupvec,table=FALSE)$Z
paircomp<-dunn.test(metricvec,groupvec,table=FALSE)$comparisons
observedpairs<-data.frame(paircomp,obspairs)

#Create output matrix of pairwise comparisons
#c is number of pairs being compared as gathered from observedpairs file
c<-21
pairsmatrix<-matrix(0,nrow=c,ncol=P)
rownames(pairsmatrix)<-paircomp
random2<-rep(1:P)

#create sna matrix for randomization
library(sna)
snanet <- as.matrix(net)
N <- max(c(snanet[,"i"],snanet[,"j"]))
g <- matrix(data=0, nrow=N, ncol=N)
g[snanet[,c("i","j")]] <- snanet[,"w"]
snanet2<- as.sociomatrix.sna(g)

#Runn Dunn's test for random networks
for (i in c(1:P)) {
  rand_df<-attribute[c("NumberID","ID")]
  node<-rand_df$NumberID
  rand_df<-cbind(rand_df,node)
  randnet<-rmperm(snanet2)
  metricrand<-degree_w(randnet, measure=c("degree","output","alpha"), alpha=0.5)
  metricrand2<-data.frame(metricrand)
  rand_df<-merge(rand_df,metricrand2,by="node",all=TRUE)
  rand_df[is.na(rand_df)]<-0
  Randmetricvec<-metricrand2$output
  random2[i]<-dunn.test(Randmetricvec,groupvec,table=FALSE)$chi2
  pairsmatrix[,i]<-dunn.test(Randmetricvec,groupvec,table=FALSE)$Z}

NAomit<-sum(is.nan(random2))
random3<-na.omit(random2)
pairsmatrix2<-t(na.omit(t(pairsmatrix)))
upperp<-sum(H_obs<random3)/P
lowerp<-sum(H_obs>random3)/P
absp<-sum(abs(H_obs)<abs(random3))/P
randmean<-mean(random3)
randmedian<-median(random3)
randsd<-sd(random3)

pairpvalue<-rep(1:c)
for(i in c(1:c)) {
  pairpvalue[i]<-sum(abs(obspairs[i])<abs(pairsmatrix2[i,]))/P}
pairmean<-rep(1:c)
for(i in c(1:c)) {
  pairmean[i]<-mean(pairsmatrix2[i,])}
pairmedian<-rep(1:c)
for(i in c(1:c)) {
  pairmedian[i]<-median(pairsmatrix2[i,])}
pairsd<-rep(1:c)
for(i in c(1:c)) {
  pairsd[i]<-sd(pairsmatrix2[i,])}

detach("package:sna")

Result14<-data.frame(group=Group,network="SuAll",attribute="RepStat",metric="strength",random="nodeperm",teststat=H_obs,lowerp=lowerp,upperp=upperp,absp=absp,
                     NaNomit=NAomit,testrandmean=randmean,testrandmedian=randmedian,testrandsd=randsd,
                     Class1="stallion_1",Class1median=Repstat1_median,Class1mean=Repstat1_mean,Class1sd=Repstat1_sd,Class1cv=Repstat1_cv,
                     Class2="repmare_2",Class2median=Repstat2_median,Class2mean=Repstat2_mean,Class2sd=Repstat2_sd,Class2cv=Repstat2_cv,
                     Class3="nonrepmare_3",Class3median=Repstat3_median,Class3mean=Repstat3_mean,Class3sd=Repstat3_sd,Class3cv=Repstat3_cv,
                     Class4="colt_4",Class4median=Repstat4_median,Class4mean=Repstat4_mean,Class4sd=Repstat4_sd,Class4cv=Repstat4_cv,
                     Class5="filly_5",Class5median=Repstat5_median,Class5mean=Repstat5_mean,Class5sd=Repstat5_sd,Class5cv=Repstat5_cv,
                     Class6="foal_colt_6",Class6median=Repstat6_median,Class6mean=Repstat6_mean,Class6sd=Repstat6_sd,Class6cv=Repstat6_cv,
                     Class7="foal_filly_7",Class7median=Repstat7_median,Class7mean=Repstat7_mean,Class7sd=Repstat7_sd,Class7cv=Repstat7_cv)
write.csv(Result14,file="Result14.csv")

#Creating pairwise output file
observedpairs$pairpvalue<-pairpvalue
observedpairs$teststatmean<-pairmean
observedpairs$teststatmedian<-pairmedian
observedpairs$teststatsd<-pairsd
permmethod<-rep("nodeperm",times=c)
obsnetwork<-rep("SuAll",times=c)
obsattribute<-rep("RepStat",times=c)
obsmetric<-rep("strength",times=c)
observedpairs_result14<-cbind(obsnetwork,obsattribute,obsmetric,permmethod,observedpairs)
observedpairs_result14
write.table(observedpairs_result14,"observedpairs_result14.txt")

###########################################################################################
#Su All vs Repstat with degree0.5
#########################################################################################

#create grouping vector
groupvec<-attribute$RepStatNum
metricvec<-attribute$Su15_All_degreeA0.5

#Run permutations with weight permutation
#calculate observed test stat 
H_obs<-dunn.test(metricvec,groupvec,table=FALSE)$chi2
obspairs<-dunn.test(metricvec,groupvec,table=FALSE)$Z
paircomp<-dunn.test(metricvec,groupvec,table=FALSE)$comparisons
observedpairs<-data.frame(paircomp,obspairs)

#Create output matrix of pairwise comparisons
#c is number of pairs being compared as gathered from observedpairs file
c<-21
pairsmatrix<-matrix(0,nrow=c,ncol=P)
rownames(pairsmatrix)<-paircomp
random2<-rep(1:P)

#Runn Dunn's test for random networks
for (i in c(1:P)) {
  rand_df<-attribute[c("NumberID","ID")]
  node<-rand_df$NumberID
  rand_df<-cbind(rand_df,node)
  randnet<-rg_reshuffling_w(net,option="weights")
  metricrand<-degree_w(randnet, measure=c("degree","output","alpha"), alpha=0.5)
  metricrand2<-data.frame(metricrand)
  rand_df<-merge(rand_df,metricrand2,by="node",all=TRUE)
  rand_df[is.na(rand_df)]<-0
  Randmetricvec<-metricrand2$alpha
  random2[i]<-dunn.test(Randmetricvec,groupvec,table=FALSE)$chi2
  pairsmatrix[,i]<-dunn.test(Randmetricvec,groupvec,table=FALSE)$Z}

NAomit<-sum(is.nan(random2))
random3<-na.omit(random2)
pairsmatrix2<-t(na.omit(t(pairsmatrix)))
upperp<-sum(H_obs<random3)/P
lowerp<-sum(H_obs>random3)/P
absp<-sum(abs(H_obs)<abs(random3))/P
randmean<-mean(random3)
randmedian<-median(random3)
randsd<-sd(random3)

pairpvalue<-rep(1:c)
for(i in c(1:c)) {
  pairpvalue[i]<-sum(abs(obspairs[i])<abs(pairsmatrix2[i,]))/P}
pairmean<-rep(1:c)
for(i in c(1:c)) {
  pairmean[i]<-mean(pairsmatrix2[i,])}
pairmedian<-rep(1:c)
for(i in c(1:c)) {
  pairmedian[i]<-median(pairsmatrix2[i,])}
pairsd<-rep(1:c)
for(i in c(1:c)) {
  pairsd[i]<-sd(pairsmatrix2[i,])}

#creating main test stat output file
Repstat1<-attribute[which(attribute$RepStatNum==1),]
Repstat1_median<-median(Repstat1$Su15_All_degreeA0.5)
Repstat1_mean<-mean(Repstat1$Su15_All_degreeA0.5)
Repstat1_sd<-sd(Repstat1$Su15_All_degreeA0.5)
Repstat1_cv<-(Repstat1_sd/Repstat1_mean)
Repstat2<-attribute[which(attribute$RepStatNum==2),]
Repstat2_median<-median(Repstat2$Su15_All_degreeA0.5)
Repstat2_mean<-mean(Repstat2$Su15_All_degreeA0.5)
Repstat2_sd<-sd(Repstat2$Su15_All_degreeA0.5)
Repstat2_cv<-(Repstat2_sd/Repstat2_mean)
Repstat3<-attribute[which(attribute$RepStatNum==3),]
Repstat3_median<-median(Repstat3$Su15_All_degreeA0.5)
Repstat3_mean<-mean(Repstat3$Su15_All_degreeA0.5)
Repstat3_sd<-sd(Repstat3$Su15_All_degreeA0.5)
Repstat3_cv<-(Repstat3_sd/Repstat3_mean)
Repstat4<-attribute[which(attribute$RepStatNum==4),]
Repstat4_median<-median(Repstat4$Su15_All_degreeA0.5)
Repstat4_mean<-mean(Repstat4$Su15_All_degreeA0.5)
Repstat4_sd<-sd(Repstat4$Su15_All_degreeA0.5)
Repstat4_cv<-(Repstat4_sd/Repstat4_mean)
Repstat5<-attribute[which(attribute$RepStatNum==5),]
Repstat5_median<-median(Repstat5$Su15_All_degreeA0.5)
Repstat5_mean<-mean(Repstat5$Su15_All_degreeA0.5)
Repstat5_sd<-sd(Repstat5$Su15_All_degreeA0.5)
Repstat5_cv<-(Repstat5_sd/Repstat5_mean)
Repstat6<-attribute[which(attribute$RepStatNum==6),]
Repstat6_median<-median(Repstat6$Su15_All_degreeA0.5)
Repstat6_mean<-mean(Repstat6$Su15_All_degreeA0.5)
Repstat6_sd<-sd(Repstat6$Su15_All_degreeA0.5)
Repstat6_cv<-(Repstat6_sd/Repstat6_mean)
Repstat7<-attribute[which(attribute$RepStatNum==7),]
Repstat7_median<-median(Repstat7$Su15_All_degreeA0.5)
Repstat7_mean<-mean(Repstat7$Su15_All_degreeA0.5)
Repstat7_sd<-sd(Repstat7$Su15_All_degreeA0.5)
Repstat7_cv<-(Repstat7_sd/Repstat7_mean)
Result15<-data.frame(group=Group,network="SuAll",attribute="RepStat",metric="degree0.5",random="weights",teststat=H_obs,lowerp=lowerp,upperp=upperp,absp=absp,
                     NaNomit=NAomit,testrandmean=randmean,testrandmedian=randmedian,testrandsd=randsd,
                     Class1="stallion_1",Class1median=Repstat1_median,Class1mean=Repstat1_mean,Class1sd=Repstat1_sd,Class1cv=Repstat1_cv,
                     Class2="repmare_2",Class2median=Repstat2_median,Class2mean=Repstat2_mean,Class2sd=Repstat2_sd,Class2cv=Repstat2_cv,
                     Class3="nonrepmare_3",Class3median=Repstat3_median,Class3mean=Repstat3_mean,Class3sd=Repstat3_sd,Class3cv=Repstat3_cv,
                     Class4="colt_4",Class4median=Repstat4_median,Class4mean=Repstat4_mean,Class4sd=Repstat4_sd,Class4cv=Repstat4_cv,
                     Class5="filly_5",Class5median=Repstat5_median,Class5mean=Repstat5_mean,Class5sd=Repstat5_sd,Class5cv=Repstat5_cv,
                     Class6="foal_colt_6",Class6median=Repstat6_median,Class6mean=Repstat6_mean,Class6sd=Repstat6_sd,Class6cv=Repstat6_cv,
                     Class7="foal_filly_7",Class7median=Repstat7_median,Class7mean=Repstat7_mean,Class7sd=Repstat7_sd,Class7cv=Repstat7_cv)
write.csv(Result15,file="Result15.csv")

#Creating pairwise output file
observedpairs$pairpvalue<-pairpvalue
observedpairs$teststatmean<-pairmean
observedpairs$teststatmedian<-pairmedian
observedpairs$teststatsd<-pairsd
permmethod<-rep("weights",times=c)
obsnetwork<-rep("SuAll",times=c)
obsattribute<-rep("RepStat",times=c)
obsmetric<-rep("degree0.5",times=c)
observedpairs_result15<-cbind(obsnetwork,obsattribute,obsmetric,permmethod,observedpairs)
observedpairs_result15
write.table(observedpairs_result15,"observedpairs_result15.txt")

#Run strengthpermutations with link permutation
#calculate observed test stat 
H_obs<-dunn.test(metricvec,groupvec,table=FALSE)$chi2
obspairs<-dunn.test(metricvec,groupvec,table=FALSE)$Z
paircomp<-dunn.test(metricvec,groupvec,table=FALSE)$comparisons
observedpairs<-data.frame(paircomp,obspairs)

#Create output matrix of pairwise comparisons
#c is number of pairs being compared as gathered from observedpairs file
c<-21
pairsmatrix<-matrix(0,nrow=c,ncol=P)
rownames(pairsmatrix)<-paircomp
random2<-rep(1:P)

#Runn Dunn's test for random networks
for (i in c(1:P)) {
  rand_df<-attribute[c("NumberID","ID")]
  node<-rand_df$NumberID
  rand_df<-cbind(rand_df,node)
  randnet<-rg_reshuffling_w(net,option="links")
  metricrand<-degree_w(randnet, measure=c("degree","output","alpha"), alpha=0.5)
  metricrand2<-data.frame(metricrand)
  rand_df<-merge(rand_df,metricrand2,by="node",all=TRUE)
  rand_df[is.na(rand_df)]<-0
  Randmetricvec<-metricrand2$alpha
  random2[i]<-dunn.test(Randmetricvec,groupvec,table=FALSE)$chi2
  pairsmatrix[,i]<-dunn.test(Randmetricvec,groupvec,table=FALSE)$Z}

NAomit<-sum(is.nan(random2))
random3<-na.omit(random2)
pairsmatrix2<-t(na.omit(t(pairsmatrix)))
upperp<-sum(H_obs<random3)/P
lowerp<-sum(H_obs>random3)/P
absp<-sum(abs(H_obs)<abs(random3))/P
randmean<-mean(random3)
randmedian<-median(random3)
randsd<-sd(random3)

pairpvalue<-rep(1:c)
for(i in c(1:c)) {
  pairpvalue[i]<-sum(abs(obspairs[i])<abs(pairsmatrix2[i,]))/P}
pairmean<-rep(1:c)
for(i in c(1:c)) {
  pairmean[i]<-mean(pairsmatrix2[i,])}
pairmedian<-rep(1:c)
for(i in c(1:c)) {
  pairmedian[i]<-median(pairsmatrix2[i,])}
pairsd<-rep(1:c)
for(i in c(1:c)) {
  pairsd[i]<-sd(pairsmatrix2[i,])}

Result16<-data.frame(group=Group,network="SuAll",attribute="RepStat",metric="degree0.5",random="links",teststat=H_obs,lowerp=lowerp,upperp=upperp,absp=absp,
                     NaNomit=NAomit,testrandmean=randmean,testrandmedian=randmedian,testrandsd=randsd,
                     Class1="stallion_1",Class1median=Repstat1_median,Class1mean=Repstat1_mean,Class1sd=Repstat1_sd,Class1cv=Repstat1_cv,
                     Class2="repmare_2",Class2median=Repstat2_median,Class2mean=Repstat2_mean,Class2sd=Repstat2_sd,Class2cv=Repstat2_cv,
                     Class3="nonrepmare_3",Class3median=Repstat3_median,Class3mean=Repstat3_mean,Class3sd=Repstat3_sd,Class3cv=Repstat3_cv,
                     Class4="colt_4",Class4median=Repstat4_median,Class4mean=Repstat4_mean,Class4sd=Repstat4_sd,Class4cv=Repstat4_cv,
                     Class5="filly_5",Class5median=Repstat5_median,Class5mean=Repstat5_mean,Class5sd=Repstat5_sd,Class5cv=Repstat5_cv,
                     Class6="foal_colt_6",Class6median=Repstat6_median,Class6mean=Repstat6_mean,Class6sd=Repstat6_sd,Class6cv=Repstat6_cv,
                     Class7="foal_filly_7",Class7median=Repstat7_median,Class7mean=Repstat7_mean,Class7sd=Repstat7_sd,Class7cv=Repstat7_cv)
write.csv(Result16,file="Result16.csv")

#Creating pairwise output file
observedpairs$pairpvalue<-pairpvalue
observedpairs$teststatmean<-pairmean
observedpairs$teststatmedian<-pairmedian
observedpairs$teststatsd<-pairsd
permmethod<-rep("links",times=c)
obsnetwork<-rep("SuAll",times=c)
obsattribute<-rep("RepStat",times=c)
obsmetric<-rep("degree0.5",times=c)
observedpairs_result16<-cbind(obsnetwork,obsattribute,obsmetric,permmethod,observedpairs)
observedpairs_result16
write.table(observedpairs_result16,"observedpairs_result16.txt")

#Run permutations with node permutation
#calculate observed test stat 
H_obs<-dunn.test(metricvec,groupvec,table=FALSE)$chi2
obspairs<-dunn.test(metricvec,groupvec,table=FALSE)$Z
paircomp<-dunn.test(metricvec,groupvec,table=FALSE)$comparisons
observedpairs<-data.frame(paircomp,obspairs)

#Create output matrix of pairwise comparisons
#c is number of pairs being compared as gathered from observedpairs file
c<-21
pairsmatrix<-matrix(0,nrow=c,ncol=P)
rownames(pairsmatrix)<-paircomp
random2<-rep(1:P)

#create sna matrix for randomization
library(sna)
snanet <- as.matrix(net)
N <- max(c(snanet[,"i"],snanet[,"j"]))
g <- matrix(data=0, nrow=N, ncol=N)
g[snanet[,c("i","j")]] <- snanet[,"w"]
snanet2<- as.sociomatrix.sna(g)

#Runn Dunn's test for random networks
for (i in c(1:P)) {
  rand_df<-attribute[c("NumberID","ID")]
  node<-rand_df$NumberID
  rand_df<-cbind(rand_df,node)
  randnet<-rmperm(snanet2)
  metricrand<-degree_w(randnet, measure=c("degree","output","alpha"), alpha=0.5)
  metricrand2<-data.frame(metricrand)
  rand_df<-merge(rand_df,metricrand2,by="node",all=TRUE)
  rand_df[is.na(rand_df)]<-0
  Randmetricvec<-metricrand2$alpha
  random2[i]<-dunn.test(Randmetricvec,groupvec,table=FALSE)$chi2
  pairsmatrix[,i]<-dunn.test(Randmetricvec,groupvec,table=FALSE)$Z}

NAomit<-sum(is.nan(random2))
random3<-na.omit(random2)
pairsmatrix2<-t(na.omit(t(pairsmatrix)))
upperp<-sum(H_obs<random3)/P
lowerp<-sum(H_obs>random3)/P
absp<-sum(abs(H_obs)<abs(random3))/P
randmean<-mean(random3)
randmedian<-median(random3)
randsd<-sd(random3)

pairpvalue<-rep(1:c)
for(i in c(1:c)) {
  pairpvalue[i]<-sum(abs(obspairs[i])<abs(pairsmatrix2[i,]))/P}
pairmean<-rep(1:c)
for(i in c(1:c)) {
  pairmean[i]<-mean(pairsmatrix2[i,])}
pairmedian<-rep(1:c)
for(i in c(1:c)) {
  pairmedian[i]<-median(pairsmatrix2[i,])}
pairsd<-rep(1:c)
for(i in c(1:c)) {
  pairsd[i]<-sd(pairsmatrix2[i,])}

detach("package:sna")

Result17<-data.frame(group=Group,network="SuAll",attribute="RepStat",metric="degree0.5",random="nodeperm",teststat=H_obs,lowerp=lowerp,upperp=upperp,absp=absp,
                     NaNomit=NAomit,testrandmean=randmean,testrandmedian=randmedian,testrandsd=randsd,
                     Class1="stallion_1",Class1median=Repstat1_median,Class1mean=Repstat1_mean,Class1sd=Repstat1_sd,Class1cv=Repstat1_cv,
                     Class2="repmare_2",Class2median=Repstat2_median,Class2mean=Repstat2_mean,Class2sd=Repstat2_sd,Class2cv=Repstat2_cv,
                     Class3="nonrepmare_3",Class3median=Repstat3_median,Class3mean=Repstat3_mean,Class3sd=Repstat3_sd,Class3cv=Repstat3_cv,
                     Class4="colt_4",Class4median=Repstat4_median,Class4mean=Repstat4_mean,Class4sd=Repstat4_sd,Class4cv=Repstat4_cv,
                     Class5="filly_5",Class5median=Repstat5_median,Class5mean=Repstat5_mean,Class5sd=Repstat5_sd,Class5cv=Repstat5_cv,
                     Class6="foal_colt_6",Class6median=Repstat6_median,Class6mean=Repstat6_mean,Class6sd=Repstat6_sd,Class6cv=Repstat6_cv,
                     Class7="foal_filly_7",Class7median=Repstat7_median,Class7mean=Repstat7_mean,Class7sd=Repstat7_sd,Class7cv=Repstat7_cv)
write.csv(Result17,file="Result17.csv")

#Creating pairwise output file
observedpairs$pairpvalue<-pairpvalue
observedpairs$teststatmean<-pairmean
observedpairs$teststatmedian<-pairmedian
observedpairs$teststatsd<-pairsd
permmethod<-rep("nodeperm",times=c)
obsnetwork<-rep("SuAll",times=c)
obsattribute<-rep("RepStat",times=c)
obsmetric<-rep("degree0.5",times=c)
observedpairs_result17<-cbind(obsnetwork,obsattribute,obsmetric,permmethod,observedpairs)
observedpairs_result17
write.table(observedpairs_result17,"observedpairs_result17.txt")

########################################################################################################################
##Output final results###
###############################################################################################################
FinalResult_SuAllRepstat<-rbind(Result_9b,Result10,Result11,Result12,Result13,Result14,Result15,Result16,Result17)
FinalPairwise_SuAllRepstat<-rbind(observedpairs_result9b,observedpairs_result10,observedpairs_result11,observedpairs_result12,observedpairs_result13,
                                  observedpairs_result14,observedpairs_result15,observedpairs_result16,observedpairs_result17)

write.csv(FinalResult_SuAllRepstat,"FinalResult_SuAllRepstat.csv")
write.table(FinalPairwise_SuAllRepstat,"FinalPairwiseResult_SuAllRepstat.txt")
