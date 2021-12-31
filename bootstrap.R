# STDrop.Rcm
setwd("C:/Users/USER/Dropbox/thesis berkah/command") 
d1 <- read.csv("5051.csv",h=T, as.is=T) #read data file 1

d2 <- read.csv("5254.csv",h=T,as.is=T) #read data file 2

d3 <- read.csv("fm.csv",h=T,as.is=T)# father and mother Education level


d <- rbind(d1,d2)

d <- merge(d,d3,by="Std_ID")


str(d)


d <- subset(d,d$Status!="Death") #remove death cases

str(d)


#d$Income.Father<-as.integer(d$Income.Father)

#d$Income.Mother<-as.integer(d$Income.Mother)


d$yr <- as.factor(d$Year)


d$fac <- ifelse(d$Faculty=="Faculty of Education","1:Edu",
	ifelse(d$Faculty=="College of Islamic Study","2:IS",
	ifelse(d$Faculty=="Faculty of Political Science","3:PS",
	ifelse(d$Faculty=="Faculty of Humanities and Social Sciences","4:HSS", 
	ifelse(d$Faculty=="Faculty of Communication Sciences","5:CS",
	ifelse(d$Faculty=="Faculty of Science and Technology","7:SAT","6:FAA"))))))



d$Rel <- ifelse(d$Religion=="Islamism","Muslim","Other")


d$gen.rel <- ifelse(d$Rel=="Muslim"&d$Gender=="Female","1:FM",
	ifelse(d$Rel=="Other"&d$Gender=="Female","2:FO",
	ifelse(d$Rel=="Muslim"&d$Gender=="Male","3:MM","4:MO")))



s <- d$Status

d$y <- ifelse(s %in% c("Graduate","Studying","studying"), 0,1)


d$dropout <- ifelse(s %in% c("Graduate","Studying","studying"),"1:Non-dropout", 
	ifelse(s %in% c("Resign", "resigned", "Not Registered"), "2: Voluntary", "3: Involuntary"))



e <- d$EM #Admission type

d$em <- ifelse(e %in% c("Project Test","Project"),"1:Centrazlied System",
	ifelse(e=="Admission Program","2:Direct System","3:Other"))



g <- d$S_gpa

d$gpa <- ifelse(g<=2,"2:Low","1:High")



h <- d$Hometown

d$ht <- ifelse(h=="Pattani","2:Pattani",ifelse(h=="Narathiwat","1:Narathiwat", 
	ifelse(h=="Yala","3:Yala",ifelse(h=="Songkla","4:Songkla","5:Other"))))



#f<-d$Income.Father #143 NAs

#f<-ifelse(f%%2!=0, NA, f) # 169 NAs

#f<-ifelse(f>0 & f<1000, NA, f)#209 NAs

#f<-f/100

#f<-ifelse(is.wholenumber(f)==TRUE, f, NA)#460 NAs

#d$finc<-f*100



#m<-d$Income.Mother #120 NAs

#m<-ifelse(m%%2!=0, NA, m) # 135 NAs

#m<-ifelse(m>0 & m<1000, NA, m)# 187 NAs

#m<-m/100
#m<-ifelse(is.wholenumber(m)==TRUE, m, NA)#335 NAs

#d$minc<-m*100

#d$minc<-ifelse(d$minc<0, NA, d$minc) #replace negative income with NA, 338 NAs



#d$finc<-ifelse(is.na(d$finc), 0, d$finc)# Median=5000

#d$finc<-ifelse(d$finc<=10000, "1:Low", ifelse(d$finc>10000 &d$finc<=15000,"2:Middle","3:High"))


#d$minc<-ifelse(is.na(d$minc), 0, d$minc)#Median=3500

#d$minc<-ifelse(d$minc<=10000, "1:Low", ifelse(d$minc>10000 & d$minc<=15000,"2:Middle","3:High"))



me <- d$Mot_Edu

me <- ifelse(is.na(me)==T,"Others",me) # recode NA as Others


d$medu <- ifelse(me %in% c("elementary educatio 1th","elementary educatio 2th",
	"elementary educatio 3th","elementary educatio 4th","elementary educatio 5th",
	"elementary educatio 6th","elementary educatio 7th","elementary education",
	"high elementary educatio","prelimimary elementary educatio"),"1:Elementary", 
	ifelse(me %in% c("high school","high school 1","high school 1th","high school 2th",
	"high school 3th","high school 4th","high school 5th","high school 6th","high school 7th",
	"high school 8th","less than under graduate","high shcool"), "2:High School", 
	ifelse(me %in% c("Master degree","High Education","Master degree???","Research Fellow",
	"master degree or equa","under graduate", "under graduate - master degree"),"4:Bachelors",
	ifelse(me %in% c("Certificate 2","certificate","certificate 1","diploma","high diphoma",
	"high vocational Certificate","vocational certificate","vocational certificate high",
	"vocational certificate techniqcal"),"3:Certificate/Diploma",
	ifelse(me=="no edu brackgrund","0:No Education","5:Unknown")))))


addmargins(table(d$medu, d$y))


fe <- d$Fat_Edu

fe <- ifelse(is.na(fe)==T,"Others",fe) # recode NA as Others

d$fedu <- ifelse(fe %in% c("elementary educatio 1th","elementary educatio 2th",
	"elementary educatio 3th","elementary educatio 4th","elementary educatio 5th",
	"elementary educatio 6th","elementary educatio 7th","elementary education",
	"high elementary educatio","prelimimary elementary educatio"),"1:Elementary", 
	ifelse(fe %in% c("high school","high school 1","high school 1th","high school 2th",
	"high school 3th","high school 4th","high school 5th","high school 6th",
	"high school 7th","high school 8th","less than under graduate","high shcool"), 
	"2:High School", ifelse(fe %in% c("Master degree","High Education","Master degree???",
	"Research Fellow","master degree or equa","under graduate", 
	"under graduate - master degree"),"4:Bachelors", 
	ifelse(fe %in% c("Certificate 2","certificate","certificate 1","diploma","high diphoma",
	"high vocational Certificate","vocational certificate","vocational certificate high",
	"vocational certificate techniqcal"),"3:Certificate/Diploma",
	ifelse(fe=="no edu brackgrund","0:No Education","5:Unknown")))))



addmargins(table(d$fedu,d$y))


d$yr <- as.factor(d$Year)

d$yr <- ifelse(d$yr=="2550","2007",ifelse(d$yr=="2551","2008", 
	ifelse(d$yr=="2552","2009",ifelse(d$yr=="2553","2010","2011"))))

str(d)






mod_full <- glm(data=d,family="binomial",y~factor(Year)+factor(fac)+factor(gpa)+factor(em)+
	factor(gen.rel)+factor(ht)+factor(fedu)+factor(medu))
drop1(mod_full,test="Chisq")


mod_reduced <- glm(data=d,family="binomial",y~factor(yr)+factor(fac)+factor(gpa)+factor(gen.rel)+
	factor(ht)+factor(medu))


summary(mod_reduced)
#---------------------------------------------------------

# Inflate data using bootstrap
IF <- 2		# Inflation factor
set.seed(7531)
n0 <- nrow(d)
n <- n0*IF
SampIDs <- sample(1:n0,replace=T,size=n)
d <- d[SampIDs,]

d$yr <- as.factor(d$yr)
d$fac <- as.factor(d$fac)
d$gpa <- as.factor(d$gpa)
d$gen.rel <- as.factor(d$gen.rel)
d$ht <- as.factor(d$ht)
d$medu <- as.factor(d$medu)
d$yf <- as.factor(d$y)

options(scipen=8)                                                   #why
d.lr <- glm(family=binomial,data=d,y~yr+fac+gpa+gen.rel+ht+medu)
summary(d.lr)
drop1(d.lr,test="Chisq") -> rez1
rez1
pval <- rez1$"Pr(>Chi)"[2:3]
pval1 <- ifelse(pval[1]<0.0001,"<0.0001",round(pval[1],4))
pval2 <- ifelse(pval[2]<0.0001,"<0.0001",round(pval[2],4))

prob.d.lr <- predict(d.lr,type="response")                       ########
co.lr <- 0.42
pred.d.lr <- ifelse(prob.d.lr>co.lr,1,0)
cTab.lr <- table(pred.d.lr,d$y)
addmargins(cTab.lr)
pa.d.lr <- 100*sum(diag(cTab.lr))/sum(cTab.lr)
pa.d.lr

#--------------------------------------------
# recursive partitioning

library(rpart)
cpval <- 0.001
d.rp <- rpart(data=d,y~yr+fac+gpa+gen.rel+ht+medu,cp=cpval)
printcp(d.rp)

#windows(12,8)			# Figure 2: Error plot and decision tree

par(mfrow=c(2,1),las=1,oma=c(0,1,1,0),mar=c(1,3,1,2),mgp=c(1.1,0.1,0),tcl=0.2)
plotcp(d.rp)
mtext(side=3,adj=0.5,line=1,"Size of tree")
mtext(side=1,adj=0.5,line=1,"Complexity parameter (cp)")
plot(d.rp,uniform=T,margin=0.1)
text(d.rp,use.n=T,cex=0.7,font=2,digits=3)
mtext(side=2,line=-3,padj=-6,"Decision Tree")
n <- nrow(d)
mtext(side=2,line=-3,padj=-4,paste("n =",n))

prob.d.rp <- predict(d.rp,type="vector")
co.rp <- 0.42
pred.d.rp <- ifelse(prob.d.rp>co.rp,1,0)
cTab.rp <- table(pred.d.rp,d$y)
addmargins(cTab.rp)
pa.d.rp <- 100*sum(diag(cTab.rp))/sum(cTab.rp)
pa.d.rp

#windows(12,8)			# Figure 3: Scatter plot from decision tree

par(mfrow=c(2,1),las=1,oma=c(0.5,1.5,0,0),mar=c(2,1,1,1),mgp=c(1.1,0.1,0),tcl=0.2)
plot(d.rp,uniform=T,margin=0.11)
text(d.rp,use.n=T,cex=0.7,digits=3,font=2)
mtext(side=2,line=-4,padj=-6,"Decision Tree")
mtext(side=2,line=-4,padj=-4,paste("n =",n))

library(partykit)
d.gp <- predict(as.party(d.rp),type="node")
addmargins(table(d.gp))

d.gm <- tapply(d$y,d.gp,mean)	# group means
ng <- length(d.gm)

xlm <- c(0.5,ng+0.5)
ylm <- c(-0.06,max(d.gm)+0.06)
plot(1,type="n",xlim=xlm,ylim=ylm,ylab="",xaxt="n",xlab="Group",cex.axis=0.8,cex.lab=0.9)
abline(h=co.rp,col=2)
points(c(1:ng),d.gm,pch=20)
ns <- rep(c(1,-1),ng)[1:ng]
text(c(1:ng),d.gm+0.05*ns,table(d.gp),cex=0.8)
axis(side=1,at=c(1:ng),lab=c(1:ng),cex.axis=0.8)
mtext(side=3,line=0.15,adj=-0.05,"Dropout Rate",cex=0.9)
titl <- paste(n,"samples")
mtext(side=3,line=0.1,adj=1,titl,cex=0.9)

#--------------------------------------------

library(randomForest)
d.rf <- randomForest(data=d,yf~yr+fac+gpa+gen.rel+ht+medu,
	importance=T,ntree=1000)

#windows(6,6)			# Figure 4: Importance plot

varImpPlot(d.rf,pch=21,bg=4)
mtext(line=1,adj=1,paste("Sample size: ",nrow(d),sep=""))
prob.d.rf <- predict(d.rf,type="prob")[,2]
co.rf <- 0.4
pred.d.rf <- ifelse(prob.d.rf>co.rf,1,0)
cTab.rf <- table(pred.d.rf,d$y)
addmargins(cTab.rf)
pa.d.rf <- 100*sum(diag(cTab.rf))/sum(cTab.rf)
pa.d.rf

library(nnet)
set.seed(1234567)

nn <- 21			# number of (hidden) nodes

d.nt <- nnet(data=d,y~yr+fac+gpa+gen.rel+ht+medu,
	trace=F,size=nn,rang=0.1,decay=1e-2,maxit=2000)
prob.d.nt <- predict(d.nt,type="raw")
co.nt <- 0.4
pred.d.nt <- ifelse(prob.d.nt>co.nt,1,0)
cTab.nt <- table(pred.d.nt,d$y)
addmargins(cTab.nt)
pa.d.nt <- 100*sum(diag(cTab.nt))/sum(cTab.nt)
pa.d.nt

#windows(12,8)			# Figure 5: Plot comparing methods

par(mfrow=c(2,2),oma=c(0,0,0,0),mar=c(3,3,3,4),
	las=1,mgp=c(1.1,0.1,0),tcl=0.2)

ylm <- c(-0.1,1.1); xlm <- c(0,max(prob.d.lr))
xlb <- "Estimated Risk"
plot(1,type="n",ylab="",xlab=xlb,ylim=ylm,xlim=xlm,yaxt="n")
abline(v=co.lr,col=2,lwd=2)
died <- prob.d.lr[d$y==1]
lived <- prob.d.lr[d$y==0]
q1 <- quantile(died,probs=c(0:4)/4)
q0 <- quantile(lived,probs=c(0:4)/4)
wd <- 0.08
clr <- "lemonchiffon"
polygon(c(q1[2],q1[4],q1[4],q1[2],q1[2]),1+wd*c(-1,-1,1,1,-1),col=clr)
points(c(q1[3],q1[3]),1+wd*c(-1,1),type="l")
polygon(c(q0[2],q0[4],q0[4],q0[2],q0[2]),wd*c(-1,-1,1,1,-1),col=clr)
points(c(q0[3],q0[3]),wd*c(-1,1),type="l")
points(prob.d.lr,d$y)
axis(side=2,at=c(0.2,0.8),lab=c("No","Yes"))
mtext(side=3,adj=-0.1,line=0.2,"Drop",cex=0.8)
mtext(side=3,adj=1,line=0.2,paste(n,"Samples"),cex=0.8)
axis(side=4,at=1,lab="Total",font=3,hadj=-0.25,tcl=0)
mtext(side=3,adj=0.5,line=1.4,"Logistic Regression Model",font=2,cex=0.8)
tSum <- table(d$y)
axis(side=4,at=c(0.2,0.8),lab=tSum,hadj=-0.2)
legend("bottomleft",inset=c(-0.02,0.2),bty="n",leg=cTab.lr[1,1])
lg1 <- paste(cTab.lr[2,1],"+")
legend("bottomright",inset=c(0.02,0.2),bty="n",leg=lg1,text.col=2)
lg2 <- paste(cTab.lr[1,2],"-")
legend("topleft",inset=c(-0.02,0.2),bty="n",leg=lg2,text.col=2)
legend("topright",inset=c(0.02,0.2),bty="n",leg=cTab.lr[2,2])
lg <- paste("Accuracy:",round(pa.d.lr,2),"%")
legend("right",inset=0.25,leg=lg,x.intersp=0.2,bg="ivory")

ylm <- c(-0.1,1.1); xlm <- c(0,max(prob.d.rp))
xlb <- "Estimated Risk"
plot(1,type="n",ylab="",xlab=xlb,ylim=ylm,xlim=xlm,yaxt="n")
abline(v=co.rp,col=2,lwd=2)
died <- prob.d.rp[d$y==1]
lived <- prob.d.rp[d$y==0]
q1 <- quantile(died,probs=c(0:4)/4)
q0 <- quantile(lived,probs=c(0:4)/4)
wd <- 0.08
clr <- "lemonchiffon"
polygon(c(q1[2],q1[4],q1[4],q1[2],q1[2]),1+wd*c(-1,-1,1,1,-1),col=clr)
points(c(q1[3],q1[3]),1+wd*c(-1,1),type="l")
polygon(c(q0[2],q0[4],q0[4],q0[2],q0[2]),wd*c(-1,-1,1,1,-1),col=clr)
points(c(q0[3],q0[3]),wd*c(-1,1),type="l")
points(prob.d.rp,d$y)
axis(side=2,at=c(0.2,0.8),lab=c("No","Yes"))
mtext(side=3,adj=-0.1,line=0.2,"Drop",cex=0.8)
mtext(side=3,adj=1,line=0.2,paste(n,"Samples"),cex=0.8)
axis(side=4,at=1,lab="Total",font=3,hadj=-0.25,tcl=0)
titl.rp <- paste("Recursive Partitioning (cp=",cpval,")",sep="")
mtext(side=3,adj=0.5,line=1.4,titl.rp,font=2,cex=0.8)
tSum <- table(d$y)
axis(side=4,at=c(0.2,0.8),lab=tSum,hadj=-0.2)
legend("bottomleft",inset=c(-0.02,0.2),bty="n",leg=cTab.rp[1,1])
lg1 <- paste(cTab.rp[2,1],"+")
legend("bottomright",inset=c(0.02,0.2),bty="n",leg=lg1,text.col=2)
lg2 <- paste(cTab.rp[1,2],"-")
legend("topleft",inset=c(-0.02,0.2),bty="n",leg=lg2,text.col=2)
legend("topright",inset=c(0.02,0.2),bty="n",leg=cTab.rp[2,2])
lg <- paste("Accuracy:",round(pa.d.rp,2),"%")
legend("right",inset=0.25,leg=lg,x.intersp=0.2,bg="ivory")

ylm <- c(-0.1,1.1); xlm <- c(0,max(prob.d.rf))
xlb <- "Estimated Risk"
plot(1,type="n",ylab="",xlab=xlb,ylim=ylm,xlim=xlm,yaxt="n")
abline(v=co.rf,col=2,lwd=2)
died <- prob.d.rf[d$y==1]
lived <- prob.d.rf[d$y==0]
q1 <- quantile(died,probs=c(0:4)/4)
q0 <- quantile(lived,probs=c(0:4)/4)
wd <- 0.08
clr <- "lemonchiffon"
polygon(c(q1[2],q1[4],q1[4],q1[2],q1[2]),1+wd*c(-1,-1,1,1,-1),col=clr)
points(c(q1[3],q1[3]),1+wd*c(-1,1),type="l")
polygon(c(q0[2],q0[4],q0[4],q0[2],q0[2]),wd*c(-1,-1,1,1,-1),col=clr)
points(c(q0[3],q0[3]),wd*c(-1,1),type="l")
points(prob.d.rf,d$y)
axis(side=2,at=c(0.2,0.8),lab=c("No","Yes"))
mtext(side=3,adj=-0.1,line=0.2,"Drop",cex=0.8)
mtext(side=3,adj=1,line=0.2,paste(n,"Samples"),cex=0.8)
axis(side=4,at=1,lab="Total",font=3,hadj=-0.25,tcl=0)
mtext(side=3,adj=0.5,line=1.4,"Random Forest",font=2,cex=0.8)
tSum <- table(d$y)
axis(side=4,at=c(0.2,0.8),lab=tSum,hadj=-0.2)
legend("bottomleft",inset=c(-0.02,0.2),bty="n",leg=cTab.rf[1,1])
lg1 <- paste(cTab.rf[2,1],"+")
legend("bottomright",inset=c(0.02,0.2),bty="n",leg=lg1,text.col=2)
lg2 <- paste(cTab.rf[1,2],"-")
legend("topleft",inset=c(-0.02,0.2),bty="n",leg=lg2,text.col=2)
legend("topright",inset=c(0.02,0.2),bty="n",leg=cTab.rf[2,2])
lg <- paste("Accuracy:",round(pa.d.rf,2),"%")
legend("right",inset=0.25,leg=lg,x.intersp=0.2,bg="ivory")

ylm <- c(-0.1,1.1); xlm <- c(0,max(prob.d.nt))
xlb <- "Estimated Risk"
plot(1,type="n",ylab="",xlab=xlb,ylim=ylm,xlim=xlm,yaxt="n")
abline(v=co.nt,col=2,lwd=2)
died <- prob.d.nt[d$y==1]
lived <- prob.d.nt[d$y==0]
q1 <- quantile(died,probs=c(0:4)/4)
q0 <- quantile(lived,probs=c(0:4)/4)
wd <- 0.08
clr <- "lemonchiffon"
polygon(c(q1[2],q1[4],q1[4],q1[2],q1[2]),1+wd*c(-1,-1,1,1,-1),col=clr)
points(c(q1[3],q1[3]),1+wd*c(-1,1),type="l")
polygon(c(q0[2],q0[4],q0[4],q0[2],q0[2]),wd*c(-1,-1,1,1,-1),col=clr)
points(c(q0[3],q0[3]),wd*c(-1,1),type="l")
points(prob.d.nt,d$y)
axis(side=2,at=c(0.2,0.8),lab=c("No","Yes"))
mtext(side=3,adj=-0.1,line=0.2,"Drop",cex=0.8)
mtext(side=3,adj=1,line=0.2,paste(n,"Samples"),cex=0.8)
axis(side=4,at=1,lab="Total",font=3,hadj=-0.25,tcl=0)
titl <- paste("Neural Network with",nn,"hidden nodes")
mtext(side=3,adj=0.5,line=1.4,titl,font=2,cex=0.8)
tSum <- table(d$y)
axis(side=4,at=c(0.2,0.8),lab=tSum,hadj=-0.2)
legend("bottomleft",inset=c(-0.02,0.2),bty="n",leg=cTab.nt[1,1])
lg1 <- paste(cTab.nt[2,1],"+")
legend("bottomright",inset=c(0.02,0.2),bty="n",leg=lg1,text.col=2)
lg2 <- paste(cTab.nt[1,2],"-")
legend("topleft",inset=c(-0.02,0.2),bty="n",leg=lg2,text.col=2)
legend("topright",inset=c(0.02,0.2),bty="n",leg=cTab.nt[2,2])
lg <- paste("Accuracy:",round(pa.d.nt,2),"%")
legend("right",inset=0.25,leg=lg,x.intersp=0.2,bg="ivory")

#--------------------------------------------
# split sample randomly into two halves

set.seed(2468)		# make result repeatable
N <- nrow(d)
N1 <- floor(N/2)	# training data sample size
N2 <- N-N1		# test data sample size
trIDs <- sample(c(1:N),
	replace=F,size=N1)	# training record IDs
teIDs <- c(1:N)[-trIDs]		# test record IDs
dTr <- d[trIDs,] 	# training sample
dTe <- d[teIDs,]	# test sample

# logistic reg in train and test samples

dTr.lr <- glm(family=binomial,data=dTr,y~yr+fac+gpa+gen.rel+ht+medu)
prob.dTr.lr <- predict(dTr.lr,type="response")
pred.dTr.lr <- ifelse(prob.dTr.lr>co.lr,1,0)
cTab.dTr.lr <- table(pred.dTr.lr,dTr$y)
addmargins(cTab.dTr.lr)
pa.dTr.lr <- 100*sum(diag(cTab.dTr.lr))/sum(cTab.dTr.lr)
pa.dTr.lr

prob.dTe.lr <- predict(dTr.lr,dTe,type="response")
pred.dTe.lr <- ifelse(prob.dTe.lr>co.lr,1,0)
cTab.dTe.lr <- table(pred.dTe.lr,dTe$y)
addmargins(cTab.dTe.lr)
pa.dTe.lr <- 100*sum(diag(cTab.dTe.lr))/sum(cTab.dTe.lr)
pa.dTe.lr

# rpart in train and test samples

dTr.rp <- rpart(data=dTr,y~yr+fac+gpa+gen.rel+ht+medu,cp=cpval)
printcp(dTr.rp)

prob.dTr.rp <- predict(dTr.rp,type="vector")
pred.dTr.rp <- ifelse(prob.dTr.rp>co.rp,1,0)
cTab.dTr.rp <- table(pred.dTr.rp,dTr$y)
addmargins(cTab.dTr.rp)
pa.dTr.rp <- 100*sum(diag(cTab.dTr.rp))/sum(cTab.dTr.rp)
pa.dTr.rp

prob.dTe.rp <- predict(dTr.rp,dTe)
pred.dTe.rp <- ifelse(prob.dTe.rp>co.rp,1,0)
cTab.dTe.rp <- table(pred.dTe.rp,dTe$y)
addmargins(cTab.dTe.rp)
pa.dTe.rp <- 100*sum(diag(cTab.dTe.rp))/sum(cTab.dTe.rp)
pa.dTe.rp

# randomForest in train and test samples

dTr$yf <- as.factor(dTr$y)
dTe$yf <- as.factor(dTe$y)	# factor outcome for rf

dTr.rf <- randomForest(data=dTr,yf~yr+fac+gpa+gen.rel+ht+medu,
	importance=T,ntree=400)
prob.dTr.rf <- predict(dTr.rf,type="prob")[,2]
pred.dTr.rf <- ifelse(prob.dTr.rf>co.rf,1,0)
cTab.dTr.rf <- table(pred.dTr.rf,dTr$y)
addmargins(cTab.dTr.rf)
pa.dTr.rf <- 100*sum(diag(cTab.dTr.rf))/sum(cTab.dTr.rf)
pa.dTr.rf

prob.dTe.rf <- predict(dTr.rf,dTe,type="prob")[,2]
pred.dTe.rf <- ifelse(prob.dTe.rf>co.rf,1,0)
cTab.dTe.rf <- table(pred.dTe.rf,dTe$yf)
addmargins(cTab.dTe.rf)
pa.dTe.rf <- 100*sum(diag(cTab.dTe.rf))/sum(cTab.dTe.rf)
pa.dTe.rf

# nnet in train and test samples

dTr.nt <- nnet(data=dTr,yf~yr+fac+gpa+gen.rel+ht+medu,
	trace=F,size=nn,rang=0.1,decay=1e-2,maxit=500)
prob.dTr.nt <- predict(dTr.nt,type="raw")
pred.dTr.nt <- ifelse(prob.dTr.nt>co.nt,1,0)
cTab.dTr.nt <- table(pred.dTr.nt,dTr$y)
addmargins(cTab.dTr.nt)
pa.dTr.nt <- 100*sum(diag(cTab.dTr.nt))/sum(cTab.dTr.nt)
pa.dTr.nt

prob.dTe.nt <- predict(dTr.nt,dTe,type="raw")
pred.dTe.nt <- ifelse(prob.dTe.nt>co.nt,1,0)
cTab.dTe.nt <- table(pred.dTe.nt,dTe$yf)
addmargins(cTab.dTe.nt)
pa.dTe.nt <- 100*sum(diag(cTab.dTe.nt))/sum(cTab.dTe.nt)
pa.dTe.nt
#-----------------------------------
# create ROC curves for each method

trueYes <- dTr$y	

sens1 <- NULL		# sensitivity for lr
spec1 <- NULL		# specificity for lr
sens2 <- NULL		# sensitivity for rp
spec2 <- NULL		# specificity for rp
sens3 <- NULL		# sensitivity for rf
spec3 <- NULL		# specificity for rf
sens4 <- NULL		# sensitivity for nt
spec4 <- NULL		# specificity for nt

p10 <- NULL

probs <- c(prob.dTr.lr,prob.dTr.rp,prob.dTr.rf,prob.dTr.nt)
ct.lr <- unique(round(probs,3))
ct.lr <- ct.lr[order(ct.lr)]

for (pi in ct.lr) {
 predictYes1 <- ifelse(prob.dTr.lr>=pi,1,0)
 predictYes2 <- ifelse(prob.dTr.rp>=pi,1,0)
 predictYes3 <- ifelse(prob.dTr.rf>=pi,1,0)
 predictYes4 <- ifelse(prob.dTr.nt>=pi,1,0)

 table(predictYes1,trueYes,useNA="always") -> tt
 sensi <- tt[2,2]/(tt[1,2]+tt[2,2])
 speci <- tt[1,1]/(tt[1,1]+tt[2,1])
 sens1 <- c(sens1,sensi)
 spec1 <- c(spec1,speci)
 table(predictYes2,trueYes,useNA="always") -> tt
 sensi <- tt[2,2]/(tt[1,2]+tt[2,2])
 speci <- tt[1,1]/(tt[1,1]+tt[2,1])
 sens2 <- c(sens2,sensi)
 spec2 <- c(spec2,speci)
 table(predictYes3,trueYes,useNA="always") -> tt
 sensi <- tt[2,2]/(tt[1,2]+tt[2,2])
 speci <- tt[1,1]/(tt[1,1]+tt[2,1])
 sens3 <- c(sens3,sensi)
 spec3 <- c(spec3,speci)
 table(predictYes4,trueYes,useNA="always") -> tt
 sensi <- tt[2,2]/(tt[1,2]+tt[2,2])
 speci <- tt[1,1]/(tt[1,1]+tt[2,1])
 sens4 <- c(sens4,sensi)
 spec4 <- c(spec4,speci)
 p10 <- c(p10,pi)
}
roc1 <- as.data.frame(cbind(p10,1-spec1,sens1))
roc1 <- subset(roc1,V2>0)
roc1 <- rbind(c(0,1,1,0),roc1,c(0,0,0,0))
roc2 <- as.data.frame(cbind(p10,1-spec2,sens2))
roc2 <- subset(roc2,V2>0)
roc2 <- rbind(c(0,1,1,0),roc2,c(0,0,0,0))
roc3 <- as.data.frame(cbind(p10,1-spec3,sens3))
roc3 <- subset(roc3,V2>0)
roc3 <- rbind(c(0,1,1,0),roc3,c(0,0,0,0))
roc4 <- as.data.frame(cbind(p10,1-spec4,sens4))
roc4 <- subset(roc4,V2>0)
roc4 <- rbind(c(0,1,1,0),roc4,c(0,0,0,0))

#windows(6,6)	# Figure 6: Plot comparing ROC curves in training sample

par(mfrow=c(1,1),mar=c(2.5,0.5,0,0.6),mgp=c(1.1,0.2,0),oma=c(0,1.8,2.5,0),las=1,tcl=-0.2)

plot(1,type="n",ylim=c(-0.05,1.05),xlim=c(0,1),yaxs="i",
	xlab="False positive rate",ylab="",cex.lab=1.1,lwd=2)
points(c(0,1),c(0,1),type="l")

clrs <- c(1,2,3,4,"grey60")
points(roc1[,2],roc1[,3],type="l",col=clrs[1],lwd=2)
points(roc2[,2],roc2[,3],type="l",col=clrs[2],lwd=2)
points(roc3[,2],roc3[,3],type="l",col=clrs[3],lwd=2)
points(roc4[,2],roc4[,3],type="l",col=clrs[4],lwd=2)
polygon(c(0,1,1,0,0),c(0,0,1,1,0))

mtext(side=3,adj=-0.08,line=0.2,"Sensitivity")
mtext(side=3,adj=0.5,line=1.3,"ROC Curves",font=2)

titl.roc <- paste("Dropout Rate in Training Sample: n=",N1,sep="")
mtext(side=3,adj=1,line=0.2,titl.roc)

Area <- function(X) {		# function to compute area of a polygon
 X <- rbind(X,X[1,])
 x <- X[,1]
 y <- X[,2]
 lx <- length(x)
 sum((x[2:lx]-x[1:lx-1])*(y[2:lx]+y[1:lx-1]))/2
}
xy1 <- rbind(roc1[,c(2,3)],c(1,0),c(1,1))
area1 <- 2*(-Area(xy1)-0.5)
xy2 <- rbind(roc2[,c(2,3)],c(1,0),c(1,1))
area2 <- 2*(-Area(xy2)-0.5)
xy3 <- rbind(roc3[,c(2,3)],c(1,0),c(1,1))
area3 <- 2*(-Area(xy3)-0.5)
xy4 <- rbind(roc4[,c(2,3)],c(1,0),c(1,1))
area4 <- 2*(-Area(xy4)-0.5)

lg1 <- c(paste("LR",round(100*area1,2)),paste("RP",round(100*area2,2)),
	paste("RF",round(100*area3,2)),paste("NT",round(100*area4,2)))
legend("topleft",leg=lg1,inset=c(0.01,0.01),x.intersp=0.2,lwd=c(2,2,2,2),
	col=clrs,bg="ivory",title="%AUC above diagonal",ncol=2)

x1 <- cTab.dTr.lr[2,1]/sum(cTab.dTr.lr[,1])
y1 <- 1-cTab.dTr.lr[1,2]/sum(cTab.dTr.lr[,2])
points(c(0,x1),c(y1,y1),type="l",lwd=1)
points(c(x1,x1),c(y1,0),type="l",lwd=1)
points(x1,y1,pch=21,bg="grey40",cex=0.8)

x2 <- cTab.dTr.rp[2,1]/sum(cTab.dTr.rp[,1])
y2 <- 1-cTab.dTr.rp[1,2]/sum(cTab.dTr.rp[,2])
points(c(0,x2),c(y2,y2),type="l",col="red",lwd=1)
points(c(x2,x2),c(y2,0),type="l",col="red",lwd=1)
points(x2,y2,pch=21,bg="red",cex=0.8)

x3 <- cTab.dTr.rf[2,1]/sum(cTab.dTr.rf[,1])
y3 <- 1-cTab.dTr.rf[1,2]/sum(cTab.dTr.rf[,2])
points(c(0,x3),c(y3,y3),type="l",col="green",lwd=1)
points(c(x3,x3),c(y3,0),type="l",col="green",lwd=1)
points(x3,y3,pch=21,bg="green",cex=0.8)

x4 <- cTab.dTr.nt[2,1]/sum(cTab.dTr.nt[,1])
y4 <- 1-cTab.dTr.nt[1,2]/sum(cTab.dTr.nt[,2])
points(c(0,x4),c(y4,y4),type="l",col="blue",lwd=1)
points(c(x4,x4),c(y4,0),type="l",col="blue",lwd=1)
points(x4,y4,pch=21,bg="blue",cex=0.8)

lg2 <- round(c(pa.dTr.lr,pa.dTr.rp,pa.dTr.rf,pa.dTr.nt),2)
lg2 <- paste(c("LR ","RP ","RF ","NT "),lg2)
legend("bottomright",leg=lg2,inset=c(0.01,0.01),x.intersp=0.8,pch=21,ncol=2,
	pt.bg=c("grey40",2,3,4),bg="ivory",title="Predictive Accuracy (%)")

# repeat for test sample

trueYes <- dTe$y	

sens1 <- NULL		# sensitivity for lr
spec1 <- NULL		# specificity for lr
sens2 <- NULL		# sensitivity for rp
spec2 <- NULL		# specificity for rp
sens3 <- NULL		# sensitivity for rf
spec3 <- NULL		# specificity for rf
sens4 <- NULL		# sensitivity for nt
spec4 <- NULL		# specificity for nt

p10 <- NULL

probs <- c(prob.dTe.lr,prob.dTe.rp,prob.dTe.rf,prob.dTe.nt)
ct.lr <- unique(round(probs,3))
ct.lr <- ct.lr[order(ct.lr)]

for (pi in ct.lr) {
 predictYes1 <- ifelse(prob.dTe.lr>=pi,1,0)
 predictYes2 <- ifelse(prob.dTe.rp>=pi,1,0)
 predictYes3 <- ifelse(prob.dTe.rf>=pi,1,0)
 predictYes4 <- ifelse(prob.dTe.nt>=pi,1,0)

 table(predictYes1,trueYes,useNA="always") -> tt
 sensi <- tt[2,2]/(tt[1,2]+tt[2,2])
 speci <- tt[1,1]/(tt[1,1]+tt[2,1])
 sens1 <- c(sens1,sensi)
 spec1 <- c(spec1,speci)
 table(predictYes2,trueYes,useNA="always") -> tt
 sensi <- tt[2,2]/(tt[1,2]+tt[2,2])
 speci <- tt[1,1]/(tt[1,1]+tt[2,1])
 sens2 <- c(sens2,sensi)
 spec2 <- c(spec2,speci)
 table(predictYes3,trueYes,useNA="always") -> tt
 sensi <- tt[2,2]/(tt[1,2]+tt[2,2])
 speci <- tt[1,1]/(tt[1,1]+tt[2,1])
 sens3 <- c(sens3,sensi)
 spec3 <- c(spec3,speci)
 table(predictYes4,trueYes,useNA="always") -> tt
 sensi <- tt[2,2]/(tt[1,2]+tt[2,2])
 speci <- tt[1,1]/(tt[1,1]+tt[2,1])
 sens4 <- c(sens4,sensi)
 spec4 <- c(spec4,speci)
 p10 <- c(p10,pi)
}
roc1 <- as.data.frame(cbind(p10,1-spec1,sens1))
roc1 <- subset(roc1,V2>0)
roc1 <- rbind(c(0,1,1,0),roc1,c(0,0,0,0))
roc2 <- as.data.frame(cbind(p10,1-spec2,sens2))
roc2 <- subset(roc2,V2>0)
roc2 <- rbind(c(0,1,1,0),roc2,c(0,0,0,0))
roc3 <- as.data.frame(cbind(p10,1-spec3,sens3))
roc3 <- subset(roc3,V2>0)
roc3 <- rbind(c(0,1,1,0),roc3,c(0,0,0,0))
roc4 <- as.data.frame(cbind(p10,1-spec4,sens4))
roc4 <- subset(roc4,V2>0)
roc4 <- rbind(c(0,1,1,0),roc4,c(0,0,0,0))

#windows(6,6)	# Figure 7: Plot comparing ROC curves

par(mfrow=c(1,1),mar=c(2.5,0.5,0,0.6),mgp=c(1.1,0.2,0),oma=c(0,1.8,2.5,0),las=1,tcl=-0.2)

plot(1,type="n",ylim=c(-0.05,1.05),xlim=c(0,1),yaxs="i",
	xlab="False positive rate",ylab="",cex.lab=1.1,lwd=2)
points(c(0,1),c(0,1),type="l")

clrs <- c(1,2,3,4,"grey60")
points(roc1[,2],roc1[,3],type="l",col=clrs[1],lwd=2)
points(roc2[,2],roc2[,3],type="l",col=clrs[2],lwd=2)
points(roc3[,2],roc3[,3],type="l",col=clrs[3],lwd=2)
points(roc4[,2],roc4[,3],type="l",col=clrs[4],lwd=2)
polygon(c(0,1,1,0,0),c(0,0,1,1,0))

mtext(side=3,adj=-0.08,line=0.2,"Sensitivity")
mtext(side=3,adj=0.5,line=1.3,"ROC Curves",font=2)

titl.roc <- paste("Dropout Rate in Test Sample: n=",N2,sep="")
mtext(side=3,adj=1,line=0.2,titl.roc)

xy1 <- rbind(roc1[,c(2,3)],c(1,0),c(1,1))
area1 <- 2*(-Area(xy1)-0.5)
xy2 <- rbind(roc2[,c(2,3)],c(1,0),c(1,1))
area2 <- 2*(-Area(xy2)-0.5)
xy3 <- rbind(roc3[,c(2,3)],c(1,0),c(1,1))
area3 <- 2*(-Area(xy3)-0.5)
xy4 <- rbind(roc4[,c(2,3)],c(1,0),c(1,1))
area4 <- 2*(-Area(xy4)-0.5)

lg1 <- c(paste("LR",round(100*area1,2)),paste("RP",round(100*area2,2)),
	paste("RF",round(100*area3,2)),paste("NT",round(100*area4,2)))
legend("topleft",leg=lg1,inset=c(0.01,0.01),x.intersp=0.2,lwd=c(2,2,2,2),
	col=clrs,bg="ivory",title="%AUC above diagonal",ncol=2)

x1 <- cTab.dTe.lr[2,1]/sum(cTab.dTe.lr[,1])
y1 <- 1-cTab.dTe.lr[1,2]/sum(cTab.dTe.lr[,2])
points(c(0,x1),c(y1,y1),type="l",lwd=1)
points(c(x1,x1),c(y1,0),type="l",lwd=1)
points(x1,y1,pch=21,bg="grey40",cex=0.8)

x2 <- cTab.dTe.rp[2,1]/sum(cTab.dTe.rp[,1])
y2 <- 1-cTab.dTe.rp[1,2]/sum(cTab.dTe.rp[,2])
points(c(0,x2),c(y2,y2),type="l",col="red",lwd=1)
points(c(x2,x2),c(y2,0),type="l",col="red",lwd=1)
points(x2,y2,pch=21,bg="red",cex=0.8)

x3 <- cTab.dTe.rf[2,1]/sum(cTab.dTe.rf[,1])
y3 <- 1-cTab.dTe.rf[1,2]/sum(cTab.dTe.rf[,2])
points(c(0,x3),c(y3,y3),type="l",col="green",lwd=1)
points(c(x3,x3),c(y3,0),type="l",col="green",lwd=1)
points(x3,y3,pch=21,bg="green",cex=0.8)

x4 <- cTab.dTe.nt[2,1]/sum(cTab.dTe.nt[,1])
y4 <- 1-cTab.dTe.nt[1,2]/sum(cTab.dTe.nt[,2])
points(c(0,x4),c(y4,y4),type="l",col="blue",lwd=1)
points(c(x4,x4),c(y4,0),type="l",col="blue",lwd=1)
points(x4,y4,pch=21,bg="blue",cex=0.8)

lg2 <- round(c(pa.dTe.lr,pa.dTe.rp,pa.dTe.rf,pa.dTe.nt),2)
lg2 <- paste(c("LR ","RP ","RF ","NT "),lg2)
legend("bottomright",leg=lg2,inset=c(0.01,0.01),x.intersp=0.8,pch=21,ncol=2,
	pt.bg=c("grey40",2,3,4),bg="ivory",title="Predictive Accuracy (%)")
#-------------------------------------------------END
