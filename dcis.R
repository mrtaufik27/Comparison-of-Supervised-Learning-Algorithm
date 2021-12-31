# Democratic confidence intervals for linear and logistic models

# linear model

lm.dci <- function(dat,yID,xIDs) {
 y <- dat[,yID]
 xs <- as.data.frame(dat[,xIDs])
 np <- length(xIDs)

 for (j in c(1:np)) {		# fit model with weighted sum contrasts
  nj <- length(unique(xs[,j]))
  nj1 <- tapply(xs[,j],xs[,j],length)
  rho <- 0*(1:nj)
  for (i in c(1:nj)) {
   rho[i] <- nj1[i]/sum(nj1)
  }
  D1 <- rbind(rho,cbind(diag(nj-1),0))
  C1 <- solve(D1)
  C <- C1[,-1]
  xs[,j] <- as.factor(xs[,j])
  contrasts(xs[,j]) <- C
 }
 lm(y~.,data=xs) -> mod
 summary(mod) -> rez

 for (j in c(1:np)) {		# repeat with last two levels reversed
  nj <- length(unique(xs[,j]))
  nj1 <- tapply(xs[,j],xs[,j],length)
  rho <- 0*(1:nj)
  for (i in c(1:nj)) {
   rho[i] <- nj1[i]/sum(nj1)
  }
  D1 <- rbind(rho,cbind(diag(nj-1),0))
  D1[nj,nj-1] <- 0
  D1[nj,nj] <- 1
  C1 <- solve(D1)
  C <- C1[,-1]
  xs[,j] <- as.factor(xs[,j])
  contrasts(xs[,j]) <- C
 }
 lm(y~.,data=xs) -> mod.a
 summary(mod.a) -> rez.a

 tCV <- qt(0.975,rez$df[2])

 cfs <- rez$coef		# assemble coefficients and SEs
 cfs.a <- rez.a$coef  
 cf <- NULL
 se <- NULL
 mAdj <- NULL
 cilb <- NULL
 ciub <- NULL
 npos <- 0
 mean1 <- mean(y,rm.na=T)
 for (j in c(1:np)) {
  nj <- length(unique(xs[,j]))
  rhoj <- 0*(1:nj)
  njj <- tapply(xs[,j],xs[,j],length)
  for (i in c(1:nj)) {
   rhoj[i] <- njj[i]/sum(njj)
  }
  cfj <- c(cfs[npos+c(2:nj),1],cfs.a[npos+nj,1])
  sej <- c(cfs[npos+c(2:nj),2],cfs.a[npos+nj,2])
  mAdjj <- mean1+cfj-sum(cfj*rhoj)
  cilbj <- mAdjj-tCV*sej
  ciubj <- mAdjj+tCV*sej
  cf <- c(cf,cfj)
  se <- c(se,sej)
  mAdj <- c(mAdj,mAdjj)
  cilb <- c(cilb,cilbj)
  ciub <- c(ciub,ciubj)
  npos <- npos+nj-1
 }
 cbind(cf,se,mAdj,cilb,ciub)
}

# logistic model with case-by-case data

glm.dci <- function(dat,yID,xIDs,reverse=FALSE,delta) {
 y <- dat[,yID]
 if (reverse==TRUE) y <- 1-dat[,yID]
 xs <- as.data.frame(dat[,xIDs])
 np <- length(xIDs)

 for (j in c(1:np)) {		# fit model with weighted sum contrasts
  nj <- length(unique(xs[,j]))
  nj1 <- tapply(xs[,j],xs[,j],length)
  rho <- 0*(1:nj)
  for (i in c(1:nj)) {
   rho[i] <- nj1[i]/sum(nj1)
  }
  D1 <- rbind(rho,cbind(diag(nj-1),0))
  C1 <- solve(D1)
  C <- C1[,-1]
  xs[,j] <- as.factor(xs[,j])
  contrasts(xs[,j]) <- C
 }
 glm(y~.,family=binomial,data=xs) -> mod
 summary(mod) -> rez

 for (j in c(1:np)) {		# repeat with last two levels reversed
  nj <- length(unique(xs[,j]))
  nj1 <- tapply(xs[,j],xs[,j],length)
  rho <- 0*(1:nj)
  for (i in c(1:nj)) {
   rho[i] <- nj1[i]/sum(nj1)
  }
  D1 <- rbind(rho,cbind(diag(nj-1),0))
  D1[nj,nj-1] <- 0
  D1[nj,nj] <- 1
  C1 <- solve(D1)
  C <- C1[,-1]
  xs[,j] <- as.factor(xs[,j])
  contrasts(xs[,j]) <- C
 }
 glm(family=binomial,y~.,data=xs) -> mod.a
 summary(mod.a) -> rez.a

 cf <- rez$coef			# assemble coefficients and SEs
 cf.a <- rez.a$coef  
 cfs <- NULL
 ses <- NULL
 npos <- 0
 for (j in c(1:np)) {
  nj <- length(unique(xs[,j]))
  cfs <- c(cfs,cf[npos+c(2:nj),1],cf.a[npos+nj,1])
  ses <- c(ses,cf[npos+c(2:nj),2],cf.a[npos+nj,2])
  npos <- npos+nj-1
 }
 
 meanPc <- mean(100*y)		# create adjusted percents & their CIs
 k <- -log(100/meanPc-1)
 if (np==1) k <- cf[1]
 pcs <- NULL
 cilbs <- NULL
 ciubs <- NULL
 npos <- 0
 for (j in c(1:np)) {
  nj <- length(unique(xs[,j]))
  cfj <- cfs[npos+c(1:nj)]
  sej <- ses[npos+c(1:nj)]
  nj1 <- tapply(xs[,j],xs[,j],length)
  rho <- 0*(1:nj)
  for (i in c(1:nj)) {
   rho[i] <- nj1[i]/sum(nj1)
  }
  dd <- delta			# Marquardt damping constant
  epsilon <- 0.00005		# convergence criterion
  nit <- 20 			# maximum number of iterations
  a0 <- 1			# initial value of constant
  a1 <- 1
  it <- 0
  aDiff <- 1
  while ( (abs(aDiff)>epsilon) && ((it <- it+1) < nit) ) {
   adjPc <- ifelse(cfj<=0,100/(1+exp(-k-cfj)),100/(1+exp(-k-a1*cfj)))
   expCoef <- ifelse(cfj<=0,0,exp(-k-a1*cfj))
   F0 <- sum((adjPc/100)*rho) - meanPc/100
   DF0 <- sum(rho*(adjPc/100)^2*expCoef*cfj)
   a1 <- a0-dd*(F0/DF0)
   aDiff <- a1-a0
   a0 <- a1
  }
  meanj <- sum(adjPc*nj1)/sum(nj1)
  DF <- meanPc/meanj
  adjPc <- adjPc*DF
  pcs <- c(pcs,adjPc)
  sej <- ses[npos+c(1:nj)]
  cilbs <- c(cilbs,DF*ifelse(cfj<=0,100/(1+exp(-k-cfj+1.96*sej)),
	100/(1+exp(-k-a1*cfj+1.96*sej))))
  ciubs <- c(ciubs,DF*ifelse(cfj<=0,100/(1+exp(-k-cfj-1.96*sej)),
	100/(1+exp(-k-a1*cfj-1.96*sej))))
  npos <- npos+nj
 }
 if (reverse==TRUE) {
  cfs <- -cfs
  pcs <- 100-pcs
  zz1 <- cilbs
  zz2 <- ciubs
  cilbs <- 100-zz2
  ciubs <- 100-zz1
 }
 cbind(cfs,ses,pcs,cilbs,ciubs)
}

# logistic model with data grouped as (n0,n1) frequencies

glm.dcig <- function(dat,yIDs,xIDs,reverse=FALSE,delta) {
 if (reverse==TRUE) yIDs <- yIDs[c(2,1)]
 y1 <- dat[,yIDs[1]]
 y0 <- dat[,yIDs[2]]
 datx <- dat[,xIDs]
 np <- length(xIDs)

 for (j in c(1:np)) {		# fit model with weighted sum contrasts
  nj <- length(unique(datx[,j]))
  njTot <- tapply(y0+y1,datx[,j],sum)
  rho <- njTot/sum(njTot)
  D1 <- rbind(rho,cbind(diag(nj-1),0))
  C1 <- solve(D1)
  C <- C1[,-1]
  datx[,j] <- as.factor(datx[,j])
  contrasts(datx[,j]) <- C
 }
 glm(cbind(y1,y0)~.,family=binomial,data=datx) -> mod
 summary(mod) -> rez

 for (j in c(1:np)) {		# repeat with last two levels reversed
  nj <- length(unique(datx[,j]))
  njTot <- tapply(y0+y1,datx[,j],sum)
  rho <- njTot/sum(njTot)
  D1 <- rbind(rho,cbind(diag(nj-1),0))
  D1[nj,nj-1] <- 0
  D1[nj,nj] <- 1
  C1 <- solve(D1)
  C <- C1[,-1]
  datx[,j] <- as.factor(datx[,j])
  contrasts(datx[,j]) <- C
 }
 glm(cbind(y1,y0)~.,family=binomial,data=datx) -> mod.a
 summary(mod.a) -> rez.a

 cf <- rez$coef			# assemble coefficients and SEs
 cf.a <- rez.a$coef  
 cfs <- NULL
 ses <- NULL
 npos <- 0
 for (j in c(1:np)) {
  nj <- length(unique(datx[,j]))
  cfs <- c(cfs,cf[npos+c(2:nj),1],cf.a[npos+nj,1])
  ses <- c(ses,cf[npos+c(2:nj),2],cf.a[npos+nj,2])
  npos <- npos+nj-1
 }
 
 meanPc <- 100*sum(y1)/sum(y1+y0) # create adjusted percents & their CIs
 k <- -log(100/meanPc-1)
 if (np==1) k <- cf[1]
 pcs <- NULL
 cilbs <- NULL
 ciubs <- NULL
 npos <- 0
 for (j in c(1:np)) {
  nj <- length(unique(datx[,j]))
  cfj <- cfs[npos+c(1:nj)]
  sej <- ses[npos+c(1:nj)]
  njTot <- tapply(y0+y1,datx[,j],sum)
  rho <- njTot/sum(njTot)
  dd <- delta			# Marquardt damping constant
  epsilon <- 0.00005		# convergence criterion
  nit <- 20 			# maximum number of iterations
  a0 <- 1			# initial value of constant
  a1 <- 1
  it <- 0
  aDiff <- 1
  while ( (abs(aDiff)>epsilon) && ((it <- it+1) < nit) ) {
   adjPc <- ifelse(cfj<=0,100/(1+exp(-k-cfj)),100/(1+exp(-k-a1*cfj)))
   expCoef <- ifelse(cfj<=0,0,exp(-k-a1*cfj))
   F0 <- sum((adjPc/100)*rho) - meanPc/100
   DF0 <- sum(rho*(adjPc/100)^2*expCoef*cfj)
   a1 <- a0-dd*(F0/DF0)
   aDiff <- a1-a0
   a0 <- a1
  }
  meanj <- sum(adjPc*rho)
  DF <- meanPc/meanj
  adjPc <- adjPc*DF
  pcs <- c(pcs,adjPc)
  sej <- ses[npos+c(1:nj)]
  cilbs <- c(cilbs,DF*ifelse(cfj<=0,100/(1+exp(-k-cfj+1.96*sej)),
	100/(1+exp(-k-a1*cfj+1.96*sej))))
  ciubs <- c(ciubs,DF*ifelse(cfj<=0,100/(1+exp(-k-cfj-1.96*sej)),
	100/(1+exp(-k-a1*cfj-1.96*sej))))
  npos <- npos+nj
 }
 if (reverse==TRUE) {
  cfs <- -cfs
  pcs <- 100-pcs
  zz1 <- cilbs
  zz2 <- ciubs
  cilbs <- 100-zz2
  ciubs <- 100-zz1
 }
 cbind(cfs,ses,pcs,cilbs,ciubs)
}

#-----------------end