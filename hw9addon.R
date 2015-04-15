#Chad Foley
#4/14/15
#Stat 261
#Assignment 9 add on

sampData = rnorm(20, mean = 5, sd = 2) #var(sampData) = 3.021603

thetaTrue = 4
alpha = 0.1
B = 100
reps = 100
data = matrix(NA, reps, 10) #10 columns
for (i in 1:reps){
    bsVar = rep(NA, B)
    #sampI = sample(sampData, size = length(sampData), replace = TRUE)
    for (j in 1:B){
        #bsSamp = sample(sampI, size = length(sampI), replace = TRUE)
        #bsSamp = sample(sampData, size = length(sampData), replace = TRUE)
        bsSamp = rnorm(20, mean = 5, sd = 2)
        bsVar[j] = var(bsSamp)
    }
    #thetaHat
    data[i,1] = mean(bsVar)
    #percentile CI
    data[i,2] = quantile(bsVar, alpha/2)[[1]]
    data[i,3] = quantile(bsVar, 1-alpha/2)[[1]]
    #percentile CI include thetaTrue?
    ifelse(data[i,2] <= thetaTrue && data[i,3] >= thetaTrue, data[i,4] <- 1, data[i,4] <- 0)
    
    #bias corrected CI
    data[i,5] = quantile(bsVar, pnorm(qnorm(alpha/2) + 2*qnorm(length(bsVar[bsVar > mean(bsVar)])/length(bsVar))))[[1]]
    data[i,6] = quantile(bsVar, pnorm(-qnorm(alpha/2) + 2*qnorm(length(bsVar[bsVar > mean(bsVar)])/length(bsVar))))[[1]]
    #bias corrected CI include thetaTrue?
    ifelse(data[i,5] <= thetaTrue && data[i,6] >= thetaTrue, data[i,7] <- 1, data[i,7] <- 0)
    
    #calculate Zknot (for simplicity) and a
    Zknot = qnorm(length(bsVar[bsVar > mean(bsVar)])/length(bsVar))
    bsVarMinus = bsVar[1:length(bsVar)-1]
    bsVarBarMinus = mean(bsVarMinus)
    aNum = 0
    aDenom = 0
    for (ii in 1:length(bsVarMinus)){
        aNum = aNum + (bsVarBarMinus - bsVarMinus[ii])**3
        aDenom = aDenom + (bsVarBarMinus - bsVarMinus[ii])**2
    }
    a = aNum/(6*sqrt(aDenom))
    
    #bias corrected accelerated CI : Zknot = qnorm(kboot)
    data[i,8] = quantile(bsVar, pnorm(Zknot + ((qnorm(alpha/2) + Zknot) / (1-a*(qnorm(alpha/2) + Zknot)))))[[1]]
    data[i,9] = quantile(bsVar, pnorm(Zknot + ((qnorm(1-alpha/2) + Zknot) / (1-a*(qnorm(1-alpha/2) + Zknot)))))[[1]]
    #bias corrected accelerated CI include thetaTrue?
    ifelse(data[i,8] <= thetaTrue && data[i,9] >= thetaTrue, data[i,10] <- 1, data[i,10] <- 0)
    ###ifelse(min(data[i,8],data[i,9]) <= thetaTrue && max(data[i,8],data[i,9]) >= thetaTrue, data[i,11] <- 1, data[i,11] <- 0)
}

colnames(data) = c('thetaHat', 'percLow', 'percUp', 'percContainsTrue', 'bcLow', 'bcUp', 'bcContainsTrue', 'bcaLow', 'bcaUp', 'bcaContainsTrue')
#B1000VarParam = data

