#Chad Foley
#4/6/15
#Stat 261
#Assignment 9

#pnorm: takes sd from mean, returns percentile  ===  capPsi
#qnorm: takes percentile, returns sd from mean  ===  capPsiInverse

alpha = 0.05
thetaTrue = 0.01/0.017

B = 1000
reps = 1000  #1000
thetaVec = rep(NA, B)
data = matrix(NA, reps, 10)
for (i in 1:reps){
    Xsamp = sample(x = c(0,1), size = 11037, replace = TRUE, prob = c(0.99, 0.01))
    Ysamp = sample(x = c(0,1), size = 11034, replace = TRUE, prob = c(0.983, 0.017))
    for (j in 1:B){
        XBsSamp = sum(sample(Xsamp, size = length(Xsamp), replace = TRUE))/11037
        YBsSamp = sum(sample(Ysamp, size = length(Ysamp), replace = TRUE))/11034
        thetaVec[j] = XBsSamp / YBsSamp
    }
    #thetaHat
    data[i,1] = mean(thetaVec)
    #percentile CI
    data[i,2] = quantile(thetaVec, alpha/2)[[1]]
    data[i,3] = quantile(thetaVec, 1-alpha/2)[[1]]
    #percentile CI include thetaTrue?
    ifelse(data[i,2] <= thetaTrue && data[i,3] >= thetaTrue, data[i,4] <- 1, data[i,4] <- 0)
    
    #bias corrected CI
    data[i,5] = quantile(thetaVec, pnorm(qnorm(alpha/2) + 2*qnorm(length(thetaVec[thetaVec > mean(thetaVec)])/length(thetaVec))))[[1]]
    data[i,6] = quantile(thetaVec, pnorm(-qnorm(alpha/2) + 2*qnorm(length(thetaVec[thetaVec > mean(thetaVec)])/length(thetaVec))))[[1]]
    #bias corrected CI include thetaTrue?
    ifelse(data[i,5] <= thetaTrue && data[i,6] >= thetaTrue, data[i,7] <- 1, data[i,7] <- 0)
    
    #calculate Zknot (for simplicity) and a
    Zknot = qnorm(length(thetaVec[thetaVec > mean(thetaVec)])/length(thetaVec))
    thetaVecMinus = thetaVec[1:length(thetaVec)-1]
    thetaBarMinus = mean(thetaVecMinus)
    aNum = 0
    aDenom = 0
    for (ii in 1:length(thetaVecMinus)){
        aNum = aNum + (thetaBarMinus - thetaVecMinus[ii])**3
        aDenom = aDenom + (thetaBarMinus - thetaVecMinus[ii])**2
    }
    a = aNum/(6*sqrt(aDenom))
    
    #bias corrected accelerated CI : Zknot = qnorm(kboot)
    data[i,8] = quantile(thetaVec, pnorm(Zknot + ((qnorm(alpha/2) + Zknot) / (1-a*(qnorm(alpha/2) + Zknot)))))[[1]]
    data[i,9] = quantile(thetaVec, pnorm(Zknot + ((qnorm(1-alpha/2) + Zknot) / (1-a*(qnorm(1-alpha/2) + Zknot)))))[[1]]
    #bias corrected accelerated CI include thetaTrue?
    ifelse(data[i,8] <= thetaTrue && data[i,9] >= thetaTrue, data[i,10] <- 1, data[i,10] <- 0)
}

colnames(data) = c('thetaHat', 'percLow', 'percUp', 'percContainsTrue', 'bcLow', 'bcUp', 'bcContainsTrue', 'bcaLow', 'bcaUp', 'bcaContainsTrue')

B1000 = data









