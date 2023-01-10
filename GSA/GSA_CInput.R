#================================
# Global sensitivity analysis using Sobols method in R using sensitivity package
#================================
library(sensitivity)
library(boot)
library(ggplot2)
library(doParallel)
library(parallel)
library(foreach)

# Set working directory where all the files are from GitHub repository were saved
wkdir <- "~/SIR-Bayesian-Calibration-of-IPCC-Model/"

setwd(wkdir)
Driver="Region"
value="West"
getwd()

source("./RCodes/RunIPCC_GSA.R")

# read prior distribution from a csv file
paramBounds <- read.csv("./InputData/cal_parameters_Cinput_V1.csv", stringsAsFactors = FALSE)

#paramBounds[nrow(paramBounds)+1,]=c("cin", 1.000, "cinput multiplier", 0.5, 1.500, 1)

# names of parameters that are allowed to vary
varSI       <- paramBounds$Parameter



nParams     <- length(varSI) # add cinput as a Param

# Random sampling for GSA
set.seed(10)

# sample size (10 used for illustration purposes)
N <- 100 # (1024 used in Gurung et al., 2020)

paramBounds[1,4]=3.036
paramBounds[1,5]=3.036

paramBounds[2,4]=2.075
paramBounds[2,5]=2.075

# Sobols method required 2 matrix
m1 = matrix(runif(nParams*N), nrow=N);
m2 = matrix(runif(nParams*N), nrow=N);
M1 <- matrix(0, nrow = N, ncol = nParams)
M2 <- matrix(0, nrow = N, ncol = nParams)


# transform standard uniform to prior distribution 
for(i in 1:nParams){
     pos <- which(paramBounds$Parameter == varSI[i])
     lower <- paramBounds[pos, "lower"]
     upper <- paramBounds[pos, "upper"]
     M1[, i] <- qunif(m1[, i], min = lower, max = upper)
     M2[, i] <- qunif(m2[, i], min = lower, max = upper)
}
X1 = data.frame(M1)
X2 = data.frame(M2)
names(X1) <- varSI
names(X2) <- varSI

# choose a sensitivity method of your choice from the sensitivity package in R.
# see documantation for available options.
#    - soboljansen is used for ilustration here

si_obj2 <- soboljansen(model = NULL, X1 = X1, X2 = X2, nboot = 100)
X <- si_obj2$X
n <- nrow(X)
X <- cbind("SampleID" = 1:nrow(X), X)


# End of sample generation for GSA

# Run the model and calculate log-likelihood
#    - likelihoods were calculated assuming that the error (modeled - mesasured)
#      are iid 





Lkhood <- NULL

ncores=detectCores(logical=T)-2
cl=makeCluster(ncores)
registerDoParallel(cl)

Lkhood=foreach(i=1:nrow(X), 
               .combine = rbind, 
               .packages = c("parallel", 
                             "doParallel", 
                             "tidyverse"))%dopar%(RunIPCC(i, X, Driver ,value))
stopCluster(cl)

# code for non-paralleled run

# for(i in 1:nrow(X)){
# #i=1        
#     print(paste(date(), "Case: ", i))
#     lkd <- RunIPCC(i, X)
#     #View(lkd)
#     Lkhood <- rbind(Lkhood, lkd)
# }
#Lkhood%>%mutate(loglik=ifelse(loglik==-Inf, NA, loglik))%>%group_by(SampleID)%>%summarise(loglik=mean(loglik, na.rm = T))->Lkhood1

Lkhood->Lkhood1

setwd(wkdir)
#==============================
# Calculate First-order and Total global sensitivity indices
#==============================
si_obj2_llkhd <- tell(x = si_obj2, y = Lkhood1$loglik)
singleSI <-si_obj2_llkhd$S # individual sensitivity indices
singleSI$parmas <- row.names(singleSI)
names(singleSI) <- c("singsi", "bias", "std.error", "singsi.lci", "singsi.uci", "params")
singleSI <- singleSI[order(-singleSI$singsi), ]
rownames(singleSI) <- 1:nrow(singleSI)
singleSI$ID <- 1:nrow(singleSI)
singleSI <- singleSI[, c("ID", "params", "singsi", "singsi.lci", "singsi.uci")]
SingPlot <- ggplot(singleSI, aes(x = reorder(params, -ID), y = singsi, ymax=singsi.uci, ymin=singsi.lci))+ 
        xlab("Parameters") + ylab("First Order Sensitivity Index") + geom_errorbar(width=0.2, size=1, color="black")+
        geom_bar(stat='identity', fill="grey", alpha=0.70 ) +
        coord_flip() + theme_bw()
SingPlot




         

#==============================
# Total-Order sensitivity indices
#==============================
totalSI <- si_obj2_llkhd$T # total sensitivity indices
totalSI$parmas <- row.names(totalSI)
names(totalSI) <- c("totsi", "bias", "std.error", "totsi.lci", "totsi.uci", "params")
totalSI <- totalSI[order(-totalSI$totsi), ]
rownames(totalSI) <- 1:nrow(totalSI)
totalSI$ID <- 1:nrow(totalSI)
totalSI <- totalSI[, c("ID", "params", "totsi", "totsi.lci", "totsi.uci")]

singleSI%>%select(-ID)%>%left_join(totalSI%>%select(-ID), by="params")->AllSI
AllSI$Driver=Driver
AllSI$DriverValue=value
write.csv(AllSI, paste0("./Output/GSA/Data/GSA_SJ_", Driver, "_", value, 
                        ".csv"), row.names = F)

 
TotSIPlot <- ggplot(totalSI, aes(x = reorder(params, -ID), y = totsi, ymax=totsi.uci, ymin=totsi.lci))+ 
        xlab("Parameters") + ylab("Total Sensitivity Index") + 
        geom_errorbar(width=0.2, size=1, color="black")+
        geom_bar(stat='identity', fill="grey", alpha=0.70) +
        coord_flip() + theme_bw()

AllSI%>%ggplot()+
        geom_bar(aes(x=reorder(params, totsi), y=singsi), stat='identity', fill="black", alpha=0.7)+
        geom_bar(aes(x=reorder(params, totsi), y=totsi), stat='identity', fill="grey", alpha=0.7)+
        geom_errorbar(aes(x=reorder(params, totsi), 
                          y=totsi, ymax=totsi.uci, ymin=totsi.lci), width=0.2)+
        coord_flip() + theme_bw()+xlab("")+ylab("")->ALLSIPlot
        
ALLSIPlot



ggsave(ALLSIPlot,file=paste0("./Output/GSA/Fig/GSA_SJ", Driver, "_", value, 
                             ".jpg"), device = "jpg", width=7, height = 3, unit="in")





###-------------------Fast99----------------- ###


factors=c("tillfac_FT", "tillfac_RT", "wfacirri", 
          "k10", "k20", "k30", "k40", "k50", 
          "f1", "f2", "f3", "f5", "f6", "f7", "f8",
          "tmax", "topt", "plig", "cin")

ls_tillfac_FT=list(min=3.026, max=3.026)
ls_tillfac_RT=list(min=2.075, max=2.075)
ls_wfacirri=list(min=paramBounds[3,4], max=paramBounds[3,5])
ls_k10=list(min=paramBounds[4,4], max=paramBounds[4,5])
ls_k20=list(min=paramBounds[5,4], max=paramBounds[5,5])
ls_k30=list(min=paramBounds[6,4], max=paramBounds[6,5])
ls_k40=list(min=paramBounds[7,4], max=paramBounds[7,5])
ls_k50=list(min=paramBounds[8,4], max=paramBounds[8,5])
ls_f1=list(min=paramBounds[9,4], max=paramBounds[9,5])
ls_f2=list(min=paramBounds[10,4], max=paramBounds[10,5])
ls_f3=list(min=paramBounds[11,4], max=paramBounds[11,5])
ls_f5=list(min=paramBounds[12,4], max=paramBounds[12,5])
ls_f6=list(min=paramBounds[13,4], max=paramBounds[13,5])
ls_f7=list(min=paramBounds[14,4], max=paramBounds[14,5])
ls_f8=list(min=paramBounds[15,4], max=paramBounds[15,5])
ls_tmax=list(min=paramBounds[16,4], max=paramBounds[16,5])
ls_topt=list(min=paramBounds[17,4], max=paramBounds[17,5])
ls_plig=list(min=paramBounds[18,4], max=paramBounds[18,5])
ls_cin=list(min=paramBounds[19,4], max=paramBounds[19,5])







fast99_obj=fast99(model = NULL, 
                  factors=factors, 
                  n=200, M=4,
                  q="qunif", 
                  q.arg=list(ls_tillfac_FT,ls_tillfac_RT,ls_wfacirri,
                                                                         ls_k10,ls_k20,ls_k30, ls_k40, ls_k50,
                                                                         ls_f1, ls_f2, ls_f3, ls_f5,ls_f6, ls_f7, ls_f8,
                                                                         ls_tmax, ls_topt, ls_plig, ls_cin))

X2=fast99_obj$X

X2 <- cbind("SampleID" = 1:nrow(X2), X2)

Lkhood2=NULL
#ncores=detectCores()-2
cl=makeCluster(ncores)
registerDoParallel(cl)

Lkhood2=foreach(i=1:nrow(X2), 
               .combine = rbind, 
               .packages = c("parallel", 
                             "doParallel", 
                             "tidyverse"))%dopar% RunIPCC(i, X2, Driver, value)
stopCluster(cl)



res=tell(fast99_obj, Lkhood2$loglik)
#saveRDS(res)
plot(res)
res$V

res_df <- data.frame(cbind(res$D1 / res$V, 1 - res$Dt / res$V ))
colnames(res_df)=c("Main", "Interactions")
res_df$Params <- colnames(res$X)
res_df$Driver=Driver
res_df$DriverValue=value
res_df%>%ggplot()+
        geom_bar(aes(x=reorder(Params, Interactions), y=Main),stat='identity', 
                 fill="black") +
        geom_bar(aes(x=reorder(Params, Interactions), y=Interactions),
                 stat='identity', fill="grey",alpha=0.7 ) +
        coord_flip() + theme_bw()+xlab("")+ylab("")->fast99Plot
fast99Plot

write.csv(res_df, paste0("./Output/GSA/Data/GSA_Fast99_", Driver, "_", value, 
                        ".csv"), row.names = F)
ggsave(fast99Plot,file=paste0("./Output/GSA/Fig/GSA_Fast99", Driver, "_", value, 
                             ".jpg"), device = "jpg", width=7, height = 3, unit="in")




