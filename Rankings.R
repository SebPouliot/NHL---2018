##############################################
#First remove all dataset from memory
rm(list = ls())

setwd("D:/Box Sync/Sports rankings/Hockey/2014/")

library(stringr)
library(boot)
library(matrixStats)
library(snow)
library(Rlab)
library(gtable)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(XML)

start1 <- Sys.time()

set.seed(123457)

#Number of loops and number of cores
n.rep <- 1000 #for simulating season
n.boot <- 2000 #for bootstrap on ranking
n.core <- 6

####################################
######### Read the data ############
####################################

source("Data.R")

#################################
######## Home team wins #########
#################################

eq.reg_w <- as.formula(paste("win", paste(paste(home.l,collapse="+"), paste(visitor.l,collapse="+"), sep="+"), sep="~ -1 +"))

reg_w <- glm(eq.reg_w, data=dta, family=binomial(link=logit), weights=weight)
#summary(reg_w)
rm(eq.reg_w)

##############################################
######## The game goes into overtime #########
##############################################

eq.reg_ot <- as.formula(paste("OT", paste(paste(home.l,collapse="+"), paste(visitor.l,collapse="+"), sep="+"), sep="~ -1 +"))

reg_ot <- glm(eq.reg_ot, data=dta, family=binomial(link=logit))
#summary(reg_ot)
rm(eq.reg_ot)

####################################
######## Calculate ranking #########
####################################

cl<-makeSOCKcluster(n.core)
clusterEvalQ(cl,source("Ranking_func_snow.R"))
clusterExport(cl, c("dta", "n.boot", "home.l", "visitor.l"))
results_temp <- parLapply(cl, 1:n.boot, function(i){bs_snow(dta)})
stopCluster(cl)
results <- rbind(results_temp[[1]], results_temp[[2]])
for (j in 3:n.boot){
  results <- rbind(results, results_temp[[j]])
}
results <- data.frame(results)
rm(results_temp, j, cl)

dta_boot <- data.frame(array(NA,c(30,5)))
rownames(dta_boot) <- unique(dta$home)
colnames(dta_boot) <- c("Original", "Mean", "s.e.","min_ci", "max_ci")

source("Exp_pts.R")

dta_boot[,2] <- colMeans(results)
dta_boot[,3] <- colSds(as.matrix(results))
for (l in 1:30){
  dta_boot[l,1] <- points[l, "Original"]
  dta_boot[l,4] <- quantile(results[,l], 0.05)
  dta_boot[l,5] <- quantile(results[,l], 0.95)
}

rm(points, l, results)

##########################################
######## Simulate rest of season #########
##########################################

source("Simulations.R")

#########################################
######## Build figure and table #########
#########################################

source("Results.R")

#dev.off()
png(filename = paste(paste("ranking", last_game, sep="_"), ".png"), width = 4,height = 6, units = "in", res = 600)
par(omi=c(0,0,0.2,0), mgp=c(2,1,0), mar=c(1,1,1,0.5))#set the size of the outer margins
grid.draw(gt)
dev.off()


#dev.off()
png(filename = paste(paste("prediction", last_game, sep="_"), ".png"), width = 4.5, height = 4.5, units = "in", res = 600)
tab_plot
dev.off()

#Send email with results

# from <- "<sebpouliot@gmail.com>"
# to <- "<sebpouliot@gmail.com>"
# subject <- paste("NHL ranking", last_game, sep=":")
# body <- list("Here is the NHL ranking", mime_part( paste(paste("ranking", last_game, sep="_"), ".png")), mime_part( paste(paste("prediction", last_game, sep="_"), ".png")))
# sendmail(from, to, subject, body, control=list(smtpServer="ASPMX.L.GOOGLE.COM"))


end1 <- Sys.time()
end1-start1




 