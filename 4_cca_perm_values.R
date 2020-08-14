# Non-parametric details and associated plots - Go from this point if don't want to redo the
# permutation

library(ggplot2)
library(jtools)#for apa_theme

#read in data
perm <- read.csv("scored_data/permuted_results.csv", header=TRUE, stringsAsFactors = FALSE)
ccatable <- read.csv("scored_data/ccatable.csv", header=TRUE, stringsAsFactors = FALSE)
xcoeftable <- read.csv("scored_data/xcoeftable.csv", header=TRUE, stringsAsFactors = FALSE)
ycoeftable <- read.csv("scored_data/ycoeftable.csv", header=TRUE, stringsAsFactors = FALSE)

#get rid of annoying extra variable created
perm$X <- NULL

#calculate non-parametric pvalue for Rc #make a loop
np_rc1 <- length(which(perm$corr > ccatable$V1[1]))/length(perm$corr)
np_rc2 <- length(which(perm$corr2 > ccatable$V1[2]))/length(perm$corr2)
np_rc3 <- length(which(perm$corr3 > ccatable$V1[3]))/length(perm$corr3)
np_rc4 <- length(which(perm$corr4 > ccatable$V1[4]))/length(perm$corr4)
np_rc5 <- length(which(perm$corr5 > ccatable$V1[5]))/length(perm$corr5)
np_rc6 <- length(which(perm$corr6 > ccatable$V1[6]))/length(perm$corr6)
np_rc7 <- length(which(perm$corr7 > ccatable$V1[7]))/length(perm$corr7)
np_rc8 <- length(which(perm$corr8 > ccatable$V1[8]))/length(perm$corr8)
np_rc9 <- length(which(perm$corr9 > ccatable$V1[9]))/length(perm$corr9)

np_rc<-c(np_rc1, np_rc2, np_rc3, np_rc4, np_rc5, np_rc6, np_rc7, np_rc8, np_rc9) 

#Fperm
np_f1 <- length(which(perm$F1 > ccatable$V4[1]))/length(perm$F1)
np_f2 <- length(which(perm$F2 > ccatable$V4[2]))/length(perm$F2)
np_f3 <- length(which(perm$F3 > ccatable$V4[3]))/length(perm$F3)
np_f4 <- length(which(perm$F4 > ccatable$V4[4]))/length(perm$F4)
np_f5 <- length(which(perm$F5 > ccatable$V4[5]))/length(perm$F5)
np_f6 <- length(which(perm$F6 > ccatable$V4[6]))/length(perm$F6)
np_f7 <- length(which(perm$F7 > ccatable$V4[7]))/length(perm$F7)
np_f8 <- length(which(perm$F8 > ccatable$V4[8]))/length(perm$F8)
np_f9 <- NA

np_f<-c(np_f1, np_f2, np_f3, np_f4, np_f5, np_f6, np_f7, np_f8, np_f9) 

#bind the non-parametric p values for both Rc and F together for the 9 dimensions
np_rc_F<- cbind(np_rc, np_f)

# Structure coefficients ---------------------------------------------------------------------------

# 2 Loops to run the non-parametric test and also plot the data
# Data will be stored in NPX and NPY

n <- length(xcoeftable$X)
NPX <- rep(NA, n)

for (i in 1:n) {
  null_dist <- perm[,xcoeftable$X[i]]
  emp_value <- xcoeftable$str_coef[i]
  hist(null_dist, main = xcoeftable$X[i], col="lightblue", xlim = c(-1,1))
  abline(v = emp_value, col="red", lwd=3, lty=2)
  NPX[i] <- sum(null_dist > abs(emp_value))/length(null_dist)
}

n <- length(ycoeftable$X)
NPY <- rep(NA, n)

for (i in 1:n) {
  null_dist <- perm[,ycoeftable$X[i]]
  emp_value <- ycoeftable$str_coef[i]
  hist(null_dist, main = ycoeftable$X[i], col="lightblue", xlim = c(-1,1))
  abline(v = emp_value, col="red", lwd=3, lty=2)
  NPY[i] <- sum(null_dist > abs(emp_value))/length(null_dist)
}


# ##Plots-------------------------------------------------------------------------------------------
# Plots haven't been updated

# #Plot canonical correlations.
# ##Red dotted line is original value
# ggplot(perm, aes(x=corr)) +
#   geom_histogram(colour="black", fill="white") +
#   geom_vline(aes(xintercept=ccatable$V1[1]), color="red", linetype="dashed", size=1)+
#   theme_apa()+
#   labs(x = "Canonical Correlation", y="Number of Permutations") +
#   theme(panel.border = element_blank(), axis.line = element_line())
# 
# # plot 2nd canonical correlation
# ggplot(perm, aes(x=corr2)) +
#   geom_histogram(colour="black", fill="white") +
#   geom_vline(aes(xintercept=0.53), color="red", linetype="dashed", size=1)+
#   theme_apa()+ 
#   labs(x = "Canonical Correlation", y="Number of Permutations") +
#   theme(panel.border = element_blank(), axis.line = element_line())
# 
# #Plot Fperm 
# ##Red dotted line is original value
# ggplot(perm, aes(x=F1)) +
#   geom_histogram(colour="black", fill="white") +
#   geom_vline(aes(xintercept=1.63), color="red", linetype="dashed", size=1)+
#   theme_apa()+ 
#   labs(x = "Wilks", y="Number of Permutations") +
#   theme(panel.border = element_blank(), axis.line = element_line())
# 
# ##Red dotted line is original value
# ggplot(perm, aes(x=F2)) +
#   geom_histogram(colour="black", fill="white") +
#   geom_vline(aes(xintercept=1.19), color="red", linetype="dashed", size=1)+
#   theme_apa()+ 
#   labs(x = "Wilks2", y="Number of Permutations") +
#   theme(panel.border = element_blank(), axis.line = element_line())
# 

# SAVING and output --------------------------------------------------------------------------------

write.csv(np_rc_F, file = "scored_data/non-para_rc_F.csv")
write.csv(NPX, file = "scored_data/NPX.csv")
write.csv(NPY, file = "scored_data/NPY.csv")
