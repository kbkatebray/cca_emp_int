# Non-parametric details and associated plots 
# Go from this point if don't want to redo the permutation

library(ggplot2)
library(jtools)#for apa_theme

perm <- read.csv("scored_data/permuted_results_regressedout.csv", header=TRUE, 
                 stringsAsFactors = FALSE)
alt2_ccatable <- read.csv("scored_data/alt2_ccatable.csv", header=TRUE, stringsAsFactors = FALSE)
alt2_xcoeftable <- read.csv("scored_data/alt2_xcoeftable.csv", header=TRUE, stringsAsFactors = FALSE)
alt2_ycoeftable <- read.csv("scored_data/alt2_ycoeftable.csv", header=TRUE, stringsAsFactors = FALSE)

#get rid of annoying extra variable created
perm$X <- NULL

#calculate non-parametric pvalue
np_rc1 <- length(which(perm$corr1 > alt2_ccatable$V1[1]))/length(perm$corr1)
np_rc2 <- length(which(perm$corr2 > alt2_ccatable$V1[2]))/length(perm$corr2)
np_rc3 <- length(which(perm$corr3 > alt2_ccatable$V1[3]))/length(perm$corr3)
np_rc4 <- length(which(perm$corr4 > alt2_ccatable$V1[4]))/length(perm$corr4)
np_rc5 <- length(which(perm$corr5 > alt2_ccatable$V1[5]))/length(perm$corr5)

np_rc<-c(np_rc1, np_rc2, np_rc3, np_rc4, np_rc5) 

#calculate non-parametric pvalue for F (not wilks by the way)
np_f1 <- length(which(perm$wilks1 > alt2_ccatable$V4[1]))/length(perm$wilks1)
np_f2 <- length(which(perm$wilks2 > alt2_ccatable$V4[2]))/length(perm$wilks2)
np_f3 <- length(which(perm$wilks3 > alt2_ccatable$V4[3]))/length(perm$wilks3)
np_f4 <- length(which(perm$wilks4 > alt2_ccatable$V4[4]))/length(perm$wilks4)
np_f5 <- length(which(perm$wilks5 > alt2_ccatable$V4[5]))/length(perm$wilks5)


np_f<-c(np_f1, np_f2, np_f3, np_f4, np_f5) 
#bind the non-parametric p values for both Rc and F together for the 5 dimensions
np_rc_F_alt2<- cbind(np_rc, np_f)

# Structure coefficients ---------------------------------------------------------------------------

# 2 Loops to run the non-parametric test and also plot the data
# Data will be stored in alt2_NPX and alt2_NPY

n <- length(alt2_xcoeftable$X)
alt2_NPX <- rep(NA, n)

for (i in 1:n) {
  null_dist <- perm[,alt2_xcoeftable$X[i]]
  emp_value <- alt2_xcoeftable$str_coef[i]
  hist(null_dist, main = alt2_xcoeftable$X[i], col="lightblue", xlim = c(-1,1))
  abline(v = emp_value, col="red", lwd=3, lty=2)
  alt2_NPX[i] <- sum(null_dist > abs(emp_value))/length(null_dist)
}

n <- length(alt2_ycoeftable$X)
alt2_NPY <- rep(NA, n)

for (i in 1:n) {
  null_dist <- perm[,alt2_ycoeftable$X[i]]
  emp_value <- alt2_ycoeftable$str_coef[i]
  hist(null_dist, main = alt2_ycoeftable$X[i], col="lightblue", xlim = c(-1,1))
  abline(v = emp_value, col="red", lwd=3, lty=2)
  alt2_NPY[i] <- sum(null_dist > abs(emp_value))/length(null_dist)
}




# # Plots ------------------------------------------------------------------------------------------
# Plots haven't been updated with new numbers 


# #Plot canonical correlation. 
# ##Red dotted line is original value
# ggplot(perm, aes(x=corr1)) +
#   geom_histogram(colour="black", fill="white") +
#   geom_vline(aes(xintercept=0.54), color="red", linetype="dashed", size=1)+
#   theme_apa()+ 
#   labs(x = "Canonical Correlation", y="Number of Permutations") +
#   theme(panel.border = element_blank(), axis.line = element_line())
# 
# #Plot canonical correlation 2 
# ##Red dotted line is original value
# ggplot(perm, aes(x=corr2)) +
#   geom_histogram(colour="black", fill="white") +
#   geom_vline(aes(xintercept=0.46), color="red", linetype="dashed", size=1)+
#   theme_apa()+ 
#   labs(x = "Canonical Correlation", y="Number of Permutations") +
#   theme(panel.border = element_blank(), axis.line = element_line())
# 
# #Plot canonical correlation 3 
# ##Red dotted line is original value
# ggplot(perm, aes(x=corr3)) +
#   geom_histogram(colour="black", fill="white") +
#   geom_vline(aes(xintercept=0.27), color="red", linetype="dashed", size=1)+
#   theme_apa()+ 
#   labs(x = "Canonical Correlation", y="Number of Permutations") +
#   theme(panel.border = element_blank(), axis.line = element_line())
# 
# #Plot canonical correlation 4 
# ##Red dotted line is original value
# 
# ggplot(perm, aes(x=corr4)) +
#   geom_histogram(colour="black", fill="white") +
#   geom_vline(aes(xintercept=0.13), color="red", linetype="dashed", size=1)+
#   theme_apa()+ 
#   labs(x = "Canonical Correlation", y="Number of Permutations") +
#   theme(panel.border = element_blank(), axis.line = element_line())
# 
# #Plot canonical correlation 4 
# ##Red dotted line is original value
# 
# ggplot(perm, aes(x=corr5)) +
#   geom_histogram(colour="black", fill="white") +
#   geom_vline(aes(xintercept=0.05), color="red", linetype="dashed", size=1)+
#   theme_apa()+ 
#   labs(x = "Canonical Correlation", y="Number of Permutations") +
#   theme(panel.border = element_blank(), axis.line = element_line())
# 
# ##Red dotted line is original value
# #T%his is actually Fstat
# 
# ggplot(perm, aes(x=wilks1)) +
#   geom_histogram(colour="black", fill="white") +
#   geom_vline(aes(xintercept=1.72), color="red", linetype="dashed", size=1)+
#   theme_apa()+ 
#   labs(x = "Canonical Correlation", y="Number of Permutations") +
#   theme(panel.border = element_blank(), axis.line = element_line())
# 
# ggplot(perm, aes(x=wilks2)) +
#   geom_histogram(colour="black", fill="white") +
#   geom_vline(aes(xintercept=1.15), color="red", linetype="dashed", size=1)+
#   theme_apa()+ 
#   labs(x = "Canonical Correlation", y="Number of Permutations") +
#   theme(panel.border = element_blank(), axis.line = element_line())
# 
# ggplot(perm, aes(x=wilks3)) +
#   geom_histogram(colour="black", fill="white") +
#   geom_vline(aes(xintercept=0.48), color="red", linetype="dashed", size=1)+
#   theme_apa()+ 
#   labs(x = "Canonical Correlation", y="Number of Permutations") +
#   theme(panel.border = element_blank(), axis.line = element_line())
# 
# ggplot(perm, aes(x=wilks4)) +
#   geom_histogram(colour="black", fill="white") +
#   geom_vline(aes(xintercept=0.18), color="red", linetype="dashed", size=1)+
#   theme_apa()+ 
#   labs(x = "Canonical Correlation", y="Number of Permutations") +
#   theme(panel.border = element_blank(), axis.line = element_line())
# 
# ggplot(perm, aes(x=wilks5)) +
#   geom_histogram(colour="black", fill="white") +
#   geom_vline(aes(xintercept=0.05), color="red", linetype="dashed", size=1)+
#   theme_apa()+ 
#   labs(x = "Canonical Correlation", y="Number of Permutations") +
#   theme(panel.border = element_blank(), axis.line = element_line())
# 

# SAVING and output --------------------------------------------------------------------------------

write.csv(np_rc_F_alt2, file = "scored_data/non-para_rc_F_alt2.csv")
write.csv(alt2_NPX, file = "scored_data/alt2_NPX.csv")
write.csv(alt2_NPY, file = "scored_data/alt2_NPY.csv")
