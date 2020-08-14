# Non-parametric details and associated plots 
# Go from this point if don't want to redo the permutation

library(ggplot2)
library(jtools)#for apa_theme

perm <- read.csv("scored_data/permuted_results_alt_cov_onright.csv", header=TRUE, 
                 stringsAsFactors = FALSE)
alt1_ccatable <- read.csv("scored_data/alt1_ccatable.csv", header=TRUE, stringsAsFactors = FALSE)
alt1_xcoeftable <- read.csv("scored_data/alt1_xcoeftable.csv", header=TRUE, stringsAsFactors = FALSE)
alt1_ycoeftable <- read.csv("scored_data/alt1_ycoeftable.csv", header=TRUE, stringsAsFactors = FALSE)

#get rid of annoying extra variable created
perm$X <- NULL

#calculate non-parametric pvalue
np_rc1 <- length(which(perm$corr1 > alt1_ccatable$V1[1]))/length(perm$corr1)
np_rc2 <- length(which(perm$corr2 > alt1_ccatable$V1[2]))/length(perm$corr2)
np_rc3 <- length(which(perm$corr3 > alt1_ccatable$V1[3]))/length(perm$corr3)
np_rc4 <- length(which(perm$corr4 > alt1_ccatable$V1[4]))/length(perm$corr4)
np_rc5 <- length(which(perm$corr5 > alt1_ccatable$V1[5]))/length(perm$corr5)

np_rc<-c(np_rc1, np_rc2, np_rc3, np_rc4, np_rc5) 

#calculate non-parametric pvalue for F (not wilks by the way)
np_f1 <- length(which(perm$wilks1 > alt1_ccatable$V4[1]))/length(perm$wilks1)
np_f2 <- length(which(perm$wilks2 > alt1_ccatable$V4[2]))/length(perm$wilks2)
np_f3 <- length(which(perm$wilks3 > alt1_ccatable$V4[3]))/length(perm$wilks3)
np_f4 <- length(which(perm$wilks4 > alt1_ccatable$V4[4]))/length(perm$wilks4)
np_f5 <- length(which(perm$wilks5 > alt1_ccatable$V4[5]))/length(perm$wilks5)

np_f<-c(np_f1, np_f2, np_f3, np_f4, np_f5) 
#bind the non-parametric p values for both Rc and F together for the 5 dimensions
np_rc_F_alt1<- cbind(np_rc, np_f)

# Structure coefficients ---------------------------------------------------------------------------

# 2 Loops to run the non-parametric test and also plot the data
# Data will be stored in alt1_NPX and alt1_NPY

n <- length(alt1_xcoeftable$X)
alt1_NPX <- rep(NA, n)

for (i in 1:n) {
  null_dist <- perm[,alt1_xcoeftable$X[i]]
  emp_value <- alt1_xcoeftable$str_coef[i]
  hist(null_dist, main = alt1_xcoeftable$X[i], col="lightblue", xlim = c(-1,1))
  abline(v = emp_value, col="red", lwd=3, lty=2)
  alt1_NPX[i] <- sum(null_dist > abs(emp_value))/length(null_dist)
}

n <- length(alt1_ycoeftable$X)
alt1_NPY <- rep(NA, n)

for (i in 1:n) {
  null_dist <- perm[,alt1_ycoeftable$X[i]]
  emp_value <- alt1_ycoeftable$str_coef[i]
  hist(null_dist, main = alt1_ycoeftable$X[i], col="lightblue", xlim = c(-1,1))
  abline(v = emp_value, col="red", lwd=3, lty=2)
  alt1_NPY[i] <- sum(null_dist > abs(emp_value))/length(null_dist)
}




# # Plots ------------------------------------------------------
# Plots haven't been updated

# #Plot canonical correlation. 
# ##Red dotted line is original value
# ggplot(perm, aes(x=corr1)) +
#   geom_histogram(colour="black", fill="white") +
#   geom_vline(aes(xintercept=0.57), color="red", linetype="dashed", size=1)+
#   theme_apa()+ 
#   labs(x = "Canonical Correlation", y="Number of Permutations") +
#   theme(panel.border = element_blank(), axis.line = element_line())
# 
# #Plot canonical correlation 2 
# ##Red dotted line is original value
# ggplot(perm, aes(x=corr2)) +
#   geom_histogram(colour="black", fill="white") +
#   geom_vline(aes(xintercept=0.53), color="red", linetype="dashed", size=1)+
#   theme_apa()+ 
#   labs(x = "Canonical Correlation", y="Number of Permutations") +
#   theme(panel.border = element_blank(), axis.line = element_line())
# 
# #Plot canonical correlation 3 
# ##Red dotted line is original value
# ggplot(perm, aes(x=corr3)) +
#   geom_histogram(colour="black", fill="white") +
#   geom_vline(aes(xintercept=0.42), color="red", linetype="dashed", size=1)+
#   theme_apa()+ 
#   labs(x = "Canonical Correlation", y="Number of Permutations") +
#   theme(panel.border = element_blank(), axis.line = element_line())
# 
# #Plot canonical correlation 4 
# ##Red dotted line is original value
# 
# ggplot(perm, aes(x=corr4)) +
#   geom_histogram(colour="black", fill="white") +
#   geom_vline(aes(xintercept=0.32), color="red", linetype="dashed", size=1)+
#   theme_apa()+ 
#   labs(x = "Canonical Correlation", y="Number of Permutations") +
#   theme(panel.border = element_blank(), axis.line = element_line())
# 
# #Plot canonical correlation 5 
# ##Red dotted line is original value
# 
# ggplot(perm, aes(x=corr5)) +
#   geom_histogram(colour="black", fill="white") +
#   geom_vline(aes(xintercept=0.13), color="red", linetype="dashed", size=1)+
#   theme_apa()+ 
#   labs(x = "Canonical Correlation", y="Number of Permutations") +
#   theme(panel.border = element_blank(), axis.line = element_line())
# 
# #Plot canonical correlation 5 
# ##Red dotted line is original value
# 
# ggplot(perm, aes(x=corr5)) +
#   geom_histogram(colour="black", fill="white") +
#   geom_vline(aes(xintercept=0.13), color="red", linetype="dashed", size=1)+
#   theme_apa()+ 
#   labs(x = "Canonical Correlation", y="Number of Permutations") +
#   theme(panel.border = element_blank(), axis.line = element_line())
# 
# #Plot canonical correlation 4 
# ##Red dotted line is original value
# 
# 
# #actually F not wilks
# 
# ggplot(perm, aes(x=wilks1)) +
#   geom_histogram(colour="black", fill="white") +
#   geom_vline(aes(xintercept=1.90), color="red", linetype="dashed", size=1)+
#   theme_apa()+ 
#   labs(x = "Canonical Correlation", y="Number of Permutations") +
#   theme(panel.border = element_blank(), axis.line = element_line())
# 
# ggplot(perm, aes(x=wilks2)) +
#   geom_histogram(colour="black", fill="white") +
#   geom_vline(aes(xintercept=1.56), color="red", linetype="dashed", size=1)+
#   theme_apa()+ 
#   labs(x = "Canonical Correlation", y="Number of Permutations") +
#   theme(panel.border = element_blank(), axis.line = element_line())
# 
# ggplot(perm, aes(x=wilks3)) +
#   geom_histogram(colour="black", fill="white") +
#   geom_vline(aes(xintercept=1.09), color="red", linetype="dashed", size=1)+
#   theme_apa()+ 
#   labs(x = "Canonical Correlation", y="Number of Permutations") +
#   theme(panel.border = element_blank(), axis.line = element_line())
# 
# ggplot(perm, aes(x=wilks4)) +
#   geom_histogram(colour="black", fill="white") +
#   geom_vline(aes(xintercept=0.69), color="red", linetype="dashed", size=1)+
#   theme_apa()+ 
#   labs(x = "Canonical Correlation", y="Number of Permutations") +
#   theme(panel.border = element_blank(), axis.line = element_line())
# 
# ggplot(perm, aes(x=wilks5)) +
#   geom_histogram(colour="black", fill="white") +
#   geom_vline(aes(xintercept=0.19), color="red", linetype="dashed", size=1)+
#   theme_apa()+ 
#   labs(x = "Canonical Correlation", y="Number of Permutations") +
#   theme(panel.border = element_blank(), axis.line = element_line())
# 


# SAVING and output --------------------------------------------------------------------------------

write.csv(np_rc_F_alt1, file = "scored_data/non-para_rc_F_alt1.csv")
write.csv(alt1_NPX, file = "scored_data/alt1_NPX.csv")
write.csv(alt1_NPY, file = "scored_data/alt1_NPY.csv")
