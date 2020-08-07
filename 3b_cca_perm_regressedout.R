library(gridExtra)
library(CCP)
library(jtools)#for apa_theme


#Start from section 2 the plots if don't want to redo the permutation

##########Generate the permutation###################
#run cca 2c script first in the console

##Make the blank dataframe where the values of interest can be saved to
perm <- data.frame(matrix(NA, nrow = 10000, ncol = 25))
colnames(perm) <- c("corr1", "corr2", "corr3", "corr4","corr5", 
                    "wilks1", "wilks2","wilks3", "wilks4", "wilks5", 
                    "aff_shar", "cog_emp", "emp_conc", "emp_dist", "silent_films_total",
                    "negmood_phys_bin", "neg_selfest_bin", "ineff_bin", "intprobs_bin", "scasseps",
                    "scassoc", "scasphysinj", "scasgad", "scasocd", "scasopanicag")

#loop through all the rows of the newly made dataframe 
##shuffle the variables, then run the CCA on the shuffled dataset
##save the correlations into the dataframe
for (i in c(1:nrow(perm))){
  
  #shuffle the xmatrix
  permX <- sapply(mainX, sample)
  #shuffle the Y matrix
  permY <- sapply(mainY, sample)
  #do the CCA with the shuffles x matrix  
  res.cc=cc(permX,permY)
  #Save the correlation and pvlaue to the blank dataframe called perm (col 1=r and col2=pvalue)   
  perm[i,1]<- res.cc$cor[1]
  perm[i,2]<- res.cc$cor[2]
  perm[i,3]<- res.cc$cor[3]
  perm[i,4]<- res.cc$cor[4]
  perm[i,5]<- res.cc$cor[5]
  wilkresults<-cca_signif(res.cc,mainX,mainY)
  perm[i,6]<- wilkresults[1,2]
  perm[i,7]<- wilkresults[2,2]
  perm[i,8]<- wilkresults[3,2]
  perm[i,9]<- wilkresults[4,2]
  perm[i,10]<- wilkresults[5,2]
  structurecoeff<- cca_cor_sig(mainX, mainY, res.cc)
  perm[i,11]<- structurecoeff$xvar.corr.r[2]
  perm[i,12]<- structurecoeff$xvar.corr.r[3]
  perm[i,13]<- structurecoeff$xvar.corr.r[4]
  perm[i,14]<- structurecoeff$xvar.corr.r[5]
  perm[i,15]<- structurecoeff$xvar.corr.r[6]
  perm[i,16]<- structurecoeff$yvar.corr.r[2]
  perm[i,17]<- structurecoeff$yvar.corr.r[3]
  perm[i,18]<- structurecoeff$yvar.corr.r[4]
  perm[i,19]<- structurecoeff$yvar.corr.r[5]
  perm[i,20]<- structurecoeff$yvar.corr.r[6]
  perm[i,21]<- structurecoeff$yvar.corr.r[7]
  perm[i,22]<- structurecoeff$yvar.corr.r[8]
  perm[i,23]<- structurecoeff$yvar.corr.r[9]
  perm[i,24]<- structurecoeff$yvar.corr.r[10]
  perm[i,25]<- structurecoeff$yvar.corr.r[11]
  perm[i,26]<- structurecoeff$yvar.corr.r[12]
  perm[i,27]<- structurecoeff$yvar.corr.r[13]
  perm[i,28]<- structurecoeff$yvar.corr.r[14]
  perm[i,29]<- structurecoeff$yvar.corr.r[15]
}

#Write perm table
write.csv(perm, 
          file = "scored_data/permuted_results_regressedout.csv")


########Plots - Go from this point if don't want to redo the permutation##################


perm <- read.csv("scored_data/permuted_results_regressedout.csv", header=TRUE, stringsAsFactors = FALSE)

#calculate non-parametric pvalue
np_rc1 <- length(which(perm$corr1 > 0.54))/length(perm$corr1)
np_rc2 <- length(which(perm$corr2 > 0.46))/length(perm$corr2)
np_rc3 <- length(which(perm$corr3 > 0.27))/length(perm$corr3)
np_rc4 <- length(which(perm$corr4 > 0.13))/length(perm$corr4)
np_rc5 <- length(which(perm$corr5 > 0.05))/length(perm$corr5)

np_rc<-c(np_rc1, np_rc2, np_rc3, np_rc4, np_rc5) 

#calculate non-parametric pvalue for F (not wilks by the way)
np_f1 <- length(which(perm$wilks1 > 1.72))/length(perm$wilks1)
np_f2 <- length(which(perm$wilks2 > 1.15))/length(perm$wilks2)
np_f3 <- length(which(perm$wilks3 > 0.48))/length(perm$wilks3)
np_f4 <- length(which(perm$wilks4 > 0.18))/length(perm$wilks4)
np_f5 <- length(which(perm$wilks5 > 0.05))/length(perm$wilks5)


np_f<-c(np_f1, np_f2, np_f3, np_f4, np_f5) 
#bind the non-parametric p values for both Rc and F together for the 5 dimensions
np_rc_F_alt2<- cbind(np_rc, np_f)


#calculate non-parametric pvalue
X1 <- length(which(perm$aff_shar < -0.8950896))/length(perm$aff_shar)
X2 <- length(which(perm$cog_emp < -0.2206367))/length(perm$cog_emp)
X3 <- length(which(perm$emp_conc < -0.3977909))/length(perm$emp_conc)
X4 <- length(which(perm$emp_dist < -0.6509764))/length(perm$emp_dist)
X5 <- length(which(perm$silent_films_total > 0.4035908))/length(perm$silent_films_total)

Y1 <- length(which(perm$negmood_phys_bin < -0.2516486 ))/length(perm$negmood_phys_bin)
Y2 <- length(which(perm$neg_selfest_bin > 0.1416600 ))/length(perm$neg_selfest_bin)
Y3 <- length(which(perm$ineff_bin < -0.2791820 ))/length(perm$ineff_bin)
Y4 <- length(which(perm$intprobs_bin < -0.1202286 ))/length(perm$intprobs_bin)
Y5 <- length(which(perm$scasseps < -0.5169413 ))/length(perm$scasseps)
Y6 <- length(which(perm$scassoc < -0.7710572 ))/length(perm$scassoc)
Y7 <- length(which(perm$scasphysinj < -0.5004022 ))/length(perm$scasphysinj)
Y8 <- length(which(perm$scasgad < -0.8053940  ))/length(perm$scasgad)
Y9 <- length(which(perm$scasocd < -0.6862993  ))/length(perm$scasocd)
Y10 <- length(which(perm$scasopanicag < -0.6071823 ))/length(perm$scasopanicag)


alt2_NPX <- c(X1, X2, X3, X4, X5)
alt2_NPY <- c(Y1, Y3, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10)

#grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, nrow=3)
#grid.arrange(p10,p11,p12,p13,p14,p15,p16,p17,p18, p19, nrow=3)


# ##Using the CCP package
# p.perm(mainX, mainY, nboot = 9999, rhostart = 1, type = "Wilks")
# wilks_perm<- p.perm(mainX, mainY, nboot = 9999, rhostart = 1, type = "Wilks")
# 
# plt.perm(wilks_perm)
# 
# 
# p.perm(mainX, mainY, nboot = 9999, rhostart = 2, type = "Wilks")
# #etc

# Plots ----------------------------------------------------------------------------------------------------------

#Plot canonical correlation. 
##Red dotted line is original value
ggplot(perm, aes(x=corr1)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=0.54), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Canonical Correlation", y="Number of Permutations") +
  theme(panel.border = element_blank(), axis.line = element_line())

#Plot canonical correlation 2 
##Red dotted line is original value
ggplot(perm, aes(x=corr2)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=0.46), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Canonical Correlation", y="Number of Permutations") +
  theme(panel.border = element_blank(), axis.line = element_line())

#Plot canonical correlation 3 
##Red dotted line is original value
ggplot(perm, aes(x=corr3)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=0.27), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Canonical Correlation", y="Number of Permutations") +
  theme(panel.border = element_blank(), axis.line = element_line())

#Plot canonical correlation 4 
##Red dotted line is original value

ggplot(perm, aes(x=corr4)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=0.13), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Canonical Correlation", y="Number of Permutations") +
  theme(panel.border = element_blank(), axis.line = element_line())

#Plot canonical correlation 4 
##Red dotted line is original value

ggplot(perm, aes(x=corr5)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=0.05), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Canonical Correlation", y="Number of Permutations") +
  theme(panel.border = element_blank(), axis.line = element_line())

##Red dotted line is original value
#T%his is actually Fstat

ggplot(perm, aes(x=wilks1)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=1.72), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Canonical Correlation", y="Number of Permutations") +
  theme(panel.border = element_blank(), axis.line = element_line())

ggplot(perm, aes(x=wilks2)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=1.15), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Canonical Correlation", y="Number of Permutations") +
  theme(panel.border = element_blank(), axis.line = element_line())

ggplot(perm, aes(x=wilks3)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=0.48), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Canonical Correlation", y="Number of Permutations") +
  theme(panel.border = element_blank(), axis.line = element_line())

ggplot(perm, aes(x=wilks4)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=0.18), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Canonical Correlation", y="Number of Permutations") +
  theme(panel.border = element_blank(), axis.line = element_line())

ggplot(perm, aes(x=wilks5)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=0.05), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Canonical Correlation", y="Number of Permutations") +
  theme(panel.border = element_blank(), axis.line = element_line())

#Plot structure coefficients

p1 <- ggplot(perm, aes(x=aff_shar)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept= -0.8950896 ), color="red", linetype="dashed", size=1) +
  theme_apa()+ 
  labs(x = "Affective Sharing", y="Number of Permutations")+
  theme(axis.title.y =element_blank(), panel.border = element_blank(), axis.line = element_line())

p2 <- ggplot(perm, aes(x=cog_emp)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.2206367), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Cognitive Empathy")+
  theme(axis.title.y =element_blank(), panel.border = element_blank(), 
        axis.line = element_line())

p3 <- ggplot(perm, aes(x=emp_conc)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.3977909), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Empathic Concern")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

p4 <- ggplot(perm, aes(x=emp_dist)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.6509764), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Empathic Distress", y="Number of Permutations")+
  theme(panel.border = element_blank(),
        axis.line = element_line())

p5 <- ggplot(perm, aes(x=silent_films_total)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=0.4035908), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Silent Films")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

p10 <- ggplot(perm, aes(x=negmood_phys_bin)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.2516486), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Neg. Mood/Phys. Sympt.", y="Number of Permutations")+
  theme(axis.title.y =element_blank(), panel.border = element_blank(),
        axis.line = element_line())
p11 <- ggplot(perm, aes(x=neg_selfest_bin)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=0.1416600), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Neg. Self-est.")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

p12 <- ggplot(perm, aes(x=ineff_bin)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.2791820), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Ineff.")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

p13 <- ggplot(perm, aes(x=intprobs_bin)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.1202286), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Interpersonal")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

p14 <- ggplot(perm, aes(x=scasseps)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.5169413 ), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Sep. Anx.", y="Number of Permutations")+
  theme(panel.border = element_blank(),
        axis.line = element_line())

p15 <- ggplot(perm, aes(x=scassoc)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.7710572), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Social Anx.")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

p16 <- ggplot(perm, aes(x=scasphysinj)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.5004022), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Phys. Injury")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

p17 <- ggplot(perm, aes(x=scasgad)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.8053940 ), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "GAD")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

p18 <- ggplot(perm, aes(x=scasocd)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.6862993 ), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "OCD", y="Number of Permutations")+
  theme(axis.title.y =element_blank(), panel.border = element_blank(),
        axis.line = element_line())

p19 <- ggplot(perm, aes(x=scasopanicag)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.6071823), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Panic/Ag.")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

# SAVING and output -----------------------------------------------------------------------------------------------

write.csv(np_rc_F_alt2, file = "scored_data/non-para_rc_F_alt2.csv")
write.csv(alt2_NPX, file = "scored_data/alt2_NPX.csv")
write.csv(alt2_NPY, file = "scored_data/alt2_NPY.csv")
