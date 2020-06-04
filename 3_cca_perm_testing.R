library(gridExtra)
library(CCP)
library(jtools)#for apa_theme

setwd("C:/Users/Kate/SkyDrive/PhD/CCA/CCA_scored_data/")#Read in scored dataframe


#Start from section 2 the plots if don't want to redo the permutation

##########Generate the permutation###################
#run cca script first

##Make the blank dataframe where the values of interest can be saved to
perm <- data.frame(matrix(NA, nrow = 10000, ncol = 56))
colnames(perm) <- c("corr", "corr2", "corr3", "corr4","corr5", "corr6", "corr7", "corr8", "corr9",
                    "F1", "F2","F3", "F4", "F5", "F6", "F7", "F8", "F9",
                    "aff_shar", "cog_emp", "emp_conc", "emp_dist", "silent_films_total",
                    "gender", "FSIQ", "F_T1_P_education", "seifa_irsad_aus_percent",
                    "negmood_phys_bin", "neg_selfest_bin", "ineff_bin", "intprobs_bin", "scasseps",
                    "scassoc", "scasphysinj", "scasgad", "scasocd", "scasopanicag",
                    "aff_shar2", "cog_emp2", "emp_conc2", "emp_dist2", "silent_films_total2",
                    "gender2", "FSIQ2", "F_T1_P_education2", "seifa_irsad_aus_percent2",
                    "negmood_phys_bin2", "neg_selfest_bin2", "ineff_bin2", "intprobs_bin2", "scasseps2",
                    "scassoc2", "scasphysinj2", "scasgad2", "scasocd2", "scasopanicag2")

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
  perm[i,6]<- res.cc$cor[6]
  perm[i,7]<- res.cc$cor[7]
  perm[i,8]<- res.cc$cor[8]
  perm[i,9]<- res.cc$cor[9]
  wilkresults<-cca_signif(res.cc,mainX,mainY)
  perm[i,10]<- wilkresults[1,2]
  perm[i,11]<- wilkresults[2,2]
  perm[i,12]<- wilkresults[3,2]
  perm[i,13]<- wilkresults[4,2]
  perm[i,14]<- wilkresults[5,2]
  perm[i,15]<- wilkresults[6,2]
  perm[i,16]<- wilkresults[7,2]
  perm[i,17]<- wilkresults[8,2]
  perm[i,18]<- wilkresults[9,2]
  structurecoeff<- cca_cor_sig(mainX, mainY, res.cc)
  perm[i,19]<- structurecoeff$xvar.corr.r[2]
  perm[i,20]<- structurecoeff$xvar.corr.r[3]
  perm[i,21]<- structurecoeff$xvar.corr.r[4]
  perm[i,22]<- structurecoeff$xvar.corr.r[5]
  perm[i,23]<- structurecoeff$xvar.corr.r[6]
  perm[i,24]<- structurecoeff$xvar.corr.r[7]
  perm[i,25]<- structurecoeff$xvar.corr.r[8]
  perm[i,26]<- structurecoeff$xvar.corr.r[9]
  perm[i,27]<- structurecoeff$xvar.corr.r[10]
  perm[i,28]<- structurecoeff$yvar.corr.r[2]
  perm[i,29]<- structurecoeff$yvar.corr.r[3]
  perm[i,30]<- structurecoeff$yvar.corr.r[4]
  perm[i,31]<- structurecoeff$yvar.corr.r[5]
  perm[i,32]<- structurecoeff$yvar.corr.r[6]
  perm[i,33]<- structurecoeff$yvar.corr.r[7]
  perm[i,34]<- structurecoeff$yvar.corr.r[8]
  perm[i,35]<- structurecoeff$yvar.corr.r[9]
  perm[i,36]<- structurecoeff$yvar.corr.r[10]
  perm[i,37]<- structurecoeff$yvar.corr.r[11]
  structurecoeff2<- cca_cor_sig2(mainX, mainY, res.cc)
  perm[i,38]<- structurecoeff2$xvar.corr.r[2]
  perm[i,39]<- structurecoeff2$xvar.corr.r[3]
  perm[i,40]<- structurecoeff2$xvar.corr.r[4]
  perm[i,41]<- structurecoeff2$xvar.corr.r[5]
  perm[i,42]<- structurecoeff2$xvar.corr.r[6]
  perm[i,43]<- structurecoeff2$xvar.corr.r[7]
  perm[i,44]<- structurecoeff2$xvar.corr.r[8]
  perm[i,45]<- structurecoeff2$xvar.corr.r[9]
  perm[i,46]<- structurecoeff2$xvar.corr.r[10]
  perm[i,47]<- structurecoeff2$yvar.corr.r[2]
  perm[i,48]<- structurecoeff2$yvar.corr.r[3]
  perm[i,49]<- structurecoeff2$yvar.corr.r[4]
  perm[i,50]<- structurecoeff2$yvar.corr.r[5]
  perm[i,51]<- structurecoeff2$yvar.corr.r[6]
  perm[i,52]<- structurecoeff2$yvar.corr.r[7]
  perm[i,53]<- structurecoeff2$yvar.corr.r[8]
  perm[i,54]<- structurecoeff2$yvar.corr.r[9]
  perm[i,55]<- structurecoeff2$yvar.corr.r[10]
  perm[i,56]<- structurecoeff2$yvar.corr.r[11]
  }

#Write perm table
write.csv(perm, 
          file = "C:/Users/Kate/SkyDrive/PhD/CCA/CCA_scored_data/permuted_results.csv")


########Plots - Go from this point if don't want to redo the permutation##################


perm <- read.csv("permuted_results.csv", header=TRUE, stringsAsFactors = FALSE)


#Plot canonical correlation. 
##Red dotted line is original value
ggplot(perm, aes(x=corr)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=0.615), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Canonical Correlation", y="Number of Permutations") +
  theme(panel.border = element_blank(), axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$corr > 0.615))/length(perm$corr)

# plot 2nd canonical correlation
ggplot(perm, aes(x=corr2)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=0.53), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Canonical Correlation", y="Number of Permutations") +
  theme(panel.border = element_blank(), axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$corr2 > 0.53))/length(perm$corr2)

length(which(perm$corr3 > 0.39))/length(perm$corr)
length(which(perm$corr4 > 0.31))/length(perm$corr)
length(which(perm$corr5 > 0.26))/length(perm$corr)
length(which(perm$corr6 > 0.18))/length(perm$corr)
length(which(perm$corr7 > 0.11))/length(perm$corr)
length(which(perm$corr8 > 0.06))/length(perm$corr)
length(which(perm$corr9 > 0.02))/length(perm$corr)


#Plot Fperm 
##Red dotted line is original value
ggplot(perm, aes(x=F1)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=1.63), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Wilks", y="Number of Permutations") +
  theme(panel.border = element_blank(), axis.line = element_line())

length(which(perm$F1 >1.63))/length(perm$F1)

##Red dotted line is original value
ggplot(perm, aes(x=F2)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=1.19), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Wilks2", y="Number of Permutations") +
  theme(panel.border = element_blank(), axis.line = element_line())

length(which(perm$F2 > 1.19))/length(perm$F2)
length(which(perm$F3 > 0.81))/length(perm$F3)
length(which(perm$F4 > 0.61))/length(perm$F4)
length(which(perm$F5 > 0.46))/length(perm$F5)
length(which(perm$F6 > 0.28))/length(perm$F6)
length(which(perm$F7 > 0.16))/length(perm$F7)
length(which(perm$F8 > 0.08))/length(perm$F8)



#Plot structure coefficients

p1 <- ggplot(perm, aes(x=aff_shar)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept= -0.711548450 ), color="red", linetype="dashed", size=1) +
  theme_apa()+ 
  labs(x = "Affective Sharing", y="Number of Permutations")+
  theme(axis.title.y =element_blank(), panel.border = element_blank(), axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$aff_shar < -0.711548450))/length(perm$aff_shar)
length(which(perm$aff_shar2 > 0.46733893))/length(perm$aff_shar2)


p2 <- ggplot(perm, aes(x=cog_emp)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=0.114423237), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Cognitive Empathy")+
  theme(axis.title.y =element_blank(), panel.border = element_blank(), 
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$cog_emp > 0.114423237))/length(perm$cog_emp)
length(which(perm$cog_emp2 > 0.62245778))/length(perm$cog_emp2)

p3 <- ggplot(perm, aes(x=emp_conc)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.092901485), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Empathic Concern")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$emp_conc < -0.092901485))/length(perm$emp_conc)
length(which(perm$emp_conc2 > 0.60903509))/length(perm$emp_conc2)

p4 <- ggplot(perm, aes(x=emp_dist)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.519141834), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Empathic Distress", y="Number of Permutations")+
  theme(panel.border = element_blank(),
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$emp_dist < -0.519141834))/length(perm$emp_dist)
length(which(perm$emp_dist2 > 0.27358087))/length(perm$emp_dist2)

p5 <- ggplot(perm, aes(x=silent_films_total)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=0.036096505), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Silent Films")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$silent_films_total > 0.036096505))/length(perm$silent_films_total)
length(which(perm$silent_films_total2 < -0.43473230))/length(perm$silent_films_total2)


p6 <- ggplot(perm, aes(x=gender)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.564990699), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Sex")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$gender < -0.564990699))/length(perm$gender)
length(which(perm$gender2 < -0.23614955))/length(perm$gender2)

p7 <- ggplot(perm, aes(x=Total_scaledscores)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=0.068551719), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "IQ", y="Number of Permutations")+
  theme(axis.title.y =element_blank(), panel.border = element_blank(), axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$FSIQ > 0.068551719))/length(perm$FSIQ)
length(which(perm$FSIQ2 > 0.35548896))/length(perm$FSIQ2)


p8 <- ggplot(perm, aes(x=F_T1_P_education)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.007958488), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Parent Education")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$F_T1_P_education < -0.007958488 ))/length(perm$F_T1_P_education)
length(which(perm$F_T1_P_education2 > 0.17858541 ))/length(perm$F_T1_P_education2)

p9 <- ggplot(perm, aes(x=seifa_irsad_aus_percent)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=0.355955905), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Neighbourhood Advantage")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$seifa_irsad_aus_percent > 0.355955905 ))/length(perm$seifa_irsad_aus_percent)
length(which(perm$seifa_irsad_aus_percent2 > 0.06968283 ))/length(perm$seifa_irsad_aus_percent2)


p10 <- ggplot(perm, aes(x=negmood_phys_bin)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.3777543), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Neg. Mood/Phys. Sympt.", y="Number of Permutations")+
  theme(axis.title.y =element_blank(), panel.border = element_blank(),
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$negmood_phys_bin < -0.3777543 ))/length(perm$negmood_phys_bin)
length(which(perm$negmood_phys_bin2 < -0.09051147 ))/length(perm$negmood_phys_bin2)


p11 <- ggplot(perm, aes(x=neg_selfest_bin)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.2415007), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Neg. Self-est.")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$neg_selfest_bin < -0.2415007 ))/length(perm$neg_selfest_bin)
length(which(perm$neg_selfest_bin2 < -0.58550236 ))/length(perm$neg_selfest_bin2)


p12 <- ggplot(perm, aes(x=ineff_bin)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.4553013), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Ineff.")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$ineff_bin < -0.4553013 ))/length(perm$ineff_bin)
length(which(perm$ineff_bin2 < -0.35148733 ))/length(perm$ineff_bin2)

p13 <- ggplot(perm, aes(x=intprobs_bin)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.2963291), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Interpersonal")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$intprobs_bin < -0.2963291 ))/length(perm$intprobs_bin)
length(which(perm$intprobs_bin2 < -0.27954178 ))/length(perm$intprobs_bin2)

p14 <- ggplot(perm, aes(x=scasseps)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.7611357 ), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Sep. Anx.", y="Number of Permutations")+
  theme(panel.border = element_blank(),
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$scasseps < -0.7611357 ))/length(perm$scasseps)
length(which(perm$scasseps2 < -0.03000987 ))/length(perm$scasseps2)


p15 <- ggplot(perm, aes(x=scassoc)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.8791108), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Social Anx.")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$scassoc < -0.8791108 ))/length(perm$scassoc)
length(which(perm$scassoc2 > 0.07235125 ))/length(perm$scassoc2)


p16 <- ggplot(perm, aes(x=scasphysinj)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.5459857), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Phys. Injury")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$scasphysinj < -0.5459857 ))/length(perm$scasphysinj)
length(which(perm$scasphysinj2 > 0.10853916 ))/length(perm$scasphysinj2)


p17 <- ggplot(perm, aes(x=scasgad)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.6483604), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "GAD")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$scasgad < -0.6483604 ))/length(perm$scasgad)
length(which(perm$scasgad2 > 0.42791802 ))/length(perm$scasgad2)


p18 <- ggplot(perm, aes(x=scasocd)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.4649840), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "OCD", y="Number of Permutations")+
  theme(axis.title.y =element_blank(), panel.border = element_blank(),
        axis.line = element_line())

length(which(perm$scasocd < -0.4649840 ))/length(perm$scasocd)
length(which(perm$scasocd2 > 0.41406938 ))/length(perm$scasocd2)


p19 <- ggplot(perm, aes(x=scasopanicag)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.4480388), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Panic/Ag.")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

length(which(perm$scasopanicag < -0.4480388 ))/length(perm$scasopanicag)
length(which(perm$scasopanicag2 > 0.22853840 ))/length(perm$scasopanicag2)


grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, nrow=3)
grid.arrange(p10,p11,p12,p13,p14,p15,p16,p17,p18, p19, nrow=3)


##Using the CCP package
p.perm(mainX, mainY, nboot = 9999, rhostart = 1, type = "Wilks")
wilks_perm<- p.perm(mainX, mainY, nboot = 9999, rhostart = 1, type = "Wilks")

plt.perm(wilks_perm)


p.perm(mainX, mainY, nboot = 9999, rhostart = 2, type = "Wilks")
#etc
