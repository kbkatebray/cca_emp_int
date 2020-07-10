library(gridExtra)
library(CCP)
library(jtools)#for apa_theme


#Start from section 2 the plots if don't want to redo the permutation

##########Generate the permutation###################
#run cca script first

##Make the blank dataframe where the values of interest can be saved to
perm <- data.frame(matrix(NA, nrow = 10000, ncol = 58))
colnames(perm) <- c("corr1", "corr2", "corr3", "corr4","corr5", 
                    "wilks1", "wilks2","wilks3", "wilks4", "wilks5", 
                    "aff_shar", "cog_emp", "emp_conc", "emp_dist", "silent_films_total",
                    "negmood_phys_bin", "neg_selfest_bin", "ineff_bin", "intprobs_bin", "scasseps",
                    "scassoc", "scasphysinj", "scasgad", "scasocd", "scasopanicag",
                    "gender", "FSIQ", "F_T1_P_education", "seifa_irsad_aus_percent", 
                    "aff_shar2", "cog_emp2", "emp_conc2", "emp_dist2", "silent_films_total2",
                    "negmood_phys_bin2", "neg_selfest_bin2", "ineff_bin2", "intprobs_bin2", "scasseps2",
                    "scassoc2", "scasphysinj2", "scasgad2", "scasocd2", "scasopanicag2",
                    "gender2", "FSIQ2", "F_T1_P_education2", "seifa_irsad_aus_percent2")

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
  structurecoeff2<- cca_cor_sig2(mainX, mainY, res.cc)
  perm[i,30]<- structurecoeff2$xvar.corr.r[2]
  perm[i,31]<- structurecoeff2$xvar.corr.r[3]
  perm[i,32]<- structurecoeff2$xvar.corr.r[4]
  perm[i,33]<- structurecoeff2$xvar.corr.r[5]
  perm[i,34]<- structurecoeff2$xvar.corr.r[6]
  perm[i,35]<- structurecoeff2$yvar.corr.r[2]
  perm[i,36]<- structurecoeff2$yvar.corr.r[3]
  perm[i,37]<- structurecoeff2$yvar.corr.r[4]
  perm[i,38]<- structurecoeff2$yvar.corr.r[5]
  perm[i,39]<- structurecoeff2$yvar.corr.r[6]
  perm[i,40]<- structurecoeff2$yvar.corr.r[7]
  perm[i,41]<- structurecoeff2$yvar.corr.r[8]
  perm[i,42]<- structurecoeff2$yvar.corr.r[9]
  perm[i,43]<- structurecoeff2$yvar.corr.r[10]
  perm[i,44]<- structurecoeff2$yvar.corr.r[11]
  perm[i,45]<- structurecoeff2$yvar.corr.r[12]
  perm[i,46]<- structurecoeff2$yvar.corr.r[13]
  perm[i,47]<- structurecoeff2$yvar.corr.r[14]
  perm[i,48]<- structurecoeff2$yvar.corr.r[15]
  }

#Write perm table
write.csv(perm, 
          file = "scored_data/permuted_results_alt_cov_onright.csv")


########Plots - Go from this point if don't want to redo the permutation##################


perm <- read.csv("scored_data/permuted_results_alt_cov_onright.csv", header=TRUE, stringsAsFactors = FALSE)


#Plot canonical correlation. 
##Red dotted line is original value
ggplot(perm, aes(x=corr1)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=0.57), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Canonical Correlation", y="Number of Permutations") +
  theme(panel.border = element_blank(), axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$corr1 > 0.57))/length(perm$corr1)

#Plot canonical correlation 2 
##Red dotted line is original value
ggplot(perm, aes(x=corr2)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=0.53), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Canonical Correlation", y="Number of Permutations") +
  theme(panel.border = element_blank(), axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$corr2 > 0.53))/length(perm$corr2)

#Plot canonical correlation 3 
##Red dotted line is original value
ggplot(perm, aes(x=corr3)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=0.42), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Canonical Correlation", y="Number of Permutations") +
  theme(panel.border = element_blank(), axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$corr3 > 0.42))/length(perm$corr3)

#Plot canonical correlation 4 
##Red dotted line is original value

ggplot(perm, aes(x=corr4)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=0.32), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Canonical Correlation", y="Number of Permutations") +
  theme(panel.border = element_blank(), axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$corr4 > 0.32))/length(perm$corr4)

#Plot canonical correlation 4 
##Red dotted line is original value

ggplot(perm, aes(x=corr5)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=0.13), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Canonical Correlation", y="Number of Permutations") +
  theme(panel.border = element_blank(), axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$corr5 > 0.13))/length(perm$corr4)


#Plot canonical correlation 4 
##Red dotted line is original value
#actually F not wilks

ggplot(perm, aes(x=wilks1)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=1.90), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Canonical Correlation", y="Number of Permutations") +
  theme(panel.border = element_blank(), axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$wilks1 > 1.90))/length(perm$wilks1)

ggplot(perm, aes(x=wilks2)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=1.56), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Canonical Correlation", y="Number of Permutations") +
  theme(panel.border = element_blank(), axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$wilks2 > 1.56))/length(perm$wilks2)

ggplot(perm, aes(x=wilks3)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=1.09), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Canonical Correlation", y="Number of Permutations") +
  theme(panel.border = element_blank(), axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$wilks3 > 1.09))/length(perm$wilks3)

ggplot(perm, aes(x=wilks4)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=0.69), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Canonical Correlation", y="Number of Permutations") +
  theme(panel.border = element_blank(), axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$wilks4 > 0.69))/length(perm$wilks4)


ggplot(perm, aes(x=wilks5)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=0.19), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Canonical Correlation", y="Number of Permutations") +
  theme(panel.border = element_blank(), axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$wilks5 > 0.19))/length(perm$wilks5)

#Plot structure coefficients

p1 <- ggplot(perm, aes(x=aff_shar)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept= -0.9319041 ), color="red", linetype="dashed", size=1) +
  theme_apa()+ 
  labs(x = "Affective Sharing", y="Number of Permutations")+
  theme(axis.title.y =element_blank(), panel.border = element_blank(), axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$aff_shar < -0.9319041))/length(perm$aff_shar)
length(which(perm$aff_shar2 < -0.03297094))/length(perm$aff_shar2)
length(which(perm$aff_shar3 < -0.03297094))/length(perm$aff_shar3)


p2 <- ggplot(perm, aes(x=cog_emp)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.2322313), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Cognitive Empathy")+
  theme(axis.title.y =element_blank(), panel.border = element_blank(), 
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$cog_emp < -0.2322313))/length(perm$cog_emp)
length(which(perm$cog_emp2 < -0.20518983))/length(perm$cog_emp2)
length(which(perm$cog_emp2 < -0.20518983))/length(perm$cog_emp2)

p3 <- ggplot(perm, aes(x=emp_conc)) +
  geom_histogram(colour="black", fill="white") +
    geom_vline(aes(xintercept=-0.3939769), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Empathic Concern")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$emp_conc < -0.3939769))/length(perm$emp_conc)
length(which(perm$emp_conc2 < -0.14484547))/length(perm$emp_conc2)
length(which(perm$emp_conc < -0.3939769))/length(perm$emp_conc)


p4 <- ggplot(perm, aes(x=emp_dist)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.7031995 ), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Empathic Distress", y="Number of Permutations")+
  theme(panel.border = element_blank(),
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$emp_dist < -0.7031995 ))/length(perm$emp_dist)
length(which(perm$emp_dist2 > 0.34716472 ))/length(perm$emp_dist2)
length(which(perm$emp_dist < -0.7031995 ))/length(perm$emp_dist)


p5 <- ggplot(perm, aes(x=silent_films_total)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=0.1398098), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Silent Films")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$silent_films_total > 0.1398098))/length(perm$silent_films_total)
length(which(perm$silent_films_total2 > 0.90144111))/length(perm$silent_films_total2)
length(which(perm$silent_films_total > 0.1398098))/length(perm$silent_films_total)


p6 <- ggplot(perm, aes(x=gender)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept= -0.383888813), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Sex")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$gender <  -0.383888813))/length(perm$gender)
length(which(perm$gender2 >  0.171168137))/length(perm$gender2)
length(which(perm$gender <  -0.383888813))/length(perm$gender)


p7 <- ggplot(perm, aes(x=FSIQ)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.140285854), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "IQ", y="Number of Permutations")+
  theme(axis.title.y =element_blank(), panel.border = element_blank(), axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$FSIQ < -0.140285854))/length(perm$FSIQ)
length(which(perm$FSIQ2 > 0.530448325))/length(perm$FSIQ2)
length(which(perm$FSIQ < -0.140285854))/length(perm$FSIQ)

p8 <- ggplot(perm, aes(x=F_T1_P_education)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.151056953), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Parent Education")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$F_T1_P_education < -0.151056953 ))/length(perm$F_T1_P_education)
length(which(perm$F_T1_P_education2 < -0.226008908 ))/length(perm$F_T1_P_education2)
length(which(perm$F_T1_P_education < -0.151056953 ))/length(perm$F_T1_P_education)

p9 <- ggplot(perm, aes(x=seifa_irsad_aus_percent)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=0.007943708), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Neighbourhood Advantage")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$seifa_irsad_aus_percent > 0.007943708 ))/length(perm$seifa_irsad_aus_percent)
length(which(perm$seifa_irsad_aus_percent2 < -0.181086486 ))/length(perm$seifa_irsad_aus_percent2)
length(which(perm$seifa_irsad_aus_percent > 0.007943708 ))/length(perm$seifa_irsad_aus_percent)

p10 <- ggplot(perm, aes(x=negmood_phys_bin)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.297196287 ), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Neg. Mood/Phys. Sympt.", y="Number of Permutations")+
  theme(axis.title.y =element_blank(), panel.border = element_blank(),
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$negmood_phys_bin < -0.297196287  ))/length(perm$negmood_phys_bin)
length(which(perm$negmood_phys_bin2 > 0.287779750   ))/length(perm$negmood_phys_bin2)
length(which(perm$negmood_phys_bin < -0.297196287  ))/length(perm$negmood_phys_bin)


p11 <- ggplot(perm, aes(x=neg_selfest_bin)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=0.043588198), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Neg. Self-est.")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$neg_selfest_bin > 0.043588198 ))/length(perm$neg_selfest_bin)
length(which(perm$neg_selfest_bin2 > 0.284799666  ))/length(perm$neg_selfest_bin2)
length(which(perm$neg_selfest_bin > 0.043588198 ))/length(perm$neg_selfest_bin)

p12 <- ggplot(perm, aes(x=ineff_bin)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.290748542), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Ineff.")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$ineff_bin < -0.290748542 ))/length(perm$ineff_bin)
length(which(perm$ineff_bin2 > 0.327017714 ))/length(perm$ineff_bin2)
length(which(perm$ineff_bin < -0.290748542 ))/length(perm$ineff_bin)


p13 <- ggplot(perm, aes(x=intprobs_bin)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.217321902), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Interpersonal")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$intprobs_bin < -0.217321902 ))/length(perm$intprobs_bin)
length(which(perm$intprobs_bin2 > 0.270424638 ))/length(perm$intprobs_bin2)
length(which(perm$intprobs_bin < -0.217321902 ))/length(perm$intprobs_bin)

p14 <- ggplot(perm, aes(x=scasseps)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept= -0.573028345  ), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Sep. Anx.", y="Number of Permutations")+
  theme(panel.border = element_blank(),
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$scasseps <  -0.573028345  ))/length(perm$scasseps)
length(which(perm$scasseps2 >  0.007622409   ))/length(perm$scasseps2)
length(which(perm$scasseps <  -0.573028345  ))/length(perm$scasseps)

p15 <- ggplot(perm, aes(x=scassoc)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.813145276), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Social Anx.")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$scassoc < -0.813145276 ))/length(perm$scassoc)
length(which(perm$scassoc2 > 0.131018946  ))/length(perm$scassoc2)
length(which(perm$scassoc < -0.813145276 ))/length(perm$scassoc)

p16 <- ggplot(perm, aes(x=scasphysinj)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.515711041), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Phys. Injury")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$scasphysinj < -0.515711041 ))/length(perm$scasphysinj)
length(which(perm$scasphysinj2 < -0.056607611 ))/length(perm$scasphysinj2)
length(which(perm$scasphysinj < -0.515711041 ))/length(perm$scasphysinj)


p17 <- ggplot(perm, aes(x=scasgad)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.798723100 ), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "GAD")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

#calculate non-parametric pvalue
length(which(perm$scasgad < -0.798723100  ))/length(perm$scasgad)
length(which(perm$scasgad2 < -0.006561444  ))/length(perm$scasgad2)
length(which(perm$scasgad < -0.798723100  ))/length(perm$scasgad)

p18 <- ggplot(perm, aes(x=scasocd)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.594082510), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "OCD", y="Number of Permutations")+
  theme(axis.title.y =element_blank(), panel.border = element_blank(),
        axis.line = element_line())

length(which(perm$scasocd < -0.594082510 ))/length(perm$scasocd)
length(which(perm$scasocd2 < -0.261887608))/length(perm$scasocd2)
length(which(perm$scasocd < -0.594082510 ))/length(perm$scasocd)

p19 <- ggplot(perm, aes(x=scasopanicag)) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=-0.591691903 ), color="red", linetype="dashed", size=1)+
  theme_apa()+ 
  labs(x = "Panic/Ag.")+
  theme(axis.title.y=element_blank(), panel.border = element_blank(),
        axis.line = element_line())

length(which(perm$scasopanicag < -0.591691903  ))/length(perm$scasopanicag)
length(which(perm$scasopanicag2 < -0.059922081   ))/length(perm$scasopanicag2)
length(which(perm$scasopanicag < -0.591691903  ))/length(perm$scasopanicag)

#grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, nrow=3)
#grid.arrange(p10,p11,p12,p13,p14,p15,p16,p17,p18, p19, nrow=3)


##Using the CCP package
p.perm(mainX, mainY, nboot = 9999, rhostart = 1, type = "Wilks")
wilks_perm<- p.perm(mainX, mainY, nboot = 9999, rhostart = 1, type = "Wilks")

plt.perm(wilks_perm)


p.perm(mainX, mainY, nboot = 9999, rhostart = 2, type = "Wilks")
#etc
