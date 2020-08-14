library(CCA)

# ONLY RERUN THIS SCRIPT IF YOU HAVE CHANGED SOMETHING IN THE SCRIPTS/DATA.
# OTHERWISE GO STRIGHT TO 4a_perm_values.Rmd

#Load in required data
load(file = "scored_data/2aXY.RData")
#Source the functions the examine stats within the CCA stored in CCA_func.R
source("CCA_func.R")

# Generate the permutation -------------------------------------------------------------------------

##Make the blank dataframe where the values of interest can be saved to
perm <- data.frame(matrix(NA, nrow = 10000, ncol = 58))
colnames(perm) <- c("corr1", "corr2", "corr3", "corr4","corr5", 
                    "wilks1", "wilks2","wilks3", "wilks4", "wilks5", 
                    "aff_shar", "cog_emp", "emp_conc", "emp_dist", "silent_films_total",
                    "negmood_phys_bin", "neg_selfest_bin", "ineff_bin", "intprobs_bin", "scasseps",
                    "scassoc", "scasphysinj", "scasgad", "scasocd", "scasopanicag",
                    "gender", "FSIQ", "F_T1_P_education", "seifa_irsad_aus_percent", 
                    "aff_shar2", "cog_emp2", "emp_conc2", "emp_dist2", "silent_films_total2",
                    "negmood_phys_bin2", "neg_selfest_bin2", "ineff_bin2", "intprobs_bin2", 
                    "scasseps2", "scassoc2", "scasphysinj2", "scasgad2", "scasocd2", "scasopanicag2",
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
  perm[i,1:5]<- res.cc$cor[1]
 
  wilkresults<-cca_signif(res.cc,mainX,mainY)
  perm[i,6:10]<- wilkresults[,2]

  structurecoeff<- cca_cor_sig(mainX, mainY, res.cc)
  perm[i,11:15]<- structurecoeff$xvar.corr.r[2:6]
  perm[i,16:29]<- structurecoeff$yvar.corr.r[2:15]

  structurecoeff2<- cca_cor_sig2(mainX, mainY, res.cc)
  perm[i,30:34]<- structurecoeff2$xvar.corr.r[2:6]
  perm[i,35:48]<- structurecoeff2$yvar.corr.r[2:15]

  }

#Write perm table
write.csv(perm, 
          file = "scored_data/permuted_results_alt_cov_onright.csv")


