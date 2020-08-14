library(CCA)

# ONLY RERUN THIS SCRIPT IF YOU HAVE CHANGED SOMETHING IN THE SCRIPTS/DATA.
# OTHERWISE GO STRIGHT TO 4b_perm_values.Rmd

#Load in required data
load(file = "scored_data/2bXY.RData")
#Source the functions the examine stats within the CCA stored in CCA_func.R
source("CCA_func.R")

# Generate the permutation -------------------------------------------------------------------------

##Make the blank dataframe where the values of interest can be saved to
perm <- data.frame(matrix(NA, nrow = 10000, ncol = 25))
colnames(perm) <- c("corr1", "corr2", "corr3", "corr4","corr5", 
                    "wilks1", "wilks2","wilks3", "wilks4", "wilks5", 
                    "aff_shar_resid", "cog_emp_resid", "emp_conc_resid", "emp_dist_resid", 
                    "silent_films_total_resid","negmood_phys_resid", "neg_selfest_resid", 
                    "ineff_resid", "intprobs_resid", "scasseps_resid", "scassoc_resid", 
                    "scasphysinj_resid", "scasgad_resid", "scasocd_resid", "scasopanicag_resid")

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
  perm[i,1:5]<- res.cc$cor
 
  wilkresults<-cca_signif(res.cc,mainX,mainY)
  perm[i,6:10]<- wilkresults[,2]
 
  structurecoeff<- cca_cor_sig(mainX, mainY, res.cc)
  perm[i,11:15]<- structurecoeff$xvar.corr.r[2:6]
    perm[i,16:29]<- structurecoeff$yvar.corr.r[2:15]
 
}

#Write perm table
write.csv(perm, 
          file = "scored_data/permuted_results_regressedout.csv")

