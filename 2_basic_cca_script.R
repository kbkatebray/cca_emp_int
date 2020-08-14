# Main CCA script

# This script takes the scored data and runs CCA between empathy variables and internalizing symptom
# variables.


#Packages
require(dplyr)# needed for select function
require(CCA) #needed for cc function etc.
require(Hmisc) #needed for rcorr function


#Source the functions the examine stats within the CCA stored in CCA_func.R
source("CCA_func.R")

#Data
cca_maindata <- read.csv("scored_data/cca_maindata.csv", header=TRUE, stringsAsFactors = FALSE)



# Setting up X and Y variable sets -----------------------------------------------------------------
mainX <- dplyr::select(cca_maindata, aff_shar, cog_emp, emp_conc, emp_dist, silent_films_total,
                       gender,FSIQ, F_T1_P_education, seifa_irsad_aus_percent)


mainY <- dplyr::select(cca_maindata, negmood_phys_bin, neg_selfest_bin, ineff_bin, intprobs_bin, 
                             scasseps, scassoc , scasphysinj , scasgad, 
                             scasocd, scasopanicag)

#running the command to do the cca -----------------------------------------------------------------

res.cc=cc(mainX,mainY)


#Compute/display the stats for the cca ------------------------------------------------------------

##The different important stat outputs
# display the canonical correlations
#cc
res.cc$cor

# this gives you the canonical R2
#cc
res.cc$cor^2

#wilks stat, plus significance
cca_signif(res.cc,mainX,mainY)

#unstandardised coefficients
res.cc$xcoef
res.cc$ycoef

#standarsised coefficients
standardised_xcanonical_coefficients(res.cc,mainX,mainY)
standardised_ycanonical_coefficients(res.cc,mainX,mainY)


#correlations between original variables and the synthetic variable as well as pvalues
structurecoefs<- cca_cor_sig(mainX, mainY, res.cc)
structurecoefs

structurecoefs2<- cca_cor_sig2(mainX, mainY, res.cc)
structurecoefs2
#squared correlations
(structurecoefs$xvar.corr.r)^2
(structurecoefs$yvar.corr.r)^2
(structurecoefs2$xvar.corr.r)^2
(structurecoefs2$yvar.corr.r)^2

#Parametric ps for the correlations
#for all the functions
rcorr(res.cc$scores$xscores[,1], res.cc$scores$yscores[,1])$P[2,1]
rcorr(res.cc$scores$xscores[,2], res.cc$scores$yscores[,2])$P[2,1]
rcorr(res.cc$scores$xscores[,3], res.cc$scores$yscores[,3])$P[2,1]
rcorr(res.cc$scores$xscores[,4], res.cc$scores$yscores[,4])$P[2,1]
rcorr(res.cc$scores$xscores[,5], res.cc$scores$yscores[,5])$P[2,1]
rcorr(res.cc$scores$xscores[,6], res.cc$scores$yscores[,6])$P[2,1]
rcorr(res.cc$scores$xscores[,7], res.cc$scores$yscores[,7])$P[2,1]
rcorr(res.cc$scores$xscores[,8], res.cc$scores$yscores[,8])$P[2,1]
rcorr(res.cc$scores$xscores[,9], res.cc$scores$yscores[,9])$P[2,1]

#put these things into a table
ccatable<- cbind(res.cc$cor, res.cc$cor^2, cca_signif(res.cc,mainX,mainY)[,1], 
                 cca_signif(res.cc,mainX,mainY)[,2], cca_signif(res.cc,mainX,mainY)[,3], 
                 cca_signif(res.cc,mainX,mainY)[,4], cca_signif(res.cc,mainX,mainY)[,5], 
                 rbind(rcorr(res.cc$scores$xscores[,1], res.cc$scores$yscores[,1])$P[2,1], 
                       rcorr(res.cc$scores$xscores[,2], res.cc$scores$yscores[,2])$P[2,1], 
                       rcorr(res.cc$scores$xscores[,3], res.cc$scores$yscores[,3])$P[2,1], 
                       rcorr(res.cc$scores$xscores[,4], res.cc$scores$yscores[,4])$P[2,1], 
                       rcorr(res.cc$scores$xscores[,5], res.cc$scores$yscores[,5])$P[2,1], 
                       rcorr(res.cc$scores$xscores[,6], res.cc$scores$yscores[,6])$P[2,1], 
                       rcorr(res.cc$scores$xscores[,7], res.cc$scores$yscores[,7])$P[2,1], 
                       rcorr(res.cc$scores$xscores[,8], res.cc$scores$yscores[,8])$P[2,1],
                       rcorr(res.cc$scores$xscores[,9], res.cc$scores$yscores[,9])$P[2,1]))

# Exports ------------------------------------------------------------------------------------------

#create coefficients tables
xcoeftable <- cbind(standardised_xcanonical_coefficients(res.cc,mainX,mainY)[,1],
                    structurecoefs$xvar.corr.r[2:10], ((structurecoefs$xvar.corr.r)^2)[2:10], 
                    structurecoefs$xvar.corr.p[2:10])
colnames(xcoeftable) <- c("stnd_x_can_coef","str_coef", "sq_str_coef","para_p")

ycoeftable <- cbind(standardised_ycanonical_coefficients(res.cc,mainX,mainY)[,1],
                    structurecoefs$yvar.corr.r[2:11], ((structurecoefs$yvar.corr.r)^2)[2:11], 
                    structurecoefs$yvar.corr.p[2:11])
colnames(ycoeftable) <- c("stnd_y_can_coef","str_coef", "sq_str_coef","para_p")

#Create the table for scatterplot
scatter <- cbind(res.cc$scores$xscores[,1],res.cc$scores$yscores[,1])
colnames(scatter) <- c("X","Y")

#Export X and Y datasets for permutation testing in 3_cca_perm_testing.R
save(mainX, mainY, file = "scored_data/2XY.RData")

#Export the csv files
saveRDS(structurecoefs$corrected.p, "scored_data/2_bonferroni_val.rds")
write.csv(scatter, file = "scored_data/scatter.csv")
write.csv(ccatable, file = "scored_data/ccatable.csv")
write.csv(xcoeftable, file = "scored_data/xcoeftable.csv")
write.csv(ycoeftable, file = "scored_data/ycoeftable.csv")


