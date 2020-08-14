#Main CCA script

require(dplyr)# needed for select function
require(CCA) #needed for cc function etc.
require(Hmisc) #needed for rcorr function
library(ggplot2)

#Source the functions the examine stats within the CCA stored in CCA_func.R
source("CCA_func.R")

cca_maindata <- read.csv("scored_data/cca_maindata.csv", header=TRUE, stringsAsFactors = FALSE)

# Setting up X and Y variable sets -------------------------------------------------------------
mainX <- dplyr::select(cca_maindata, aff_shar, cog_emp, emp_conc, emp_dist, silent_films_total)

mainY <- dplyr::select(cca_maindata, negmood_phys_bin, neg_selfest_bin, ineff_bin, intprobs_bin, 
                       scasseps, scassoc , scasphysinj , scasgad, 
                       scasocd, scasopanicag,
                       gender,FSIQ, F_T1_P_education, seifa_irsad_aus_percent)

#running the command to do the cca ---------------------------------------------------------------

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


#correlations between original varibales and the sythetic varibale as well as pvalues
structurecoefs<- cca_cor_sig(mainX, mainY, res.cc)
structurecoefs

structurecoefs2<- cca_cor_sig2(mainX, mainY, res.cc)
structurecoefs2

structurecoefs3<- cca_cor_sig3(mainX, mainY, res.cc)
structurecoefs3
#squard correlations
(structurecoefs$xvar.corr.r)^2
(structurecoefs$yvar.corr.r)^2

(structurecoefs2$xvar.corr.r)^2
(structurecoefs2$yvar.corr.r)^2

(structurecoefs3$xvar.corr.r)^2
(structurecoefs3$yvar.corr.r)^2

#Parametric ps for the correlations
#first function - to fifth function
rcorr(res.cc$scores$xscores[,1], res.cc$scores$yscores[,1])$P[2,1]
rcorr(res.cc$scores$xscores[,2], res.cc$scores$yscores[,2])$P[2,1]
rcorr(res.cc$scores$xscores[,3], res.cc$scores$yscores[,3])$P[2,1]
rcorr(res.cc$scores$xscores[,4], res.cc$scores$yscores[,4])$P[2,1]
rcorr(res.cc$scores$xscores[,5], res.cc$scores$yscores[,5])$P[2,1]


#put these things into a table
alt1_ccatable<- cbind(res.cc$cor, res.cc$cor^2, cca_signif(res.cc,mainX,mainY)[,1], 
                      cca_signif(res.cc,mainX,mainY)[,2], cca_signif(res.cc,mainX,mainY)[,3], 
                      cca_signif(res.cc,mainX,mainY)[,4], cca_signif(res.cc,mainX,mainY)[,5], 
                 rbind(rcorr(res.cc$scores$xscores[,1], res.cc$scores$yscores[,1])$P[2,1],
                       rcorr(res.cc$scores$xscores[,2], res.cc$scores$yscores[,2])$P[2,1],
                       rcorr(res.cc$scores$xscores[,3], res.cc$scores$yscores[,3])$P[2,1], 
                       rcorr(res.cc$scores$xscores[,4], res.cc$scores$yscores[,4])$P[2,1], 
                       rcorr(res.cc$scores$xscores[,5], res.cc$scores$yscores[,5])$P[2,1]))



# ggplot(data = cca_maindata, aes(x = res.cc$scores$xscores[,1], y = res.cc$scores$yscores[,1])) +
#   geom_point() +
#   theme_apa()+ 
#   labs(x = "X Synthetic Variable", y="Y Synthetic Variable")+
#   theme(panel.border = element_blank(), axis.line = element_line())
# 
# ggplot(data = cca_maindata, aes(x = res.cc$scores$xscores[,1], y = res.cc$scores$yscores[,1], 
#   colour = factor(gender))) + geom_point()

# Exports-----------------------------------------------------------------------------

#create coefficients tables
alt1_xcoeftable <- cbind(standardised_xcanonical_coefficients(res.cc,mainX,mainY)[,1],
                         structurecoefs$xvar.corr.r[2:6], ((structurecoefs$xvar.corr.r)^2)[2:6], 
                         structurecoefs$xvar.corr.p[2:6])
colnames(alt1_xcoeftable) <- c("stnd_x_can_coef","str_coef", "sq_str_coef","para_p")

alt1_ycoeftable <- cbind(standardised_ycanonical_coefficients(res.cc,mainX,mainY)[,1],
                         structurecoefs$yvar.corr.r[2:15], ((structurecoefs$yvar.corr.r)^2)[2:15], 
                         structurecoefs$yvar.corr.p[2:15])
colnames(alt1_ycoeftable) <- c("stnd_y_can_coef","str_coef", "sq_str_coef","para_p")

#Export X and Y datasets for permutation testing in 3_cca_perm_testing.R
save(mainX, mainY, file = "scored_data/2aXY.RData")

#write files
saveRDS(structurecoefs$corrected.p, "scored_data/2a_bonferroni_val.rds")
write.csv(alt1_ccatable, file = "scored_data/alt1_ccatable.csv")
write.csv(alt1_xcoeftable, file = "scored_data/alt1_xcoeftable.csv")
write.csv(alt1_ycoeftable, file = "scored_data/alt1_ycoeftable.csv")



