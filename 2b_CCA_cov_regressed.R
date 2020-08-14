# Main CCA script

require(dplyr)# needed for select function
require(CCA) #needed for cc function etc.
require(Hmisc) #needed for rcorr function
library(ggplot2)

# Source the functions the examine stats within the CCA stored in CCA_func.R
source("CCA_func.R")

cca_maindata <- read.csv("scored_data/cca_maindata.csv", header=TRUE, stringsAsFactors = FALSE)


# regress out the variables ------------------------------------------------------------------------

# Create new matrix with the variables regressed out. Do this by running regressions for each of the 
# Y variables using the 
# covariates as predictors. Then use the residuals.

# #For the Y variables
negmood_phys_resid<- resid(lm(negmood_phys_bin~ gender + FSIQ + F_T1_P_education+ 
                                seifa_irsad_aus_percent, data= cca_maindata))
neg_selfest_resid<- resid(lm(neg_selfest_bin~ gender + FSIQ+ F_T1_P_education+ 
                               seifa_irsad_aus_percent, data= cca_maindata))
ineff_resid<- resid(lm(ineff_bin~ gender + FSIQ+ F_T1_P_education+ seifa_irsad_aus_percent, 
                       data= cca_maindata))
intprobs_resid<- resid(lm(intprobs_bin~ gender + FSIQ+ F_T1_P_education+ seifa_irsad_aus_percent, 
                          data= cca_maindata))
scasseps_resid<- resid(lm(scasseps~ gender + FSIQ+ F_T1_P_education+ seifa_irsad_aus_percent, 
                          data= cca_maindata))
scassoc_resid<- resid(lm(scassoc~ gender + FSIQ+ F_T1_P_education+ seifa_irsad_aus_percent, 
                         data= cca_maindata))
scasphysinj_resid<- resid(lm(scasphysinj~ gender + FSIQ+ F_T1_P_education+ seifa_irsad_aus_percent, 
                             data= cca_maindata))
scasgad_resid<- resid(lm(scasgad~ gender + FSIQ+ F_T1_P_education+ seifa_irsad_aus_percent, 
                         data= cca_maindata))
scasocd_resid<- resid(lm(scasocd~ gender + FSIQ+ F_T1_P_education+ seifa_irsad_aus_percent, 
                         data= cca_maindata))
scasopanicag_resid<- resid(lm(scasopanicag~ gender + FSIQ+ F_T1_P_education+ seifa_irsad_aus_percent,
                              data= cca_maindata))

cca_maindata <- cbind(cca_maindata, negmood_phys_resid, neg_selfest_resid, ineff_resid, 
                      intprobs_resid, scasseps_resid, scassoc_resid, scasphysinj_resid, 
                      scasgad_resid, scasocd_resid, scasopanicag_resid)

# #For the X variables
aff_shar_resid<- resid(lm(aff_shar~ gender + FSIQ+ F_T1_P_education+ seifa_irsad_aus_percent, 
                          data= cca_maindata))
cog_emp_resid<- resid(lm(cog_emp~ gender + FSIQ+ F_T1_P_education+ seifa_irsad_aus_percent, 
                         data= cca_maindata))
emp_conc_resid<- resid(lm(emp_conc~ gender + FSIQ+ F_T1_P_education+ seifa_irsad_aus_percent, 
                          data= cca_maindata))
emp_dist_resid<- resid(lm(emp_dist~ gender + FSIQ+ F_T1_P_education+ seifa_irsad_aus_percent, 
                          data= cca_maindata))
silent_films_total_resid<- resid(lm(silent_films_total~ gender + FSIQ+ F_T1_P_education+ 
                                      seifa_irsad_aus_percent, data= cca_maindata))

cca_maindata <- cbind(cca_maindata, aff_shar_resid, cog_emp_resid, emp_conc_resid, emp_dist_resid,
                      silent_films_total_resid)

# Setting up X and Y variable sets -------------------------------------------------------------
mainX <- dplyr::select(cca_maindata, aff_shar_resid, cog_emp_resid, emp_conc_resid, emp_dist_resid, 
                       silent_films_total_resid)

#

mainY <- dplyr::select(cca_maindata, negmood_phys_resid, neg_selfest_resid, ineff_resid, 
                       intprobs_resid, 
                       scasseps_resid, scassoc_resid , scasphysinj_resid , scasgad_resid, 
                       scasocd_resid, scasopanicag_resid)

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

#standardised coefficients
standardised_xcanonical_coefficients(res.cc,mainX,mainY)
standardised_ycanonical_coefficients(res.cc,mainX,mainY)


#correlations between original variables and the sythetic variable as well as p-values
structurecoefs<- cca_cor_sig(mainX, mainY, res.cc)
structurecoefs

structurecoefs2<- cca_cor_sig2(mainX, mainY, res.cc)
structurecoefs2
#squared correlations
(structurecoefs$xvar.corr.r)^2
(structurecoefs$yvar.corr.r)^2

#Parametric ps for the correlations
#first function - fifth
rcorr(res.cc$scores$xscores[,1], res.cc$scores$yscores[,1])$P[2,1]
rcorr(res.cc$scores$xscores[,2], res.cc$scores$yscores[,2])$P[2,1]
rcorr(res.cc$scores$xscores[,3], res.cc$scores$yscores[,3])$P[2,1]
rcorr(res.cc$scores$xscores[,4], res.cc$scores$yscores[,4])$P[2,1]
rcorr(res.cc$scores$xscores[,5], res.cc$scores$yscores[,5])$P[2,1]


#put these things into a table
alt2_ccatable<- cbind(res.cc$cor, res.cc$cor^2, cca_signif(res.cc,mainX,mainY)[,1], 
                      cca_signif(res.cc,mainX,mainY)[,2], cca_signif(res.cc,mainX,mainY)[,3], 
                      cca_signif(res.cc,mainX,mainY)[,4], cca_signif(res.cc,mainX,mainY)[,5], 
                      rbind(rcorr(res.cc$scores$xscores[,1], res.cc$scores$yscores[,1])$P[2,1], 
                            rcorr(res.cc$scores$xscores[,2], res.cc$scores$yscores[,2])$P[2,1],
                            rcorr(res.cc$scores$xscores[,3], res.cc$scores$yscores[,3])$P[2,1], 
                            rcorr(res.cc$scores$xscores[,4], res.cc$scores$yscores[,4])$P[2,1], 
                            rcorr(res.cc$scores$xscores[,5], res.cc$scores$yscores[,5])$P[2,1]))



# ##Make the matrix
# corrmatrix <-  dplyr::select(cca_maindata, 
#                              aff_shar_resid, cog_emp_resid, emp_conc_resid, emp_dist_resid,
#                              silent_films_total_resid, negmood_phys_resid, neg_selfest_resid, 
#                              ineff_resid, intprobs_resid, scasseps_resid, scassoc_resid , 
#                              scasphysinj_resid , scasgad_resid, scasocd_resid, scasopanicag_resid,
#                              gender,FSIQ, F_T1_P_education, seifa_irsad_aus_percent, age)
# 

# M <- cor(corrmatrix, use = "complete.obs")
# colnames(M) <- c("Aff. Sharing", "Cog. Emp.", "Emp. Concern", "Emp. Distress",
#                  "Silent Films", "Neg. Mood", "Neg. Self-est.",
#                  "Ineff.", "Interpersonal", "Sep. Anx.", "Social Anx.",
#                  "Phys. Injury", "GAD", "OCD",
#                  "Panic/Ag.","Sex","IQ","Parent Ed.","Neighbourhood", "Age")
# rownames(M) <- c("Aff. Sharing", "Cog. Emp.", "Emp. Concern", "Emp. Distress",
#                  "Silent Films", "Neg. Mood", "Neg. Self-est.",
#                  "Ineff.", "Interpersonal", "Sep. Anx.", "Social Anx.",
#                  "Phys. Injury", "GAD", "OCD",
#                  "Panic/Ag.","Sex","IQ","Parent Ed.","Neighbourhood", "Age")
# res1 <- cor.mtest(corrmatrix, use = "complete.obs", 
#                   conf.level = .95)
# corrplot(M, method = "circle", order = "hclust", 
#          tl.col = "black", tl.srt = 45,tl.cex = 1,
#          p.mat = res1$p, sig.level = .05, insig = "blank")
# 
# 
# ggplot(data = cca_maindata, aes(x = res.cc$scores$xscores[,1], y = res.cc$scores$yscores[,1])) +
#   geom_point() +
#   theme_apa()+ 
#   labs(x = "X Synthetic Variable", y="Y Synthetic Variable")+
#   theme(panel.border = element_blank(), axis.line = element_line())
# 
# ggplot(data = cca_maindata, aes(x = res.cc$scores$xscores[,1], y = res.cc$scores$yscores[,1], 
# colour = factor(gender))) +
#   geom_point()



# Exports ------------------------------------------------------------------------------------------

#create coefficients tables
alt2_xcoeftable <- cbind(standardised_xcanonical_coefficients(res.cc,mainX,mainY)[,1],
                         structurecoefs$xvar.corr.r[2:6], ((structurecoefs$xvar.corr.r)^2)[2:6], 
                         structurecoefs$xvar.corr.p[2:6])
colnames(alt2_xcoeftable) <- c("stnd_x_can_coef","str_coef", "sq_str_coef","para_p")

alt2_ycoeftable <- cbind(standardised_ycanonical_coefficients(res.cc,mainX,mainY)[,1],
                         structurecoefs$yvar.corr.r[2:11], ((structurecoefs$yvar.corr.r)^2)[2:11],
                         structurecoefs$yvar.corr.p[2:11])
colnames(alt2_ycoeftable) <- c("stnd_y_can_coef","str_coef", "sq_str_coef","para_p")

#Export X and Y datasets for permutation testing in 3_cca_perm_testing.R
save(mainX, mainY, file = "scored_data/2bXY.RData")

#write files
saveRDS(structurecoefs$corrected.p, "scored_data/2b_bonferroni_val.rds")
write.csv(alt2_ccatable, file = "scored_data/alt2_ccatable.csv")
write.csv(alt2_xcoeftable, file = "scored_data/alt2_xcoeftable.csv")
write.csv(alt2_ycoeftable, file = "scored_data/alt2_ycoeftable.csv")
