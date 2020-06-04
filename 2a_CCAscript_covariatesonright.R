#Main CCA script

require(dplyr)# needed for select function
require(CCA) #needed for cc function etc.
require(Hmisc) #needed for rcorr function
library(ggplot2)

setwd("C:/Users/Kate/SkyDrive/PhD/CCA/CCA_scored_data/")#Read in scored dataframe
cca_maindata <- read.csv("cca_maindata.csv", header=TRUE, stringsAsFactors = FALSE)

# Setting up X and Y varibale sets -------------------------------------------------------------
mainX <- dplyr::select(cca_maindata, aff_shar, cog_emp, emp_conc, emp_dist, silent_films_total)

#

mainY <- dplyr::select(cca_maindata, negmood_phys_bin, neg_selfest_bin, ineff_bin, intprobs_bin, 
                       scasseps, scassoc , scasphysinj , scasgad, 
                       scasocd, scasopanicag,
                       gender,FSIQ, F_T1_P_education, seifa_irsad_aus_percent)

#running the command to do the cca ---------------------------------------------------------------

res.cc=cc(mainX,mainY)

#Making some functions to caluclate stats to be used in next section -----------------------------

##Create function that gives the Wilks stat for all components, the F stat, degrees of freedom and pvalues

cca_signif <- function(cca_output, xvar, yvar) {
  
  ev <- (1 - cca_output$cor^2)
  
  n <- dim(xvar)[1]
  p <- length(xvar)
  q <- length(yvar)
  k <- min(p, q)
  m <- n - 3/2 - (p + q)/2
  
  w <- rev(cumprod(rev(ev)))
  
  # initialize
  d1 <- d2 <- f <- vector("numeric", k)
  
  for (i in 1:k) {
    s <- sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
    si <- 1/s
    d1[i] <- p * q
    d2[i] <- m * s - p * q/2 + 1
    r <- (1 - w[i]^si)/w[i]^si
    f[i] <- r * d2[i]/d1[i]
    p <- p - 1
    q <- q - 1
  }
  
  pv <- pf(f, d1, d2, lower.tail = FALSE)
  (dmat <- cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv))
}

##function for the standardised coefficients
standardised_xcanonical_coefficients <- function(cca_output, xvar, yvar){
  # standardized X canonical coefficients diagonal matrix of X sd's
  s1 <- diag(sqrt(diag(cov(xvar, use = "pairwise.complete.obs"))))
  s1 %*% cca_output$xcoef
}

standardised_ycanonical_coefficients <- function(cca_output, xvar, yvar){
  # standardized Y canonical coefficients diagonal matrix of Y sd's
  s2 <- diag(sqrt(diag(cov(yvar, use = "pairwise.complete.obs"))))
  s2 %*% cca_output$ycoef
}

#this section finds the correlation between the xvariables and the synthetic x variable, and the y vavriables and the synthetic y. Made from modifying the compute function.
#gets p-values and the then corrects by number of total correlations -bonferroni

cca_cor_sig <- function (X, Y, res) 
{
  
  
  xsynth <- res$scores[[1]][,1]
  ysynth <- res$scores[[2]][,1]
  
  xmatrix <- cbind(xsynth, X)
  xcor.res <- rcorr(as.matrix(xmatrix))
  #get the correltions out
  xcor.r <- xcor.res$r
  #just get the first column of the xvariables against the synthesised x variable scores
  xvar.corr.r<- xcor.r[,1]
  #get the p-values out
  xcor.p<- xcor.res$P
  #just get the first column of the xvariables against the synthesised x variable scores
  xvar.corr.p <- xcor.p[,1]
  
  
  #do the same for y
  ymatrix <- cbind(ysynth, Y)
  ycor.res <- rcorr(as.matrix(ymatrix))
  #get the correltions out
  ycor.r <- ycor.res$r
  #just get the first column of the xvariables against the synthesised x variable scores
  yvar.corr.r <- ycor.r[,1]
  #get the p-values out
  ycor.p<- ycor.res$P
  #just get the first column of the xvariables against the synthesised x variable scores
  yvar.corr.p <-ycor.p[,1]
  
  #bonferroni correction to see if p-values are below this threshold
  corrected.p<- 0.05/(ncol(X)+ncol(Y))
  
  return(list(xvar.corr.r = xvar.corr.r, 
              xvar.corr.p  = xvar.corr.p , yvar.corr.r = yvar.corr.r, 
              yvar.corr.p = yvar.corr.p, corrected.p = corrected.p))
}


#for the second compoennt
cca_cor_sig2 <- function (X, Y, res) 
{
  
  
  xsynth <- res$scores[[1]][,2]
  ysynth <- res$scores[[2]][,2]
  
  xmatrix <- cbind(xsynth, X)
  xcor.res <- rcorr(as.matrix(xmatrix))
  #get the correltions out
  xcor.r <- xcor.res$r
  #just get the column of the xvariables against the synthesised x variable scores
  xvar.corr.r<- xcor.r[,1]
  #get the p-values out
  xcor.p<- xcor.res$P
  #just get thecolumn of the xvariables against the synthesised x variable scores
  xvar.corr.p <- xcor.p[,1]
  
  
  #do the same for y
  ymatrix <- cbind(ysynth, Y)
  ycor.res <- rcorr(as.matrix(ymatrix))
  #get the correltions out
  ycor.r <- ycor.res$r
  #just get the column of the xvariables against the synthesised x variable scores
  yvar.corr.r <- ycor.r[,1]
  #get the p-values out
  ycor.p<- ycor.res$P
  #just get the column of the xvariables against the synthesised x variable scores
  yvar.corr.p <-ycor.p[,1]
  
  #bonferroni correction to see if p-values are below this threshold
  corrected.p<- 0.05/(ncol(X)+ncol(Y))
  
  return(list(xvar.corr.r = xvar.corr.r, 
              xvar.corr.p  = xvar.corr.p , yvar.corr.r = yvar.corr.r, 
              yvar.corr.p = yvar.corr.p, corrected.p = corrected.p))
}

#for the second compoennt
cca_cor_sig3 <- function (X, Y, res) 
{
  
  
  xsynth <- res$scores[[1]][,3]
  ysynth <- res$scores[[2]][,3]
  
  xmatrix <- cbind(xsynth, X)
  xcor.res <- rcorr(as.matrix(xmatrix))
  #get the correltions out
  xcor.r <- xcor.res$r
  #just get the column of the xvariables against the synthesised x variable scores
  xvar.corr.r<- xcor.r[,1]
  #get the p-values out
  xcor.p<- xcor.res$P
  #just get thecolumn of the xvariables against the synthesised x variable scores
  xvar.corr.p <- xcor.p[,1]
  
  
  #do the same for y
  ymatrix <- cbind(ysynth, Y)
  ycor.res <- rcorr(as.matrix(ymatrix))
  #get the correltions out
  ycor.r <- ycor.res$r
  #just get the column of the xvariables against the synthesised x variable scores
  yvar.corr.r <- ycor.r[,1]
  #get the p-values out
  ycor.p<- ycor.res$P
  #just get the column of the xvariables against the synthesised x variable scores
  yvar.corr.p <-ycor.p[,1]
  
  #bonferroni correction to see if p-values are below this threshold
  corrected.p<- 0.05/(ncol(X)+ncol(Y))
  
  return(list(xvar.corr.r = xvar.corr.r, 
              xvar.corr.p  = xvar.corr.p , yvar.corr.r = yvar.corr.r, 
              yvar.corr.p = yvar.corr.p, corrected.p = corrected.p))
}


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
#first function
rcorr(res.cc$scores$xscores[,1], res.cc$scores$yscores[,1])
#second function etc.
rcorr(res.cc$scores$xscores[,2], res.cc$scores$yscores[,2])
rcorr(res.cc$scores$xscores[,3], res.cc$scores$yscores[,3])
rcorr(res.cc$scores$xscores[,4], res.cc$scores$yscores[,4])
rcorr(res.cc$scores$xscores[,5], res.cc$scores$yscores[,5])



ggplot(data = cca_maindata, aes(x = res.cc$scores$xscores[,1], y = res.cc$scores$yscores[,1])) +
  geom_point() +
  theme_apa()+ 
  labs(x = "X Synthetic Variable", y="Y Synthetic Variable")+
  theme(panel.border = element_blank(), axis.line = element_line())

ggplot(data = cca_maindata, aes(x = res.cc$scores$xscores[,1], y = res.cc$scores$yscores[,1], colour = factor(gender))) +
  geom_point()



