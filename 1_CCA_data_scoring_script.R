###Scoring code###

# This script takes the raw data exported from the Access database and scores it. 
#It also does some basic cleaning such as getting rid of pilots, and only retaining those who returned for phase 2.

# Inputs:
# csv files that are currently in "raw_data/"
# This is the raw data exported from Access database and copied into this folder
# There is one file per measure

# Outputs:
# One dataframe saved as a csv file with all the raw and scored data merged together
# Kept in "scored_data/"
# Called data_scored.csv

# Author: Kate Bray
# Last edited: 10/7/2020

# Set up ---------------------------------------------------------------------------------------------------


# Load packages
library(psych) # for scoring functions
library(lubridate) #for date functions
library(plyr) # join
library(car) # for recode

# Read in the csv files for each measure
empathy <- read.csv("raw_data/41_F_T2_C_Empathy.csv", header=TRUE, stringsAsFactors = FALSE)
silent_films <- read.csv("raw_data/38_F_T2_C_Silent Films.csv ", header=TRUE, stringsAsFactors = FALSE)
child_CDI <- read.csv("raw_data/28_F_T2_C_CDI.csv", header=TRUE, stringsAsFactors = FALSE)
SCAS <- read.csv("raw_data/29_F_T2_C_SCAS.csv", header=TRUE, stringsAsFactors = FALSE)
switchboard <- read.csv("raw_data/Switchboard.csv", header=FALSE, stringsAsFactors = FALSE) 
WISC <- read.csv("raw_data/36-6_F_T2_C_WISC.csv", header=TRUE, stringsAsFactors = FALSE)
FSIQ <- read.csv("raw_data/FSIQ_lookup_table.csv", header=TRUE, stringsAsFactors = FALSE)
parent_demog<- read.csv("raw_data/26_F_T2_P_ParentDemog.csv", header=TRUE, stringsAsFactors = FALSE)
SEIFA <- read.csv("raw_data/SEIFA.csv", header=TRUE, stringsAsFactors = FALSE)
FSIQ <- read.csv("raw_data/FSIQ_lookup_table.csv", header=TRUE, stringsAsFactors = FALSE)


# Empathy ---------------------------------------------------------------------------------
# Comprised of the Adolescent Measure of Empathy and Sympathy (Vossen et al. 2015) 
# and the Empathic Distress subscale  from the Empathic Responsiveness Questionnaire (Olweus & Endresen, 1998).

# Tidy up by getting rid of ID varibable which doesn't hold useful info
empathy$ID <- NULL

#change -9 (missing) and -3 (not relevant) to NA
empathy[empathy == -9]  <- NA
empathy[empathy == -3]  <- NA

# Score the raw data (psych package)
# Firstly create lists defining which items are in which subscale
# cog_emp, aff_shar and emp_conc are scored first as they have Likert scale with 5 points, emp_dist has 6
keys.listempathy1 <- list(cog_emp=c("F_T1_C_EMPATHY_1_1", "F_T1_C_EMPATHY_1_3","F_T1_C_EMPATHY_1_8","F_T1_C_EMPATHY_1_10"),
                          aff_shar=c("F_T1_C_EMPATHY_1_5","F_T1_C_EMPATHY_1_7","F_T1_C_EMPATHY_1_9","F_T1_C_EMPATHY_1_12"),
                          emp_conc=c("F_T1_C_EMPATHY_1_2","F_T1_C_EMPATHY_1_4","F_T1_C_EMPATHY_1_6","F_T1_C_EMPATHY_1_11") 
)
# Subscale totals are created by summing the items, and imputing any missing by the mean
empathyscored1 <- scoreItems(keys.listempathy1,empathy, impute= "mean", totals=TRUE, min=1,max=5) # note - FALSE:average, TRUE:sum
#empathyscored$scores #The scores themselves
empathy_subscales1 <- as.data.frame(empathyscored1$scores)#the totals
# The same process is repeated fro the emp_dist subscale
keys.listempathy2 <- list(emp_dist=c("F_T1_C_EMPATHY_2_1","F_T1_C_EMPATHY_2_2","F_T1_C_EMPATHY_2_3"))
empathyscored2 <- scoreItems(keys.listempathy2,empathy, impute= "mean", totals=TRUE, min=1,max=6) # note - FALSE:average, TRUE:sum
#empathyscored$scores #The scores themselves
empathy_subscales2 <- as.data.frame(empathyscored2$scores)#the totals
empathy <- cbind(empathy, empathy_subscales1, empathy_subscales2)#raw scores and totals together

# This step excludes any participants who have less than 70% of the subscale items present (more than 1 item missing for cog_emp, aff_shar and emp_conc
# and any items missing for emp_dist)
# cog_emp
empathy[which(rowSums(is.na(empathy[, c("F_T1_C_EMPATHY_1_1", "F_T1_C_EMPATHY_1_3","F_T1_C_EMPATHY_1_8","F_T1_C_EMPATHY_1_10")]))>1),"cog_emp"] <- NA
#aff_shar
empathy[which(rowSums(is.na(empathy[, c("F_T1_C_EMPATHY_1_5","F_T1_C_EMPATHY_1_7","F_T1_C_EMPATHY_1_9","F_T1_C_EMPATHY_1_12")]))>1),"aff_shar"] <- NA
#empconc
empathy[which(rowSums(is.na(empathy[, c("F_T1_C_EMPATHY_1_2","F_T1_C_EMPATHY_1_4","F_T1_C_EMPATHY_1_6","F_T1_C_EMPATHY_1_11")]))>1),"emp_conc"] <- NA
#emp_dist
empathy[which(rowSums(is.na(empathy[, c("F_T1_C_EMPATHY_2_1","F_T1_C_EMPATHY_2_2","F_T1_C_EMPATHY_2_3")]))>0),"emp_dist"] <- NA


# Children's Depression Inventory (CDI) -----------------------------------------------------------------------------------------------------------

# Remove "ID" column
child_CDI$ID <- NULL

#change -9 and -3 to NA
child_CDI[child_CDI == -9]  <- NA
child_CDI[child_CDI == -3]  <- NA

##calculation of scores for child CDI (1,2,3 to 0,1,2)
#save Fnumbers
FACTS_ID <- child_CDI[,1]
#first minus 1 from all scores (1,2,3 to 0,1,2)
child_CDI <- child_CDI[,-1]-1
#put F number back on
child_CDI <-  cbind(FACTS_ID, child_CDI)


#scoring code from psych package
keys.listcCDI <- list(totalCDI=c("F_T1_C_CDI_1" , "-F_T1_C_CDI_2" , "F_T1_C_CDI_3" , "F_T1_C_CDI_4" , "F_T1_C_CDI_5" , "-F_T1_C_CDI_6" , 
                                 "-F_T1_C_CDI_7" , "F_T1_C_CDI_8" , "-F_T1_C_CDI_9" , "-F_T1_C_CDI_10" , "F_T1_C_CDI_11" , "-F_T1_C_CDI_12" ,
                                 "F_T1_C_CDI_13" , "-F_T1_C_CDI_14" , "-F_T1_C_CDI_15" , "F_T1_C_CDI_16" , "-F_T1_C_CDI_17" , "F_T1_C_CDI_18" ,
                                 "F_T1_C_CDI_19" , "-F_T1_C_CDI_20" , "F_T1_C_CDI_21" , "F_T1_C_CDI_22" , "-F_T1_C_CDI_23" , "-F_T1_C_CDI_24" ,
                                 "F_T1_C_CDI_25" , "-F_T1_C_CDI_26" , "-F_T1_C_CDI_27" , "F_T1_C_CDI_28"),
                      emo_cCDI=c("F_T1_C_CDI_1","-F_T1_C_CDI_9","-F_T1_C_CDI_10","-F_T1_C_CDI_15" , "F_T1_C_CDI_16" , "-F_T1_C_CDI_17" ,
                                 "F_T1_C_CDI_18","-F_T1_C_CDI_26" , "-F_T1_C_CDI_27", 
                                 "-F_T1_C_CDI_2","-F_T1_C_CDI_6","-F_T1_C_CDI_7" , "F_T1_C_CDI_8", "F_T1_C_CDI_13","-F_T1_C_CDI_24"),
                      negmood_phys=c("F_T1_C_CDI_1","-F_T1_C_CDI_9","-F_T1_C_CDI_10","-F_T1_C_CDI_15" , "F_T1_C_CDI_16" , "-F_T1_C_CDI_17" ,
                                     "F_T1_C_CDI_18","-F_T1_C_CDI_26" , "-F_T1_C_CDI_27"), 
                      neg_selfest=c("-F_T1_C_CDI_2","-F_T1_C_CDI_6","-F_T1_C_CDI_7" , "F_T1_C_CDI_8", "F_T1_C_CDI_13","-F_T1_C_CDI_24"),
                      func_cCDI=c("F_T1_C_CDI_3" , "F_T1_C_CDI_4","-F_T1_C_CDI_12", "-F_T1_C_CDI_14","-F_T1_C_CDI_20","F_T1_C_CDI_22", "-F_T1_C_CDI_23", 
                                  "F_T1_C_CDI_28",
                                  "F_T1_C_CDI_5","F_T1_C_CDI_11","F_T1_C_CDI_19","F_T1_C_CDI_21", "F_T1_C_CDI_25"),
                      ineff=c("F_T1_C_CDI_3" , "F_T1_C_CDI_4","-F_T1_C_CDI_12", "-F_T1_C_CDI_14","-F_T1_C_CDI_20","F_T1_C_CDI_22", "-F_T1_C_CDI_23", 
                              "F_T1_C_CDI_28"),
                      intprobs=c("F_T1_C_CDI_5","F_T1_C_CDI_11","F_T1_C_CDI_19","F_T1_C_CDI_21", "F_T1_C_CDI_25"))

CDIscored <- scoreItems(keys.listcCDI,child_CDI, impute= "mean", totals=TRUE, min=0,max=2) # note - FALSE:average, TRUE:sum
CDIscored$scores #The scores themselves
child_CDI_totals <- as.data.frame(CDIscored$scores)#the totals
child_CDI <- cbind(child_CDI, child_CDI_totals)#raw scores and totals together


# This step excludes any participants who have less than 70% of the subscale items present 
# negmood
child_CDI[which(rowSums(is.na(child_CDI[, c("F_T1_C_CDI_1","F_T1_C_CDI_9","F_T1_C_CDI_10","F_T1_C_CDI_15" , "F_T1_C_CDI_16" , "F_T1_C_CDI_17" ,
                                            "F_T1_C_CDI_18","F_T1_C_CDI_26" , "F_T1_C_CDI_27")]))>2),"negmood_phys"] <- NA
#neg_selfest
child_CDI[which(rowSums(is.na(child_CDI[, c("F_T1_C_CDI_2","F_T1_C_CDI_6","F_T1_C_CDI_7" , "F_T1_C_CDI_8", "F_T1_C_CDI_13","F_T1_C_CDI_24")]))>1),"neg_selfest"] <- NA

#ineff
child_CDI[which(rowSums(is.na(child_CDI[, c("F_T1_C_CDI_3" , "F_T1_C_CDI_4","F_T1_C_CDI_12", "F_T1_C_CDI_14","F_T1_C_CDI_20","F_T1_C_CDI_22", "F_T1_C_CDI_23", 
                                            "F_T1_C_CDI_28")]))>2),"ineff"] <- NA
#intprobs
child_CDI[which(rowSums(is.na(child_CDI[, c("F_T1_C_CDI_5","F_T1_C_CDI_11","F_T1_C_CDI_19","F_T1_C_CDI_21", "F_T1_C_CDI_25")]))>1),"intprobs"] <- NA



#Transforming them into binary variables
child_CDI$negmood_phys_bin <- car::recode(child_CDI$negmood_phys, "0='0'; NA=NA;
                                     else='1'")
child_CDI$neg_selfest_bin <- car::recode(child_CDI$neg_selfest, "0='0'; NA=NA;
                                    else='1'")
child_CDI$ineff_bin <- car::recode(child_CDI$ineff, "0='0'; NA=NA;
                              else='1'")
child_CDI$intprobs_bin <- car::recode(child_CDI$intprobs, "0='0'; NA=NA;
                                 else='1'")

# SCAS -----------------------------------------------------------------------------------------------------------------------------------
SCAS$ID <- NULL

#reassign -9s and -3s to NA
SCAS[SCAS == -9]  <- NA
SCAS[SCAS == -3]  <- NA

#scoring code from psych package. 
keys.listSCAS <- list(totalSCAS=c("F_T1_C_SCAS_1","F_T1_C_SCAS_2","F_T1_C_SCAS_3","F_T1_C_SCAS_4",
                                  "F_T1_C_SCAS_5","F_T1_C_SCAS_6","F_T1_C_SCAS_7","F_T1_C_SCAS_8",  
                                  "F_T1_C_SCAS_9","F_T1_C_SCAS_10","F_T1_C_SCAS_12","F_T1_C_SCAS_13", 
                                  "F_T1_C_SCAS_14","F_T1_C_SCAS_15","F_T1_C_SCAS_16","F_T1_C_SCAS_18", 
                                  "F_T1_C_SCAS_19","F_T1_C_SCAS_20","F_T1_C_SCAS_21", "F_T1_C_SCAS_22",
                                  "F_T1_C_SCAS_23","F_T1_C_SCAS_24","F_T1_C_SCAS_25","F_T1_C_SCAS_27",
                                  "F_T1_C_SCAS_28","F_T1_C_SCAS_29","F_T1_C_SCAS_30","F_T1_C_SCAS_32",
                                  "F_T1_C_SCAS_33","F_T1_C_SCAS_34","F_T1_C_SCAS_35","F_T1_C_SCAS_36",
                                  "F_T1_C_SCAS_37","F_T1_C_SCAS_39","F_T1_C_SCAS_40","F_T1_C_SCAS_41",
                                  "F_T1_C_SCAS_42","F_T1_C_SCAS_44"),
                      scasseps=c("F_T1_C_SCAS_5","F_T1_C_SCAS_8","F_T1_C_SCAS_12","F_T1_C_SCAS_15" , 
                                 "F_T1_C_SCAS_16","F_T1_C_SCAS_44"),
                      scassoc=c("F_T1_C_SCAS_6","F_T1_C_SCAS_7","F_T1_C_SCAS_9","F_T1_C_SCAS_10" , 
                                "F_T1_C_SCAS_29","F_T1_C_SCAS_35"),
                      scasocd=c("F_T1_C_SCAS_14","F_T1_C_SCAS_19","F_T1_C_SCAS_27","F_T1_C_SCAS_40" , 
                                "F_T1_C_SCAS_41","F_T1_C_SCAS_42"),
                      scasopanicag=c("F_T1_C_SCAS_13","F_T1_C_SCAS_21","F_T1_C_SCAS_28","F_T1_C_SCAS_30" , 
                                     "F_T1_C_SCAS_32","F_T1_C_SCAS_34","F_T1_C_SCAS_36","F_T1_C_SCAS_37","F_T1_C_SCAS_39"),
                      scasphysinj=c("F_T1_C_SCAS_2","F_T1_C_SCAS_18","F_T1_C_SCAS_23","F_T1_C_SCAS_25" , 
                                    "F_T1_C_SCAS_33"),
                      scasgad=c("F_T1_C_SCAS_1","F_T1_C_SCAS_3","F_T1_C_SCAS_4","F_T1_C_SCAS_20" , 
                                "F_T1_C_SCAS_22","F_T1_C_SCAS_24"))

SCASscored <- scoreItems(keys.listSCAS, SCAS, impute= "mean", totals=TRUE, min=0,max=3) # note - FALSE:average, TRUE:sum
SCASscored$scores #The scores themselves
SCAS_totals <- as.data.frame(SCASscored$scores)#just the total scores
SCAS <- cbind(SCAS, SCAS_totals)#totals and raw scores


# This step excludes any participants who have less than 70% of the subscale items present 
# scasseps
SCAS[which(rowSums(is.na(SCAS[, c("F_T1_C_SCAS_5","F_T1_C_SCAS_8","F_T1_C_SCAS_12","F_T1_C_SCAS_15" , 
                                  "F_T1_C_SCAS_16","F_T1_C_SCAS_44")]))>1),"scasseps"] <- NA
#scassoc
SCAS[which(rowSums(is.na(SCAS[, c("F_T1_C_SCAS_6","F_T1_C_SCAS_7","F_T1_C_SCAS_9","F_T1_C_SCAS_10" , 
                                  "F_T1_C_SCAS_29","F_T1_C_SCAS_35")]))>1),"scassoc"] <- NA

#scasocd
SCAS[which(rowSums(is.na(SCAS[, c("F_T1_C_SCAS_14","F_T1_C_SCAS_19","F_T1_C_SCAS_27","F_T1_C_SCAS_40" , 
                                  "F_T1_C_SCAS_41","F_T1_C_SCAS_42")]))>1),"scasocd"] <- NA
#scasopanicag
SCAS[which(rowSums(is.na(SCAS[, c("F_T1_C_SCAS_13","F_T1_C_SCAS_21","F_T1_C_SCAS_28","F_T1_C_SCAS_30" , 
                                  "F_T1_C_SCAS_32","F_T1_C_SCAS_34","F_T1_C_SCAS_36","F_T1_C_SCAS_37","F_T1_C_SCAS_39")]))>2),"scasopanicag"] <- NA

#scasphysinj
SCAS[which(rowSums(is.na(SCAS[, c("F_T1_C_SCAS_2","F_T1_C_SCAS_18","F_T1_C_SCAS_23","F_T1_C_SCAS_25" , 
                                  "F_T1_C_SCAS_33")]))>1),"scasphysinj"] <- NA

#scasgad
SCAS[which(rowSums(is.na(SCAS[, c("F_T1_C_SCAS_1","F_T1_C_SCAS_3","F_T1_C_SCAS_4","F_T1_C_SCAS_20" , 
                                  "F_T1_C_SCAS_22","F_T1_C_SCAS_24")]))>1),"scasgad"] <- NA



#####################Silent Films#########################

silent_films$ID <- NULL

#reassign -9s to NA
silent_films[silent_films == -9]  <- NA

keys.listsilentfilms <- list(silent_films_total=c("F_T1_C_SF_1","F_T1_C_SF_2","F_T1_C_SF_3","F_T1_C_SF_4","F_T1_C_SF_5","F_T1_C_SF_6"))

# Subscale totals are created by summing the items, and imputing any missing by the mean
sfscored <- scoreItems(keys.listsilentfilms, silent_films, impute= "mean", totals=TRUE, min=0,max=2) # note - FALSE:average, TRUE:sum
#The scores themselves
sf_subscale <- as.data.frame(sfscored$scores)#the totals

silent_films <- cbind(silent_films, sf_subscale)#raw scores and totals together

# This step excludes any participants who have less than 70% of the subscale items present (more than 1 item missing)
silent_films[which(rowSums(is.na(silent_films[, c("F_T1_C_SF_1","F_T1_C_SF_2","F_T1_C_SF_3","F_T1_C_SF_4","F_T1_C_SF_5","F_T1_C_SF_6")]))>1),"silent_films_total"] <- NA

# 'Switchboard' data demographics such as gender, age etc. ------------------------------------------------------------------------------

#change the variable names of switchboard dataframe
names(switchboard)[names(switchboard) == 'V2'] <- 'FACTS_ID'
names(switchboard)[names(switchboard) == 'V3'] <- 'gender'
names(switchboard)[names(switchboard) == 'V4'] <- 'DOB'
names(switchboard)[names(switchboard) == 'V8'] <- 'AxT2_date'

#delete variables don't need (Who entered, date entered, data entry complete and notes)
switchboard$V1 <- NULL
switchboard$V5 <- NULL
switchboard$V6 <- NULL
switchboard$V7 <- NULL
switchboard$V9 <- NULL

switchboard =switchboard[-1,]

#Calculate age
#change formatting of date
formatted_DOB<- as.Date(switchboard$DOB, format="%d/%m/%Y")
formatted_AxT2_date <-  as.Date(switchboard$AxT2_date , format="%d/%m/%Y")


switchboard$age <- interval(start=formatted_DOB, end = formatted_AxT2_date)  /
  duration(num=1, units = "months")

#Round to the nearest whole number (month)
switchboard$age <- round(switchboard$age)

#makes negative incorrect ages NA
#switchboard[switchboard < 0] <-  NA

#temp <-  switchboard[, "age"] < 9
#temp2<- which(temp == TRUE) 
#switchboard[temp2, "age"] <- NA

# IQ -------------------------------------------------------------------------------------------------------------------------------------------

WISC[WISC == -9]  <- NA
WISC[WISC == -3]  <- NA
WISC$ID <- NULL

WISC$Total_scaledscores <- WISC$F_T1_C_WISC_vocab_scale + WISC$F_T1_C_WISC_mat_scale + WISC$F_T1_C_WISC_sym_scale


WISC$FSIQ <- WISC$Total_scaledscores

WISC$FSIQ[] <- FSIQ$FSIQ[match(unlist(WISC$Total_scaledscores), FSIQ$Sum_scaledscores)]


# Parent demographics------------------------------------------------------------------------------------------

parent_demog$ID <- NULL
parent_demog[parent_demog == -9]  <- NA
parent_demog[parent_demog == -3]  <- NA


# SEIFA  --------------------------------------------------------------------------------------------------------------------------------

# Merging all the dataframes together by FACTS ID -----------------------------------------------------------------------------------------
list.of.data.frames = list(switchboard, empathy, silent_films, child_CDI, SCAS, WISC, parent_demog, SEIFA)
data_scored= Reduce(function(...) merge(..., by = "FACTS_ID", all=T), list.of.data.frames)

# Some basic cleaning of pilots and just select phase 2 returners -----------------------------------------------------------------------------------------

#First step is to get rid of pilots and the first 3 who have no F numbers 
data_scored_onlyFs<- data_scored[grep("F", data_scored$FACTS_ID),]

#Next, get rid of the participants who have all their data missing - these are people who did not come back for wave 2
hist(rowSums(is.na(data_scored_onlyFs)))
which(rowSums(is.na(data_scored_onlyFs))>120)

my_dataset_scored<- data_scored_onlyFs[which(rowSums(is.na(data_scored_onlyFs))<120),]

write.csv(my_dataset_scored, file = "scored_data/cca_data_scored.csv")


# Select what's relevant for CCA -------------------------------------------------------------------------------------------------------------

cca_maindata<- dplyr::select(my_dataset_scored, FACTS_ID,
                             aff_shar, cog_emp,emp_conc, emp_dist,silent_films_total, 
                             totalCDI, totalSCAS,
                             negmood_phys_bin, neg_selfest_bin, ineff_bin, intprobs_bin, 
                             negmood_phys, neg_selfest, ineff, intprobs,
                             scasseps, scassoc, scasphysinj, scasgad, scasocd, scasopanicag, 
                             gender, FSIQ, F_T1_P_education, seifa_irsad_aus_percent, age)

#Allows having a look at what questionnaires/subscales people missing
missingrows<- which(rowSums(is.na(cca_maindata))>0)
is.na(cca_maindata[missingrows,])      

#Then get rid of any missing data
cca_maindata <- cca_maindata[rowSums(is.na(cca_maindata))<1, ]

# SAVING and output -----------------------------------------------------------------------------------------------
# write the scored and merged data to a csv file


write.csv(cca_maindata, file = "scored_data/cca_maindata.csv")

