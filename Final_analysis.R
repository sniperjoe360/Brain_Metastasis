#######################################################################
# read the no chemo and the chemo data sets in for all NSCLC patients #
# also read in the symptom dataset                            #
#######################################################################

filePath1 = 'H:\\Kondziolka Data\\Final Analysis 1-20-2017\\Final_Database_Chemo_11_28_Normalized_SPW.csv'
filePath2 = 'H:\\Kondziolka Data\\Final Analysis 1-20-2017\\Final_Database_No_Chemo_11_28_Normalized_SPW.csv'
filePath3 = 'H:\\Kondziolka Data\\Final Analysis 1-20-2017\\SymptomData.csv'
filePath4 =  'H:\\Kondziolka Data\\Final Analysis 1-20-2017\\'

master_nochemo = read.csv(filePath2)
master_chemo   = read.csv(filePath1)
master_symptom = read.csv(filePath3)

library(RColorBrewer)
library(gplots)
###############################
# merge the symptom data only #
# with the MRNs known in the  #
# lung database               # 
#                             #
# this is done in a loop      #  
###############################

date_brainmet<-as.Date(master_symptom['first_noted_brainmet'][[1]], format = "%m/%d/%Y")
date_symptom<-as.Date(master_symptom['first_noted_dt'][[1]], format = "%m/%d/%Y")

test<-master_symptom[complete.cases(master_symptom),]
dev.new()
par( mfrow = c( 2, 3 ) )
totalsymptom2 = NULL
totalsymptom = NULL
datevector = c(10,30,120,240,360,600)
for (j in datevector)
{
print(j)

# CHANGE THIS ROW FLAG TO SUIT YOUR DATE RANGE
flag =  ((date_symptom - date_brainmet < j) & (date_symptom - date_brainmet > 0))
print(length(flag))
# filter the data now with row flag
MASTER_unique <- master_symptom[flag,]
print(dim(MASTER_unique))
MASTER_unique <- MASTER_unique[complete.cases(MASTER_unique),]
print(dim(MASTER_unique))

# sum the incidences over each unique patients

# the unique MRN
MRN = master_nochemo$MRN
print(length(unique(MRN)))

# data frame to store data in during loop
unique_pts = NULL

# for every unique patient
for (i in 1:length(unique(MRN)))
{
# create a dataframe with all the symptoms from that patient
dataf <- MASTER_unique[unique(MRN)[i] == MASTER_unique$MRN,c(10:77)]
unique_pts <- rbind(colSums(dataf[1:68]),unique_pts)
}
unique_pts_f = data.frame(unique_pts)
names(unique_pts_f) <- names(dataf)[1:68]
unique_pts_f$MRN = unique(MRN)
print(dim(unique_pts_f))

unique_pts_f2<-unique_pts_f[unique_pts_f$MRN %in% master_nochemo$MRN,]


# merge with the master document
master_nochemo_sx<-merge(unique_pts_f2,master_nochemo,by="MRN",all=TRUE)

print(dim(master_nochemo_sx))

master_nochemo_sx[is.na(master_nochemo_sx)] <- 0
x11()
print(colSums(master_nochemo_sx[,c(2:69)]))
totalsymptom = rbind(colSums(master_nochemo_sx[,c(2:69)]),totalsymptom)
totalsymptom2 = rbind(colSums(master_nochemo_sx[,c(2:69)]>0),totalsymptom2)
par(mar=c(1,1,1,1))
heatmap.2(t(as.matrix(master_nochemo_sx[,c(2:69)])),dendrogram='none',col=c(1,2,3,4,5,6,7,8),trace='none',Colv=FALSE,Rowv=FALSE,breaks=0:8,main=paste('Symptoms for Cohort of patients by ',j,' days'),key=1,)
# 9843600 
}

#####################
##recode tx modality for Surgery+GK and Surgery+WBrt 
##variable= FINAL_TX_GROUP
##filename= Sansosti_Final_ Nochemo_9.7.2016

attach(master_nochemo)

install.packages("tableone")
library(tableone)
install.packages("ReporteRs")
library(ReporteRs)
install.packages("magrittr")
library(magrittr)

listVars<-(c("Age", "Sex", "FINAL_TX_GROUP", "FinalCharlsonScore", "marital_st", "race"))
catVars<-(c( "Sex", "marital_st", "race")) 
table1_tx <- CreateTableOne(vars = listVars, data = master_nochemo, factorVars = catVars, strata = c("Molecular_Marker"),includeNA = TRUE)
table1_tx
 
table1_tx<-print(table1_tx) #, need to be matrix or data frame
docx( ) %>% 
  addFlexTable(table1_tx %>%
  FlexTable(add.rownames = TRUE ) %>%
  setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ) %>%
  writeDoc(file = paste(filePath4,"Demographics2.docx"))
 ########################################################################################
 ##Utilization Table 
  
 
#Treatment 
 listVars<-(c("CT_Norm", "PET.CT_Norm", "MRI_Norm", "X.ray_Norm","Number.of.ED.Visits_Norm", "Days.Inpatient_Norm", "Days_FU"
,"No.of.Inpatient.Visits_Norm","Discharged.to","payor"))
catVars<-(c( "Discharged.to","payor")) 
table3_Utilization <- CreateTableOne(vars = listVars, data = master_nochemo, strata = c("Molecular_Marker"),includeNA = TRUE)
table3_Utilization
 
table3_Utilization <- print(table3_Utilization) #, need to be matrix or data frame
docx( ) %>% 
  addFlexTable(table3_Utilization %>%
  FlexTable(add.rownames = TRUE ) %>%
  setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ) %>%
  writeDoc(file = paste(filePath4,"NormalizedUtilization_2.docx"))


#Cost tables

listVars<-(c("Direct_Drug_Cost_Norm", "Total_Cost_Inpatient_and_TX_Norm", "Total_cost_MRI_Norm", "Total_cost_CT_Norm", "Total_cost_PETCT_Norm", "Total_cost_Xray_Norm", "Total_cost_ED_Norm")) 
catVars<-(c( "Discharged.to","payor")) 
table3_Cost_Hist <- CreateTableOne(vars = listVars, data = master_nochemo, strata = c("Molecular_Marker"),includeNA = TRUE)
table3_Cost_Hist

table3_Cost_Hist <- print(table3_Cost_Hist) #, need to be matrix or data frame
docx( ) %>% 
  addFlexTable(table3_Cost_Hist %>%
  FlexTable(add.rownames = TRUE ) %>%
  setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ) %>%
  writeDoc(file = paste(filePath4,"NormalizedCost_2.docx"))
    

  
#######################################################################
##Survival Analysis 
  
install.packages('survival')
library('survival')

#master_nochemo = master_nochemo[master_nochemo ==1,]
attach(master_nochemo)


#find the follow up and censor time
followup_time = master_nochemo["Days_FU"][[1]]

followup_time <- (followup_time / 365) * 12
censor  = master_nochemo["Surv_Censor"][[1]]

# create survival object
allpts <- survfit(Surv(followup_time, Surv_Censor)~ 1)
summary(allpts)
plot(allpts, xlab="Time", ylab="Survival Probability",lwd=2)



# stratify by some variable
subset <- survfit(Surv(followup_time, censor)~FINAL_TX_GROUP)
plot(subset, 
xlab="Months", 
ylab="Survival Probability",
main='Overall Survival',
lwd=7,
col=c(1:length(names(subset$strata))),
lty=1,
cex.axis = 2,
cex.lab = 1.5,
cex.main = 2,
xmax = 25)


legend(18, 1.0, c("GammaKnife","Observation","Resection", "Resection and GK", "Resection and WBRT","WBRT"),lwd=4,col=c(1:length(names(subset$strata))),lty=1)

T
followup2<- followup_time[FINAL_TX_GROUP == "No Treatment" | FINAL_TX_GROUP == "SURGERY"]
censor2<- censor[FINAL_TX_GROUP == "No Treatment" | FINAL_TX_GROUP == "SURGERY"]
FINAL_TX_GROUP2<- FINAL_TX_GROUP[FINAL_TX_GROUP == "No Treatment" | FINAL_TX_GROUP == "SURGERY"]

# do log rank test
logrank <- survdiff(Surv(followup2, censor2)~FINAL_TX_GROUP2)
pval <- 1 - pchisq(logrank$chisq, length(logrank$n) - 1)
pval

text(20,0.14,paste("Observation vs. Resection logrank p =",round(pval,3),sep = ' '))

followup2<- followup_time[FINAL_TX_GROUP == "No Treatment" | FINAL_TX_GROUP == "SURGERY_GK"]
censor2<- censor[FINAL_TX_GROUP == "No Treatment" | FINAL_TX_GROUP == "SURGERY_GK"]
FINAL_TX_GROUP2<- FINAL_TX_GROUP[FINAL_TX_GROUP == "No Treatment" | FINAL_TX_GROUP == "SURGERY_GK"]

# do log rank test
logrank <- survdiff(Surv(followup2, censor2)~FINAL_TX_GROUP2)
pval <- 1 - pchisq(logrank$chisq, length(logrank$n) - 1)
pval

text(20,0.10,paste("Observation vs. Resection_GK logrank p =",round(pval,3),sep = ' '))

followup2<- followup_time[FINAL_TX_GROUP == "No Treatment" | FINAL_TX_GROUP == "SURGERY_WBRT"]
censor2<- censor[FINAL_TX_GROUP == "No Treatment" | FINAL_TX_GROUP == "SURGERY_WBRT"]
FINAL_TX_GROUP2<- FINAL_TX_GROUP[FINAL_TX_GROUP == "No Treatment" | FINAL_TX_GROUP == "SURGERY_WBRT"]

# do log rank test
logrank <- survdiff(Surv(followup2, censor2)~FINAL_TX_GROUP2)
pval <- 1 - pchisq(logrank$chisq, length(logrank$n) - 1)
pval

text(20,0.06,paste("Observation vs. Resection_WBRT logrank p =",round(pval,3),sep = ' '))

# followup2<- followup_time[FINAL_TX_GROUP == "GK" | FINAL_TX_GROUP == "SURGERY_TRIPLE"]
# censor2<- censor[FINAL_TX_GROUP == "GK" | FINAL_TX_GROUP == "SURGERY_TRIPLE"]
# FINAL_TX_GROUP2<- FINAL_TX_GROUP[FINAL_TX_GROUP == "GK" | FINAL_TX_GROUP == "SURGERY_TRIPLE"]

# # do log rank test
# logrank <- survdiff(Surv(followup2, censor2)~FINAL_TX_GROUP2)
# pval <- 1 - pchisq(logrank$chisq, length(logrank$n) - 1)
# pval

# text(20,0.050,paste("GK vs. Resection_Triple_Therapy logrank p =",round(pval,3),sep = ' '))

followup2<- followup_time[FINAL_TX_GROUP == "No Treatment" | FINAL_TX_GROUP == "WBRT"]
censor2<- censor[FINAL_TX_GROUP == "No Treatment" | FINAL_TX_GROUP == "WBRT"]
FINAL_TX_GROUP2<- FINAL_TX_GROUP[FINAL_TX_GROUP == "No Treatment" | FINAL_TX_GROUP == "WBRT"]

# do log rank test
logrank <- survdiff(Surv(followup2, censor2)~FINAL_TX_GROUP2)
pval <- 1 - pchisq(logrank$chisq, length(logrank$n) - 1)
pval

text(20,0.02,paste("Observation vs. WBRT logrank p =",round(pval,2),sep = ' '))

followup2<- followup_time[FINAL_TX_GROUP == "No Treatment" | FINAL_TX_GROUP == "GK"]
censor2<- censor[FINAL_TX_GROUP == "No Treatment" | FINAL_TX_GROUP == "GK"]
FINAL_TX_GROUP2<- FINAL_TX_GROUP[FINAL_TX_GROUP == "No Treatment" | FINAL_TX_GROUP == "GK"]

# do log rank test
logrank <- survdiff(Surv(followup2, censor2)~FINAL_TX_GROUP2)
pval <- 1 - pchisq(logrank$chisq, length(logrank$n) - 1)
pval

text(20,-0.02,paste("Observation vs. GK logrank p =",round(pval,3),sep = ' '))



	
# (5) Cox Proportional Hazards Model 
	# - Dependent variable: Survival
	# - Independent variables: Surv ~ ... Age, Sex, CPS, Histology 
	# - Do for lung and melanoma and breast .... Age, Sex, CPS, Marker Status (BRAF / Molecular_Marker)
	

# Cox Proportional Hazard
# define what variables you want in your cox analysis



install.packages(c('stargazer'))
library(stargazer)

attach(master_nochemo)
coxmodel <- coxph(Surv(followup_time, censor)~Sex + Age + CharlsonScore + FINAL_TX_GROUP)
stargazer(coxmodel,type='text')

newtable=cbind(summary(coxmodel)$coefficients[,c(2,5)],summary(coxmodel)$conf.int[,c(3,4)])
newtable <- print(newtable) #, need to be matrix or data frame
docx( ) %>% 
  addFlexTable(newtable %>%
  FlexTable(add.rownames = TRUE ) %>%
  setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ) %>%
  writeDoc(file = "H:/Kondziolka Data/COXMODEL_Sansosti_7.22.2016.docx")
