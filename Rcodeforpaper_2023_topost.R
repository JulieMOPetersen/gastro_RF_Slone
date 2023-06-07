install.packages("haven")
library(haven)
alldata <- read_sas("C:\\Machine Learning\\SloneBDS\\data\\Gastroschisis\\data4paper.sas7bdat")

summary(alldata$Center)

paper <- read.csv("C:\\Machine Learning\\SloneBDS\\data\\Gastroschisis\\data4paper20210305.csv")

center <- alldata$Center

paper <- cbind(center, paper)

summary (paper)

paper <- within(paper, {
  gastro <- factor(gastro)
  center <- factor (center)
  SSRI4to8wks2 <- factor(SSRI4to8wks2)
  planned <- factor(planned)
  bmicat <- factor(bmicat, ordered = TRUE)
  menarchecat <- factor(menarchecat, ordered = TRUE)
  currsmoker <- factor(currsmoker)
  feverever <- factor(feverever)
  genitinf <- factor(genitinf)
  MAsian <- factor(MAsian)
  MOther <- factor (MOther)
  FAsian <- factor(FAsian)
  FOther <- factor(FOther)
  mblack <- factor(mblack)
  mnative <- factor(mnative)
  fblack <- factor(fblack)
  fnative <- factor(fnative)
  fhispanic <-factor (fhispanic)
  mhispanic <- factor (mhispanic)
  unemployedparents <- factor(unemployedparents, ordered = TRUE)
  prnteduc4new <- factor(prnteduc4new, ordered = TRUE)
  momage4 <- factor (momage4, ordered = TRUE)
  dadage4 <- factor (dadage4, ordered = TRUE)
  parity2 <-factor (parity2)
  nottogether <- factor(nottogether)
  MJ2 <-factor(MJ2)
  OC2<-factor(OC2)
  sumofff3<-factor(sumofff3, ordered = TRUE)
  sumoffruitveg4 <- factor(sumoffruitveg4, ordered = TRUE)
  adjustedpol4<-factor (adjustedpol4)
  adjustedchol31 <-factor (adjustedchol31)
  adjustedchol33 <-factor (adjustedchol33)
  adjustedsfat2 <-factor(adjustedsfat2)
  famincome3 <- factor (famincome3, ordered = TRUE)
  peridrinkcat3 <-factor (peridrinkcat3, ordered = TRUE)
  shortipinew <-factor (shortipinew)
  adjustedtfat2 <- factor (adjustedtfat2)
  adjustedmonofat2 <- factor (adjustedmonofat2)
  adjustedsod2 <- factor (adjustedsod2)
  adjustedafat31 <-factor (adjustedafat31)
  adjustedafat33 <-factor (adjustedafat33)
  FAsupp30 <-factor (FAsupp30)
  FAsupp31 <- factor (FAsupp31)
  nsaidexposed1_time1to31 <- factor(nsaidexposed1_time1to31)
  nsaidexposed1_time1to39 <- factor(nsaidexposed1_time1to39)
})

summary (paper)

if (!require(party, quietly=TRUE)) {install.packages('party', dependencies=TRUE); library('party')}

mycontrols2 <- ctree_control(testtype = 'Univariate', mincriterion = 0.99)
set.seed(3000)
mytree <- ctree(gastro ~., data=paper, controls=mycontrols2)
plot(mytree, drop_terminal=TRUE, inner_panel=node_inner(mytree, pval=FALSE), type='simple')
treedat.w <- data.frame(TerminalIDs=factor(where(mytree)),
                        gastro.prob.t=sapply(treeresponse(mytree), function(x) x[2]),
                        row.names=rownames(paper))

mycontrols <- cforest_unbiased(ntree=500, mtry=5, minsplit=20)
set.seed(3000)
myrf <- cforest(gastro ~ ., data = paper, controls=mycontrols)

saveRDS(myrf, file = "C:\\Machine Learning\\SloneBDS\\data\\Gastroschisis\\myrf.Rds")

#New code

treedat.w <- transform(treedat.w, gastro.prob.rf=sapply(treeresponse(myrf, OOB=TRUE), function(x) x[,2])) # Save gastro probabilities
write.csv(treedat.w,'C:\\Machine Learning\\SloneBDS\\data\\Gastroschisis\\treedat.finalpaperoverall2022.csv')

# VARIABLE IMPORTANCE #
set.seed(3000)
myvarimp <- varimpAUC(myrf)
dotchart(sort(myvarimp), xlab="Variable Importance (AUC)", col='darkblue', pch=16, cex=1.1)
abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)

print(myvarimp)

myvarimp

myvarimpdf <- data.frame(myvarimp)

labels = c("Study Center", 
           "Asian Maternal Race", 
           "Other Maternal Race",
           "Asian Paternal Race",
           "Other Paternal Race",
           "SSRI Use",
           "Black Maternal Race",
           "Native Maternal Race",
           "Black Paternal Race",
           "Native Paternal Race",
           "Planned Pregnancy",
           "Prepregnancy BMI",
           "Age at Menarche",
           "Smoking",
           "Unemployed Parents",
           "Maternal Age",
           "Paternal Age",
           "Parity",
           "Parents Not Together",
           "Marijuana Use",
           "Oral Contraceptive Use",
           "Fast Food/Processed Food Intake",
           "Fruit and Vegetables Intake",
           "Household Income",
           "Alcohol Use",
           "No Folic Acid Use",
           "Folic Acid <400 mcg/day",
           "Any NSAID Use",
           "Unknown NSAID Use",
           "Total Fat Intake",
           "Saturated Fat Intake",
           "Sodium Intake",
           "Monounsaturated Fat Intake",
           "Fever",
           "Genitourinary Infection",
           "Cholesterol Intake (Q1)",
           "Cholesterol Intake (Q4)",
           "Animal Fat Intake (Q1)",
           "Animal Fat Intake (Q4)",
           "Polyunsaturated Fat Intake",
           "Paternal Ethnicity",
           "Maternal Ethnicity",
           "Parental Education",
           "Short Interpregnancy Interval")

labelsdf <- data.frame(labels)

var_implabel <- cbind(labelsdf, myvarimpdf)

var_implabel

#myvarimp2 <- varimp(myrf)
#dotchart(sort(myvarimp2), xlab="Variable Importance (Standard)", col='darkblue', pch=16, cex=1.1)
#abline(v=abs(min(myvarimp2)), col='red', lty='longdash', lwd=2)

var_implabel2 <- var_implabel[order(var_implabel$myvarimp),]
var_implabel2

saveRDS(var_implabel2, file = "C:\\Machine Learning\\SloneBDS\\data\\Gastroschisis\\var_implabel2.Rds")

dotchart(var_implabel2$myvarimp, labels =var_implabel2$labels, xlab="Variable Importance (AUC)", col='black', pch=16, cex=1.1)
abline(v=abs(min(var_implabel2$myvarimp)), col='grey', lty='longdash', lwd=2)

summary(treedat.w$gastro.prob.rf)

# PREDICTION #
y_hat_oob <- predict(myrf, OOB=TRUE)
contab <- table(paper$gastro, y_hat_oob, dnn=list('Observed Gastroschisis', 'Predicted Gastroschisis'))
cat('Confusion table:\n'); contab; cat('\n')
cat('False negative rate: '); round(contab[2,1] / rowSums(contab)[2] * 100); cat('\n')
cat('False positive rate: '); round(contab[1,2] / rowSums(contab)[1] * 100); cat('\n')
cat('Total error rate: '); round((contab[2,1] + contab[1,2]) / nrow(paper) * 100); cat('\n')

# CALCULATE AREA UNDER THE CURVE #
if (!require(ROCR, quietly=TRUE)) {install.packages('ROCR', dependencies=TRUE); library('ROCR')} # Package needed for AUC functions
preds <- prediction(treedat.w$gastro.prob.rf, paper$gastro)
perf <- performance(preds,"tpr","fpr")
cat('AUC: '); round(as.numeric(performance(preds,"auc")@y.values), 2); cat('\n')
plot(perf,col='red',lwd=3)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
install.packages("pROC")
library(pROC)
roc_obj <- roc(paper$gastro, treedat.w$gastro.prob.rf)
auc(roc_obj)

#prepare data for tmle

paperimpute <- read.csv("C:\\Machine Learning\\SloneBDS\\data\\Gastroschisis\\data4paper20210305_imputed_dummy.csv")

center <- alldata$Center

paperimpute <- cbind(center, paperimpute)

summary (paperimpute)


#### TMLE ####

# INSTALL AND LOAD PACKAGES

# Writing this as if these are separate programs; you could just re-start the R session

packages <- c("tidyverse","tabplot","VIM","rpart","rpart.plot","expss","here","boot","rlang","tmle","beepr",
              
              "SuperLearner","xgboost","glmnet","KernelKnn","randomForest", "ranger","dplyr","fastDummies")

for (package in packages) {
  
  if (!require(package, character.only=T, quietly=T)) {
    
    install.packages(package,repos='http://lib.stat.cmu.edu/R/CRAN')
    
  }
  
}



for (package in packages) {
  
  library(package, character.only=T)
  
}

sessionInfo()

# Create SuperLearner library

# Create custom learners with set tuning parameters (xgboost_learner, glmnet_learner, ranger_learner, knn_learner)


ranger_learner = create.Learner("SL.ranger",params=list(min.node.size=20),                             
                                tune=list(replace=c(T,F),num.trees=500,mtry=5))



# Composing the SuperLearner library (SL.lib.2) to include: ranger with and wo replacement

SL.lib.2 <- ranger_learner$names

#TMLE models follow

set.seed(3000)
tmle.momage41 <- tmle(Y=paperimpute$gastro, A=paperimpute$momage41, 
                      W=subset(paperimpute, 
                               select=-c(momage4, 
                                         momage44, bmicat2, FAsupp09, 
                                         gastro, bmicat4, momage41, dadage41,
                                         sumofff32, famincome30, prnteduc4new1,
                                         shortipinew)),
                    
                    Q.SL.library=SL.lib.2,
                    
                    g.SL.library=SL.lib.2,
                    
                    family="binomial", V=10)

tmle.momage41

set.seed(3000)
tmle.dadage41 <- tmle(Y=paperimpute$gastro, A=paperimpute$dadage41, 
                      W=subset(paperimpute, 
                               select=-c(dadage4, 
                                         momage44, bmicat2, FAsupp09,
                                         gastro, bmicat4, momage41, dadage41,
                                         sumofff32, famincome30, prnteduc4new1,
                                         shortipinew)),
                      
                      Q.SL.library=SL.lib.2,
                      
                      g.SL.library=SL.lib.2,
                      
                      family="binomial", V=10)

tmle.dadage41

set.seed(3000)
tmle.nottogether <- tmle(Y=paperimpute$gastro, A=paperimpute$nottogether, 
                      W=subset(paperimpute, 
                               select=-c(nottogether, 
                                         gastro, bmicat4, momage41, dadage41,
                                         sumofff32, famincome30, prnteduc4new1,
                                         shortipinew)),
                      
                      Q.SL.library=SL.lib.2,
                      
                      g.SL.library=SL.lib.2,
                      
                      family="binomial", V=10)

tmle.nottogether

set.seed(3000)
tmle.prnteduc4new1 <- tmle(Y=paperimpute$gastro, A=paperimpute$prnteduc4new1, 
                         W=subset(paperimpute, 
                                  select=-c(prnteduc4new, 
                                            gastro, bmicat4, momage41, dadage41,
                                            sumofff32, famincome30, prnteduc4new1,
                                            shortipinew)),
                         
                         Q.SL.library=SL.lib.2,
                         
                         g.SL.library=SL.lib.2,
                         
                         family="binomial", V=10)

tmle.prnteduc4new1

set.seed(3000)
tmle.bmicat4 <- tmle(Y=paperimpute$gastro, A=paperimpute$bmicat4, 
                           W=subset(paperimpute, 
                                    select=-c(bmicat, 
                                              bmicat1,
                                              momage44, bmicat2, FAsupp32,
                                              gastro, bmicat4, momage41, dadage41,
                                              sumofff32, famincome30, prnteduc4new1,
                                              shortipinew)),
                           
                           Q.SL.library=SL.lib.2,
                           
                           g.SL.library=SL.lib.2,
                           
                           family="binomial", V=10)

tmle.bmicat4 #Need to make sure other estimates properly exclude the right age, BMI and FAsupp variables

set.seed(3000)
tmle.currsmoker <- tmle(Y=paperimpute$gastro, A=paperimpute$currsmoker, 
                     W=subset(paperimpute, 
                              select=-c(currsmoker, 
                                        gastro, bmicat4, momage41, dadage41,
                                        sumofff32, famincome30, prnteduc4new1,
                                        shortipinew)),
                     
                     Q.SL.library=SL.lib.2,
                     
                     g.SL.library=SL.lib.2,
                     
                     family="binomial", V=10)

tmle.currsmoker


set.seed(3000)
tmle.parity2 <- tmle(Y=paperimpute$gastro, A=paperimpute$parity2, 
                        W=subset(paperimpute, 
                                 select=-c(parity2, 
                                           gastro, bmicat4, momage41, dadage41,
                                           sumofff32, famincome30, prnteduc4new1,
                                           shortipinew)),
                        
                        Q.SL.library=SL.lib.2,
                        
                        g.SL.library=SL.lib.2,
                        
                        family="binomial", V=10)

tmle.parity2

set.seed(3000)
tmle.famincome30 <- tmle(Y=paperimpute$gastro, A=paperimpute$famincome30, 
                     W=subset(paperimpute, 
                              select=-c(famincome3, 
                                        gastro, bmicat4, momage41, dadage41,
                                        sumofff32, famincome30, prnteduc4new1,
                                        shortipinew)),
                     
                     Q.SL.library=SL.lib.2,
                     
                     g.SL.library=SL.lib.2,
                     
                     family="binomial", V=10)

tmle.famincome30

set.seed(3000)
tmle.famincome32 <- tmle(Y=paperimpute$gastro, A=paperimpute$famincome32, 
                         W=subset(paperimpute, 
                                  select=-c(famincome3, 
                                            gastro, bmicat4, momage41, dadage41,
                                            sumofff32, famincome30, famincome32, prnteduc4new1,
                                            shortipinew)),
                         
                         Q.SL.library=SL.lib.2,
                         
                         g.SL.library=SL.lib.2,
                         
                         family="binomial", V=10)

tmle.famincome32

set.seed(3000)
tmle.mblack <- tmle(Y=paperimpute$gastro, A=paperimpute$mblack, 
                         W=subset(paperimpute, 
                                  select=-c(mblack, 
                                            gastro, bmicat4, momage41, dadage41,
                                            sumofff32, famincome30, prnteduc4new1,
                                            shortipinew)),
                         
                         Q.SL.library=SL.lib.2,
                         
                         g.SL.library=SL.lib.2,
                         
                         family="binomial", V=10)

tmle.mblack


set.seed(3000)
tmle.sumofff32 <- tmle(Y=paperimpute$gastro, A=paperimpute$sumofff32, 
                    W=subset(paperimpute, 
                             select=-c(sumofff3, 
                                       gastro, bmicat4, momage41, dadage41,
                                       sumofff32, famincome30, prnteduc4new1,
                                       shortipinew)),
                    
                    Q.SL.library=SL.lib.2,
                    
                    g.SL.library=SL.lib.2,
                    
                    family="binomial", V=10)

tmle.sumofff32

set.seed(3000)
tmle.planned <- tmle(Y=paperimpute$gastro, A=paperimpute$planned, 
                       W=subset(paperimpute, 
                                select=-c(planned, 
                                          gastro, bmicat4, momage41, dadage41,
                                          sumofff32, famincome30, prnteduc4new1,
                                          shortipinew)),
                       
                       Q.SL.library=SL.lib.2,
                       
                       g.SL.library=SL.lib.2,
                       
                       family="binomial", V=10)

tmle.planned

set.seed(3000)
tmle.FAsupp32 <- tmle(Y=paperimpute$gastro, A=paperimpute$FAsupp32, 
                     W=subset(paperimpute, 
                              select=-c(FAsupp30, 
                                        bmicat1,
                                        momage44, bmicat2, FAsupp32,
                                        gastro, bmicat4, momage41, dadage41,
                                        sumofff32, famincome30, prnteduc4new1,
                                        shortipinew)),
                     
                     Q.SL.library=SL.lib.2,
                     
                     g.SL.library=SL.lib.2,
                     
                     family="binomial", V=10)

tmle.FAsupp32 #need to take the inverse for <400 mcg vs >=400 mcg

set.seed(3000)
tmle.MAsian <- tmle(Y=paperimpute$gastro, A=paperimpute$MAsian, 
                      W=subset(paperimpute, 
                               select=-c(MAsian, 
                                         gastro, bmicat4, momage41, dadage41,
                                         sumofff32, famincome30, prnteduc4new1,
                                         shortipinew)),
                      
                      Q.SL.library=SL.lib.2,
                      
                      g.SL.library=SL.lib.2,
                      
                      family="binomial", V=10)

tmle.MAsian

set.seed(3000)
tmle.MJ2 <- tmle(Y=paperimpute$gastro, A=paperimpute$MJ2, 
                    W=subset(paperimpute, 
                             select=-c(MJ2, 
                                       gastro, bmicat4, momage41, dadage41,
                                       sumofff32, famincome30, prnteduc4new1,
                                       shortipinew)),
                    
                    Q.SL.library=SL.lib.2,
                    
                    g.SL.library=SL.lib.2,
                    
                    family="binomial", V=10)

tmle.MJ2

set.seed(3000)
tmle.adjustedchol31 <- tmle(Y=paperimpute$gastro, A=paperimpute$adjustedchol31, 
                 W=subset(paperimpute, 
                          select=-c(adjustedchol31, 
                                    gastro, bmicat4, momage41, dadage41,
                                    sumofff32, famincome30, prnteduc4new1,
                                    shortipinew)),
                 
                 Q.SL.library=SL.lib.2,
                 
                 g.SL.library=SL.lib.2,
                 
                 family="binomial", V=10)

tmle.adjustedchol31

set.seed(3000)
tmle.Bothunemployed <- tmle(Y=paperimpute$gastro, A=paperimpute$Bothunemployed, 
                            W=subset(paperimpute, 
                                     select=-c(unemployedparents,
                                               adjustedmonofat21, adjustedpol41, 
                                               peridrinkcat30, Bothunemployed, gastro, 
                                               bmicat4, momage41, dadage41,
                                               sumofff32, famincome30, prnteduc4new1,
                                               shortipinew)),
                            
                            Q.SL.library=SL.lib.2,
                            
                            g.SL.library=SL.lib.2,
                            
                            family="binomial", V=10)

tmle.Bothunemployed

set.seed(3000)
tmle.peridrinkcat30 <- tmle(Y=paperimpute$gastro, A=paperimpute$peridrinkcat30, 
                            W=subset(paperimpute, 
                                     select=-c(peridrinkcat3,
                                               adjustedmonofat21, adjustedpol41, 
                                               peridrinkcat30, Bothunemployed, gastro, 
                                               bmicat4, momage41, dadage41,
                                               sumofff32, famincome30, prnteduc4new1,
                                               shortipinew)),
                            
                            Q.SL.library=SL.lib.2,
                            
                            g.SL.library=SL.lib.2,
                            
                            family="binomial", V=10)

tmle.peridrinkcat30


set.seed(3000)
tmle.adjustedpol41 <- tmle(Y=paperimpute$gastro, A=paperimpute$adjustedpol41, 
                            W=subset(paperimpute, 
                                     select=-c(adjustedpol4,
                                               adjustedmonofat21, adjustedpol41, 
                                               peridrinkcat30, Bothunemployed, gastro, 
                                               bmicat4, momage41, dadage41,
                                               sumofff32, famincome30, prnteduc4new1,
                                               shortipinew)),
                            
                            Q.SL.library=SL.lib.2,
                            
                            g.SL.library=SL.lib.2,
                            
                            family="binomial", V=10)

tmle.adjustedpol41


set.seed(3000)
tmle.FAsian <- tmle(Y=paperimpute$gastro, A=paperimpute$FAsian, 
                           W=subset(paperimpute, 
                                    select=-c(FAsian,
                                              adjustedmonofat21, adjustedpol41, 
                                              peridrinkcat30, Bothunemployed, gastro, 
                                              bmicat4, momage41, dadage41,
                                              sumofff32, famincome30, prnteduc4new1,
                                              shortipinew)),
                           
                           Q.SL.library=SL.lib.2,
                           
                           g.SL.library=SL.lib.2,
                           
                           family="binomial", V=10)

tmle.FAsian

set.seed(3000)
tmle.fblack <- tmle(Y=paperimpute$gastro, A=paperimpute$fblack, 
                    W=subset(paperimpute, 
                             select=-c(fblack,
                                       adjustedmonofat21, adjustedpol41, 
                                       peridrinkcat30, Bothunemployed, gastro, 
                                       bmicat4, momage41, dadage41,
                                       sumofff32, famincome30, prnteduc4new1,
                                       shortipinew)),
                    
                    Q.SL.library=SL.lib.2,
                    
                    g.SL.library=SL.lib.2,
                    
                    family="binomial", V=10)

tmle.fblack

set.seed(3000)
tmle.genitinf <- tmle(Y=paperimpute$gastro, A=paperimpute$genitinf, 
                    W=subset(paperimpute, 
                             select=-c(genitinf,
                                       adjustedmonofat21, adjustedpol41, 
                                       peridrinkcat30, Bothunemployed, gastro, 
                                       bmicat4, momage41, dadage41,
                                       sumofff32, famincome30, prnteduc4new1,
                                       shortipinew)),
                    
                    Q.SL.library=SL.lib.2,
                    
                    g.SL.library=SL.lib.2,
                    
                    family="binomial", V=10)

tmle.genitinf

set.seed(3000)
tmle.nsaidexposed1_time1to31 <- tmle(Y=paperimpute$gastro, A=paperimpute$nsaidexposed1_time1to31, 
                      W=subset(paperimpute, 
                               select=-c(nsaidexposed1_time1to31,
                                         adjustedmonofat21, adjustedpol41, 
                                         peridrinkcat30, Bothunemployed, gastro, 
                                         bmicat4, momage41, dadage41,
                                         sumofff32, famincome30, prnteduc4new1,
                                         shortipinew)),
                      
                      Q.SL.library=SL.lib.2,
                      
                      g.SL.library=SL.lib.2,
                      
                      family="binomial", V=10)

tmle.nsaidexposed1_time1to31


set.seed(3000)
tmle.adjustedmonofat21 <- tmle(Y=paperimpute$gastro, A=paperimpute$adjustedmonofat21, 
                                     W=subset(paperimpute, 
                                              select=-c(adjustedmonofat2,
                                                        adjustedmonofat21, adjustedpol41, 
                                                        peridrinkcat30, Bothunemployed, gastro, 
                                                        bmicat4, momage41, dadage41,
                                                        sumofff32, famincome30, prnteduc4new1,
                                                        shortipinew)),
                                     
                                     Q.SL.library=SL.lib.2,
                                     
                                     g.SL.library=SL.lib.2,
                                     
                                     family="binomial", V=10)

tmle.adjustedmonofat21


set.seed(3000)
tmle.adjustedsfat21 <- tmle(Y=paperimpute$gastro, A=paperimpute$adjustedsfat21, 
                               W=subset(paperimpute, 
                                        select=-c(adjustedsfat2,
                                                  adjustedsfat21, adjustedtfat21,
                                                  adjustedsod21,
                                                  adjustedmonofat21, adjustedpol41, 
                                                  peridrinkcat30, Bothunemployed, gastro, 
                                                  bmicat4, momage41, dadage41,
                                                  sumofff32, famincome30, prnteduc4new1,
                                                  shortipinew)),
                               
                               Q.SL.library=SL.lib.2,
                               
                               g.SL.library=SL.lib.2,
                               
                               family="binomial", V=10)

tmle.adjustedsfat21

set.seed(3000)
tmle.FAsupp31 <- tmle(Y=paperimpute$gastro, A=paperimpute$FAsupp31, 
                            W=subset(paperimpute, 
                                     select=-c(FAsupp31,
                                               adjustedsfat21, adjustedtfat21,
                                               adjustedsod21,
                                               adjustedmonofat21, adjustedpol41, 
                                               peridrinkcat30, Bothunemployed, gastro, 
                                               bmicat4, momage41, dadage41,
                                               sumofff32, famincome30, prnteduc4new1,
                                               shortipinew)),
                            
                            Q.SL.library=SL.lib.2,
                            
                            g.SL.library=SL.lib.2,
                            
                            family="binomial", V=10)

tmle.FAsupp31

set.seed(3000)
tmle.adjustedtfat21 <- tmle(Y=paperimpute$gastro, A=paperimpute$adjustedtfat21, 
                      W=subset(paperimpute, 
                               select=-c(
                                         adjustedsfat21, adjustedtfat21,
                                         adjustedsod21,
                                         adjustedmonofat21, adjustedpol41, 
                                         peridrinkcat30, Bothunemployed, gastro, 
                                         bmicat4, momage41, dadage41,
                                         sumofff32, famincome30, prnteduc4new1,
                                         shortipinew)),
                      
                      Q.SL.library=SL.lib.2,
                      
                      g.SL.library=SL.lib.2,
                      
                      family="binomial", V=10)

tmle.adjustedtfat21

set.seed(3000)
tmle.OC2 <- tmle(Y=paperimpute$gastro, A=paperimpute$OC2, 
                            W=subset(paperimpute, 
                                     select=-c(
                                       OC2,
                                       adjustedsfat21, adjustedtfat21,
                                       adjustedsod21,
                                       adjustedmonofat21, adjustedpol41, 
                                       peridrinkcat30, Bothunemployed, gastro, 
                                       bmicat4, momage41, dadage41,
                                       sumofff32, famincome30, prnteduc4new1,
                                       shortipinew)),
                            
                            Q.SL.library=SL.lib.2,
                            
                            g.SL.library=SL.lib.2,
                            
                            family="binomial", V=10)

tmle.OC2

set.seed(3000)
tmle.adjustedsod21 <- tmle(Y=paperimpute$gastro, A=paperimpute$adjustedsod21, 
                 W=subset(paperimpute, 
                          select=-c(
                            adjustedsfat21, adjustedtfat21,
                            adjustedsod21,
                            adjustedmonofat21, adjustedpol41, 
                            peridrinkcat30, Bothunemployed, gastro, 
                            bmicat4, momage41, dadage41,
                            sumofff32, famincome30, prnteduc4new1,
                            shortipinew)),
                 
                 Q.SL.library=SL.lib.2,
                 
                 g.SL.library=SL.lib.2,
                 
                 family="binomial", V=10)

tmle.adjustedsod21


#Single classification tree 
install.packages("haven")
library(haven)
alldata <- read_sas("C:\\Machine Learning\\SloneBDS\\data\\Gastroschisis\\data4paper.sas7bdat")

summary(alldata$Center)

paper <- read.csv("C:\\Machine Learning\\SloneBDS\\data\\Gastroschisis\\data4paper20210305.csv")

center <- alldata$Center

importantvar <-subset(paper, select=-c(SSRI4to8wks2, menarchecat, feverever, 
                                       MOther, FOther, mnative, fnative, fhispanic,
                                       mhispanic, sumoffruitveg4, 
                                       adjustedchol33, shortipinew, adjustedafat31, adjustedafat33,
                                       FAsupp31, nsaidexposed1_time1to39))

paper <- cbind(center, importantvar)

summary (paper)

paper <- within(paper, {
  gastro <- factor(gastro)
  center <- factor (center)
  planned <- factor(planned)
  bmicat <- factor(bmicat, ordered = TRUE)
  currsmoker <- factor(currsmoker)
  genitinf <- factor(genitinf)
  MAsian <- factor(MAsian)
  FAsian <- factor(FAsian)
  mblack <- factor(mblack)
  fblack <- factor(fblack)
  unemployedparents <- factor(unemployedparents, ordered = TRUE)
  prnteduc4new <- factor(prnteduc4new, ordered = TRUE)
  momage4 <- factor (momage4, ordered = TRUE)
  dadage4 <- factor (dadage4, ordered = TRUE)
  parity2 <-factor (parity2)
  nottogether <- factor(nottogether)
  MJ2 <-factor(MJ2)
  sumofff3<-factor(sumofff3, ordered = TRUE)
  adjustedpol4<-factor (adjustedpol4)
  adjustedchol31 <-factor (adjustedchol31)
  famincome3 <- factor (famincome3, ordered = TRUE)
  peridrinkcat3 <-factor (peridrinkcat3, ordered = TRUE)
  adjustedmonofat2 <- factor (adjustedmonofat2)
  FAsupp30 <-factor (FAsupp30)
  nsaidexposed1_time1to31 <- factor(nsaidexposed1_time1to31)
  OC2 <- factor (OC2) 
  adjustedsfat2 <- factor (adjustedsfat2) 
  adjustedtfat2 <- factor (adjustedtfat2)
  adjustedsod2 <- factor (adjustedsod2)
})

summary (paper)

install.packages ("exprss")
library(expss)

paperlabel = apply_labels(paper,
                      center = "Study Center",
                      gastro = "Case-Control Status",
                     # gastro = c("Control" = 0,
                                # "Gastroschisis Case" = 1),
                      MAsian = "Maternal Race",
                      #MAsian = c("Other Race" = 0,
                                # "Asian" = 1),
                      FAsian = "Paternal Race",
                      #FAsian = c("Other Race" = 0,
                                 #"Asian" = 1),
                      mblack = "Maternal Race",
                      #mblack = c("Other Race" = 0,
                                 #"Black" = 1),
                      momage4 = "Maternal Age",
                      #momage4 = c("13 to 18" = 1,
                                # "19 to 20" = 2,
                                # "21 to 24" = 3,
                                # "25 to 46" = 4
                                 # ),
                      dadage4 = "Paternal Age",
                      #dadage4 = c("14 to 21" = 1,
                                 # "22 to 24" = 2,
                                  # "25 to 28" = 3,
                                  # "29 to 62" = 4
                      # ),
                      bmicat = "Prepregnancy BMI",
                      # bmicat = c("<18.5 kg/m2" = 1,
                      #             "18.5–24.9" = 2,
                      #             "25.0–29.9" = 3,
                      #             "≥30.0" = 4
                      # ),
                      currsmoker = "Smoking",
                      # currsmoker = c("Never/Past" = 0,
                      #        "Current"=1),
                      unemployedparents = "Unemployed Parents",
                      # unemployedparents = c("Both Employed" = 0,
                      #                "Father Only Employed"=1,
                      #                "Mother Only Employed" = 2,
                      #                "Neither Employed"=3),
                      nottogether = "Parents Not Together",
                      # nottogether = c("Married/Partnered/Widowed" = 0,
                      #                "Single/Living Together"=1),
                      sumofff3 = "Fast Food/Processed Foods Intake",
                      # sumofff3 = c("Q1 (<3 per week)" = 0,
                      #                 "Q2-Q3 (3-6 per week)"=1,
                      #              "Q4 (≥1 every day)" =2),
                      peridrinkcat3 = "Alcohol Consumption",
                      # peridrinkcat3 = c("None or <1 per day" = 0,
                      #                  "1 per day" = 1,
                      #                   "2+ per day" = 2),
                      FAsupp30 = "No Folic Acid Supplementation",
                      # FAsupp30 = c("Any" = 0,
                      #                 "None"=1),
                      nsaidexposed1_time1to31 = "Any NSAID Use",
                      # nsaidexposed1_time1to31 = c("Any" = 0,
                      #              "None"=1),
                      adjustedmonofat2 = "Monounsaturated Fat Intake",
                      # adjustedmonofat2 = c("Q1 (<14.9 g)" = 1,
                      #                             "Q2-Q4 (≥14.9 g)"=2),
                      adjustedchol31 = "Cholesterol Intake",
                      # adjustedchol31 = c("Q1 (<174.5)" = 1,
                      #                      "Q2-Q4 (≥174.5)"=0),
                      prnteduc4new = "Parental Education",
                      # prnteduc4new = c("At least one parent did not complete HS and no parent had a college education"=1,
                      #                  "Both high school graduates or one completed college but the other did not complete HS"=2,
                      #                       "One completed HS and one completed college" = 3,
                      #                       "Both college educated"=4),
                      genitinf = "Genitourinary Infection"
# , 
                      # genitinf = c("Any" = 0,
                      #              "None"=1)
)

summary (paperlabel)

if (!require(party, quietly=TRUE)) {install.packages('party', dependencies=TRUE); library('party')}
mycontrols2 <- ctree_control(teststat = 'max', testtype = 'Univariate', mincriterion = 0.95, minbucket=20)
mytree <- ctree(gastro ~., data=paperlabel, controls=mycontrols2)
plot(mytree, drop_terminal=TRUE, labels = TRUE, inner_panel=node_inner(mytree, pval=FALSE), type='simple')

