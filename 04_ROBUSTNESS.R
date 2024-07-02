

####################################################################
################## ROBUSTNESS ##############################
####################################################################


####################################################################
##### Table A3: Parental Responses and Cognitive Ability Control ###
####################################################################

# Re-extract sample with cognitive ability

myvars <- c("randomfamid", "random", "twin",  # IDS
            "risk1",  # indep var
            "negfeelings3", "negfeelings4", "negfeelings7", # dep vars
            "discipline3", "discipline4", "discipline7", # dep vars
            "male", "dparca1") # controls

data<-data_complete[myvars]

# NEGATIVE EMOTIONAL RESPONSES # 

model1<- plm(negfeelings3 ~ risk1+ male +dparca1 , 
               data = data,
               index = c("randomfamid"), 
               model = "within")

model2<- plm(negfeelings4 ~ risk1+ male + dparca1, 
               data = data,
               index = c("randomfamid"), 
               model = "within")

model3<- plm(negfeelings7 ~ risk1+ male +dparca1, 
               data = data,
               index = c("randomfamid"), 
               model = "within")

# DISCIPLINE BEHAVIOURS #

model4<- plm(discipline4 ~ risk1+ male + dparca1, 
               data = data,
               index = c("randomfamid"), 
               model = "within")

model5<- plm(discipline3 ~ risk1+ male +dparca1 , 
               data = data,
               index = c("randomfamid"), 
               model = "within")

model6<- plm(discipline7 ~ risk1+ male +dparca1, 
               data = data,
               index = c("randomfamid"), 
               model = "within")

# Export table

stargazer(model1, model2, model3, model4, model5, model6,
          #note you have to specify type
          type = "html",
          #note that the argument is "out" not "file"
          out="tableA3.doc",
          star.cutoffs = c(0.05, 0.01, 0.001))


####################################################################
##### Table A4: Parental Responses to Birth Weight  ################
####################################################################

# Re-extract sample with variables of interest

myvars <- c("randomfamid", "random", "twin",  # IDS
            "risk1",  # indep var
            "negfeelings3", "negfeelings4", "negfeelings7", # dep vars
            "discipline3", "discipline4", "discipline7", # dep vars
            "male", "arkidgr1") # controls

data<-data_complete[myvars]

# NEGATIVE EMOTIONAL RESPONSES # 

model7<- plm(negfeelings3 ~  arkidgr1+ male, 
               data = data,
               index = c("randomfamid"), 
               model = "within")

model8<- plm(negfeelings4 ~ arkidgr1+ male, 
               data = data,
               index = c("randomfamid"), 
               model = "within")

model9<- plm(negfeelings7 ~ arkidgr1+ male, 
               data = data,
               index = c("randomfamid"), 
               model = "within")

# DISCIPLINE BEHAVIOURS #

model10<- plm(discipline3 ~arkidgr1+ male, 
               data = data,
               index = c("randomfamid"), 
               model = "within")

model11<- plm(discipline4 ~ arkidgr1+ male, 
               data = data,
               index = c("randomfamid"), 
               model = "within")

model12<- plm(discipline7 ~ arkidgr1+ male, 
               data = data,
               index = c("randomfamid"), 
               model = "within")

# Export table
stargazer(model7, model8, model9, model10, model11, model12,
          type = "html",
          out="tableA4.doc",
          star.cutoffs = c(0.05, 0.01, 0.001))



############################################################################
##### Table A5: Parental Responses and Birth Weight Control   ################
############################################################################

# Re-extract sample with variables of interest

myvars <- c("randomfamid", "random", "twin",  # IDS
            "risk1",  # indep var
            "negfeelings3", "negfeelings4", "negfeelings7", # dep vars
            "discipline3", "discipline4", "discipline7", # dep vars
            "male", "arkidgr1") # controls

data<-data_complete[myvars]

# NEGATIVE EMOTIONAL RESPONSES # 

model13<- plm(negfeelings3 ~  risk1 + arkidgr1+ male, 
             data = data,
             index = c("randomfamid"), 
             model = "within")

model14<- plm(negfeelings4 ~ risk1 +arkidgr1+ male, 
             data = data,
             index = c("randomfamid"), 
             model = "within")

model15<- plm(negfeelings7 ~ risk1 +arkidgr1+ male, 
             data = data,
             index = c("randomfamid"), 
             model = "within")

# DISCIPLINE BEHAVIOURS #

model16<- plm(discipline3 ~risk1 +arkidgr1+ male, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

model17<- plm(discipline4 ~ risk1 +arkidgr1+ male, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

model18<- plm(discipline7 ~ risk1 +arkidgr1+ male, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

# Export table
stargazer(model13, model14, model15, model16, model17, model18,
          type = "html",
          out="tableA5.doc",
          star.cutoffs = c(0.05, 0.01, 0.001))


############################################################################
##### Table A6: Parental Responses and Gender Interaction   ################
############################################################################

# Re-extract sample with variables of interest

myvars <- c("randomfamid", "random", "twin",  # IDS
            "risk1",  # indep var
            "negfeelings3", "negfeelings4", "negfeelings7", # dep vars
            "discipline3", "discipline4", "discipline7", # dep vars
            "male") # controls

data<-data_complete[myvars]

# NEGATIVE EMOTIONAL RESPONSES # 

model19<- plm(negfeelings3 ~ risk1 *  male, 
               data = data,
               index = c("randomfamid"), 
               model = "within")

model20<- plm(negfeelings4 ~ risk1*male, 
               data = data,
               index = c("randomfamid"), 
               model = "within")

model21<- plm(negfeelings7 ~ risk1* male, 
               data = data,
               index = c("randomfamid"), 
               model = "within")

# DISCIPLINE BEHAVIOURS #

model22<- plm(discipline3 ~ risk1 * male, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

model23<- plm(discipline4 ~ risk1 *male, 
               data = data,
               index = c("randomfamid"), 
               model = "within")

model24<- plm(discipline7 ~ risk1 * male, 
               data = data,
               index = c("randomfamid"), 
               model = "within")

# Export table
stargazer(model19, model20, model21, model22, model23, model24,
          type = "html",
          out="tableA6.doc",
          star.cutoffs = c(0.05, 0.01, 0.001))



############################################################################
##### Table A7: Parental Responses and Parental Gender Control  ################
############################################################################


# Prepare variable parent responding

table(data_complete$aad1rel)
data_complete$fatheransw<-0
data_complete$fatheransw[data_complete$aad1rel==2]<-1 # 1 if father answering
table(data_complete$fatheransw) # explore

# Re-extract sample with variables of interest

myvars <- c("randomfamid", "random", "twin",  # IDS
            "risk1",  # indep var
            "negfeelings3", "negfeelings4", "negfeelings7", # dep vars
            "discipline3", "discipline4", "discipline7", # dep vars
            "male", "fatheransw") # controls

data<-data_complete[myvars]

# NEGATIVE EMOTIONAL RESPONSES # 

model25<- plm(negfeelings3 ~ risk1*fatheransw+ male, 
               data = data,
               index = c("randomfamid"), 
               model = "within")
summary(model25)

model26<- plm(discipline3 ~ risk1 * fatheransw+ male, 
               data = data,
               index = c("randomfamid"), 
               model = "within")
summary(model26)

model27<- plm(negfeelings4 ~ risk1 *fatheransw+ male, 
               data = data,
               index = c("randomfamid"), 
               model = "within")

# DISCIPLINE BEHAVIOURS #


model28<- plm(discipline4 ~ risk1 *fatheransw+ male, 
               data = data,
               index = c("randomfamid"), 
               model = "within")
model29<- plm(negfeelings7 ~ risk1 * fatheransw+ male, 
               data = data,
               index = c("randomfamid"), 
               model = "within")
model30<- plm(discipline7 ~ risk1 * fatheransw+ male, 
               data = data,
               index = c("randomfamid"), 
               model = "within")

# Table to export 

stargazer(model25, model26, model27, model28, model29, model30,
          type = "html",
          out="tableA7.doc",
          star.cutoffs = c(0.05, 0.01, 0.001))


############################################################################
##### Table A8: Parental Responses and Zygosity Interaction  ################
############################################################################

# Prepare zygosity variable

data_complete$zygos[data_complete$zygos==2]<-0 # 0 dyzygotic, 1 monozygotic
table(data_complete$zygos)

# Re-extract sample with variables of interest

myvars <- c("randomfamid", "random", "twin",  # IDS
            "risk1",  # indep var
            "negfeelings3", "negfeelings4", "negfeelings7", # dep vars
            "discipline3", "discipline4", "discipline7", # dep vars
            "male", "zygos") # controls

data<-data_complete[myvars]

# NEGATIVE EMOTIONAL RESPONSES # 

model31<- plm(negfeelings3 ~ risk1 *  zygos, 
               data = data,
               index = c("randomfamid"), 
               model = "within")

model32<- plm(negfeelings4 ~ risk1*zygos, 
               data = data,
               index = c("randomfamid"), 
               model = "within")

model33<- plm(negfeelings7 ~ risk1* zygos, 
               data = data,
               index = c("randomfamid"), 
               model = "within")

# DISCIPLINE BEHAVIOURS #

model34<- plm(discipline3 ~ risk1 * zygos, 
               data = data,
               index = c("randomfamid"), 
               model = "within")

model35<- plm(discipline4 ~ risk1*zygos, 
               data = data,
               index = c("randomfamid"), 
               model = "within")

model36<- plm(discipline7 ~ risk1* zygos, 
               data = data,
               index = c("randomfamid"), 
               model = "within")

# Export table
stargazer( model31, model32, model33, model34, model35, model36,
          type = "html",
          out="tableA8.doc",
          star.cutoffs = c(0.05, 0.01, 0.001))

############################################################################
##### Table A9: Parental Responses by Extended Parental Education ################
############################################################################

# Re-extract sample with variables of interest

myvars <- c("randomfamid", "random", "twin",  # IDS
            "risk1",  # indep var
            "negfeelings3", "negfeelings4", "negfeelings7", # dep vars
            "discipline3", "discipline4", "discipline7", # dep vars
            "male", "paredu") # controls

data<-data_complete[myvars]


# NEGATIVE EMOTIONAL RESPONSES # 

model37<- plm(negfeelings3 ~ risk1 + paredu + risk1:paredu  + male, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

model38<- plm(negfeelings4 ~ risk1 + paredu  + risk1: paredu  + male, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

model39<- plm(negfeelings7 ~ risk1 + paredu  + risk1: paredu  + male, 
              data = data,
              index = c("randomfamid"), 
              model = "within")


# DISCIPLINE BEHAVIOURS #

model40<- plm(discipline3 ~ risk1 + paredu + risk1:paredu  + male, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

model41<- plm(discipline4 ~ risk1 + paredu  + risk1: paredu + male, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

model42<- plm(discipline7 ~ risk1 + paredu  + risk1: paredu  + male, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

# Export table
stargazer(model37,model38, model39, model40, model41, model42,
          type = "html",
          out="tableA9.doc",
          star.cutoffs = c(0.05, 0.01, 0.001))


############################################################################
##### Table A10: Parental Responses by Maternal Occupation ################
############################################################################

# Create maternal occupation dummy

# Categorical
data_complete <- data_complete %>%
  mutate(
    moccup = case_when(
      ppmoemplc == 1 | ppmoemplc == 2 ~ 0,  # unemployed
      ppmoemplc >= 3 & ppmoemplc <= 11 ~ 1,  # service class
      ppmoemplc >= 12 & ppmoemplc <= 14 ~ 2,  # routine non-manual
      ppmoemplc >= 15 & ppmoemplc <= 22 ~ 3,  # skilled manual
      ppmoemplc >= 23 & ppmoemplc <= 28 ~ 4,  # unskilled manual
      ppmoemplc == 29 | ppmoemplc == 30 ~ NA_real_  # non-answers
    )
  )

# Convert to factor
data_complete$moccup <- as.factor(data_complete$moccup)

# Create binary
data_complete<-data_complete %>%
  mutate(
    moccupd = case_when(
      moccup == 0 ~ NA_real_,  # unemployed (set to NA)
      moccup == 1 ~ 1,  # non-manual
      moccup == 2 | moccup == 3 | moccup == 4 ~ 0  # manual
    )
  )


# Re-extract sample with variables of interest

myvars <- c("randomfamid", "random", "twin",  # IDS
            "risk1",  # indep var
            "negfeelings3", "negfeelings4", "negfeelings7", # dep vars
            "discipline3", "discipline4", "discipline7", # dep vars
            "male", "moccupd") # controls

data<-data_complete[myvars]


# NEGATIVE EMOTIONAL RESPONSES # 

model43<- plm(negfeelings3 ~ risk1 + moccupd + risk1: moccupd + male, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

model44<- plm(negfeelings4 ~ risk1 + moccupd + risk1: moccupd + male, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

model45<- plm(negfeelings7 ~ risk1 + moccupd + risk1: moccupd + male, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

# DISCIPLINE BEHAVIOUR # 

model46<- plm(discipline3 ~ risk1 + moccupd + risk1: moccupd + male, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

model47<- plm(discipline4 ~ risk1 + moccupd + risk1: moccupd + male, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

model48<- plm(discipline7 ~ risk1 + moccupd + risk1: moccupd + male, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

# Export table

stargazer(model43,model44, model45, model46, model47, model48,
          type = "html",
          out="tableA10.doc",
          star.cutoffs =c(0.05, 0.01, 0.001))

############################################################################
##### Table A11: Parental Responses by Household Income ################
############################################################################

# Re-extract sample with variables of interest

myvars <- c("randomfamid", "random", "twin",  # IDS
            "risk1",  # indep var
            "negfeelings3", "negfeelings4", "negfeelings7", # dep vars
            "discipline3", "discipline4", "discipline7", # dep vars
            "male", "pphhincc") # controls

data<-data_complete[myvars]


# NEGATIVE EMOTIONAL RESPONSES # 

model49<- plm(negfeelings3 ~ risk1: pphhincc + male, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

model50<- plm(negfeelings4 ~ risk1: pphhincc + male, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

model51<- plm(negfeelings7 ~ risk1: pphhincc + male, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

# DISCIPLINE BEHAVIOUR #

model52<- plm(discipline4 ~ risk1 : pphhincc  + male, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

model53<- plm(discipline3 ~  risk1: pphhincc + male, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

model54<- plm(discipline7 ~  risk1: pphhincc+ male, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

# Export table
stargazer(model49,model50, model51, model52, model53, model54,
          type = "html",
          out="tableA11.doc",
          star.cutoffs =c(0.05, 0.01, 0.001))

############################################################################
##### Table A12: Parental Responses. OLS Models with interaction ################
############################################################################

# Re-extract sample with variables of interest

myvars <- c("randomfamid", "random", "twin",  # IDS
            "risk1",  # indep var
            "negfeelings3", "negfeelings4", "negfeelings7", # dep vars
            "discipline3", "discipline4", "discipline7", # dep vars
            "male", "mumage", "siblings", "paredud") # controls

data<-data_complete[myvars]

# NEGATIVE EMOTIONAL RESPONSES # 

model55<- lm(negfeelings3 ~ risk1*paredud+ male + mumage+siblings, 
            data = data)

model56<- lm(negfeelings4 ~ risk1*paredud+ male+ mumage+siblings, 
            data = data)

model57<- lm(negfeelings7 ~ risk1*paredud+ male+ mumage+siblings, 
            data = data)

# DISCIPLINE BEHAVIOURS #


model58<- lm(discipline3 ~ risk1*paredud+ male+mumage+siblings, 
            data = data)

model59<- lm(discipline4 ~ risk1*paredud+ male+ mumage+siblings, 
            data = data)

model60<- lm(discipline7 ~ risk1*paredud+ male+ mumage+siblings, 
            data = data)

# Export table
stargazer(model55, model56, model57, model58, model59, model60,
          type = "html",
          out="tableA12.doc",
          star.cutoffs = c(0.05, 0.01, 0.001))


############################################################################
##### Table A13: Subscales of the parental discipline index by SES ################
############################################################################

# Create subscales (note prefix c means age 3, d age 4 and g age 7)

# 1st scale: smack and shout (average)

means<-data.frame(data_complete$cdismak1, data_complete$cdishou1)
data_complete$smackshout<-rowMeans(means, na.rm = F)

# 2nd scale: explain and firm.

means<-data.frame(data_complete$cdiexpl1, data_complete$cdifirm1)
data_complete$explainfirm<-rowMeans(means)

# 3rd scale: ask and joke.
means<-data.frame(data_complete$cdijoke1, data_complete$cdiasko1)
data_complete$askjoke<-rowMeans(means)


# Re-extract sample with variables of interest

myvars <- c("randomfamid", "random", "twin",  # IDS
            "risk1",  # indep var
            "negfeelings3", "negfeelings4", "negfeelings7", # dep vars
            "discipline3", "discipline4", "discipline7", # dep vars
            "male", "paredud",
            "smackshout", "explainfirm", "askjoke") # controls

data<-data_complete[myvars]


# Split sample by SES
split<-split(data, f=data$paredud)
tert<-split$Tertiary
nontert<-split$`Non-tertiary`

# TERTIARY EDUCATED #

model61<- plm(smackshout ~ risk1+ male, 
              data = tert,
              index = c("randomfamid"), 
              model = "within")
model62<- plm(explainfirm ~ risk1+ male, 
              data = tert,
              index = c("randomfamid"), 
              model = "within")
model63<- plm(askjoke ~ risk1+ male, 
              data = tert,
              index = c("randomfamid"), 
              model = "within")

# NON-TERTIARY EDUCATED #

model64<- plm(smackshout ~ risk1+ male, 
              data = nontert,
              index = c("randomfamid"), 
              model = "within")
model65<- plm(explainfirm ~ risk1+ male, 
              data = nontert,
              index = c("randomfamid"), 
              model = "within")
model66<- plm(askjoke ~ risk1+ male, 
              data = nontert,
              index = c("randomfamid"), 
              model = "within")

# Export table
stargazer(model61, model62, model63, model64, model65, model66,
          type = "html",
          out="tableA13.doc",
          star.cutoffs = c(0.05, 0.01, 0.001))

