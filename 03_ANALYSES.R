
####################################################################
################## MAIN ANALYSES ##############################
####################################################################


####################################################################
#### Table A2. Parental Responses. OLS Models ###############
####################################################################

# NEGATIVE EMOTIONAL RESPONSES # 

model1<- lm(negfeelings3 ~ risk1+paredud+ male + mumage+siblings, 
            data = data)

model2<- lm(negfeelings4 ~ risk1+paredud+ male+ mumage+siblings, 
            data = data)

model3<- lm(negfeelings7 ~ risk1+paredud+ male+ mumage+siblings, 
            data = data)

# DISCIPLINE BEHAVIOURS #

model4<- lm(discipline3 ~ risk1+paredud+ male+mumage+siblings, 
            data = data)

model5<- lm(discipline4 ~ risk1+paredud+ male+ mumage+siblings, 
            data = data)

model6<- lm(discipline7 ~ risk1+paredud+ male+ mumage+siblings, 
            data = data)

# Table to export

stargazer(model1, model2, model3, model4, model5, model6,
          type = "html",
          out="tableA2.doc",
          star.cutoffs = c(0.05, 0.01, 0.001))

####################################################################
################### Table 1: Twins fixed effects models  ###########
####################################################################

# NEGATIVE EMOTIONAL RESPONSES # 

model7<- plm(negfeelings3 ~ risk1 +  male + twin,  # note that the variable "twin" identifies the twin birth order
             data = data,
             index = c("randomfamid"), 
             model = "within")

model8<- plm(negfeelings4 ~ risk1+ male + twin, 
             data = data,
             index = c("randomfamid"), 
             model = "within")

model9<- plm(negfeelings7 ~ risk1+ male + twin, 
             data = data,
             index = c("randomfamid"), 
             model = "within")

# DISCIPLINE BEHAVIOURS #

model10<- plm(discipline3 ~ risk1 + male+ twin, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

model11<- plm(discipline4 ~ risk1+ male+ twin, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

model12<- plm(discipline7 ~ risk1+ male+ twin, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

# Table to export

stargazer(model7, model8, model9, model10, model11, model12,
          type = "html",
          out="table1.doc",
          star.cutoffs = c(0.05, 0.01, 0.001))



##############################################################################
################### Table 2: Parental responses by family SES  ##############
##############################################################################

# NEGATIVE EMOTIONAL RESPONSES # 

model13<- plm(negfeelings3 ~ risk1 + paredud + risk1:paredud  + male, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

model14<- plm(negfeelings4 ~ risk1 + paredud  + risk1: paredud  + male, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

model15<- plm(negfeelings7 ~ risk1 + paredud  + risk1: paredud  + male, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

# DISCIPLINE BEHAVIOURS #

model16<- plm(discipline3 ~ risk1 + paredud + risk1:paredud  + male, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

model17<- plm(discipline4 ~ risk1 + paredud  + risk1: paredud + male, 
              data = data,
              index = c("randomfamid"), 
              model = "within")

model18<- plm(discipline7 ~ risk1 + paredud  + risk1: paredud  + male, 
              data = data,
              index = c("randomfamid"), 
              model = "within")


# Table to export

stargazer(model13,model14, model15, model16, model17, model18,
          type = "html",
          out="table2.doc",
          star.cutoffs = c(0.05, 0.01, 0.001))

