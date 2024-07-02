
####################################################################
################## CLEANING VARIABLES ##############################
####################################################################


################## INDEPENDENT VARIABLE ##############################

# Health risk measured at age 1: Twin Medical Risk Factor scale.

# From the TEDS dictionary: 
# Mean of standardised versions of days in special care,
# days in hospital, reversed twin birth weight and medical problems. 
# Require at least two of these to be non-missing.

summary(data$atwmed1) # explore the variable
hist(data$atwmed1) 

data$risk1<-data$atwmed1 # #rename

# For benchmarking each component:
summary(data$arkidgr1) # birth weight
summary(data$arspcad1) # days in special care
summary(data$arhosp1) # days in hospital after birth
summary(data$amprob1) # medical problems at birth



################## OUTCOME VARIABLES ##############################

#########  NEGATIVE EMOTIONAL RESPONSES ########

### AGE 3 #####

summary(data$cbfac11) #explore
data$negfeelings3<-data$cbfac11 #relabel

### AGE 4 #####

summary(data$dbfac11) #explore 
data$negfeelings4<-data$dbfac11 #relabel

### AGE 7 #####

summary(data$gbfac11) #explore
data$negfeelings7<-data$gbfac11 #relabel

#########  DISCIPLINE BEHAVIOURS ########

### AGE 3 #####

summary(data$cdisto1) #explore
data$discipline3<-data$cdisto1 #relabel

### AGE 4 #####

summary(data$ddisto1) #explore
data$discipline4<-data$ddisto1 #relabel

### AGE 7 #####

summary(data$gdisp1) #explore
data$discipline7<- data$gdisp1 #relabel


################## CONTROL VARIABLES ##############################

#########  GENDER ########

table(data$sex1)
data$male<-data$sex1 # 0 female, 1 male

#########  MOTHERS' EDUCATION ########

# Categorical version: 	
# Original variable: 1=none, 2=O/GCSE grade D-G or CSE grade 2-5, 3=O/GCSE grade A-C or CSE grade 1, 4=A/S level, 5=HNC, 6=HND, 7=undergraduate degree, 8=postgrad
# Transformed variable: 0=none, 1= GCSE, 2=A levels, 3= Vocational, 4=Undergraduate, 5=Postgraduate.

table(data$amohqual)  # original variable

data <- data %>%  # transformed variable
  mutate(motheredu = case_when(
    amohqual == 1 ~ 0,
    amohqual == 2 | amohqual == 3 ~ 1,
    amohqual == 4 ~ 2,
    amohqual == 5 | amohqual == 6 ~ 3,
    amohqual == 7 ~ 4,
    amohqual == 8 ~ 5,
    TRUE ~ as.numeric(NA) 
  ))

# Binary version
data <- data %>% 
  mutate(motheredud = case_when(
    motheredu <= 3 ~ 0,  # Non-tertiary education
    motheredu == 4 | motheredu == 5 ~ 1,  # Tertiary education
    TRUE ~ as.numeric(NA) 
  ))

# Relabel the variables
data$motheredu <-factor (data$motheredu,
                       levels=c(0, 1, 2, 3, 4, 5), # defines the values of the variable 
                       labels=c("No education", "Secondary education", "A-levels", "Vocational", "Undergrad", "Postgrad"))
table(data$motheredu) #check 


#########  FATHERS' EDUCATION ########

# Categorical version
# Original variable: 1=none, 2=O/GCSE grade D-G or CSE grade 2-5, 3=O/GCSE grade A-C or CSE grade 1, 4=A/S level, 5=HNC, 6=HND, 7=undergraduate degree, 8=postgrad
# Transformed variable: 0=none, 1= GCSE, 2=A levels, 3= Vocational, 4=Undergraduate, 5=Postgraduate.

table(data$afahqual) # original variable 

data <- data %>%  # transformed variable
  mutate(fatheredu = case_when(
    afahqual == 1 ~ 0,
    afahqual == 2 | afahqual == 3 ~ 1,
    afahqual == 4 ~ 2,
    afahqual == 5 | afahqual == 6 ~ 3,
    afahqual == 7 ~ 4,
    afahqual == 8 ~ 5,
    TRUE ~ as.numeric(NA) 
  ))

# Binary
data <- data %>%
  mutate(fatheredud = case_when(
    fatheredu <= 3 ~ 0,  # Non-tertiary education
    fatheredu == 4 | fatheredu == 5 ~ 1,  # Tertiary education
    TRUE ~ as.numeric(NA)  
  ))

# Relabel the variables
data$fatheredu<-factor(data$fatheredu,
                       levels=c(0, 1, 2, 3, 4, 5), # defines the values of the variable 
                       labels=c("No education", "Secondary education", "A-levels", "Vocational", "Undergrad", "Postgrad"))
table(data$fatheredu)
data$fatheredud<-factor(data$fatheredud,
                        levels=c(0,1),
                        labels=c("Non-tertiary", "Tertiary"))


######## PARENTS' EDUCATION - DOMINANCE MODEL ########

# Pick highest education mother or father

data$dominance<-
  ifelse(data$afahqual>=data$amohqual, data$afahqual, data$amohqual)

#Categorical
data <- data %>%
  mutate(paredu = case_when(
    dominance == 1 ~ 0,
    dominance == 2 | dominance == 3 ~ 1,
    dominance == 4 ~ 2,
    dominance == 5 | dominance == 6 ~ 3,
    dominance == 7 ~ 4,
    dominance == 8 ~ 5,
    TRUE ~ as.numeric(NA)  
  ))

# Binary
data <- data %>%
  mutate(paredud = case_when(
    paredu <= 3 ~ 0,  # Non-tertiary education
    paredu == 4 | paredu == 5 ~ 1,  # Tertiary education
    TRUE ~ as.numeric(NA) 
  ))

# Relabel  the variables

data$paredu<-factor(data$paredu,
                    levels=c(0, 1, 2, 3, 4, 5), # defines the values of the variable 
                    labels=c("No education", "Secondary education", "A-levels", "Vocational", "Undergrad", "Postgrad"))

data$paredud<-factor(data$paredud,
                     levels=c(0,1),
                     labels=c("Non-tertiary", "Tertiary"))



#########  AGE OF THE MOTHER AT BIRTH ########

table(data$amumagetw)
data$mumage<-data$amumagetw # rename

######### HAVING OTHER SIBLINGS ########

table(data$anyngsib)
data$siblings<-data$anyngsib # rename


####################################################################
################## EXTRACT FINAL SAMPLE ##############################
####################################################################

data_complete <- data # to preserve the original data 

myvars <- c("randomfamid", "random", "twin",  # IDS
            "risk1",  # indep var
            "negfeelings3", "negfeelings4", "negfeelings7", # dep vars
            "discipline3", "discipline4", "discipline7", # dep vars
            "male", "motheredu", "motheredud", # controls
            "fatheredu", "fatheredud", # controls
            "paredu", "paredud", # controls
            "mumage", "siblings") # controls

data<-data_complete[myvars] # extract sample

df <-  as.data.frame(do.call(cbind, data)) # important for the reshape to label the datset as a data frame
