
## Project:  SOC 302 Final Multivariate 
# Located:   FILL THIS OUT
# File Name: FILL THIS OUT
# Date:      FILL THIS OUT
# Who:       FILL THIS OUT


####################################################################################
############              Pre-Analysis: settings, packages, and data    ############
####################################################################################


### Settings + Packages
setwd("/courses/SOC302/schubek1") 

library("dplyr")

library("psych")
options(max.print = 1000000)


### Load data 
GSS <- read.csv("GSS2022.csv")



####################################################################################
############              PHASE 1: CLEAN DATA FOR ANALYSIS              ############
####################################################################################


## Steps of cleaning variables Clear vars
# Step 1: Examine variable and coding schema: Table() / summary() 
table(GSS$satjob)
# Step 2: Recode (if necessary/warrented): mutate(), ifelse(), etc
GSS <- mutate(GSS, high_satjob = ifelse(satjob==1, 1, 0))
# Step 3: Confirm: table() / summary()



############                     DEPENDENT VARIABLE                     ############
############                     JOB SATISFACTION                      ############

# STEP 1: Examine variable and coding schema 
table(GSS$satjob)

# STEP 2: Recode if necessary or justify if not neccesary
GSS <- mutate(GSS, verysat = ifelse(satjob==1, 1, 0))
GSS <- mutate(GSS, modsat = ifelse(satjob==2, 1, 0))
GSS <- mutate(GSS, littledissat = ifelse(satjob==3, 1, 0))
GSS <- mutate(GSS, verydissat = ifelse(satjob==4, 1, 0))

# variable for overall satisfaction with job for satjob 
GSS <- mutate(GSS, satisfied = ifelse(satjob<=2, 1, 0))
GSS <- mutate(GSS, dissatisfied = ifelse(satjob>=3, 1, 0))

# variable for satisfied with job 
GSS <- mutate(GSS, satisfied = ifelse(satjob == 2 |satjob == 1, 1, 0))

# STEP 3: Confirm creation (if necessary)
table(GSS$satjob, GSS$satisfied)



############                  INDEPENDENT VARIABLE                    ############
############                         degree                           ############

# STEP 1: Examine variable and coding schema 
table(GSS$degree)
# STEP 2: Recode if necessary or justify if not neccesary
GSS <-mutate(GSS, hs_or_less = ifelse(degree<=1,1,0))
GSS <-mutate(GSS, assoc_or_more = ifelse(degree>=2,1,0))


#STEP 2: Make into binary for any 4-year college degree

# STEP 3: Confirm creation (if necessary)
table(GSS$degree, GSS$hs_or_less)
table(GSS$degree, GSS$assoc_or_more)




############                  INDEPENDENT VARIABLE                    ############
############                        Gender                            ############

# STEP 1: Examine variable and coding schema 
table(GSS$sexnow1)
# STEP 2: Create dummy variables for men and women
GSS <- mutate(GSS, male = ifelse(sexnow1==1, 1, 0))
GSS <- mutate(GSS, non_male = ifelse(sexnow1>=2, 1, 0))



# STEP 3: Confirm creation (if necessary)
table (GSS$sexnow1, GSS$male)
table (GSS$sexnow1, GSS$non_male)


############                  INDEPENDENT VARIABLE                    ############
############                          race                            ############

# STEP 1: Examine variable and coding schema 
table(GSS$race)
# STEP 2: Create dummy variables for men and women
GSS <- mutate(GSS, white = ifelse(race== 1,1,0))
GSS <- mutate(GSS, black = ifelse(race== 2,1,0))
GSS <- mutate(GSS, other_race = ifelse(race== 3,1,0))

# STEP 3: Confirm creation (if necessary)
table(GSS$race, GSS$white)
table(GSS$race, GSS$black)
table(GSS$race, GSS$other_race)

############                  INDEPENDENT VARIABLE                    ############
############                        children                          ############

# STEP 1: Examine variable and coding schema 
table(GSS$childs)
# STEP 2: Create dummy variables for men and women
GSS <- mutate(GSS, no_kids = ifelse(childs==0, 1, 0))
GSS <- mutate(GSS, yes_kids = ifelse(childs>=1, 1, 0))



# STEP 2: Make into binary for any children

# STEP 3: Confirm creation (if necessary)
table(GSS$childs, GSS$no_kids)
table(GSS$childs, GSS$yes_kids)





############                  INDEPENDENT VARIABLE                    ############
############                     time worked                          ############

# STEP 1: Examine variable and coding schema 
table(GSS$wrksched)
# STEP 2: Create dummy variables for men and women
GSS <- mutate(GSS, day_shift = ifelse(wrksched==1, 1, 0))
GSS <- mutate(GSS, other_shift = ifelse(wrksched>=2, 1, 0))




# STEP 3: Confirm creation (if necessary)
table(GSS$wrksched, GSS$day_shift)
table(GSS$wrksched, GSS$other_shift)



############                  INDEPENDENT VARIABLE                    ############
############                     job stability                       ############

# STEP 1: Examine variable and coding schema 
table(GSS$joblose)
# STEP 2: Create dummy variables for men and women
GSS <- mutate(GSS, likely = ifelse(wrksched<=2, 1, 0))
GSS <- mutate(GSS, not_likely = ifelse(wrksched>=3, 1, 0))
GSS <- mutate(GSS, leaving_labor_force = ifelse(wrksched==5, 1, 0))


# STEP 3: Confirm creation (if necessary)
table(GSS$joblose, GSS$likely)
table(GSS$joblose, GSS$not_likely)
table(GSS$joblose, GSS$leaving_labor_force)

############                  INDEPENDENT VARIABLE                    ############
############                     job safety                       ############

# STEP 1: Examine variable and coding schema 
table(GSS$safetywk)
# STEP 2: Create dummy variables for men and women
GSS <- mutate(GSS, only_agree = ifelse(safetywk<=2, 1, 0))
GSS <- mutate(GSS, only_disagree = ifelse(safetywk>=3, 1, 0))


# STEP 3: Confirm creation (if necessary)
table(GSS$safetywk, GSS$only_agree)
table(GSS$safetywk, GSS$only_disagree)


############              INDEPENDENT VARIABLE                ############
############                     income                       ############

# STEP 1: Examine variable and coding schema 
hist(GSS$realinc)

# STEP 2: Create dummy variables for men and women
#natural log to interpret percent change
GSS$lnincome <- log(GSS$realinc)

# STEP 3: Confirm creation (if necessary)
GSS$test_income <- GSS$lnincome - log(GSS$realinc)
summary(GSS$test_income)


####################################################################################
############              PHASE 2: CREATE MY DATASET                    ############
####################################################################################

### STEP 1: Create a list of variables to keep
my_varlist <- c("satjob", "degree", "satisfied", "hs_or_less", "assoc_or_more", "male", 
                "non_male", "white", "black", "other_race", "no_kids", "yes_kids", 
                "day_shift", "other_shift", "likely", "not_likely", "leaving_labor_force",
                "only_agree", "only_disagree", "realinc")

### STEP 2: create a new dataset with only your variables and complete case
my_dataset <- GSS %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))


### STEP 3: Gather Summary Statistics and confirm valid dataset construction
describe(my_dataset)


####################################################################################
############              PHASE 3: Descriptive Statistics     ############
####################################################################################
# TABLE 1: DESCRIPTIVE STATISTICS HERE
describe(my_dataset)

####################################################################################
############              PHASE 4: correlation matrix                  ############
####################################################################################
#correlation between key IV and DV#
cor(my_dataset$hs, my_dataset$satjob)
cor(my_dataset)

# TABLE 2: CONTINGENCY TABLE HERE
table(my_dataset$satjob)
table(my_dataset$degree)
table(my_dataset$realinc)
table(my_dataset$childs)
table(my_dataset$joblose)
table(my_dataset$wksched)
table(my_dataset$sexnow1)
table(my_dataset$racerank1)
table(my_dataset$safetywk)




chisq.test(table(my_dataset$satjob, my_dataset$degree)
)

###############################################################################################
#########################           Phase 5: Regression                 #######################
###############################################################################################

##Model 1a
model1 <- glm(satisfied ~ hs_or_less + assoc_or_more, data = my_dataset, family = binomial)
summary(model1)


##Model 2a
model2 <- glm(satisfied ~ hs_or_less + assoc_or_more + male + non_male, data = my_dataset, family = binomial)
summary(model2)

##Model 3a
model3 <- glm(satisfied ~ hs_or_less + assoc_or_more + male + non_male + white + black + other_race, data = my_dataset, family = binomial)
summary(model3)

##Model 4a
model4 <- glm(satisfied ~ hs_or_less + assoc_or_more + male + non_male + white + black + other_race + no_kids + yes_kids, data = my_dataset, family = binomial)
summary(model4)

##Model 5a
model5 <- glm(satisfied ~ hs_or_less + assoc_or_more + male + non_male + white + black + other_race + no_kids + yes_kids + day_shift + other_shift, data = my_dataset, family = binomial)
summary(model5)

##Model 6a
model6 <- glm(satisfied ~ hs_or_less + assoc_or_more + male + non_male + white + black + other_race + no_kids + yes_kids + day_shift + other_shift + likely + not_likely, data = my_dataset, family = binomial)
summary(model6)

##Model 7a 
model7 <- glm(satisfied ~ hs_or_less + assoc_or_more + male + non_male + white + black + other_race + no_kids + yes_kids + day_shift + other_shift + likely + not_likely + only_agree + only_disagree, data = my_dataset, family = binomial)
summary(model7)

##Model 8a 
model8 <- glm(satisfied ~ hs_or_less + assoc_or_more + male + non_male + white + black + other_race + no_kids + yes_kids + day_shift + other_shift + likely + not_likely + only_agree + only_disagree + realinc, data = my_dataset, family = binomial)
summary(model8)