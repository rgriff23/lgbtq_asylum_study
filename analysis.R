################
# PREPARATIONS #
################

# Load packages
library('tidyverse')
library('psycho')
library('MuMIn') 

# The data file imported here has been manually modified from the data file produced by
# ****-**-**-create-clean-data.csv
data <- read.csv('~/Dropbox/Asylum Data/Clean data and code/2018-12-24-clean-data.csv')

# Compute revised outness scale & overwrite old one
# Create 'unacceptability' scale for supplementary model
out_cols <- c('Outness_1','Outness_2','Outness_4','Outness_5')
data_out <- data # data_out gets overwritten several times in this script
for (i in out_cols) {
  data_out[,i] <- ifelse(data[,i]==2, 1, 0) # 1 = out, unaccepted
  data[,i] <- ifelse(data[,i]==1, 0, 1) # 1 = out
  }
data$out <- data[,out_cols] %>% rowMeans(na.rm=TRUE)
data$unaccepted <- data_out[,out_cols] %>% rowMeans(na.rm=TRUE)

#####################
# DESCRIPTIVE STATS #
#####################

# Number of rows (post-cleaning)
nrow(data) # 308

# Number of unique countries (excluding invalid countries)
data$origin_country %>% unique %>% length - 5 # 48

# Number of unique states and provinces
data$state_province %>% unique %>% length # 29

# Percent answering 'yes' to mental_health question
sum(data$Mental_health==1, na.rm=TRUE)/nrow(data) # 38.6 %

# Percent answering 'yes' to mental health question based on immigration status
nrow(data[data$Mental_health==1 & data$Immigration_status=='Granted',])/
  sum(data$Immigration_status=='Granted') # 39.25 %
nrow(data[data$Mental_health==1 & data$Immigration_status!='Granted',])/
  sum(data$Immigration_status!='Granted') # 39.8 %

# Summary stats for RHS
rhs_columns <- c('RHS_1','RHS_2','RHS_3','RHS_4','RHS_5','RHS_6','RHS_7','RHS_8','RHS_9','RHS_10','RHS_11','RHS_12','RHS_13','RHS_14')
(rowSums(data[,rhs_columns])-14) %>% mean # 21.44
rowSums(data[,rhs_columns]) %>% sd # 12.11
rowSums(data[,rhs_columns]) %>% range # 14 - 70
# percent positive RHS
data$rhs %>% sum / nrow(data) # 80.19 %
# want therapist / positive RHS
sum(data[data$rhs,'Therapist'] == 1, na.rm=TRUE) / sum(data$rhs) # 70.45 %
sum(data[!data$rhs,'Therapist'] == 1, na.rm=TRUE) / sum(!data$rhs) # 44.26 %

# Summary stats for Loneliness
data$lonely %>% mean # 63.19
data$lonely %>% sd # 10.86
data$lonely %>% range # 38.18 - 84.61
# facebook
sum(data[data$lonely > mean(data$lonely),'Facebook'] == 1, na.rm=TRUE) / 
  sum(data$lonely > mean(data$lonely)) # 59.33 %
# website
sum(data[data$lonely > mean(data$lonely),'Website'] == 1, na.rm=TRUE) / 
  sum(data$lonely > mean(data$lonely)) # 52.0 %
# community center
sum(data[data$lonely > mean(data$lonely),'Community_Center'] == 1, na.rm=TRUE) / 
  sum(data$lonely > mean(data$lonely)) # 70.0 %
# mentor
sum(data[data$lonely > mean(data$lonely),'Buddy'] == 1, na.rm=TRUE) / 
  sum(data$lonely > mean(data$lonely)) # 82.0 %

# Summary stats for emotional support
data$support %>% mean(na.rm=TRUE) # 46.71 
data$support %>% sd(na.rm=TRUE) # 9.42
data$support %>% range(na.rm=TRUE) # 25.7 - 62.0
# facebook
sum(data[data$support < mean(data$support, na.rm=TRUE),'Facebook'] == 1, na.rm=TRUE) / 
  sum(data$support < mean(data$support, na.rm=TRUE), na.rm=TRUE) # 60.12 %
# website
sum(data[data$support < mean(data$support, na.rm=TRUE),'Website'] == 1, na.rm=TRUE) / 
  sum(data$support < mean(data$support, na.rm=TRUE), na.rm=TRUE) # 52.76 %
# community center
sum(data[data$support < mean(data$support, na.rm=TRUE),'Community_Center'] == 1, na.rm=TRUE) / 
  sum(data$support < mean(data$support, na.rm=TRUE), na.rm=TRUE) # 68.71 %
# mentor
sum(data[data$support < mean(data$support, na.rm=TRUE),'Buddy'] == 1, na.rm=TRUE) / 
  sum(data$support < mean(data$support, na.rm=TRUE), na.rm=TRUE) # 82.82 %

# Summary stats for outness
data$out %>% mean(na.rm=TRUE) # 0.73
data$out %>% sd(na.rm=TRUE) # 0.3
data$out %>% range(na.rm=TRUE) # 0 - 1
# no lgbtq friends
sum(data[(data$out < mean(data$out, na.rm=TRUE)) %in% TRUE,'Network_1'] == 2, na.rm=TRUE) / 
  sum((data$out < mean(data$out, na.rm=TRUE)) %in% TRUE) # 14.29 %
# friend number = 0
sum(data[(data$out < mean(data$out, na.rm=TRUE)) %in% TRUE,'LGBTQFriends_number'] == '0', na.rm=TRUE) / 
  sum((data$out < mean(data$out, na.rm=TRUE)) %in% TRUE) # 8.03 %

# mean outness score for those who answered all 4 (n=237)
out_data <- data[,out_columns] 
out_data <- out_data[complete.cases(out_data),]
out_sums <- rowSums(out_data)
out_sums %>% mean(na.rm=TRUE) # 2.86
out_sums %>% sd(na.rm=TRUE) # 1.24
out_sums %>% range(na.rm=TRUE) # 0 - 4

##########
# TABLES #
##########

# Table 1. Participant characteristics
# counts
table(data$language)
table(data$region)
table(data$origin_country)
table(data$residence_country)
table(data$state_province)
table(data$Gender)
table(data$Sexualorientation)
table(data$age_group)
table(data$years_lived_grp)
table(data$Immigration_status)
table(data$English_proficiency)
table(data$Education)
table(data$Employment)
table(data$School)
# percents
table(data$language)/sum(table(data$language))*100
table(data$region)/sum(table(data$region))*100
table(data$origin_country)/sum(table(data$origin_country))*100
table(data$residence_country)/sum(table(data$residence_country))*100
table(data$state_province)/sum(table(data$state_province))*100
table(data$Gender)/sum(table(data$Gender))*100
table(data$Sexualorientation)/sum(table(data$Sexualorientation))*100
table(data$age_group)/sum(table(data$age_group))*100
table(data$years_lived_grp)/sum(table(data$years_lived_grp))*100
table(data$Immigration_status)/sum(table(data$Immigration_status))*100
table(data$English_proficiency)/sum(table(data$English_proficiency))*100
table(data$Education)/sum(table(data$Education))*100
table(data$Employment)/sum(table(data$Employment))*100
table(data$School)/sum(table(data$School))*100

# Table 2. Social support
# Network questions
table(data$Network_1) 
table(data$Network_2)
table(data$Network_4) 
table(data$Network_5)
# Number of LGBT friends
table(data$LGBTQFriends_number)
# Who gives you the most support
data$Top3_support <- as.character(data$Top3_support)
sum(grepl('1', data$Top3_support)) - sum(grepl('10', data$Top3_support))
sum(grepl('2', data$Top3_support))
sum(grepl('3', data$Top3_support))
sum(grepl('4', data$Top3_support))
sum(grepl('5', data$Top3_support))
sum(grepl('6', data$Top3_support))
sum(grepl('7', data$Top3_support))
sum(grepl('8', data$Top3_support))
sum(grepl('9', data$Top3_support))
sum(grepl('10', data$Top3_support))

# Table 3. Interventions
table(data$Facebook)
table(data$Website)
table(data$Community_Center)
table(data$Buddy)
table(data$Therapist)

# Table 4. Outness
table(data$Outness_1)
table(data$Outness_2)
table(data$Outness_3)
table(data$Outness_4)
table(data$Outness_5)

#######################
# FEATURE ENGINEERING #
#######################

# Loneliness
#data$lonely <- (data$lonely - mean(data$lonely))/sd(data$lonely)

# Outness
data$out[is.na(data$out)] <- mean(data$out, na.rm=TRUE)

# Unacceptibility
data$unaccepted[is.na(data$unaccepted)] <- mean(data$unaccepted, na.rm=TRUE)

# Support
data$support[is.na(data$support)] <- mean(data$support, na.rm=TRUE)

# Transgender/other
data$trans <- ifelse(data$Gender %in% c('Transgender female','Transgender male','Other'), 1, 0)

# Cis-female
data$female <- ifelse(data$Gender == 'Cis-female', 1, 0)

# Bisexual
data$bisexual <- ifelse(data$Sexualorientation == 'Bisexual', 1, 0)

# Education
data$postsecondary <- ifelse(data$Education == 'Post-secondary', 1, 0)

# Language
data$english <- ifelse(data$English_proficiency %in% c('Excellent','Very good','Good'), 1, 0)

# Immigration status
data$status <- ifelse(data$Immigration_status == 'Granted', 1, 0)

# Years lived
data$years_lived <- log1p(data$years_lived)

# Reduce data
columns <- c('rhs','out','support','lonely','Age','years_lived','postsecondary','english','status','trans','female','bisexual')
data_reduce <- data[,columns]

# Save separate data frame for supplementary model (rhs ~ acceptance among out people)
# trans is removed because there are no trans people after subsetting to only out individuals (i.e., no out trans people!)
columns_out <- c('rhs','unaccepted','support','lonely','Age','years_lived','postsecondary','english','status','female','bisexual')
data_out <- data[data$out==1,columns_out]

##########
# MODELS #
##########

# Univariate models
glm(rhs ~ out, family='binomial', data=data_reduce) %>% 
  summary() 
glm(rhs ~ support, family='binomial', data=data_reduce) %>% 
  summary() # ***
glm(rhs ~ lonely, family='binomial', data=data_reduce) %>% 
  summary() # ***
glm(rhs ~ Age, family='binomial', data=data_reduce) %>% 
  summary()
glm(rhs ~ years_lived, family='binomial', data=data_reduce) %>% 
  summary()
glm(rhs ~ postsecondary, family='binomial', data=data_reduce) %>% 
  summary()
glm(rhs ~ english, family='binomial', data=data_reduce) %>% 
  summary()
glm(rhs ~ status, family='binomial', data=data_reduce) %>% 
  summary() # *
glm(rhs ~ trans, family='binomial', data=data_reduce) %>% 
  summary() 
glm(rhs ~ female, family='binomial', data=data_reduce) %>% 
  summary() 
glm(rhs ~ bisexual, family='binomial', data=data_reduce) %>% 
  summary() 

# Multivariate models
glm(rhs ~ ., family='binomial', data=data_reduce) %>%
  summary() # lonely***, status**, out*, english*, trans.

# AIC model comparison
mod <- glm(rhs ~ ., family='binomial', data=data_reduce, na.action=na.fail)
dredge_mod <- dredge(mod)
dredge_mod[dredge_mod$AICc < (min(dredge_mod$AICc) + 2),] # within 2 AIC
dredge_mod[1:10,] # top 10

# Model averaging
model.avg(dredge_mod, subset = delta < 2)

########################
# SUPPLEMENTARY MODELS #
########################

# Factor analysis to check that outness is unitary scale
fa <- n_factors(data[,out_cols])
fa # 1 factor supported by 8/10 methods

# Differences between Russians and non-Russians
lm(russian ~ ., data=data[,c('russian','out','support','lonely','Age','years_lived','postsecondary','english','status','trans','female','bisexual')]) %>%
  summary()

# Among out individuals, does acceptance affect RHS
glm(rhs ~ ., family='binomial', data=data_out) %>%
  summary() 

# Try model with interaction between outness and sex (in case men and women suffer different psychological consequences to being out)
glm(rhs ~ out*female+support+lonely+Age+years_lived+postsecondary+english+status+trans+bisexual, family='binomial', data=data_reduce) %>%
  summary() 

#######
# END #
#######