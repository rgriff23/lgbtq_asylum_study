################
# PREPARATIONS #
################

# Load packages
library('tidyverse')

# Import files
data <- read.csv('~/Dropbox/Asylum Data/Original data and pre-processing/2018-10-13-data.csv')
column_names <- read.csv('~/Dropbox/Asylum Data/Original data and pre-processing/questions.csv', header=FALSE)

###########
# FILTERS #
###########

# Filter out incomplete surveys
data$Completeness[is.na(data$Completeness)] <- 8
data <- data %>% filter(Completeness > 5) 

# Function to identify invalid responses
invalid <- function(data) {
  # Initiate Boolean matrix
  mat <- matrix(FALSE, ncol=6, nrow=nrow(data))
  # Add 1 if origin country = US or other impossible country
  mat[,1] <- data$Q72 %in% c(75, 65, 9, 84, 185, 31, 10, 48, 99, 187)
  # Add 1 if orientation = hetero & gender identity = cis
  mat[,2] <- data$Q83 == 4 & data$Q82 %in% (1:2)
  # Add 1 if no LGBT friends but listed >0 friends
  mat[,3] <- (data$Q101_1 == 2 & data$Q289 > 0) %in% TRUE
  # Add 1 if LGBT friends but listed 0 friends
  mat[,4] <- (data$Q101_1 == 1 & data$Q289 == 0) %in% TRUE
  # Add 1 if the age is invalid
  mat[,5] <- data$Q81 < 17 | data$Q81 > 100
  # Add 1 if the number of years in US/Canada > Age
  mat[,6] <- (data$Q75 > data$Q81) %in% TRUE
  return(mat)
}

# Filter out invalid surveys (>1 invalid responses)
mat <- invalid(data)
data <- data[rowSums(mat) < 2,]

# Filter out survey times < 9 minutes
data <- data %>% filter(`Duration..in.seconds.` > 9*60)

##################
# RENAME COLUMNS #
##################

# Drop first 11 columns
drop_columns <- c('StartDate','EndDate','Status','Progress', 'Duration..in.seconds.','Finished',
                  'Completeness','RecordedDate','ResponseId','DistributionChannel','UserLanguage')
data <- data %>% select(-drop_columns)

# Rename columns
names(data) <- as.character(column_names$V2)

##################
# COMPUTE SCALES #
##################

# Refugee health screen (response variable)
rhs_columns <- c('RHS_1','RHS_2','RHS_3','RHS_4','RHS_5','RHS_6','RHS_7','RHS_8','RHS_9','RHS_10','RHS_11','RHS_12','RHS_13','RHS_14')
data$rhs <- rowSums(data[,rhs_columns]) > 25 | (data[,'RHS_distress'] > 4) %in% TRUE

# Outness (leaving out sparse Outness3 questions)
out_columns <- c('Outness_1','Outness_2','Outness_4','Outness_5')
data$Outness_1 <- ifelse(data$Outness_1 == 4, NA, data$Outness_1)
data$Outness_2 <- ifelse(data$Outness_2 == 4, NA, data$Outness_2)
data$Outness_4 <- ifelse(data$Outness_4 == 4, NA, data$Outness_4)
data$Outness_5 <- ifelse(data$Outness_5 == 4, NA, data$Outness_5)
data$out <- rowMeans(data[,out_columns], na.rm = TRUE)
data$out <- ifelse(is.nan(data$out), NA, data$out)
#fa <- n_factors(data[,out_columns]) # factor analysis
#fa$summary

# Emotional support (use PROMIS scoring scale)
support_columns <- c('EMSupport_1','EMSupport_2','EMSupport_3','EMSupport_4')
data$support <- rowSums(data[,support_columns])
data$support <- plyr::mapvalues(data$support,
          from=4:20,
          to=c(25.7,29.9,32.1,34,35.7,37.3,38.9,40.5,42.1,43.7,45.4,47.2,49,50.8,53,55.6,62))
#t.test(data$support, mu=50) # t-test comparing to population mean

# Loneliness
lonely_columns <- c('Loneliness_1','Loneliness_2','Loneliness_3','Loneliness_4')
data$lonely <- rowSums(data[,lonely_columns])

############################
# MAP CATEGORICAL FEATURES #
############################

# Language
data$language %>% unique()
data$language <- plyr::mapvalues(data$language,
                                from=1:5,
                                to=c('English','French','Spanish','Russian','Arabic'))

# Country of origin
from <- data$origin_country %>% unique() %>% sort()
data$origin_country <- plyr::mapvalues(data$origin_country,
                                 from=from,
                                 to=c('Afghanistan','Argentina','Armenia','Australia','Azerbaijan','Belarus','Belize',
                                      'Brazil','Burundi','Canada','China','Colombia','Croatia','Cuba','Cyprus','Dominican Republic',
                                      'Ecuador','El Salvador','Gambia','Georgia','Germany','Ghana','Honduras','Indonesia',
                                      'Iran','Iraq','Italy','Jamaica','Jordan','Kazakhstan','Kenya','Kyrgyzstan','Mali','Mexico',
                                      'Nigeria','Panama','South Korea','Republic of Moldova','Russia','Rwanda',
                                      'Senegal','Serbia','Syria','Tunisia','Turkey','Uganda','Ukraine',
                                      'Great Britain & Northern Ireland','United States','Uruguay',
                                      'Uzbekistan','Venezuela','Zimbabwe'))

# Region of origin
data$region <- plyr::mapvalues(data$origin_country,
                               from=sort(unique(data$origin_country)),
                               to=c('ECA','LA','MENA', NA,'ECA','ECA', 'LA', 
                                    'LA','SSA',NA,'A', 'LA','ECA','C','ECA', 
                                    'C','LA','LA','SSA','ECA',NA,'SSA', 
                                    'LA','A', 'MENA','MENA', NA,'C','C','ECA',
                                    'SSA','ECA','SSA','LA','SSA','LA','A', NA,
                                    'ECA','SSA','SSA','ECA','MENA','MENA','MENA','SSA','ECA',
                                    NA,NA,'LA','ECA','LA','SSA')) 

# Country of residence
data$residence_country %>% unique() %>% sort()
data$residence_country <- plyr::mapvalues(data$residence_country,
                                          from=1:2,
                                          to=c('United States','Canada'))

# Current US state / Canadian province
from <- data$state %>% unique() %>% sort()
data$state <- plyr::mapvalues(data$state,
                              from=from,
                              to=c('AL','AZ','CA','CT','DE','DC','FL','GA','HI','ID','IL','KA','KY','LA','ME','MD','MA','MN',
                                   'NV','NJ','NY','NC','OK','OR','PA','TX','VA','WA'))
data$province %>% unique() %>% sort()
data$province <- plyr::mapvalues(data$province,
                              from=c(1,7),
                              to=c('AB','ON'))
which(!is.na(data$state) & !is.na(data$province)) # One person entered a state AND a province
data$state_province <- ifelse(is.na(data$state), data$province, data$state)
sum(is.na(data$state_province)) # Data is complete

# Gender identity
data$Gender %>% unique() %>% sort()
data$Gender <- plyr::mapvalues(data$Gender,
                               from=1:6,
                               to=c('Cis-male','Cis-female','Transgender male','Transgender female','Other','Other'))

# Sexual orientation
data$Sexualorientation %>% unique() %>% sort()
data$Sexualorientation <- plyr::mapvalues(data$Sexualorientation,
                                          from=1:5,
                                          to=c('Lesbian','Bisexual','Gay','Heterosexual','Other'))

# Age range
data$Age %>% unique() %>% sort()
data$age_group <- cut(data$Age,
                      breaks=c(0,20,29,39,49,100),
                      labels=c('<21','21-29','30-39','40-49','50-59'))

# Years in US/Canada
data$years_lived
data$years_lived <- ifelse(is.na(data$years_lived), 0, data$years_lived)
data$years_lived_grp <- cut(data$years_lived,
                        breaks=c(0,1,3,5,7,9,11,13,100),
                        labels=c('<1','1-2','3-4','5-6','7-8','9-10','11-12','12+'), include.lowest=TRUE, right=FALSE)
  
# Stage of asylum process
data$Immigration_status %>% unique %>% sort
data$Immigration_status <- plyr::mapvalues(data$Immigration_status,
                                           from=1:5,
                                           to=c('NotSubmitted','Submitted','WorkPermit','Granted','Denied'))

# English proficiency
data$English_proficiency %>% unique() %>% sort()
data$English_proficiency <- plyr::mapvalues(data$English_proficiency,
                                            from=1:5,
                                            to=c('Excellent','Very good','Good','Fair','Beginner'))

# Education
data$Education %>% unique() %>% sort()
data$Education <- plyr::mapvalues(data$Education,
                                  from=1:4,
                                  to=c('None','Primary','Secondary','Post-secondary'))

# Employment
data$Employment %>% unique() %>% sort()
data$Employment <- plyr::mapvalues(data$Employment,
                                   from=1:2,
                                   to=c('Yes','No'))

# In school
data$School %>% unique() %>% sort()
data$School <- plyr::mapvalues(data$School,
                               from=1:2,
                               to=c('Yes','No'))

# Number of LGBTQ friends
data$LGBTQFriends_number <- cut(data$LGBTQFriends_number,
                                breaks=c(0,1,3,6,11,1001),
                                labels=c('0','1-2','3-5','6-10','11+'), include.lowest=TRUE, right=FALSE)

#####################
# EXPORT CLEAN DATA #
#####################

# Export
write.csv(data, file='~/Dropbox/Asylum Data/Clean data and code/2018-12-01-clean-data.csv',
          row.names = FALSE)

#######
# END #
#######