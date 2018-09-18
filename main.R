### setup

library(jsonlite)
library(tidyverse)
library(data.table)

train <- read.csv('train.csv')
test <- read.csv('test.csv')

install.packages('devtools')

devtools::install_github("Microsoft/LightGBM",
                         subdir = "LightGBM",
                         build_vignettes = TRUE)

library(LightGBM)



# All JSON columns must be turned into proper arrays.
# i.e. with a separator "," between each object and wrapped up with "[]" 

json2df <- function(x) { 
  sprintf('[%s]', paste(x, collapse = ',')) %>%
    fromJSON(flatten = T)
}

# Since there aren't too many columns we could manually create a new dataframe.
# Column names may be checked with names(train).

train <- data.frame(
  
  fullVisitorId = train$fullVisitorId, # ID
  channelGrouping = train$channelGrouping,
  date = train$date,
  sessionID = train$sessionId,
  socialEngagementType = train$socialEngagementType,
  visitId = train$visitId,
  visitNumber = train$visitNumber,
  visitStartTime = train$visitStartTime,
  
  json2df(train$device), # device (JSON)
  json2df(train$geoNetwork), #geoNetwork (JSON)
  json2df(train$totals), # totals (JSON)
  json2df(train$trafficSource) # trafficSource (JSON)
  
)

# Do the same for the test set!

test <- data.frame(
  
  fullVisitorId = test$fullVisitorId, # ID
  channelGrouping = test$channelGrouping,
  date = test$date,
  sessionID = test$sessionId,
  socialEngagementType = test$socialEngagementType,
  visitId = test$visitId,
  visitNumber = test$visitNumber,
  visitStartTime = test$visitStartTime,
  
  json2df(test$device), # device (JSON)
  json2df(test$geoNetwork), #geoNetwork (JSON)
  json2df(test$totals), # totals (JSON)
  json2df(test$trafficSource) # trafficSource (JSON)
  
)

# Save them as new csv files!

write.csv(train.df, "traindf.csv", row.names = F)
write.csv(test.df, "testdf.csv", row.names = F)


### Data checking & formatting

## Drop variables with 1 unique factor

toRm.tr <- sapply(train, function(x) length(unique(x)) == 1) %>%
  which() %>% names()
toRm.te <- sapply(test, function(x) length(unique(x)) == 1) %>%
  which() %>% names()

identical(names(toRm.tr), names(toRm.te)) # True thus drop them all!

train <- subset(train, select = -toRm.tr)
test <- subset(test, select = -toRm.te)

## Convert characters into proper data type, with NA coersions.

# From observations, there are a different types of missing value presentations
# 1. NA
# 2. not available in demo dataset
# 3. metro: (not set)
# 4. network.Domain: unknown.unknown
# 5. campaign: (not set)
# 6. keyword: (not provided)

strCleaner <- function(x) {
  type.convert(x,
               na.strings = c('not available in demo dataset',
                              '(not set)', '(not provided)',
                              'unknown.unknown'),
               as.is = T)
}

toConvert <- sapply(train, mode) == 'character'
train[, toConvert] <- sapply(train[, toConvert], strCleaner)

toConvert <- sapply(test, mode) == 'character'
test[, toConvert] <- sapply(test[, toConvert], strCleaner)

## Check missing values

sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))

names(train)

