###########################################################################
#exploration script
#Miley Thomson
#LIS 4493 Data Stewardship
###########################################################################
library(tidyverse)
library(reshape2)
library(dplyr)

data <- read.csv("C:\\Users\\Miley\\OneDrive\\school 24-25\\spring\\data-stewerdship\\week8\\Seattle_Book_Checkouts_2010_2017-1.csv", na.strings = c("", "NA"))
library_checkouts <- data.frame(data)
#summarize the data 
summary(library_checkouts)

#determine missing values 
na_counts <- library_checkouts %>%
  summarise_all(~ sum(is.na(.)))
na_counts

#problem 1: creators contains 33686 missing values, Subjects containt 6131 
# missing values, Publisher contains 19062 missing values, and PublicationYear
# contains 19038 "" values

#viw unique values and their counts in Publication Year
count_PublicationYear <- melt(table(library_checkouts$PublicationYear))
count_PublicationYear
#there are a few problems here. 
#problem 2: PublicationDate contains information that should be in multiple fields
# namely, their being both copywrite date and publication date. this should 
# be altered so that there is a  publication data feild and a copywrited date field
#problem 3: 
# the syntax for PublicationDate is not consistent. 

#view unique values in 'Publisher' 
count_Publisher <- melt(table(library_checkouts$Publisher))
count_Publisher

#there is a significant problem in Publisher:
#Problem 4: the syntax of distinct publishers is not consisestent leading to 
# what should be the same publisher being read as a distinct publisher. 
# some issues are the presence or lack of spaces in publisher names, the presence of 
# unnecesary commas, and inconsistent capilatization. 

#view unique values in 'Subjects'
count_Subjects <- melt(table(library_checkouts$Subjects))
count_Subjects
#no major problems are found on initial cursury look. 

#view unique values in 'Creator'
count_Creator <- melt(table(library_checkouts$Creator))
count_Creator
#two issues are noticed in the Creator feature. 
#problem 5: there are Dates included in some of the creators records which 
# should not be present. 

#view unique values in 'CheckoutYear'
count_CheckoutYear <- melt(table(library_checkouts$CheckoutYear))
count_CheckoutYear
#no issues are found 

#view unique values in 'CheckoutMonth'
count_CheckoutMonth <- melt(table(library_checkouts$CheckoutMonth))
count_CheckoutMonth
#no issues are found 

#view unique values in 'CheckoutType'
count_CheckoutType <- melt(table(library_checkouts$CheckoutType))
count_CheckoutType
#problem 6: CheckoutType is a meaningless feature as it contains only one value.

#view unique values in 'MaterialType'
count_MaterialType <- melt(table(library_checkouts$MaterialType))
count_MaterialType
#problem 7: MaterialType is a meaningless feature as it contains only one value.

#view unique values in Usage Class
count_UsageClass <- melt(table(library_checkouts$UsageClass))
count_UsageClass
#problem 8: UsageClass is a meaningless feature as it contains only one value.

#view unique values in 'Titles'
count_Titles <- melt(table(library_checkouts$Title))
count_Titles 


###########################################################################
#cleaning script
#Miley Thomson
#LIS 4493 Data Stewardship
###########################################################################
library(tidyverse)
library(reshape2)
library(dplyr)

data <- read.csv("C:\\Users\\Miley\\OneDrive\\school 24-25\\spring\\data-stewerdship\\week8\\Seattle_Book_Checkouts_2010_2017-1.csv", na.strings = c("", "NA"))
library_checkouts <- data.frame(data)
library_checkouts_clean <- library_checkouts

#create a new feature 'CheckoutDate' and populate it with CheckoutYear and CheckoutMonth
for(i in 1:nrow(library_checkouts)){
  if (library_checkouts$CheckoutMonth[i] == 1){
    library_checkouts_clean$CheckoutMonthText[i] <- 'January'
  }else if (library_checkouts$CheckoutMonth[i] == 2){
    library_checkouts_clean$CheckoutMonthText[i] <- 'Febuary'
  }else if (library_checkouts$CheckoutMonth[i] == 3){
    library_checkouts_clean$CheckoutMonthText[i] <- 'March'
  }else if (library_checkouts$CheckoutMonth[i] == 4){
    library_checkouts_clean$CheckoutMonthText[i] <- 'Apil'
  }else if (library_checkouts$CheckoutMonth[i] == 5){
    library_checkouts_clean$CheckoutMonthText[i] <- 'May'
  }else if (library_checkouts$CheckoutMonth[i] == 6){
    library_checkouts_clean$CheckoutMonthText[i] <- 'June'
  }else if (library_checkouts$CheckoutMonth[i] == 7){
    library_checkouts_clean$CheckoutMonthText[i] <- 'July'
  }else if (library_checkouts$CheckoutMonth[i] == 8){
    library_checkouts_clean$CheckoutMonthText[i] <- 'August'
  }else if (library_checkouts$CheckoutMonth[i] == 9){
    library_checkouts_clean$CheckoutMonthText[i] <- 'September'
  }else if (library_checkouts$CheckoutMonth[i] == 10){
    library_checkouts_clean$CheckoutMonthText[i] <- 'October'
  }else if (library_checkouts$CheckoutMonth[i] == 11){
    library_checkouts_clean$CheckoutMonthText[i] <- 'November'
  }else if (library_checkouts$CheckoutMonth[i] == 12){
    library_checkouts_clean$CheckoutMonthText[i] <- 'December'
  }else{
    library_checkouts_clean$CheckoutMonthText[i] <- 'NULL'
  }
  
  library_checkouts_clean$CheckoutDate[i] <- paste(library_checkouts_clean$CheckoutMonthText[i], library_checkouts$CheckoutYear[i])
}
features_selected <- c('CheckoutDate','Checkouts','Title')
library_checkouts_prepared <- library_checkouts_clean[, colnames(library_checkouts_clean) %in% features_selected]
