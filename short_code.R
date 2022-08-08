################### noar nosher dashboard data ###################
#Libraries 
library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

###### schools' general information  ######
##### create one schools' general information file from MABAT website #####
#### import schools' general information files  #### using readxl
mabat_2022 <- read_excel("C:/Users/danie/Desktop/machon/education_3/raw_data/mabat/mabat_2022.xlsx")
mabat_2021 <- read_excel("C:/Users/danie/Desktop/machon/education_3/raw_data/mabat/mabat_2021.xlsx")
mabat_2020 <- read_excel("C:/Users/danie/Desktop/machon/education_3/raw_data/mabat/mabat_2020.xlsx")
mabat_2019 <- read_excel("C:/Users/danie/Desktop/machon/education_3/raw_data/mabat/mabat_2019.xlsx")
mabat_2018 <- read_excel("C:/Users/danie/Desktop/machon/education_3/raw_data/mabat/mabat_2018.xlsx")
mabat_2017 <- read_excel("C:/Users/danie/Desktop/machon/education_3/raw_data/mabat/mabat_2017.xlsx")
mabat_2016 <- read_excel("C:/Users/danie/Desktop/machon/education_3/raw_data/mabat/mabat_2016.xlsx")
mabat_2015 <- read_excel("C:/Users/danie/Desktop/machon/education_3/raw_data/mabat/mabat_2015.xlsx")
mabat_2014 <- read_excel("C:/Users/danie/Desktop/machon/education_3/raw_data/mabat/mabat_2014.xlsx")
mabat_2013 <- read_excel("C:/Users/danie/Desktop/machon/education_3/raw_data/mabat/mabat_2013.xlsx")
mabat_2012 <- read_excel("C:/Users/danie/Desktop/machon/education_3/raw_data/mabat/mabat_2012.xlsx")

#### unite all data frames together  ####
mabat <- rbind(mabat_2012, mabat_2013, mabat_2014, mabat_2015, mabat_2016, mabat_2017,
               mabat_2018, mabat_2019, mabat_2020, mabat_2021, mabat_2022)

###### shkifut information  ######
##### education information  #####
### import files ### 
edu_2014_2016 <- read_excel("C:/Users/danie/Desktop/machon/education_3/raw_data/shkifut/edu_2014_2016.xlsx")
edu_2017_2018 <- read_excel("C:/Users/danie/Desktop/machon/education_3/raw_data/shkifut/edu_2017_2018.xlsx")
edu_2019_2020 <- read_excel("C:/Users/danie/Desktop/machon/education_3/raw_data/shkifut/edu_2019_2020.xlsx")

## unite files ##
edu_united <- rbind(edu_2014_2016, edu_2017_2018, edu_2019_2020)

##### bagrut information  #####
### import file ### 
bagrut_2015_2020 <- read_excel("C:/Users/danie/Desktop/machon/education_3/raw_data/shkifut/bagrut_2015_2020.xlsx")

##### budget information  #####
### import files ###
budget_2014_2016 <- read_excel("C:/Users/danie/Desktop/machon/education_3/raw_data/shkifut/budget_2014_2016.xlsx")
budget_2017_2020 <- read_excel("C:/Users/danie/Desktop/machon/education_3/raw_data/shkifut/budget_2017_2020.xlsx")

## unite files ##
budget_united <- rbind(budget_2014_2016, budget_2017_2020)

###### socioeconomic information  ######
#### import SIE  edited file ####
stat_SIE <- read_excel("C:/Users/danie/Desktop/machon/education_3/proccesed_data/stat_SIE.xlsx")

### create quin. SIE variable ###
## create  a data frame with schools which have SIE information 
SIE <- stat_SIE %>% 
  select(school_symbol, SIE_2017) %>%
  filter(!is.na(SIE_2017))

#function for creating quintiles
quin <- function(var1) {
  floor(rank(var1) * 5 / (length(var1) + 1))
}

## apply quin. function
SIE$SIE_quin <- quin(SIE$SIE_2017)
SIE$SIE_quin <- recode(SIE$SIE_quin, "0"="1", "1"="2", "2"="3", "3"="4", "4"="5")

##### classifications  #####
### import edited file ###
classification <- read_excel("C:/Users/danie/Desktop/machon/education_3/raw_data/classifications/haredi_dati/elazar_classification/classification.xlsx")
sug_yeshiva <- read_excel("C:/Users/danie/Desktop/machon/education_dataset2/sug_yeshiva2.xlsx")

## delete duplicated rows of the same yeshiva
sug_yeshiva <- sug_yeshiva %>% distinct(school_symbol, .keep_all = T)

##### boys-girls numbers #####
### import files ###
bg_hiloni <- read_excel("C:/Users/danie/Desktop/machon/education_3/raw_data/classifications/hiloni/bg_hiloni.xlsx")
bg_haredi <- read_excel("C:/Users/danie/Desktop/machon/education_3/raw_data/classifications/haredi_dati/bg_haredi.xlsx")

dataset_edu <- merge(x = by_age_10, y = mabat, by = c("school_symbol", "year"), all = T)
dataset_edu <- merge(x = dataset_edu, y = bg_nums, by = c("school_symbol", "year"), all.x = T)
dataset_edu <- merge(x = dataset_edu, y = edu_united, , by = c("school_symbol", "year"), all = T)
dataset_edu <- merge(x = dataset_edu, y = bagrut_2015_2020, , by = c("school_symbol", "year"), all = T)
dataset_edu <- merge(x = dataset_edu, y = budget_united, , by = c("school_symbol", "year"), all = T)
dataset_edu <- merge(x = dataset_edu, y = stat_SIE, by = "school_symbol", all.x = T)
dataset_edu <- merge(x = dataset_edu, y = classification , by = "school_symbol", all.x = T)
dataset_edu <- merge(x = dataset_edu, y = sug_yeshiva, by = "school_symbol", all.x = T)

writexl::write_xlsx(dataset_edu, "C:/Users/danie/Desktop/machon/education_dataset2/dataset2.xlsx")

