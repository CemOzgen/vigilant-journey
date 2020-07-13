# Assignment-1 ------------------------------------------------------
#
#
# In this assignment, we have three different dataset about 
# death causes. CDC (Centers for Disease Control), is a national
# public health institute in the US. Google, on the other hand,
# draws data from its own search engine. Lastly, New York Times
# data signifies which death causes NYT mentions in its news.
# The assignment paves the way to analyze whether there is a
# correlation between the real causes of death and what cause
# does people engages through Google and New York Times.
#
#
#Step-1: Setting up Packages-----------------------------------------
#
# First, we have to use library function to load the packages.
#
library(tidyverse)
library(rio)
library(dplyr)
#
#Step-2: Data Importing---------------------------------------------
#
# All three files will be imported to the R Script.
#
# read_csv is needed when file is separeted by comma.
Google <- read_csv("Assignments/Google-trends.csv")
# 
# read_tsv is needed when file is separeted by tab.
CDC <- read_tsv(file = "Assignments/CDC.txt")
# list.files is used in order to import and combine all different 
# year files at once.
NYT <- list.files(path = "Assignments/Years/", full.names = TRUE) %>%
  map_df(read_csv)
#
#
#
# Step-3: Data Inspection----------------------------------------------
#
# Here, you can see several functions to inspect the data. It is needed
# in order to understand and know how to handle the data. 
#
#
# glimpse function helps to see the dataframe's columns and indicates
# glimpse of what it has.
glimpse(NYT)
# shows last six rows of the data
tail(NYT)
# shows first six rows of the data
head(NYT)
# shows number of columns in the data
ncol(NYT)
# shows number of rows in the data
nrow(NYT)
# counting the number of each cause 
count(NYT, Words)
# demonstrates unique values of the column "Words"
unique(NYT$Words)
# summary functions helps one by showing the type of columns, median and mean
# values of the columns... and so on
summary(NYT)
#
# glimpse function helps to see the dataframe's columns and indicates
# glimpse of what it has.
glimpse(CDC)
# shows last six rows of the data
tail(CDC)
# shows first six rows of the data
head(CDC)
# shows number of columns in the data
ncol(CDC)
# shows number of rows in the data
nrow(CDC)
# summary functions helps one by showing the type of columns, median and mean
# values of the columns... and so on
summary(CDC)
#
# glimpse function helps to see the dataframe's columns and indicates
# glimpse of what it has.
glimpse(Google)
# shows last six rows of the data
tail(Google)
# shows first six rows of the data
head(Google)
# shows number of columns in the data
ncol(Google)
# shows number of rows in the data
nrow(Google)
# summary functions helps one by showing the type of columns, median and mean
# values of the columns... and so on
summary(Google)
#
#
# Step-4: Data Preparation ----------------------------------------------
#
# Note prior to Data Preparation: 
#
#Defining new name for the CDC Data
NYT_new <- NYT %>%
  # Removing ID column is necessary, since ID is does not have any 
  # functionality when binding the data with other datasets.
  select(-ID) %>%
  # Renaming the column is done due to matching column names with other datasets.
  rename(cause = Words) %>%
  # Mutating the causes into standardized names is done to compare
  # different sources and analyze them.
  mutate(
    cause = recode(cause,
        "alzheimer's disease" = "alzheimer's",
        "heart failure" = "heart disease",
        "cardiovascular disease" = "heart disease",
        "malignant neoplasms" = "cancer",
        "bronchitis" = "respiratory disease",
        "emphysema" = "respiratory disease",
        "asthma" = "respiratory disease",
        "cerebrovascular diseases" = "stroke",
        "terrorist" = "terrorism",
        "terror attack" = "terrorism",
        "unintentional injuries" = "accident",
        "car accident" = "accident",
        "pileup" = "accident",
        "car crash" = "accident",
        "Influenza" = "influenza/pneumonia",
        "pneumonia" = "influenza/pneumonia",
        "flu" = "influenza/pneumonia",
        "nephrosis" = "kidney disease",
        "nephritis" = "kidney disease",
        "nephrotic syndrome" = "kidney disease",
        "self-harm" = "suicide",
        "murder" = "homicide",
        "manslaughter" = "homicide",
        "assassination" = "homicide",
        "shootings" = "homicide",
        "gun violence" = "homicide",
        "knife attack" = "homicide",
        "knifing" = "homicide",
        "lynching" = "homicide",
        "drug overdose" = "overdose"
    )
  ) %>%
  group_by(year, cause) %>%
  # sum all rows which has same causes
  summarize(
    count = sum(count)
) %>%
  group_by(year) %>%
  # sum all rows which has same values in causes column
  mutate(
    count_total = sum(count),
  # changing the number of total causes into proportions
    proportion = count / count_total) %>%
  # deselecting count and count_total, since it is not needed anymore
  select(-count, -count_total) %>%
  # transforming 'numeric' year to 'character' year, so that it can 
  # match with other datasets' year columns.
  transform(year = as.character(year)) %>%
  # added a source name. Thus, when it is combined, it enables to see
  # each row to signify its source
  mutate(
    source = "New York Times")
#
#Defining new name for the CDC Data
CDC_new <- CDC %>%
  # pivot_longer is used in order to change the shape of the data frame
  pivot_longer(cols = -cause, names_to = 'year', values_to = 'proportion') %>%
  # this will arrange the data frame according to year
  arrange(year) %>%
  # Mutating the causes into standardized names is done to compare
  # different sources and analyze them.
  mutate(cause = recode(cause,
      "alzheimer" = "alzheimer's",
      "cancer_all" = "cancer",
      "cancer_lung" = "cancer",
      "cancer_anal" = "cancer",
      "cancer_breast" = "cancer",
      "homicide_all" = "homicide",
      "homicide_firearm" = "homicide",
      "homicide_legmil" = "homicide",
      "influpneu" = "influenza/pneumonia",
      "heart" = "heart disease",
      "loresp" = "respiratory disease",
      "kidney" = "kidney disease"
    )) %>%
  group_by(year, cause) %>%
  # sum all rows which has same values in causes column
  summarize(
    proportion = sum(proportion)
  ) %>%
  # added a source name. Thus, when it is combined, it enables to see
  # each row to signify its source
  mutate(
    source = "CDC") 
# 
#
# Defining new name for the Google Data
Google_new <- Google %>%
  # pivot_longer is used in order to change the shape of the data frame
  pivot_longer(cols = -Words, names_to = 'year', values_to = 'proportion') %>%
  # Renaming the column is done due to matching column names with other datasets.
  rename(cause = year) %>%
  rename(year = Words) %>%
  # added a source name. Thus, when it is time to combine it enables to see
  # each row to signify its source
  mutate(source = "Google") %>%
  # this will arrange the data frame according to year
  arrange(year) %>%
  # Mutating the causes into standardized names is done to compare
  # different sources and analyze them.
  mutate(
    cause = recode(cause,
    "car accidents" = "accident",
    "pneumonia" = "influenza/pneumonia"
    )) %>%
  group_by(cause, year) %>%
  # Average is not needed, therefore it is excluded. 
  subset(year!= "Average")
# 
# binding rows is the last step of data preparation, because afterward, 
# one can start either visualize or export the data.
death_data <- bind_rows(Google_new, CDC_new, NYT_new)
#
#checking whether there is a mistake in mutating 
unique(death_data$cause)

# Step 5: Exporting the Data-----------------------------------------------------
export(death_data, "death_data.csv")

# Optional Step 6: Data Visualization--------------------------------------------
#
#
# filtering a year 
not_so_recent <- filter(death_data, year == 2007)
# ggplot helps us to create beautiful and more importantly informative
# visualizations like this one
ggplot(not_so_recent, aes(x = cause, y = proportion, fill = source)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
