library(tidyverse)
library(micromapST)

###################################################################
###################################################################

# Micromap for comparing male and female deathrates in 2012 and 2014

##import dataset
MV_death <- read.csv("MotorVehicle.csv", header = TRUE, sep = ",")

##add a column that calculates the difference between 2014 male death rates and 2012 male death rates to the data frame  
MV_death$MaleDiff <- with(MV_death, Male..2014 - Male..2012)

##create a new male data frame 
males <- MV_death[,c('State','Male..2012','Male..2014','MaleDiff')]

# create a column that is all zeroes in order to create an anchor for the difference column
males$Zero <- rep(0,nrow(males))

##remove states without data for one or both years: DC, Vermont and Rhode Island
males <- subset(males, State!= 'District of Columbia, Vermont, Rhode Island')

##create panel description for males that includes two bar charts and an arrow chart 
##bar charts show male death rates in 2012 and 2014
##arrow chart shows the positive or negative difference between the death rates in 2014 and 2012
##column 5 is the column containing the 0s

MV_death <- na.omit(MV_death)
panelDescU <- data.frame(
  type=c('maptail','id','bar','bar','arrow'),
  lab1=c('','','Male 2012','Male 2014','Change from 2012-2014'),
  lab2=c('' ,'','per 100,000','per 100,000','per 100,000'),
  col1 = c(NA,NA,'2','3','5'),
  col2 = c(NA,NA,NA,NA,'4'))

##create micromap PDF for males, sorted by death rate difference column
fName = "Male1.pdf"
pdf(file=fName,width=10.5,height=8.5)

micromapST(males, panelDescU,
           rowNamesCol='State',
           rowNames='full',
           sortVar='4',ascend=FALSE,
           axisScale = "o",
           title=c("Motor vehicle death rates of males in 2012 & 2014 - All ages", "Source: CDC National Center for Injury Prevention and Control"),
           ignoreNoMatches=TRUE)
dev.off()

#########################################################
#########################################################
##Motor vehicle death rates for females in 2012 and 2014

##import dataset
MV_death <- read.csv("MotorVehicle.csv", header = TRUE, sep = ",")

##add a column that calculates the difference between 2014 male death rates and 2012 male death rates to the data frame  
MV_death$FemaleDiff <- with(MV_death, Female..2014 - Female..2012)

##create a new male data frame 
females <- MV_death[,c('State','Female..2012','Female..2014','FemaleDiff')]

##create a column that is all zeroes in order to create an anchor for the difference column
females$Zero <- rep(0,nrow(females))

##remove states without data for one or both years: Rhode Island, Hawaii, the District of Columbia, Vermont and Alaska 
sub_females <- females[-c(41, 11, 16, 24, 43), ]

##create panel description for females that includes two bar charts and an arrow chart 
##bar charts show female death rates in 2012 and 2014
##arrow chart shows the positive or negative difference between the death rates in 2014 and 2012
##column 5 is the column containing the 0s
panelDescU <- data.frame(
  type=c('maptail','id','bar','bar','arrow'),
  lab1=c('','','Female 2012','Female 2014','Change from 2012-2014'),
  lab2=c('' ,'','per 100,000','per 100,000','per 100,000'),
  col1 = c(NA,NA,'2','3','5'),
  col2 = c(NA,NA,NA,NA,'4'))

##create micromap for females
fName = "Female.pdf"
pdf(file=fName,width=10.5,height=8.5)
micromapST(sub_females, panelDescU,
           rowNamesCol='State',
           rowNames='full',
           sortVar='4',ascend=FALSE,
           axisScale = "o",
           title=c("Motor vehicle death rates of females in 2012 & 2014 - All ages", "Source: CDC National Center for Injury Prevention and Control"),
           ignoreNoMatches=TRUE)
dev.off()


#################################################################
#################################################################

# Micromap for Death rates comparison of All ages in 2012 and 2014

# read in CSV
death_rates <- read.csv('MotorVehicle.csv')

# add a calculated column to the death_rates data frame that subtracts 2014 death rates from 2012
death_rates$AllAgesDiff <- with(death_rates, All.Ages..2014 - All.Ages..2012)

# create a new all ages data frame for ease of manipulation
all_ages <- death_rates[,c('State','All.Ages..2012','All.Ages..2014','AllAgesDiff')]

# create a column that is all zeroes in order to create an anchor for the difference column
all_ages$Zero <- rep(0,nrow(all_ages))

# remove DC since it kept messing up and it's not actually a state
all_ages <- subset(all_ages, State!= 'District of Columbia')

# panel description that includes 2 bar charts and 1 arrow chart
# bar charts take 2012 total and 2014 totals. Arrow charts takes the difference
# numbers in the col sections refer to columns 2,3,4, and 5 in the data frame
# 5 is the zero column to anchor the arrows

panel_desc <- data.frame(
  type=c('map','id','bar','bar','arrow'),
  lab1=c(NA,NA,'All Ages 2012','All Ages 2014', 'Difference'),
  lab2=c(NA,NA,'Per 100,000 People','Per 100,000 People', 'Overall Percentage'),
  col1= c(NA,NA,2,3,5),
  col2=c(NA,NA,2,3,4)
)

# Create PDF using all_ages dataset and panel description
# Rank by column 4 - aka Difference
# Sort descending
# Ignore null values

fName = "Motor Vehicle Death Rates.pdf"
pdf(file=fName,width=7.75,height=10)
micromapST(all_ages, panel_desc,
           rowNamesCol='State',
           rowNames='full',
           sortVar= 4,
           ascend=FALSE,
           title=c('Motor Vehicle Accident Death Rates - 2012 & 2014 - All Ages',
                   'Source: CDC National Center for Injury Prevention and Control'),
           ignoreNoMatches=TRUE)
dev.off()

##############################################
##############################################

# Micromap for comparing deathrates comparison of ages 55+ in 2012 and 2014

#import dataset
MV_death <- read.csv(file="MotorVehicle.csv", header = TRUE, sep = ",")

##add a column that calculates the difference between 2014 55+ death rates and 2012 55+ death rates to the data frame  
MV_death$oldDiff <-(MV_death$Age.55...2014 - MV_death$Age.55...2012)

##create a new data frame
old <- MV_death[,c('State','Age.55...2012','Age.55...2014','oldDiff')]

##create a column that is all zeroes in order to create an anchor for the difference column
old$Zero <- rep(0,nrow(old))

##remove states without data for one or both years 
elderly <- old[-c(41, 11, 27, 19), ]

##create panel description for 55+ that includes two bar charts and an arrow chart 
panelDesc <- data.frame(
  type=c('maptail','id','bar','bar','arrow'),
  lab1=c('','','Age 55+ 2012','Age 55+ 2014','Change from 2012-2014'),
  lab2=c('' ,'','per 100,000','per 100,000','per 100,000'),
  col1 = c(NA,NA,'2','3','5'),
  col2 = c(NA,NA,NA,NA,'4'))

##create micromap for 55+
fName = "old55+.pdf"
pdf(file=fName,width=10.5,height=8.5)
micromapST(elderly, panelDesc,
           rowNamesCol='State',
           rowNames='full',
           sortVar='4',ascend=FALSE,
           axisScale = "o",
           title=c("Motor Vehicle Death Rates of Age 55+ in 2012 & 2014", "Source: CDC National Center for Injury Prevention and Control"),
           ignoreNoMatches=TRUE)
dev.off()

##################################################################
##################################################################

# Micromap for comparing deathrates for ages 0-20 in 2012 and 2014

# Read the file MotorVehicle.csv into a vector
dmv <-read.csv(file = "MotorVehicle.csv",header = T,as.is = TRUE)

# Summarize for fetching the deatils about the dataset
summary(dmv)

# Remove the null values from the dataset
dmv <- na.omit(dmv)

# Adding a difference column to store the difference of deathrates from 2012 and 2014
dmv$diff <- dmv$Age.0.20..2014-dmv$Age.0.20..2012

# Adding an all zeroes column to the dataset for better visualizations
dmv$zero <- rep(0,nrow(dmv))

# The panel description which we used to define two bar graphs for Ages 0-20 in 2012 and 2014, and one arrow graph 
# for the difference between deathrates in 2012 and 2014
PanelDesc <-data.frame(
  type=c('map','id','bar','bar','arrow'),
  lab1=c(NA,NA,'Ages 0-20 2012','Ages 0-20 2014','Difference from 2012 to 2014'),
  lab2=c(NA,NA,"per 100,000 people","per 100,000 people",NA),
  col1=c(NA,NA,'Age.0.20..2012','Age.0.20..2014','zero'),
  col2=c(NA,NA,NA,NA,'diff')
)

# Create a micromap for the deathrates in ages 0-20 from 2012 to 2014 and the difference

fname = "DeathrateMV.pdf"
pdf(file=fname,width=10.5,height=10)
micromapST(dmv,PanelDesc,
           sortVar = 'diff',
           ascend= FALSE,
           rowNames = 'full',
           rowNamesCol = 'State',
           axisScale = "o",
           title=c("Motor Vehicle Accident Death Rates in 2012 per 100,000 people", "Source: CDC National Center for Injury Prevention and Control"),
           ignoreNoMatches = TRUE)
dev.off()