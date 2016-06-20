## from http://jason.bryer.org/likert/
## blogpost http://reganmian.net/blog/2013/10/02/likert-graphs-in-r-embedding-metadata-for-easier-plotting/
#requiåre(devtools)
#install_github('jbryer/likert', force = TRUE)
require(likert)
ls("package:likert")
library("dplyr")
detach("package:reshape", unload=TRUE)
library("reshape2")
library("cowplot")

## setworking directory and load data
setwd("~/Github/NSB_surveys/data")
results_dir <- "~/Github/NSB_survey/results"
mydata <- "NSB_impact.csv"

## read in and clean data
mydata <- read.csv(mydata, header=TRUE, na.strings=c("NA","NaN", " ", "") )
names(mydata)[27] <- "ResearchGrowth"
names(mydata)[28] <- "CareerDevelopment"
names(mydata)[29] <- "ProfessionalNetwork"
names(mydata)[30] <- "NoData"
names(mydata)[31] <- "ObtainFunding"
names(mydata)[13] <- "role"


## slim to people with 1 role only
mydata <- mydata %>%
  filter(role %in% c("Course Director", "Course Assistant", "Faculty", "Student", "Teaching Assistant")) %>%
  filter(grepl('impact', ResearchGrowth)) %>%
  filter(grepl('impact', ObtainFunding)) %>%
  filter(grepl('impact', CareerDevelopment))
str(mydata)
summary(mydata)

## subset and order data
impact <- mydata %>%
  select(ResearchGrowth, CareerDevelopment, ProfessionalNetwork, ObtainFunding) %>%
  rename(c(ResearchGrowth = "Research Growth", 
           CareerDevelopment = "Career Development", 
           ProfessionalNetwork = "Professional Network", 
           ObtainFunding = "Ability to Obtain Funding"))
summary(impact)
str(impact)

impact$"Research Growth" <- factor(impact$"Research Growth", 
                                   c("No impact",
                                     "Slight impact", 
                                     "Some impact", 
                                     "Moderate impact",
                                     "High impact"))

impact$"Professional Network" <- factor(impact$"Professional Network", 
                                   c("No impact",
                                     "Slight impact", 
                                     "Some impact", 
                                     "Moderate impact",
                                     "High impact"))

impact$"Professional Network" <- factor(impact$"Professional Network", 
                                   c("No impact",
                                     "Slight impact", 
                                     "Some impact", 
                                     "Moderate impact",
                                     "High impact"))

impact$"Ability to Obtain Funding" <- factor(impact$"Ability to Obtain Funding", 
                                   c("No impact",
                                     "Slight impact", 
                                     "Some impact", 
                                     "Moderate impact",
                                     "High impact"))

impact$"Research Growth" <- factor(impact$"Research Growth", 
                                   c("No impact",
                                     "Slight impact", 
                                     "Some impact", 
                                     "Moderate impact",
                                     "High impact"))



## plot grouped by course
impact_g <- likert(impact, grouping = mydata$role)
plot(impact_g)


###################################
## now for importance
## read in and clean data
mydata <- read.csv(mydata, header=TRUE, na.strings=c("NA","NaN", " ", "") )
names(mydata)[35] <- "Lectures"
names(mydata)[36] <- "HandsOnExercises"
names(mydata)[37] <- "IndependentProjects"
names(mydata)[38] <- "StudentPresentations"
names(mydata)[39] <- "InterdisciplinaryResearchExperience"
names(mydata)[40] <- "ProfessionalDevelopment"

## subset and order data
importance <- mydata %>%
  select(Lectures, HandsOnExercises, IndependentProjects, StudentPresentations,
         InterdisciplinaryResearchExperience, ProfessionalDevelopment) %>%
  rename(c(HandsOnExercises = "Hands-on Exercises", 
           IndependentProjects = "Independent Projects", 
           StudentPresentations = "Student Presentations",
           InterdisciplinaryResearchExperience = "Interdisciplinary Research Experience",
           ProfessionalDevelopment = "Professional Development))
summary(impact)
str(impact)

