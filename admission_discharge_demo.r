
rm(list=ls())

library(dplyr)
library(tidyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(xlsx)
setwd("~/teamavivo")

dat_admission <- as_data_frame(fread("Daanes admissions 1.1.14-12.31.17.csv",header = TRUE,
                           na.strings=c("NA","NaN","","N/A","unknown","unknwn","None","-","Na"," ","99"),sep =",",
                           skip = 0,stringsAsFactors = TRUE,check.names = T))

dat_discharge <- as_data_frame(fread("Daanes discharges 1.1.14-12.31.17.csv",header = TRUE,
                                     na.strings=c("NA","NaN","","N/A","unknown","unknwn","None","-","Na"," ","99"),sep =",",
                                     skip = 0,stringsAsFactors = TRUE,check.names = T))
dat_demo <- as_data_frame(read.xlsx("Age Calculation for Demographics.xlsx",sheetName = "Demographics-Boyd"))



colnames(dat_admission) <- suppressWarnings(make.names(colnames(dat_admission), unique=TRUE))

colnames(dat_discharge) <- suppressWarnings(make.names(colnames(dat_discharge), unique=TRUE))

colnames(dat_demo) <- suppressWarnings(make.names(colnames(dat_demo), unique=TRUE))

#Dropping of the Form Ineffective date column which is useless

drops <- c("Form.Ineffective.Date")
dat_admission <- dat_admission[ , !(names(dat_admission) %in% drops)]

# Convert the Form date field to date format
dat_admission$`Form.Date` <- as.Date(dat_admission$`Form.Date`,format = "%m/%d/%y")

dat_discharge$`Form.Date` <- as.Date(dat_discharge$`Form.Date`,format = "%m/%d/%y")

dat_admission$Client.Number <- as.factor(dat_admission$Client.Number)

dat_discharge$Client.Number <- as.factor(dat_discharge$Client.Number)

dat_demo$Client.Number <- as.factor(dat_demo$Client.Number)

dat_demo[duplicated(dat_demo),]

dat_demo <- unique(dat_demo) # lets remove the duplicate records

dat_admission <- left_join(dat_admission,dat_demo,by = c("Client.Number"))

dat_admission$Form.Flag <- "admission"

dat_discharge <- left_join(dat_discharge,dat_demo,by = c("Client.Number"))

dat_discharge$Form.Flag <- "discharge"

dat <- rbind(dat_admission,dat_discharge)

write.csv(dat, "admission_discharge_demographic.csv")

