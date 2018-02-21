
rm(list=ls())

library(dplyr)
library(tidyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(xlsx)
setwd("~/teamavivo")

##### import the datesets

dat_admission <- as_data_frame(fread("Daanes admissions 1.1.14-12.31.17.csv",header = TRUE,
                           na.strings=c("NA","NaN","","N/A","unknown","unknwn","None","-","Na"," ","99"),sep =",",
                           skip = 0,stringsAsFactors = TRUE,check.names = T))
dat_discharge <- as_data_frame(fread("Daanes discharges 1.1.14-12.31.17.csv",header = TRUE,
                                     na.strings=c("NA","NaN","","N/A","unknown","unknwn","None","-","Na"," ","99"),sep =",",
                                     skip = 0,stringsAsFactors = TRUE,check.names = T))
dat_demo <- as_data_frame(read.xlsx("Age Calculation for Demographics.xlsx",sheetName = "Demographics-Boyd"))




#### make sure that columns are named correctly

colnames(dat_admission) <- suppressWarnings(make.names(colnames(dat_admission), unique=TRUE))
colnames(dat_discharge) <- suppressWarnings(make.names(colnames(dat_discharge), unique=TRUE))
colnames(dat_demo) <- suppressWarnings(make.names(colnames(dat_demo), unique=TRUE))


#### Dropping off the Form Ineffective date column which is useless
drops <- c("Form.Ineffective.Date")
dat_admission <- dat_admission[ , !(names(dat_admission) %in% drops)]

#### Convert the Form date field to date format and converting Client number to factor
dat_admission$Form.Date <- as.Date(dat_admission$Form.Date,format = "%m/%d/%Y")
dat_discharge$Form.Date <- as.Date(dat_discharge$Form.Date,format = "%m/%d/%Y")
dat_admission$Client.Number <- as.factor(dat_admission$Client.Number)
dat_discharge$Client.Number <- as.factor(dat_discharge$Client.Number)
dat_demo$Client.Number <- as.factor(dat_demo$Client.Number)



#### lets remove the duplicate records from the demographic dataset
dat_demo[duplicated(dat_demo),]
dat_demo <- unique(dat_demo) 



#### Now lets merge the files together
# add demo data to both the files
dat_admission <- left_join(dat_admission,dat_demo,by = c("Client.Number"))
dat_admission$Form.Flag <- "admission"
dat_discharge <- left_join(dat_discharge,dat_demo,by = c("Client.Number"))
dat_discharge$Form.Flag <- "discharge"



#### append the admission and discharge files together
dat <- rbind(dat_admission,dat_discharge)


#### write the files to share with the team
write.csv(dat, "admission_discharge_demographic.csv")

#### lets do some more preprocessing and then save the dataset for my analysis
##check the class of the data attributes - almost all of them are factors, 
##we need to convert them to right format before we start our EDA
#table(sapply(dat,class))
# colnames(dat) # check the colnames
cols.num <- c("Days.used.in.past.30..Primary.",
              "Days.used.in.past.30..Tertiary.",                                    
              "Days.used.in.past.30.days..Secondary.", 
              "Hours.of.treatment",
              "Number.of..sessions.living.skills.development.in.past.30.days",        
              "Number.of.alcohol.drug.testing.sessions.in.past.30.days",       
              "Number.of.arrests.in.past.30.days",                                   
              "Number.of.Childcare.sessions.in.past.30.days",                         
              # "Number.of.children",                                                   
              # "Number.of.children.client.lost.parental.rights.to",                   
              # "Number.of.children.living.elsewhere",                                  
              "Number.of.Co.Occurring.mental.illness.sessions.in.past.30.days",       
              "Number.of.Coordination.of.services.sessions.in.past.30.days",          
              "Number.of.days.client.used.alcohol.in.past.30.days",                   
              "Number.of.days.client.used.illicit.drugs.in.past.30.days",             
              "Number.of.detox.sessions.in.past.30.days",                             
              "Number.of.Employment.Education.sessions.in.past.30.days",              
              "Number.of.Group.Counseling.sessions.in.past.30.days",                  
              "Number.of.Individual.Counseling.sessions.in.past.30.days",             
              # "Number.of.interactions.with.supportive.family.friends.in.past.30.days",
              "Number.of.Medical.Care.sessions.in.past.30.days",                      
              "Number.of.Relationship.Family.Counseling.sessions.in.past.30.days",    
              # "Number.of.self.help.programs.attended.in.past.30.days",                
              "Number.of.Spiritual.support.sessions.in.past.30.days",                 
              "Number.of.Substance.Abuse.Education.sessions.in.past.30.days",         
              "Number.of.Therapeutic.recreation.sessions.in.past.30.days",            
              "Number.of.Transportation.services.in.past.30.days",
              "Total.Charges..dollars.only."
              )
dat[cols.num] <- sapply(sapply(dat[cols.num],as.character),as.numeric)

#### save the rdata file so that it is ready to use for future analysis
saveRDS(dat, "dat.rds")

rm(list=ls())
