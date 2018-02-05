# Team Avivo - Exploratory Data Analysis

rm(list=ls())
library(dplyr)
library(tidyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(ggthemes)
setwd("~/teamavivo")


# lets get the admission data in first 
dat <- as_data_frame(fread("Daanes admissions 1.1.14-12.31.17.csv",header = TRUE,
                           na.strings=c("NA","NaN","","N/A","unknown","unknwn","None","-","Na"," "),sep =",",
                           skip = 0,stringsAsFactors = TRUE,check.names = T))



# We first check the size and structure of data
dim(dat)
summary(dat)
str(dat)
head(dat)



# Convert the Form date field to date format
dat$`Form Date` <- as.Date(dat$`Form Date`,format = "%m/%d/%y")



# Replace all the spaces in the column names, makes it easy to work with them in r
colnames(dat) <- make.names(colnames(dat), unique=TRUE)




####### Queestion 1 - There are multiple entries for the same client number. Those client numbers are 
#######                111157844,111157920,111160126,111156150,111156354,111156731. 
#######                Why is that? We are assuming that there should be one record for each customer, is that right?
View (dat %>% 
        filter(Client.Number== 4547) %>% 
        select(Form.Date,Program,Staff,Disabilities.or.Barriers.to.Treatment,disability_flag))




######## check the class of the data attributes - almost all of them are factors, 
######## we need to convert them to right format before we start our EDA
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



###### Lets drop the column Form Ineffective date because it is all NAs
dat <- dat[,-2]




##### Lets check the summary of the data again now
summary(dat)



#### Lets start by looking at the different programs offered by Avivo and their split
df_program <- dat %>%
  group_by(Program) %>%
  summarize(counts = n()) %>% 
  mutate(percent_total = round(counts / sum(counts) * 100,2))

ggplot(data=df_program, aes(reorder(x=Program,percent_total),y = percent_total))+ 
  geom_bar(colour="black", width=.8, stat="identity") + 
  guides(fill=FALSE) +
  xlab("Program Name") + ylab("Count") +
  ggtitle("Avivo CT Programs Overview") + theme_tufte() + coord_flip() +
  geom_text(aes(label=percent_total),hjust =-0.25,color='black')
#  Top 5 programs constitute almost 80% of the program.




##### Lets now start looking at why patients leave the program, we have a variable called Reasons for Discharge
df_discharges <- dat %>%
  group_by(Reason.for.Discharge) %>%
  summarize(counts = n()) %>% 
  mutate(percent_total = round(counts / sum(counts) * 100,2))

ggplot(data=df_discharges, aes(reorder(x=Reason.for.Discharge,percent_total),y = percent_total))+ 
  geom_bar(colour="black", width=.8, stat="identity") + 
  guides(fill=FALSE) +
  xlab("Discharge Reason") + ylab("Count") +
  ggtitle("Reason for Discharges") + theme_tufte() + coord_flip() +
  geom_text(aes(label=percent_total),hjust =-0.25,color='black')
# 30.8% of the patients left without staff approval, patient conduct is also a big factor, almost 20%.Wonder what that means!





#### Let's create a flag for patients who completed the program and another flag for patients who 
####  left without staff approval and compare two populations
dat$Completed.Program <- 0
dat$Completed.Program <-  as.factor(ifelse(dat$Reason.for.Discharge %in% c("Completed program"), 1, 0))




### Lets review the distribution of these two populations by the different numeric variables we have
par(mfrow=c(3,2), mar=c(2,2,2,2))
plot(aggregate(Completed.Program~Days.used.in.past.30..Primary.,data=dat, mean), type="b", main="Completion by usage")
plot(aggregate(Completed.Program~Hours.of.treatment,data=dat, mean), type="b", main="Hours.of.treatment")
plot(aggregate(Completed.Program~Number.of.detox.sessions.in.past.30.days,data=dat, mean), type="b", main="Detox.sessions.in.past.30.days")
plot(aggregate(Completed.Program~Number.of.Group.Counseling.sessions.in.past.30.days,data=dat, mean), type="b", main="Completion by Group Counselling sessions")
plot(aggregate(Completed.Program~Number.of.Individual.Counseling.sessions.in.past.30.days,data=dat, mean), type="b", main="Completion by Individual Counselling sessions")
plot(aggregate(Completed.Program~Number.of.Spiritual.support.sessions.in.past.30.days,data=dat, mean), type="b", main="Completion with self help sessions")
par(mfrow=c(1,1))






##### Lets do some comparison of the categorical variables by completion rate
## Does having children show any effect on program completion?
df_completion_children <- dat %>%
  group_by(Does.client.have.children,Completed.Program) %>%
  summarize(counts = n()) %>% 
  mutate(percent_total = round(counts / sum(counts) * 100,2))

ggplot(data=df_completion_children, aes(x=Does.client.have.children,y = percent_total,fill=Completed.Program))+ 
  geom_bar(colour="black", width=.8, stat="identity") + 
  xlab("Client has Children") + ylab("Count") +
  ggtitle("Completion rate by Children") + theme_tufte() + 
  geom_text(aes(label=percent_total),color='black',vjust = 1.5) +
  scale_fill_manual(values=c("#999999", "#E69F00"))

# there is not much significant difference we can see between them





##### 
## Does labor force status have any effect on program completion?
df_completion_laborforce <- dat %>%
  group_by(Current.labor.force.status,Completed.Program) %>%
  summarize(counts = n()) %>% 
  mutate(percent_total = round(counts / sum(counts) * 100,2))

ggplot(data=df_completion_laborforce, aes(x=Current.labor.force.status,y = percent_total,fill=Completed.Program))+ 
  geom_bar(colour="black", width=.8, stat="identity") + 
  xlab("Labor Force Status") + ylab("Count") +
  ggtitle("Completion Rate by labor force status") + theme_tufte() + 
  geom_text(aes(label=percent_total),color='black',vjust = 1.5) +
  scale_fill_manual(values=c("#999999", "#E69F00")) + coord_flip()

dat$employment_flag <- 1
dat$employment_flag <-ifelse(trimws(dat$Current.labor.force.status) %in% 
                               c("Employed full-time (>= 35 hours/week)",
                                 "Employed part-time (< 35 hours/week)"),0,1)
dat$employment_flag <- as.factor(dat$employment_flag)
chisq.test(table(dat$Completed.Program,dat$employment_flag)) # very high correlation between employment and completion rate

# full time or part time employment has a higher percentage of completions, even the 
# laid off who are still looking for work has a higher completion rate



##### 
## Does the abuse history have any correlation with program completion?

df_completion_abuse <- dat %>%
  group_by(Perpetrator.of.Abuse,Completed.Program) %>%
  summarize(counts = n()) %>% 
  mutate(percent_total = round(counts / sum(counts) * 100,2)) %>% 
  select(Perpetrator.of.Abuse,Completed.Program,percent_total)


ggplot(data=df_completion_abuse, aes(x=Perpetrator.of.Abuse,y = percent_total,fill=Completed.Program))+ 
  geom_bar(colour="black", width=.8, stat="identity",position = 'dodge') + 
  geom_text(aes(label=percent_total),color='black',position = position_dodge(width = 1), vjust = -0.5, size = 3) +
  xlab("Perpretator of Abuse") + ylab("Percent Total") +
  ggtitle("Completion Rate by abuse indicator") + 
  theme_tufte() + 
  scale_fill_manual(values=c("#999999", "#E69F00")) 


dat$abuse_history_flag <- 0
dat$abuse_history_flag <-  as.factor(ifelse(trimws(dat$Perpetrator.of.Abuse) %in%
                                         c("Yes, physical and sexual","Yes, physical only","Yes, sexual only"), 1, 0))

prop.table(table(dat$Completed.Program,dat$abuse_history_flag),1)

chisq.test(table(dat$Completed.Program,dat$abuse_history_flag))
# it seems like there is a very high inverse correlation between abuse history and program completion rate 

###### Does the disability have any relation with completion rate

dat$disability_flag <- 1
dat$disability_flag <-ifelse(trimws(dat$Disabilities.or.Barriers.to.Treatment) == "<NA>",0,1)
dat$disability_flag[is.na(dat$disability_flag)] <- 0
dat$disability_flag <- as.factor(dat$disability_flag)
chisq.test(table(dat$Completed.Program,dat$disability_flag))
# The chisquare test tells us there is no relationship here.

