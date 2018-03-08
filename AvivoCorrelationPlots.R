
# Change Recovery Environment Column Names
colnames(AvivoCorrClean)[colnames(AvivoCorrClean)=="Will_client_be_living_in_environment__conducive_to_recovery-Unknown"] <- "Unknown"
colnames(AvivoCorrClean)[colnames(AvivoCorrClean)=="Will_client_be_living_in_environment__conducive_to_recovery-Yes"] <- "Yes"
colnames(AvivoCorrClean)[colnames(AvivoCorrClean)=="Will_client_be_living_in_environment__conducive_to_recovery-No"] <- "No"

# Change Self-Help Column Names
colnames(AvivoCorrClean)[colnames(AvivoCorrClean)=="Number_of_self_help_programs_attended_in_past_30_days-1-3 times past month (less than once per week)"] <- "1-3 last month"
colnames(AvivoCorrClean)[colnames(AvivoCorrClean)=="Number_of_self_help_programs_attended_in_past_30_days-16-30 times past month (0ver 3 times per week)"] <- "3 or More Weekly"
colnames(AvivoCorrClean)[colnames(AvivoCorrClean)=="Number_of_self_help_programs_attended_in_past_30_days-4-7 times past month (once per week)"] <- "Weekly"
colnames(AvivoCorrClean)[colnames(AvivoCorrClean)=="Number_of_self_help_programs_attended_in_past_30_days-8-15 times past month (2 or 3 times per week)"] <- "2 or 3 Weekly"
colnames(AvivoCorrClean)[colnames(AvivoCorrClean)=="Number_of_self_help_programs_attended_in_past_30_days-No attendance"] <- "None"
colnames(AvivoCorrClean)[colnames(AvivoCorrClean)=="Number_of_self_help_programs_attended_in_past_30_days-Some attendance, but frequency unknown"] <- "Some"
colnames(AvivoCorrClean)[colnames(AvivoCorrClean)=="Number_of_self_help_programs_attended_in_past_30_days-Unknown"] <- "Unknown"

colnames(AvivoCorrClean)[colnames(AvivoCorrClean)=="Number_of_interactions_with_supportive_family_friends_in_past_30_days-No"] <- "No Interactions"
colnames(AvivoCorrClean)[colnames(AvivoCorrClean)=="Number_of_interactions_with_supportive_family_friends_in_past_30_days-Unknown"] <- "Unknown Interactions"
colnames(AvivoCorrClean)[colnames(AvivoCorrClean)=="Number_of_interactions_with_supportive_family_friends_in_past_30_days-Yes"] <- "Yes Interactions"

colnames(AvivoCorrClean)[colnames(AvivoCorrClean)=="CHSR_Dimension_4-Extreme problem"] <- "Extreme Problem"
colnames(AvivoCorrClean)[colnames(AvivoCorrClean)=="CHSR_Dimension_4-Serious problem"] <- "Serious Problem"
colnames(AvivoCorrClean)[colnames(AvivoCorrClean)=="CHSR_Dimension_4-Moderate problem"] <- "Moderate Problem"
colnames(AvivoCorrClean)[colnames(AvivoCorrClean)=="CHSR_Dimension_4-Minor problem"] <- "Minor Problem"
colnames(AvivoCorrClean)[colnames(AvivoCorrClean)=="CHSR_Dimension_4-No problem"] <- "No Problem"
colnames(AvivoCorrClean)[colnames(AvivoCorrClean)=="CHSR_Dimension_4-Unable to assess"] <- "Unable to Assess"



# Overall Plot

OverallPlot <-AvivoCorrClean[,c("completed-0","completed-1","Unknown","No","Yes","Unknown","Some","1-3 last month","Weekly","2 or 3 Weekly","3 or More Weekly","No Interactions","Unknown Interactions","Yes Interactions")]
install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(cor(OverallPlot),p.mat = cor_pmat(OverallPlot), hc.order = TRUE, type = "lower",insig = "blank",
                      outline.col = "white",
                      ggtheme = ggplot2::theme_gray,
                      colors = c("#6D9EC1", "white", "#E46726"))

# CHSR Dimension 4 Plot
CHSR4Plot <-AvivoCorrClean [,c("completed-0","completed-1","Extreme Problem","Serious Problem","Moderate Problem","Minor Problem", "No Problem", "Unable to Assess")]
ggcorrplot(cor(CHSR4Plot), p.mat = cor_pmat(CHSR4Plot), hc.order=FALSE, type='lower',
           lab = TRUE,insig = "blank",outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))
           

# Recovery Environment Plot
RecoveryEnvironmentCorr <-AvivoCorrClean[,c("completed-0","completed-1","Unknown","No","Yes")]
ggcorrplot(cor(RecoveryEnvironmentCorr), p.mat = cor_pmat(RecoveryEnvironmentCorr), hc.order=FALSE, type='lower',
           lab = TRUE,insig = "blank",outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))

# Self Help Plot
SelfHelpHorr <-AvivoCorrClean[,c("completed-0","completed-1","None","Unknown","Some","1-3 last month","Weekly","2 or 3 Weekly","3 or More Weekly")]
ggcorrplot(cor(SelfHelpHorr), p.mat = cor_pmat(SelfHelpHorr), hc.order=FALSE, type='lower',
           lab = TRUE,insig = "blank",outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))


# Supportive Interactions Plot
supprotiveinteractionscorr <-AvivoCorrMatrix [,c("completed-0","completed-1","No Interactions","Unknown Interactions","Yes Interactions")]
ggcorrplot(cor(supprotiveinteractionscorr), p.mat = cor_pmat(supprotiveinteractionscorr), hc.order=FALSE, type='lower',
           lab = TRUE,insig = "blank",outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))

