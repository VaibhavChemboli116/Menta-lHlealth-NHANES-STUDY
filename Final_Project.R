library(NHANES)
library(nhanesA)
library(dplyr)
library(ggplot2)
library(table1)
library(corrplot)
library(car)
library(patchwork)

data("NHANES")
ls(NHANES)
df <- NHANES[!duplicated(NHANES$ID),]

#Test Hypothesis1: Finding which Age group and which gender have more depression 
vars1 <- c("Gender", "Age", "DaysMentHlthBad", "Depressed")
sample_df1 <- df %>%
  select(all_of(vars1))

hist(sample_df1$Age)
hist(sample_df1$DaysMentHlthBad)

tbl1 <- table(sample_df1$Gender)
barplot(tbl1, col = 'azure1',ylim = c(0,3500), main = 'Barplot of Gender')
tbl2 <- table(sample_df1$Depressed)
barplot(tbl2, col = 'azure3',ylim = c(0,3500), main = 'Barplot of Depressed')

#Recode all categorical variables into factors

sample_df1$Gender_Factors <- factor(sample_df1$Gender)
sample_df1$Depressed_Factors <- factor(sample_df1$Depressed)

sample_df1$Age_Factors <- ifelse(sample_df1$Age <= 4, "Infant",
                                 ifelse(sample_df1$Age <= 12, "Childhood",
                                        ifelse(sample_df1$Age <= 19, "Adolescence",
                                               ifelse(sample_df1$Age <= 39, "Young Adult",
                                                      ifelse(sample_df1$Age <= 59, "Adult",
                                                             ifelse(sample_df1$Age <= 80, "Senior Adult"))))))
sample_df1$Age_Factors <- factor(sample_df1$Age_Factors)

sample_df1$DaysMentHlthBad_Factors <- ifelse(sample_df1$DaysMentHlthBad == 0, "No Impact",
                                             ifelse(sample_df1$DaysMentHlthBad >= 1 & sample_df1$DaysMentHlthBad <= 5, "Low Impact",
                                                    ifelse(sample_df1$DaysMentHlthBad >= 6 & sample_df1$DaysMentHlthBad <= 10, "Moderate Impact",
                                                           ifelse(sample_df1$DaysMentHlthBad >= 11 & sample_df1$DaysMentHlthBad <= 20, "High Impact",
                                                                  ifelse(sample_df1$DaysMentHlthBad >= 21 & sample_df1$DaysMentHlthBad <= 30, "Severe Impact", NA)))))
sample_df1$DaysMentHlthBad_Factors <- factor(sample_df1$DaysMentHlthBad_Factors)

print(length(na.omit(sample_df1$DaysMentHlthBad)))
print(length(na.omit(sample_df1$Gender)))
print(length(na.omit(sample_df1$Age)))
sample_df1 <- sample_df1[!is.na(sample_df1$DaysMentHlthBad),]

table1 <- table1(~ Age_Factors + Gender_Factors | DaysMentHlthBad_Factors, data = sample_df1 )
print(table1)


ggplot(data = sample_df1, aes(x = Age_Factors, fill = DaysMentHlthBad_Factors)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "Age_Factors", y = "Frequency", fill = "DaysMentHlthBad_Factors") +
  ggtitle("Grouped Bar Plot")

ggplot(data = sample_df1, aes(x = Gender_Factors, fill = DaysMentHlthBad_Factors)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "Gender_Factors", y = "Frequency", fill = "DaysMentHlthBad_Factors") +
  ggtitle("Grouped Bar Plot")

#From the above table and graph we can observe that the severe impact, high impact categories in DaysMentHlthBad_Factors are mostly affected by females from gender group and adults and young adults from age group.

#This is the observation of categories individually affecting the Days Mental Health Bad variable. By performing chi-square test we can verify our observation from the graphs are true or not.


glm_age <- glm(DaysMentHlthBad ~ Age_Factors, family = poisson(link = "log"), data = sample_df1)
summary(glm_age)

glm_gender_male <- glm(DaysMentHlthBad ~ Gender_Factors, family = poisson(link = "log"), data = sample_df1)
summary(glm_gender_male)

glm_gender_female <- glm(DaysMentHlthBad ~ relevel(sample_df1$Gender_Factors, ref = "male"), family = poisson(link = "log"), data = sample_df1)
summary(glm_gender_female)


# From the above result, we can conclude our observation that in Age group Adult and Young Adult are the one which are mostly facing more mental health issues. In gender though all the values are same we can observe the nature of the association which is males have negative association from which we can conclude that, males are having less number of days with mental health bad i.e. females are more depressed.

#Hypothesis 2: Finding what factors contribute more to the Depression
vars2 <- c("Gender", "Age", "HHIncomeMid", "Poverty", "Work", "HomeOwn",
           "Weight", "Height", "BMI", "DaysMentHlthBad", "DaysPhysHlthBad", "LittleInterest",
           "SleepHrsNight", "PhysActiveDays", "Education","Alcohol12PlusYr"
           ,"HardDrugs","HealthGen","TVHrsDay","MaritalStatus","Diabetes")

sample_df2 <- df %>%
  select(all_of(vars2))

print(length(na.omit(sample_df2$DaysMentHlthBad)))
sample_df2 <- sample_df2[!is.na(sample_df2$DaysMentHlthBad), ]

# Lifestyle Factors

sample_df2$PhysActiveDays_Factors <- ifelse(sample_df2$PhysActiveDays >= 1 & sample_df2$PhysActiveDays <= 3, "Low",
                                            ifelse(sample_df2$PhysActiveDays > 3 & sample_df2$PhysActiveDays <= 6, "Moderate", "High"))
sample_df2$PhysActiveDays_Factors <- factor(sample_df2$PhysActiveDays_Factors)

sample_df2$SleepHrsNight_Factors <- ifelse(sample_df2$SleepHrsNight <= 5, "Low",
                                           ifelse(sample_df2$SleepHrsNight > 5 & sample_df2$SleepHrsNight <= 8, "Normal", "High"))
sample_df2$SleepHrsNight_Factors <- factor(sample_df2$SleepHrsNight_Factors)

sample_df2$Alcohol12PlusYr_Factors <- factor(sample_df2$Alcohol12PlusYr)

sample_df2$HardDrugs_Factors <- factor(sample_df2$HardDrugs)

sample_df2$TVHrsDay_Factors <- factor(sample_df2$TVHrsDay)

# Socioeconomic Factors

sample_df2$HHIncomeMid_Factors <- ifelse(sample_df2$HHIncomeMid < 40000, "Low",
                                         ifelse(sample_df2$HHIncomeMid >= 40000 & sample_df2$HHIncomeMid < 75000, "Middle", "High"))
sample_df2$HHIncomeMid_Factors <- factor(sample_df2$HHIncomeMid_Factors)

sample_df2$Poverty_Factors <- ifelse(sample_df2$Poverty < 1, "Low",
                                     ifelse(sample_df2$Poverty >= 1 & sample_df2$Poverty < 3, "Middle", "High"))
sample_df2$Poverty_Factors <- factor(sample_df2$Poverty_Factors)

sample_df2$Education_Factors <- factor(sample_df2$Education)

sample_df2$Work_Factors <- factor(sample_df2$Work)

sample_df2$HomeOwn_Factors <- factor(sample_df2$HomeOwn)

# Demographic Factors

sample_df2$Gender_Factors <- factor(sample_df2$Gender)

sample_df2$Height_Factors <- ifelse(sample_df2$Height < 150, "Short",
                                    ifelse(sample_df2$Height >= 150 & sample_df2$Height < 173, "Average", "Tall"))
sample_df2$Height_Factors <- factor(sample_df2$Height_Factors)

sample_df2$Weight_Factors <- ifelse(sample_df2$Weight < 60, "Underweight",
                                    ifelse(sample_df2$Weight >= 60 & sample_df2$Weight < 80, "Normal",
                                           ifelse(sample_df2$Weight >= 80 & sample_df2$Weight < 100, "Overweight", "Obese")))
sample_df2$Weight_Factors <- factor(sample_df2$Weight_Factors)

sample_df2$Age_Factors <- ifelse(sample_df2$Age <= 4, "Infant",
                                 ifelse(sample_df2$Age <= 12, "Childhood",
                                        ifelse(sample_df2$Age <= 19, "Adolescence",
                                               ifelse(sample_df2$Age <= 39, "Young Adult",
                                                      ifelse(sample_df2$Age <= 59, "Adult",
                                                             ifelse(sample_df2$Age <= 80, "Senior Adult"))))))
sample_df2$Age_Factors <- factor(sample_df2$Age_Factors)

sample_df2$MaritalStatus_Factors <- factor(sample_df2$MaritalStatus)

# Health Factors

sample_df2$BMI_Factors <- ifelse(sample_df2$BMI > 30, "High", ifelse(sample_df2$BMI >= 18.5 & sample_df2$BMI <= 30, "Normal", "Low"))
sample_df2$BMI_Factors <- factor(sample_df2$BMI_Factors)

sample_df2$DaysPhysHlthBad_Factors <- ifelse(sample_df2$DaysPhysHlthBad == 0, "No Impact",
                                             ifelse(sample_df2$DaysPhysHlthBad > 0 & sample_df2$DaysPhysHlthBad <= 5, "Low Impact",
                                                    ifelse(sample_df2$DaysPhysHlthBad > 5 & sample_df2$DaysPhysHlthBad <= 10, "Moderate Impact",
                                                           ifelse(sample_df2$DaysPhysHlthBad > 10 & sample_df2$DaysPhysHlthBad <= 20, "High Impact",
                                                                  ifelse(sample_df2$DaysPhysHlthBad > 20 & sample_df2$DaysPhysHlthBad <= 30, "Severe Impact", NA)))))
sample_df2$DaysPhysHlthBad_Factors <- factor(sample_df2$DaysPhysHlthBad_Factors)

sample_df2$LittleInterest_Factors <- factor(sample_df2$LittleInterest)

sample_df2$HealthGen_Factor <- factor(sample_df2$HealthGen)

sample_df2$Diabetes_Factor <- factor(sample_df2$Diabetes)

#Target Factor
sample_df2$DaysMentHlthBad_Factors <- ifelse(sample_df2$DaysMentHlthBad == 0, "No Impact",
                                             ifelse(sample_df2$DaysMentHlthBad >= 1 & sample_df2$DaysMentHlthBad <= 5, "Low Impact",
                                                    ifelse(sample_df2$DaysMentHlthBad >= 6 & sample_df2$DaysMentHlthBad <= 10, "Moderate Impact",
                                                           ifelse(sample_df2$DaysMentHlthBad >= 11 & sample_df2$DaysMentHlthBad <= 20, "High Impact",
                                                                  ifelse(sample_df2$DaysMentHlthBad >= 21 & sample_df2$DaysMentHlthBad <= 30, "Severe Impact", NA)))))
sample_df2$DaysMentHlthBad_Factors <- factor(sample_df2$DaysMentHlthBad_Factors)

# Tables
table_lifestyle <- table1(~ PhysActiveDays + SleepHrsNight + 
                            PhysActiveDays_Factors + SleepHrsNight_Factors + 
                            HardDrugs_Factors + Alcohol12PlusYr_Factors + TVHrsDay_Factors | DaysMentHlthBad_Factors, data = sample_df2)
print(table_lifestyle)

table_socioeconomic <- table1(~ HHIncomeMid + Poverty + HHIncomeMid_Factors
                              + Poverty_Factors + Education_Factors
                              + Work_Factors + HomeOwn_Factors | DaysMentHlthBad_Factors, data = sample_df2)
print(table_socioeconomic)

table_demographic <- table1(~ Weight + Height + Age + Gender_Factors + Height_Factors
                            + Weight_Factors + Age_Factors + MaritalStatus_Factors | DaysMentHlthBad_Factors, data = sample_df2)
print(table_demographic)

table_health <- table1(~ BMI + DaysPhysHlthBad + DaysPhysHlthBad_Factors
                       + BMI_Factors + LittleInterest_Factors + HealthGen_Factor + Diabetes_Factor | DaysMentHlthBad_Factors, data = sample_df2)
print(table_health)

#Plotting

# Life Style Factor
plot1 <- ggplot(data =  sample_df2, aes(x = PhysActiveDays_Factors, fill = DaysMentHlthBad_Factors)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "PhysActiveDays_Factors", y = "Frequency", fill = "DaysMentHlthBad_Factors") +
  ggtitle("Life Style: PhysActiveDays vs. DaysMentHlthBad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot2 <- ggplot(data =  sample_df2, aes(x = SleepHrsNight_Factors, fill = DaysMentHlthBad_Factors)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "SleepHrsNight_Factors", y = "Frequency", fill = "DaysMentHlthBad_Factors") +
  ggtitle("Life Style: SleepHrsNight vs. DaysMentHlthBad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot3 <- ggplot(data =  sample_df2, aes(x = Alcohol12PlusYr_Factors, fill = DaysMentHlthBad_Factors)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "Alcohol12PlusYr_Factors", y = "Frequency", fill = "DaysMentHlthBad_Factors") +
  ggtitle("Life Style: Alcohol12PlusYr vs. DaysMentHlthBad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot4 <- ggplot(data =  sample_df2, aes(x = HardDrugs_Factors, fill = DaysMentHlthBad_Factors)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "HardDrugs_Factors", y = "Frequency", fill = "DaysMentHlthBad_Factors") +
  ggtitle("Life Style: HardDrugs vs. DaysMentHlthBad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot5 <- ggplot(data =  sample_df2, aes(x = TVHrsDay_Factors, fill = DaysMentHlthBad_Factors)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "TVHrsDay_Factors", y = "Frequency", fill = "DaysMentHlthBad_Factors") +
  ggtitle("Life Style: TVHrsDay vs. DaysMentHlthBad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_plot <- plot1 + plot2 + plot3 + plot4 + plot5
print(combined_plot)

# Socioeconomic Factor

plot1 <- ggplot(data =  sample_df2, aes(x = HHIncomeMid_Factors, fill = DaysMentHlthBad_Factors)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "HHIncomeMid_Factors", y = "Frequency", fill = "DaysMentHlthBad_Factors") +
  ggtitle("HHIncomeMid vs. DaysMentHlthBad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot2 <- ggplot(data =  sample_df2, aes(x = Education_Factors, fill = DaysMentHlthBad_Factors)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "Education_Factors", y = "Frequency", fill = "DaysMentHlthBad_Factors") +
  ggtitle("Education vs. DaysMentHlthBad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot3 <- ggplot(data =  sample_df2, aes(x = Work_Factors, fill = DaysMentHlthBad_Factors)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "Work_Factors", y = "Frequency", fill = "DaysMentHlthBad_Factors") +
  ggtitle("Work vs. DaysMentHlthBad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot4 <- ggplot(data =  sample_df2, aes(x = HomeOwn_Factors, fill = DaysMentHlthBad_Factors)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "HomeOwn_Factors", y = "Frequency", fill = "DaysMentHlthBad_Factors") +
  ggtitle("HomeOwn vs. DaysMentHlthBad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot5 <- ggplot(data =  sample_df2, aes(x = Poverty_Factors, fill = DaysMentHlthBad_Factors)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "Poverty_Factors", y = "Frequency", fill = "DaysMentHlthBad_Factors") +
  ggtitle("Poverty vs. DaysMentHlthBad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_plot <- plot1 + plot2 + plot3 + plot4 + plot5
print(combined_plot)

# Demographic Factor

plot1 <- ggplot(data =  sample_df2, aes(x = Weight_Factors, fill = DaysMentHlthBad_Factors)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "Weight_Factors", y = "Frequency", fill = "DaysMentHlthBad_Factors") +
  ggtitle("Weight vs. DaysMentHlthBad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot2 <- ggplot(data =  sample_df2, aes(x = Height_Factors, fill = DaysMentHlthBad_Factors)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "Height_Factors", y = "Frequency", fill = "DaysMentHlthBad_Factors") +
  ggtitle("Height vs. DaysMentHlthBad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot3 <- ggplot(data =  sample_df2, aes(x = Age_Factors, fill = DaysMentHlthBad_Factors)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "Age_Factors", y = "Frequency", fill = "DaysMentHlthBad_Factors") +
  ggtitle("Age vs. DaysMentHlthBad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot4 <- ggplot(data =  sample_df2, aes(x = Gender_Factors, fill = DaysMentHlthBad_Factors)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "Gender_Factors", y = "Frequency", fill = "DaysMentHlthBad_Factors") +
  ggtitle("Gender vs. DaysMentHlthBad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot5 <- ggplot(data =  sample_df2, aes(x = MaritalStatus_Factors, fill = DaysMentHlthBad_Factors)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "MaritalStatus_Factors", y = "Frequency", fill = "DaysMentHlthBad_Factors") +
  ggtitle("MaritalStatus vs. DaysMentHlthBad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_plot <- plot1 + plot2 + plot3 + plot4 + plot5
print(combined_plot)

# Health Factor

plot1 <- ggplot(data =  sample_df2, aes(x = DaysPhysHlthBad_Factors, fill = DaysMentHlthBad_Factors)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "DaysPhysHlthBad_Factors", y = "Frequency", fill = "DaysMentHlthBad_Factors") +
  ggtitle("DaysPhysHlthBad vs. DaysMentHlthBad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot2 <- ggplot(data =  sample_df2, aes(x = BMI_Factors, fill = DaysMentHlthBad_Factors)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "BMI_Factors", y = "Frequency", fill = "DaysMentHlthBad_Factors") +
  ggtitle("BMI vs. DaysMentHlthBad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot3 <- ggplot(data =  sample_df2, aes(x = LittleInterest_Factors, fill = DaysMentHlthBad_Factors)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "LittleInterest_Factors", y = "Frequency", fill = "DaysMentHlthBad_Factors") +
  ggtitle("LittleInterest vs. DaysMentHlthBad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot4 <- ggplot(data =  sample_df2, aes(x = HealthGen_Factor, fill = DaysMentHlthBad_Factors)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "HealthGen_Factor", y = "Frequency", fill = "DaysMentHlthBad_Factors") +
  ggtitle("HealthGen vs. DaysMentHlthBad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot5 <- ggplot(data =  sample_df2, aes(x = Diabetes_Factor, fill = DaysMentHlthBad_Factors)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "Diabetes_Factor", y = "Frequency", fill = "DaysMentHlthBad_Factors") +
  ggtitle("Diabetes vs. DaysMentHlthBad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_plot <- plot1 + plot2 + plot3 + plot4 + plot5
print(combined_plot)

#From the above table and graph we can observe that in lifestyle category
#The Severe Impact, High Impact categories in DaysMentHlthBad_Factors are mostly affect by people having less physical activity, normal sleep, more alcohol and people who watch atleast 2 or more hours of TV.

#From the above table and graph we can observe that in socio-economic category
#The Severe Impact, High Impact categories in DaysMentHlthBad_Factors are mostly affect by people with less income, working people, college students, and people owning houses.

#From the above table and graph we can observe that in demographic category
#The Severe Impact, High Impact categories in DaysMentHlthBad_Factors are mostly affect by females, adults, young adults, the married ones and so on.

#From the above table and graph we can observe that in health category
#The Severe Impact, High Impact categories in DaysMentHlthBad_Factors are mostly people with less physical issues, no diabetes, normal BMI and good health. Though these variables have severe and high impact on mental health which should not be the case, we plotted graph with observed values the predicted values may show different categories affecting mental health for which we need to construct glm models

#This is the observation of categories individually affecting the Days Mental Health Bad variable. Our hypothesis is to find what category affects the mental health more for which we need to compare AIC, BIC, Estimates values from glm models

# Regression for Life Style Factor
glm_lifestyle <- glm(DaysMentHlthBad ~ PhysActiveDays_Factors + SleepHrsNight_Factors
                     + Alcohol12PlusYr_Factors + HardDrugs_Factors + TVHrsDay_Factors,
                     family = poisson(link = "log"), data = sample_df2)
summary(glm_lifestyle)
vif_results <- car::vif(glm_lifestyle)
print(vif_results)

# Regression for Socio-Economic Factor
glm_socioeconomic <- glm(DaysMentHlthBad ~ HHIncomeMid_Factors + Poverty_Factors
                         + Education_Factors + Work_Factors + HomeOwn_Factors,
                         family = poisson(link = "log"), data = sample_df2)
summary(glm_socioeconomic)
vif_results <- car::vif(glm_socioeconomic)
print(vif_results)

#Since there is collinearity, we remove Poverty
glm_socioeconomic <- glm(DaysMentHlthBad ~ HHIncomeMid_Factors + Education_Factors
                         + Work_Factors + HomeOwn_Factors,
                         family = poisson(link = "log"), data = sample_df2)
summary(glm_socioeconomic)
vif_results <- car::vif(glm_socioeconomic)
print(vif_results)

# Regression for Demographic Factor
glm_demographic <- glm(DaysMentHlthBad ~ Weight_Factors + Height_Factors
                       + Age_Factors + Gender_Factors + MaritalStatus_Factors,
                       family = poisson(link = "log"), data = sample_df2)
summary(glm_demographic)
vif_results <- car::vif(glm_demographic)
print(vif_results)

# Regression for Health Factor
glm_health <- glm(DaysMentHlthBad ~ BMI_Factors + DaysPhysHlthBad_Factors
                  + LittleInterest_Factors + HealthGen_Factor + Diabetes_Factor,
                  family = poisson(link = "log"), data = sample_df2)
summary(glm_health)
vif_results <- car::vif(glm_health)
print(vif_results)

null_deviance_lifestyle <- summary(glm_lifestyle)$null.deviance
residual_deviance_lifestyle <- summary(glm_lifestyle)$deviance[1]
aic_lifestyle <- AIC(glm_lifestyle)
bic_lifestyle <- BIC(glm_lifestyle)

null_deviance_socioeconomic <- summary(glm_socioeconomic)$null.deviance
residual_deviance_socioeconomic <- summary(glm_socioeconomic)$deviance[1]
aic_socioeconomic <- AIC(glm_socioeconomic)
bic_socioeconomic <- BIC(glm_socioeconomic)

null_deviance_demographic <- summary(glm_demographic)$null.deviance
residual_deviance_demographic <- summary(glm_demographic)$deviance[1]
aic_demographic <- AIC(glm_demographic)
bic_demographic <- BIC(glm_demographic)

null_deviance_health <- summary(glm_health)$null.deviance
residual_deviance_health <- summary(glm_health)$deviance[1]
aic_health <- AIC(glm_health)
bic_health <- BIC(glm_health)

# Creating a comparison table
comparison_table <- data.frame(
  Model = c("Lifestyle Factor", "Socio-Economic Factor", "Demographic Factor", "Health Factor"),
  Null_Deviance = c(null_deviance_lifestyle, null_deviance_socioeconomic, null_deviance_demographic, null_deviance_health),
  Residual_Deviance = c(residual_deviance_lifestyle, residual_deviance_socioeconomic, residual_deviance_demographic, residual_deviance_health),
  AIC = c(aic_lifestyle, aic_socioeconomic, aic_demographic, aic_health),
  BIC = c(bic_lifestyle, bic_socioeconomic, bic_demographic, bic_health)
)

print(comparison_table)

# From the above table we can observe that glm_health model has lower AIC and BIC values indicating better fit and the residual deviance is also lower which tells us that it explains more variability than other models

# After health category if we want to select another category which affects the mental health is lifestyle, as it has lower AIC, BIC values and residual deviance but, there is a huge difference in the values compared to the other model because the lifestyle factors have more NA's in the data. So, for this matter of concern we compare estimates and after health category, lifestyle category has more estimates which are not close to zero, which means it has more variables which have stronger association than the other categories. 

#Therefore, health and lifestyle factors affect our mental health more than the socioeconomic and demographic categories.

