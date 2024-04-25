# Import data
dissertation_data
head(dissertation_data)
str(dissertation_data)


## Preparing my data

# Rename Columns
colnames(dissertation_data)[colnames(dissertation_data) %in% c("2. Gender", "3. Age", "4. Nationality", "5. What is your department of study?", "6. What is your current academic level?", "8. How would you rate your overall engagement with your studies?")] <-c("Gender", "Age", "Nationality", "Department", "AcadLevel", "OveralLrnEng")
colnames(dissertation_data)[colnames(dissertation_data) %in% c("9.1.a. I actively participate in online class discussions on Blackboard. - Cognitive Engagement", "9.2.a. I critically analyse and engage with the course materials available on Blackboard. - Cognitive Engagement", "9.3.a. I set specific learning goals for myself with my activities on Blackboard. - Cognitive Engagement", "9.4.a. I seek out additional resources available on Blackboard to deepen my understanding of the subject matter. - Cognitive Engagement")] <-c("CogLrnEng1", "CogLrnEng2", "CogLrnEng3", "CogLrnEng4")
colnames(dissertation_data)[colnames(dissertation_data) %in% c("10.1.a. I am interested in the online resources available on Blackboard. - Emotional Engagement", "10.2.a. I feel motivated to succeed academically with the resources and support available on Blackboard. - Emotional Engagement", "10.3.a. I experience feelings of frustration or discouragement while accessing resources available on Blackboard. - Emotional Engagement", "10.4.a. I feel connected with my studies through Blackboard resources and activities. - Emotional Engagement")] <-c("EmoLrnEng1", "EmoLrnEng2", "EmoLrnEng3", "EmoLrnEng4")
colnames(dissertation_data)[colnames(dissertation_data) %in% c("11.1.a. I frequently access lecture and seminar recordings, course materials, and assessment guides available on Blackboard. - Behavioral Engagement", "11.2.a. I often complete assigned readings or directed learnings associated with Blackboard. - Behavioral Engagement", "11.3.a. I participate in extracurricular activities related to my field of study accessible on Blackboard. - Behavioral Engagement", "11.4.a. I seek help or clarification from teaching staff or classmates when I encounter difficulties with resources on Blackboard. - Behavioral Engagement")] <-c("BhvLrnEng1", "BhvLrnEng2", "BhvLrnEng3", "BhvLrnEng4")
colnames(dissertation_data)[colnames(dissertation_data) %in% c("12.1.a. Blackboard - Effectiveness", "12.2.a. OneDrive - Effectiveness", "12.3.a. Microsoft Teams - Effectiveness", "12.4.a. Zoom - Effectiveness", "12.5.a. OneNote - Effectiveness", "12.6.a. Canvas - Effectiveness", "12.7.a. Moodle - Effectiveness", "12.8.a. Turnitin - Effectiveness")] <-c("Blackboard", "OneDrive", "M_Teams", "Zoom", "OneNote", "Canvas", "Moodle", "Turnitin")
colnames(dissertation_data)[colnames(dissertation_data) %in% c("13.1.a. Content – Lecture slides - Perception of Blackboard Features", "13.2.a. Content – Online course materials - Perception of Blackboard Features", "13.3.a. Content – Online reading list - Perception of Blackboard Features", "13.4.a. Content – eBook and e-journals - Perception of Blackboard Features", "13.5.a. Content – Lecture recordings - Perception of Blackboard Features", "13.6.a. Content – Classroom collaborates - Perception of Blackboard Features", "13.7.a. Gradebook - Perception of Blackboard Features", "13.8.a. Discussion board - Perception of Blackboard Features", "13.9.a. Blackboard Announcements - Perception of Blackboard Features", "13.10.a. Blackboard messaging system - Perception of Blackboard Features", "13.11.a. Blackboard Groups - Perception of Blackboard Features")] <-c("LectureSlide", "CourseMaterial", "ReadingList", "ebookandejournal", "LectureRecording", "ClassCollaborate", "Gradebook", "DiscussionBoard", "Announcement", "MessageSystem", "BlackboardGroup")
colnames(dissertation_data)[colnames(dissertation_data) %in% c("14.1.a. Using blackboard enhances my learning productivity. - Usefulness of Blackboard", "14.2.a. Using the resources on blackboard enables me to accomplish tasks more efficiently. - Usefulness of Blackboard", "14.3.a. Using the resources on blackboard improves the quality of my work. - Usefulness of Blackboard", "Zoom - Effectiveness", "14.4.a. Overall, I find blackboard to be useful in my work or daily life. - Usefulness of Blackboard")] <-c("PU1", "PU2", "PU3", "PU4")
colnames(dissertation_data)[colnames(dissertation_data) %in% c("15.1.a. Interacting with blackboard is easy for me.", "15.2.a. Learning to use blackboard was effortless.", "15.3.a. Blackboard is user-friendly.", "Zoom - Effectiveness.", "15.4.a. Overall, I perceive blackboard as easy to use.")] <-c("PEOU1", "PEOU2", "PEOU3", "PEOU4")
colnames(dissertation_data)

## Reverse coding
# install 'car' package
install.packages("car")

# Load package
library(car)

# Re-code column
dissertation_data$EmoLrnEng3 <- recode(dissertation_data$EmoLrnEng3, "1 = 5; 2 = 4; 3 = 3; 4 = 2; 5 = 1")
print(dissertation_data$EmoLrnEng3)


dissertation_data$OveralLrnEng <- as.numeric(recode(dissertation_data$OveralLrnEng, "'Excellent' = 5; 'Good' = 4; 'Neutral' = 3; 'Fair' = 2; 'Poor' = 1"))
print(dissertation_data$OveralLrnEng)



## Frequency and Percentage of demography

# Nationality
fnationality <- table(dissertation_data$Nationality)
fnationality
Pernationality <- prop.table(fnationality)*100
Pernationality
round(Pernationality,0)


## Effectiveness of digital learning tools (mean,median,min,max)

mydata <- dissertation_data[,19:37]
names(mydata)

summary(mydata)

# Calculate variance
var(mydata$Blackboard)
var(mydata$OneDrive)
var(mydata$M_Teams)
var(mydata$Zoom)
var(mydata$OneNote)
var(mydata$Canvas)
var(mydata$Moodle)
var(mydata$Turnitin)


# Calculate Standard deviation
sd(mydata$Blackboard)
sd(mydata$OneDrive)
sd(mydata$M_Teams)
sd(mydata$Zoom)
sd(mydata$OneNote)
sd(mydata$Canvas)
sd(mydata$Moodle)
sd(mydata$Turnitin)


## Perception of Blackboard Features

POBBF <- dissertation_data[,27:37]
names(POBBF)

summary(POBBF)

# Calculate variance
var(POBBF$LectureSlide)
var(POBBF$CourseMaterial)
var(POBBF$ReadingList)
var(POBBF$ebookandejournal)
var(POBBF$LectureRecording)
var(POBBF$ClassCollaborate)
var(POBBF$Gradebook)
var(POBBF$DiscussionBoard)
var(POBBF$Announcement)
var(POBBF$MessageSystem)
var(POBBF$BlackboardGroup)


# Calculate Standard deviation
sd(POBBF$LectureSlide)
sd(POBBF$CourseMaterial)
sd(POBBF$ReadingList)
sd(POBBF$ebookandejournal)
sd(POBBF$LectureRecording)
sd(POBBF$ClassCollaborate)
sd(POBBF$Gradebook)
sd(POBBF$DiscussionBoard)
sd(POBBF$Announcement)
sd(POBBF$MessageSystem)
sd(POBBF$BlackboardGroup)



## Test for Normality
shapiro.test(MyVariables$LectureSlide)
shapiro.test(MyVariables$CourseMaterial)
shapiro.test(MyVariables$LectureRecording)
shapiro.test(MyVariables$PBBCC)
shapiro.test(MyVariables$CogLrnEng)
shapiro.test(MyVariables$EmoLrnEng)
shapiro.test(MyVariables$BhvLrnEng)
shapiro.test(MyVariables$LrnEng)
shapiro.test(MyVariables$PU)
shapiro.test(MyVariables$PEOU)


# Reliability test for internal consistency
# Install psych
install.packages("psych")

# Load package
library(psych)

## Cronbach on Cognitive Learning Engagement

CogLrnEngData <- dissertation_data[,7:10]
names(CogLrnEngData)

# Run Cronbach's alpha
alpha(CogLrnEngData)


## Cronbach on Emotional Learning Engagement

EmoLrnEngData <- dissertation_data[,11:14]
names(EmoLrnEngData)

# Run Cronbach's alpha
alpha(EmoLrnEngData)


## Cronbach on Behavioural Learning Engagement

BhvLrnEngData <- dissertation_data[,15:18]
names(BhvLrnEngData)

# Run Cronbach's alpha
alpha(BhvLrnEngData)


## Cronbach on Learning Engagement

LrnEngData <- dissertation_data[,6:18]
names(LrnEngData)

# Run Cronbach's alpha
alpha(LrnEngData)


## Cronbach on Perceive Usefulness

PUData <- dissertation_data[,38:41]
names(PUData)

# Run Cronbach's alpha
alpha(PUData)


## Cronbach on Perceive Ease of Use

PEOUData <- dissertation_data[,42:45]
names(PEOUData)

# Run Cronbach's alpha
alpha(PEOUData)


## Cronbach on Perceive Ease of Use

PBBCCData <- dissertation_data[,c(27,28,31)]
names(PBBCCData)

# Run Cronbach's alpha
alpha(PBBCCData)


## Correlation ##

# Install packages
install.packages("GGally")
install.packages("ggcorrplot")
library(tidyverse)
library(GGally)
library(broom)
library(ggcorrplot)


## FInd mean for Overall Learning Engagement variable
LrnEng <-c(round(rowMeans(dissertation_data[,6:18])))
LrnEng<-as.data.frame(LrnEng)


## FInd mean for Cognitive variable
CogLrnEng <-c(round(rowMeans(dissertation_data[,7:10])))
CogLrnEng<-as.data.frame(CogLrnEng)

# FInd mean for Emotional variable
EmoLrnEng<-c(round(rowMeans(dissertation_data[,11:14])))
EmoLrnEng<-as.data.frame(EmoLrnEng)

# FInd mean for Behavioural variable
BhvLrnEng<-c(round(rowMeans(dissertation_data[,15:18])))
BhvLrnEng<-as.data.frame(BhvLrnEng)

# FInd mean for PU variable
PU<-c(round(rowMeans(dissertation_data[,38:41])))
PU<-as.data.frame(PU)

# FInd mean for PEOU variable
PEOU<-c(round(rowMeans(dissertation_data[,42:45])))
PEOU<-as.data.frame(PEOU)


# Effectiveness of specific features 
BBFeatures<-c(round(dissertation_data[,c(27,28,31)]))
BBFeatures<-as.data.frame(BBFeatures)

PBBCC<-c(round(rowMeans(dissertation_data[,c(27,28,31)])))
PBBCC<-as.data.frame(PBBCC)



# Variables for the Correlation analysis

MyVariables<-cbind(BBFeatures,PBBCC,CogLrnEng,EmoLrnEng,BhvLrnEng,LrnEng,PU,PEOU)
summary(MyVariables)
head(MyVariables)
str(MyVariables)
colnames(MyVariables)



## Normalize the variables
# Create formula

min_max <- function(x) {
  res <- (x - min(x)) / (max(x) - min(x))
  return(res)
}

MyVariablesNorm <- as.data.frame(sapply(MyVariables[,1:10], min_max))
names(MyVariablesNorm)
summary(MyVariablesNorm)


## Correlation of Normalized Data

# Correlation of All necessary variables
ggcorrplot(cor(MyVariablesNorm), lab = TRUE, type = "lower", title = "Correlation Matrix")
ggpairs(MyVariablesNorm)
cor(MyVariablesNorm)



## Standardization of the variables

z_score <- function(x) {
  res <- (x - mean(x)) / sd(x)
  return(res)
}
MyVariablesStd <- as.data.frame(sapply(MyVariables, z_score))
names(MyVariablesStd)
summary(MyVariablesStd)
MyVariablesStd





## Linearity Test (Scatterred Plot)
install.packages("ggpubr")
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(rlang)


a1 <- ggplot(MyVariables, aes(x = PU, y = LrnEng)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = NULL, y = "Overall Learning Engagegemnt")

a2 <- ggplot(MyVariables, aes(x = PBBCC, y = LrnEng)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = NULL, y = NULL)

a3 <- ggplot(MyVariables, aes(x = PEOU, y = LrnEng)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = NULL, y = NULL)

b1 <- ggplot(MyVariables, aes(x = PU, y = CogLrnEng)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = NULL, y = "Cognitive Learning Engagegemnt")

b2 <- ggplot(MyVariables, aes(x = PBBCC, y = CogLrnEng)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = NULL, y = NULL)

b3 <- ggplot(MyVariables, aes(x = PEOU, y = CogLrnEng)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = NULL, y = NULL)

c1 <- ggplot(MyVariables, aes(x = PU, y = EmoLrnEng)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = NULL, y = "Emotional Learning Engagegemnt")

c2 <- ggplot(MyVariables, aes(x = PBBCC, y = EmoLrnEng)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = NULL, y = NULL)

c3 <- ggplot(MyVariables, aes(x = PEOU, y = EmoLrnEng)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = NULL, y = NULL)

d1 <- ggplot(MyVariables, aes(x = PU, y = BhvLrnEng)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Perceived Usefulness of Blackboard", y = "Behavioural Learning Engagegemnt")

d2 <- ggplot(MyVariables, aes(x = PBBCC, y = BhvLrnEng)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Perception of the Features on Blackboard Content", y = NULL)

d3 <- ggplot(MyVariables, aes(x = PEOU, y = BhvLrnEng)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Perceived Ease of Use", y = NULL)

library(patchwork)

waka<-(a1|a2|a3)/(b1|b2|b3)/(c1|c2|c3)/(d1|d2|d3)

waka <- waka + ggtitle("Scattered Plots of Variables with Reegression Lines")
waka

# Save the plot with the overall title and updated x-axis label
ggsave(filename = "Scatterplot.png", plot = waka,
       width = 25, height = 28, dpi = 2500, units = "cm")



## Normality Plot of the Variabiles
hist(MyVariables$PBBCC, freq = FALSE)

hist(MyVariables$LrnEng)











Model_1 <- lm(LrnEng ~ PU, data = MyVariablesStd)
summary(Model_1)
plot(MyVariablesStd$PU, MyVariablesStd$LrnEng)
abline(Model_1)
plot(Model_1)
Model_2 <- lm(LrnEng ~ PBBCC, data = MyVariablesStd)
summary(Model_2)
Model_3 <- lm(LrnEng ~ PEOU, data = MyVariablesStd)
summary(Model_3)
Model_4 <- lm(CogLrnEng ~ PU, data = MyVariablesStd)
summary(Model_4)
Model_5 <- lm(CogLrnEng ~ PBBCC, data = MyVariablesStd)
summary(Model_5)
Model_6 <- lm(CogLrnEng ~ PEOU, data = MyVariablesStd)
summary(Model_6)
Model_7 <- lm(EmoLrnEng ~ PU, data = MyVariablesStd)
summary(Model_7)
Model_8 <- lm(EmoLrnEng ~ PBBCC, data = MyVariablesStd)
summary(Model_8)
Model_9 <- lm(EmoLrnEng ~ PEOU, data = MyVariablesStd)
summary(Model_9)
Model_10 <- lm(BhvLrnEng ~ PU, data = MyVariablesStd)
summary(Model_10)
Model_11 <- lm(BhvLrnEng ~ PBBCC, data = MyVariablesStd)
summary(Model_11)
Model_12 <- lm(BhvLrnEng ~ PEOU, data = MyVariablesStd)
summary(Model_12)
Model_13 <- lm(PU ~ PBBCC, data = MyVariablesStd)
summary(Model_13)
Model_14 <- lm(PEOU ~ PBBCC, data = MyVariablesStd)
summary(Model_14)


# Regression Analysis of Standardized Data
Model_1 <- lm(LrnEng ~ PU, data = MyVariablesStd)
summary(Model_1)
plot(MyVariablesStd$PU, MyVariablesStd$LrnEng)
abline(Model_1)
Model_2 <- lm(LrnEng ~ PBBCC, data = MyVariablesStd)
summary(Model_2)
Model_3 <- lm(LrnEng ~ PEOU, data = MyVariablesStd)
summary(Model_3)
Model_4 <- lm(CogLrnEng ~ PU, data = MyVariablesStd)
summary(Model_4)
Model_5 <- lm(CogLrnEng ~ PBBCC, data = MyVariablesStd)
summary(Model_5)
Model_6 <- lm(CogLrnEng ~ PEOU, data = MyVariablesStd)
summary(Model_6)
Model_7 <- lm(EmoLrnEng ~ PU, data = MyVariablesStd)
summary(Model_7)
Model_8 <- lm(EmoLrnEng ~ PBBCC, data = MyVariablesStd)
summary(Model_8)
Model_9 <- lm(EmoLrnEng ~ PEOU, data = MyVariablesStd)
summary(Model_9)
Model_10 <- lm(BhvLrnEng ~ PU, data = MyVariablesStd)
summary(Model_10)
Model_11 <- lm(BhvLrnEng ~ PBBCC, data = MyVariablesStd)
summary(Model_11)
Model_12 <- lm(BhvLrnEng ~ PEOU, data = MyVariablesStd)
summary(Model_12)
Model_13 <- lm(PU ~ PBBCC, data = MyVariablesStd)
summary(Model_13)
Model_14 <- lm(PEOU ~ PBBCC, data = MyVariablesStd)
summary(Model_14)





library(car)
vif(lm(LrnEng ~ PU + PBBCC, data = MyVariablesStd))
vif(lm(LrnEng ~ PEOU + PBBCC, data = MyVariablesStd))
vif(lm(CogLrnEng ~ PU + PBBCC, data = MyVariablesStd))
vif(lm(CogLrnEng ~ PEOU + PBBCC, data = MyVariablesStd))
vif(lm(EmoLrnEng ~ PU + PBBCC, data = MyVariablesStd))
vif(lm(EmoLrnEng ~ PEOU + PBBCC, data = MyVariablesStd))
vif(lm(BhvLrnEng ~ PU + PBBCC, data = MyVariablesStd))
vif(lm(BhvLrnEng ~ PEOU + PBBCC, data = MyVariablesStd))
vif(lm(LrnEng ~ PEOU + PU + PBBCC, data = MyVariablesStd))


## Parallel Mediation Analysis using Hayes Process Macros Model 4

process(data=MyVariablesStd, y="LrnEng", x="PBBCC", m=c("PU","PEOU"),model=4,describe=1, stand=1, contrast=1)
process(data=MyVariablesStd, y="CogLrnEng", x="PBBCC", m=c("PU","PEOU"),model=4,describe=1, stand=1, contrast=1)
process(data=MyVariablesStd, y="EmoLrnEng", x="PBBCC", m=c("PU","PEOU"),model=4,describe=1, stand=1, contrast=1)
process(data=MyVariablesStd, y="BhvLrnEng", x="PBBCC", m=c("PU","PEOU"),model=4,describe=1, stand=1, contrast=1)



## Parallel Mediation Analysis using lavaan package

install.packages("lavaan")
library(lavaan)

# Create model for Overall Learning Engagement

Model_A <- 'LrnEng ~ b*PU + d*PEOU + e*PBBCC
PU ~ a*PBBCC
PEOU ~ c*PBBCC
Bbc_Pu_Le:=a*b
bbc_Peou_Le:=c*d
Total:=a*b + c*d + e'

# bootstrap and view summary of model
fit<-sem(Model_A, MyVariablesStd, se="bootstrap", bootstrap=1000)

summary(fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


# Create model for Cognitive Learning Engagement

Model_B <- 'CogLrnEng ~ b*PU + d*PEOU + e*PBBCC
PU ~ a*PBBCC
PEOU ~ c*PBBCC
Bbc_Pu_Le:=a*b
bbc_Peou_Le:=c*d
Total:=a*b + c*d + e'

# bootstrap and view summary of model
fit<-sem(Model_B, MyVariablesStd, se="bootstrap", bootstrap=1000)

summary(fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


# Create model for Emotional Learning Engagement

Model_C <- 'EmoLrnEng ~ b*PU + d*PEOU + e*PBBCC
PU ~ a*PBBCC
PEOU ~ c*PBBCC
Bbc_Pu_CLe:=a*b
bbc_Peou_CLe:=c*d
Total:=a*b + c*d + e'

# bootstrap and view summary of model

fit<-sem(Model_C, MyVariablesStd, se="bootstrap", bootstrap=1000)

summary(fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


# Create model for Behaviourl Learning Engagement

Model_D <- 'BhvLrnEng ~ b*PU + d*PEOU + e*PBBCC
PU ~ a*PBBCC
PEOU ~ c*PBBCC
Bbc_Pu_BLe:=a*b
bbc_Peou_BLe:=c*d
Total:=a*b + c*d + e'


# bootstrap and view summary of model

fit<-sem(Model_D, MyVariablesStd, se="bootstrap", bootstrap=1000)

summary(fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
