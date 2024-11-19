library(readxl)
library(tidyverse)
library(olsrr)
library(moments)
library(performance)
library(see)
library(patchwork)
library(qqplotr)
library(sjPlot)
library(sjmisc)

#Upload dataset. 
df <- read_excel("C:/Users/katet/OneDrive/Desktop/Masterarbeit/Psychological risk indicators of disordered.xlsx")
dim(df)

table(na.exclude(df$`How old are you (years)?`))
prop.table(table(na.exclude(df$`How old are you (years)?`))) * 100

#0=female, 1=male
table(na.exclude(df$`What is your gender?`))
prop.table(table(na.exclude(df$`What is your gender?`))) * 100


#0=lean, 1=non-lean
table(na.exclude(df$`LEAN.1: Is your sport a 'lean' sport?`))
prop.table(table(na.exclude(df$`LEAN.1: Is your sport a 'lean' sport?`))) * 100


#0 = off-season, 1= in-season
#table(na.exclude(df$"SEASON.1: If your sport has seasons, at what point in the season are you currently?"))
#prop.table(table(na.exclude(df$"SEASON.1: If your sport has seasons, at what point in the season are you currently?")))*100

names(df)[names(df) == "LEVEL.1: What is the highest level in which you have competed?"] <- "competitive_level"

# Replace values 2 and 3 with 2, leaving 1 as is
df$competitive_level <- replace(df$competitive_level, df$competitive_level %in% c(2, 3), 2)
unique(df$competitive_level)

#1 = non-elite, 2 and 3 = elite
table(na.exclude(df$"competitive_level"))
prop.table(table(na.exclude(df$"competitive_level")))*100

#1 = 1-3years, 2= 4-8 years, 3= 9-15 years, 4= 16+ years 
#table(na.exclude(df$`SPORTYRS.1: How many years have you actively competed in your sport?`))
#prop.table(table(na.exclude(df$`SPORTYRS.1: How many years have you actively competed in your sport?`))) * 100

#1= 10-15h/w, 2= 16-25h/w, 3= 26+ h/w
table(na.exclude(df$`HRSWK.1: How many hours per week do you train for your sport?`))
prop.table(table(na.exclude(df$`HRSWK.1: How many hours per week do you train for your sport?`))) * 100

vars <- c("How old are you (years)?",
          "What is your gender?",
          "LEAN.1: Is your sport a 'lean' sport?",
          "SPORTYRS.1: How many years have you actively competed in your sport?",                                                                                         
          "SEASON.1: If your sport has seasons, at what point in the season are you currently?",                                                                          
          "HRSWK.1: How many hours per week do you train for your sport?",                                                                                                
          "competitive_level",
          colnames(df)[19:23],
          colnames(df)[29:37],
          colnames(df)[73:78],
          colnames(df)[154:158],
          colnames(df)[163:184],
          colnames(df)[210:231])

df1 <- df[,vars]


#Social media pressures
pressures_mean <- c(
  item1 = c ("SMED5.1: Social media is an important source of information about fashion and 'being attractive'"),
  item2 = c("SMED6.1: I compare my appearance to the appearance people on social media"),
  item3 = c("SMED7.1: I've felt pressure from social media to be thin"),
  item4 = c("SMED8.1: I wish I looked like the influencers on social media"),
  item5 = c("SMED9.1: I compare my life to the the life portrayed by people on social media"))

#calculate the mean score for social media pressures
df1$pressures_mean <- apply(df1[,pressures_mean], MARGIN=1, FUN=mean)

#Check for missing values 
any(is.na(df1[,pressures_mean]))


#Perfectionism:
perf_items <- c(
  item1 = "PERF1.1: Only outstanding performance is good enough in my family",                 
  item2 = "PERF2.1: As a child, I tried hard to avoid disappointing parents and teachers",                 
  item3 = "PERF3.1: I hate being less than best at things",                
  item4 = "PERF4.1: My parents have expected excellence of me",                 
  item5 = "PERF5.1: I feel that I must do things perfectly or not do them at all",
  item6 = "PERF6.1: I have extremely high goals"
)

#Calculate mean score for Perfectionism and compare with original paper
df1$perf_mean <- apply(df1[,perf_items], MARGIN=1, FUN=mean)

all(df$"PERF.1: MEAN SCALE SCORE - Perfectionism" == df1$perf_mean)

# Eating disorder symptoms 
  #Subscale 1: restraint
ed_subscales <- c(
  subscale1_item1 = "EDEQR1.1: Have you been deliberately trying to limit the amount of food you eat to influence your shape or weight",                 
  subscale1_item2 = "EDEQR2.1: Have you gone for long periods of time (8 hours or more) without eating anything in order to lose weight",                 
  subscale1_item3 = "EDEQR3.1: Have you tried to avoid eating any foods which you like in order to influence your shape or weight",                
  subscale1_item4 = "EDEQR4.1: Have you tried to follow definite rules regarding your eating in order to influence your shape or weight",                 
  subscale1_item5 = "EDEQR5.1: Have you wanted your stomach to be empty?",
  
  #Subscale 2: eating concerns
  
  subscale2_item1 = "EDEQC1.1: Has thinking about food or its calorie content made it much more difficult to concentrate on things you are interested in",
  subscale2_item2 = "EDEQC2.1: Have you been afraid of losing control over eating",                                                                       
  subscale2_item3 = "EDEQC3.1: Have you eaten in secret",                                                                                                
  subscale2_item4 = "EDEQC4.1: How concerned have you been about other people seeing you eat",                                                            
  subscale2_item5 = "EDEQC5.1: In what proportion of times that you have eaten have you felt guilty because of the effect on your shape or weight",
  
  #Subscale 3: shape concerns (frequency) 
  
  subscale3_item1 = "EDEQS1.1: Have you definitely wanted your stomach to be flat",                                                                      
  subscale3_item2 = "EDEQS2.1: Has thinking about shape or weight made it more difficult to concentrate on things you are interested in",                 
  subscale3_item3 = "EDEQS3.1: Have you had a definite fear that you might gain weight or become fat",                                                    
  subscale3_item4 = "EDEQS4.1: Have you felt fat", 
  
  #Subscale 4: shape concerns (severity)
  
  subscale4_item5 = "EDEQS5.1: Has your shape influenced how you think about (judge) yourself as a person",                                               
  subscale4_item6 = "EDEQS6.1: How dissatisfied have you felt about your shape",                                                                        
  subscale4_item7 = "EDEQS7.1: How uncomfortable have you felt seeing your body; for example, in the mirror, in shop window",                             
  subscale4_item8 = "EDEQS8.1: How uncomfortable have you felt about others seeing your body",
  
  #Subscale 5: body weight
  
  subscale5_item1 = "EDEQW1.1: Have you had a strong desire to lose weight",                                                                             
  subscale5_item2 = "EDEQW2.1: Has your weight influenced how you think about (judge) yourself as a person",                                            
  subscale5_item3 = "EDEQW3.1: How much would it upset you if you had to weigh yourself once a week for the next few weeks",                              
  subscale5_item4 = "EDEQW4.1: How dissatisfied have you felt about your weight"
)

# Calculate the mean for each subscale (MM)
edeqMeanFun <- function(edeqData=NULL) {
  return(apply(edeqData, MARGIN=1, function(x) {
    if(length(which(is.na(x))) >= length(x)/2) {
      NA
    } else {
      mean(x, na.rm=TRUE)
    }
  }))
}

ed_subscaleNames <- paste0("subscale", 1:5)
for(i in 1:5) {
  idxSub_i <- grep(pattern=ed_subscaleNames[i], x=names(ed_subscales))
  ed_i <- ed_subscales[idxSub_i]
  df1[,paste0("ed_sub", i, "Mean")] <- edeqMeanFun(edeqData=df1[,ed_i])
}

edMeans <- paste0("ed_sub", 1:5, "Mean") 
df1[,edMeans]

#Compare with the original results for each subscale
all(df1[,"EDEQR.1: MEAN SCALE SCORE - Eating disorder, restraint"] == df1[,"ed_sub1Mean"])
all(df1[,"EDEQC.1: MEAN SCALE SCORE - Eating disorder, eating concerns"] == df1[,"ed_sub2Mean"])
all(df1[,"EDEQS.1: MEAN SCALE SCORE - Eating disorder, body shape"] == df1[,"ed_sub3Mean"])
all(df1[,"EDEQW.1: MEAN SCALE SCORE - Eating disorder, body weight"] == df1[,"ed_sub5Mean"])

df1$ed_totalMean <- apply(df1[,edMeans[-4]], MARGIN=1, mean)

#Compare with the original results for the global scale
all(df1$`EDEQ.1: MEAN SCALE SCORE - Eating disorder, overall scale score` == df1$ed_totalMean)


#Determine the control variables for the analysis
df1$adjust_level <- as.factor(df1$"competitive_level")
df1$adjust_hours_week <- as.factor(df1$`HRSWK.1: How many hours per week do you train for your sport?`)

#Hypothesis 1a
Hypothesis1a <- lm(ed_totalMean ~ perf_mean, data = df1)
summary (Hypothesis1a)


#Hypothesis 1a with control variables 
Hypothesis1a_adjusted <- lm(ed_totalMean ~ perf_mean + adjust_level + adjust_hours_week, data = df1)
summary (Hypothesis1a_adjusted)


#Hypothesis 1b
#Firts, rename variable 
names(df1)[names(df1) == "LEAN.1: Is your sport a 'lean' sport?"] <- "athlete_type"

Hypothesis1b <- lm(ed_totalMean ~ perf_mean * athlete_type, data = df1)
summary (Hypothesis1b)


#Hypothesis 1b with control variables 
Hypothesis1b_adjusted <- lm(ed_totalMean ~ perf_mean * athlete_type + adjust_level + adjust_hours_week, data = df1)
summary (Hypothesis1b_adjusted)


#Visualisation of H1b
#MM
plotH1b <- sjPlot::plot_model(model=Hypothesis1b, type="pred", terms=c("perf_mean", "athlete_type"))

plotH1b.nicer <- plotH1b +
  xlab(label="Perfectionism") +
  ylab(label="Eating disorder symptoms") +
  labs(title="Interaction by athlete type") +
  theme(
    panel.background = element_blank(),
    axis.text.x=element_text(size=14),
    axis.title.x=element_text(size=14),
    axis.text.y=element_text(size=14),
    axis.title.y = element_text(size=14),
    panel.border = element_rect(color="grey", fill=NA),
    legend.title = element_text(size=14),
    legend.text=element_text(size=14),
    legend.position = "top"
  ) +
  scale_color_manual(
    values = c("0" = "deeppink", "1" = "deepskyblue"),
    labels = c("0" = "Lean Athletes", "1" = "Non-lean Athletes") 
  ) +
  scale_fill_manual(
    values = c("0" = "deeppink", "1" = "deepskyblue"),
    labels = c("0" = "Lean Athletes", "1" = "Non-lean Athletes")
  )

plotH1b.nicer

#Hypothesis 1c
names(df1)[names(df1) == "What is your gender?"] <- "gender"

#Create a subset of only lean athletes, filtering out non-lean athletes 
lean_athletes_data <- subset(df1, athlete_type == "0")

#Gender 
table(na.exclude(lean_athletes_data$gender))
prop.table(table(na.exclude(lean_athletes_data$gender))) * 100

Hypothesis1c <- lm(ed_totalMean ~ perf_mean * gender, data = lean_athletes_data)
summary(Hypothesis1c)


#Hypothesis 1c with controlling variables 
Hypothesis1c_adjusted <- lm(ed_totalMean ~ perf_mean * gender + adjust_level + adjust_hours_week, data = lean_athletes_data)
summary (Hypothesis1c_adjusted)


#Visualisation of H1c
#MM
plotH1c <- sjPlot::plot_model(model=Hypothesis1c, type="pred", terms=c("perf_mean", "gender"))

plotH1c.nicer <- plotH1c +
  xlab(label="Perfectionism") +
  ylab(label="Eating disorder symptoms") +
  labs(title="Interaction by gender") +
  theme(
    panel.background = element_blank(),
    axis.text.x=element_text(size=14),
    axis.title.x=element_text(size=14),
    axis.text.y=element_text(size=14),
    axis.title.y = element_text(size=14),
    panel.border = element_rect(color="grey", fill=NA),
    legend.title = element_text(size=14),
    legend.text=element_text(size=14),
    legend.position = "top"
  ) +
  scale_color_manual(
    values = c("0" = "deeppink", "1" = "deepskyblue"),
    labels = c("0" = "Female", "1" = "Male") 
  ) +
  scale_fill_manual(
    values = c("0" = "deeppink", "1" = "deepskyblue"),
    labels = c("0" = "Female", "1" = "Male")
  )

plotH1c.nicer


#Assumptions tests 


# model1 would be your linear regression model (MM)
assumptionsModel <- plot(performance::check_model(Hypothesis1a))
ols_test_breusch_pagan(Hypothesis1a)
shapiro.test(Hypothesis1a$residuals)
lmtest::raintest(Hypothesis1a)
lmtest::dwtest(Hypothesis1a)


assumptionsMode2 <- plot(performance::check_model(Hypothesis1b))
ols_test_breusch_pagan(Hypothesis1b)
shapiro.test(Hypothesis1b$residuals)
lmtest::raintest(Hypothesis1b)
lmtest::dwtest(Hypothesis1b)

assumptionsMode3 <- plot(performance::check_model(Hypothesis1c))
ols_test_breusch_pagan(Hypothesis1c)
shapiro.test(Hypothesis1b$residualc)
lmtest::raintest(Hypothesis1c)
lmtest::dwtest(Hypothesis1c)


#Hypothesis 2a
Hypothesis2a <- lm(ed_totalMean ~ pressures_mean, data = df1)
summary (Hypothesis2a)

#Hypothesis 2a with control variables 
Hypothesis2a_adjusted <- lm(ed_totalMean ~ pressures_mean + adjust_level + adjust_hours_week, data = df1)
summary (Hypothesis2a_adjusted)

#Hypothesis2b
Hypothesis2b <- lm(ed_totalMean ~ pressures_mean * athlete_type, data = df1)
summary (Hypothesis2b)

#Hypothesis 2b with control variables 
Hypothesis2b_adjusted <- lm(ed_totalMean ~ pressures_mean * athlete_type + adjust_level + adjust_hours_week, data = df1)
summary (Hypothesis2b_adjusted)

#Visualisation of H2b
#MM
plotH2b <- sjPlot::plot_model(model=Hypothesis2b, type="pred", terms=c("pressures_mean", "athlete_type"))

plotH2b.nicer <- plotH1c +
  xlab(label="Social media pressures") +
  ylab(label="Eating disorder symptoms") +
  labs(title="Interaction by athlete type") +
  theme(
    panel.background = element_blank(),
    axis.text.x=element_text(size=14),
    axis.title.x=element_text(size=14),
    axis.text.y=element_text(size=14),
    axis.title.y = element_text(size=14),
    panel.border = element_rect(color="grey", fill=NA),
    legend.title = element_text(size=14),
    legend.text=element_text(size=14),
    legend.position = "top"
  ) +
  scale_color_manual(
    values = c("0" = "deeppink", "1" = "deepskyblue"),
    labels = c("0" = "Lean Athletes", "1" = "Non-lean Athletes") 
  ) +
  scale_fill_manual(
    values = c("0" = "deeppink", "1" = "deepskyblue"),
    labels = c("0" = "Lean Athletes", "1" = "Non-lean Athletes")
  )

plotH2b.nicer

#Hypothesis 2c
Hypothesis2c <- lm(ed_totalMean ~ pressures_mean * gender, data = lean_athletes_data)
summary(Hypothesis2c)


#Hypothesis 2c with controlling variables 
Hypothesis2c_adjusted <- lm(ed_totalMean ~ pressures_mean * gender + adjust_level + adjust_hours_week, data = lean_athletes_data)
summary (Hypothesis2c_adjusted)

#Visualisation of H2c
#MM
plotH2c <- sjPlot::plot_model(model=Hypothesis2c, type="pred", terms=c("pressures_mean", "gender"))

plotH2c.nicer <- plotH2c +
  xlab(label="Social Media Pressure") +
  ylab(label="Eating disorder symptoms") +
  labs(title="Interaction by gender") +
  theme(
    panel.background = element_blank(),
    axis.text.x=element_text(size=14),
    axis.title.x=element_text(size=14),
    axis.text.y=element_text(size=14),
    axis.title.y = element_text(size=14),
    panel.border = element_rect(color="grey", fill=NA),
    legend.title = element_text(size=14),
    legend.text=element_text(size=14),
    legend.position = "top"
  ) +
  scale_color_manual(
    values = c("0" = "deeppink", "1" = "deepskyblue"),
    labels = c("0" = "Female", "1" = "Male") 
  ) +
  scale_fill_manual(
    values = c("0" = "deeppink", "1" = "deepskyblue"),
    labels = c("0" = "Female", "1" = "Male")
  )

plotH2c.nicer



#Assumptions tests 


# model1 would be your linear regression model (MM)
assumptionsMode4 <- plot(performance::check_model(Hypothesis2a))
ols_test_breusch_pagan(Hypothesis2a)
shapiro.test(Hypothesis2a$residuals)
lmtest::raintest(Hypothesis2a)
lmtest::dwtest(Hypothesis2a)

assumptionsMode5 <- plot(performance::check_model(Hypothesis2b))
ols_test_breusch_pagan(Hypothesis2b)
shapiro.test(Hypothesis2b$residuals)
lmtest::raintest(Hypothesis2b)
lmtest::dwtest(Hypothesis2b)

assumptionsMode6 <- plot(performance::check_model(Hypothesis2c))
ols_test_breusch_pagan(Hypothesis2c)
shapiro.test(Hypothesis2c$residuals)
lmtest::raintest(Hypothesis2c)
lmtest::dwtest(Hypothesis2c)

