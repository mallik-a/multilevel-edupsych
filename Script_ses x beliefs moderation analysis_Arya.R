# load data 
beliefs_data <- read.csv("C:/Arya/UvA/Internship/BELIEFS DATA/BELIEFS DATA.csv", header = TRUE)
 
# remove irrelevant data 
dataclean <- beliefs_data[c(-1,-2),-c(1:5,7:17)]
# initial n = 156
# 10 under 16
# 2 no consent
# 8 for attention checks
# 2 RT 
# final n = 135

# attention checks 
dataatt <- dataclean[dataclean$att_check == 5 | dataclean$att_check.1== 1,] 
# nr rem = 8 

# remove row if no consent is given, or if age is >18
dataconsent<- dataatt[dataatt$Inf_con !=  
                        "Nee, ik doe niet mee" & dataatt$Inf_con_.16 != "Nee, ik doe niet mee"
                      & dataatt$Age !="19 jaar of ouder" & dataatt$Age !="11", ] 

# Compress ESSA questions
ESSAin <- c("ESSA_1", "ESSA_2", "ESSA_3", "ESSA_4", "ESSA_5", "ESSA_6", "ESSA_7",
                        "ESSA_8", "ESSA_9", "ESSA_10", "ESSA_11", "ESSA_12", "ESSA_13",
                        "ESSA_14", "ESSA_15", "ESSA_16")
dataconsent[ESSAin] <- lapply(dataconsent[ESSAin], function(x) as.numeric(as.character(x)))
dataconsent$ESSA <- rowMeans(dataconsent[ESSAin], na.rm = TRUE) # ESSA mean score is added to end of dataframe
dataconsent <- dataconsent[, !names(dataconsent) %in% ESSAin] # removes sub ESSA questions, leaves ESSA in dataframe
print(names(dataconsent)) # to check if ESSA has been added > yes!

# descriptive statistics
data_age <- dataconsent[,"Age"]
data_age <- as.numeric(data_age)
meanage <- mean(data_age)
sdage <- sd(data_age)

data_year<-dataconsent[,"Schoolyear"]
data_year <- as.numeric(data_year)
meanyear <- mean(data_year)
sdyear <- sd(data_year)

data_essa <- mydata[, "ESSA"]
data_essa <- as.numeric(data_essa)
meanessa <- mean(data_essa)
sdessa <- sd(data_essa)

# number and % of males and females
gender_counts <- table(mydata$Gender) 
gender_percentages <- prop.table(gender_counts) * 100 
gender_summary <- data.frame(
  Gender = names(gender_counts),
  Count = as.vector(gender_counts),
  Percentage = round(as.vector(gender_percentages), 2)
)

# data pre-processing: reaction time
data_rt <- dataconsent[,"Duration..in.seconds."] 
data_rt <- as.numeric(data_rt)
mean_rt <- mean(data_rt) # mean RT
sd_rt <- sd(data_rt) # SD RT
max_rt <- mean_rt + 3 * sd_rt
min_rt <- mean_rt - 3 * sd_rt
rt_toohigh <- data_rt >= max_rt
rt_toolow <- data_rt <= min_rt
# view RTs that are too high or too low 
print(rt_toohigh)
print(rt_toolow)
# remove rows with too high or too low RTs > 1 excluded!
dataconsent <- dataconsent[! (rt_toohigh | rt_toolow), ]

# extract relevant columns for my analysis
mydata <- dataconsent[,c("SES", 
                          "ABP_lev_1_eff", "ABP_lev_1_lu", "ABP_eff_ind", 
                          "ABP_eff_prox_f", "ABP_eff_prox_t", "ABP_eff_prox_p",
                          "ESSA", "class_code")] 

# remove participants under 16 from dataset as schools did not send parental consent
mydata <- mydata[-c(5, 12, 19, 21, 28, 46, 56, 57, 59, 60), ]
# n final = 134

# Create composite for effort beliefs because we have multiple items
# First, convert all effort items to numeric
mydata$ABP_lev_1_eff    <- as.numeric(as.character(mydata$ABP_lev_1_eff))
mydata$ABP_eff_ind      <- as.numeric(as.character(mydata$ABP_eff_ind))
mydata$ABP_eff_prox_f   <- as.numeric(as.character(mydata$ABP_eff_prox_f))
mydata$ABP_eff_prox_t   <- as.numeric(as.character(mydata$ABP_eff_prox_t))
mydata$ABP_eff_prox_p   <- as.numeric(as.character(mydata$ABP_eff_prox_p))
# composite effort beliefs score
mydata$effort_beliefs <- rowMeans(mydata[, c("ABP_lev_1_eff", "ABP_eff_ind", 
                                             "ABP_eff_prox_f", "ABP_eff_prox_t", 
                                             "ABP_eff_prox_p")], na.rm = TRUE)
# renaming "ABP_lev_1_lu" to "luck beliefs"
names(mydata)[names(mydata) == "ABP_lev_1_lu"] <- "luck_beliefs"

# descriptives for me
data_luck <- mydata[, "luck_beliefs"]
data_luck <- as.numeric(data_luck)
meanluck <- mean(data_luck)
sdluck <- sd(data_luck)

data_effort <- mydata[, "effort_beliefs"]
data_effort <- as.numeric(data_effort)
meaneffort <- mean(data_effort)
sdeffort <- sd(data_effort)

# assumption checks 
model <- lm(ESSA ~ ., data = mydata) # . uses all other variables as predictors

## Residual vs Fitted Plot: checks linearity & homoscedasticity
plot(model, which = 1)  
# interpretation: The residuals are fairly symmetrically spread above and below 
# the 0 line, without a clear curved pattern. Linearity assumption is reasonably 
# met. The relationship between your predictors and the outcome appears roughly 
# linear. The spread looks relatively even in the middle of the plot (around 
# fitted values of 3.0–3.8), but there's slightly more variability in the tails.
# this slight increase in variance is not alarming. 

## QQ-plot; normality of residuals
plot(model, which = 2)
# most residuals/data points fall along the normal dist except at the tails which show some 
# deviation. 
# Histogram of residuals
hist(residuals(model), breaks = 20, main = "Histogram of Residuals", xlab = "Residuals")

# Shapiro-Wilk test for normality 
shapiro.test(residuals(model))
# p value is 0.97, non-significant, suggesting normality of residuals

# full factor multilevel model 
library(lme4)   # For multilevel modelling
library(lmerTest)  # For p-values

# Step 1: Null model (to calculate intraclass correlation or ICC)
null_model <- lmer(ESSA ~ 1 + (1 | class_code), data = mydata)
summary(null_model)

# Compute ICC: Proportion of variance explained at the class level
icc <- as.numeric(VarCorr(null_model)$class_code[1]) /
  (as.numeric(VarCorr(null_model)$class_code[1])+ attr(VarCorr(null_model), "sc")^2)
print(paste("ICC:", round(icc, 3)))
# tells you how much of the total variance in your outcome variable 
# (academic pressure or ESSA) is due to clustering/grouping, in your case by class_code.
# "ICC: 0.103" here ICC is greater than 0, justifying the inclusion of a random 
# intercept for class_code in the model. This value indicates that ~10% of
# 10.3% of the variance in academic pressure is due to differences between classes 
# while the remaining ~90% is due to individual differences within classes.

# Step 2: Full model with random intercepts only (with only ABP_eff_ind)
full_model <- lmer(ESSA ~ SES * ABP_eff_ind + SES * luck_beliefs + 
                     (1 | class_code), data = mydata)
summary(full_model)

# Step 2.1: Full model with random intercepts only (with effort beliefs composite score)
full_model1 <- lmer(ESSA ~ SES * effort_beliefs + SES * luck_beliefs + 
                     (1 | class_code), data = mydata)
summary(full_model1)

# both models provide similar results. we use full_model1 in our analysis 

# reporting results 
install.packages("modelsummary")
library(modelsummary)
modelsummary(full_model1, stars = TRUE,
             title = "Multilevel Model Results for SES, Beliefs and Academic Pressure")

str(mydata$SES)
str(mydata$luck_beliefs)
str(mydata$effort_beliefs)
str(mydata$ABP_eff_ind)

print(mydata$SES)
mydata$SES <- as.numeric(mydata$SES)
ses_counts <- table(mydata$SES)
print(ses_counts)



# plotting the data 
install.packages("interactions")
library(interactions)
# SES × Effort Beliefs interaction
eff <- interact_plot(full_model1, pred = SES, modx = effort_beliefs,
              plot.points = FALSE, interval = TRUE,
              x.label = "SES", y.label = "Academic Pressure")


# SES × Luck Beliefs interaction
lu <- interact_plot(full_model1, pred = SES, modx = luck_beliefs,
              plot.points = FALSE, interval = TRUE,
              x.label = "SES", y.label = "Academic Pressure")

# combining both interaction plots
library(ggpubr)
ggarrange(eff, lu + rremove("x.text"), 
          labels = c("A", "B"),
          ncol = 2, nrow = 2)
