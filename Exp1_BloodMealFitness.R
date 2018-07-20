#Analysis of Culex fitness by vertebrate blood host
#June 11, 2018
#M. Fritz and M.K. Cuadera

library(lme4)

#use setwd() to set your working directory
Exp1_data <- read.csv("~/Desktop/Mervin-Culex_Fitness_by_Blood/data/Exp1_Host_Blood_Fitness_Study_Replicates.csv", header = T) 

#adding column for number eggs per fed female
Exp1_data$No_eggs_per_Fem <- Exp1_data$Total_Egg_Produced/Exp1_data$No_Fed
Fed_only <- subset(Exp1_data, No_Fed > 0)
Fed_only$Trans_No_eggs_per_Fem <- Fed_only$No_eggs_per_Fem + 0.00001 #adding a small number to response so that gamma model works.

#Explore our data with plots
plot(No_eggs_per_Fem ~ Strain, data = Fed_only)#example - fix the outlier UPDATE: fixed
plot(No_eggs_per_Fem ~ Host_Type, data = Fed_only)
plot(No_eggs_per_Fem ~ Treatment, data = Fed_only)

attach(Fed_only)
barplot(tapply(No_eggs_per_Fem, list(Strain, Treatment),mean), main = "Effect of Blood Type Per Treatment", xlab = "Blood Type" , ylab = "Average Eggs per Female", beside = T, ylim = c(0,120), col = NULL)

#Full model comparison

model_Full1_Gau <- lmer(No_eggs_per_Fem ~ 1 + Strain*Treatment + (1|Replicate), data = Fed_only)
summary(model_Full1_Gau)

model_Full1_TransGau <- lmer(Trans_No_eggs_per_Fem ~ 1 + Strain*Treatment + (1|Replicate), data = Fed_only)
summary(model_Full1_TransGau)

model_Full1_Gam <- glmer(Trans_No_eggs_per_Fem ~ 1 + Strain*Treatment + (1|Replicate), data = Fed_only, family = Gamma)
summary(model_Full1_Gam)

model_Full1_LogNorm <- glmer(Trans_No_eggs_per_Fem ~ 1 + Strain*Treatment + (1|Replicate), data = Fed_only, family = gaussian(link = "log"))
summary(model_Full1_LogNorm)

model_Full2_LogNorm <- lmer(log(Trans_No_eggs_per_Fem) ~ 1 + Strain*Treatment + (1|Replicate), data = Fed_only)
summary(model_Full2_LogNorm)

model_Full3_LogNorm <- glm(log(Trans_No_eggs_per_Fem) ~ 1 + Strain*Treatment + (1|Replicate), data = Fed_only)
summary(model_Full3_LogNorm)

AIC(model_Full1_Gam, model_Full1_TransGau, model_Full1_LogNorm, model_Full2_LogNorm, model_Full3_LogNorm)
BIC(model_Full1_Gam, model_Full1_TransGau, model_Full1_LogNorm, model_Full2_LogNorm, model_Full3_LogNorm)
BIC(model_Full1_Gam, model_Full1_TransGau)

model_Full1 <- lmer(No_eggs_per_Fem ~ 1 + Strain*Treatment + (1|Replicate), data = Fed_only)
summary(model_Full1)

model_Full2 <- lmer(No_eggs_per_Fem ~ 1 + Form*Treatment + (1|Replicate), data = Fed_only)
summary(model_Full2)

model_Full3 <- lmer(No_eggs_per_Fem ~ 1 + Form*Host_Type + (1|Replicate), data = Fed_only)
summary(model_Full3)

model_Full4 <- lmer(No_eggs_per_Fem ~ 1 + Strain*Host_Type + (1|Replicate), data = Fed_only)
summary(model_Full4)

AIC(model_Full1, model_Full2, model_Full3, model_Full4, model_Full2_LogNorm) #model_Full2_LogNorm is best fit
BIC(model_Full1, model_Full2, model_Full3, model_Full4, model_Full2_LogNorm) #again with model_Full2_LogNorm
qqnorm(resid(model_Full2_LogNorm))

#Model Reductions
model_reduced1 <- lmer(log(Trans_No_eggs_per_fem) ~ 1 + Strain+Treatment + (1|Replicate), data = Fed_only)
summary(model_reduced1)
anova(model_Full2_LogNorm, model_reduced1, test = "Chisq") #interaction is important

model_reduced2 <- lmer(log(Trans_No_eggs_per_fem) ~ 1 + Strain + (1|Replicate), data = Fed_only)
summary(model_reduced2)
anova(model_Full2_LogNorm, model_reduced2, test = "Chisq") #including treatment is important

model_reduced3 <- lmer(log(Trans_No_eggs_per_fem) ~ 1 +Treatment + (1|Replicate), data = Fed_only)
summary(model_reduced3)
anova(model_Full2_LogNorm, model_reduced3, test = "Chisq") #strain is important

#So full model is still model_Full2_LogNorm; albeit a saturated model.
anova(model_Full2_LogNorm)
summary(model_Full2_LogNorm)
qqnorm(resid(model_Full2_LogNorm))
qqline(resid(model_Full2_LogNorm))
plot(model_Full2_LogNorm)

# host type instead of treatment ------------------------------------------
model_reduced_4 <- lmer(No_eggs_per_Fem ~ 1 + Strain + Host_Type + (1|Replicate), data = Fed_only)
summary(model_reduced_4)
anova(model_Full4, model_reduced_4, test = "F")

#07162018: put on 1st figure, significant strain by host type interaction and the P-value. 