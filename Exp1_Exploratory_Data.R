#Experiment 1 Exploratory Data
#Subject: How Host Blood Affects Egg Production of Cx. pipiens mosquitoes
#Authors: Mervin Keith Cuadera and Megan L. Fritz
#Date: July 06, 2018

# Setting Up Data Sets ----------------------------------------------------
Exp_1_data <- read.csv(file = "~/Desktop/Mervin-Culex_Fitness_By_Blood/data/Exp1_Host_Blood_Fitness_Study_Replicates.csv",header = T)
head(Exp_1_data)
Exp_1_data$No_eggs_per_fem <- Exp_1_data$Total_Egg_Produced/Exp_1_data$No_Fed
Fed_only <- subset(Exp_1_data, No_Fed > 0)

str(Fed_only)
Fed_only_mammalian <- subset(Fed_only, Host_Type =="mammalian")
Fed_only_avian <- subset(Fed_only, Host_Type == "avian")
Fed_only_molestus <- subset(Fed_only, Form =="m")
Fed_only_pip <- subset(Fed_only, Form =="p")

Fed_only_mol_bovine <- subset(Fed_only_molestus, Treatment =="Bovine")
Fed_only_mol_rabbit <- subset(Fed_only_molestus, Treatment =="Rabbit")
Fed_only_mol_equine <- subset(Fed_only_molestus, Treatment =="Equine")
Fed_only_mol_goose <- subset(Fed_only_molestus, Treatment =="Goose")
Fed_only_mol_chicken <- subset(Fed_only_molestus, Treatment =="Chix")
Fed_only_mol_turkey <- subset(Fed_only_molestus, Treatment =="Turkey")

Fed_only_pip_bovine <- subset(Fed_only_pip, Treatment =="Bovine")
Fed_only_pip_rabbit <- subset(Fed_only_pip, Treatment =="Rabbit")
Fed_only_pip_equine <- subset(Fed_only_pip, Treatment =="Equine")
Fed_only_pip_goose <- subset(Fed_only_pip, Treatment =="Goose")
Fed_only_pip_chicken <- subset(Fed_only_pip, Treatment =="Chix")
Fed_only_pip_turkey <- subset(Fed_only_pip, Treatment =="Turkey")
# Function for summarizing data ------------------------------------------
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE),
      No_Replicates = length(x[[col]]))}
  
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

# Function for multiplotting ----------------------------------------------
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/ 
# Bootstrapping function --------------------------------------------------
boot.fn <- function(x, N=5000) {
  Int.1 <- replicate(N, mean(sample(x, size= length(x), replace=T)))
  Int.CI <- quantile(Int.1, probs=c(0.025,0.975))
  Int.CI
}
# Bootstrapping means -----------------------------------------------------
Boot_strap_CI <- with(Fed_only,tapply(No_eggs_per_fem,list(Strain, Treatment), boot.fn))

# Data Summaries ----------------------------------------------------------
#summarizing the data
Exp_1_Data_Summary <- data_summary(Fed_only, varname = "No_eggs_per_fem", groupnames = c("Strain", "Treatment"))
Exp_1_Data_Summary$se <- Exp_1_Data_Summary$sd / sqrt(Exp_1_Data_Summary$No_Replicates)
head(Exp_1_Data_2)

Exp_1_Data_Mammalian <- data_summary(Fed_only_mammalian, varname = "No_eggs_per_fem", groupnames = c("Strain", "Treatment"))
Exp_1_Data_Mammalian$se <- Exp_1_Data_Mammalian$sd / sqrt(Exp_1_Data_Mammalian$No_Replicates)
head(Exp_1_Data_Mammalian)

Exp_1_Data_Avian <- data_summary(Fed_only_avian, varname = "No_eggs_per_fem", groupnames = c("Strain", "Treatment"))
Exp_1_Data_Avian$se <- Exp_1_Data_Avian$sd / sqrt(Exp_1_Data_Avian$No_Replicates)
head(Exp_1_Data_Avian)

Exp_1_Data_Molestus <- data_summary(Fed_only_molestus, varname = "No_eggs_per_fem", groupnames = c("Strain", "Treatment"))
Exp_1_Data_Molestus$se <- Exp_1_Data_Molestus$sd / sqrt(Exp_1_Data_Molestus$No_Replicates)
head(Exp_1_Data_Molestus)

Exp_1_Data_Pip <- data_summary(Fed_only_pip, varname = "No_eggs_per_fem", groupnames = c("Strain", "Treatment"))
Exp_1_Data_Pip$se <- Exp_1_Data_Pip$sd / sqrt(Exp_1_Data_Pip$No_Replicates)
head(Exp_1_Data_Pip)

Exp_1_Data_Host_Type <-data_summary(Fed_only, varname = "No_eggs_per_fem", groupnames = c("Strain", "Host_Type"))
Exp_1_Data_Host_Type$se <- Exp_1_Data_Host_Type$sd / sqrt(Exp_1_Data_Host_Type$No_Replicates)
head(Exp_1_Data_Host_Type)
# Packages Required -------------------------------------------------------
#activating packages required
library (lme4)
library(ggplot2)
library(car)
library(dplyr)
library(knitr)
# Bar Plot Exploratory Data -----------------------------------------------
Strain_Box_Mammalian <- ggplot(Exp_1_Data_Mammalian, aes(x=Strain, y=No_eggs_per_fem, fill=Treatment)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=No_eggs_per_fem-se, ymax=No_eggs_per_fem+se), width=.2,
                position=position_dodge(.9)) + labs(title="Effect of Mammalian Blood Type on Eggs Produced", x="Strain", y = "Average Eggs", fill = "Treatment")+
  theme_classic()+theme(text=element_text(family="Calibri"))
print(Strain_Box_Mammalian)

Strain_Box_Avian <- ggplot(Exp_1_Data_Avian, aes(x=Strain, y=No_eggs_per_fem, fill=Treatment)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=No_eggs_per_fem-se, ymax=No_eggs_per_fem+se), width=.2,
                position=position_dodge(.9)) + labs(title="Effect of Avian Blood Type on Eggs Produced", x="Strain", y = "Average Eggs", fill = "Treatment")+
  theme_classic()+scale_fill_brewer(palette="Spectral")
print(Strain_Box_Avian)

Strain_Box_Mol <- ggplot(Exp_1_Data_Molestus, aes(x=Strain, y=No_eggs_per_fem, fill=Treatment)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=No_eggs_per_fem-se, ymax=No_eggs_per_fem+se), width=.2,
                position=position_dodge(.9)) + labs(title="Effect of Blood Type on Eggs Produced", x="Strain", y = "Average Eggs", fill = "Treatment")+
  theme_classic()
print(Strain_Box_Mol)

multiplot(Strain_Box_Mammalian, Strain_Box_Avian)

Strain_Box_Pip <- ggplot(Exp_1_Data_Pip, aes(x=Strain, y=No_eggs_per_fem, fill=Treatment)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=No_eggs_per_fem-se, ymax=No_eggs_per_fem+se), width=.2,
                position=position_dodge(.9)) + labs(title="Effect of Blood Type on Eggs Produced", x="Strain", y = "Average Eggs", fill = "Treatment")+
  theme_classic()
print(Strain_Box_Pip)

plot(No_eggs_per_fem ~ Strain, data = Fed_only, ylab = "Average Eggs per Female", xlab = "Strain", main = "Average Eggs per Host Type" )
plot(No_eggs_per_fem ~ Host_Type, data = Fed_only, ylab = "Average Eggs per Female", xlab = "Host Type", main = "Average Eggs per Host Type" )
plot(No_eggs_per_fem ~ Treatment, data = Fed_only, ylab = "Average Eggs per Female", xlab = "Treatment", main = "Average Eggs per Treatment")


# Bar Plot and Box Plot ---------------------------------------------------
whole_data_box <- ggplot(Fed_only) + geom_boxplot(aes(x=Strain, y= No_eggs_per_fem, color = Treatment), notch = F)+
  theme_classic()+ labs(title="Effect of Blood Type on Eggs Produced", x="Strain", y = "Average Eggs", fill = "Treatment") #set notch to F since notches went outside hinges.
print(whole_data_box)

whole_data_bar <-ggplot(Exp_1_Data_2, aes(x=Strain, y=No_eggs_per_fem, fill=Treatment)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=No_eggs_per_fem-se, ymax=No_eggs_per_fem+se), width=.2,
                position=position_dodge(.9)) + labs(x="Strain", y = "Average Eggs", fill = "Treatment")+
  theme_classic()+ scale_color_brewer(palette = "Spectral")+theme(text=element_text(family="Calibri"))
print(whole_data_bar)#used for fig. 10

Host_Type_bar <-ggplot(Exp_1_Data_Host_Type, aes(x=Strain, y=No_eggs_per_fem, fill=Host_Type)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=No_eggs_per_fem-se, ymax=No_eggs_per_fem+se), width=.2,
                position=position_dodge(.9)) + labs(x="Strain", y = "Average Eggs", fill = "Host Type")+
  theme_classic()+ scale_color_brewer(palette = "Spectral")+theme(text=element_text(family="Calibri"))
print(Host_Type_bar) #used for fig. 9

Host_Type_bar_with_CI <-ggplot(Exp_1_Data_Host_Type, aes(x=Strain, y=No_eggs_per_fem, fill=Host_Type)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  theme_classic()+ scale_color_brewer(palette = "Spectral")+theme(text=element_text(family="Calibri"))
print(Host_Type_bar) #used for fig. 9
# Testing Normality -------------------------------------------------------
# Testing Normality Assumptions (UPDATE: these might be useless due to the nature of replicates in the data. i.e. not enough replicates, but replicates themselves are collected from large amount of sample)

#Shapiro-Wilk Normality Test by Strain; CAL1 population not normally distributed
cat("Normality p-values by Factor Strain: ")
for ( i in unique(factor(Fed_only$Strain))){
  cat(shapiro.test(Fed_only[Fed_only$Strain==i, ]$No_eggs_per_fem)$p.value," ")
}

#Shapiro-Wilk Normality Test by Treatment; Turkey not normally distributed
cat("Normality p-values by Factor Treatment: ")
for (i in unique(factor(Fed_only$Treatment))){
  cat(shapiro.test(Fed_only[Fed_only$Treatment==i, ]$No_eggs_per_fem)$p.value," ")
}

#Variance Tests for Strain and Treatment Factors
bartlett.test(No_eggs_per_fem~Strain, data = Fed_only ) #variances per strain is not significantly different
bartlett.test(No_eggs_per_fem~Treatment, data = Fed_only ) #variances per treatment is significantly different


# Model Fitting and Reductions --------------------------------------------
#Exploring Full Models To Fit Data
model_Full1 <- lmer(No_eggs_per_fem ~ 1 + Strain*Treatment + (1|Replicate), data = Fed_only)
summary(model_Full1)

model_Full2 <- lmer(No_eggs_per_fem ~ 1 + Form*Treatment + (1|Replicate), data = Fed_only)
summary(model_Full2)

model_Full3 <- lmer(No_eggs_per_fem ~ 1 + Form*Host_Type + (1|Replicate), data = Fed_only)
summary(model_Full3)

model_Full4 <- lmer(No_eggs_per_fem ~ 1 + Strain*Host_Type + (1|Replicate), data = Fed_only)
summary(model_Full4)

Fed_only_pois <- Fed_only %>% mutate(No_eggs_per_fem = round(No_eggs_per_fem, 0))
model_Full5 <- glmer(No_eggs_per_fem ~ 1 + Strain*Treatment + (1|Replicate), data = Fed_only_pois, family = "poisson")
summary(model_Full5)

AIC(model_Full1, model_Full2, model_Full3, model_Full4, model_Full5) #model_Full1 is best fit
BIC(model_Full1, model_Full2, model_Full3, model_Full4, model_Full5) #again with model_Full1

#model_Full1 reduction
model_reduced1 <- lmer(No_eggs_per_fem ~ 1 + Strain + Treatment + (1|Replicate), data = Fed_only)
summary(model_reduced1)
anova(model_Full1, model_reduced1, test = "Chisq") #interaction is important

model_reduced2 <- lmer(No_eggs_per_fem ~ 1 + Strain + (1|Replicate), data = Fed_only)
summary(model_reduced2)
anova(model_reduced1, model_reduced2, test = "Chisq") #including treatment is important
# *** ANOVA
model_reduced3 <- lmer(No_eggs_per_fem ~ 1 + Treatment + (1|Replicate), data = Fed_only)
summary(model_reduced3)
anova(model_reduced1, model_reduced3, test = "Chisq") #strain is important

#So full model is still model_Full1; albeit a saturated model.
anova(model_Full1)
summary(model_Full1)
qqnorm(resid(model_Full1))
qqline(resid(model_Full1))
plot(model_Full1)

# Statistical Analyses ----------------------------------------------------
host_effects_anova_1 <- aov(No_eggs_per_fem ~ Host_Type*Strain, data = Exp_1_Data_Host_Type)
summary(host_effects_anova_1)
host_effects_anova_2 <- aov(No_eggs_per_fem ~ Strain + Host_Type, data = Exp_1_Data_Host_Type)
summary(host_effects_anova_2)
TukeyHSD(host_effects_anova_2, which = "Strain")

#ANOVA for each of the single treatments? Yeah
Bovine_Anova <- aov(No_eggs_per_fem ~ Strain, data = Fed_only_bovine)
summary(Bovine_Anova)
TukeyHSD(Bovine_Anova)
Rabbit_Anova <- aov(No_eggs_per_fem ~ Strain, data = Fed_only_rabbit)
summary(Rabbit_Anova)
Equine_Anova <- aov(No_eggs_per_fem ~ Strain, data = Fed_only_equine)
summary(Equine_Anova)
Goose_Anova <- aov(No_eggs_per_fem ~ Strain, data = Fed_only_goose)
summary(Goose_Anova)
Chicken_Anova <- aov(No_eggs_per_fem ~ Strain, data = Fed_only_chicken)
summary(Chicken_Anova)
Turkey_Anova <- aov(No_eggs_per_fem ~ Strain, data = Fed_only_turkey)
summary(Turkey_Anova)

#anova for just host type
Avian_Anova <- aov(No_eggs_per_fem ~ Strain, data = Fed_only_avian)
summary(Avian_Anova)
TukeyHSD(Avian_Anova)

Mammalian_Anova <- aov(No_eggs_per_fem ~ Strain, data = Fed_only_mammalian)
summary(Mammalian_Anova)
TukeyHSD(Mammalian_Anova)

Host_Anova <- aov(No_eggs_per_fem ~ Host_Type*Strain, data = Fed_only)
summary(Host_Anova)
TukeyHSD(Host_Anova) #used for fig. 9

#Then T-Test within each bioforms? Yeap 
t.test(No_eggs_per_fem~Strain, data = Fed_only_mol_bovine)
t.test(No_eggs_per_fem~Strain, data = Fed_only_mol_chicken)
t.test(No_eggs_per_fem~Strain, data = Fed_only_mol_equine)
t.test(No_eggs_per_fem~Strain, data = Fed_only_mol_goose)
t.test(No_eggs_per_fem~Strain, data = Fed_only_mol_rabbit)
t.test(No_eggs_per_fem~Strain, data = Fed_only_mol_turkey)

t.test(No_eggs_per_fem~Strain, data = Fed_only_pip_bovine)
t.test(No_eggs_per_fem~Strain, data = Fed_only_pip_chicken)
t.test(No_eggs_per_fem~Strain, data = Fed_only_pip_equine)
t.test(No_eggs_per_fem~Strain, data = Fed_only_pip_goose)
t.test(No_eggs_per_fem~Strain, data = Fed_only_pip_rabbit)
t.test(No_eggs_per_fem~Strain, data = Fed_only_pip_turkey)