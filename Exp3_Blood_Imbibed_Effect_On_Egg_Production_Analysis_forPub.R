#Blood Meal Weight Experiment
#June 21, 2018
#M. K. Cuadera and M. L. Fritz

#packages required: ggplot2, plyr, lme4
library(ggplot2); library(plyr); library(lme4)

#required functions

# Summary Function --------------------------------------------------------
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE),
      N = length(x[[col]]))}
  
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

# Multiplot function ------------------------------------------------------

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

# Set Up and Working Directories and Libraries-----------------------------------------

#setwd("~/Desktop/Mervin-Culex_Fitness_By_Blood")#Mervin uses this
setwd("~/Desktop/Culex_Fitness_By_Blood")#Megan uses this

Exp_3_Data <- read.csv(file="./data/Exp3_CSV_Blood_Meal_Weight_Data.csv", header = T)
head(Exp_3_Data)

#Noticed an outlier for blood imbibed on Evanston (5 mg!), removed it. Suspected human error.
Exp_3_Data_Outlier_Removed <- subset(Exp_3_Data, Blood_Imbibed < 4)

CAL_1 <- subset(Exp_3_Data_Outlier_Removed, Strain == "CAL1")
CAL_1_Goose <- subset(CAL_1, Blood_Type == "Goose")
CAL_1_Bovine <- subset(CAL_1, Blood_Type == "Bovine")

Evanston <- subset(Exp_3_Data_Outlier_Removed,Strain == "Evanston")
Evanston_Bovine <- subset(Evanston, Blood_Type == "Bovine")
Evanston_Goose <- subset(Evanston, Blood_Type == "Goose")

#subsetting the dataset to include only those females that produced eggs
With_eggs <- subset(Exp_3_Data_Outlier_Removed, Eggs_Produced > 0)

CAL1_With_eggs <- subset(With_eggs, Strain == "CAL1")
CAL1_With_eggs_bovine <- subset(CAL1_With_eggs, Blood_Type == "Bovine")
CAL1_With_eggs_goose <- subset(CAL1_With_eggs, Blood_Type == "Goose")

Evanston_With_eggs <- subset(With_eggs, Strain == "Evanston")
Evanston_With_eggs_bovine <- subset(Evanston_With_eggs, Blood_Type == "Bovine")
Evanston_With_eggs_goose <- subset(Evanston_With_eggs, Blood_Type == "Goose")


# Data Summary ------------------------------------------------------------
Exp_3_Data_2 <- data_summary(Exp_3_Data_Outlier_Removed, varname = "Blood_Imbibed", groupnames = c("Strain", "Blood_Type"))
Exp_3_Data_2$se <- Exp_3_Data_2$sd / sqrt(Exp_3_Data_2$N)
head(Exp_3_Data_2)
Exp_3_Data_3 <- data_summary(Exp_3_Data_Outlier_Removed, varname = "Eggs_Produced", groupnames = c("Strain", "Blood_Type"))
Exp_3_Data_3$se <- Exp_3_Data_3$sd / sqrt(Exp_3_Data_3$N)
head(Exp_3_Data_3)
Exp_3_Data_4 <- data_summary(With_eggs, varname = "Eggs_Produced", groupnames = c("Strain", "Blood_Type"))
Exp_3_Data_4$se <- Exp_3_Data_4$sd / sqrt(Exp_3_Data_4$N)
head(Exp_3_Data_4)


#Data Exploration --------------------------------------------

#Blood Imbibed vs. Eggs Produced Scatterplots
imbibed_vs_eggs_scatter <- ggplot(With_eggs, aes(x=Blood_Imbibed, y=Eggs_Produced, color = Blood_Type, shape = Blood_Type)) + geom_point() +
  geom_smooth(method = lm, fullrange = TRUE, se=FALSE) +
  labs(title = "Egg Production versus Blood Imbibed Overall", x="Blood Imbibed (mg)", y = "Eggs Produced", color = "Blood_Type")+
  theme_classic()
print(imbibed_vs_eggs_scatter)



imbibed_vs_eggs_scatter_CAL1_bovine <-ggplot(CAL_1_Bovine, aes(x=Blood_Imbibed, y=Eggs_Produced)) + geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, level = 0.95, color = "grey") +
  labs(title = "Egg Production versus Blood Imbibed for CAL1 Bovine", x="Blood Imbibed (mg)", y = "Eggs Produced")+
  theme_classic()
print(imbibed_vs_eggs_scatter_CAL1_bovine)

imbibed_vs_eggs_scatter_CAL1_goose <-ggplot(CAL_1_Goose, aes(x=Blood_Imbibed, y=Eggs_Produced)) + geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, level = 0.95, color = "grey") +
  labs(title = "Egg Production versus Blood Imbibed for CAL1 Goose", x="Blood Imbibed (mg)", y = "Eggs Produced")+
  theme_classic()
print(imbibed_vs_eggs_scatter_CAL1_goose)

imbibed_vs_eggs_scatter_Evanston_main <-ggplot(Evanston, aes(x=Blood_Imbibed, y=Eggs_Produced)) + geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, level = 0.95, color = "grey") +
  labs(title = "Egg Production versus Blood Imbibed for Evanston Overall", x="Blood Imbibed (mg)", y = "Eggs Produced")+
  theme_classic()
print(imbibed_vs_eggs_scatter_Evanston_main)

imbibed_vs_eggs_scatter_Evanston_bovine <-ggplot(Evanston_Bovine, aes(x=Blood_Imbibed, y=Eggs_Produced)) + geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, level = 0.95, color = "grey") +
  labs(title = "Egg Production versus Blood Imbibed for Evanston Bovine", x="Blood Imbibed (mg)", y = "Eggs Produced")+
  theme_classic()
print(imbibed_vs_eggs_scatter_Evanston_bovine)

imbibed_vs_eggs_scatter_Evanston_goose <-ggplot(Evanston_Goose, aes(x=Blood_Imbibed, y=Eggs_Produced)) + geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, level = 0.95, color = "grey") +
  labs(title = "Egg Production versus Blood Imbibed for Evanston Goose", x="Blood Imbibed (mg)", y = "Eggs Produced")+
  theme_classic()
print(imbibed_vs_eggs_scatter_Evanston_goose)

#Initial Weight vs. Eggs Produced Scatterplots
initial_vs_eggs_scatter <- ggplot(Exp_3_Data, aes(x=Initial_Weight, y=Eggs_Produced)) + geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, level = 0.95, color = "grey") +
  labs(title = "Egg Production versus Initial Weight Overall", x="Initial Weight (mg)", y = "Eggs Produced")+
  theme_classic()
print(initial_vs_eggs_scatter)

initial_vs_eggs_scatter_CAL1_main <-ggplot(CAL_1, aes(x=Initial_Weight, y=Eggs_Produced)) + geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, level = 0.95, color = "grey") +
  labs(title = "Egg Production versus Initial Weight for CAL1 Overall", x="Initial Weight (mg)", y = "Eggs Produced")+
  theme_classic()
print(initial_vs_eggs_scatter_CAL1_main)

initial_vs_eggs_scatter_CAL1_bovine <-ggplot(CAL_1_Bovine, aes(x=Initial_Weight, y=Eggs_Produced)) + geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, level = 0.95, color = "grey") +
  labs(title = "Egg Production versus Initial Weight for CAL1 Bovine", x="Initial Weight (mg)", y = "Eggs Produced")+
  theme_classic()
print(initial_vs_eggs_scatter_CAL1_bovine)

initial_vs_eggs_scatter_CAL1_goose <-ggplot(CAL_1_Goose, aes(x=Initial_Weight, y=Eggs_Produced)) + geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, level = 0.95, color = "grey") +
  labs(title = "Egg Production versus Initial Weight for CAL1 Goose", x="Initial Weight (mg)", y = "Eggs Produced")+
  theme_classic()
print(initial_vs_eggs_scatter_CAL1_goose)

initial_vs_eggs_scatter_Evanston_main <-ggplot(Evanston_outlier_removed, aes(x=Initial_Weight, y=Eggs_Produced)) + geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, level = 0.95, color = "grey") +
  labs(title = "Egg Production versus Initial Weight for Evanston Overall", x="Initial Weight (mg)", y = "Eggs Produced")+
  theme_classic()
print(initial_vs_eggs_scatter_Evanston_main)

initial_vs_eggs_scatter_Evanston_bovine <- ggplot(Evanston_Bovine, aes(x=Initial_Weight, y=Eggs_Produced)) + geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, level = 0.95, color = "grey") +
  labs(title = "Egg Production versus Initial Weight for Evanston Bovine", x="Initial Weight (mg)", y = "Eggs Produced")+
  theme_classic()
print(initial_vs_eggs_scatter_Evanston_bovine)

initial_vs_eggs_scatter_Evanston_goose <- ggplot(Evanston_Goose_Removed_Outlier, aes(x=Initial_Weight, y=Eggs_Produced)) + geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, level = 0.95, color = "grey") +
  labs(title = "Egg Production versus Initial Weight for Evanston Goose", x="Initial Weight (mg)", y = "Eggs Produced")+
  theme_classic()
print(initial_vs_eggs_scatter_Evanston_goose)

#Initial Weight vs. Amount of Blood Imbibed Scatterplots
initial_vs_imbibed_main <- ggplot(Exp_3_Data, aes(x=Blood_Imbibed, y=Initial_Weight)) + geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, level = 0.95, color = "grey") +
  labs(title = "Blood Imbibed versus Initial Weight Overall", x="Blood Imbibed (mg)", y = "Initial Weight (mg)")+
  theme_classic()
print(initial_vs_imbibed_main)

initial_vs_imbibed_CAL1_main <-ggplot(CAL_1, aes(x=Blood_Imbibed, y=Initial_Weight)) + geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, level = 0.95, color = "grey") +
  labs(title = "Blood Imbibed versus Initial Weight for CAL1 Overall", x="Blood Imbibed (mg)", y = "Initial Weight (mg)")+
  theme_classic()
print(initial_vs_imbibed_CAL1_main)

initial_vs_imbibed_CAL1_bovine <-ggplot(CAL_1_Bovine, aes(x=Blood_Imbibed, y=Initial_Weight)) + geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, level = 0.95, color = "grey") +
  labs(title = "Blood Imbibed versus Initial Weight for CAL1 Bovine", x="Blood Imbibed (mg)", y = "Initial Weight (mg)")+
  theme_classic()
print(initial_vs_imbibed_CAL1_bovine)

initial_vs_imbibed_CAL1_goose <-ggplot(CAL_1_Goose, aes(x=Blood_Imbibed, y=Initial_Weight)) + geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, level = 0.95, color = "grey") +
  labs(title = "Blood Imbibed versus Initial Weight Overall", x="Blood Imbibed (mg)", y = "Initial Weight (mg)")+
  theme_classic()
print(initial_vs_imbibed_CAL1_goose)

initial_vs_imbibed_Evanston_main <-ggplot(Evanston_outlier_removed, aes(x=Blood_Imbibed, y=Initial_Weight)) + geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, level = 0.95, color = "grey") +
  labs(title = "Blood Imbibed versus Initial Weight for Evanston Overall", x="Blood Imbibed (mg)", y = "Initial Weight (mg)")+
  theme_classic()
print(initial_vs_imbibed_Evanston_main)

initial_vs_imbibed_Evanston_bovine <-ggplot(Evanston_Bovine, aes(x=Blood_Imbibed, y=Initial_Weight)) + geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, level = 0.95, color = "grey") +
  labs(title = "Blood Imbibed versus Initial Weight for Evanston Bovine", x="Blood Imbibed (mg)", y = "Initial Weight (mg)")+
  theme_classic()
print(initial_vs_imbibed_Evanston_bovine)

initial_vs_imbibed_Evanston_goose <-ggplot(Evanston_Goose, aes(x=Blood_Imbibed, y=Initial_Weight)) + geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, level = 0.95, color = "grey") +
  labs(title = "Blood Imbibed versus Initial Weight for Evanston Goose", x="Blood Imbibed (mg)", y = "Initial Weight (mg)")+
  theme_classic()
print(initial_vs_imbibed_CAL1_goose)


# Histograms of Populations In Terms of Blood Imbibed As A Whole ----------
hist(Exp_3_Data$Blood_Imbibed, main = "Blood Imbibed Across Populations", xlab = "Amount Imbibed (mg)", breaks = 12, xlim = c(0, 5))
hist( CAL_1_Bovine$Blood_Imbibed, main = "CAl 1 Imbibed Bovine Blood Distribution", xlab = "Amount Imbibed (mg)", breaks = seq(0,5.5, by =0.5), ylim = c(0,20))#adding more break, keep consistent, xaxt
hist( CAL_1_Goose$Blood_Imbibed, main = "CAL 1 Imbibed Goose Blood Distribution", xlab = "Amount Imbibed (mg)", breaks = seq(0,5.5, by =0.5), ylim = c(0,20))
hist( Evanston_Bovine$Blood_Imbibed, main = "Evanston Imbibed Bovine Blood Distribution", xlab = "Amount Imbibed (mg)", breaks = seq(0,5.5, by =0.5), ylim = c(0,20))
hist( Evanston_Goose_Removed_Outlier$Blood_Imbibed, main = "Evanston Imbibed Goose Blood Distribution", xlab = "Amount Imbibed (mg)", breaks = seq(0,5.5, by =0.5), ylim = c(0,20))

# Full Models -------------------------------------------------------------
model_Full1_Gau <- lmer(Blood_Imbibed ~ 1 + Strain*Blood_Type + (1|Replicate), data = Exp_3_Data_Outlier_Removed)
summary(model_Full1_Gau)

model_Full1_TransGau <- lmer(Blood_Imbibed ~ 1 + Strain*Blood_Type + (1|Replicate), data = Exp_3_Data_Outlier_Removed)
summary(model_Full1_TransGau)

model_Full1_Gam <- glmer(Blood_Imbibed ~ 1 + Strain*Blood_Type + (1|Replicate), data = Exp_3_Data_Outlier_Removed, family = Gamma)
summary(model_Full1_Gam)

model_Full1_LogNorm <- glmer(Blood_Imbibed ~ 1 + Strain*Blood_Type + (1|Replicate), data = Exp_3_Data_Outlier_Removed, family = gaussian(link = "log"))
summary(model_Full1_LogNorm)

model_Full2_LogNorm <- lmer(log(Blood_Imbibed) ~ 1 + Strain*Blood_Type + (1|Replicate), data = Exp_3_Data_Outlier_Removed)
summary(model_Full2_LogNorm)
plot(model_Full2_LogNorm)
qqnorm(resid(model_Full2_LogNorm))
qqline(resid(model_Full2_LogNorm))
model_Full3_LogNorm <- lmer(log(Blood_Imbibed) ~ 1 + Initial_Weight*Blood_Type + (1|Replicate), data = Exp_3_Data_Outlier_Removed)
summary(model_Full3_LogNorm)

model_Full4_LogNorm <- glm(log(Blood_Imbibed) ~ 1 + Strain*Blood_Type + (1|Replicate), data = Exp_3_Data_Outlier_Removed)
summary(model_Full4_LogNorm)

AIC(model_Full1_Gam, model_Full1_TransGau, model_Full1_LogNorm, model_Full2_LogNorm, model_Full3_LogNorm, model_Full4_LogNorm)
BIC(model_Full1_Gam, model_Full1_TransGau, model_Full1_LogNorm, model_Full2_LogNorm, model_Full3_LogNorm, model_Full4_LogNorm)
BIC(model_Full1_Gam, model_Full1_TransGau)
plot(model_Full4_LogNorm)

model_full1 <- lmer(Blood_Imbibed ~ 1 + Strain*Blood_Type + Initial_Weight + (1|Replicate), data = Exp_3_Data_Outlier_Removed)
summary(model_full1)

model_full2 <- lmer(Blood_Imbibed ~ 1 + Strain*Blood_Type + (1|Replicate), data = Exp_3_Data_Outlier_Removed)
summary(model_full2)
anova(model_full2)
qqnorm(model_full2)

model_full3 <- lmer(Blood_Imbibed ~ 1 +Strain*Initial_Weight + (1|Replicate), data = Exp_3_Data_Outlier_Removed)
summary(model_full3)

model_full4 <- lmer (Blood_Imbibed ~ 1 + Initial_Weight*Blood_Type + (1|Replicate), data = Exp_3_Data_Outlier_Removed)
summary(model_full4)

AIC(model_full1, model_full2, model_full3, model_full4, model_Full4_LogNorm) #model_full2; UPDATE: model_Full4_LogNorm is the best
BIC(model_full1, model_full2, model_full3, model_full4, model_Full4_LogNorm) #again model_full2 best fit; model_FUll4_LogNorm is the best



#Figure 3 for Pub - Amt blood by population and blood type
png(filename = "Figure_3_AmtBlood_AvgEggs.png", units = "px", height =  800, width = 500)

p1 <- ggplot(Exp_3_Data_2, aes(x=Strain, y=Blood_Imbibed, fill=Blood_Type)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Blood_Imbibed-se, ymax=Blood_Imbibed+se), width=.2,
                position=position_dodge(.9)) + labs(title="A", x="Strain", y = "Average Blood Imbibed (mg) + SE", fill = "Blood Type")+
  theme_classic() +
  theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14), plot.title = element_text(face="bold", size = 16)) +
  scale_fill_manual(values=c('#999999','#f2f3f4'))


p2 <- ggplot(Exp_3_Data_3, aes(x=Strain, y=Eggs_Produced, fill=Blood_Type)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Eggs_Produced-se, ymax=Eggs_Produced+se), width=.2,
                position=position_dodge(.9)) + labs(title="B", x="Strain", y = "Average Egg Production + SE", fill = "Blood Type")+
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14), plot.title = element_text(face="bold", size = 16)) +
  scale_fill_manual(values=c('#999999','#f2f3f4'))

multiplot(p1,p2, cols = 1)

dev.off()


#Figure 4 - blood meal mass by blood imbibed
imbibed_vs_eggs_scatter_CAL1_main <- ggplot(CAL1_With_eggs, aes(x=Blood_Imbibed, y=Eggs_Produced, color = Blood_Type, shape = Blood_Type)) + geom_point() +
  geom_smooth(method = lm, fullrange = TRUE, se=FALSE) +
  labs(title = "A", x="Blood Imbibed (mg)", y = "Eggs Produced", color = "Blood_Type")+
  theme_classic()+ scale_fill_manual(values=c('#cbe432','#2e2fe3')+
                                        theme(axis.text=element_text(size=12),
                                              axis.title=element_text(size=14), plot.title = element_text(face="bold", size = 16))) 

imbibed_vs_eggs_scatter_Evanston_main <- ggplot(Evanston_With_eggs, aes(x=Blood_Imbibed, y=Eggs_Produced, color = Blood_Type, shape = Blood_Type)) + geom_point() +
  geom_smooth(method = lm, fullrange = TRUE, se=FALSE) +
  labs(title = "B", x="Blood Imbibed (mg)", y = "Eggs Produced", color = "Blood_Type")+
  theme_classic()+scale_fill_manual(values=c('#fcba12','#343009') +
                                       theme(axis.text=element_text(size=12),
                                             axis.title=element_text(size=14), plot.title = element_text(face="bold", size = 16)))

png(filename = 'Figure_4_EggsProduced_by_BloodmealMass.png', units="in", width=8, height=5, res=300)
multiplot(imbibed_vs_eggs_scatter_CAL1_main, imbibed_vs_eggs_scatter_Evanston_main)
dev.off() 


# Model Reductions --------------------------------------------------------
model_reduced1 <- glm(log(Blood_Imbibed) ~ 1 + Strain + Blood_Type + (1|Replicate), data = Exp_3_Data)
summary(model_reduced1)
anova(model_Full4_LogNorm, model_reduced1, test = "Chisq") #eliminate interaction term; interaction term is important

model_reduced2 <- glm(log(Blood_Imbibed) ~ 1 + Strain + (1|Replicate), data = Exp_3_Data)
summary(model_reduced2)
anova(model_Full4_LogNorm, model_reduced2, test = "Chisq") #Blood_Type is important

model_reduced3 <- glm(log(Blood_Imbibed) ~ 1 + Blood_Type + (1|Replicate), data = Exp_3_Data)
summary(model_reduced3)
anova(model_Full4_LogNorm, model_reduced3, test = "Chisq") #Strain is important

plot(model_reduced3)
#new model is still model_Full4_LogNorm

# Correlation Tests -------------------------------------------------------

#Correlation Between Eggs Produced and Blood Imbibed
cor.test(With_eggs$Eggs_Produced, With_eggs$Blood_Imbibed, method = "pearson")

cor.test(CAL1_With_eggs$Eggs_Produced, CAL1_With_eggs$Blood_Imbibed, method = "pearson")
cor.test(CAL1_With_eggs_bovine$Eggs_Produced, CAL1_With_eggs_bovine$Blood_Imbibed, method = "pearson")
cor.test(CAL1_With_eggs_goose$Eggs_Produced, CAL1_With_eggs_goose$Blood_Imbibed, method = "pearson")

cor.test(Evanston$Eggs_Produced, Evanston$Blood_Imbibed, method = "pearson")
cor.test(Evanston_With_eggs_bovine$Eggs_Produced, Evanston_With_eggs_bovine$Blood_Imbibed, method = "pearson")
cor.test(Evanston_With_eggs_goose$Eggs_Produced, Evanston_With_eggs_goose$Blood_Imbibed, method = "pearson")


#Correlation Between Initial Weight and Eggs Produced
cor.test(With_eggs$Initial_Weight, With_eggs$Eggs_Produced, method = "pearson")

cor.test(CAL1_With_eggs$Initial_Weight, CAL1_With_eggs$Eggs_Produced, method = "pearson")
cor.test(CAL1_With_eggs_bovine$Initial_Weight, CAL1_With_eggs_bovine$Eggs_Produced, method = "pearson")
cor.test(CAL1_With_eggs_goose$Initial_Weight, CAL1_With_eggs_goose$Eggs_Produced, method = "pearson")

cor.test(Evanston_With_eggs$Initial_Weight, Evanston_With_eggs$Eggs_Produced, method = "pearson")
cor.test(Evanston_With_eggs_bovine$Initial_Weight, Evanston_With_eggs_bovine$Eggs_Produced, method = "pearson")
cor.test(Evanston_With_eggs_goose$Initial_Weight, Evanston_With_eggs_goose$Eggs_Produced, method = "pearson")

#Correlation Between Initial Weight and Blood Imbibed
cor.test(With_eggs$Initial_Weight, With_eggs$Blood_Imbibed, method = "pearson")

cor.test(CAL1_With_eggs$Initial_Weight, CAL1_With_eggs$Blood_Imbibed, method = "pearson")
cor.test(CAL1_With_eggs_bovine$Initial_Weight, CAL1_With_eggs_bovine$Blood_Imbibed, method = "pearson")
cor.test(CAL1_With_eggs_goose$Initial_Weight, CAL1_With_eggs_goose$Blood_Imbibed, method = "pearson")

cor.test(Evanston_With_eggs$Initial_Weight, Evanston_With_eggs$Blood_Imbibed, method = "pearson")
cor.test(Evanston_With_eggs_bovine$Initial_Weight, Evanston_With_eggs_bovine$Blood_Imbibed, method = "pearson")
cor.test(Evanston_With_eggs_goose$Initial_Weight, Evanston_With_eggs_goose$Blood_Imbibed, method = "pearson")
# T-Tests -----------------------------------------------------------------
t.test(Blood_Imbibed~Blood_Type, data = Evanston)
t.test(Blood_Imbibed~Blood_Type, data = CAL_1)

t.test(data= Evanston, Initial_Weight~Blood_Type)
t.test(data = CAL_1, Initial_Weight~Blood_Type)

# ANOVA -------------------------------------------------------------------
#analyzing whether variances are similar, with a nonparametric assumption (Fligner-Killen Test)
fligner.test(Blood_Imbibed ~ Strain, data = Exp_3_Data)
#p-value = 0.02, variance significantly different (ANOVA may not work)

#this is used for Figure 15
anov_model_1 <- aov(Blood_Imbibed~Blood_Type*Strain, data = Exp_3_Data)
summary(anov_model_1) 
plot(anov_model_1)

anov_model_1 <- aov(Blood_Imbibed~Blood_Type*Strain, data = Exp_3_Data_Outlier_Removed)
summary(anov_model_1)
#this is used for Figure 16
anov_model_2 <- aov(Eggs_Produced~Blood_Type*Strain, data = With_eggs)
summary(anov_model_2)
plot(anov_model_2)

# 07/16/2018: remove titles of all graphs
#07162018: look at the outliers; for poster, report model results with outliers included but verbally acknowledge how to handle outliers
#07162018: include DF