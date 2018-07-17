#Blood Meal Weight Experiment
#June 21, 2018
#M. K. Cuadera and M. L. Fritz

#easier navigation by going to Edit -> Folding -> Collapse All (alt+control+o)
#packages required: ggplot2, plyr, lme4

# Notes -------------------------------------------------------------------
#07/06/2018: noticed an outlier for blood imbibed on Evanston (5 mg!), removed it. Suspected human error.
# Set Up and Working Directories and Libraries-----------------------------------------

Exp_3_Data <- read.csv(file="~/Desktop/CSV_Blood_Meal_Weight_Data.csv", header = T)
head(Exp_3_Data)
Exp_3_Data_Outlier_Removed <- subset(Exp_3_Data, Blood_Imbibed < 4)
With_eggs <- subset(Exp_3_Data, Eggs_Produced > 0)

CAL_1 <- subset(Exp_3_Data, Strain == "CAL1")
CAL_1_Goose <- subset(CAL_1, Blood_Type == "Goose")
CAL_1_Bovine <- subset(CAL_1, Blood_Type == "Bovine")

Evanston <- subset(Exp_3_Data,Strain == "Evanston")
Evanston_outlier_removed <- subset(Evanston, Blood_Imbibed < 4)
Evanston_Bovine <- subset(Evanston, Blood_Type == "Bovine")
Evanston_Goose <- subset(Evanston, Blood_Type == "Goose")
Evanston_Goose_Removed_Outlier <-subset(Evanston_Goose, Blood_Imbibed < 4)

CAL1_With_eggs <- subset(With_eggs, Strain == "CAL1")
CAL1_With_eggs_bovine <- subset(CAL1_With_eggs, Blood_Type == "Bovine")
CAL1_With_eggs_goose <- subset(CAL1_With_eggs, Blood_Type == "Goose")

Evanston_With_eggs <- subset(With_eggs, Strain == "Evanston")
Evanston_With_eggs_bovine <- subset(Evanston_With_eggs, Blood_Type == "Bovine")
Evanston_With_eggs_goose <- subset(Evanston_With_eggs, Blood_Type == "Goose")

library(ggplot2); library(plyr); library(lme4)

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


# Data Summary ------------------------------------------------------------
Exp_3_Data_2 <- data_summary(Exp_3_Data, varname = "Blood_Imbibed", groupnames = c("Strain", "Blood_Type"))
Exp_3_Data_2$se <- Exp_3_Data_2$sd / sqrt(Exp_3_Data_2$N)
head(Exp_3_Data_2)
Exp_3_Data_3 <- data_summary(Exp_3_Data, varname = "Eggs_Produced", groupnames = c("Strain", "Blood_Type"))
Exp_3_Data_3$se <- Exp_3_Data_3$sd / sqrt(Exp_3_Data_3$N)
head(Exp_3_Data_3)
Exp_3_Data_4 <- data_summary(With_eggs, varname = "Eggs_Produced", groupnames = c("Strain", "Blood_Type"))
Exp_3_Data_4$se <- Exp_3_Data_4$sd / sqrt(Exp_3_Data_4$N)
head(Exp_3_Data_4)

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
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/ 
# Main Experiment Barplot -------------------------------------------------
p<- ggplot(Exp_3_Data_2, aes(x=Strain, y=Blood_Imbibed, fill=Blood_Type)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Blood_Imbibed-se, ymax=Blood_Imbibed+se), width=.2,
                position=position_dodge(.9)) + labs(title="Effect of Blood Type on Amount Imbibed", x="Strain", y = "Average Blood Imbibed (mg)", fill = "Blood Type")+
  theme_classic() +
  scale_fill_manual(values=c('#999999','#f2f3f4'))
print(p)

p2<- ggplot(Exp_3_Data_3, aes(x=Strain, y=Eggs_Produced, fill=Blood_Type)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Eggs_Produced-se, ymax=Eggs_Produced+se), width=.2,
                position=position_dodge(.9)) + labs(title="Effect of Blood Type on Eggs Produced", x="Strain", y = "Average Egg Production", fill = "Blood Type")+
  theme_classic() +
  scale_fill_manual(values=c('#999999','#f2f3f4'))
print(p2)

# Main Experiment Boxplot -------------------------------------------------

#this is used for figure 15
box <- ggplot(With_eggs, aes(x=Strain, y= Blood_Imbibed, fill = Blood_Type))+ stat_boxplot(geom = "errorbar")+
  theme_classic()+ labs(title = "Average Blood Imbibed per Treatment", x = "Strain",
  y = "Average Blood Imbibed (mg)", fill = "Blood Type") + scale_fill_manual(values=c('#d61a46','#98ca32'))#set notch to F since notches went outside hinges.
box +geom_boxplot()

#this is used for figure 16
box_2 <-ggplot(With_eggs, aes(x=Strain, y= Eggs_Produced, fill = Blood_Type)) +  theme_classic()+ labs(title = "Average Number of Eggs per Treatment", x = "Strain", y = "Average Egg Count", fill = "Blood Type") + 
  scale_fill_manual(values = c('#fc600a','#1489b8'))#set notch to F since notches went outside hinges.
box_2+geom_boxplot()
# Experiment Boxplot with Jitter ------------------------------------------
box_n_jitter <- ggplot(Exp_3_Data, aes(x= Strain, y= Blood_Imbibed, color = Blood_Type)) + geom_boxplot() + geom_jitter(position = position_jitter(0.1))+
  theme_classic()+scale_fill_manual(values=c('#999999','#f2f3f4'))+ labs(title = "Amount of Blood Imbibed per Blood Type per Strain", x = "Strain", y = "Blood Imbibed", color = "Blood Type")

print(box_n_jitter) #unsure if helpful


# Scatterplot Data Exploration --------------------------------------------

#Blood Imbibed vs. Eggs Produced Scatterplots
imbibed_vs_eggs_scatter <- ggplot(With_eggs, aes(x=Blood_Imbibed, y=Eggs_Produced, color = Blood_Type, shape = Blood_Type)) + geom_point() +
  geom_smooth(method = lm, fullrange = TRUE, se=FALSE) +
  labs(title = "Egg Production versus Blood Imbibed Overall", x="Blood Imbibed (mg)", y = "Eggs Produced", color = "Blood_Type")+
  theme_classic()
print(imbibed_vs_eggs_scatter)

imbibed_vs_eggs_scatter_CAL1_main <- ggplot(CAL1_With_eggs, aes(x=Blood_Imbibed, y=Eggs_Produced, color = Blood_Type, shape = Blood_Type)) + geom_point() +
  geom_smooth(method = lm, fullrange = TRUE, se=FALSE) +
  labs(title = "Egg Production versus Blood Imbibed for CAL 1", x="Blood Imbibed (mg)", y = "Eggs Produced", color = "Blood_Type")+
  theme_classic()+ scale_color_manual(values=c('#cbe432','#2e2fe3'))
print(imbibed_vs_eggs_scatter_CAL1_main) #this is used for figure 17 or 18.

imbibed_vs_eggs_scatter_Evanston_main <- ggplot(Evanston_With_eggs, aes(x=Blood_Imbibed, y=Eggs_Produced, color = Blood_Type, shape = Blood_Type)) + geom_point() +
  geom_smooth(method = lm, fullrange = TRUE, se=FALSE) +
  labs(title = "Egg Production versus Blood Imbibed for Evanston", x="Blood Imbibed (mg)", y = "Eggs Produced", color = "Blood_Type")+
  theme_classic()+scale_color_manual(values=c('#fcba12','#343009'))
print(imbibed_vs_eggs_scatter_Evanston_main) #this is used for figure 19 or 20.

multiplot(imbibed_vs_eggs_scatter_CAL1_main, imbibed_vs_eggs_scatter_Evanston_main)

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
model_full1 <- lmer(Blood_Imbibed ~ 1 + Strain*Blood_Type + Initial_Weight + (1|Replicate), data = Exp_3_Data)
summary(model_full1)

model_full2 <- lmer(Blood_Imbibed ~ 1 + Strain*Blood_Type + (1|Replicate), data = Exp_3_Data)
summary(model_full2)
anova(model_full2)

model_full3 <- lmer(Blood_Imbibed ~ 1 +Strain*Initial_Weight + (1|Replicate), data = Exp_3_Data)
summary(model_full3)

model_full4 <- lmer (Blood_Imbibed ~ 1 + Initial_Weight*Blood_Type + (1|Replicate), data = Exp_3_Data)
summary(model_full4)

AIC(model_full1, model_full2, model_full3, model_full4) #model_full2 best fit 
BIC(model_full1, model_full2, model_full3, model_full4) #again model_full2 best fit
# Model Reductions --------------------------------------------------------
model_reduced1 <- lmer(Blood_Imbibed ~ 1 + Blood_Type + Strain + (1|Replicate), data = Exp_3_Data)
summary(model_reduced1)
anova(model_full2, model_reduced1, test = "Chisq") #eliminate interaction term; important

model_reduced2 <- lmer(Blood_Imbibed ~ 1 +Strain + (1|Replicate), data = Exp_3_Data)
summary(model_reduced2)
anova(model_full2, model_reduced2, test = "Chisq") #Blood_Type is important

model_reduced3 <- lmer(Blood_Imbibed ~ 1 + Blood_Type + (1|Replicate), data = Exp_3_Data)
summary(model_reduced3)
anova(model_full2, model_reduced3, test = "Chisq") #Strain is important
#best model is still model_Full2

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