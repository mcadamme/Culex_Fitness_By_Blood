#Blood Meal Weight Experiment
#June 21, 2018
#M. K. Cuadera and M. L. Fritz

#packages required: ggplot2, plyr, lme4
library(ggplot2); library(plyr); library(lme4); library(MASS); library(lmtest)

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

setwd("~/Desktop/Mervin-Culex_Fitness_By_Blood")#Mervin uses this
#setwd("~/Desktop/Culex_Fitness_By_Blood")#Megan uses this

Exp_3_Data <- read.csv(file="./data/Exp3_CSV_Blood_Meal_Weight_Data.csv", header = T)
head(Exp_3_Data)
tail(Exp_3_Data)

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

###blood imbibed exploration
# Histograms of Populations In Terms of Blood Imbibed As A Whole ----------
hist(Exp_3_Data_Outlier_Removed$Blood_Imbibed, main = "Blood Imbibed Across Populations", xlab = "Amount Imbibed (mg)", breaks = 12, xlim = c(0, 5))
hist(CAL_1_Bovine$Blood_Imbibed, main = "CAl 1 Imbibed Bovine Blood Distribution", xlab = "Amount Imbibed (mg)", breaks = seq(0,5.5, by =0.5), ylim = c(0,20))#adding more break, keep consistent, xaxt
hist(CAL_1_Goose$Blood_Imbibed, main = "CAL 1 Imbibed Goose Blood Distribution", xlab = "Amount Imbibed (mg)", breaks = seq(0,5.5, by =0.5), ylim = c(0,20))
hist(Evanston_Bovine$Blood_Imbibed, main = "Evanston Imbibed Bovine Blood Distribution", xlab = "Amount Imbibed (mg)", breaks = seq(0,5.5, by =0.5), ylim = c(0,20))
hist(Evanston_Goose$Blood_Imbibed, main = "Evanston Imbibed Goose Blood Distribution", xlab = "Amount Imbibed (mg)", breaks = seq(0,5.5, by =0.5), ylim = c(0,20))

#Initial Weight vs. Amount of Blood Imbibed Scatterplots

initial_vs_imbibed_CAL1_main <-ggplot(CAL_1, aes(x=Blood_Imbibed, y=Initial_Weight)) + geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, level = 0.95, color = "grey") +
  labs(title = "Blood Imbibed versus Initial Weight for CAL1 Overall", x="Blood Imbibed (mg)", y = "Initial Weight (mg)")+
  theme_classic()
print(initial_vs_imbibed_CAL1_main)

initial_vs_imbibed_Evanston_main <-ggplot(Evanston, aes(x=Blood_Imbibed, y=Initial_Weight)) + geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, level = 0.95, color = "grey") +
  labs(title = "Blood Imbibed versus Initial Weight for Evanston Overall", x="Blood Imbibed (mg)", y = "Initial Weight (mg)")+
  theme_classic()
print(initial_vs_imbibed_Evanston_main)


###Exploration of factors influencing num Eggs Produced

#Eggs produced by blood imbibed
imbibed_vs_eggs_scatter <- ggplot(With_eggs, aes(x=Blood_Imbibed, y=Eggs_Produced, color = Blood_Type, shape = Blood_Type)) + geom_point() +
  geom_smooth(method = lm, fullrange = TRUE, se=FALSE) +
  labs(title = "Egg Production versus Blood Imbibed Overall", x="Blood Imbibed (mg)", y = "Eggs Produced", color = "Blood_Type")+
  theme_classic()

print(imbibed_vs_eggs_scatter)

#CAL1 - broken out by blood type
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

#Evanston - broken out by blood type
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

#Eggs produced by initial weight
initial_vs_eggs_scatter <- ggplot(With_eggs, aes(x=Initial_Weight, y=Eggs_Produced)) + geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, level = 0.95, color = "grey") +
  labs(title = "Egg Production versus Initial Weight Overall", x="Initial Weight (mg)", y = "Eggs Produced")+
  theme_classic()

print(initial_vs_eggs_scatter)

#Broken down by strain.
initial_vs_eggs_scatter_CAL1_main <-ggplot(CAL_1, aes(x=Initial_Weight, y=Eggs_Produced)) + geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, level = 0.95, color = "grey") +
  labs(title = "Egg Production versus Initial Weight for CAL1 Overall", x="Initial Weight (mg)", y = "Eggs Produced")+
  theme_classic()
print(initial_vs_eggs_scatter_CAL1_main)

initial_vs_eggs_scatter_Evanston_main <-ggplot(Evanston, aes(x=Initial_Weight, y=Eggs_Produced)) + geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, level = 0.95, color = "grey") +
  labs(title = "Egg Production versus Initial Weight for Evanston Overall", x="Initial Weight (mg)", y = "Eggs Produced")+
  theme_classic()
print(initial_vs_eggs_scatter_Evanston_main)



# Full Model Exploration -------------------------------------------------------------
model_Full1_Gau <- glm(Blood_Imbibed ~ 1 + Strain*Blood_Type*Initial_Weight, data = Exp_3_Data_Outlier_Removed)
summary(model_Full1_Gau)
qqnorm(resid(model_Full1_Gau))
qqline(resid(model_Full1_Gau))

model_Full1_Gam <- glm(Blood_Imbibed ~ 1 + Strain*Blood_Type*Initial_Weight, data = Exp_3_Data_Outlier_Removed, family = Gamma)
summary(model_Full1_Gam)
qqnorm(resid(model_Full1_Gam))
qqline(resid(model_Full1_Gam))

AIC(model_Full1_Gam, model_Full1_Gau)
BIC(model_Full1_Gam, model_Full1_Gau)

#gamma distribution looks the best.


#Model Reduction using gamma dist for Statistical Analysis --------------------------------------------------------

model_red1_Gam <- glm(Blood_Imbibed ~ 1 + Strain*Blood_Type+Initial_Weight, data = Exp_3_Data_Outlier_Removed, family = Gamma)
summary(model_red1_Gam)
lrtest(model_red1_Gam, model_Full1_Gam) #no diff between models when initial weight is removed from interaction, so not important

model_red2_Gam <- glm(Blood_Imbibed ~ 1 + Strain*Blood_Type, data = Exp_3_Data_Outlier_Removed, family = Gamma)
summary(model_red2_Gam)
lrtest(model_red2_Gam, model_red1_Gam) #no diff between models with initial weight removed altogether, so not important

model_red3_Gam <- glm(Blood_Imbibed ~ 1 + Strain + Blood_Type, data = Exp_3_Data_Outlier_Removed, family = Gamma)
summary(model_red3_Gam)
lrtest(model_red3_Gam, model_red2_Gam)

model_red4_Gam <- glm(Blood_Imbibed ~ 1 + Blood_Type, data = Exp_3_Data_Outlier_Removed, family = Gamma)
summary(model_red4_Gam)
lrtest(model_red4_Gam, model_red3_Gam) #strain is marginally significant

model_red5_Gam <- glm(Blood_Imbibed ~ 1 + Strain, data = Exp_3_Data_Outlier_Removed, family = Gamma)
summary(model_red5_Gam)
lrtest(model_red3_Gam,model_red5_Gam) #blood type is marginally significant


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


#####Initial model selection for egg production analysis

model_Full2_Gau <- glm(Eggs_Produced ~ 1 + Strain*Blood_Type*Blood_Imbibed, data = With_eggs)
summary(model_Full2_Gau)
qqnorm(resid(model_Full2_Gau))
qqline(resid(model_Full2_Gau))

model_Full2_Gam <- glm(Eggs_Produced ~ 1 + Strain*Blood_Type*Blood_Imbibed, data = With_eggs, family = Gamma(link = log))
summary(model_Full2_Gam)
qqnorm(resid(model_Full2_Gam))
qqline(resid(model_Full2_Gam))

model_Full2_Pois <- glm(Eggs_Produced ~ 1 + Strain*Blood_Type*Blood_Imbibed, data = With_eggs, family = poisson)
summary(model_Full2_Pois)
qqnorm(resid(model_Full2_Pois))
qqline(resid(model_Full2_Pois))

model_Full2_nbi <- glm.nb(Eggs_Produced ~ 1 + Strain*Blood_Type*Blood_Imbibed, data = With_eggs)
summary(model_Full2_nbi)
qqnorm(resid(model_Full2_nbi))
qqline(resid(model_Full2_nbi))

AIC(model_Full2_Gam, model_Full2_Gau, model_Full2_Pois, model_Full2_nbi)
BIC(model_Full2_Gam, model_Full2_Gau, model_Full2_Pois, model_Full2_nbi)

#model_Full2_nbi looks best of these according to ICs, no overdispersion.  Going to use that one.

##Model Reduction
nbi_red2 <- glm.nb(Eggs_Produced ~ 1 + Strain+Blood_Type*Blood_Imbibed, data = With_eggs)
summary(nbi_red2)
anova(model_Full2_nbi, nbi_red2, test = "Chisq")#strain does not significantly influence as interaction.

nbi_red3 <- glm.nb(Eggs_Produced ~ 1 + Blood_Type*Blood_Imbibed, data = With_eggs)
summary(nbi_red3)
anova(nbi_red2, nbi_red3, test = "Chisq")#strain significantly influences num eggs

nbi_red4 <- glm.nb(Eggs_Produced~ 1 + Strain+Blood_Type+Blood_Imbibed, data = With_eggs)
summary(nbi_red4)
anova(nbi_red2, nbi_red4, test = "Chisq")#Not a significant interaction

exp(coef(nbi_red4))#getting backtransformed model coefficients.

nbi_red5 <- glm.nb(Eggs_Produced~ 1 + Strain+Blood_Imbibed, data = With_eggs)
summary(nbi_red5)
anova(nbi_red4, nbi_red5, test= "Chisq") #blood type matters

nbi_red6 <- glm.nb(Eggs_Produced~ 1 + Strain+Blood_Type, data = With_eggs)
summary(nbi_red6)
anova(nbi_red4, nbi_red6, test = "Chisq") #Amt blood imbibed matters too.

#Figure 4 - Eggs_produced_by blood meal mass
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

#Supplementary Figure - eggs produced by initial weight

png(filename = "Supp_Fig_EggsProduced_by_InitialWeight.png", units = "px", height = 900, width = 900)
initial_vs_eggs_scatter_CAL1_bovine <-ggplot(CAL1_With_eggs_bovine, aes(x=Initial_Weight, y=Eggs_Produced)) + geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, level = 0.95, color = "grey") +
  labs(title = "CAL1 Bovine", x="Initial Weight (mg)", y = "Eggs Produced")+
  theme_classic()

initial_vs_eggs_scatter_CAL1_goose <-ggplot(CAL1_With_eggs_goose, aes(x=Initial_Weight, y=Eggs_Produced)) + geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, level = 0.95, color = "grey") +
  labs(title = "CAL1 Goose", x="Initial Weight (mg)", y = "Eggs Produced")+
  theme_classic()

initial_vs_eggs_scatter_Evanston_bovine <- ggplot(Evanston_With_eggs_bovine, aes(x=Initial_Weight, y=Eggs_Produced)) + geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, level = 0.95, color = "grey") +
  labs(title = "Evanston Bovine", x="Initial Weight (mg)", y = "Eggs Produced")+
  theme_classic()

initial_vs_eggs_scatter_Evanston_goose <- ggplot(Evanston_With_eggs_goose, aes(x=Initial_Weight, y=Eggs_Produced)) + geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, level = 0.95, color = "grey") +
  labs(title = "Evanston Goose", x="Initial Weight (mg)", y = "Eggs Produced")+
  theme_classic()

multiplot(initial_vs_eggs_scatter_CAL1_bovine,initial_vs_eggs_scatter_CAL1_goose,initial_vs_eggs_scatter_Evanston_bovine,initial_vs_eggs_scatter_Evanston_goose, cols=2)

dev.off()

#Supplementary Figure - Blood imbibed by initial weight

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


# Calculating the correlations between num eggs produced and amt blood imbibed -------------------------------------------------------

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
