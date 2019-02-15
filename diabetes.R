my_data <- read.csv("C:/Users/Trishala/Desktop/diabetes.csv")
summary(my_data)
#columns
#PregnanciesNumber of times pregnant
#GlucosePlasma glucose concentration a 2 hours in an oral glucose tolerance test
#BloodPressureDiastolic blood pressure (mm Hg)
#SkinThicknessTriceps skin fold thickness (mm)
#Insulin2-Hour serum insulin (mu U/ml)
#BMIBody mass index (weight in kg/(height in m)^2)
#DiabetesPedigreeFunctionDiabetes pedigree function
#AgeAge (years)
#OutcomeClass variable (0 or 1) 268 of 768 are 1, the others are 0

head(my_data)
structure(my_data)

library(Amelia) #This library is used to plot missmap 
library(ggplot2)
library(ggcorrplot)
library(GGally)
library(PerformanceAnalytics)


#Changing outcome from numerical to categorical varibale.
my_data$Outcome<- is.factor(my_data$Outome)
levels(my_data$Outcome) <- c("No","Yes")


#correlation plot
ggpairs(my_data, aes(color=Outcome, alpha=0.75), lower=list(continuous="smooth"))+ theme_bw()+
labs(title="Correlation Plot of Variance(diabetes)")+
theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))

# Correlation matrix
data(my_data)
corr <- round(cor(my_data), 1)
# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower",  
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram", 
           ggtheme=theme_bw)

#Plots a missingness map showing where missingness occurs in the dataset
missmap(my_data, main ="Missing values vs observed")
