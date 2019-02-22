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
library(gridExtra)

chooseCRANmirror(graphics=FALSE, ind=1)


#t-test - #It is used to determine whether there is a significant difference between the means of two groups


attach(my_data)

with(data=my_data,t.test(Pregnancies[Outcome==1],Pregnancies[Outcome==0],var.equal=TRUE)) 
with(data=my_data,t.test(Glucose[Outcome==1],Glucose[Outcome==0],var.equal=TRUE))
with(data=my_data,t.test(Insulin[Outcome==1],Insulin[Outcome==0],var.equal=TRUE))
with(data=my_data,t.test(SkinThickness[Outcome==1],SkinThickness[Outcome==0],var.equal=TRUE))
with(data=my_data,t.test(BMI[Outcome==1],BMI[Outcome==0],var.equal=TRUE))
with(data=my_data,t.test(DiabetesPedigreeFunction[Outcome==0],DiabetesPedigreeFunction[Outcome==1],var.equal=TRUE))
with(data=my_data,t.test(Age[Outcome==1],Age[Outcome==0],var.equal=TRUE))
with(data=my_data,t.test(BloodPressure[Outcome==1],BloodPressure[Outcome==0],var.equal=TRUE))

#Hotelling

install.packages("Hotelling")
library(Hotelling)

t2testsparr <- hotelling.test(Pregnancies + Glucose + Insulin + SkinThickness + BMI+ DiabetesPedigreeFunction + Age  ~ Outcome, data=my_data)
cat("T2 statistic =",t2testsparr$stat[[1]],"\n")
print(t2testsparr)


#F-test
attach(my_data)
var.test(Pregnancies[Outcome==1],Pregnancies[Outcome==0])
var.test(Glucose[Outcome==1],Glucose[Outcome==0])
var.test(Insulin[Outcome==1],Insulin[Outcome==0])
var.test(SkinThickness[Outcome==1],SkinThickness[Outcome==0])
var.test(BMI[Outcome==1],BMI[Outcome==0])
var.test(DiabetesPedigreeFunction[Outcome==1],DiabetesPedigreeFunction[Outcome==0])
var.test(Age[Outcome==1],Age[Outcome==0])
var.test(BloodPressure[Outcome==1],BloodPressure[Outcome==0])

#levene Test - To test equality of variables 

install.packages("car")
library(car)
#my_data$Outcome <- is.factor(my_data$outcome)
#levels(my_data$Outcome) <- c("Non-Diabetic","Diabetic")
#leveneTest(my_data$Pregnancies, my_data$Outcome,center=mean)

with(my_data,leveneTest(Outcome,Pregnancies))
with(my_data,leveneTest(Outcome,BMI))
with(my_data,leveneTest(Outcome,Insulin))
with(my_data,leveneTest(Outcome,BloodPressure))
with(my_data,leveneTest(Outcome,Age))

# Correlation matrix
#This plot shows us correlation coeeficents of all the varaibles. 

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
# From the plot, we can say that the variable Glucose has a higher impact on the Outcome variable. They are highly Co-rrelated. 
#Pregnancies and Age are strongly correlated with coeeficient value 0.54. 
#SkinThickness , BMI and Skinthickness and Insulin are positively correlated with coeeficient values 0.4. 

#plot pregnancies and outcome 
ggplot(my_data,aes(x=Pregnancies,fill=(Outcome==1)))+geom_bar(position="Dodge")+scale_fill_manual(values=c("yellow","green"))+scale_x_continuous(limits=c(0,16))+labs(title="Pregnancies v Outcome")
 #not much impact on output variable

#Changing outcome from numerical to categorical varibale.
my_data$Outcome<- is.factor(my_data$Outome)
levels(my_data$Outcome) <- c("No","Yes")


#correlation plot
#This plot shows the relationship between the variables. 
ggpairs(my_data, aes(color=Outcome, alpha=0.75), lower=list(continuous="smooth"))+ theme_bw()+
  labs(title="Correlation Plot of Variance(diabetes)")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))
#From the box plots in the last segment, we see the variables Insulin, BloodPressure, and the DiabetesPedigreefunction 
#contain many outliers.

#stripchart is like scatter plots (or dot plots) of the given data. 
#It's like an alternative to boxplots when sample sizes are small and are also used to check outliers present in each variables
stripchart(my_data$BloodPressure,
           main="Blood pressure levels",
           xlab="Pressure levels",
           ylab="",
           method="jitter",
           col="orange",
           pch=1)

#Densityplots 
plot1 = qplot(my_data$Pregnancies, data = my_data, geom = "density", fill = "red") 
plot2 = qplot(my_data$Age, data = my_data, geom = "density", fill = "red")
plot3 = qplot(my_data$Glucose, data = my_data, geom = "density", fill = "red")
plot4 = qplot(my_data$BloodPressure, data = my_data, geom = "density", fill = "red")
grid.arrange(plot1, plot2, plot3, ncol = 3)
#The density plot here shows the distribution of the data and if they are positively or negatively skewed. 

#Plots a missingness map showing where missingness occurs in the dataset

missmap(my_data, main ="Missing values vs observed")

#No missing Values occured in our dataset. 






