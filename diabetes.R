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



Principal Component Analysis

diabetes = read.csv("C:/Users/Trishala/Desktop/Multivariate/diabetes.csv")
dim(diabetes)
attach(diabetes)
head(diabetes)
cor(diabetes[-9])
#principal component analysis
# Using prcomp to compute the principal components(eigenvalues and eigenvectors). 
#With scale=TRUE, variable means are set to zero, and variances set to one
diabetes_pca <- prcomp(diabetes[,-9],scale=TRUE)
diabetes_pca
names(diabetes_pca)
#outputs means of the vaiables
diabetes_pca$center

#outputs the standard deviation of the variables
diabetes_pca$sdev

#Rotation provides with principal component loadings.
diabetes_pca$rotation
diabetes_pca$rotation[1:5,1:4]
dim(diabetes_pca$x)

#plot resultant principal components
biplot(diabetes_pca,scale=0)

#standard deviation of the principal component
std_dev <- diabetes_pca$sdev
std_dev

#variance
var <- std_dev^2
var
#first 8 values
var[1:8]

(eigen_diabetes <- diabetes_pca$sdev^2)
names(eigen_diabetes) <- paste("PC",1:3,sep="")

eigen_diabetes


sumlambdas = sum(eigen_diabetes)

sumlambdas

#proportion of variance
prop_var <- var/sum(var)
prop_var

#cumulative proportion of variance
cum_var<-cumsum(prop_var)

matlambdas = rbind(eigen_diabetes,prop_var,cum_var) #vertically binds
rownames(matlambdas) = c("Eigenvalues","Prop. variance","Cum. prop. variance")
round(matlambdas,4)

#scree plot
plot1 <-plot(prop_var,xlab="Principal Components",
     ylab="Proportion Of Variance Explained",
     type="l")

plot2 <-plot(cumsum(prop_var),xlab="principal Component",
     ylab="Cumulative Proportin of Variance Explained",
     type="l")


# Principal component scores stored in diabetes_pca$x
diabetes_pca$x

# Identifying the scores by their diabetic status
diabtyp_pca = cbind(data.frame(Outcome),diabetes_pca$x)
diabtyp_pca
dim(diabtyp_pca)

# Means of scores for all the PC's classified by Diabetic status
tabmeansPC = aggregate(diabtyp_pca[,1:4],by=list(Diabetic=diabetes$Outcome),mean)
tabmeansPC
tabmeansPC = tabmeansPC[rev(order(tabmeansPC$Outcome)),]
tabmeansPC
tabfmeans = t(tabmeansPC[,-1])
tabfmeans
colnames(tabfmeans) = t(as.vector(tabmeansPC[1]))
tabfmeans

# Standard deviations of scores for all the PC's classified by Diabetic status
tabsdsPC = aggregate(diabtyp_pca[,1:4],by=list(Diabetic=diabetes$Outcome),sd)
tabfsds = t(tabsdsPC[,-1])
colnames(tabfsds) = t(as.vector(tabsdsPC[1]))
tabfsds

#t-test
t.test(PC1~diabetes$Outcome,data=diabtyp_pca)
t.test(PC2~diabetes$Outcome,data=diabtyp_pca)
t.test(PC3~diabetes$Outcome,data=diabtyp_pca)
t.test(PC4~diabetes$Outcome,data=diabtyp_pca)
t.test(PC5~diabetes$Outcome,data=diabtyp_pca)

# F ratio tests
var.test(PC1~diabetes$Outcome,data=diabtyp_pca)
var.test(PC2~diabetes$Outcome,data=diabtyp_pca)
var.test(PC3~diabetes$Outcome,data=diabtyp_pca)
var.test(PC4~diabetes$Outcome,data=diabtyp_pca)
var.test(PC5~diabetes$Outcome,data=diabtyp_pca)



# Plotting the scores for the first and second components
plot(diabtyp_pca$PC1, diabtyp_pca$PC2,pch=ifelse(diabtyp_pca$Outcome == "0",1,16),xlab="PC1", ylab="PC2", main="")
abline(h=0)
abline(v=0)
legend("bottomleft", legend=c("Diabetic","Non-diabetic"), pch=c(1,16))
plot(eigen_diabetes, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")
plot(log(eigen_diabetes), xlab = "Component number",ylab = "log(Component variance)", type="l",main = "Log (eigenvalue) diagram")
print(summary(diabetes_pca))
View(diabetes_pca)

#get the original value of the data based on PCA
center = diabetes_pca$center
scale = diabetes_pca$scale
new_diabetes = as.matrix(diabetes[,-1])
new_diabetes
drop(scale(new_diabetes,center=center, scale=scale) %*% diabetes_pca$rotation[,1])
predict(diabetes_pca)[,1] 
#predict calculates values based on loading matrix and new data

#The aboved two gives us the same thing. predict is a good function to know.



