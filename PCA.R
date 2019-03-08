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
