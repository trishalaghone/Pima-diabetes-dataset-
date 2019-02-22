# Pima-diabetes-dataset-
This dataset is from National Institute of Diabetes and Digestive and Kidney Diseases. The aim of this project is to diagnose if a particular patient has diabetes based on various medical factors in the dataset. All patients are females and are atleast 21 years old. 
This dataset contains of one targer variable i.e the outcome and several predictor variables such as BMI, weight, No. of pregancies etc. 


We would like to build a model that would  accurately predict if the patient has diabetes or not. 

The Pima diabetes dataset consists of 9 variables:  

#Pregnancies -Number of times pregnant
#GlucosePlasma -glucose concentration a 2 hours in an oral glucose tolerance test
#BloodPressure- Diastolic blood pressure (mm Hg)
#SkinThicknessTriceps -skin fold thickness (mm)
#Insulin2- Hour serum insulin (mu U/ml)
#BMIBody mass index- (weight in kg/(height in m)^2)
#DiabetesPedigreeFunctionDiabetes -pedigree function
#Age -(years)
#OutcomeClass variable (0 or 1) 268 of 768 are 1, the others are 0


Data Is Only As Good As The Questions You Ask
 The truth is that no matter how advanced your IT infrastructure is, your data will not provide you with a ready-made solution unless you ask it specific questions regarding data analysis. 

Diabetes dataset. 
What exactly do I want to find out? 
The database consists of various medical factors that will tells us if a female is diabetic or non-diabetic.Build different models which will fit the data and will help us accurately predict if the females has diabetes or not. 

What does the data 'look' like? Does it follow any known probability distributions?
The dataset looks like it is complete and has no missing value. Missingness can be found using the missingmap function. Looking the data , I can plot histograms to check if the data is right or left skewed and perform transformations accordingly.Histograms help us give the distribution of the data. Using Distribution tests can also help us find the probability distribution the data follows. Considering p-value as 0.05, wee can check if the distributions have null hypothesis or an alternate hypothesis. 

How are the various measurements related?
We can understand the relationship between the variables with the help of the correlogram matrix. The correlogram matrix shows correlation coefficients between each variable. A positive correlation coeeficient shows that they are directly proportional to each other and negative coeffiecinet shows that they are inversely proportional. 

 Number of Pregnancies has an impact over diabetes outcome ?
 plotting preganancies against outcome will show how strongly they are correlated.

Performed Visualizations on the dataset to understand the data better, to understand relation between the variables and to detect the outliers. 
Data Visualizations will help us select the correct models. 
