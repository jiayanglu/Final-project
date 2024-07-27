#load packages needed
library(tidyverse)
library(caret)
library(plumber)

set.seed(11)

#read in csv data
data_original <- read_csv('diabetes_binary_health_indicators_BRFSS2015.csv')
deplicates <- data_original[duplicated(data_original), ]
#then we need to exclude data that are duplicated
data_no_duplicates <- data_original[!duplicated(data_original), ]

#Now we want to convert numeric variables to factors according to previous sorted unique values for each variable.
data <- data_no_duplicates

data$Diabetes_binary <- factor(
  data$Diabetes_binary, 
  levels = c(0,1), 
  labels = c("No_diabetes", "Prediabetes_or_diabetes")
)

data$HighBP <- factor(
  data$HighBP,
  levels = c(0,1), 
  labels = c("No_high_BP", "High_BP")
)

data$HighChol <- factor(
  data$HighChol,
  levels = c(0,1), 
  labels = c("No_high_cholesterol", "High_cholesterol")
)

data$CholCheck <- factor(
  data$CholCheck,
  levels = c(0,1), 
  labels = c("No_cholesterol_check", "Cholesterol_check")
)

data$Smoker <- factor(
  data$Smoker,
  levels = c(0,1), 
  labels = c("No", "Yes")
)

data$Stroke <- factor(
  data$Stroke,
  levels = c(0,1), 
  labels = c("No", "Yes")
)

data$HeartDiseaseorAttack <- factor(
  data$HeartDiseaseorAttack,
  levels = c(0,1), 
  labels = c("No", "Yes")
)

data$PhysActivity <- factor(
  data$PhysActivity,
  levels = c(0,1), 
  labels = c("No", "Yes")
)

data$Fruits <- factor(
  data$Fruits,
  levels = c(0,1), 
  labels = c("No", "Yes")
)

data$Veggies <- factor(
  data$Veggies,
  levels = c(0,1), 
  labels = c("No", "Yes")
)

data$HvyAlcoholConsump <- factor(
  data$HvyAlcoholConsump,
  levels = c(0,1), 
  labels = c("No", "Yes")
)

data$AnyHealthcare <- factor(
  data$AnyHealthcare,
  levels = c(0,1), 
  labels = c("No", "Yes")
)

data$NoDocbcCost <- factor(
  data$NoDocbcCost,
  levels = c(0,1), 
  labels = c("No", "Yes")
)

data$GenHlth <- factor(
  data$GenHlth,
  levels = c(1, 2, 3, 4, 5), 
  labels = c("Excellent", "Very_good", "Good", "Fair", "Poor")
)

data$DiffWalk <- factor(
  data$DiffWalk,
  levels = c(0,1), 
  labels = c("No", "Yes")
)

data$Sex <- factor(
  data$Sex,
  levels = c(0,1), 
  labels = c("Female", "Male")
)

data$Age <- factor(
  data$Age,
  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), 
  labels = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80_or_older")
)

data$Education <- factor(
  data$Education,
  levels = c(1, 2, 3, 4, 5, 6), 
  labels = c("Never_attended_school_or_only_kindergarten", "Elementary", "Some_high_school", "High school_graduate", "Some_college_or_technical_school", "College_graduate")
)

data$Income <- factor(
  data$Income,
  levels = c(1, 2, 3, 4, 5, 6, 7, 8), 
  labels = c("Less_than_10K", "10K_to_less_than_15K", "15K_to_Less_than_20K", "20K_to_less_than_25K", "25K_to_less_than_35K", "35K_to_less_than_50k", "50k_to_less_than_75k", "75k_or_more")
)

#Our best model: Logistic regression Model 3
trctrl <- trainControl(method = "cv", number = 5)

logistic_M3_fit <- train(Diabetes_binary ~ GenHlth + HighBP + DiffWalk + BMI + HighChol + Age + HeartDiseaseorAttack + PhysHlth + Income + PhysActivity, 
                         data = data, 
                         method = "glm",
                         family="binomial",
                         trControl=trctrl)

# Calculate default values
default_values <- data.frame(
  GenHlth = names(sort(table(data$GenHlth), decreasing = TRUE))[1],
  HighBP = names(sort(table(data$HighBP), decreasing = TRUE))[1],
  DiffWalk = names(sort(table(data$DiffWalk), decreasing = TRUE))[1],
  BMI = mean(data$BMI, na.rm = TRUE),
  HighChol = names(sort(table(data$HighChol), decreasing = TRUE))[1],
  Age = names(sort(table(data$Age), decreasing = TRUE))[1],
  HeartDiseaseorAttack = names(sort(table(data$HeartDiseaseorAttack), decreasing = TRUE))[1],
  PhysHlth = round(mean(data$PhysHlth, na.rm = TRUE),0),
  Income = names(sort(table(data$Income), decreasing = TRUE))[1],
  PhysActivity = names(sort(table(data$PhysActivity), decreasing = TRUE))[1]
)
default_values

#Define API
#create a pred endpoint

#* Predict diabetes
#* @param GenHlth Factor. General health rating ("Excellent", "Very good", "Good", "Fair", "Poor").
#* @param HighBP Factor. High blood pressure indicator ("No_high_BP" or "High_BP").
#* @param DiffWalk Factor. Difficulty walking or climbing stairs ("No" or "Yes").
#* @param BMI Numeric. Body Mass Index.
#* @param HighChol Factor. High cholesterol indicator ("No_high_cholesterol" or "High_cholesterol").
#* @param Age Factor. Age group ("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80_or_older").
#* @param HeartDiseaseorAttack Factor. Heart disease or attack indicator ("No" or "Yes").
#* @param PhysHlth Numeric. Days of poor physical health in the past 30 days.
#* @param Income Factor. Income level ("Less_than_10K", "10K_to_less_than_15K", "15K_to_Less_than_20K", "20K_to_less_than_25K", "25K_to_less_than_35K", "35K_to_less_than_50k", "50k_to_less_than_75k", "75k_or_more").
#* @param PhysActivity Factor. Physical activity indicator ("No" or "Yes").
#* @get /pred

fun_API <- function(GenHlth = default_values$GenHlth,
                    HighBP = default_values$HighBP,
                    DiffWalk = default_values$DiffWalk,
                    BMI = default_values$BMI,
                    HighChol = default_values$HighChol,
                    Age = default_values$Age,
                    HeartDiseaseorAttack = default_values$HeartDiseaseorAttack,
                    PhysHlth = default_values$PhysHlth,
                    Income = default_values$Income,
                    PhysActivity = default_values$PhysActivity) {
  # Create a new data frame with the input values
  new_data <- data.frame(
    GenHlth = factor(GenHlth, levels = levels(data$GenHlth)),
    HighBP = factor(HighBP, levels = levels(data$HighBP)),
    DiffWalk = factor(DiffWalk, levels = levels(data$DiffWalk)),
    BMI = as.numeric(BMI),
    HighChol = factor(HighChol, levels = levels(data$HighChol)),
    Age = factor(Age, levels = levels(data$Age)),
    HeartDiseaseorAttack = factor(HeartDiseaseorAttack, levels = levels(data$HeartDiseaseorAttack)),
    PhysHlth = as.numeric(PhysHlth),
    Income = factor(Income, levels = levels(data$Income)),
    PhysActivity = factor(PhysActivity, levels = levels(data$PhysActivity))
  )
  
  # Predict the class probabilities
  predictions <- predict(logistic_M3_fit, new_data, type = "prob")
  
  return(predictions)
}

#http://localhost:8000/pred

#Example 1
fun_API(GenHlth = "Poor",
        HighBP = "High_BP",
        DiffWalk = "Yes",
        BMI = 100,
        HighChol = "High_cholesterol",
        Age = "80_or_older",
        HeartDiseaseorAttack = "Yes",
        PhysHlth = 30,
        Income = "Less_than_10K",
        PhysActivity = "No") 

#http://localhost:8000/pred?GenHlth=Poor&HighBP=High_BP&DiffWalk=Yes&BMI=100&HighChol=High_cholesterol&Age=80_or_older&HeartDiseaseorAttack=Yes&PhysHlth=30&Income=Less_than_10K&PhysActivity=No

#Example 2
fun_API(GenHlth = "Excellent",
        HighBP = "No_high_BP",
        DiffWalk = "No",
        BMI = 20,
        HighChol = "No_high_cholesterol",
        Age = "18-24",
        HeartDiseaseorAttack = "No",
        PhysHlth = 0,
        Income = "75k_or_more",
        PhysActivity = "Yes") 

#http://localhost:8000/pred?GenHlth=Excellent&HighBP=No_high_BP&DiffWalk=No&BMI=20&HighChol=No_high_cholesterol&Age=18-24&HeartDiseaseorAttack=No&PhysHlth=0&Income=75k_or_more&PhysActivity=Yes

#Example 3
fun_API(GenHlth = "Fair",
        HighBP = "High_BP",
        DiffWalk = "Yes",
        BMI = 30,
        HighChol = "High_cholesterol",
        Age = "65-69",
        HeartDiseaseorAttack = "Yes",
        PhysHlth = 7,
        Income = "50k_to_less_than_75k",
        PhysActivity = "No")

#http://localhost:8000/pred?GenHlth=Fair&HighBP=High_BP&DiffWalk=Yes&BMI=30&HighChol=High_cholesterol&Age=65-69&HeartDiseaseorAttack=Yes&PhysHlth=7&Income=50k_to_less_than_75k&PhysActivity=No

#* Information about the API
#* @get /info
function() {
  list(
    name = "Jia Lu",
    github_pages_url = "https://jiayanglu.github.io/Final-project/"
  )
}

#http://localhost:8000/info
