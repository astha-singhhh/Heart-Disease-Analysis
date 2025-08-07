# 1: Download dataset
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"

# Column names for the heart disease dataset
col_names <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", 
               "thalach", "exang", "oldpeak", "slope", "ca", "thal", "target")
# renamed num to target

heart_data <- read.csv(url, header = FALSE, col.names = col_names, na.strings = "?")

#------------------------------------------------------------------------------#

# 2: Clean and understand the dataset

# Remove rows with missing values
heart_data <- heart_data[complete.cases(heart_data), ]
cat("Rows after removing missing data:", nrow(heart_data), "\n")


# Disease Classification (target variable)
# < 50% blockage = Usually not dangerous, blood flow is adequate
# > 50% blockage = Significantly restricts blood flow, high risk for heart attack
heart_data$disease_status <- ifelse(heart_data$target == 0, 
                                    "No Significant Disease",  # 0 = < 50% diameter narrowing
                                    "Significant Heart Disease") # 1-4 = > 50% diameter narrowing


heart_data$sex_label <- ifelse(heart_data$sex == 1, "Male", "Female")

# Chest pain types (cp variable)
heart_data$chest_pain_type <- factor(heart_data$cp, 
                                     levels = 1:4,
                                     labels = c("Typical Angina",    # Classic heart pain
                                                "Atypical Angina",   # Similar but not classic
                                                "Non-Anginal Pain",  # Chest pain, not heart-related
                                                "Asymptomatic"))     # No chest pain

# Exercise induced angina (exang variable)
heart_data$exercise_angina <- ifelse(heart_data$exang == 1, 
                                     "Exercise Induced Angina",   # Pain during exercise
                                     "No Exercise Angina")        # No exercise pain

# Fasting blood sugar (fbs variable) 
heart_data$high_blood_sugar <- ifelse(heart_data$fbs == 1, 
                                      "Fasting BS > 120 mg/dl",  # Diabetic range
                                      "Normal Fasting BS")       # Normal

# Distribution
print("Disease distribution (based on angiographic analysis):")
print("No Significant Disease = < 50% vessel diameter narrowing")
print("Significant Heart Disease = > 50% vessel diameter narrowing")
table(heart_data$disease_status)

print("\nSex distribution:")
table(heart_data$sex_label)

print("\nChest pain types:")
table(heart_data$chest_pain_type)

#------------------------------------------------------------------------------#

# 3: Statistical Analysis

cat("\n=== AGE ANALYSIS ===")
# Compare ages between disease groups
no_disease_age <- heart_data$age[heart_data$disease_status == "No Significant Disease"]
disease_age <- heart_data$age[heart_data$disease_status == "Significant Heart Disease"]

print(paste("Average age - No Disease:", round(mean(no_disease_age), 1)))
print(paste("Average age - Heart Disease:", round(mean(disease_age), 1)))

# Statistical test
age_test <- t.test(disease_age, no_disease_age)
print(paste("P-value for age difference:", round(age_test$p.value, 4)))

cat("\n=== CHOLESTEROL ANALYSIS ===")
# Compare cholesterol levels
no_disease_chol <- heart_data$chol[heart_data$disease_status == "No Significant Disease"]
disease_chol <- heart_data$chol[heart_data$disease_status == "Significant Heart Disease"]

print(paste("Average cholesterol - No Disease:", round(mean(no_disease_chol), 1)))
print(paste("Average cholesterol - Heart Disease:", round(mean(disease_chol), 1)))

chol_test <- t.test(disease_chol, no_disease_chol)
print(paste("P-value for cholesterol difference:", round(chol_test$p.value, 4)))

cat("\n=== MAX HEART RATE ANALYSIS ===")
# Compare maximum heart rate achieved during exercise
no_disease_hr <- heart_data$thalach[heart_data$disease_status == "No Significant Disease"]
disease_hr <- heart_data$thalach[heart_data$disease_status == "Significant Heart Disease"]

print(paste("Average max heart rate - No Disease:", round(mean(no_disease_hr), 1)))
print(paste("Average max heart rate - Heart Disease:", round(mean(disease_hr), 1)))

hr_test <- t.test(disease_hr, no_disease_hr)
print(paste("P-value for heart rate difference:", round(hr_test$p.value, 4)))

#------------------------------------------------------------------------------#

# 4: Visualizations

# Plot 1: Age distribution by disease status
boxplot(age ~ disease_status, data = heart_data,
        main = "Age Distribution by Heart Disease Status",
        xlab = "Disease Status",
        ylab = "Age (years)",
        col = c("lightblue", "lightcoral"))

# Plot 2: Cholesterol levels
boxplot(chol ~ disease_status, data = heart_data,
        main = "Cholesterol Levels by Heart Disease Status",
        xlab = "Disease Status", 
        ylab = "Cholesterol (mg/dl)",
        col = c("lightblue", "lightcoral"))

# Plot 3: Heart rate comparison
boxplot(thalach ~ disease_status, data = heart_data,
        main = "Maximum Heart Rate by Disease Status",
        xlab = "Disease Status",
        ylab = "Max Heart Rate (bpm)",
        col = c("lightblue", "lightcoral"))

#------------------------------------------------------------------------------#

# Step 5: Summary Table

cat("\n=== SUMMARY OF ALL TESTS ===\n")
summary_results <- data.frame(
  Variable = c("Age", "Cholesterol", "Max Heart Rate"),
  No_Disease_Mean = c(round(mean(no_disease_age), 1),
                      round(mean(no_disease_chol), 1),
                      round(mean(no_disease_hr), 1)),
  Heart_Disease_Mean = c(round(mean(disease_age), 1),
                         round(mean(disease_chol), 1),
                         round(mean(disease_hr), 1)),
  P_Value = c(round(age_test$p.value, 4),
              round(chol_test$p.value, 4),
              round(hr_test$p.value, 4)),
  Significant = c(age_test$p.value < 0.05,
                  chol_test$p.value < 0.05,
                  hr_test$p.value < 0.05)
)
print(summary_results)

# Save results
write.csv(heart_data, "heart_disease_data.csv", row.names = FALSE)
write.csv(summary_results, "analysis_results.csv", row.names = FALSE)
