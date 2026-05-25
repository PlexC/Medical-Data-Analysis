library(plyr)
library(ggplot2)
source("C:/Users/Main/Desktop/Medical-Data-Analysis/graphs.R")
library(rpart)
library(rpart.plot)

logic <- function(){
  
  # Remove any remaining NA rows would kill the amount of data
  #model_data <- na.omit(model_data)
  median_poverty <- median(final_df$PatientPopulationPercentageBelowPoverty, na.rm = TRUE)
  final_df$PatientPopulationPercentageBelowPoverty[is.na(final_df$PatientPopulationPercentageBelowPoverty)] <- median_poverty
  
  
  # 1. FEATURE SELECTION (Preventing Data Leakage)
  # We only select columns a doctor would know on Discharge Day.
  model_data <- final_df[, c("Readmitted30", "PatientGender", "AgeAtAdmission", 
                             "Initial_LOS", "PatientPopulationPercentageBelowPoverty")]
  
  # 2. TRAIN / TEST SPLIT (80% 20% split)
  set.seed(123) # Ensures you get the same random split every time you run it
  sample_size <- floor(0.80 * nrow(model_data))
  train_indices <- sample(seq_len(nrow(model_data)), size = sample_size)
  
  train_data <- model_data[train_indices, ]
  test_data  <- model_data[-train_indices, ]
  
  cat("\nTraining Logistic Regression Model (glm)...\n")
  
  #testing amount
  cat("\nTotal rows in Model Data:", nrow(model_data), "(Should be exactly 36,143)\n")
  cat("Total rows in Test Data:", nrow(test_data), "(Should be exactly 10,843)\n\n")
  
  # The formula: Predict Readmitted30 using all other variables ( . )
  log_model <- glm(Readmitted30 ~ ., data = train_data, family = "binomial")
  
  # Print the academic summary 
  print(summary(log_model))
  
  # =====================================================================
  # VISUAL 1: THE FORCED DECISION TREE (Cost-Sensitive Learning)
  # =====================================================================
  cat("Training Cost-Sensitive Classification Tree...\n")
  
  # The Penalty Matrix: Missing a 'Yes' costs 50x more than a False Positive.
  penalty_matrix <- matrix(c(0, 1,   
                             50, 0), 
                           byrow = TRUE, nrow = 2)
  
  tree_model_forced <- rpart(Readmitted30 ~ ., data = train_data, method = "class", 
                             parms = list(loss = penalty_matrix),
                             control = rpart.control(cp = 0.0005, maxdepth = 4))
  
  rpart.plot(tree_model_forced, 
             type = 2, 
             extra = 104, 
             fallen.leaves = TRUE, 
             main = "Cost-Sensitive Classification Tree: High-Risk Profiling",
             box.palette = c("#5b9bd5", "#ed7d31"),
             shadow.col = "gray")
  
  
  # =====================================================================
  # EVALUATION: CONFUSION MATRIX
  # =====================================================================
  cat("\n--- Decision Tree Accuracy on UNSEEN Test Data (30%) ---\n")
  tree_predictions <- predict(tree_model_forced, test_data, type = "class")
  confusion_matrix <- table(Predicted = tree_predictions, Actual = test_data$Readmitted30)
  print(confusion_matrix)
  
  
  # =====================================================================
  # VISUAL 2: LOGISTIC REGRESSION FOREST PLOT (Odds Ratios)
  # =====================================================================
  cat("\nTraining Logistic Regression and Generating Forest Plot...\n")
  
  log_model <- glm(Readmitted30 ~ ., data = train_data, family = "binomial")
  
  summary_df <- as.data.frame(coef(summary(log_model)))
  summary_df$Variable <- rownames(summary_df)
  summary_df <- summary_df[-1, ] 
  
  summary_df$OddsRatio <- exp(summary_df$Estimate)
  summary_df$LowerCI <- exp(summary_df$Estimate - 1.96 * summary_df$`Std. Error`)
  summary_df$UpperCI <- exp(summary_df$Estimate + 1.96 * summary_df$`Std. Error`)
  
  summary_df$Variable <- gsub("PatientGenderMale", "Gender (Male)", summary_df$Variable)
  summary_df$Variable <- gsub("AgeAtAdmission", "Age at Admission", summary_df$Variable)
  summary_df$Variable <- gsub("Initial_LOS", "Initial Length of Stay", summary_df$Variable)
  summary_df$Variable <- gsub("PatientPopulationPercentageBelowPoverty", "Poverty Percentage", summary_df$Variable)
  
  forest_plot <- ggplot(summary_df, aes(x = OddsRatio, y = reorder(Variable, OddsRatio))) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red", linewidth = 1) +
    geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2, color = "#5b9bd5", linewidth = 1.2) +
    geom_point(size = 5, color = "#ed7d31") +
    labs(title = "Logistic Regression: Readmission Odds Ratios",
         subtitle = "Dots to the right of the red line INCREASE risk. Dots to the left DECREASE risk.",
         x = "Odds Ratio (Impact on Readmission)", y = "Predictive Feature") +
    theme_bw() + theme(axis.text.y = element_text(face = "bold", size = 11))
  
  print(forest_plot)
}
logic()