library(plyr)
library(ggplot2)
library(rpart)
library(rpart.plot)

logic <- function(){
  
  cat("\nLoading and Cleaning Healthcare_DATA.csv for Modeling...\n")
  data <- read.csv("Healthcare_DATA.csv", stringsAsFactors = FALSE)
  
  # --- COMMON DATA CLEANING BLOCK ---
  unique_adms <- data[!duplicated(data[c("PatientID", "AdmissionID")]), ]
  
  unique_adms$AdmissionStartDate <- as.Date(substr(unique_adms$AdmissionStartDate, 1, 10), format="%Y-%m-%d")
  unique_adms$AdmissionEndDate   <- as.Date(substr(unique_adms$AdmissionEndDate, 1, 10), format="%Y-%m-%d")
  unique_adms <- unique_adms[!is.na(unique_adms$AdmissionStartDate), ]
  unique_adms <- unique_adms[order(unique_adms$PatientID, unique_adms$AdmissionStartDate), ]
  
  calc_readmission <- function(sub_df) {
    n <- nrow(sub_df)
    sub_df$DaysToNext <- NA
    sub_df$Readmitted30 <- 0 
    if (n > 1) {
      diff_days <- as.numeric(difftime(sub_df$AdmissionStartDate[-1], sub_df$AdmissionEndDate[-n], units = "days"))
      sub_df$DaysToNext[1:(n-1)] <- diff_days
      sub_df$Readmitted30 <- ifelse(!is.na(sub_df$DaysToNext) & sub_df$DaysToNext <= 30, 1, 0)
    }
    return(sub_df)
  }
  
  final_df <- ddply(unique_adms, .(PatientID), calc_readmission)
  final_df$Readmitted30 <- factor(final_df$Readmitted30, levels = c(0, 1), labels = c("No", "Yes"))
  
  final_df$PatientDateOfBirth <- as.Date(substr(final_df$PatientDateOfBirth, 1, 10), format="%Y-%m-%d")
  final_df$Initial_LOS <- as.numeric(difftime(final_df$AdmissionEndDate, final_df$AdmissionStartDate, units = "days"))
  final_df$AgeAtAdmission <- as.numeric(difftime(final_df$AdmissionStartDate, final_df$PatientDateOfBirth, units = "days")) / 365.25
  final_df$PatientPopulationPercentageBelowPoverty <- as.numeric(as.character(final_df$PatientPopulationPercentageBelowPoverty))
  # ----------------------------------
  
  cat("\n--- PHASE 2: CLINICAL FEATURE ENGINEERING ---\n")
  
  # IMPUTE MISSING POVERTY DATA
  median_poverty <- median(final_df$PatientPopulationPercentageBelowPoverty, na.rm = TRUE)
  final_df$PatientPopulationPercentageBelowPoverty[is.na(final_df$PatientPopulationPercentageBelowPoverty)] <- median_poverty
  
  # CREATE CLINICAL FEATURE: "Total Lab Tests"
  cat("Calculating Lab Test Volume per patient...\n")
  valid_labs <- data[!is.na(data$LabName) & data$LabName != "", ]
  lab_counts <- as.data.frame(table(valid_labs$AdmissionID))
  colnames(lab_counts) <- c("AdmissionID", "Total_Lab_Tests")
  
  final_df <- merge(final_df, lab_counts, by="AdmissionID", all.x=TRUE)
  final_df$Total_Lab_Tests[is.na(final_df$Total_Lab_Tests)] <- 0 
  
  # CREATE CLINICAL FEATURE: "High-Risk Diagnosis"
  cat("Flagging High-Risk Diagnoses...\n")
  readmitted_only <- final_df[final_df$Readmitted30 == "Yes", ]
  diag_freq <- as.data.frame(table(readmitted_only$PrimaryDiagnosisDescription))
  
  top_5_diags <- as.character(diag_freq[order(-diag_freq$Freq), ][1:5, "Var1"])
  
  final_df$Has_HighRisk_Disease <- ifelse(final_df$PrimaryDiagnosisDescription %in% top_5_diags, 1, 0)
  
  # FEATURE SELECTION
  model_data <- final_df[, c("Readmitted30", "PatientGender", "AgeAtAdmission", 
                             "Initial_LOS", "PatientPopulationPercentageBelowPoverty",
                             "Total_Lab_Tests", "Has_HighRisk_Disease")] 
  
  # TRAIN / TEST SPLIT (80% / 20%)
  set.seed(123) 
  sample_size <- floor(0.80 * nrow(model_data))
  train_indices <- sample(seq_len(nrow(model_data)), size = sample_size)
  
  train_data <- model_data[train_indices, ]
  test_data  <- model_data[-train_indices, ]
  
  cat("\nTraining Clinical Logistic Regression Model (glm)...\n")
  log_model <- glm(Readmitted30 ~ ., data = train_data, family = "binomial")
  print(summary(log_model))
  
  # VISUAL 1: THE CLINICAL DECISION TREE
  cat("Training Cost-Sensitive Classification Tree...\n")
  penalty_matrix <- matrix(c(0, 1, 50, 0), byrow = TRUE, nrow = 2)
  
  tree_model_forced <- rpart(Readmitted30 ~ ., data = train_data, method = "class", 
                             parms = list(loss = penalty_matrix),
                             control = rpart.control(cp = 0.0005, maxdepth = 4))
  
  Sys.sleep(0.5) 
  
  rpart.plot(tree_model_forced, 
             type = 2, 
             extra = 104, 
             fallen.leaves = TRUE, 
             main = "Clinical Decision Tree: High-Risk Profiling",
             box.palette = c("#5b9bd5", "#ed7d31"),
             shadow.col = "gray")
  
  # EVALUATION: CONFUSION MATRIX
  cat("\n--- Decision Tree Accuracy on UNSEEN Test Data (20%) ---\n")
  tree_predictions <- predict(tree_model_forced, test_data, type = "class")
  confusion_matrix <- table(Predicted = tree_predictions, Actual = test_data$Readmitted30)
  print(confusion_matrix)
  
  # VISUAL 2: LOGISTIC REGRESSION FOREST PLOT
  cat("\nGenerating Forest Plot...\n")
  
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
  summary_df$Variable <- gsub("Total_Lab_Tests", "Total Lab Tests", summary_df$Variable)
  summary_df$Variable <- gsub("Has_HighRisk_Disease", "High-Risk Disease (Top 5)", summary_df$Variable)
  
  forest_plot <- ggplot(summary_df, aes(x = OddsRatio, y = reorder(Variable, OddsRatio))) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red", linewidth = 1) +
    geom_errorbar(aes(xmin = LowerCI, xmax = UpperCI), width = 0.2, color = "#5b9bd5", linewidth = 1.2, orientation = "y") +
    geom_point(size = 5, color = "#ed7d31") +
    labs(title = "Clinical Logistic Regression: Readmission Odds Ratios",
         subtitle = "Dots to the right of the red line INCREASE risk. Dots to the left DECREASE risk.",
         x = "Odds Ratio (Impact on Readmission)", y = "Predictive Feature") +
    theme_bw() + theme(axis.text.y = element_text(face = "bold", size = 11))
  
  Sys.sleep(0.5)
  print(forest_plot)
}

logic()