library(plyr)
library(ggplot2)

graphs <- function(){
  
  cat("Loading Healthcare_DATA.csv...\n")
  data <- read.csv("Healthcare_DATA.csv", stringsAsFactors = FALSE)
  
  unique_adms <- data[!duplicated(data[c("PatientID", "AdmissionID")]), ]
  
  cat("\nTotal Unique Patients Loaded:", length(unique(unique_adms$PatientID)), "\n")
  cat("Total Admissions Loaded:", nrow(unique_adms), "\n\n")
  
  # Date Formatting
  unique_adms$AdmissionStartDate <- as.Date(substr(unique_adms$AdmissionStartDate, 1, 10), format="%Y-%m-%d")
  unique_adms$AdmissionEndDate   <- as.Date(substr(unique_adms$AdmissionEndDate, 1, 10), format="%Y-%m-%d")
  unique_adms <- unique_adms[!is.na(unique_adms$AdmissionStartDate), ]
  unique_adms <- unique_adms[order(unique_adms$PatientID, unique_adms$AdmissionStartDate), ]
  
  cat("Calculating 30-Day Readmissions...\n")
  
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
  
  # Feature Engineering
  final_df$PatientDateOfBirth <- as.Date(substr(final_df$PatientDateOfBirth, 1, 10), format="%Y-%m-%d")
  final_df$Initial_LOS <- as.numeric(difftime(final_df$AdmissionEndDate, final_df$AdmissionStartDate, units = "days"))
  final_df$AgeAtAdmission <- as.numeric(difftime(final_df$AdmissionStartDate, final_df$PatientDateOfBirth, units = "days")) / 365.25
  final_df$PatientPopulationPercentageBelowPoverty <- as.numeric(as.character(final_df$PatientPopulationPercentageBelowPoverty))
  
  cat("Generating All EDA Graphs...\n")
  
  # GRAPH 1: PIE CHART
  unique_patients_df <- final_df[!duplicated(final_df$PatientID), ]
  p1 <- ggplot(unique_patients_df, aes(x = "", fill = PatientGender)) +
    geom_bar(width = 1, stat = "count") +
    coord_polar("y", start = 0) +               
    geom_text(stat = 'count', aes(label = after_stat(count)), position = position_stack(vjust = 0.5), size = 6, color = "white") + 
    labs(title = "True Patient Distribution by Gender") +
    theme_void() + theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
  print(p1)
  
  # GRAPH 2: 30-DAY READMISSIONS BAR CHART
  p2 <- ggplot(final_df, aes(x = Readmitted30, fill = Readmitted30)) +
    geom_bar() +
    geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.5, size = 5) + 
    labs(title = "Class Distribution: 30-Day Readmission", x = "Readmitted Within 30 Days?", y = "Total Admissions") +
    scale_fill_manual(values = c("No" = "#5b9bd5", "Yes" = "#ed7d31")) +
    theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
  print(p2)
  
  # GRAPH 3: Admissions per Patient
  admin_counts <- as.data.frame(table(final_df$PatientID))
  colnames(admin_counts) <- c("PatientID", "NumAdmissions")
  p3 <- ggplot(admin_counts, aes(x = NumAdmissions)) +
    geom_bar(fill = "#2c3e50", color = "black", alpha = 0.8) +
    scale_x_continuous(breaks = seq(1, max(admin_counts$NumAdmissions), by = 1)) +
    labs(title = "Hospital Utilization: Admissions per Patient", x = "Total Number of Admissions", y = "Number of Patients") +
    theme_minimal()
  print(p3)
  
  # =====================================================================
  # GRAPH 4 FIX: CLEAN BAR CHART (geom_col)
  # Groups patients into age brackets and plots the Average Length of Stay
  # =====================================================================
  # 1. Create Age Brackets using Base R
  final_df$AgeGroup <- cut(final_df$AgeAtAdmission, 
                           breaks = c(0, 30, 40, 50, 60, 70, 80, 120), 
                           labels = c("<30", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))
  
  # 2. Calculate Mean Length of Stay per Group
  age_los_summary <- aggregate(Initial_LOS ~ AgeGroup + Readmitted30, data = final_df, FUN = mean)
  
  # 3. Plot it with geom_col()
  p4 <- ggplot(age_los_summary, aes(x = AgeGroup, y = Initial_LOS, fill = Readmitted30)) +
    geom_col(position = "dodge", color = "black", alpha = 0.85) +
    scale_fill_manual(values = c("No" = "#5b9bd5", "Yes" = "#ed7d31")) +
    labs(title = "Clinical Profile: Average Length of Stay by Age Group", 
         subtitle = "Side-by-side comparison of normal recoveries vs readmissions",
         x = "Patient Age Group", y = "Average Initial Length of Stay (Days)") +
    theme_bw() + theme(axis.text.x = element_text(face = "bold"))
  print(p4)
  
  # GRAPH 5: LENGTH OF STAY (Normalized Histogram)
  p5 <- ggplot(final_df, aes(x = Initial_LOS, fill = Readmitted30)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 1, alpha = 0.6, position = "identity", color = "black") +
    coord_cartesian(xlim = c(0, 20)) +  
    scale_fill_manual(values = c("No" = "#5b9bd5", "Yes" = "#ed7d31")) +
    labs(title = "Length of Stay Distribution: Readmitted vs Normal Recovery", 
         x = "Initial Length of Stay (Days)", y = "Percentage of Group's Total Population") +
    theme_bw()
  print(p5)
  
  # GRAPH 6: POVERTY (Density Plot)
  p6 <- ggplot(final_df, aes(x = PatientPopulationPercentageBelowPoverty, fill = Readmitted30)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("No" = "#5b9bd5", "Yes" = "#ed7d31")) +
    labs(title = "Socioeconomic Impact: Poverty Levels vs Readmission", 
         x = "Neighborhood Poverty Percentage (%)", y = "Density") +
    theme_minimal()
  print(p6)
  
  # GRAPH 7: Age Density
  p7 <- ggplot(final_df, aes(x = AgeAtAdmission, fill = Readmitted30)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("No" = "#5b9bd5", "Yes" = "#ed7d31")) +
    labs(title = "Age Distribution Density by Readmission Status", x = "Age at Admission (Years)", y = "Density") +
    theme_minimal()
  print(p7)
  
  # GRAPH 8: Top 5 Diagnoses
  readmitted_only <- final_df[final_df$Readmitted30 == "Yes", ]
  diag_freq <- as.data.frame(table(readmitted_only$PrimaryDiagnosisDescription))
  colnames(diag_freq) <- c("Diagnosis", "Frequency")
  top_diags_readm <- diag_freq[order(-diag_freq$Frequency), ][1:5, ]
  
  p8 <- ggplot(top_diags_readm, aes(x = reorder(Diagnosis, Frequency), y = Frequency)) +
    geom_col(fill = "#e74c3c", width = 0.6) +
    coord_flip() +
    labs(title = "Top 5 Diagnoses Driving 30-Day Readmissions", x = "Primary Diagnosis", y = "Number of Readmissions") +
    theme_bw()
  print(p8)
  
  assign("final_df", final_df, envir = .GlobalEnv)
}

graphs()