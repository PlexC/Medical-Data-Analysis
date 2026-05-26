#read the files and organizes them to 1 csv file  
org <- function(){ 
  patients   <- read.table("PatientCorePopulatedTable.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  admissions <- read.table("AdmissionsCorePopulatedTable.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  diagnoses  <- read.table("AdmissionsDiagnosesCorePopulatedTable.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  labs <- read.table("LabsCorePopulatedTable.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  #merge data based on ID
  df_step1 <- merge(patients, admissions, by = "PatientID", all.y = TRUE)
  df_step2 <- merge(df_step1, diagnoses, by = c("PatientID", "AdmissionID"), all.x = TRUE)
  master_data <- merge(df_step2, labs, by = c("PatientID", "AdmissionID"), all.x = TRUE)
  master_data <- master_data[order(master_data$PatientID, 
                                   master_data$AdmissionStartDate, 
                                   master_data$LabDateTime), ]
  #turn it into csv file
  write.csv(master_data, file = "Healthcare_DATA.csv", row.names = FALSE)
}