# Load libraries we need
library(tidyverse)
library(rstudioapi)
library(knitr)
library(flextable)
library(stats)
library(DescTools) 
library(vcd)
library(car)
library(caret)

# ----------------------------------------------------------------------------------------
# 1) Getting the path of your current open file
# ----------------------------------------------------------------------------------------


# Getting the path of your current open file
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))

data <- read.csv("diabetes.csv", sep = ";")

cols_delete <- c("acetohexamide","examide","citoglipton","glimepiride.pioglitazone","metformin.rosiglitazone","metformin.pioglitazone")

data <- data%>% select(-cols_delete)

#data %>% group_by(examide) %>% summarise(count = n()) 
#data %>% group_by(acetohexamide) %>% summarise(count = n()) 
#data %>% group_by(citoglipton) %>% summarise(count = n()) 
#data %>% group_by(glimepiride.pioglitazone) %>% summarise(count = n()) 
#data %>% group_by(metformin.rosiglitazone) %>% summarise(count = n()) 
#data %>% group_by(metformin.pioglitazone) %>% summarise(count = n()) 


data %>% group_by(weight) %>% summarise(count = n()) 
data %>% group_by(age) %>% summarise(count = n()) 

#"weight"
#"age"


# ----------------------------------------------------------------------------------------
# 2) Transform data
# ----------------------------------------------------------------------------------------


data$diag_1 <- replace(data$diag_1, data$diag_1=='?', 'M')
data$diag_2 <- replace(data$diag_2, data$diag_2=='?', 'M')
data$diag_3 <- replace(data$diag_3, data$diag_3=='?', 'M')



data <- data %>% 
  mutate(
    diag_1_group = case_when(
      grepl("V", diag_1) ~ "Other",
      grepl("E", diag_1) ~ "Other",
      as.numeric(diag_1) > 0 & as.numeric(diag_1) <= 139 ~ "Infectious",
      as.numeric(diag_1) >= 140  & as.numeric(diag_1) <= 239 ~ "Neoplasms",
      as.numeric(diag_1) >= 250  & as.numeric(diag_1) < 251 ~ "Diabetes mellitus",
      as.numeric(diag_1) >= 240  & as.numeric(diag_1) < 250 ~ "Endocrine_Nutrional_Immunity",
      as.numeric(diag_1) >= 251  & as.numeric(diag_1) <= 279 ~ "Endocrine_Nutrional_Immunity",
      as.numeric(diag_1) >= 280  & as.numeric(diag_1) <= 289 ~ "Blood",
      as.numeric(diag_1) >= 290  & as.numeric(diag_1) <= 319 ~ "Mental Disorder",
      as.numeric(diag_1) >= 320  & as.numeric(diag_1) <= 389 ~ "Nervous System",
      as.numeric(diag_1) >= 390  & as.numeric(diag_1) <= 459 ~ "Circulatory",
      as.numeric(diag_1) >= 460  & as.numeric(diag_1) <= 519 ~ "Respiratory",
      as.numeric(diag_1) >= 520  & as.numeric(diag_1) <= 579 ~ "Digestive",
      as.numeric(diag_1) >= 580  & as.numeric(diag_1) <= 629 ~ "Genitourinary",
      as.numeric(diag_1) >= 630  & as.numeric(diag_1) <= 679 ~ "Pregnancy",
      as.numeric(diag_1) >= 680  & as.numeric(diag_1) <= 709 ~ "Skin",
      as.numeric(diag_1) >= 710  & as.numeric(diag_1) <= 739 ~ "Musculoskeletal",
      as.numeric(diag_1) >= 740  & as.numeric(diag_1) <= 759 ~ "Congenital Anomalies",
      as.numeric(diag_1) >= 780  & as.numeric(diag_1) < 996 ~ "Injury_Unspecified",
      as.numeric(diag_1) >= 996  & as.numeric(diag_1) < 1000 ~ "Medical Care Complications",
      T                       ~ "A_CHECK"
    ), .keep = "unused"
  ) %>% 
  mutate(
    diag_2_group = case_when(
      grepl("V", diag_2) ~ "Other",
      grepl("E", diag_2) ~ "Other",
      as.numeric(diag_2) > 0 & as.numeric(diag_2) <= 139 ~ "Infectious",
      as.numeric(diag_2) >= 140  & as.numeric(diag_2) <= 239 ~ "Neoplasms",
      as.numeric(diag_2) >= 250  & as.numeric(diag_2) < 251 ~ "Diabetes mellitus",
      as.numeric(diag_2) >= 240  & as.numeric(diag_2) < 250 ~ "Endocrine_Nutrional_Immunity",
      as.numeric(diag_2) >= 251  & as.numeric(diag_2) <= 279 ~ "Endocrine_Nutrional_Immunity",
      as.numeric(diag_2) >= 280  & as.numeric(diag_2) <= 289 ~ "Blood",
      as.numeric(diag_2) >= 290  & as.numeric(diag_2) <= 319 ~ "Mental Disorder",
      as.numeric(diag_2) >= 320  & as.numeric(diag_2) <= 389 ~ "Nervous System",
      as.numeric(diag_2) >= 390  & as.numeric(diag_2) <= 459 ~ "Circulatory",
      as.numeric(diag_2) >= 460  & as.numeric(diag_2) <= 519 ~ "Respiratory",
      as.numeric(diag_2) >= 520  & as.numeric(diag_2) <= 579 ~ "Digestive",
      as.numeric(diag_2) >= 580  & as.numeric(diag_2) <= 629 ~ "Genitourinary",
      as.numeric(diag_2) >= 630  & as.numeric(diag_2) <= 679 ~ "Pregnancy",
      as.numeric(diag_2) >= 680  & as.numeric(diag_2) <= 709 ~ "Skin",
      as.numeric(diag_2) >= 710  & as.numeric(diag_2) <= 739 ~ "Musculoskeletal",
      as.numeric(diag_2) >= 740  & as.numeric(diag_2) <= 759 ~ "Congenital Anomalies",
      as.numeric(diag_2) >= 780  & as.numeric(diag_2) < 996 ~ "Injury_Unspecified",
      as.numeric(diag_2) >= 996  & as.numeric(diag_2) < 1000 ~ "Medical Care Complications",
      T                       ~ "A_CHECK"
    ), .keep = "unused"
  ) %>% 
  mutate(
    diag_3_group = case_when(
      grepl("V", diag_3) ~ "Other",
      grepl("E", diag_3) ~ "Other",
      as.numeric(diag_3) > 0 & as.numeric(diag_3) <= 139 ~ "Infectious",
      as.numeric(diag_3) >= 140  & as.numeric(diag_3) <= 239 ~ "Neoplasms",
      as.numeric(diag_3) >= 250  & as.numeric(diag_3) < 251 ~ "Diabetes mellitus",
      as.numeric(diag_3) >= 240  & as.numeric(diag_3) < 250 ~ "Endocrine_Nutrional_Immunity",
      as.numeric(diag_3) >= 251  & as.numeric(diag_3) <= 279 ~ "Endocrine_Nutrional_Immunity",
      as.numeric(diag_3) >= 280  & as.numeric(diag_3) <= 289 ~ "Blood",
      as.numeric(diag_3) >= 290  & as.numeric(diag_3) <= 319 ~ "Mental Disorder",
      as.numeric(diag_3) >= 320  & as.numeric(diag_3) <= 389 ~ "Nervous System",
      as.numeric(diag_3) >= 390  & as.numeric(diag_3) <= 459 ~ "Circulatory",
      as.numeric(diag_3) >= 460  & as.numeric(diag_3) <= 519 ~ "Respiratory",
      as.numeric(diag_3) >= 520  & as.numeric(diag_3) <= 579 ~ "Digestive",
      as.numeric(diag_3) >= 580  & as.numeric(diag_3) <= 629 ~ "Genitourinary",
      as.numeric(diag_3) >= 630  & as.numeric(diag_3) <= 679 ~ "Pregnancy",
      as.numeric(diag_3) >= 680  & as.numeric(diag_3) <= 709 ~ "Skin",
      as.numeric(diag_3) >= 710  & as.numeric(diag_3) <= 739 ~ "Musculoskeletal",
      as.numeric(diag_3) >= 740  & as.numeric(diag_3) <= 759 ~ "Congenital Anomalies",
      as.numeric(diag_3) >= 780  & as.numeric(diag_3) < 996 ~ "Injury_Unspecified",
      as.numeric(diag_3) >= 996  & as.numeric(diag_3) < 1000 ~ "Medical Care Complications",
      T                       ~ "A_CHECK"
    ), .keep = "unused"
  )

data <- data %>% 
  mutate(
    Infectious = ifelse(diag_1_group=="Infectious"|diag_2_group=="Infectious"|diag_3_group=="Infectious", 1, 0),
    Neoplasms = ifelse(diag_1_group=="Neoplasms"|diag_2_group=="Neoplasms"|diag_3_group=="Neoplasms", 1, 0),
    Diabetes = ifelse(diag_1_group=="Diabetes mellitus"|diag_2_group=="Diabetes mellitus"|diag_3_group=="Diabetes mellitus", 1, 0),
    Endocrine = ifelse(diag_1_group=="Endocrine_Nutrional_Immunity"|diag_2_group=="Endocrine_Nutrional_Immunity"|diag_3_group=="Endocrine_Nutrional_Immunity", 1, 0),
    Blood = ifelse(diag_1_group=="Blood"|diag_2_group=="Blood"|diag_3_group=="Blood", 1, 0),
    Mental_Disorder = ifelse(diag_1_group=="Mental Disorder"|diag_2_group=="Mental Disorder"|diag_3_group=="Mental Disorder", 1, 0),
    Nervous_System = ifelse(diag_1_group=="Nervous System"|diag_2_group=="Nervous System"|diag_3_group=="Nervous System", 1, 0),
    Circulatory = ifelse(diag_1_group=="Circulatory"|diag_2_group=="Circulatory"|diag_3_group=="Circulatory", 1, 0),
    Respiratory = ifelse(diag_1_group=="Respiratory"|diag_2_group=="Respiratory"|diag_3_group=="Respiratory", 1, 0),
    Digestive = ifelse(diag_1_group=="Digestive"|diag_2_group=="Digestive"|diag_3_group=="Digestive", 1, 0),
    Genitourinary = ifelse(diag_1_group=="Genitourinary"|diag_2_group=="Genitourinary"|diag_3_group=="Genitourinary", 1, 0),
    Pregnancy = ifelse(diag_1_group=="Pregnancy"|diag_2_group=="Pregnancy"|diag_3_group=="Pregnancy", 1, 0),
    Skin = ifelse(diag_1_group=="Skin"|diag_2_group=="Skin"|diag_3_group=="Skin", 1, 0),
    Musculoskeletal = ifelse(diag_1_group=="Musculoskeletal"|diag_2_group=="Musculoskeletal"|diag_3_group=="Musculoskeletal", 1, 0),
    Congenital = ifelse(diag_1_group=="Congenital Anomalies"|diag_2_group=="Congenital Anomalies"|diag_3_group=="Congenital Anomalies", 1, 0),
    Injury_Unspecified = ifelse(diag_1_group=="Injury_Unspecified"|diag_2_group=="Injury_Unspecified"|diag_3_group=="Injury_Unspecified", 1, 0),
    Complications = ifelse(diag_1_group=="Medical Care Complications"|diag_2_group=="Medical Care Complications"|diag_3_group=="Medical Care Complications", 1, 0),
    .keep = "unused"
  )


data <- data %>% filter(encounter_id != "277875756")


get_init_stats <- function(df,col_target){
  # Calculate the number of distinct values for each column
  distinct_values <- df %>%
    summarise(across(everything(), ~ n_distinct(., na.rm = TRUE)))
  
  # Calculate the number of missing values for each column
  missing_values <- df %>%
    summarise(across(everything(), ~ sum(is.na(.))))
  
  # Create a summary dataframe
  summary_df <- data.frame(
    var = colnames(df),
    num_values = t(distinct_values),
    missings = t(missing_values),
    por_missing_Values = t(missing_values)/dim(df)[1]
  )
  summary_df <- summary_df %>% filter(var != col_target)
  return(summary_df)
}

#init_stats_data <- get_init_stats(data, col_target ="target")
#write.csv(init_stats_data , "C:\\Users\\lelr2\\Documents\\Lillian\\Challenges\\CDC\\init_stats.csv")


# ----------------------------------------------------------------------------------------
# 2)  set factors columns
# ----------------------------------------------------------------------------------------


cols_id <- c("encounter_id","patient_nbr","readmitted")
num_features <- c("time_in_hospital","num_lab_procedures","num_procedures",
                  "num_medications","number_outpatient","number_emergency",
                  "number_inpatient","number_diagnoses")

cat_features <- c("Infectious","Neoplasms","Diabetes","Endocrine","Blood","Mental_Disorder",
                  "Nervous_System","Circulatory","Respiratory","Digestive","Genitourinary",
                  "Pregnancy","Skin","Musculoskeletal","Congenital","Injury_Unspecified",
                  "Complications","age","admission_type_id","race","gender","weight","max_glu_serum",
                  "A1Cresult","metformin","repaglinide","nateglinide","chlorpropamide","glimepiride",
                  "glipizide","glyburide","tolbutamide","pioglitazone","rosiglitazone","acarbose",
                  "miglitol","troglitazone","tolazamide","insulin",
                  "glyburide.metformin","glipizide.metformin",
                  "change","diabetesMed")

data_model <- data %>%
  mutate(target = case_when(readmitted == "<30"~ 1, readmitted == "NO"~ 0)) %>%
  filter(is.na(target)== FALSE)

data_model <- data_model %>%
  select_at(c("encounter_id","target", num_features, cat_features)) %>%
  mutate_at(vars(all_of(cat_features)), as.factor)

# ----------------------------------------------------------------------------------------
# 3)  split data
# ----------------------------------------------------------------------------------------


indexes = createDataPartition(data_model$encounter_id, p = .50, list = F)
train = data_model[-indexes, ] %>% select(-encounter_id)
test = data_model[indexes, ] %>% select(-encounter_id)

# ----------------------------------------------------------------------------------------
# 4)  select significant features
# ----------------------------------------------------------------------------------------


get_sig_features <- function(col_target,df, alpha) {
  summary_df <- data.frame()
  for (i in (setdiff(colnames(df), col_target))){
    print(i)
    formula <- as.formula(paste(col_target, "~ factor(",i,")"))
    train_lg <- glm(formula, data = df, family = binomial(link = "logit")) 
    summary_lg = car::Anova(train_lg, test = "LR", type = "III")
    
    summary_i<- data.frame(
      'var' = i,
      'binary_level_or' = names(coef(train_lg)[2]),
      'binary_or' = exp(coef(train_lg)[2]),
      'p_value' = summary_lg$`Pr(>Chisq)`
    )
    summary_df <- rbind(summary_df,summary_i)
  }
  
  summary_df <- summary_df %>% mutate (vars_significant = ifelse(p_value< alpha, 1,0 ) )
  return(summary_df)
}


#important_featues <- get_sig_features("target",train, alpha = 0.05)
#write.csv(important_featues , "C:\\Users\\lelr2\\Documents\\Lillian\\Challenges\\CDC\\important_featues.csv")




cat_features <- c("pioglitazone","Injury_Unspecified","Endocrine","Complications","Diabetes",
                  "gender","Circulatory","miglitol","glipizide","weight","Respiratory",
                  "max_glu_serum","A1Cresult","admission_type_id","Musculoskeletal",
                  "Genitourinary","race","Pregnancy","change","Skin","metformin",
                  "diabetesMed","age","insulin")
