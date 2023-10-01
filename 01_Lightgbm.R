library(lightgbm)
library(caret)
library(ggplot2)
library(tidyverse)
library(Hmisc)
library(ROCit)
library(patchwork) # to plot multiple plot together
library(pROC)



# ----------------------------------------------------------------------------------------
# 1) Getting the path of your current open file
# ----------------------------------------------------------------------------------------
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))

data <- read.csv("diabetes.csv", sep = ";")
data_ini <- read.csv("diabetes.csv", sep = ";")

data_ini<- data_ini %>% mutate(target = case_when(readmitted == "<30"~ 1, TRUE ~ 0))
  

data<- data %>% mutate(age = 
                         case_when(age %in% c("[0-10)","[10-20)","[20-30)","[30-40)")~ "less_40",
                                   TRUE ~ age))


data <- data %>% mutate(age = gsub("\\[", "a_", age) )
data <- data %>% mutate(age = gsub("\\)", "", age) )
data <- data %>% mutate(age = gsub("\\-", "_", age) )


data<- data %>% mutate(weight = 
                         case_when(weight =="?" ~ "no_weight",
                                   TRUE ~ "with_weight"))


data %>% group_by(time_in_hospital) %>% summarise(count = n()) 


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

data <- data %>% mutate()

data %>% group_by(age) %>% summarise(count = n()) 


# ----------------------------------------------------------------------------------------
# 2) Data for modeling
# ----------------------------------------------------------------------------------------


num_features <- c("num_lab_procedures","num_procedures","num_medications","time_in_hospital","number_outpatient",
                  "number_diagnoses","number_emergency","number_inpatient","severity_of_disease")

cat_features <- c("gender","pioglitazone","Injury_Unspecified","Endocrine","Complications","Diabetes",
                  "Circulatory","miglitol","glipizide","Respiratory",
                  "max_glu_serum","A1Cresult","admission_type_id","Musculoskeletal",
                  "Genitourinary","race","Pregnancy","change","Skin","metformin",
                  "diabetesMed","insulin")

#"weight"
#"age"

#cat_features <- c("gender","pioglitazone","Injury_Unspecified","Endocrine","Complications","Diabetes",
#                  "Circulatory","miglitol","glipizide","weight","Respiratory",
#                  "max_glu_serum","A1Cresult","admission_type_id","Musculoskeletal",
#                  "Genitourinary","race","Pregnancy","change","Skin","metformin",
#                  "diabetesMed","age","insulin")



data_model <- data %>%
                  mutate(target = case_when(readmitted == "<30"~ 1, TRUE~ 0)) %>%
                  mutate(filter_target = case_when(readmitted == ">30"~ 1, TRUE~ 0)) %>%
                  mutate(severity_of_disease = (time_in_hospital + num_procedures + num_medications + num_lab_procedures + number_diagnoses)) %>%
                  select_at(c("filter_target","encounter_id", num_features, cat_features, "target"))

#                  filter(is.na(target)== FALSE) %>%


data_model <- data_model %>%
  select_at(c("filter_target","encounter_id","target", num_features, cat_features)) %>%
  mutate_at(vars(all_of(cat_features)), as.factor)


# ----------------------------------------------------------------------------------------
# 3) Split data
# ----------------------------------------------------------------------------------------


data_model_tot <- model.matrix(~ ., data = data_model%>% select(-encounter_id))

data_model_en <- data_model_tot[data_model_tot[,which(colnames(data_model_tot) %in% c("filter_target"))]==0,]

data_model_en <- data_model_en[,-which(colnames(data_model_tot) %in% c("filter_target"))]

indexes = createDataPartition(data_model_en[,which(colnames(data_model_en) %in% c("target"))], 
                              p = .70, list = F)

train_df = data_model[-indexes,] 
test_df = data_model[indexes, ] 


train = data_model_en[-indexes,] 
test = data_model_en[indexes, ] 

train_x = train[, -which(colnames(train) %in% c("(Intercept)","target"))]
train_y = train[, which(colnames(train) %in% c("target"))]

test_x = test[, -which(colnames(test) %in% c("(Intercept)","target"))]
test_y = test[, which(colnames(test) %in% c("target"))]

### filter only the most important features 
train_x = train_x[, which(colnames(train_x) %in% c(  "number_inpatient","number_emergency","num_lab_procedures","number_diagnoses","time_in_hospital","num_medications","metforminSteady","diabetesMedYes","severity_of_disease","num_procedures","raceCaucasian","Diabetes1","Circulatory1"))]
test_x = test_x[, which(colnames(test_x) %in% c(  "number_inpatient","number_emergency","num_lab_procedures","number_diagnoses","time_in_hospital","num_medications","metforminSteady","diabetesMedYes","severity_of_disease","num_procedures","raceCaucasian","Diabetes1","Circulatory1"))]



dtrain = lgb.Dataset(train_x, label = train_y)
dtest = lgb.Dataset.create.valid(dtrain, test_x, label = test_y)

lgb.Dataset.set.categorical(dtrain,setdiff(colnames(train_x),num_features))
lgb.Dataset.set.categorical(dtest, setdiff(colnames(train_x),num_features))


# ----------------------------------------------------------------------------------------
# 3) Training Lightgbm
# ----------------------------------------------------------------------------------------

# define parameters
params = list(
  objective = "binary"
  , metric = "auc"
)

# validation data
valids = list(test = dtest,
              train =dtrain)

# train model
model = lgb.train(
  params = params
  , data = dtrain
  , nrounds = 5L
  , valids = valids
)

# ----------------------------------------------------------------------------------------
# 4) Meassure AUC 
# ----------------------------------------------------------------------------------------


# Get auc values for "test" dataset
lgb.get.eval.result(model, "train", "auc")
lgb.get.eval.result(model, "test", "auc")

# feature importance
tree_imp = lgb.importance(model, percentage = TRUE)
lgb.plot.importance(tree_imp, top_n = 13L, measure = "Gain")

# ----------------------------------------------------------------------------------------
# 5) Probabilities predicted 
# ----------------------------------------------------------------------------------------

data_ini$prob <- predict(model, 
                           data = data_model_tot[,which(colnames(data_model_tot) %in% c(  "number_inpatient","number_emergency","num_lab_procedures","number_diagnoses","time_in_hospital","num_medications","metforminSteady","diabetesMedYes","severity_of_disease","num_procedures","raceCaucasian","Diabetes1","Circulatory1"))], 
                           type = "response")

##save final prob 

#write.csv(data_ini , "C:\\Users\\lelr2\\Documents\\Lillian\\Challenges\\CDC\\data_prob.csv")


#train_df$prob <- predict(model, data = train_x, type = "response")
#test_df$prob  <- predict(model, data = test_x, type = "response")

# ----------------------------------------------------------------------------------------
# 6) ROC +lift
# ----------------------------------------------------------------------------------------

data_roc <- rocit(data_ini$prob, data_ini$target)
summary(data_roc)
plot(data_roc)
# get best cut off
plot(data_roc)$optimal


logit_lift_train <- gainstable(data_roc,ngroup = 20)


# add ntiles to data_ini
data_ini <- data_ini %>% mutate(n_tiles = ntile(prob, 20))
data_ini %>% group_by(n_tiles) %>% dplyr::summarise(count = n(),
                                                   target = sum(target),
                                                   min_prob = min(prob),
                                                   max_prob = max(prob))


data_ini <- data_ini %>% mutate(readmission_prob_group = case_when(prob>=0.28 ~ "High",
                                                              prob>=0.20 ~ "Upper Intermediate",
                                                              prob>=0.167 ~ "Intermediate",
                                                              prob>=0.148 ~ "Low Intermediate",
                                                              TRUE ~ "LOW",
                                                              ))

write.csv(data_ini , "C:\\Users\\lelr2\\Documents\\Lillian\\Challenges\\CDC\\data_prob_final.csv")


# ----------------------------------------------------------------------------------------
# 6) discrimination slope
# ----------------------------------------------------------------------------------------


discrimination_slope <- function(split_name,df_prob_real){
  p1 <- df_prob_real %>% filter(target == 1) %>% select(prob)
  p0 <- df_prob_real %>% filter(target == 0) %>% select(prob)
  coef <- mean(p1$prob) - mean(p0$prob)
  
  plot_dis <- ggplot(df_prob_real, aes(prob, fill = factor(target))) +
    geom_density(alpha = 0.7) +
    scale_fill_grey() + 
    labs(x = "Predicted Probability", fill = "Outcome",
         title = paste(split_name ,": Coefficient of Discrimination = ",
                       round(coef, 3), sep = ""))
  
  coef_discrim <- list()
  
  coef_discrim[["coefficient_discrimination"]] <- coef
  coef_discrim[["plot_discrmination_slope"]] <- plot_dis
  
  return(coef_discrim)
}

discrimination_slope("Total data", dat_ini)
