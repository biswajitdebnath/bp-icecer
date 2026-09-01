#https://www.kaggle.com/datasets/cdc/national-health-and-nutrition-examination-survey

# Install packages (run once)
#install.packages(c("tidyverse", "haven", "tableone", "broom"))
#install.packages("data.table")

# Load libraries
library(data.table)
library(tidyverse)
library(haven)
library(tableone)
library(broom)
library(caret)
library(pROC)
library(scales)
library(gridExtra)

## 1. IMPORT NHANES FILES
demographic <- fread("C:/Biswajit/personal/Ritisha/data/demographic.csv")
diet  <- fread("C:/Biswajit/personal/Ritisha/data/diet.csv")
examination  <- fread("C:/Biswajit/personal/Ritisha/data/examination.csv")
labs  <- fread("C:/Biswajit/personal/Ritisha/data/labs.csv")
medications  <- fread("C:/Biswajit/personal/Ritisha/data/medications.csv")
questionnaire  <- fread("C:/Biswajit/personal/Ritisha/data/questionnaire.csv")

## Merge all the data files into one
nhanes <- demographic %>%
  left_join(diet, by="SEQN") %>%
  left_join(examination, by="SEQN") %>%
  left_join(labs, by="SEQN") %>%
  left_join(medications, by="SEQN") %>%
  left_join(questionnaire, by="SEQN") 

## For including only hypertension patients for my research
nhanes <- nhanes %>%
  filter(BPQ020 == 1      #BPQ020 = 1 means participant reports being diagnosed with hypertension.
         | BPQ040A == 1)  #BPQ040A = 1 doctor asked to take medication

## Consider only adults (age 18 and above)
nhanes <- nhanes %>%
  filter(RIDAGEYR >= 18)

## Create output variable (if blood pressure is controlled or not)
nhanes <- nhanes %>%
  mutate(
    mean_sbp = rowMeans(cbind(BPXSY1, BPXSY2, BPXSY3, BPXSY4), na.rm = TRUE),
    mean_dbp = rowMeans(cbind(BPXDI1, BPXDI2, BPXDI3, BPXDI4), na.rm = TRUE)
  )
nhanes <- nhanes %>% mutate(bp_controlled = ifelse(mean_sbp < 140 & mean_dbp < 90, 1, 0))

## Re-coding some of the variables for better readability
nhanes <- nhanes %>%
  mutate(
    health_insurance = case_when(
      HIQ011 == 1 ~ "Insured",
      HIQ011 == 2 ~ "Not insured",
      TRUE ~ NA_character_
    ),
    age = RIDAGEYR,
    gender = factor(RIAGENDR, levels = c(1,2), labels = c("Male","Female")),
    smoker = case_when(
      SMQ020 == 1 ~ "Current/Former",
      SMQ020 == 2 ~ "Never",
      TRUE ~ NA_character_
    ),
    medication = case_when(
      BPQ050A == 1 ~ "Taking Medication",
      BPQ050A == 2 ~ "Not taking Medication",
      TRUE ~ NA_character_
    ),
    physically_active = case_when(
      PAQ650 == 1 | PAQ665 == 1 ~ "Active",
      PAQ650 == 2 & PAQ665 == 2 ~ "Inactive",
      TRUE ~ NA_character_
    ),
    diabetes = case_when(
      DIQ010 == 1 ~ "Diabetic",
      DIQ010 == 2 ~ "Non-Diabetic",
      TRUE ~ NA_character_
    ),
    education = DMDEDUC2,
    income_ratio = INDFMPIR       # The ratio of a household’s income to the federal 
    # poverty level, adjusted for family size and composition.
  )
nhanes <- nhanes %>% filter ( education <= 5 ) 
nhanes$bp_controlled <- factor(nhanes$bp_controlled, 
                     levels = c(0, 1), 
                     labels = c("Uncontrolled", "Controlled"))
nhanes <- nhanes %>% select(bp_controlled, health_insurance, age, gender, physically_active, smoker, medication, diabetes, education, income_ratio)
## Statistical analysis
dim(nhanes)
summary(nhanes)
data <- nhanes

## Handle Missing Value
# Numeric columns -> median
num_cols <- names(which(sapply(data, is.numeric)))
for (col in num_cols) {
  data[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
}
# Categorical columns -> mode
cat_cols <- names(which(sapply(data, is.character)))
for (col in cat_cols) {
  mode_val <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
  data[[col]][is.na(data[[col]])] <- mode_val
}
# Convert character variables to factors
data[, (cat_cols) := lapply(.SD, as.factor), .SDcols = cat_cols]
data$duration_htn

## analysis
vars <- c(
  "health_insurance",
  "age",
  "gender",
  "physically_active",
  "smoker",
  "medication",
  "diabetes",
  "education",
  "income_ratio"
)
colnames(data)

table1 <- CreateTableOne(
  vars = vars,
  strata = "bp_controlled",
  data = data
)
print(table1)

## Before model analysis

## Create theme
bp_colors <- c("Controlled" = "#00BFC4", "Uncontrolled" = "#F8766D")
theme_nhanes <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, margin = margin(b = 6)),
    plot.subtitle    = element_text(size = 11, color = "grey50", margin = margin(b = 10)),
    axis.title       = element_text(size = 11),
    legend.position  = "top",
    legend.title     = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold")
  )


## Overall BP Distribution
overall_bp_dist <- data %>%
  filter(!is.na(bp_controlled)) %>%
  count(bp_controlled) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = bp_controlled, y = pct, fill = bp_controlled)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(pct * 100, 1), "%")),
            vjust = -0.5, fontface = "bold", size = 4.5) +
  scale_fill_manual(values = bp_colors) +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Overall BP Control Distribution",
       x = NULL, y = "Proportion") +
  theme_nhanes
overall_bp_dist

# Age Distribution by BP Control
age_dist <- ggplot(data = data %>% filter(!is.na(bp_controlled)), aes(x = age, fill = bp_controlled)) +
  geom_density(alpha = 0.6, color = NA) +
  scale_fill_manual(values = bp_colors) +
  labs(title = "Age Distribution by BP Control",
       x = "Age (years)", y = "Density") +
  theme_nhanes
age_dist

diabetes_dist <- data %>%
  filter(!is.na(diabetes), !is.na(bp_controlled)) %>%
  count(bp_controlled, diabetes) %>%
  group_by(bp_controlled) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = bp_controlled, y = pct, fill = diabetes)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", pct * 100)),
            position = position_stack(vjust = 0.5),
            color = "white",
            fontface = "bold",
            size = 4) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Diabetes Status by Blood Pressure Control",
    x = "Blood Pressure Status",
    y = "Percentage"
  ) +
  theme_nhanes
diabetes_dist

# Health Insurance distribution by BP Control
health_insurance_dist <- data %>%
  filter(!is.na(health_insurance), !is.na(bp_controlled)) %>%
  count(bp_controlled, health_insurance) %>%
  group_by(bp_controlled) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = bp_controlled, y = pct, fill = health_insurance)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", pct * 100)),
            position = position_stack(vjust = 0.5),
            color = "white",
            fontface = "bold",
            size = 4) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Health Insurance by Blood Pressure Control",
    x = "Blood Pressure Status",
    y = "Percentage"
  ) +
  theme_nhanes
health_insurance_dist

# Gender distribution by BP Control
gender_dist <- data %>%
  filter(!is.na(gender), !is.na(bp_controlled)) %>%
  count(bp_controlled, gender) %>%
  group_by(bp_controlled) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = bp_controlled, y = pct, fill = gender)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", pct * 100)),
            position = position_stack(vjust = 0.5),
            color = "white",
            fontface = "bold",
            size = 4) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Gender by Blood Pressure Control",
    x = "Blood Pressure Status",
    y = "Percentage"
  ) +
  theme_nhanes
gender_dist

# Income Ratio by BP Control
income_ratio_dist <- ggplot(data, aes(x = bp_controlled, y = income_ratio, fill = bp_controlled)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = NA) +
  geom_boxplot(width = 0.15, fill = "white", outlier.shape = 21,
               outlier.size = 1.5, outlier.alpha = 0.4) +
  scale_fill_manual(values = bp_colors) +
  labs(title = "Income-to-Poverty Ratio by BP Control",
       x = NULL, y = "Income Ratio") +
  theme_nhanes + theme(legend.position = "none")
income_ratio_dist

# Income Ratio Distribution by BP Control
income_ratio_dist <- ggplot(data = data %>% filter(!is.na(bp_controlled)), aes(x = income_ratio, fill = bp_controlled)) +
  geom_density(alpha = 0.6, color = NA) +
  scale_fill_manual(values = bp_colors) +
  labs(title = "Income-to-Poverty Ratio by BP Control",
       x = "Income-to-poverty Ratio", y = "Density") +
  theme_nhanes
income_ratio_dist

# Medication Status vs BP Control (stacked bar) 
medication_dist <- data %>%
  filter(!is.na(medication), !is.na(bp_controlled)) %>%
  count(bp_controlled, medication) %>%
  group_by(bp_controlled) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = bp_controlled, y = pct, fill = medication)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", pct * 100)),
            position = position_stack(vjust = 0.5),
            color = "white",
            fontface = "bold",
            size = 4) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Medication Status by Blood Pressure Control",
    x = "Blood Pressure Status",
    y = "Percentage"
  ) +
  theme_nhanes
medication_dist


# physical_activity distribution by BP Control
physically_active_dist <- data %>%
  filter(!is.na(physically_active), !is.na(bp_controlled)) %>%
  count(bp_controlled, physically_active) %>%
  group_by(bp_controlled) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = bp_controlled, y = pct, fill = physically_active)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", pct * 100)),
            position = position_stack(vjust = 0.5),
            color = "white",
            fontface = "bold",
            size = 4) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Physical Activity by Blood Pressure Control",
    x = "Blood Pressure Status",
    y = "Percentage"
  ) +
  theme_nhanes
physically_active_dist

# Smoking Status vs BP Control 
smoking_dist <- data %>%
  filter(!is.na(smoker), !is.na(bp_controlled)) %>%
  count(bp_controlled, smoker) %>%
  group_by(bp_controlled) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = bp_controlled, y = pct, fill = smoker)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", pct * 100)),
            position = position_stack(vjust = 0.5),
            color = "white",
            fontface = "bold",
            size = 4) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Smoking Status by Blood Pressure Control",
    x = "Blood Pressure Status",
    y = "Percentage"
  ) +
  theme_nhanes
smoking_dist

# Education Level vs BP Control
education_dist <- data %>%
  filter(!is.na(education), !is.na(bp_controlled)) %>%
  count(bp_controlled, education) %>%
  group_by(bp_controlled) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = bp_controlled, y = pct, fill = education)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", pct * 100)),
            position = position_stack(vjust = 0.5),
            color = "white",
            fontface = "bold",
            size = 4) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Education Status by Blood Pressure Control",
    x = "Blood Pressure Status",
    y = "Percentage"
  ) +
  theme_nhanes
education_dist

# Age × income_ratio scatter colored by BP Control
age_income_ratio_dist <- ggplot(data = data %>% filter(!is.na(bp_controlled)), aes(x = age, y = income_ratio, color = bp_controlled)) +
  geom_point(alpha = 0.25, size = 1.2) +
  geom_smooth(method = "loess", se = TRUE, linewidth = 1.2) +
  scale_color_manual(values = bp_colors) +
  labs(title = "Age vs Income-to-Poverty Ratio by BP Control",
       x = "Age (years)", y = "Income Ratio") +
  theme_nhanes
age_income_ratio_dist

# Print EDA plots
grid.arrange(gender_dist, health_insurance_dist, ncol = 2)
grid.arrange(smoking_dist, physically_active_dist, ncol = 2)
grid.arrange(medication_dist, diabetes_dist, ncol = 2)

## Logistic regression
log_model <- glm( bp_controlled ~ ., family = binomial(), data = data)
summary(log_model)

## Odd ratio
or_table <- exp(cbind(OR = coef(log_model), confint(log_model)))
print(round(or_table, 3))


## Odd Ratios from logistic regression
or_df <- as.data.frame(or_table) %>%
  tibble::rownames_to_column("term") %>%
  filter(term != "(Intercept)") %>%
  rename(lower = `2.5 %`, upper = `97.5 %`)

p_or <- ggplot(or_df, aes(x = reorder(term, OR), y = OR)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25,
                color = "grey40", linewidth = 0.8) +
  geom_point(aes(color = OR > 1), size = 3.5) +
  scale_color_manual(values = c("TRUE" = "#1D9E75", "FALSE" = "#D85A30"),
                     labels = c("TRUE" = "Increases control", "FALSE" = "Decreases control")) +
  coord_flip() +
  labs(title = "Odds Ratios from Logistic Regression",
       subtitle = "Points right of 1 increase the odds of BP being controlled",
       x = NULL, y = "Odds Ratio (log scale)", color = NULL) +
  scale_y_log10() +
  theme_nhanes
print(p_or)


