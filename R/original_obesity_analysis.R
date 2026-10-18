rm(list = ls())

library(lawstat)
library(car)
library(caret)
library(pROC)


# 1. Import and inspect data

obesity_data = read.csv("data/obesity_cleaned.csv")

print(dim(obesity_data))

str(obesity_data)

print(colSums(is.na(obesity_data)))

print(table(obesity_data$obesity))

print(nrow(obesity_data))

print(table(obesity_data$obesity))


# 2. Encoding

obesity_data$obesity_f = factor(
  obesity_data$obesity,
  levels = c(0, 1),
  labels = c("Non-Obese", "Obese")
)

obesity_data$gender_f = factor(
  obesity_data$gender,
  levels = c(1, 0),
  labels = c("Male", "Female")
)

obesity_data$family_history_f = factor(
  obesity_data$family_history,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

obesity_data$high_calorie_f = factor(
  obesity_data$high_calorie,
  levels = c(0, 1),
  labels = c("Low", "High")
)

obesity_data$smoke_f = factor(
  obesity_data$smoke,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

obesity_data$physical_activity_f = factor(
  obesity_data$physical_activity,
  levels = c(0, 1, 2, 3),
  labels = c("Never", "Once", "Twice", "Thrice")
)

obesity_data$alcohol_f = factor(
  obesity_data$alcohol,
  levels = c(0, 1, 2),
  labels = c("Never", "Sometimes", "Frequently")
)


# 3. Boxplots of height and weight

boxplot(
  obesity_data$height,
  main = "Boxplot of Height",
  ylab = "Height (metres)"
)

boxplot(
  obesity_data$weight,
  main = "Boxplot of Weight",
  ylab = "Weight (kg)"
)


# 4. Stacked bar charts

plot_stacked_percent = function(data, predictor, predictor_label) {
  tab = table(data$obesity_f, data[[predictor]])
  pct = prop.table(tab, margin = 1) * 100
  
  barplot(
    pct,
    beside = FALSE,
    col = grDevices::rainbow(ncol(pct)),
    main = paste("Stacked Bar Chart: Obesity and", predictor_label),
    xlab = "Obesity status",
    ylab = "Percent within obesity status",
    legend.text = colnames(pct),
    args.legend = list(x = "topright", cex = 0.7, bty = "n")
  )
}

plot_stacked_percent(obesity_data, "alcohol_f", "Alcohol Consumption")
plot_stacked_percent(obesity_data, "physical_activity_f", "Physical Activity")
plot_stacked_percent(obesity_data, "smoke_f", "Smoking Status")
plot_stacked_percent(obesity_data, "family_history_f", "Family History")
plot_stacked_percent(obesity_data, "high_calorie_f", "Calorie Intake")
plot_stacked_percent(obesity_data, "gender_f", "Gender")


# 5. Yule's Q for 2 x 2 tables

# Yule's Q = (ad - bc) / (ad + bc)

# Gender
gender_tab = table(
  obesity_data$obesity_f,
  factor(
    obesity_data$gender,
    levels = c(0, 1),
    labels = c("Female", "Male")
  )
)

print(gender_tab)

a = gender_tab[1, 1]
b = gender_tab[1, 2]
c = gender_tab[2, 1]
d = gender_tab[2, 2]

Q_gender = (a * d - b * c) / (a * d + b * c)
print(Q_gender)


# Family history
family_tab = table(
  obesity_data$obesity_f,
  obesity_data$family_history_f
)

print(family_tab)

a = family_tab[1, 1]
b = family_tab[1, 2]
c = family_tab[2, 1]
d = family_tab[2, 2]

Q_family = (a * d - b * c) / (a * d + b * c)
print(Q_family)


# Dietary habit
diet_tab = table(
  obesity_data$obesity_f,
  obesity_data$high_calorie_f
)

print(diet_tab)

a = diet_tab[1, 1]
b = diet_tab[1, 2]
c = diet_tab[2, 1]
d = diet_tab[2, 2]

Q_diet = (a * d - b * c) / (a * d + b * c)
print(Q_diet)


# Smoking
smoke_tab = table(
  obesity_data$obesity_f,
  obesity_data$smoke_f
)

print(smoke_tab)

a = smoke_tab[1, 1]
b = smoke_tab[1, 2]
c = smoke_tab[2, 1]
d = smoke_tab[2, 2]

Q_smoke = (a * d - b * c) / (a * d + b * c)
print(Q_smoke)


# 6. Chi-square tests of independence

alcohol_tab = table(
  obesity_data$obesity_f,
  obesity_data$alcohol_f
)

activity_tab = table(
  obesity_data$obesity_f,
  obesity_data$physical_activity_f
)

print(alcohol_tab)
print(chisq.test(alcohol_tab, correct = FALSE))

print(activity_tab)
print(chisq.test(activity_tab, correct = FALSE))


# 7. Bootstrap symmetry tests

print(lawstat::symmetry.test(obesity_data$weight, boot = TRUE))

print(lawstat::symmetry.test(obesity_data$height, boot = TRUE))


# 8. Proportion test: age categories

obesity_data$age_group = ifelse(
  obesity_data$age < 30,
  "Below 30",
  "30 and above"
)

age_obesity_table = table(
  obesity_data$age_group,
  obesity_data$obesity
)

print(age_obesity_table)

age_group_counts = table(obesity_data$age_group)

age_obese_counts = tapply(
  obesity_data$obesity,
  obesity_data$age_group,
  sum
)

age_group_order = c("30 and above", "Below 30")

age_n = as.numeric(age_group_counts[age_group_order])
age_obese = as.numeric(age_obese_counts[age_group_order])

print(prop.test(x = age_obese, n = age_n, correct = FALSE))


# 9. Logistic regression and VIF

model_data = obesity_data

model_data$gender = factor(
  model_data$gender,
  levels = c(1, 0),
  labels = c("Male", "Female")
)

model_data$family_history = factor(
  model_data$family_history,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

model_data$high_calorie = factor(
  model_data$high_calorie,
  levels = c(0, 1),
  labels = c("Low", "High")
)

model_data$smoke = factor(
  model_data$smoke,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

model_data$alcohol = factor(
  model_data$alcohol,
  levels = c(0, 1, 2),
  labels = c("Never", "Sometimes", "Frequently")
)

model_data$physical_activity = factor(
  model_data$physical_activity,
  levels = c(0, 1, 2, 3),
  labels = c("Never", "Once", "Twice", "Thrice")
)

model_initial = glm(
  obesity ~ gender + age + alcohol + physical_activity +
    family_history + high_calorie + smoke + weight + height,
  data = model_data,
  family = binomial(link = "logit")
)

summary(model_initial)

car::vif(model_initial)


# Final model excluding height

model_final = glm(
  obesity ~ gender + age + alcohol + physical_activity +
    family_history + high_calorie + smoke + weight,
  data = model_data,
  family = binomial(link = "logit")
)

summary(model_final)

car::vif(model_final)


# 10. Choose cutoff and confusion matrix

predicted_probability = predict(model_final, type = "response")

actual_class = factor(
  model_data$obesity,
  levels = c(0, 1),
  labels = c("Non-Obese", "Obese")
)

thresholds = sort(unique(c(0, predicted_probability, 1)))

threshold_results = data.frame(
  threshold = thresholds,
  TPR = NA_real_,
  FPR = NA_real_,
  objective = NA_real_
)

for (i in seq_along(thresholds)) {
  
  predicted_class = ifelse(
    predicted_probability >= thresholds[i],
    1,
    0
  )
  
  tp = sum(predicted_class == 1 & model_data$obesity == 1)
  fn = sum(predicted_class == 0 & model_data$obesity == 1)
  fp = sum(predicted_class == 1 & model_data$obesity == 0)
  tn = sum(predicted_class == 0 & model_data$obesity == 0)
  
  tpr = if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
  fpr = if ((fp + tn) > 0) fp / (fp + tn) else NA_real_
  
  threshold_results$TPR[i] = tpr
  threshold_results$FPR[i] = fpr
  threshold_results$objective[i] = tpr * (1 - fpr)
}

best_index = which.max(threshold_results$objective)

optimal_threshold = threshold_results$threshold[best_index]

print(optimal_threshold)

chosen_threshold = optimal_threshold

predicted_class = ifelse(
  predicted_probability >= chosen_threshold,
  1,
  0
)

predicted_factor = factor(
  predicted_class,
  levels = c(0, 1),
  labels = c("Non-Obese", "Obese")
)

caret::confusionMatrix(
  data = predicted_factor,
  reference = actual_class,
  positive = "Obese"
)


# 11. ROC curve and AUC

roc_object = pROC::roc(
  response = model_data$obesity,
  predictor = predicted_probability,
  levels = c(0, 1),
  direction = "<",
  quiet = TRUE
)

pROC::auc(roc_object)

plot(
  roc_object,
  main = "ROC Curve for the Logistic Regression Model",
  col = "blue",
  lwd = 2
)

abline(a = 0, b = 1, lty = 2, col = "gray")
