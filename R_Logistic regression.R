library(ggplot2)
library(cowplot)

url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"

data <- read.csv(url, header=FALSE)
head(data) 

colnames(data) <- c(
  "age",
  "sex",# 0 = female, 1 = male
  "cp", # chest pain
  # 1 = typical angina,
  # 2 = atypical angina,
  # 3 = non-anginal pain,
  # 4 = asymptomatic
  "trestbps", # resting blood pressure (in mm Hg)
  "chol", # serum cholestoral in mg/dl
  "fbs",  # fasting blood sugar if less than 120 mg/dl, 1 = TRUE, 0 = FALSE
  "restecg", # resting electrocardiographic results
  # 1 = normal
  # 2 = having ST-T wave abnormality
  # 3 = showing probable or definite left ventricular hypertrophy
  "thalach", # maximum heart rate achieved
  "exang",   # exercise induced angina, 1 = yes, 0 = no
  "oldpeak", # ST depression induced by exercise relative to rest
  "slope", # the slope of the peak exercise ST segment
  # 1 = upsloping
  # 2 = flat
  # 3 = downsloping
  "ca", # number of major vessels (0-3) colored by fluoroscopy
  "thal", # this is short of thalium heart scan
  # 3 = normal (no cold spots)
  # 6 = fixed defect (cold spots during rest and exercise)
  # 7 = reversible defect (when cold spots only appear during exercise)
  "hd" # (the predicted attribute) - diagnosis of heart disease
  # 0 if less than or equal to 50% diameter narrowing
  # 1 if greater than 50% diameter narrowing
)

str(data)
head(data)
summary(data)

#SEX
unique(data$sex)
# data$sex <- ifelse(data$sex ==0, "F", "M")
data[data$sex==0,]$sex <- "F"
data[data$sex==1,]$sex <- "M"
data$sex <- as.factor(data$sex)
summary(data)

#CP: Chest Pain
data$cp <- as.factor(data$cp)
summary(data)

#fbs: if less than 120 mg/dl, 1 = TRUE, 0 = FALSE
data$fbs <- as.factor(data$fbs)

#"trestbps", resting blood pressure (in mm Hg)
#chol: Cholesterol 
#fbs: fasting blood suger
#thalach: maximum heart rate achieved
# Old peak : ST depression induced by exercise relative to rest
typeof(data$trestbps)
typeof(data$chol)
typeof(data$fbs)
typeof(data$thalach)
typeof(data$oldpeak)

#restecg: resting ecg 
unique(data$restecg)
data$restecg <- as.factor(data$restecg)
summary(data)

#exercise induced angina, 1 = yes, 0 = no
unique(data$exang)
data$exang <- as.factor(data$exang)

#slope, the slope of the peak exercise ST segment
data$slope <- as.factor(data$slope)

#ca, number of major vessels (0-3) colored by fluoroscopy
unique(data$ca)
data$ca[data$ca == "?"] <- NA
unique(data$ca)
data$ca <- as.factor(data$ca)
unique(data$ca)

# this is short of thalium heart scan
# 3 = normal (no cold spots)
# 6 = fixed defect (cold spots during rest and exercise)
# 7 = reversible defect (when cold spots only appear during exercise)
unique(data$thal)
data$thal[data$thal == "?"] <- NA
data$thal <- as.factor(data$thal)

#hd, (the predicted attribute) - diagnosis of heart disease
# 0 if less than or equal to 50% diameter narrowing
# 1 if greater than 50% diameter narrowing
unique(data$hd)
data$hd <- ifelse(data$hd == 0, "Healthy", "Unhealthy")
data$hd <- as.factor(data$hd)

summary(data)

# Dealing with NAs 
summary(data)
# ca / thal 
nrow(data[is.na(data$ca)|is.na(data$thal),])
nrow(data)
# Delete the NA values 
data <- data[(!is.na(data$ca))&(!is.na(data$thal)),]
nrow(data)

# Quality control 
xtabs(~ hd+sex, data)
xtabs(~ hd+cp, data)
xtabs(~ hd+fbs, data)
xtabs(~ hd+restecg, data)
xtabs(~ hd+exang, data)
xtabs(~ hd+slope, data)
xtabs(~ hd+ca, data)
xtabs(~ hd+thal, data)

## NOTE: We also want to exclude variables that only have 1 or 2 samples in
## a category since +/- one or two samples can have a large effect on the
## odds/log(odds)

xtabs(~ hd+sex, data)
simple_logistic <- glm(hd ~ sex, data=data, family="binomial")
summary(simple_logistic)

predicted.data <- data.frame(probability_of_hd = simple_logistic$fitted.values,
                             sex= data$sex)

ggplot(predicted.data, aes(x=sex, y=probability_of_hd)) +
  geom_point(aes(color=sex), size=5)

xtabs(~probability_of_hd+sex, predicted.data)

multiple_logistic <- glm(hd ~ ., data=data, family="binomial")
summary(multiple_logistic)

final_predicted.data <- data.frame(probability_of_hd = 
                                     multiple_logistic$fitted.values,
                                   hd = data$hd)
final_predicted.data <- final_predicted.data[order(
  final_predicted.data$probability_of_hd, decreasing = FALSE),]
final_predicted.data$rank <- 1:nrow(final_predicted.data)

ggplot(final_predicted.data, aes(x=rank, y=probability_of_hd)) +
  geom_point(aes(color=hd), alpha=0.4, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of getting heart disease")
