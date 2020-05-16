#####  Using Nearest Neighbors --------------------

# import the CSV file
wbcd <- read.csv("Pokemon_GO.csv", stringsAsFactors = FALSE)

# examine the structure of the wbcd data frame
str(wbcd)

# drop the id feature
wbcd <- wbcd[-1]

# table of diagnosis
table(wbcd$Legendary)

# recode diagnosis as a factor
wbcd$Legendary <- factor(wbcd$Legendary, levels = c(0, 1),
                         labels = c("Non-Legendary", "Legendary"))

# table or proportions with more informative labels
round(prop.table(table(wbcd$Legendary)) * 100, digits = 1)

# summarize three numeric features
#summary(wbcd[c("Attack", "Defence")])

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# test normalization function - result should be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

# normalize the wbcd data
wbcd_n <- as.data.frame(lapply(wbcd[4:11], normalize))

# confirm that normalization worked
##summary(wbcd_n$area_mean)

# create training and test data
wbcd_train <- wbcd_n[1:600, ]
wbcd_test <- wbcd_n[601:800, ]

# create labels for training and test data

wbcd_train_labels <- wbcd[1:600, 12]
wbcd_test_labels <- wbcd[601:800, 12]

## Step 3: Training a model on the data ----

# load the "class" library
library(class)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)



## Step 4: Evaluating model performance ----

# load the "gmodels" library
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq = FALSE)

## Step 5: Improving model performance ----

# use the scale() function to z-score standardize a data frame
wbcd_z <- as.data.frame(scale(wbcd[-1]))

# confirm that the transformation was applied correctly
#summary(wbcd_z$area_mean)

# create training and test datasets
wbcd_train <- wbcd_z[1:600, ]
wbcd_test <- wbcd_z[601:800, ]

# re-classify test cases
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq = FALSE)

# try several different values of k
wbcd_train <- wbcd_n[1:600, ]
wbcd_test <- wbcd_n[601:800, ]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=2)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=10)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=20)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=27)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=50)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

