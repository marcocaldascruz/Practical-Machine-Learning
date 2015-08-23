# Practical-Machine-Learning
Practical Machine Learning - Project Course

# Step 1. Building and Getting the Data.
We download the two datasets
<pre>
downloadcsv <- function(url, nastrings) {
    temp <- tempfile()
    download.file(url, temp, method = "curl")
    data <- read.csv(temp, na.strings = nastrings)
    unlink(temp)
    return(data)
}

trainurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
train <- downloadcsv(trainurl, c("", "NA", "#DIV/0!"))

testurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
test <- downloadcsv(testurl, c("", "NA", "#DIV/0!"))
</pre>

Show the summary of the train data:
<pre>
> summary(train$classe)
   A    B    C    D    E 
5580 3797 3422 3216 3607 
</pre>

# Step 2. Preprocessing
Partition the data, make the train set and validation set.

<pre>
library(caret)
set.seed(123456)
trainset <- createDataPartition(train$classe, p = 0.8, list = FALSE)
Training <- train[trainset, ]
Validation <- train[-trainset, ]
</pre>
Clean the missing values.
<pre>
# exclude near zero variance features
nzvcol <- nearZeroVar(Training)
Training <- Training[, -nzvcol]

# exclude columns with missing values exclude descriptive
# columns like name etc
cntlength <- sapply(Training, function(x) {
    sum(!(is.na(x) | x == ""))
})
nullcol <- names(cntlength[cntlength < 0.6 * length(Training$classe)])
descriptcol <- c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", 
    "cvtd_timestamp", "new_window", "num_window")
excludecols <- c(descriptcol, nullcol)
Training <- Training[, !names(Training) %in% excludecols]
</pre>

# Step 3. Training and Validate.
Use the ramndom Forest to compute the model.

<pre>
> library(randomForest)
> rfModel <- randomForest(classe ~ ., data = Training, importance = TRUE, ntrees = 10)
</pre>

With the Training Set
<pre>
> ptraining <- predict(rfModel, Training)
> print(confusionMatrix(ptraining, Training$classe))

Confusion Matrix and Statistics

          Reference
Prediction    A    B    C    D    E
         A 4464    0    0    0    0
         B    0 3038    0    0    0
         C    0    0 2738    0    0
         D    0    0    0 2573    0
         E    0    0    0    0 2886

Overall Statistics
                                     
               Accuracy : 1          
                 95% CI : (0.9998, 1)
    No Information Rate : 0.2843     
    P-Value [Acc > NIR] : < 2.2e-16  
                                     
                  Kappa : 1          
 Mcnemar's Test P-Value : NA         

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E
Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
Prevalence             0.2843   0.1935   0.1744   0.1639   0.1838
Detection Rate         0.2843   0.1935   0.1744   0.1639   0.1838
Detection Prevalence   0.2843   0.1935   0.1744   0.1639   0.1838
Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000
</pre>

With the Validation Set
<pre>
> pvalidation <- predict(rfModel, Validation)
> print(confusionMatrix(pvalidation, Validation$classe))

Confusion Matrix and Statistics

          Reference
Prediction    A    B    C    D    E
         A 1116    7    0    0    0
         B    0  751    4    0    0
         C    0    1  680    4    0
         D    0    0    0  639    4
         E    0    0    0    0  717

Overall Statistics
                                          
               Accuracy : 0.9949          
                 95% CI : (0.9921, 0.9969)
    No Information Rate : 0.2845          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.9936          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E
Sensitivity            1.0000   0.9895   0.9942   0.9938   0.9945
Specificity            0.9975   0.9987   0.9985   0.9988   1.0000
Pos Pred Value         0.9938   0.9947   0.9927   0.9938   1.0000
Neg Pred Value         1.0000   0.9975   0.9988   0.9988   0.9988
Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
Detection Rate         0.2845   0.1914   0.1733   0.1629   0.1828
Detection Prevalence   0.2863   0.1925   0.1746   0.1639   0.1828
Balanced Accuracy      0.9988   0.9941   0.9963   0.9963   0.9972
</pre>


# Step 4. The Prediction
Run the model with the Test Set.

<pre>
> ptest <- predict(rfModel, test)
> ptest

> predictions <- as.vector(ptest)
> predictions
 [1] "B" "A" "B" "A" "A" "E" "D" "B" "A" "A" "B" "C" "B" "A" "E" "E" "A" "B" "B" "B"
</pre>
