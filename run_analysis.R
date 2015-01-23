run_analysis <- function() {
# reading of the files
y_train = read.table('./UCI HAR Dataset/train/y_train.txt')
x_train = read.table('./UCI HAR Dataset/train/X_train.txt')
y_test = read.table('./UCI HAR Dataset/test/y_test.txt')
x_test = read.table('./UCI HAR Dataset/test/X_test.txt')
subjects_train = read.table('./UCI HAR Dataset/train/subject_train.txt')
subjects_test = read.table('./UCI HAR Dataset/test/subject_test.txt')
activity_labels = read.table('./UCI HAR Dataset/activity_labels.txt')
features = read.table('./UCI HAR Dataset/features.txt')

# organizing the train data
names(y_train) = 'Activity'
y_train$Activity <-factor(y_train$Activity)
levels(y_train$Activity) <- activity_labels$V2
train <- data.frame(subjects = subjects_train$V1, Activity = y_train$Activity)
names(x_train) <- features$V2
group = rep(1, nrow(train))
train$group <- group
train = cbind(train, x_train)

# organizing the test data
names(y_test) = 'Activity'
y_test$Activity <-factor(y_test$Activity)
levels(y_test$Activity) <- activity_labels$V2
test <- data.frame(subjects = subjects_test$V1, Activity = y_test$Activity)
names(x_test) <- features$V2
group = rep(2, nrow(test))
test$group <- group
test = cbind(test, x_test)

alldata = rbind(train, test)
alldata$group <- factor(alldata$group)
levels(alldata$group) <- c('train', 'test')

# take only the mean and std measurment
means_index <- grep('mean', features$V2) 
stds_index <- grep('std', features$V2) 
index <- sort(c(means_index, stds_index)) + 3
index <- c(1,2,3,index)
data_mean_std <- alldata[,index]

# calculating the mean
nr <- ncol(data_mean_std)
empty <- data_mean_std[1,]
average_data <- empty[-1,-3]

for (s in 1:length(unique(data_mean_std$subjects))){
    tmp1 <- subset(data_mean_std, data_mean_std$subjects == s)
      for(a in 1:length(unique(tmp1$Activity))){
        A <- activity_labels[a,2]
        tmp2 <- subset(tmp1, tmp1$Activity == A)
        means <- colMeans(tmp2[,4:nr])
        tmp_data <- data.frame(Subject_Number=s,Activity = A,t(means))
        average_data =  rbind(average_data, tmp_data) 
      }
  }

write.table(average_data, 'average_data.txt', row.name=FALSE)

average_data

}
