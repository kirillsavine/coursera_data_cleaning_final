

# cd into my working directory that contains the data
setwd('C:/Users/kis/Documents/UCI HAR Dataset/');

# By inspecting the text files unziopped, looks like the data does not have headers, 
# the first line of text file is the first row of data table, therefore need to say `header=FALSE`

d_features = read.table('features.txt',header=FALSE) 
d_activity_labels = read.table('activity_labels.txt',header=FALSE)

# read the train data
d_x_train = read.table('train/x_train.txt',header=FALSE) 
d_y_train = read.table('train/y_train.txt',header=FALSE) 
d_subject_train = read.table('train/subject_train.txt',header=FALSE) 


# read the test data
d_subject_test = read.table('test/subject_test.txt',header=FALSE)
d_x_test       = read.table('test/x_test.txt',header=FALSE)
d_y_test       = read.table('test/y_test.txt',header=FALSE)

head(d_x_train)
head(d_y_train)
head(d_features)

# name the columns
names(d_features)=c("featureId","featureType")

names(d_x_train) = d_features$featureType 
names(d_y_train) = "activityId"

names(d_x_test)  = d_features$featureType 
names(d_y_test) = "activityId"

names(d_subject_train) = "subjectId"
names(d_subject_test) = "subjectId"

names(d_activity_labels) = c('activityId','activityType')


# Task 1: Merges the training and the test sets to create one data set.

# column-bind DFs together
d_train = cbind(d_y_train,d_subject_train,d_x_train)
d_test = cbind(d_y_test,d_subject_test,d_x_test)


head(d_train)
head(d_test)
# row-bind test and train data
d = rbind(d_train,d_test)

# Create a vector for the column names from the finalData, which will be used
# to select the desired mean() & stddev() columns
col_names= colnames(d); 

# Task 2: Extracts only the measurements on the mean and standard deviation for each measurement.

# identify veriables containing ids along with means adn SDs
col_names_mean_and_sds=
	grepl("activityId",col_names) |
	grepl("subjectId",col_names) |
	(grepl("-mean",col_names) & !grepl("-meanFreq",col_names) & !grepl("mean..-",col_names)) |
	(grepl("-std",col_names) & !grepl("-std()..-",col_names))

table(col_names_mean_and_sds)

d_mean_sd = d[,col_names_mean_and_sds];

# Task 3: Uses descriptive activity names to name the activities in the data set

# use merge() function to lookup activity name by its id in the d_activity_labels DF

d_mean_sd = merge(d_mean_sd,d_activity_labels,by='activityId',all.x=TRUE);
head(d_mean_sd)	


# 4. Appropriately label the data set with descriptive activity names. 

col_names = names(d_mean_sd); 

for (i in 1:length(col_names)) {
  col_names[i] = gsub("\\(\\)","",col_names[i]) # remove brackets
  col_names[i] = gsub("-std$","StdDev",col_names[i]) # rename standard deviation 
  col_names[i] = gsub("-mean","Mean",col_names[i]) # reanme mean
  col_names[i] = gsub("^(t)","time",col_names[i]) # indicate time
  col_names[i] = gsub("^(f)","freq",col_names[i]) # indicate frequency 
  col_names[i] = gsub("([Gg]ravity)","Gravity",col_names[i]) # Capitalize 
  col_names[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",col_names[i]) # Capitalize 
  col_names[i] = gsub("[Gg]yro","Gyro",col_names[i]) # Capitalize 
  col_names[i] = gsub("AccMag","AccMagnitude",col_names[i]) # Capitalize 
  col_names[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",col_names[i]) # Capitalize 
  col_names[i] = gsub("JerkMag","JerkMagnitude",col_names[i]) # Capitalize 
  col_names[i] = gsub("GyroMag","GyroMagnitude",col_names[i]) # Capitalize 
};


names(d_mean_sd) = col_names;

# Task 5: From the data set in step 4, creates a second, 
# independent tidy data set with the average of each variable for each activity and each subject.

library(data.table)
d_td=as.data.frame(data.table(d_mean_sd[,names(d_mean_sd)[names(d_mean_sd)!="activityType"]])[,lapply(.SD,mean),by=.(activityId,subjectId)])

# lookup acitivy name again by activityId
d_td    = merge(d_td,d_activity_labels,by='activityId',all.x=TRUE)
head(d_td)
# write data to disk
write.table(d_td, 'tidy_data.txt',row.names=FALSE,sep='\t')

