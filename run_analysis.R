#- 'train/X_train.txt': Training set.
#- 'train/y_train.txt': Training labels.
#- 'test/X_test.txt': Test set.
#- 'test/y_test.txt': Test labels.

#STEP1:Merges the training and the test sets to create one data set.
#extract the feature
label_feature<-read.table("./features.txt",stringsAsFactors = FALSE)

#extract the data
data_train<-read.table("./train/X_train.txt")
data_test<-read.table("./test/X_test.txt")
data<-rbind(data_train,data_test)
names(data)<-label_feature[,2]

#extract the subject
subject_train<-read.table("./train/subject_train.txt")
subject_test<-read.table("./test/subject_test.txt")
subject<-rbind(subject_train,subject_test)
names(subject)<-"subject"

#extract the label
label_train<-read.table("./train/y_train.txt")
label_test<-read.table("./test/y_test.txt")
label<-rbind(label_train,label_test)
names(label)<-"activityNumber"


#Combine all the data into one data set
data_all<-cbind(label,subject,data)

#STEP2: Extracts only the measurements on the mean and standard deviation for each measurement. 
#seek for the feature which is mean or std
Indices<-grep("mean\\(\\)|std\\(\\)", label_feature[, 2])
#subset the data by these features
data_meanORstd<-data_all[,Indices+2]
data_meanORstd<-cbind(label,subject,data_meanORstd)

#STEP3: Uses descriptive activity names to name the activities in the data set
label_activity<-read.table("activity_labels.txt")
names(label_activity)<-c("activity.Number","activity.Name")
data_meanORstd<-merge(label_activity,data_meanORstd,by.y = "activityNumber",by.x = "activity.Number")


#STEP4:Appropriately labels the data set with descriptive variable names. 
names(data_meanORstd)<-gsub("\\(\\)","",names(data_meanORstd))
names(data_meanORstd)<-gsub("mean","Mean",names(data_meanORstd))
names(data_meanORstd)<-gsub("std","Std",names(data_meanORstd))

#STEP5:From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
data_output<-aggregate(data_meanORstd, by=list(subject = data_meanORstd$subject, activity=data_meanORstd$activity.Name), FUN=mean, na.rm=TRUE)
data_output<-data_output[,-c(3,4,5)]
write.table(data_output, "data_output.txt",row.name=FALSE)