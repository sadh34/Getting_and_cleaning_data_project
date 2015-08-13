############## Sets the working directory and install needed packages

setwd("")   
install.packages("dplyr")
library(dplyr)


############## Reads the datasets needed 

subject_train <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")
x_train <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")
features <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/features.txt", stringsAsFactors = F)
subject_test <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")
x_test <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")
activity_label <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt", stringsAsFactors = F)




############## Gives names to datasets with results from dataset "features" 

names(x_test) <- features[,2]
names(x_train) <- features[,2]



############## Creates a vector named "logical" with the numbers of columns wich include means and standard deviations  
logical <- c()
j=1
for (i in 1:561){
        logical[j] <- ifelse(length(grep("mean", names(x_test)[i])) | length(grep("std", names(x_test)[i])), i,0)
        j = j+1
}
logical <- logical[(logical > 0)]


############# Extracts columns selected above
x_train <- x_train[,logical]
x_test <- x_test[,logical]



############ Binds subjects and results by column for train and test datasets
x_train <- cbind(subject_train, x_train)
x_test <- cbind(subject_test, x_test)


############ Binds numbers of activities from test and train datsets by rows
act_nr <- rbind(y_test, y_train)


############ Binds train and test datasets together 
x_all <- rbind(x_test, x_train)

############ Binds resulting dataset with activities names 
x_all <- cbind(act_nr, x_all)


############ Gives apprioprate names for this dataset
names <- names(x_all)
names[1] = "Activity_name"
names[2] = "Person"
names(x_all) <- names


########### Replaces the numbers of activitis with apprioprate names
for (i in 1:6){
        for (j in 1:10299){
        if(x_all[j,1] == i){
            x_all[j,1] <- activity_label[i,2]    
        }
}}


########## Creates a new variable witch returns different number for each combinations of subject and activity (30 * 6 = 180)
b=0
for(i in unique(x_all[,1])){
        for (j in unique(x_all[,2])){
                b = b + 1
                for(z in 1:10299){
                        if(x_all[z,1] == i & x_all[z,2] == j){
                                x_all[z,82] <- b
                        }                        
                }
        }
}


######### Creates a new data frame with the average of each variable for each activity and each subject
date <- as.data.frame(matrix(NA, ncol = 82, nrow = 30*6), stringsAsFactors = F)
names(date) <- names(x_all)
for(i in 2:81){
        date[,i] <- as.numeric(tapply(x_all[,i], x_all[,82], mean))
}

z = matrix(NA, ncol = 6, nrow = 30)
for(i in 1:6){
        z[,i] = rep(unique(x_all[,1])[i], 30)
}
date[,1] <- as.character(z)
date <- arrange(date, Person, Activity_name)


######### Create a vector with names for tidy dataset 
names = matrix(ncol=81,nrow = 6)
for (i in 1:6){
        for(j in 3:81){
               names[i,j] = paste(names(date[j]), date[i,1], sep="_") 
        }
}
names <- names[,3:81]
names <- c("Person", names[1,], names[2,], names[3,], names[4,], names[5,], names[6,])


######### Create a vector with results for tidy dataset
vector <- c()
j = 1
for(i in 1:(30*6)){
        for(w in 3:81){
                vector[j] <- date[i,w]
                j = j +1                
        }
}

########## Creates a tidy dataset
tidy_data <- as.data.frame(matrix(data = vector, nrow = 30, ncol = 79 *6, byrow= T))
tidy_data$subject = unique(date[,2])
tidy_data <- tidy_data[,c(475, 1:474)]
names(tidy_data) <- names

########### Writes datasets ###############
write.table(date, file = "tidy_data1.txt", row.names=F, sep = ",")
write.table(tidy_data, file = "tidy_data.txt", row.names=F, sep = ",")

