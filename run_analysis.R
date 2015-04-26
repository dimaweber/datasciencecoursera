# read column names from file 
features <- read.table("features.txt", stringsAsFactors =  FALSE)
desc_col_names <- features[,2]

# find mean/std columns indices by parsing column names
col_indices <- grep ("(mean|std)\\(\\)", desc_col_names, ignore.case = FALSE)

# read activity names from file
activities <- read.table('activity_labels.txt')

# this function do data read, merging observations data with subject id and activity name
read_table <- function(dataFilename, activityFilename, subjectFilename) {
  # read activities numbers
  act <- read.table(activityFilename, header = FALSE, col.names=c('activity_id'))
  
  # add column with activities names
  act$activityName <- factor(act$activity_id, activities[,1], labels=activities[,2])
  activityName <- act$activityName
    
  subj <- read.table(subjectFilename, header=FALSE, col.names=c('subject'))

  # read whole data (and bind column names from features.txt)  
  raw <- read.table(dataFilename, header = FALSE);
  colnames(raw) <- desc_col_names
  
  # reduce data by leaving only activity and mean/std columns
  reduced <- cbind(activityName, subj, raw[,col_indices])

  # return value
  reduced
}

# read test / train data
train <- read_table("train/X_train.txt", "train/y_train.txt", 'train/subject_train.txt')
tests <- read_table("test/X_test.txt", "test/y_test.txt", 'test/subject_test.txt')

# build output 
output <- rbind(tests,train )

# delete dataframes we don't need anymore so it don't eat memory
tests <- NULL
train <- NULL
features <- NULL
activities <- NULL
#col_indices <- NULL

# melt output, splitting it by activity/subject key
melted_output <- melt(output, id=c('activityName','subject'), mesure.vars=desc_col_names)

# calculate means for every activity / subject / measure variable
summary <- dcast(melted_output, activityName + subject ~ variable, mean)

# delete not needed data frames (memory clean up)
#desc_col_names <- NULL
melted_output <- NULL

# write result data frames to files
write.table(output, file = 'output.txt', row.name = FALSE)
write.table(summary, file = 'summary.txt', row.name = FALSE)

summary