# CODE BOOK

Identifiers
* Subject.
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. (see. /UCI HAR Dataset/README.txt)
* Activity. 
Each person performed six activities wearing a smartphone (Samsung Galaxy S II) on the waist. 
  (see. /UCI HAR Dataset/README.txt and UCI HAR Dataset/activity_labels.txt )
  
      WALKING
      WALKING_UPSTAIRS
      WALKING_DOWNSTAIRS
      SITTING
      STANDING
      LAYING

  ======================================

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz.  

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern: 

'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions. 

    tBodyAcc-XYZ 
    tGravityAcc-XYZ 
    tBodyAccJerk-XYZ 
    tBodyGyro-XYZ 
    tBodyGyroJerk-XYZ 
    tBodyAccMag 
    tGravityAccMag 
    tBodyAccJerkMag 
    tBodyGyroMag 
    tBodyGyroJerkMag 
    fBodyAcc-XYZ 
    fBodyAccJerk-XYZ 
    fBodyGyro-XYZ 
    fBodyAccMag 
    fBodyAccJerkMag 
    fBodyGyroMag 
    fBodyGyroJerkMag

(see /UCI HAR Dataset/README.txt)

======================================

tidy_data.txt result (Column):
	
    timeBodyAccelerometer-mean()-X	
    timeBodyAccelerometer-mean()-Y	
    timeBodyAccelerometer-mean()-Z	
    timeBodyAccelerometer-std()-X	
    timeBodyAccelerometer-std()-Y	
    timeBodyAccelerometer-std()-Z	
    timeGravityAccelerometer-mean()-X	
    timeGravityAccelerometer-mean()-Y	
    timeGravityAccelerometer-mean()-Z	
    timeGravityAccelerometer-std()-X	
    timeGravityAccelerometer-std()-Y	
    timeGravityAccelerometer-std()-Z	
    timeBodyAccelerometerJerk-mean()-X	
    timeBodyAccelerometerJerk-mean()-Y	
    timeBodyAccelerometerJerk-mean()-Z	
    timeBodyAccelerometerJerk-std()-X	
    timeBodyAccelerometerJerk-std()-Y	
    timeBodyAccelerometerJerk-std()-Z	
    timeBodyGyroscope-mean()-X	
    timeBodyGyroscope-mean()-Y	
    timeBodyGyroscope-mean()-Z	
    timeBodyGyroscope-std()-X	
    timeBodyGyroscope-std()-Y	
    timeBodyGyroscope-std()-Z	
    timeBodyGyroscopeJerk-mean()-X	
    timeBodyGyroscopeJerk-mean()-Y	
    timeBodyGyroscopeJerk-mean()-Z	
    timeBodyGyroscopeJerk-std()-X	
    timeBodyGyroscopeJerk-std()-Y	
    timeBodyGyroscopeJerk-std()-Z
    timeBodyAccelerometerMagnitude-mean()	
    timeBodyAccelerometerMagnitude-std()	
    timeGravityAccelerometerMagnitude-mean()	
    timeGravityAccelerometerMagnitude-std()	
    timeBodyAccelerometerJerkMagnitude-mean()	
    timeBodyAccelerometerJerkMagnitude-std()	
    timeBodyGyroscopeMagnitude-mean()	
    timeBodyGyroscopeMagnitude-std()	
    timeBodyGyroscopeJerkMagnitude-mean()	
    timeBodyGyroscopeJerkMagnitude-std()	

    frequencyBodyAccelerometer-mean()-X	
    frequencyBodyAccelerometer-mean()-Y	
    frequencyBodyAccelerometer-mean()-Z	
    frequencyBodyAccelerometer-std()-X
    frequencyBodyAccelerometer-std()-Y	
    frequencyBodyAccelerometer-std()-Z	
    frequencyBodyAccelerometerJerk-mean()-X	
    frequencyBodyAccelerometerJerk-mean()-Y	
    frequencyBodyAccelerometerJerk-mean()-Z	
    frequencyBodyAccelerometerJerk-std()-X	
    frequencyBodyAccelerometerJerk-std()-Y	
    frequencyBodyAccelerometerJerk-std()-Z	
    frequencyBodyGyroscope-mean()-X	
    frequencyBodyGyroscope-mean()-Y	
    frequencyBodyGyroscope-mean()-Z	
    frequencyBodyGyroscope-std()-X	
    frequencyBodyGyroscope-std()-Y	
    frequencyBodyGyroscope-std()-Z	
    frequencyBodyAccelerometerMagnitude-mean()	
    frequencyBodyAccelerometerMagnitude-std()	
    frequencyBodyAccelerometerJerkMagnitude-mean()	
    frequencyBodyAccelerometerJerkMagnitude-std()	
    frequencyBodyGyroscopeMagnitude-mean()	
    frequencyBodyGyroscopeMagnitude-std()	
    frequencyBodyGyroscopeJerkMagnitude-mean()
    frequencyBodyGyroscopeJerkMagnitude-std()

