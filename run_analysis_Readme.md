## run_analysis.R  README
### author: "Olusola Soyemi"
### date: "Saturday, May 23, 2015"

The program "run_analysis.R" generates a tidy dataset from the larger Smartphone dataset by executing the following steps.

* 1. The data is loaded in the R environment from the data depository

* 2. The training and test sets are merged into a single 10229 x 563 data frame "data". The first 2 columns in data contain the factor variables subject (30 levels) and activity (6 levels). Columns 3 to 563 contain the 561 measurement variables

* 3. The data frame data is further reduced by extracting only the measurements on the mean and standard deviation for each measurement (66 total). The reduced data frame is 10229 x 68 (including the factor variables)

* 4. The elements of the data frame column labelled activity are replaced with descriptive activity names

* 5. The data frame columns (3 to 68) are labelled with the descriptive name of each measurement variable in the reduced data set.

* 6. A second data frame (tidy data set) is created with the average of each variable in the reduced dataset for each activity and each subject