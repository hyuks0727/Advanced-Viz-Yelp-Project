library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(jsonlite)

### After downloading yelp_dataset.tar, unzip
### it two times and then you get all the json
### files.
json_file<-'yelp_academic_dataset_business.json'

result<-jsonlite::stream_in(file(json_file))
head(result)
colnames(result)