library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(jsonlite)
library(dplyr)
library(superml)
json_file<-'yelp_academic_dataset_review.json'

con_out <- file(tmp <- tempfile(), open = "wb")
con_out2 <- file(tmp2 <- tempfile(), open = "wb")
jsonlite::stream_in(file(json_file),
                            handler=function(df){
                              df1<-dplyr::filter(df,stars>3)
                              df1<-df1[c('stars','text')]
                              length1=dim(df1)[1]
                              select1=sample(1:5000,length1,replace=T)
                              df1<-df1[select1==1,]
                              stream_out(df1,con_out,pagesize=1000)
                              
                              df2<-dplyr::filter(df,stars<3)
                              df2<-df2[c('stars','text')]
                              length2=dim(df2)[1]
                              select2=sample(1:5000,length2,replace=T)
                              df2<-df2[select2==1,]
                              stream_out(df2,con_out2,pagesize=1000)
                            })
close(con_out)
close(con_out2)

good_review <- stream_in(file(tmp))
bad_review <- stream_in(file(tmp2))

head(good_review$text,1)

cv=CountVectorizer$new(max_df=0.99,min_df=0.01)
cv_count_matrix1<-cv$fit_transform(good_review$text)
cv_count_matrix2<-cv$fit_transform(bad_review$text)

word1 = apply(cv_count_matrix1,2,sum)
sort(word1)

word2 = apply(cv_count_matrix2,2,sum)
sort(word2)

write.csv(word1, 'good_review.csv',row.names=F)
write.csv(word2, 'bad_review.csv',row.names=F)

#################

