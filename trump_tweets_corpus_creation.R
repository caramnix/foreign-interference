

library(stringr)
library(ngram)

#save(wikileaks_corpus, file = "/Users/caranix/OneDrive - The Ohio State University/2021-2022/Spring/POLITSC 7781/Wikileaks/wikileaks_corpus.Rda")

setwd("/Users/caranix/OneDrive - The Ohio State University/2021-2022/Spring/POLITSC 7781/Wikileaks")

trump_tweets<- read.csv("realdonaldtrump.csv")

test <- trump_tweets[1:5,]


within_time_frame<- function(date_i){
  return(ifelse(strptime(as.Date(date_i), format = "%Y-%m-%d") < strptime(as.Date("2017-01-07"), format = "%Y-%m-%d"),ifelse(strptime(as.Date(date_i), format = "%Y-%m-%d")> strptime(as.Date("2016-07-21"), format = "%Y-%m-%d"), 1, 0),0))
}

#1- within time frame, 0, not within time frame
hold<- unlist(lapply(trump_tweets$date, within_time_frame))


trump_tweets$within_time_frame<- hold

#subset to keep just the ones w/n our time frame
trump_tweets_within_time_frame<- subset(trump_tweets, within_time_frame ==1)

#1532 
trump_tweets_within_time_frame



#now we want to know if the tokens arw in the text 

trump_tweets_within_time_frame$content<- as.character(trump_tweets_within_time_frame$content)

doc_name <- paste("Utterance",1:nrow(trump_tweets_within_time_frame))
trump_tweets_within_time_frame["doc_name"] <- doc_name 

#make corpus
trump_tweets_corpus_long<- corpus(trump_tweets_within_time_frame,  
                               docid_field = "doc_name",
                               text_field = "content")
#make dict object
my_dict2 <- dictionary(list(dict= short_dict))

#pull out if the token is in the text or not
mentions_mat <- dfm(trump_tweets_corpus_long, dictionary = my_dict2)
mentions_df <- convert(mentions_mat, to = "data.frame")

#merge
merged_trump<- merge(x = trump_tweets_within_time_frame, y = mentions_df, by.x="doc_name", by.y="doc_id", all = TRUE)

#now subset! 
#so now, this is the data which contains anything in our dictionary within the given time frame! 
merged_trump_subset<- subset(merged_trump, dict>0)


save(merged_trump_subset, file = "/Users/caranix/OneDrive - The Ohio State University/2021-2022/Spring/POLITSC 7781/Wikileaks/trump_tweets_df.Rda")

file2= "/Users/caranix/OneDrive - The Ohio State University/2021-2022/Spring/POLITSC 7781/Wikileaks/trump_tweets_df.Rda"
load(file2) 





