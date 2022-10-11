
rm(list=ls())

library(stringr)
library(ngram)
library(quanteda)

#build shortened dict 
short_dict<- c("Wikileaks", "Julian Assange", "DNC Hack", "Election Interference", "Hacked Emails",
              "Election Meddling", "Data Breach", "Russian Hackers", "Connections to Russia",
              "Foreign Agent", "Foreign Interference", "RNC Hacked", "Mueller", "dnc", "hack", "emails", "e-mails", "Crooked Hillary")

short_dict<- tolower(short_dict)

#read in full data
load('/Users/caranix/OneDrive - The Ohio State University/2021-2022/Spring/POLITSC 7781/Russian Interference/foreign_interference.Rda')

#columns to keep in fulldata
cols_to_keep<- c("airdate", "show_length", "text", "year", "day", "month", "date", "filename")
foreign_interference_data_keep<- foreign_interference_data[,cols_to_keep]

#lowercase text
foreign_interference_data_keep$text <- tolower(foreign_interference_data_keep$text)

foreign_interference_data_keep

#find utterances within the time frame we care about
within_time_frame<- function(date_i){
  return(ifelse(strptime(as.Date(date_i), format = "%Y-%m-%d") < strptime(as.Date("2017-01-07"), format = "%Y-%m-%d"),ifelse(strptime(as.Date(date_i), format = "%Y-%m-%d")> strptime(as.Date("2016-07-21"), format = "%Y-%m-%d"), 1, 0),0))
}

#1- within time frame, 0, not within time frame
hold<- unlist(lapply(foreign_interference_data_keep$date, within_time_frame))

foreign_interference_data_keep$within_time_frame<- hold

#subset to keep just the ones w/n our time frame
foreign_interference_data_keep_subset<- subset(foreign_interference_data_keep, within_time_frame ==1)

#function which will split the filename into just the show title 
extract_show<- function(x) {
  return(unlist(str_split(unlist(str_split(x, "CHAMP_Updates/"))[2], "/"))[1]) 
}

#note this data has "CNN-Old" and "Fox-Old"
show_long<- unlist(lapply(foreign_interference_data_keep_subset$filename, extract_show))

#recode manually- bleh 
show_long[show_long=="CNN-Old"] <- "CNN"
show_long[show_long=="Fox-Old"] <- "Fox"
show_long[show_long=="CBS-Old"] <- "CBS"
show_long[show_long=="MSNBC-Old"] <- "MSNBC"
show_long[show_long=="NBC-Old"] <- "NBC"
show_long[show_long == "ABC-Old"]<- "ABC"

foreign_interference_data_keep_subset["show_short"] <- show_long

doc_name <- paste("Utterance",1:nrow(foreign_interference_data_keep_subset))
foreign_interference_data_keep_subset["doc_name"] <- doc_name 

table(foreign_interference_data_keep_subset$show_short)





#save
save(foreign_interference_data_keep_subset, file = "/Users/caranix/OneDrive - The Ohio State University/2021-2022/Spring/POLITSC 7781/Russian Interference/foreign_interference_data_subset_wikileaks_time_frame.Rda")


foreign_interference_data_subset_wikileaks_time_frame <-foreign_interference_data_keep_subset

#make corpus
wikileaks_corpus_long<- corpus(foreign_interference_data_subset_wikileaks_time_frame,  
                               docid_field = "doc_name",
                               text_field = "text")
#make dict object
my_dict2 <- dictionary(list(dict= short_dict))

#pull out if the token is in the text or not
mentions_mat <- dfm(wikileaks_corpus_long, dictionary = my_dict2)
mentions_df <- convert(mentions_mat, to = "data.frame")

#merge
merged_wikileaks<- merge(x = foreign_interference_data_subset_wikileaks_time_frame, y = mentions_df, by.x="doc_name", by.y="doc_id", all = TRUE)

#now subset! 
#so now, this is the data which contains anything in our dictionary within the given time frame! 
merged_wikileaks_subset<- subset(merged_wikileaks, dict>0)

#65313 words total 
wordcount(merged_wikileaks_subset$text, sep=" ")

save(merged_wikileaks_subset, file = "/Users/caranix/OneDrive - The Ohio State University/2021-2022/Spring/POLITSC 7781/Wikileaks/wikileaks_df.Rda")

table(merged_wikileaks_subset$show_short)/table(foreign_interference_data_keep_subset$show_short)

wikileaks_corpus <- corpus(merged_wikileaks_subset,
                                      docid_field = "doc_name",
                                      text_field = "text")

save(wikileaks_corpus, file = "/Users/caranix/OneDrive - The Ohio State University/2021-2022/Spring/POLITSC 7781/Wikileaks/wikileaks_corpus.Rda")



plot(table(merged_wikileaks_subset$show_short)/table(foreign_interference_data_keep_subset$show_short))




library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)  # f

df<- as.data.frame((table(merged_wikileaks_subset$show_short)/table(foreign_interference_data_keep_subset$show_short)))
df$lc<- c(0,0,.5,1,-1,0)
colnames(df)<- c("Network", "Percent")

library(forcats)
df %>%
  mutate(test = fct_reorder(Network, lc)) 
  
  
net_cov<- ggplot(df, aes(x = Network, y = Percent, fill=Network)) + 
  geom_bar(stat = "identity") + 
  ylab("Percent of Total Coverage") +
  ggtitle("Major News Network Coverage of DNC Hack")+ 
  scale_fill_manual(values=c("#a8a8a8",
                             "#a8a8a8",
                             "#83a0e6",
                             "red", 
                             "#1951d4",
                             "#a8a8a8"))

net_cov
