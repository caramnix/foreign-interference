
rm(list=ls())


library(tidyverse)


file = "/Users/caranix/OneDrive - The Ohio State University/2021-2022/Spring/POLITSC 7781/Wikileaks/wikileaks_df.Rda"
load(file)

file2= "/Users/caranix/OneDrive - The Ohio State University/2021-2022/Spring/POLITSC 7781/Wikileaks/trump_tweets_df.Rda"
load(file2) 
#trump_tweets_within_time_frame

merged_trump_subset$week <- as.Date(cut(as.Date(merged_trump_subset$date), breaks="week"))

grouped_trump<- merged_trump_subset %>% group_by (week)  %>% 
  summarize (sum (within_time_frame)) %>%
  select(week, "Tweets" = "sum(within_time_frame)")

t <- ggplot(grouped_trump, aes(x= week, y= Tweets)) +
  geom_line() + 
  xlab("")
t


## PLOTS! 

# line chart- just network. 
merged_wikileaks_subset$week <- as.Date(cut(as.Date(merged_wikileaks_subset$date), breaks="week"))

grouped_wiki<- merged_wikileaks_subset %>% group_by (week,show_short)  %>% 
  summarize (sum (within_time_frame)) %>%
  select(week, "Network"= "show_short", "Utterances" = "sum(within_time_frame)")

p <- ggplot(grouped_wiki, aes(x= week, y= Utterances)) +
  geom_line() + 
  xlab("")
p

# line chart- by network! 
net <- ggplot(grouped_wiki, aes(x= week, y= Utterances, group =Network, color=Network )) +
  geom_line() + 
  xlab("July 2016- January 2017")+ 
  ylab("Number of Utterances") +
  ggtitle("Major News Network Coverage of DNC Hack")
net


#line chart -- News Network Coverage vs. Trump Tweets about DNC Hack, line charts, grouped (weekly)

grouped_wiki_2<- merged_wikileaks_subset %>% group_by (week)  %>% 
  summarize (sum (within_time_frame)) %>%
  select(week, "Utterances" = "sum(within_time_frame)")

grouped_trump_2<- merged_trump_subset %>% group_by (week)  %>% 
  summarize (sum (within_time_frame)) %>%
  select(week, "Tweets" = "sum(within_time_frame)")

net_trump <- ggplot() + 
  geom_line(data= grouped_wiki_2, aes(x= week, y= Utterances, colour="Utterances")) + #, color= "black") + 
  geom_line(data = grouped_trump_2, aes(x= week, y= Tweets, colour="Tweets")) + #), color = "red") +
  xlab("July 2016- January 2017") +
  scale_color_manual(name = "Type", values = c("Utterances" = "black", "Tweets" = "red")) + 
  ylab("Count") +
  ggtitle("News Network Coverage vs. Trump Tweets about DNC Hack")
net_trump

###

#merged_wikileaks_subset$date <- as.Date(cut(as.Date(merged_wikileaks_subset$date), breaks="day"))


#line chart -- News Network Coverage vs. Trump Tweets about DNC Hack, points for tweets/ daily
grouped_wiki_3<- merged_wikileaks_subset %>% group_by (date)  %>% 
  summarize (sum (within_time_frame)) %>%
  select(date, "Utterances" = "sum(within_time_frame)")

grouped_trump_3<- merged_trump_subset %>% group_by (date)  %>% 
  summarize (sum (within_time_frame)) %>%
  select(date, "Tweets" = "sum(within_time_frame)")

net_trump <- ggplot() + 
  geom_line(data= grouped_wiki_3, aes(x=as.Date(date), y= Utterances, colour="Utterances")) + #, color= "black") + 
  geom_point(data = grouped_trump_3, aes(x= as.Date(date), y= Tweets, colour="Tweets")) + #), color = "red") +
  xlab("July 2016- September 2017") +
  scale_color_manual(name = "Type", values = c("Utterances" = "black", "Tweets" = "red")) + 
  ylab("Count") +
  ggtitle("News Network Coverage vs. Trump Tweets about DNC Hack")
net_trump




#bar chart -- Distribution of Coverage of DNC Hack by Network 
test<- merged_wikileaks_subset %>% group_by (show_short) %>%
  summarize (sum(within_time_frame)) %>%
  select("Network" = "show_short", "Count" = "sum(within_time_frame)")

bar<-ggplot(data=test, aes(x=Network, y=Count)) +
  geom_bar(stat="identity") + 
  ggtitle("Distribution of Coverage of DNC Hack by Network")
bar

