#wine reviews
data=read.csv('winemag-data_first150k.csv',header = T,sep = ',')
summary(data)
str(data)
colnames(data)
data$X=NULL
sum(is.na(data))
length(unique(data$winery))

#First we will do a Visualization.
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(DT)
library(kableExtra)
library(knitr)

###################wines above 95 Points###########################
temp<-data %>%
  filter(points >95) %>%
  group_by(country)%>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(n = 10)

kable(temp) #for finding the countries above 95 points

temp %>%    
  ggplot(aes(x =reorder(country,n), y =  n )) +
  geom_bar(stat='identity',colour="white", fill = c("darkorchid4")) +
  labs(x = 'country', y = 'Count', title = 'Best Wine Producing Country - Point wise') +
  coord_flip() + 
  theme_bw()

################Costly wines are made############  
temp<-data %>%
  select(country,points,price,province,winery) %>%
  arrange(desc(price)) %>%#in the decending order
  head(n = 20)
kable(temp)

kable(temp,"html") %>% kable_styling("striped",full_width=T) %>% column_spec(1:2,bold=T,background="white") %>% row_spec(c(1,3,4,5,6,7,8,10,11,16,17,18,20),bold=F,color="white",background="#ffb6c1")


# Selecting top 20 from table based on Price

temp<-data %>%
  arrange(desc(price)) %>%
  head(n = 20)%>%
  group_by(country)%>%
  summarise(n = n())%>%
  arrange(desc(n)) 

kable(temp)



#country with costliest wine
temp %>%    
  ggplot(aes(x =reorder(country,n), y =  n )) +
  geom_bar(stat='identity',colour="white", fill = c("darkorchid4")) +
  labs(x = 'Country', y = 'Count', title = 'Which Country Makes Coslty Wines') +
  coord_flip() + 
  theme_bw()


#Economics where wines are made
temp<-data %>%
  select(country,points,price,province,winery) %>%
  arrange(price) %>%
  head(n = 20)

datatable(temp,options = list(searching = FALSE,pageLength = 20,lengthMenu = c(20))) %>% 
  formatStyle('country',target = 'row',backgroundColor = styleEqual(c('Spain', 'US'), c('#ffb6c1', '#b37f87')))


# Selecting bottom 20 from table based on Price

temp<-data %>%
  arrange(price) %>%
  head(n = 20)%>%
  group_by(country)%>%
  summarise(n = n())%>%
  arrange(desc(n)) 


kable(temp)
temp %>%    
  ggplot(aes(x =reorder(country,n), y =  n )) +
  geom_bar(stat='identity',colour="white", fill = c("darkorchid4")) +
  labs(x = 'Country', y = 'Count ', title = 'Which Country Produce Economic wine') +
  coord_flip() + 
  theme_bw()


temp<-data %>%
  select(country,points,price,province,winery) %>%
  group_by(country)%>%
  drop_na(price,country )%>%
  summarize(avg_price = mean(price))%>% 
  arrange(desc(avg_price)) 

datatable(temp, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))


temp %>%    
  ggplot(aes(x =reorder(country,avg_price), y =  avg_price )) +
  geom_bar(stat='identity',colour="white", fill = c("darkorchid4")) +
  labs(x = 'country', y = 'Average Price of Wine', title = 'Average Price of Wine - Country wise') +
  coord_flip() + 
  theme_bw()


#Country wise costly wine
#selecting top 100 wines based on price

temp<-data %>%
  select(country,points,price,province,winery) %>%
  arrange(price) %>%
  head(n = 100)%>%
  group_by(country,winery)%>%
  summarise(n = n())%>%
  arrange(country,desc(n)) 


colour <-c('#ddd8d9','#ffcad2','#ff7b8f','#ff405d','#ff193c','#ddd8d9','#ffcad2','#ff7b8f','#ff405d','#ff193c','#ddd8d9')

colours<-c(colour,colour,colour,colour,'#ddd8d9','#ffcad2','#ff7b8f','#ff405d')

datatable(temp,options = list(searching = FALSE,pageLength = 20,lengthMenu = c(20,40))) %>% 
  formatStyle('country',target = 'row',backgroundColor = styleEqual(c('Argentina','Australia','Chile','France','Germany','Italy','Portugal','Romania','South Africa','Spain','US' ), colour))


#12 most common word used to designate a wine-Word clould for designation column.

library(tm)
library(wordcloud)
library(RColorBrewer)
makeWordCloud <- function(documents) {
  corpus = Corpus(VectorSource(tolower(documents)))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  
  frequencies = DocumentTermMatrix(corpus)
  word_frequencies = as.data.frame(as.matrix(frequencies))
  
  words <- colnames(word_frequencies)
  freq <- colSums(word_frequencies)
  wordcloud(words, freq,
            min.freq=sort(freq, decreasing=TRUE)[[50]],
            colors=brewer.pal(8, "Dark2"),
            random.color=TRUE,random.order=FALSE) 
}  


makeWordCloud(data[["designation"]][1:50])

makeWordCloud(data[["description"]][1:50])



