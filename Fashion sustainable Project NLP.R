#####Learning more about definitions of sustainable fashion
#Tacking the data from the document (articles, blogs, transcript videos)

library(textreadr)
setwd("/Users/ericavargas/Downloads/Everything I need on R/Definitions")
nm <- list.files(path="/Users/ericavargas/Downloads/Everything I need on R/Definitions")
sus_fashion <- do.call(rbind, lapply(nm, function(x) read_doc(file=x)))

#Before working on the data it is needed to put it on a dataframe
library(dplyr)
susf_df <- data.frame(line=1:3, text=sus_fashion)
print(susf_df)

#Before tokenizing the element we want to create our own set of stop_words
custom_stop_words <- tribble(
  ~word, ~lexicon,"http", "CUSTOM", "https", "CUSTOM", "t.co", "CUSTOM",
  "amp", "CUSTOM","dm", "CUSTOM", "direct", "CUSTOM", "fashion", "CUSTOM",
  "shop", "CUSTOM","sale", "CUSTOM","bn", "CUSTOM","christmas2021", "CUSTOM",
  "la", "CUSTOM","de", "CUSTOM","der", "CUSTOM","en", "CUSTOM","el", "CUSTOM",
  "les", "CUSTOM","die", "CUSTOM","und", "CUSTOM","à", "CUSTOM","le", "CUSTOM",
  "des", "CUSTOM","der", "CUSTOM","es", "CUSTOM","ist", "CUSTOM","se", "CUSTOM",
  "al", "CUSTOM","das", "CUSTOM","van", "CUSTOM","von", "CUSTOM","sur", "CUSTOM",
  "1", "CUSTOM","c7kllt0mio", "CUSTOM","del", "CUSTOM","für", "CUSTOM",
  "mit", "CUSTOM","c7kllt0mio", "CUSTOM","wir", "CUSTOM","ที่", "CUSTOM",
  "den", "CUSTOM","ckb9g7yt3w", "CUSTOM","0", "CUSTOM","2", "CUSTOM",
  "1", "CUSTOM","3", "CUSTOM","5", "CUSTOM","4", "CUSTOM","las", "CUSTOM",
  "los", "CUSTOM","6", "CUSTOM","7", "CUSTOM","8", "CUSTOM","9", "CUSTOM",
  "100", "CUSTOM","91", "CUSTOM","90", "CUSTOM","81", "CUSTOM","80", "CUSTOM",
  "70", "CUSTOM","71", "CUSTOM","60", "CUSTOM","51", "CUSTOM",
  "50", "CUSTOM","41", "CUSTOM","40", "CUSTOM","31", "CUSTOM","30", "CUSTOM",
  "21", "CUSTOM","20", "CUSTOM","53", "CUSTOM")

#However we do not want to eliminate some words needed for this report
donoteliminate <-tribble(
  ~word, ~lexicon,
  "no", "CUSTOM","not", "CUSTOM")

#Creation of the new set of stop words
stop_words2 <- stop_words %>% 
  bind_rows(custom_stop_words) %>%
  anti_join(donoteliminate)

#tokenizing the susdf dataframe, before tokenizing we load the library needed 
library(tidytext)
library(tidyverse)
token_list_susdf <- susf_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words2) %>%
  count(word, sort=TRUE)

#Doing a wordcloud to see what is associated with sustainable fashion
#install.packages("wordcloud2")
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)

#After loading the libraries needed we select the top 100 words associated
token_list_susdf_100 <- token_list_susdf %>%
  top_n(100)

#Creation of word cloud
wordcloud2(data = token_list_susdf_100, 
           size = 0.6, color = "random-dark")

#We want to have a look at the bigger picture by having a look at the semantic 

#We load the libraries needed
library(dplyr)
library(tidytext)
library(tidyr)

#We tokenize all the texts in 3
fashion_trigrams <- susf_df %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3)

fashion_trigrams #We want to see the trigrams 

#We count the different trigrams
fashion_trigrams %>%
  count(trigram, sort = TRUE) #this has many stop words, need to remove them 

#Remove stop words from the trigram data, by using the separate function:
library(tidyr)
fashion_trigrams_separated <- fashion_trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

#Take out all the stop_words
fashion_trigrams_filtered <- fashion_trigrams_separated %>%
  filter(!word1 %in% stop_words2$word) %>%
  filter(!word2 %in% stop_words2$word)%>%
  filter(!word3 %in% stop_words2$word)

#creating the new triigram, "no-stop-words":
fashion_trigram_counts <- fashion_trigrams_filtered %>%
  count(word1, word2,word3, sort = TRUE)

#want to see the new bigrams
fashion_trigram_counts

#We load the libraires necessary to show the interconnection between words
library(igraph)
fashion_trigram_graph <- fashion_trigram_counts %>%
  filter(n>1) %>%
  graph_from_data_frame()

#We want to observe the direction of the words
fashion_trigram_graph

#plotting the network of words
library(ggraph)
ggraph(fashion_trigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

#For the second question we want to load the data from Twitter
#installing and loading rtweet
#install.packages("rtweet")
#Loading the library
library(rtweet)

#Key identification
consumer_key <- "1cE0aTVmSlePLSkpnXIHl37mk"
consumer_secret <- "P9xaJLqdOcczzVy6XA8z0zLoIqPAcylCyShVcjGzaUfEh82vw3"
access_token <- "1461490377014136835-sNw4F5twBxHTyu10gZaMBlv3bcapvR"
access_secret <- "gNgfVCnDI3MtMwhHu4QdOHSc3PCIuhGCv7fYawWz2mPcO"
name_of_app <- "FashionProjectHult"

#token to connect with my account
twitter_token <- create_token(
  app = name_of_app,
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret)

#Loading the fast fashion and sustainable comments 
rt_sus_fashion <- search_tweets("#sustainablefashion", n = 50000, 
                                include_rts = FALSE)
rt_fast_fashion <- search_tweets("#fastfashion", n = 50000, include_rts = FALSE)

#We want to tokenize the comments about sustainable fashion 
sus_rt_token <- rt_sus_fashion %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words2) %>%
  count(word, sort=TRUE)

#We run an AFINN analysis on sustainable fashion 
opinions2_affin <-sus_rt_token %>%
  inner_join(get_sentiments("afinn")) %>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

#We load the libraires necessary to show the interconnection between words
library(igraph)
fashion_trigram_graph <- fashion_trigram_counts %>%
  filter(n>1) %>%
  graph_from_data_frame()

#We want to observe the direction of the words
fashion_trigram_graph

#plotting the network of words
library(ggraph)
ggraph(fashion_trigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

#For the second question we want to load the data from Twitter
#installing and loading rtweet
#install.packages("rtweet")
#Loading the library
library(rtweet)

#Key identification
consumer_key <- "******"
consumer_secret <- "*******"
access_token <- "*******"
access_secret <- "********"
name_of_app <- "FashionProjectHult"

#token to connect with my account
twitter_token <- create_token(
  app = name_of_app,
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret)

#Loading the fast fashion and sustainable comments 
rt_sus_fashion <- search_tweets("#sustainablefashion", n = 50000, 
                                include_rts = FALSE)
rt_fast_fashion <- search_tweets("#fastfashion", n = 50000, include_rts = FALSE)

#We want to tokenize the comments about sustainable fashion 
sus_rt_token <- rt_sus_fashion %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words2) %>%
  count(word, sort=TRUE)

#We run an AFINN analysis on sustainable fashion 
opinions2_affin <-sus_rt_token %>%
  inner_join(get_sentiments("afinn")) %>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

#Loading the needed library
library(tidyr)

#We want to calculate the frequency of each word 
frequency <- bind_rows(mutate(token_list_susdf, author ="Sustainable Fashion"),
                       mutate(token_list_fastfdf, author = "Fast Fashion"),
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n), na.rm = TRUE)%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Fast Fashion`)

#let's plot the correlograms:
#Loading the library needed
library(scales)
library(plotly)

#We store the plot inside an object to use ggplotly and make it interactive
ericas<- ggplot(frequency, aes(x=proportion, y=`Sustainable Fashion`, 
                               color = abs(`Sustainable Fashion`- proportion), key=word))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=1, size=1, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Sustainable Fashion", x=NULL)

ggplotly(ericas, tooltip = c("key", "proportion"))

#We want to analyze pdf documents that will tell us what enterprises should do 
#Data from the pdf regarding to fashion and the actions that need to be done
# Importing all PDF files from the same folder
#install.packages("pdftools")
library(pdftools) # we need this library to use pdf_text
setwd("/Users/ericavargas/Downloads/Everything I need on R/Actions")
nm <- list.files(path="/Users/ericavargas/Downloads/Everything I need on R/Actions")
actions_fashion <- do.call(rbind, lapply(nm, function(x) pdf_text(x)))

#Before working on the data it is needed to put it on a dataframe
library(dplyr)
actions_fashion_df1 <- as.data.frame(x = actions_fashion[1,5:109]) 
actions_fashion_df2 <- actions_fashion[2,1]
actions_df1 <- data.frame(line=1:105, text=actions_fashion_df1)
colnames(actions_df1)[2] <- "text"
actions_df2 <- data.frame(line=1, text=actions_fashion_df2)
actions_df <- rbind(actions_df1, actions_df2)

#we tokenize the data 
token_action_df <- actions_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words2) %>%
  count(word, sort=TRUE)

#We create the quadrograms that we will evaluate  
fashion_quadrograms <- actions_df %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4)
fashion_quadrograms #We want to see the quadrograms 

#We evaluate the first quadrograms through the counts
fashion_quadrograms %>%
  count(quadrogram, sort = TRUE) #this has many stop words, need to remove them 

#Remove stop words from the quadrogram data by using the separate function:
library(tidyr)
fashion_quadrograms_separated <- fashion_quadrograms %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep = " ")

fashion_quadrograms_filtered <- fashion_quadrograms_separated %>%
  filter(!word1 %in% stop_words2$word) %>%
  filter(!word2 %in% stop_words2$word)%>%
  filter(!word3 %in% stop_words2$word)%>%
  filter(!word4 %in% stop_words2$word)

#creating the new quadrogram, "no-stop-words":
fashion_quadrogram_counts <- fashion_quadrograms_filtered %>%
  count(word1, word2,word3,word4, sort = TRUE)
#want to see the new quadrogramsgrams
fashion_quadrogram_counts

#We want to plot the quadrograms
library(igraph)
fashion_quadrogram_graph <- fashion_quadrogram_counts %>%
  filter(n>1) %>%
  graph_from_data_frame()

fashion_quadrogram_graph

#Plotting the network
library(ggraph)
ggraph(fashion_quadrogram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

