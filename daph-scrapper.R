library(rvest)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(ggthemes)
library(tm)

out = NULL
for (i in 1:6 ){
  for (j in 1:6) {
      if ( i > 1) {
     x <- read_html(paste0("http://www.analyticshour.io/all-podcast-episodes/page/", i))
    
    htmlNode <- x %>% 
      html_nodes(paste0("body > div.super-container.light-icons > div.main-content.page.archive-page > div > div > div > div > article:nth-child(",j,") > div > footer > ul > li.title.not-truncate ")) %>%
      html_children 
    
    check <- x %>% 
      html_nodes(paste0("body > div.super-container.light-icons > div.main-content.page.archive-page > div > div > div > div > article:nth-child(",j,") > div > footer > ul > li.title.not-truncate > a")) %>% html_text() %>% substr(2, 4) %>% as.numeric()
    
    url <- substr(htmlNode, gregexpr('"', htmlNode)[[1]][1]+1, gregexpr('"', htmlNode)[[1]][2]-1)

   
   
    
    if (check > 53 & check < 79) { #2017 episodes     
      
      content <- read_html(url) 
      post_id <- content %>%   html_nodes("body") %>% html_attr("class") %>% {gsub("\\D", "", .)} 
      
      test_text<- content %>% 
        html_nodes(paste0("#post-",post_id," > div ")) %>% html_text() %>% strsplit(" ") %>% unlist
      
      test_text <- test_text[which(grepl('Transcript', test_text))+1:length(test_text)] 
      
      text_df <- data.frame(ep = check, test_text)
      out = rbind(out,text_df)
    } else (
      break
    )
    
      } else {
      
        x <- read_html('http://www.analyticshour.io/all-podcast-episodes/')
        
        htmlNode <- x %>% 
          html_nodes(paste0("body > div.super-container.light-icons > div.main-content.page.archive-page > div > div > div > div > article:nth-child(",j,") > div > footer > ul > li.title.not-truncate ")) %>%
          html_children 
        
        check <- x %>% 
          html_nodes(paste0("body > div.super-container.light-icons > div.main-content.page.archive-page > div > div > div > div > article:nth-child(",j,") > div > footer > ul > li.title.not-truncate > a")) %>% html_text() %>% substr(2, 4) %>% as.numeric()
        
        url <- substr(htmlNode, gregexpr('"', htmlNode)[[1]][1]+1, gregexpr('"', htmlNode)[[1]][2]-1)
        
        
        
        
        if (check > 53 & check < 79) { #2017 episodes     
          
          content <- read_html(url) 
          post_id <- content %>%   html_nodes("body") %>% html_attr("class") %>% {gsub("\\D", "", .)} 
          
          test_text<- content %>% 
            html_nodes(paste0("#post-",post_id," > div ")) %>% html_text() %>% strsplit(" ") %>% unlist
          
          test_text <- test_text[which(grepl('Transcript', test_text))+1:length(test_text)] 
          
          text_df <- data.frame(ep = check, test_text)
          out = rbind(out,text_df)
        } 
        
      }
        
        
        
        
    }
  }
  



out$word <- str_replace_all(out$test_text, "‘|“|\\.|\\,|\\?|\\:|\"|\\!|\\`|/","")
out$word <- str_replace_all(out$word,"\n"," ")
out$word <- iconv(out$word, "latin1", "ASCII", sub="")
out <- out[c(1,3)] %>%   mutate(word = strsplit(as.character(word), " ")) %>%  unnest(word)
out$word <- tolower(out$word)

out <- out[complete.cases(out), ]

out %>% filter(word == '[laughter]') %>% group_by(ep) %>% summarise(n = n()) %>% ggplot(aes(x = ep, y = n)) + geom_col(fill = "#EF4A62") + theme_hc() + xlab('Episode') + ylab('Number of laughs') + ggtitle("The funniest episode?")

out %>% filter(grepl("fuck", word)) %>% group_by(ep) %>% summarise(n = n()) %>% ggplot(aes(x = ep, y = n)) + geom_col(fill = "#3FA0D9") + theme_hc() + xlab('Episode') + ylab('Number of #$%^') + ggtitle("Not for children!!")

out %>% filter(grepl("machine|^ai$|artificial", word)) %>% group_by(ep) %>% summarise(n = n()) %>% ggplot(aes(x = ep, y = n)) + geom_col(fill = "#39308A") + theme_hc() + xlab('Episode') + ylab('Machine learnings / ai mentions') + ggtitle("Machines taking over the world")

out %>% filter(grepl("mobile", word)) %>% group_by(ep) %>% summarise(n = n()) %>% ggplot(aes(x = ep, y = n)) + geom_col(fill = "#39308A") + theme_hc() + xlab('Episode') + ylab('Machine learnings / ai mentions') + ggtitle("Was 2017 year of mobile?")

out %>% filter(grepl("^adobe$|^google$", word)) %>% group_by(ep,word) %>% summarise(n = n()) %>% ggplot(aes(x = ep, y = n, fill = word)) + geom_col() + theme_hc() + xlab('Episode') + ylab('Machine learnings / ai mentions') + ggtitle("Google vs adobe") + scale_fill_manual(values = c("#2B2047","#FFC519"))

out <- out %>%  filter(!word %in% stop_words$word & !word %in%  c('mh','jn','sa','tw','mk','cb','im','ar','yeah','youre','[chuckle]','[laughter]','gonna','dont','ive','[music]','ss','isnt','tim','moe','helbling','jd','youve','bit','lot','whos','ago','hes','shes','doesnt','michael','wilson','theyre','wanna','mg')) 
out %>% group_by(ep,word) %>% summarise(n = n()) %>% ungroup() %>% group_by(ep) %>% filter(n == max(n)) 
forCloud <- out %>% group_by(word) %>% summarise(n = n())
wordcloud(forCloud$word, forCloud$n, min.freq=60,colors=brewer.pal(6, "Dark2"))
