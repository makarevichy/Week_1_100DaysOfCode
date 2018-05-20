library(rvest)
library(stringr)
library(stopwords)
library(tidytext)
library(wordcloud)
library(tibble)
library(dplyr)
library(RColorBrewer)

year_month <- c(201701:201712)

getURL <- function(i){
  str_c('http://fss.ru/ru/news/270221/index.shtml?', i)
}

urls <- lapply(year_month, getURL)

urls_news <- vector('character', 0)

t <- Sys.time()
for(i in seq_along(urls)){
  x <- read_html(urls[[i]]) %>% 
    html_nodes('ul') %>% 
    html_nodes('a') %>% 
    html_attr('href')
  y <- x[grep('news', x)]
  urls_news <- c(urls_news, y)
}
print(difftime(Sys.time(), t, units = 'secs'))

create_urls_news <- paste0('http://fss.ru', urls_news)

newsletter <- vector('character', 0)

t <- Sys.time()
for(i in seq_along(create_urls_news)){
  x <- read_html(create_urls_news[[i]]) %>% 
    html_nodes('p') %>% 
    html_text() %>% 
    .[-1] %>% 
    unlist
  newsletter <- c(newsletter, x)
}       
print(difftime(Sys.time(), t, units = 'secs'))

word_cloud <- newsletter %>% 
  tibble(txt = .) %>% 
  unnest_tokens(word, txt, to_lower = T) %>% 
  anti_join(tibble(word = c('также', 'ссылке', 'читайте', 'это', 'году', 'года', 'руб',
                            '1', '2', '1396', '24.12.2012', 'https', 'млн', 'тыс', 'г',
                            'http', 'далее', stopwords('ru')))) %>% 
  count(word, sort = T)

# min.freq = 29
png(paste0(getwd(),'/Day_1/cloud.png'), width = 12, height = 8, units = 'in', res = 300)
wordcloud(toupper(word_cloud$word), word_cloud$n, c(1.3,.2), min.freq = 29,
          colors = brewer.pal(6, "Dark2"), random.order = FALSE)
dev.off()
