library(rvest)
library(stringr)
library(tidytext)
library(tibble)
library(dplyr)
library(magrittr)
library(purrr)
library(markovchain)

#this code from Day 1. Start.
year_month <- c(201701:201712)

getURLs <- function(i){
  str_c('http://fss.ru/ru/news/270221/index.shtml?', i)
}

urls <- lapply(year_month, getURLs)
urls_news <- vector('character', 0)
for(i in seq_along(urls)){
  x <- read_html(urls[[i]]) %>% 
    html_nodes('ul') %>% 
    html_nodes('a') %>% 
    html_attr('href')
  y <- x[grep('news', x)]
  urls_news <- c(urls_news, y)
}

create_urls_news <- paste0('http://fss.ru', urls_news)
newsletter <- vector('character', 0)

for(i in seq_along(create_urls_news)){
  x <- read_html(create_urls_news[[i]]) %>% 
    html_nodes('p') %>% 
    html_text() %>% 
    .[-1] %>% 
    unlist
  newsletter <- c(newsletter, x)
}
#this code from Day 1. End.

newsletter %<>% 
  str_replace_all('\\r\\n', ' ') %>% 
  str_remove('http.+')

newsletter <- newsletter[str_length(newsletter) > 2]

english_df <- tibble(text = character(0))

#translates the text from russian to english without Google API
#code for this function was found on the stackoverflow.com and wrapped in a function.
get_english <- function(val){
  require(RCurl)
  require(XML)
  getParam = val
  translateFrom = "ru"
  translateTo = "en"
  search <- gsub(" ", "%20", getParam)
  URL <- paste("https://translate.google.pl/m?hl=",translateFrom,"&sl=",translateFrom,"&tl=",translateTo,"&ie=UTF-8&prev=_m&q=",search,sep="")
  page <- getURL(URL)
  tree <-htmlTreeParse(page)
  body <- tree$children$html$children$body
  tryCatch(
    body_text <- body$children[[5]]$children[[1]],
    error = function(e) return('')
  )
  return(body_text)
}

df_rus_to_eng <- function(val){
  text <- as.character(get_english(newsletter[val]))[6]
  df <- tibble(text = text)
  english_df <<- rbind.data.frame(english_df, df)
}

walk(1:length(newsletter), df_rus_to_eng)
#save(english_df, file = paste0(getwd(),'/Day_2/english_df.RData'))

clean_df <- english_df %>% 
  str_replace_all("[[:punct:]]", "") %>% 
  str_split(' ') %>% 
  unlist()

markov_fit <- markovchainFit(data = clean_df[1:5000])

vec <- vector('character', 0)
for(i in 1:50){
  vec <- c(vec, c(paste(markovchainSequence(n = sample(4:6, 1), markovchain=markov_fit$estimate), collapse=' ')))
}
write(vec, paste0(getwd(),'/Day_2/markov_chain.txt'))
