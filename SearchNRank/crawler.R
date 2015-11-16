"#Because XML does not get https, get the doc through RCurl and then let XML do its magic. 
#http://www.omegahat.org/RCurl/installed/RCurl/html/getURL.html
library(RCurl) 
library(XML)
"
library(magrittr)
library(rvest)
library(RMySQL)

crawl <- function(seed.page = "/wiki/Universe", n=2){
  eng.wiki <- "https://en.wikipedia.org"
  wiki.univ.links <- c(paste(eng.wiki, seed.page, sep=""))
  new.links <- wiki.univ.links
  for(i in seq(1,n)){
    all.links <- c()
    for(link in new.links){
      step.links <- read_html(link) %>% html_nodes(xpath = "//html/body/div[@id = 'content']/div[@id = 'bodyContent']//a" ) %>% html_attr('href') %>% grep("^/wiki/*", value=TRUE, x=.)
      #no disambiguation pages as of now. 
      step.links <- step.links[!grepl("disambiguation",step.links)]
      step.links <- step.links[!grepl(":", step.links)] %>% unique
      all.links <- c(all.links, paste(eng.wiki, step.links, sep="")) %>% unique
    }
    new.links <- all.links
    wiki.univ.links <- c(wiki.univ.links, all.links) %>% unique
  }
  #For the next depth, maybe use plyr and domc?   
  wiki.univ.links.df <- as.data.frame(wiki.univ.links)
  names(wiki.univ.links.df) = c("url")
  return(wiki.univ.links.df)
}

store.urlwords <- function(values.df){ #should have a column named url
  ignore.words <- c("the","of","to","and","a","in","is","it")
  con <- dbConnect(MySQL(), user = '<username>', password = '<password>', host = 'localhost', dbname='wikipedia') 
  success <- dbWriteTable(conn=con, name='urls', value=values.df, append = TRUE, row.names = FALSE, quote='\'')
  if(success){
    for(url in values.df$url){
      text <- read_html(url) %>% html_nodes(xpath = "//html/body/div[@id = 'content']/div[@id = 'bodyContent']//div[@id = 'mw-content-text']") %>% html_text() %>% gsub(pattern="[\n\t]+",replacement=" ",x=.) %>% gsub(pattern="[(),;':@#$%^&*\"\\]", replacement="", x=.)
      b <- as.data.frame(unlist(strsplit(text,split=" ")))
      names(b) <- c("word")
      b <- subset(b, (!b$word %in% ignore.words))
      success <- success & dbWriteTable(conn=con, name="words", value = b, append=TRUE, row.names=FALSE, quote='\'')
    }  
  }
  return(success)
}

print(Sys.time())
op <- crawl()
print(Sys.time())
suc <- store.urlwords(op)
print(Sys.time())