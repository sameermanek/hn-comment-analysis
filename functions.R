# Want to pull in the comments etc from hacker news
# Then try to categorize based on content of comments
library(data.table)
library(ggplot2)
library(RCurl)
library(stringr)
library(rjson)
library(plyr)
library(wordcloud)
library(tm)
library(SnowballC)
library(XML)
library(scales)

html2txt <- function(chr.str) {
  a <- vapply(chr.str, function(str) {
  xmlValue(
    xpathApply(
      htmlParse(
        paste0("<p> ",str,"</p>") # whitespace is intentional. In case of an empty comment
        , asText = TRUE), 
      "//body//text()")[[1]]) 
  }, character(1))
  names(a) <- NULL
  return(a)
}

# First, get listing of the articles.
# Will return a data.table with information about the articles
getArticles <- function(num.max = 2e6) {
  chr.url <- 'http://hckrnews.com/data'
  chr.listing <- readLines(chr.url)
  chr.listing <- str_match(chr.listing, '<a href="([a-zA-Z0-9]+.js)">')[, 2]
  chr.listing <- chr.listing[!is.na(chr.listing)]
  chr.listing <- paste0(chr.url, '/', chr.listing)
  int.count <- 0
  int.index <- 1
  chr.listing <- sample(chr.listing)
  
  while(int.count <= num.max && int.index <= length(chr.listing)) {
    cat(as.character(Sys.time()), ': ', int.count, ' listings, ', int.index, ' pages loaded', '\n', sep='')
    lst.posts <- tryCatch({
      fromJSON(readLines(chr.listing[int.index], warn = FALSE))
    }, error = function(e) {
      return(FALSE)
    })
    int.index <- int.index + 1
    if(is.logical(lst.posts) && lst.posts == FALSE) next;
    lst.posts <- lapply(lst.posts, function(x) {
      x <- lapply(x, function(y) if(is.null(y)) NA else y)
      do.call(data.frame, c(x, list(stringsAsFactors = FALSE)))
    })
    x <- data.table(rbind.fill(lst.posts))
    
    if(int.index == 2) dt.articles <- x else dt.articles <- rbind(dt.articles, x[1:pmin(nrow(x), num.max - int.count)], use.names = TRUE, fill = TRUE)
    int.count <- nrow(dt.articles)
  }
  return(dt.articles)
}

# Get a list of comments
getComments <- function(dt.articles = getArticles(100)) {
  chr.url.base <- "https://news.ycombinator.com/item?id="
  getComment <- function(int.id) {
    cat(int.id, '\n')
    chr.page <- getURL(paste0(chr.url.base, int.id))
    chr.comments <- str_match_all(chr.page, '<span class="comment"><font color="#[0-9abcdef]{6}">(.*?)</font></span>')[[1]]
    if(length(chr.comments) == 0) return(character(0)) 
    chr.comments <- chr.comments[, 2]
    chr.comments <- html2txt(chr.comments)
    return(chr.comments)
  }
  
  lst.comments <- lapply(dt.articles$id, getComment)
  names(lst.comments) <- dt.articles$id
  return(lst.comments)
}


# now do the processing. 
processAllComments <- function(lst.comments) {
  
  # First, unlist
  # Then I can examine using the tm package
  chr.comments <- vapply(lst.comments, paste0, character(1), collapse = "\n\n")
  a <- VCorpus(VectorSource(chr.comments))
  a <- tm_map(a, removePunctuation)
  a <- tm_map(a, removeWords, stopwords("english"))
  a <- tm_map(a, content_transformer(tolower))
  a <- tm_map(a, stemDocument) # kinda weird/illegible results. 
  
  # Now create a term-document matrix
  mat.freq <- DocumentTermMatrix(a)
  rownames(mat.freq) <- names(lst.comments)
  return(mat.freq)
}
