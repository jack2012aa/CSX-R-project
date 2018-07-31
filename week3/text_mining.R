library(bitops)
library(httr)
library(RCurl)
library(XML)
library(tm)
library(NLP)
library(tmcn)
library(jiebaRD)
library(jiebaR)
from <- 7424
to   <- 7434
prefix = "https://www.ptt.cc/bbs/Boy-Girl/index"
data <- list()
for( id in c(from:to) )
{
  url  <- paste0( prefix, as.character(id), ".html" )
  html <- htmlParse( GET(url) )
  url.list <- xpathSApply( html, "//div[@class='title']/a[@href]", xmlAttrs )
  #文章的網址
  data <- rbind( data, as.matrix(paste('https://www.ptt.cc', url.list, sep='')) )
}
data <- unlist(data)
head(data)
library(dplyr)
getdoc <- function(url)
{
  html <- htmlParse( getURL(url) )
  doc  <- xpathSApply( html, "//div[@id='main-content']", xmlValue )
  time <- xpathSApply( html, "//*[@id='main-content']/div[4]/span[2]", xmlValue )
  temp <- gsub( "  ", " 0", unlist(time) )
  part <- strsplit( temp, split=" ", fixed=T )
  timestamp <- part[[1]][4]
  timestamp <- strsplit( timestamp, split=":", fixed=T )
  hour <- timestamp[[1]][1]
  name <- paste0('~/Documents/GitHub/CSXsp/Week3/DATA/', hour, ".txt")
  write(doc, name, append = TRUE)
}
sapply(data, getdoc)
