#The automated download of HTML pages is called crawling 
#while the extraction of the textual data and/or metadata (for example, article date, headlines, author names, article text) from the 
#HTML source code (or the DOM document object model of the website) is called scraping

#Preparation and Session Set Up

# install packages
install.packages("rvest")
install.packages("readtext")
install.packages("webdriver")
install.packages("tidyverse")
install.packages("readtext")
install.packages("flextable")
install.packages("webdriver")
webdriver::install_phantomjs()
# install klippy for copy-to-clipboard button in code chunks
install.packages("remotes")
webdriver::install_phantomjs()
remotes::install_github("rlesur/klippy")

# Load Packages
library(tidyverse)
library(rvest)
library(readtext)
library(flextable)
library(webdriver)
# activate klippy for copy-to-clipboard button
klippy::klippy()



# Define url
url <- "https://www.theguardian.com/world/2017/jun/26/angela-merkel-and-donald-trump-head-for-clash-at-g20-summit"
# download content
webc <- rvest::read_html(url)
# inspect
webc

#Pipelined

#Paragraphed

webc %>%
  # extract paragraphs
  rvest::html_nodes("p") %>%
  # extract text
  rvest::html_text() -> webtxt
# inspect
head(webtxt)

#Headlined

webc %>%
  # extract paragraphs
  rvest::html_nodes("h1") %>%
  # extract text
  rvest::html_text() -> header
# inspect
head(header)

pjs_instance <- run_phantomjs()
pjs_session <- Session$new(port = pjs_instance$port)



url <- "https://www.theguardian.com/world/angela-merkel"
# go to URL
pjs_session$go(url)
# render page
rendered_source <- pjs_session$getSource()
# download text and parse the source code into an XML object
html_document <- read_html(rendered_source)

links <- html_document %>%
  html_nodes(xpath = "//div[contains(@class, 'fc-item__container')]/a") %>%
  html_attr(name = "href")
# inspect 
links


page_numbers <- 1:3
base_url <- "https://www.theguardian.com/world/angela-merkel?page="
paging_urls <- paste0(base_url, page_numbers)
# inspect
paging_urls


all_links <- NULL
for (url in paging_urls) {
  # download and parse single overview page
  pjs_session$go(url)
  rendered_source <- pjs_session$getSource()
  html_document <- read_html(rendered_source)
  # extract links to articles
  links <- html_document %>%
    html_nodes(xpath = "//div[contains(@class, 'fc-item__container')]/a") %>%
    html_attr(name = "href")
  
  # append links to vector of all links
  all_links <- c(all_links, links)
}



scrape_guardian_article <- function(url) {
  # start PhantomJS
  pjs_session$go(url)
  rendered_source <- pjs_session$getSource()
  # read raw html
  html_document <- read_html(rendered_source)
  # extract title
  title <- html_document %>%
    rvest::html_node("h1") %>%
    rvest::html_text(trim = T)
  # extract text
  text <- html_document %>%
    rvest::html_node("p") %>%
    rvest::html_text(trim = T)
  # extract date
  date <- url %>%
    stringr::str_replace_all(".*([0-9]{4,4}/[a-z]{3,4}/[0-9]{1,2}).*", "\\1")
  # generate data frame from results
  article <- data.frame(
    url = url,
    date = date,
    title = title,
    body = text
  )
  
  return(article)
  
}



# create container for loop output
all_articles <- data.frame()
# loop over links
for (i in 1:length(all_links)) {
  # print progress (optional)
  #cat("Downloading", i, "of", length(all_links), "URL:", all_links[i], "\n")
  # scrape website
  article <- scrape_guardian_article(all_links[i])
  # append current article data.frame to the data.frame of all articles
  all_articles <- rbind(all_articles, article)
}
all_articles



write.table(all_articles, here::here("data", "all_articles.txt"), sep = "\t")