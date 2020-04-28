#Build a dataset of articles/books/authors under this tag.
#Record the number of citations for each entry.
#Give me a boxplot of the logged number of citations.
#Go through multiple pages. You might be stopped.

#Jacob's example
gs_url<-"https://scholar.google.com/scholar?hl=en&as_sdt=7%2C26&q=political+parties&btnG="
pg1<-gs_url%>%
  read_html()%>%
  html_nodes(".gs_ri a , .gs_a , .gs_rt") %>%
  html_text()
pg1

#need to get first page on here somehow though
#making a vector that will give us results of first 120 hits
urls = NULL
partyResults <- NULL
for(i in 1:10){
  urls[i] <- paste0("https://scholar.google.com/scholar?start=", i, "0&q=political+parties&hl=en&as_sdt=7,26")
}
urls

#make a function that will read the necessary info

partyResults <- NULL
partyScraper <- function(urls){
  partyResults[i] <- url[i] %>%
    read_html()%>%
    html_nodes(".gs_ri a , .gs_a , .gs_rt") %>%
    html_text()
}

#reading in first 10 pages
partyResults <- list()
for(i in c(1:10)){
  if (i == 1){
    thisUrl <- "https://scholar.google.com/scholar?hl=en&as_sdt=7%2C26&q=political+parties&btnG="
    partyResults[[i]] <- read_html(thisUrl) %>%
      html_nodes(".gs_rt a , b , .gs_or_cit+ a , .gs_a") %>%
      html_text()
  } else {
    thisUrl <- paste0("https://scholar.google.com/scholar?start=", i-1, "0&q=political+parties&hl=en&as_sdt=7,26")
    partyResults[[i-1]] <- read_html(thisUrl) %>%
      html_nodes(".gs_or_cit+ a , #kKs31mjMKgsJ , #wbhgkYkjxsUJ") %>%
      html_text()
    }
}
str(partyResults)
partyResults[1]

#############################

rm(list=ls())

gs_url<-"https://scholar.google.com/scholar?hl=en&as_sdt=7%2C26&q=political+parties&btnG="
titles<-gs_url%>%
  read_html()%>%
  html_nodes(".gs_rt a") %>%
  html_text()

citations<-gs_url%>%
  read_html()%>%
  html_nodes(".gs_scl~ .gs_scl+ .gs_scl .gs_or_cit+ a , .gs_qsuggest_wrap+ .gs_scl .gs_nph+ a , .gs_ri:nth-child(1) .gs_or_cit+ a") %>%
  html_text() 
for (i in 1:100) {
  gs_url_2 = paste0("https://scholar.google.com/scholar?start=", i ,"0&hl=en&as_sdt=7%2C26&q=political+parties&btnG=")
  titles2<-gs_url_2%>%
    read_html()%>%
    html_nodes(".gs_rt a") %>%
    html_text()
  citations2<-gs_url_2%>%
    read_html()%>%
    html_nodes(".gs_scl~ .gs_scl+ .gs_scl .gs_or_cit+ a , .gs_qsuggest_wrap+ .gs_scl .gs_nph+ a , .gs_ri:nth-child(1) .gs_or_cit+ a") %>%
    html_text()
  citations = append(citations,citations2)
  titles = append(titles,titles2)
}

num_citations = extract_numeric(citations)

log_citations = log(num_citations)

boxplot(log_citations,main = "Log-citations of political parties google scholar search",ylab = 'Log Citations')

