library(rvest)
library(xml2)
url <- "https://kudamoscow.ru/place/museum/"

page <- read_html(url)

place_items <- html_elements(page, ".place_item")

urls <- html_attr(html_elements(place_items, "a"),"href")

places_pages <- lapply(seq_along(urls), function(i) { 
  cat(sprintf("Загрузка %d страницы c url '%s' ...", i, urls[[i]]))
  page <- read_html(urls[i])
  cat(" Готово!\n")
  page
})

places_names <- lapply(places_pages, function(page){
    html_text(html_elements(page, "h1"))
})

places_addresses <- lapply(places_pages, function(page){
  html_text(html_elements(page,".info p"))[1]
})


museums_df <- data.frame(
  name = unlist(places_names),
  address = unlist(places_addresses),
  url = unlist(urls),
  stringsAsFactors = FALSE
)