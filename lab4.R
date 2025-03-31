library(stringi)
library(rvest)

years <- 2014:2021
countries <- c("Canada", "United States", "Turkey", "Greece", "Israel")
colors <- c("red", "blue", "green", "purple", "orange")

getAllData <- function(year){
  cat(sprintf("Загрузка данных о странах за %d год ... ", year))
  url <- sprintf("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=%d", year)
  page <- read_html(url)
  table_node <- html_nodes(page, "table#t2")[[1]]
  table <- as.data.frame(html_table(table_node, na.strings="-"))
  table <- table[-1]
  cat("Готово\n")
  return(table)
}

allData <- lapply(years, getAllData)
names(allData) <- years
CountriesData <- lapply(allData,subset, Country %in% countries)

#Подготовка данных для 
quality_assessment_list <- lapply(2:ncol(CountriesData[[1]]),function(i){
  
  df = data.frame(
  )
  
  result_list <- lapply(years, function(year){
    
    row <- t(CountriesData[[as.character(year)]][i])
    rownames(row) = year
    colnames(row) = t(CountriesData[[as.character(year)]][1])
    row
  })
  
  df <- do.call(rbind, result_list)
  df
  
})


mains = c('Индекс качества жизни (чем выше, тем лучше)',
         'Индекс покупательной способности (чем выше, тем лучше)',
         'Индекс безопасности (чем выше, тем лучше)',
         'Индекс медицинского обслуживания (чем выше, тем лучше)',
         'Индекс прожиточного минимума (чем ниже, тем лучше)',
         'Отношение цены на жильё к доходу (чем ниже, тем лучше)',
         'Индекс времени движения на дороге (чем ниже, тем лучше)',
         'Индекс загрязнения (чем ниже, тем лучше)',
         'Климатический индекс (чем выше, тем лучше)'
        )

quality_assessment_plot <- function(quality_df, main){
  
  min <- min(as.matrix(quality_df), na.rm = TRUE)
  max <- max(as.matrix(quality_df), na.rm = TRUE)
  print(main)
  print(quality_df)
  
  matplot(
    years,
    quality_df,
    type="b",
    pch=16,
    lty=1,
    lwd=1.8,
    cex=0.8,
    col=colors,
    ylim=c(min - 10, max + 50),
    main= main,
    xlab='Год',
    ylab=strsplit(main, " (", fixed = TRUE)[[1]][1]
  )
  legend('topleft', colnames(quality_df), ncol=3, lty=1, lwd=2, col=colors)
}

lapply(seq_along(quality_assessment_list) , function(i){
  quality_assessment_plot(quality_assessment_list[[i]],mains[i])
})