library(xlsx)
preferences = read.xlsx("data.xlsx",1)

summary_stats <- data.frame(
  Max = apply(preferences[, -1], 2, max),
  Min = apply(preferences[, -1], 2, min),
  Mean = apply(preferences[, -1], 2, mean)
)
print(summary_stats)

print(preferences)

print(data.frame(Greater_07 = colSums(preferences[,-1]>0.7), Less_03 = colSums(preferences[,-1]<0.3)))

print(sort(apply(preferences[,-1],2, mean),decreasing = TRUE))

xiaomi_lovers <- preferences[preferences$Xiaomi > 7,]
print(xiaomi_lovers)


barplot(colMeans(preferences[, -1], na.rm = TRUE), 
        main = "Средние оценки брендов телефонов", 
        ylab = "Оценка", 
        col = rainbow(10), 
        las = 2)
barplot(colMeans(preferences[, -1], na.rm = TRUE), 
        main = "Средние оценки брендов телефонов", 
        horiz = TRUE, 
        col = heat.colors(10), 
        las = 1)

