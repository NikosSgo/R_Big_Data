library(xlsx)

data_xlsx <- read.xlsx("data.xlsx", 1)
data_csv <- read.csv("data.csv")

data_xlsx_stat <- data.frame(
  Min = sapply(data_xlsx[,-1], min, na.rm = TRUE),
  Max = sapply(data_xlsx[,-1], max, na.rm = TRUE),
  Mean = sapply(data_xlsx[,-1], mean, na.rm = TRUE)
)
data_csv_stat <- data.frame(
  Min = sapply(data_csv[,-1], min, na.rm = TRUE),
  Max = sapply(data_csv[,-1], max, na.rm = TRUE),
  Mean = sapply(data_csv[,-1], mean, na.rm = TRUE)
)


print(data.frame(Greater_07 = colSums(data_xlsx[,-1]>0.7), data_xlsx = colSums(data_xlsx[,-1]<0.3)))
print(sort(apply(data_xlsx[,-1],2, mean),decreasing = TRUE))

print(data.frame(Greater_07 = colSums(data_csv[,-1]>0.7), data_csv = colSums(data_csv[,-1]<0.3)))
print(sort(apply(data_csv[,-1],2, mean),decreasing = TRUE))

print(data_xlsx_stat)
print(data_csv_stat)

barplot(colMeans(data_xlsx[, -1], na.rm = TRUE), 
        main = "Средние оценки телефонов", 
        ylab = "Оценка", 
        col = rainbow(10), 
        las = 2)
barplot(colMeans(data_xlsx[, -1], na.rm = TRUE), 
        main = "Средние оценки телефонов (горизонтально)", 
        horiz = TRUE, 
        col = heat.colors(10), 
        las = 1)

barplot(colMeans(data_csv[, -1], na.rm = TRUE), 
        main = "Средние оценки фирм производителей машин", 
        ylab = "Оценка", 
        col = rainbow(10), 
        las = 2)
barplot(colMeans(data_csv[, -1], na.rm = TRUE), 
        main = "Средние оценки фирм производителей машин (горизонтально)", 
        horiz = TRUE, 
        col = heat.colors(10), 
        las = 1)

# Формирование поднабора данных
# Допустим, нас интересуют студенты, оценившие фирму Chevrolet более чем 0.3
subdata_chevrolet <- subset(data_csv, Chevrolet > 0.3)

# Выводим поднабор данных
print("Поднабор данных: студенты с оценкой Chevrolet > 0.3")
print(subdata_chevrolet)

# Подсчёт размерностей поднабора данных
dims <- dim(subdata_chevrolet)
cat("Размерности поднабора (строки, столбцы):", dims[1], "x", dims[2], "\n")

# Анализ поднабора данных

# Гистограмма для оценок Chevrolet
hist(subdata_chevrolet$Chevrolet,
     main = "Гистограмма оценок Chevrolet (> 0.3)",
     xlab = "Оценка",
     col = "lightblue",
     breaks = 5)

# Боксплот для оценок Chevrolet
boxplot(subdata_chevrolet$Chevrolet,
        main = "Боксплот оценок Chevrolet (> 0.3)",
        ylab = "Оценка",
        col = "orange")

# Вычисление центральных мер
mean_chevrolet <- mean(subdata_chevrolet$Chevrolet)
median_chevrolet <- median(subdata_chevrolet$Chevrolet)
sd_chevrolet <- sd(subdata_chevrolet$Chevrolet)
min_chevrolet <- min(subdata_chevrolet$Chevrolet)
max_chevrolet <- max(subdata_chevrolet$Chevrolet)

cat("\nСтатистические показатели для оценок Chevrolet (> 0.3):\n")
cat("Среднее значение:", mean_chevrolet, "\n")
cat("Медиана:", median_chevrolet, "\n")
cat("Стандартное отклонение:", sd_chevrolet, "\n")
cat("Минимум:", min_chevrolet, "\n")
cat("Максимум:", max_chevrolet, "\n")

# Формирование поднабора данных
# Допустим, нас интересуют студенты, оценившие фирму Xiaomi более чем 0.3
subdata_xiaomi <- subset(data_xlsx, Xiaomi > 0.3)

# Выводим поднабор данных
print("Поднабор данных: студенты с оценкой Xiaomi > 0.3")
print(subdata_xiaomi)

# Подсчёт размерностей поднабора данных
dims <- dim(subdata_xiaomi)
cat("Размерности поднабора (строки, столбцы):", dims[1], "x", dims[2], "\n")

# Анализ поднабора данных

# Гистограмма для оценок Xiaomi
hist(subdata_xiaomi$Xiaomi,
     main = "Гистограмма оценок Xiaomi (> 0.3)",
     xlab = "Оценка",
     col = "lightblue",
     breaks = 5)

# Боксплот для оценок Xiaomi
boxplot(subdata_xiaomi$Xiaomi,
        main = "Боксплот оценок Xiaomi (> 0.3)",
        ylab = "Оценка",
        col = "orange")

# Вычисление центральных мер
mean_xiaomi <- mean(subdata_xiaomi$Xiaomi)
median_xiaomi <- median(subdata_xiaomi$Xiaomi)
sd_xiaomi <- sd(subdata_xiaomi$Xiaomi)
min_xiaomi <- min(subdata_xiaomi$Xiaomi)
max_xiaomi <- max(subdata_xiaomi$Xiaomi)

cat("\nСтатистические показатели для оценок Xiaomi (> 0.3):\n")
cat("Среднее значение:", mean_xiaomi, "\n")
cat("Медиана:", median_xiaomi, "\n")
cat("Стандартное отклонение:", sd_xiaomi, "\n")
cat("Минимум:", min_xiaomi, "\n")
cat("Максимум:", max_xiaomi, "\n")
