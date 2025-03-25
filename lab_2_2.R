library(xlsx)
# Импорт данных из CSV-файла
setwd("C:/university/3_course/2_semester/big_data/labs/git") 

preferences <- read.csv("data_csv.csv")

# гистогрммы для всех брендов
par(mfrow = c(3, 4), mar = c(5, 5, 5, 5))

sapply(names(preferences)[-1], function(col_name) {
  hist(preferences[[col_name]],
       main = col_name,
       xlab = "Оценка",
       ylab = "Количество",
       xlim = c(1, 10),
       breaks = seq(0.5, 10.5, by = 1),
       col = "lightblue")
})

par(mfrow = c(1, 1))


# боксплот
par(mar = c(12, 4, 4, 2))
boxplot(preferences[, -1], 
        main = "Боксплот оценок по брендам", 
        xlab = "Жанр", 
        ylab = "Оценка", 
        col = "lightgreen", 
        las = 2)  # Поворот подписей на 90 градусов

par(mar = c(5, 4, 4, 2))


# серединные меры
middle_answers = summary(preferences[,-1])
middle_answers


# создание и обработка поднаборов
# поднабор студентов > 7 Apple
subdataset_high <- preferences[preferences$Apple > 9, ]
subdataset_high
# поднабор студентов < 6 Xiaomi
subdataset_low <- preferences[preferences$Xiaomi < 6, ]
subdataset_low
# размерности поднаборов
cat("Размерность subdataset_high:", dim(subdataset_high), "\n")
cat("Размерность subdataset_low:", dim(subdataset_low), "\n")


# гистограммы для subdataset_high
par(mfrow = c(3, 4))
sapply(names(subdataset_high)[-1], function(col_name) {
  hist(subdataset_high[[col_name]],
       main = col_name,
       xlab = "Оценка",
       ylab = "Количество",
       xlim = c(1, 10),
       breaks = seq(0.5, 10.5, by = 1),
       col = "lightblue")
})


# гистограммы для subdataset_low
par(mfrow = c(3, 4))
sapply(names(subdataset_low)[-1], function(col_name) {
  hist(subdataset_low[[col_name]],
       main = col_name,
       xlab = "Оценка",
       ylab = "Количество",
       xlim = c(1, 10),
       breaks = seq(0.5, 10.5, by = 1),
       col = "lightblue")
})
par(mfrow = c(1, 1))


# Боксплот для subdataset_high
par(mar = c(12, 4, 4, 2))
boxplot(subdataset_high[, -1], 
        main = "Боксплот для высоких оценок apple (> 9)", 
        xlab = "Жанр", 
        ylab = "Оценка", 
        col = "skyblue", 
        las = 2)
par(mar = c(5, 4, 4, 2))

# Боксплот для subdataset_low
par(mar = c(12, 4, 4, 2))
boxplot(subdataset_low[, -1], 
        main = "Боксплот для низких оценок xiaomi (< 6)", 
        xlab = "Жанр", 
        ylab = "Оценка", 
        col = "lightgreen", 
        las = 2)
par(mar = c(5, 4, 4, 2))


# серединные меры для subdataset_high
cat("Серединные меры для subdataset_high:\n")
print(summary(subdataset_high[, -1]))

# серединные меры для subdataset_low
cat("\nСерединные меры для subdataset_low:\n")
print(summary(subdataset_low[, -1]))

# загружаем две таблицы и делаем слияние
preferences_1 <- read.xlsx("data_part_one.xlsx",1)

preferences_2 <- read.xlsx("data_part_two.xlsx",1)

# Слияние таблиц по столбцу "фамилия"
preferences_1
preferences_2
preferences_merged <- merge(preferences_1, preferences_2, by = "Фамилия")
preferences_merged

# загружаем еще таблицу и добавляем ее строки к нашей
prefernces_3 <- read.xlsx("data_part_three.xlsx",1)
prefernces_3
preferences_merged <- rbind(preferences_merged, prefernces_3)

# исключение переменных
myvars <- names(preferences_merged) %in% c("Oppo", "Vivo") 
new_answers <- preferences_merged[!myvars]
remove(myvars)

new_answers <- preferences_merged[c(-2,-3)]

# subset
new_answers <- subset(preferences_merged, Apple > 9 | Xiaomi < 6, select=c(Фамилия, Samsung, Oneplus)) 





