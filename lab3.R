library(xlsx)
setwd("C:/university/3_course/2_semester/big_data/labs/lab3") 

df_man <- read.xlsx("dataset.xlsx", 1, check.names = FALSE)
df_woman <- read.xlsx("dataset.xlsx", 2, check.names = FALSE)

head(df_woman)

# Столбчатые диаграммы
df_woman_count <- apply(df_woman[,-1], 2, sum)
df_man_count <- apply(df_man[,-1], 2, sum)

par(mfrow = c(1, 2))

barplot(df_woman_count, 
        names.arg = colnames(df_woman)[-1], 
        col = "pink", 
        main = "Женщины", 
        xlab = "Места", 
        ylab = "Количество",
)

barplot(df_man_count, 
        names.arg = colnames(df_man)[-1], 
        col = "blue", 
        main = "Мужчины", 
        xlab = "Места", 
        ylab = "Количество")

# Круговая диаграмма

par(mfrow = c(1, 2))

years_woman <- df_woman$Год[df_woman[, 2] != 0]
years_man <- df_man$Год[df_man[, 2] != 0]


# Цвета для каждого года
legend_colors <- c("darkgreen", "blue", "red", "orange", "black", "grey", "purple")

# Круговая диаграмма для женщин
pie(df_woman[, 2][df_woman[,2]!=0], labels = df_woman[, 2][df_woman[,2]!=0], main = "Женщины", col = legend_colors[df_woman[,2] != 0])

legend("topright", 
       legend = years_woman,  # Годы из данных
       fill = legend_colors[df_woman[,2] != 0],   # Цвета для каждого года
       title = "Годы")         # Заголовок легенды

# Круговая диаграмма для мужчин
pie(df_man[, 2][df_man[,2]!=0], labels = df_man[, 2][df_man[,2]!=0], main = "Мужчины", col = legend_colors[df_man[,2] != 0])

# Легенда с годами и цветами
legend("topright", 
       legend = years_man,    # Годы из данных
       fill = legend_colors[df_man[,2] != 0],   # Цвета для каждого года
       title = "Годы")         # Заголовок легенды



# Функциональные графики
plot_graph <- function(df, color, title) {
  total_places <- rowSums(df[, 2:4], na.rm = TRUE)
  matplot(df$Год, total_places,main = title,  type = "b", pch = 19, lty = 1, col = color,
          xlab = "Год", ylab = "Количество мест", xaxt = "n", 
          ylim = c(0, max(rowSums(df[, 2:4])+0.5)))
  axis(1, at = df$Год, labels = df$Год)
}

par(mfrow = c(2, 1))

plot_graph(df_woman, "red", "Тенденции изменения кол-ва призовых мест для женщин за последние 30 лет")
plot_graph(df_man, "blue", "Тенденции изменения кол-ва призовых мест для мужчин за последние 30 лет")

# 3 Графики изменения спортивных достижений (золото)
df_gold <- read.xlsx("dataset.xlsx", 3)

par(mar = c(5, 5, 5, 12)) 
matplot(df_gold$Год, 
        df_gold[, -1], 
        main = "Изменение кол-ва 1-х мест по 7-ми странам-призерам\n за последние 6 олимпиад", 
        type = "b", pch = 19, 
        lty = 1, 
        col = legend_colors,
        xlab = "Год", ylab = "Количество мест (золото)", 
        xaxt = "n", 
        ylim = c(0, max(df_gold[, -1]+10)))
axis(1, at = df_gold$Год, labels = df_gold$Год)
legend("topright", legend = colnames(df_gold)[-1], xpd = TRUE, bty = "n", inset = -c(0.4, 0), fill = legend_colors)


# 4 Графики изменения спортивных достижений (все призовые)
df_prize_places <- read.xlsx("dataset.xlsx", 4)

par(mar = c(5, 5, 5, 12)) 
matplot(df_prize_places$Год, 
        df_prize_places[, -1], 
        main = "Изменение кол-ва призовых мест по 7-ми странам-призерам\n за последние 6 олимпиад", 
        type = "b", pch = 19, 
        lty = 1, 
        col = legend_colors,
        xlab = "Год", ylab = "Количество мест (призовые)", 
        xaxt = "n", 
        ylim = c(0, max(df_prize_places[, -1]+10)))
axis(1, at = df_prize_places$Год, labels = df_prize_places$Год)
legend("topright", legend = colnames(df_prize_places)[-1], xpd = TRUE, bty = "n", inset = -c(0.35, 0), fill = legend_colors)


# Динамика cпортивной гимнастики по всем за последние 6 Олимпиад
total_places1 <- rowSums(df_woman[-7, 2:4], na.rm = TRUE)
total_places2 <- rowSums(df_man[-7, 2:4], na.rm = TRUE)
total_places_all <- cbind(total_places1, total_places2)

par(mfrow = c(1, 3), oma = c(0, 0, 2, 0))

matplot(df_woman[-7,]$Год, 
        total_places_all, 
        type = "b", pch = 19, 
        lty = c(1, 6, 3),
        col = c("blue", "darkgreen"),
        xlab = "Год", ylab = "Количество мест (призовые)", 
        xaxt = "n", 
        lwd = 3,
        ylim = c(0, max(total_places_all)+0.5))
axis(1, at = df_woman[-7,]$Год, labels = df_woman[-7,]$Год)
legend('topleft', c('Мужчины', 'Женщины'), lty = c(1, 6, 3), cex = 0.8, lwd=4, col = c("blue", "darkgreen"))


total_places_barplot <- cbind(df_woman[-7,1],total_places1, total_places2)
colnames(total_places_barplot) <- c("Год", "Женщины", "Мужчины")
total_places_barplot_t <- t(total_places_barplot)
total_places_barplot_t_sorted <- total_places_barplot_t[, order(total_places_barplot_t[1, ])]
barplot(as.matrix(total_places_barplot_t_sorted[-1,]), 
        beside = TRUE,
        col = c("darkgreen", "blue"), 
        xlab = "Год", 
        names.arg = total_places_barplot_t_sorted[1,],
        ylab = "Количество мест (призовые)", 
        ylim = c(0,max(total_places_barplot_t_sorted[2]+0.5,total_places_barplot_t_sorted[3]+0.5)),
        cex.names = 0.8)
legend('topright', c('Женщины', 'Мужчины'), fill=c("darkgreen", "blue"))



total_places_pie <- rowSums(total_places_barplot_t[-1,])
  
pie(total_places_pie, 
    labels=c(total_places_pie),
    col=c("darkgreen", "blue"), 
    sub="Кол-во призовых мест за 6 Олимпиад\n (женщины, мужчины)")
legend('topright', c('Женщины', 'Мужчины'), fill=c("darkgreen", "blue"))

mtext("Общее количество призовых мест на 6 Олимпиадах по спортивной гимнастике", 
      outer = TRUE, cex = 1, font = 2)
