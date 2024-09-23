# Напишите функцию smart_hclust, которая получает на вход dataframe  с произвольным
# числом количественных переменных и число кластеров, которое необходимо выделить при
# помощи иерархической кластеризации.
# 
# Функция должна в исходный набор данных добавлять новую переменную фактор - cluster 
# -- номер кластера, к которому отнесено каждое из наблюдений.
# 
# Пример работы функции:
# 
# 
# > test_data <- read.csv("https://stepik.org/media/attachments/course/524/test_data_hclust.csv")
# > str(test_data)
# 'data.frame':  12 obs. of  5 variables:
#  $ X1: int  11 9 9 9 7 9 16 23 15 19 ...
#  $ X2: int  7 10 2 11 9 11 20 18 21 20 ...
#  $ X3: int  10 10 12 8 10 9 22 21 14 15 ...
#  $ X4: int  10 8 14 10 11 6 19 24 21 17 ...
#  $ X5: int  8 6 11 3 14 9 16 16 21 17 ...
# 
# > smart_hclust(test_data, 3) # выделено три кластера
#    X1 X2 X3 X4 X5 cluster
# 1  11  7 10 10  8       1
# 2   9 10 10  8  6       1
# 3   9  2 12 14 11       1
# 4   9 11  8 10  3       1
# 5   7  9 10 11 14       1
# 6   9 11  9  6  9       1
# 7  16 20 22 19 16       2
# 8  23 18 21 24 16       2
# 9  15 21 14 21 21       3
# 10 19 20 15 17 17       3
# 11 20 24 21 20 19       2
# 12 22 19 27 22 19       2
# 
# В этой и следующей задаче на кластерный анализ предполагается, что мы используем 
# функцию hclust() для
# ﻿кластеризации данных с параметрами ﻿по умолчанию:
#   
# hclust(d, method = "complete", members = NULL)
# 
# Для расчета матрицы расстояний предполагается, что используется функция dist() 
# также с параметрами по умолчанию:
# 
# dist(x, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
# 
# Для выделения желаемого числа кластеров по результатам иерархической кластеризации 
# воспользуйтесь функцией cutree().
# 
# Иными словами, для кластеризации данных swiss на три кластера мы бы использовали команды:
# dist_matrix <- dist(swiss) # расчет матрицы расстояний
# fit <- hclust(dist_matrix) # иерархическая кластеризация
# cluster <- cutree(fit, 3) # номер кластера для каждого наблюдения

df <- read.csv("https://stepik.org/media/attachments/course/524/test_data_hclust.csv")

smart_hclust <- function(df, cluster_count){
  dist_matrix <- dist(df, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
  fit <- hclust(dist_matrix, method = "complete", members = NULL)
  df$cluster <- as.factor(cutree(fit, cluster_count))
  return(df)
}

smart_hclust(df, 3)








# Интересной особенностью кластерного анализа является тот факт, что мы получаем только
# итоговый ответ, к какому кластеру принадлежит каждое наблюдение. Однако мы не знаем,
# по каким переменным различаются выделенные кластеры. Поэтому, если нас интересует не
# только сам факт того, что мы смогли выделить кластеры в наших данных, но мы также хотим
# понять, чем же они различаются, разумно сравнить кластеры между собой по имеющимся переменным.
# 
# Напишите функцию get_difference, которая получает на вход два аргумента: 
#   
#   test_data — набор данных с произвольным числом количественных переменных.
# n_cluster — число кластеров, которое нужно выделить в данных при помощи иерархической
# кластеризации.
# Функция должна вернуть названия переменных, по которым были обнаружен значимые различия
# между выделенными кластерами (p < 0.05)﻿. Иными словами, после того, как мы выделили
# заданное число кластеров, мы добавляем в исходные данные новую группирующую переменную
# — номер кластера, и сравниваем получившиеся группы между собой по количественным
# переменным при помощи дисперсионного анализа.
# 
# Пример работы функции:
#   
#   В первом наборе данных, очевидно, что два кластера будут значимо различаться только
# по переменной V2.


install.packages("png")
library("png")
pp <- readPNG("imgs/img_9.png")
plot.new()
rasterImage(pp,0,0,1,1)


# > test_data <- read.csv("https://stepik.org/media/attachments/course/524/cluster_1.csv")
# > get_difference﻿(test_data, 2)
# [1] "V2" 

# Во втором наборе данных при выделении двух кластеров значимые различия получаются
# по обеим переменным.


pp <- readPNG("imgs/img_15.png")
plot.new()
rasterImage(pp,0,0,1,1)


# > test_data <- read.csv("https://stepik.org/media/attachments/course/524/cluster_2.csv")
# > get_difference﻿(test_data, 2)
# [1] "V1" "V2"

# Подсказки:
# Не забудьте перевести переменную с номером кластера в фактор! 
# Вы можете использовать вашу функцию из предыдущего задания.
# Для поиска различий ﻿используйте ANOVA (функция aov). Давайте договоримся, что 
# для наших целей мы не будем проверять данные на соответствие требованиями к 
# применению этого критерия и не будем думать о поправке на множественные сравнения.

test_data <- read.csv("https://stepik.org/media/attachments/course/524/cluster_1.csv")

get_difference <- function(test_data, n_cluster){
  dist_matrix <- dist(test_data, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
  fit <- hclust(dist_matrix, method = "complete", members = NULL)
  cluster <- as.factor(cutree(fit, n_cluster))
  res <- sapply(test_data, function(col) anova(aov(col ~ cluster, test_data))$P[1])
  return(names(which(res < 0.05)))
}

get_difference(test_data, 3)







# Напишите функцию get_pc﻿, которая получает на вход dataframe с произвольным числом
# количественных переменных. Функция должна выполнять анализ главных компонент и добавлять
# в исходные данные две новые колонки со значениями первой и второй главной компоненты.
# Новые переменные должны называться "PC1"  и "PC2" соответственно.
# 
# Пример работы функции:
#   
# > test_data <- read.csv("https://stepik.org/media/attachments/course/524/pca_test.csv")
# 
# > test_data
# V1 V2 V3 V4 V5
# 1 13 15 12 13 12
# 2 16 11  8 12  6
# 3 15  7 10 12 13
# 4 12 11  6  6  4
# 5 11 13 13 10 12
# 
# > get_pc(test_data)
# V1 V2 V3 V4 V5  PC1  PC2
# 1 13 15 12 13 12 -4.5  2.4
# 2 16 11  8 12  6  3.0 -1.9
# 3 15  7 10 12 13 -2.8 -5.1
# 4 12 11  6  6  4  7.8  1.7
# 5 11 13 13 10 12 -3.6  3.0﻿
# 
# Для выполнения анализа главных компонент используйте функцию ﻿prcomp(). Изучите
# результат применения этой функции к данным, чтобы найти, где хранятся значения
# выделенных главных компонент.


df <- read.csv("https://stepik.org/media/attachments/course/524/pca_test.csv")

get_pc <- function(df){
  res <- prcomp(df, rank. = 2)$x
  return(cbind(df, res))
}







# Усложним предыдущую задачу! Напишите функцию get_pca2, которая принимает на вход
# dataframe с произвольным числом количественных переменных. Функция должна рассчитать,
# какое минимальное число главных компонент объясняет больше 90% изменчивости в исходных
# данных и добавлять значения этих компонент в исходный dataframe в виде новых переменных.
# 
# Рассмотрим работу функции на примере встроенных данных swiss:
#   
#   # посмотрим сколько главных компонент объясняют больше 90% изменчивости в данных
#   > fit <- prcomp(swiss)
# > summary(fit)
# Importance of components:
#   PC1     PC2      PC3     PC4     PC5    PC6
# Standard deviation     43.836 21.6022 12.05342 4.75916 3.65754 2.4882
# Proportion of Variance  0.746  0.1812  0.05641 0.00879 0.00519 0.0024
# Cumulative Proportion   0.746  0.9272  0.98361 0.99240 0.99760 1.0000
# 
# # в нашем случае уже две компоненты объясняют больше 90% изменчивости
# # значит значения первых двух компонент нужно добавить в исходные данные
# 
# # пример работы функции:
# > result  <- get_pca2(swiss)
# > str(result)
# 'data.frame':  47 obs. of  8 variables:
#   $ Fertility       : num  80.2 83.1 92.5 85.8 76.9 76.1 83.8 92.4 82.4 82.9 ...
# $ Agriculture     : num  17 45.1 39.7 36.5 43.5 35.3 70.2 67.8 53.3 45.2 ...
# $ Examination     : int  15 6 5 12 17 9 16 14 12 16 ...
# $ Education       : int  12 9 5 7 15 7 7 8 7 13 ...
# $ Catholic        : num  9.96 84.84 93.4 33.77 5.16 ...
# $ Infant.Mortality: num  22.2 22.2 20.2 20.3 20.6 26.6 23.6 24.9 21 24.4 ...
# $ PC1             : num  37.03 -42.8 -51.08 7.72 35.03 ...
# $ PC2             : num  -17.43 -14.69 -19.27 -5.46 5.13 ...


get_pca2 <- function(df){
  res <- prcomp(df)
  pc <- summary(res)$importance[3,]
  n <- length(which(pc < 0.9)) + 1
  return(cbind(df, res$x[,1:n]))
}

get_pca2(swiss)








# Задача для Чака Норриса.
# 
# Как я говорил, метод главных компонент может применяться для борьбы с мультиколлинеарностью
# в данных (ситуация, когда некоторые переменные очень сильно коррелируют между собой).
# Однако иногда некоторые переменные не просто сильно взаимосвязаны, но могут представлять
# линейную комбинацию друг друга. На такие переменные лучше сразу взглянуть повнимательнее
# и выяснить, откуда они взялись в наших данных.
# 
# Напишите функцию is_multicol, которая получает на вход dataframe произвольного размера
# с количественными переменными. Функция должна проверять существование строгой 
# мультиколлинеарности, а именно наличие линейной комбинации между предикторами.
# Линейной комбинацией является ситуация, когда одна переменная может быть выражена 
# через другую переменную при помощи уравнения 
# V1=k∗V2+b.
# Например V1 = V2 + 4 или V1 = V2 - 5.
# 
# Функция возвращает имена переменных, между которыми есть линейная зависимость или 
# cобщение "There is no collinearity in the data".
# 
# > #В данных нет мультиколлинеарности
#   > test_data <- read.csv("https://stepik.org/media/attachments/course/524/Norris_1.csv")
# V1 V2 V3 V4
# 1 22 20 18 20
# 2 16 28 31 15
# 3 14 24  7 16
# > is_multicol(test_data)
# [1] "There is no collinearity in the data"
# 
# > #V1 = ﻿V2 + 1
#   > test_data <- read.csv("https://stepik.org/media/attachments/course/524/Norris_2.csv")
# V1 V2 V3 V4
# 1 13 12  7 11
# 2 15 14 13 10
# 3  8  7 11 16
# > is_multicol(test_data)
# [1] "V2" "V1"
# 
# > #V1 ﻿= V2 + 1 и ﻿V3 ﻿= V4 - 2
#   > test_data <- read.csv("https://stepik.org/media/attachments/course/524/Norris_3.csv")
# V1 V2 V3 V4
# 1 20 19 12 14
# 2 11 10  5  7
# 3 12 11 10 12
# > is_multicol(test_data)
# [1] "V2" "V1" "V4" "V3"﻿
# 
# Подсказки:
# Далеко не всегда 1 == 1 или 0.2 == 0.2 ﻿есть ТRUE! Подробнее можно почитать о проблеме здесь.
# (https://stackoverflow.com/questions/9508518/why-are-these-numbers-not-equal)

df <- read.csv("https://stepik.org/media/attachments/course/524/Norris_1.csv") 
df <- read.csv("https://stepik.org/media/attachments/course/524/Norris_2.csv")
df <- as.data.frame(list(V1 = c(12, 17, 18, 6, -4), 
                         V2 = c(13, 12, -7, 6, -2), 
                         V3 = c(13, 4, 12, 7, 7), 
                         V4 = c(16, 21, -8, 9, 14), 
                         V5 = c(14, 17, 25, 17, 24), 
                         V6 = c(7, 10, 18, 10, 17), 
                         V7 = c(12, 6, 0, 12, 11), 
                         V8 = c(20, 16, 2, 1, 13)))


is_multicol <- function(df){
  res <- cor(df)
  diag(res) <- 0
  abs_res <- round(abs(res), digits=2)
  inxs <- which(abs_res == 1, arr.ind = TRUE)
  if (length(inxs) == 0) {
    return("There is no collinearity in the data")
 } else {
   return(rownames(inxs))
 }
}

is_multicol(df)







# Вот и подходит к концу наш курс. Давайте построим финальный график!
#   
# В данных swiss, используя все переменные, выделите два кластера при помощи
# иерархической кластеризации и сохраните значение кластеров как фактор в переменную
# cluster. Должно получиться:
#   
# > str(swiss)
# 'data.frame':  47 obs. of  7 variables:
#   $ Fertility       : num  80.2 83.1 92.5 85.8 76.9 76.1 83.8 92.4 82.4 82.9 ...
# $ Agriculture     : num  17 45.1 39.7 36.5 43.5 35.3 70.2 67.8 53.3 45.2 ...
# $ Examination     : int  15 6 5 12 17 9 16 14 12 16 ...
# $ Education       : int  12 9 5 7 15 7 7 8 7 13 ...
# $ Catholic        : num  9.96 84.84 93.4 33.77 5.16 ...
# $ Infant.Mortality: num  22.2 22.2 20.2 20.3 20.6 26.6 23.6 24.9 21 24.4 ...
# $ cluster         : Factor w/ 2 levels "1","2": 1 2 2 1 1 2 2 2 2 2 ...
# Затем визуализируйте взаимосвязь переменных  Education и  Catholic в двух 
# выделенных кластерах, чтобы получился следующий график:
  
  
pp <- readPNG("imgs/img_16.png")
plot.new()
rasterImage(pp,0,0,1,1)  
  
# Подсказки:
# — для визуализации линейной регрессии пользуйтесь geom_smooth();
# — точки проще всего добавить с помощью geom_point().

n_cluster <- 2

dist_matrix <- dist(swiss, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
fit <- hclust(dist_matrix, method = "complete", members = NULL)
swiss$cluster <- as.factor(cutree(fit, n_cluster))


library(ggplot2)
my_plot <- ggplot(swiss, aes(Education, Catholic, col = cluster))+
  geom_smooth(method = 'lm')+
  geom_point()
