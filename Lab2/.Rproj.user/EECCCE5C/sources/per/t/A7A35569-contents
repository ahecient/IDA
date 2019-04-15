library(tm)
library(stringr)
library(stringi)
library(dplyr)

#for package
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization




test <- scan(file ="test_2.TXT", what = character(), sep = "\n")

test <- tolower(test)

test_words <- unlist(strsplit(test, " "))



new_df <- data.frame("word","l_w","l_l", stringsAsFactors = FALSE)
names(new_df) <- c("word","x","y")   #создаем таблицу атрибутов 
for(i in 1:length(test)){
  test_words <- unlist(strsplit(test[i], " "))
  for (j in 1:length(test_words)){
    summ <- 0
    for (l in 1:str_which(test_words, test_words[j]))
    {
      if (!(str_which(test_words, test_words[l]) == str_which(test_words, test_words[j]))){
        summ <- summ + str_length(test_words[l])  #количество букв до слова
      }
    }
    new_df <- rbind(new_df, data.frame(word = test_words[j], y = str_length(test_words[j]) ,x = summ))
  }
}
new_df <- new_df[-c(1),] #криво созданный dataframe, удалем кривую строчку

new_df$x <- as.integer(new_df$x) #переводим в инт значения х и у для дальнейшего подсчёта
new_df$y <- as.integer(new_df$y)


new_df <-  new_df %>%   #считаем медиану количества букв до слова
  select(word,x,y) %>%   #выбираем что будем выводить
  group_by(word) %>%     #группируем по слову 
  summarise(x = median(x),y = mean(y), number = n())


new_df <- subset(new_df, number!=1)   #избавляемся от слов, которые встречались только 1 раз
new_df$number <- NULL

new_df_tree <- data.frame(word = new_df$word, levels = 0)

for(k in 1:(nrow(new_df_tree)-2)){    #запускаем цикл для таблицы (длина-2, что бы остались "корни" дерева (кластеры образно) )
  
  dm <- numeric(0) #пустой массив для хранения дистанций от одного слова до другого
  
  for(i in 1:nrow(new_df)){
    for(j in 1:nrow(new_df)){
      dis <- sqrt((new_df$x[j]-new_df$x[i])^2+(new_df$y[j]-new_df$y[i])^2)  #считаем по формуле евклидовое расстояние
      dm <- c(dm, dis) #добавляем в массив новоё зачение
    }
  }
  
  dm <- matrix(dm, nrow(new_df),nrow(new_df)) #делаем с массива квадратную матрицу размерностью количества слов
  colnames(dm) <- rownames(dm) <- new_df[['word']] #подписываем солонки и строчки матрицы словами с таблицы
  
  tria_dm <- dm #делаем треугольную матрицу
  tria_dm[upper.tri(tria_dm, diag = TRUE)] <-NA #длеаем НА значения верхний треугольник и диагональ матрицы
  print(tria_dm)
  min_dm <-which(tria_dm == min(tria_dm, na.rm = TRUE), arr.ind = TRUE) #ищем минимум по матрице
  min_r <- rownames(tria_dm)[min_dm[,1]] #получаем название строчки и колонки для минимума матрицы дистанций
  min_c <- colnames(tria_dm)[min_dm[,2]]
  
  for (i in 1:nrow(new_df)){   #объединяем строчки минимума и расчитываем для полученого среднее значение для кол-ва букв и длины
    for (j in 1:nrow(new_df)){
      if (new_df$word[i]==min_r & new_df$word[j]==min_c){
        print(min_c)
        print(min_r)
        new_df <- rbind(new_df, data.frame(word = paste(min_r,min_c, sep="+"), x = sum(new_df$x[i], new_df$x[j])/2, y = sum(new_df$y[i], new_df$y[j])/2))
        new_df <- new_df[-c(i,j),] # удаляем строчки, которіе объединяли
        break # для коректности работы выходим сразу с цикла
      }
    }
  }
  print(new_df)
}


#from package
#для рисунка дендограммы используем готовые пакеты + собственноручная матрица дистанций

dm <- numeric(0) 

for(i in 1:nrow(new_df)){
  for(j in 1:nrow(new_df)){
    dis <- sqrt((new_df$x[j]-new_df$x[i])^2+(new_df$y[j]-new_df$y[i])^2)  
    dm <- c(dm, dis) 
  }
}

dm <- matrix(dm, nrow(new_df),nrow(new_df)) 
colnames(dm) <- rownames(dm) <- new_df[['word']] 

hc1 <-hclust(as.dist(dm))
dend = as.dendrogram(hc1)
dend %>% plot
