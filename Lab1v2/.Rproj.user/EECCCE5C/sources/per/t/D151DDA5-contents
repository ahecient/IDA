library("tm")         #Библиотека для stopwords
library("stringr")    #Библиотека для работы со строками
library("stringi")
 
energy <- scan(file="energy.txt", what=character(),sep="\n")                        #Считываем файл, символьно, разделитель конец строки
realestatet <- scan(file ="realestate.txt", what=character(),sep="\n")
movies <- scan(file ="movies.txt",what=character(),sep="\n")

test_news <- read.table("test.txt", header=TRUE,
                        sep="\t", quote = "")                           #Считываем таблицы с хедером разделитель таб  


test_news$test <- str_replace_all(test_news$text, "[:punct:]|[:digit:]", "")        #Избавлямся от пунктуации(символов)
test_news$test <- tolower(test_news$test)                                   #Избавляемся от верхнего регистра
test_words <- unlist(strsplit(test_news$test, " "))                             #Делим предложения на слова по пробелу
test_words  <- test_words[! test_words %in% stopwords("english")]                 #Избавляемся от стоп-слов

test_words <- unique(test_words)
test_words <- stri_remove_empty(test_words)


energy <- str_replace_all(energy, "[[:punct:]]", "")
energy <- str_replace_all(energy, "[[:digit:]]", "")
energy <- tolower(energy)
energy_words <- unlist(strsplit(energy, " "))
energy_words  <- energy_words[! energy_words %in% stopwords("english")]
  
realestatet <- str_replace_all(realestatet, "[[:punct:]]", "")
realestatet <- str_replace_all(realestatet, "[[:digit:]]", "")
realestatet <- tolower(realestatet)
realestatet_words <- unlist(strsplit(realestatet, " "))
realestatet_words  <- realestatet_words[! realestatet_words %in% stopwords("english")]

movies <- str_replace_all(movies, "[[:punct:]]", "")
movies <- str_replace_all(movies, "[[:digit:]]", "")
movies <- tolower(movies)
movies_words <- unlist(strsplit(movies, " "))
movies_words  <- movies_words[! movies_words %in% stopwords("english")]

unique_words <- table(energy_words)                                                 #Уникальные слов (без повторений)
main_table <- data.frame(u_words=unique_words)                                      #Создаем дата фрейм (таблицу)
names(main_table) <- c("word","energy")                                             #Называем колонки


main_table$realestatet <- 0                                                         #Добавляем колонки для других классов
main_table$movies <- 0

for(i in 1:length(realestatet_words)){                                              #Делаем счётчик для записи количества слов встречаемых в других класса
  need_word <- TRUE
  for(j in 1:(nrow(main_table))){
    
    if(realestatet_words[i]==main_table[j,1])                       
    {
      main_table$realestatet[j] <- main_table$realestatet[j]+1
      need_word <- FALSE
    }
  }
  
  if(need_word==TRUE)
  {
    main_table <- rbind(main_table,data.frame(word=realestatet_words[i],energy=0,realestatet=1, movies=0))    #Если слово встречалось только в другом классе
  }
}

for(i in 1:length(movies_words)){
  
  need_word <- TRUE
  for(j in 1:(nrow(main_table))){
    
    if(movies_words[i]==main_table[j,1])                       
    {
      main_table$movies[j] <- main_table$movies[j]+1
      need_word <- FALSE
    }
  }
  
  if(need_word==TRUE)
  {
    main_table <- rbind(main_table,data.frame(word=movies_words[i],energy=0,realestatet=0, movies=1))
  }
}


main_table$P.energy <- NA                                    #Добавляем колонку для вероятности вхождения слова в класс
main_table$p.realestate <- NA
main_table$p.movies <- NA

words.N <- nrow(main_table)                                  #Общее количество слов в выборке
   
p.word.class <- function(N_ik,M,N_k,a)                       #Функция для расчёта независимых вероятностей + сглаживание
{                                                             
  (a+N_ik)/(a*M+N_k)
}



for(i in 1:length(test_words))                              #Высчитываем вероятности  вождения слова в класс для тестовых слов
{                                                           #Если их нет - NA
  need_word <- TRUE                                         #Если тестовых слов нет в обучающей выборке - добавляем в конец с вероятностями стремящихся к нулю(из-за сглаживания)                                           
  for(j in 1:nrow(main_table))
  {
    
    if(test_words[i]==main_table$word[j])
    {
      main_table$P.energy[j] <- p.word.class(main_table$energy[j],words.N,sum(main_table$energy),1)
      main_table$p.realestate[j] <- p.word.class(main_table$realestatet[j],words.N,sum(main_table$realestatet),1)
      main_table$p.movies[j] <- p.word.class(main_table$movies[j],words.N,sum(main_table$movies),1)
      need_word <- FALSE
    }
  }
  
  if(need_word==TRUE)
  {
    main_table <- rbind(main_table,data.frame(word=test_words[i],energy=0,realestatet=0,movies=0,P.energy=NA,p.realestate=NA,p.movies=NA))
    main_table$P.energy[nrow(main_table)] <- p.word.class(main_table$energy[nrow(main_table)],words.N,sum(main_table$energy),1)
    main_table$p.realestate[nrow(main_table)] <- p.word.class(main_table$realestatet[nrow(main_table)],words.N,sum(main_table$realestatet),1)
    main_table$p.movies[nrow(main_table)] <- p.word.class(main_table$movies[nrow(main_table)],words.N,sum(main_table$movies),1)
  }
}

test_news$probability_energy <-1                            #Вероятность вхождения предложения в один из классов (добавляем колонку)
test_news$probability_realestate<-1
test_news$probability_movies <-1




#поиск слова в предложении и высчет выроятности предложения путём сум вероятностей слова
for (i in 1:(nrow(test_news))){
  print(test_news$test[i])
  test <- unlist(strsplit(test_news$test[i]," "))
  print(test)
  for(k in 1:length(test)){
  for(j in 1:length(test_words)){
    if(test[k] == test_words[j]){
      for (l in 1:nrow(main_table)) {
        if(test_words[j]==main_table$word[l]){
          test_news$probability_energy[i] <- test_news$probability_energy[i] * main_table$P.energy[l]
          
          test_news$probability_realestate[i] <- test_news$probability_realestate[i] * main_table$p.realestate[l]     
          
          test_news$probability_movies[i] <- test_news$probability_movies[i] * main_table$p.movies[l]
        }
      }
    }
  } 
  }


}

for(i in 1:(nrow(test_news))){      #Финальная формула
  test_news$probability_energy[i] <- (length(energy)/(length(energy)+length(realestatet)+length(movies)))*test_news$probability_energy[i]
  test_news$probability_realestate[i] <- (length(realestatet)/(length(energy)+length(realestatet)+length(movies)))*test_news$probability_realestate[i]
  test_news$probability_movies[i] <- (length(movies)/(length(energy)+length(realestatet)+length(movies)))*test_news$probability_movies[i]
}

test_news$max <- NA;

for (i in 1:(nrow(test_news))){     #Проверка макс классов
  if (max(test_news$probability_energy[i],test_news$probability_realestate[i],test_news$probability_movies[i])==test_news$probability_energy[i]){
    test_news$max[i] <- 'energy'
  }
  if (max(test_news$probability_energy[i],test_news$probability_realestate[i],test_news$probability_movies[i])==test_news$probability_realestate[i]){
    test_news$max[i] <- 'real estate'
  }
  if (max(test_news$probability_energy[i],test_news$probability_realestate[i],test_news$probability_movies[i])==test_news$probability_movies[i]){
    test_news$max[i] <- 'movies'
  }
}

test_news$prediction <- NA;

for(i in 1:(nrow(test_news))){    #проверка сколько было верно предугадано
  if (test_news$label[i]==test_news$max[i]){
    test_news$prediction[i] <- 1
  }
  else test_news$prediction[i] <- 0
}



x <- sum(test_news$prediction)
y <- nrow(test_news)

G <- (x/y)*100

