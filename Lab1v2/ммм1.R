library("tm")        
library("stringr")  
library("stringi")


energy <- scan(file="energy_800.txt", what=character(),sep="\n")                        
realestatet <- scan(file ="realestate_800.txt", what=character(),sep="\n")
movies <- scan(file ="movies_800.txt",what=character(),sep="\n")

test_news <- read.table("test_800.txt", header=TRUE,
                        sep="\t", quote = "", stringsAsFactors = FALSE)                        

p.word.class <- function(N_ik,M,N_k,a){                                                             
  (a+N_ik)/(a*M+N_k)
}

clean_text <- function(sen){
  sen <- str_replace_all(sen, "[:punct:]|[:digit:]", "")
  sen <- tolower(sen)
  words <- unlist(strsplit(sen, " "))
  words  <- words[! words %in% stopwords("english")]
  words <- stri_remove_empty(words)
  return(words)
}

energy_words <- clean_text(energy)
movies_words <- clean_text(movies)
realestatet_words <- clean_text(realestatet)
test_words <- clean_text(test_news$text)


test_words <- unique(test_words)
test_words <- stri_remove_empty(test_words)


unique_words <- table(energy_words)                                                
main_table <- data.frame(u_words=unique_words)                                      
names(main_table) <- c("word","energy")                                             


main_table$realestatet <- 0                                                         
main_table$movies <- 0

for(i in 1:length(realestatet_words)){                                              
  need_word <- TRUE
  for(j in 1:(nrow(main_table))){
    if(realestatet_words[i]==main_table[j,1]){
      main_table$realestatet[j] <- main_table$realestatet[j]+1
      need_word <- FALSE
    }
  }
  
  if(need_word==TRUE)
  {
    main_table <- rbind(main_table,data.frame(word=realestatet_words[i],energy=0,realestatet=1, movies=0))    
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


main_table$P.energy <- NA                                    
main_table$p.realestate <- NA
main_table$p.movies <- NA

words.N <- nrow(main_table)                                  
for(i in 1:length(test_words))                              
{                                                           
  need_word <- TRUE                                                                                    
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

test_news$probability_energy <-1                            
test_news$probability_realestate<-1
test_news$probability_movies <-1
#FIX this SHIT
for (i in 1:(nrow(test_news))){
  print(test_news$text[i])
  test <- clean_text(test_news$text[i])
  print(test)
  for(k in 1:length(test)){
    for (l in 1:nrow(main_table)) {
        if(test[k]==main_table$word[l]){
          test_news$probability_energy[i] <- test_news$probability_energy[i] * main_table$P.energy[l]
          
          test_news$probability_realestate[i] <- test_news$probability_realestate[i] * main_table$p.realestate[l]     
          
          test_news$probability_movies[i] <- test_news$probability_movies[i] * main_table$p.movies[l]
        }
      }
    }
}

for(i in 1:(nrow(test_news))){    
  test_news$probability_energy[i] <- (length(energy)/(length(energy)+length(realestatet)+length(movies)))*test_news$probability_energy[i]
  test_news$probability_realestate[i] <- (length(realestatet)/(length(energy)+length(realestatet)+length(movies)))*test_news$probability_realestate[i]
  test_news$probability_movies[i] <- (length(movies)/(length(energy)+length(realestatet)+length(movies)))*test_news$probability_movies[i]
}

test_news$max <- NA;

for (i in 1:(nrow(test_news))){     
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

for(i in 1:(nrow(test_news))){   
  if (test_news$label[i]==test_news$max[i]){
    test_news$prediction[i] <- 1
  }
  else test_news$prediction[i] <- 0
}

x <- sum(test_news$prediction)
y <- nrow(test_news)

G <- (x/y)*100

