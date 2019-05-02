#simple for 2 var method least squares
Reg2varLS <- function(table, y, x){
  LeastSquares <- table
  
  LeastSquares$XSquare <- LeastSquares[,x]^2
  LeastSquares$XY <- LeastSquares[,x]*LeastSquares[,y]
  LeastSquares <- rbind(LeastSquares, colSums(LeastSquares))
  
  A <- array(c(LeastSquares$XSquare[nrow(LeastSquares)], LeastSquares[nrow(LeastSquares),x],
               LeastSquares[nrow(LeastSquares),x], nrow(LeastSquares)-1 ), c(2,2))
  b <- c(LeastSquares$XY[nrow(LeastSquares)], LeastSquares[nrow(LeastSquares),y])
  
  Coef <- solve(A,b)
  return(Coef)
}

#multi for 1:n
MultipleRegression <- function(table, prediction){
  Y <- table[,prediction]
  Intercept <- rep(1, nrow(table))
  table <- table[,-c(prediction)]
  table <- cbind(table, Intercept)
  X <- as.matrix(table)
  Xt <- t(X)
  XtX <- Xt %*% X
  XtY <- Xt %*% Y
  ReverseXtX <- solve(XtX)
  S = ReverseXtX %*% XtY
  return(S)
}


#######TEST#######

#simple
data(cars)
scatter.smooth(x=cars$speed, y = cars$dist, main = "Dist ~ Speed")

linearMod <- lm(dist ~ speed, data = cars)
summary(linearMod)
Reg2varLS(cars, 2, 1)


#multi
library(car)
long <- data.frame(longley)
cor(long)
scatterplotMatrix(long, main = "Соотношение переменных")

model <- lm(Population ~., data = long)
summary(model)

MultipleRegression(long, 5)

################

data(trees)
cor(trees)
scatterplotMatrix(trees, main = "Соотношение переменных")

linmodel <- lm(Height ~., data = trees)
summary(linmodel)

MultipleRegression(trees, 2)

#Intercept -  Если у нас модель представлена в виде уравнения, то тогда б0 - точка пересечения прямой с точкой координат

#R-squadred - Коэффициент детерминации указывает насколько тесной является связь между факторами регрессии и зависимой переменной, 
#это соотношение объясненных сумм квадратов возмущений, к необъясненным. Чем ближе к 1, тем ярче выражена зависимость.

#Adjusted R-squared — Проблема с R2 в том, что он по любому растет с числом факторов, поэтому высокое значение данного коэффициента
#может быть обманчивым, когда в модели присутствует множество факторов. Для того, чтобы изъять из коэффициента корреляции данное свойство
#был придуман скорректированный коэффициент детерминации.

#F-statistic — Используется для оценки значимости модели регрессии в целом, является соотношением объяснимой дисперсии, к необъяснимой. 
#Если модель линейной регрессии построена удачно, то она объясняет значительную часть дисперсии, оставляя в знаменателе малую часть. 
#Чем больше значение параметра — тем лучше.

#t value — Критерий, основанный на t распределении Стьюдента. Значение параметра в линейной регрессии указывает на значимость фактора,
#принято считать, что при t > 2 фактор является значимым для модели.

#p value — Это вероятность истинности нуль гипотезы, которая гласит, что независимые переменные не объясняют динамику зависимой переменной. 
#Если значение p value ниже порогового уровня (.05 или .01 для самых взыскательных), то нуль гипотеза ложная. Чем ниже — тем лучше.


