#простая линейная регрессия

data(cars)
scatter.smooth(x=cars$speed, y = cars$dist, main = "Dist ~ Speed")

linearMod <- lm(dist ~ speed, data = cars)
summary(linearMod)

LeastSquares <- cars

LeastSquares$speedSquare <- LeastSquares$speed^2
LeastSquares$speedXdist <- LeastSquares$speed*LeastSquares$dist
LeastSquares <- rbind(LeastSquares, colSums(LeastSquares))

A <- array(c(LeastSquares$speedSquare[nrow(LeastSquares)], LeastSquares$speed[nrow(LeastSquares)],
             LeastSquares$speed[nrow(LeastSquares)], nrow(LeastSquares)-1 ), c(2,2))
b <- c(LeastSquares$speedXdist[nrow(LeastSquares)], LeastSquares$dist[nrow(LeastSquares)])

Coef <- solve(A,b)

#множественная регрессия
library(car)
long <- data.frame(longley)
cor(long)
scatterplotMatrix(long, main = "Соотношение переменных")

model <- lm(Population ~., data = long)
summary(model)

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
