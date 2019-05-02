data(cars)
scatter.smooth(x=cars$speed, y = cars$dist, main = "Dist ~ Speed")

linearMod <- lm(dist ~ speed, data = cars)
print(linearMod)
summary(linearMod)

LeastSquares <- cars

LeastSquares$speedSquare <- LeastSquares$speed^2
LeastSquares$speedXdist <- LeastSquares$speed*LeastSquares$dist
LeastSquares <- rbind(LeastSquares, colSums(LeastSquares))

A <- array(c(LeastSquares$speedSquare[nrow(LeastSquares)], LeastSquares$speed[nrow(LeastSquares)],
             LeastSquares$speed[nrow(LeastSquares)], nrow(LeastSquares)-1 ), c(2,2))
b <- c(LeastSquares$speedXdist[nrow(LeastSquares)], LeastSquares$dist[nrow(LeastSquares)])

Coef <- solve(A,b)
