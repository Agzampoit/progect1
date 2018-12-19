## Оценка ковариационной матрицы для ЛДФ
estimateFisherCovarianceMatrix = function(points1, mu1, points2, mu2) {
    rows1 = dim(points1)[1]
    rows2 = dim(points2)[1]
    rows = rows1 + rows2
    cols = dim(points1)[2]
    sigma = matrix(0, cols, cols)

    for (i in 1:rows1)
        sigma = sigma + (t(points1[i,] - mu1) %*% (points1[i,] - mu1))

    for (i in 1:rows2)
        sigma = sigma + (t(points2[i,] - mu2) %*% (points2[i,] - mu2))

    return(sigma / (rows + 2))
}
## Количество объектов в каждом классе
ObjectsCountOfEachClass <- 150
## Генерируем тестовые данные
Sigma1 <- matrix(c(2, 0, 0, 2), 2, 2)
Sigma2 <- matrix(c(2, 0, 0, 2), 2, 2)
Mu1 <- c(1, 0)
Mu2 <- c(15, 0)
xy1 <- mvrnorm(n=ObjectsCountOfEachClass, Mu1, Sigma1)
xy2 <- mvrnorm(n=ObjectsCountOfEachClass, Mu2, Sigma2)
## Собираем два класса в одну выборку
xl <- rbind(cbind(xy1, 1), cbind(xy2, 2))
## Рисуем обучающую выборку
colors <- c(rgb(0/255, 162/255, 232/255), rgb(0/255, 200/255, 0/255))
plot(xl[,1], xl[,2], pch = 21, bg = colors[xl[,3]], asp = 1)
## Оценивание
objectsOfFirstClass <- xl[xl[,3] == 1, 1:2]
objectsOfSecondClass <- xl[xl[,3] == 2, 1:2]
mu1 <- estimateMu(objectsOfFirstClass)
mu2 <- estimateMu(objectsOfSecondClass)
Sigma <- estimateFisherCovarianceMatrix(objectsOfFirstClass, objectsOfSecondClass, mu1, mu2)
## Получаем коэффициенты ЛДФ
inverseSigma <- solve(Sigma)
alpha <- inverseSigma %*% t(mu1 - mu2)
beta <- (mu1 %*% inverseSigma %*% t(mu1) - mu2 %*% inverseSigma %*% t(mu2)) / 2
## Рисуем ЛДФ
abline(beta / alpha[2,1], -alpha[1,1]/alpha[2,1], col = "green", lwd = 3)
