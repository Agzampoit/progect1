trainingSampleNormalization <- function(xl)
{
  n <- dim(xl)[2] - 1
  for(i in 1:n)
  {
    xl[, i] <- (xl[, i] - mean(xl[, i])) / sd(xl[, i])
  }
  return (xl)
}
##приравниваем третий столбец w0=-1
trainingSamplePrepare <- function(xl)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  xl <- cbind(xl[, 1:n], seq(from = -1, to = -1, length.out = l), xl[, n + 1])
}
## квадратичная функция потерь
lossQuad <- function(x)
{
  return ((x-1)^2)
}

## Функция ADALINE
sg.ADALINE <- function(xl, eta = 1, lambda = 1/6)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  w <- c(1/2, 1/2, 1/2)

  iterCount <- 0
  Q <- 0
  for (i in 1:l)
  {
    
    wx <- sum(w * xl[i, 1:n])
    margin <- wx * xl[i, n + 1]
    Q <- Q + lossQuad(margin)
  }
  repeat
  { ##считаем ошибки на элементах
    margins <- array(dim = l)
    for (i in 1:l)
    {
      xi <- xl[i, 1:n]
      yi <- xl[i, n + 1]
      margins[i] <- crossprod(w, xi) * yi
    }
    errorIndexes <- which(margins <= 0)
    if (length(errorIndexes) > 0)
    {
      # выбираем случайным образом элемент с ошибкой
      i <- sample(errorIndexes, 1)
      iterCount <- iterCount + 1
      xi <- xl[i, 1:n]
      yi <- xl[i, n + 1]

      wx <- sum(w * xi)
      margin <- wx * yi
      ex <- lossQuad(margin)
      eta <- 1 / sqrt(sum(xi * xi))
      w <- w - eta * (wx - yi) * xi
      Qprev <- Q
      Q <- (1 - lambda) * Q + lambda * ex
    }
    else
    {
      break
    }
  }
  return (w)
}


## Персептрон Розенблатта
lossPerceptron <- function(x)
{
  return (max(-x, 0))
}


sg.Hebb <- function(xl, eta = 0.1, lambda = 1/6)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  w <- c(1/2, 1/2, 1/2)
  iterCount <- 0
  Q <- 0
  for (i in 1:l)
  {
    
    wx <- sum(w * xl[i, 1:n])
    margin <- wx * xl[i, n + 1]
    Q <- Q + lossPerceptron(margin)
  }
  repeat
  {
    
    margins <- array(dim = l)
    for (i in 1:l)
    {
      xi <- xl[i, 1:n]
      yi <- xl[i, n + 1]
      margins[i] <- crossprod(w, xi) * yi
    }
   
    errorIndexes <- which(margins <= 0)
    if (length(errorIndexes) > 0)
    {
      
      i <- sample(errorIndexes, 1)
      iterCount <- iterCount + 1
      xi <- xl[i, 1:n]
      yi <- xl[i, n + 1]
      w <- w + eta * yi * xi
    }
    else
      break;
  }
  return (w)
}


## Логистическая регрессия
lossLog <- function(x)
{
  return (log2(1 + exp(-x)))
}

sigmoidFunction <- function(z)
{
  return (1 / (1 + exp(-z)))
}


sg.Regression <- function(xl, eta = 0.1, lambda = 1/6)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  w <- c(1/2, 1/2, 1/2)
  iterCount <- 0
 
  Q <- 0
  for (i in 1:l)
  {
    
    wx <- sum(w * xl[i, 1:n])
    margin <- wx * xl[i, n + 1]
    Q <- Q + lossLog(margin)
  }
  repeat
  {

    i <- sample(1:l, 1)
    iterCount <- iterCount + 1
    xi <- xl[i, 1:n]
    yi <- xl[i, n + 1]
   
    wx <- sum(w * xi)
    margin <- wx * yi
    ex <- lossLog(margin)
    eta <- 0.3#1 / iterCount
    w <- w + eta * xi * yi * sigmoidFunction(-wx * yi)
    
    Qprev <- Q
    Q <- (1 - lambda) * Q + lambda * ex
    if (abs(Qprev - Q) / abs(max(Qprev, Q)) < 1e-5)
      break
  }
  return (w)
}

ObjectsCountOfEachClass <- 200

test <- function(ObjectsCountofEachClass) {
  library(MASS)
  sigma <- matrix(c(3,0,0,7),2,2)
  
  xy1 <- mvrnorm (ObjectsCountofEachClass,c(1,1),sigma)
  xy2 <- mvrnorm (ObjectsCountofEachClass,c(9,7),sigma)
  
  xl <- rbind(cbind(xy1, 1), cbind(xy2, -1))
  print(xl);
  
  xlNorm <- trainingSampleNormalization(xl)
  xlNorm <- trainingSamplePrepare(xlNorm)
  
  colors <- c("green2", "red")
  plot(xlNorm[, 1], xlNorm[, 2], pch = 21, asp = 1, main="Все линейные алгоритмы классификации.")
  
  for (i in 1:dim(xlNorm)[1]) {
    points(xlNorm[i, 1], xlNorm[i, 2], pch = 21, bg = colors[ifelse((xl[i, 3] < 0), 1, 2)], asp = 1)
  }
  w <- sg.ADALINE(xlNorm)
 
  abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 3, col = "darkblue")

  w <- sg.Hebb(xlNorm)
  
  abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 3, col = "orange")
  

  w <- sg.Regression(xlNorm)
 
  abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 3, col ="purple")
  
legend("bottomleft", c("ADALINE", "Персептрон Розенблатта", "Логистическая регрессия"), pch = c(15,15,15), col =c("darkblue", "orange", "purple"))
}

test(60)
