xl <- iris[, 3:5]
n=2 
m=3 
classes <- levels(xl[,3])
Py<-table(xl[,3])/dim(xl)[1]#априорные вероятности появления каждого класса


mu = matrix(0, nrow=m, ncol=n)
sigma = matrix(0, nrow=m, ncol=n)
for(i in 1:m){
  for(j in 1:n){
    temp=xl[xl[,3]==classes[i],][,j] #определяем столбец j, который содержит признаки объекта i
    mu[i,j]<-mean(temp)#определяем матрицу математических ожиданий
    sigma[i,j]<-sqrt(var(temp))#матрица среднеквадратичных отклонений
  }
}

naiveBayes = function(x, Py, mu, sigma, m, n) {
  prasp <- matrix(c('setosa','versicolor', 'virginica', 0, 0, 0), nrow = 3, ncol = 2)
  scores = rep(0, m)
  for (i in 1:m) {
    scores[i] = Py[i]
    for (j in 1:n){
      N=1/sqrt(2*pi)/sigma[i,j]*exp(-1/2*(x[j]-mu[i,j])^2/sigma[i,j]^2)##считаем эмпирическую оценку n-мерной плотности распределения
      scores[i] = scores[i] * N
    }
    prasp[i,2]=scores[i]
  }
  class <- prasp[,1][which.max(prasp[,2])] ##относим объект к тому классу, у котрого наибольшая эмпирическая оценка
}

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1, xlab = признак1:длина лепетка, ylab = признак2:ширина лепестка, main = ""Наивный" байесовский классификатор")


a=0
b=0
while(a<7){
  while(b<7){
    z <- c(b, a)
    class <- naiveBayes(z, Py, mu, sigma, m, n)
    points(z[1], z[2], pch = 21, col = colors[class], asp = 1)
    b=b+0.1
  }
  b=0
  a=a+0.1
}