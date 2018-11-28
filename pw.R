#Евклидово расстояние
dist = function(p1, p2) sqrt(sum((p1 - p2) ^ 2)) 
#Расстояние от всех points до точки u
distances = function(points, u) apply(points, 1, dist, u)



#Ядра
kernel.pr = function(r){
  return ((0.5 * (abs(r) <= 1) )) #функция прямоугольного ядра
}
kernel.tr = function(r){
  return ((1 - abs(r)) * (abs(r) <= 1)) #функция для треугольного ядра
}
kernel.kv = function(r){
  return ((15 / 16) * (1 - r ^ 2) ^ 2 * (abs(r) <= 1)) #функция для квартического ядра
}
kernel.ep = function(r){
  return ((3/4*(1-r^2)*(abs(r)<=1))) #функция для ядра Епанечникова
}
kernel.ga = function(r){
  (((2*pi)^(-1/2)) * exp(-1/2*r^2)) #функция для Гауссовского ядра
}


PW.kernel = kernel.kv 

#PW
mc.PW = function(distances, u, h) {
  weights = mc.PW.kernel(distances / h)
  classes = unique(names(distances)) 
  
  weightsByClass = sapply(classes, mc.sumByClass, weights)
  
  if (max(weightsByClass) == 0) return("") 
  
  return(names(which.max(weightsByClass)))
}


#Отрисовка карты классификации
drawPW = function(points, classes, colors, h) {
  uniqueClasses = unique(classes)
  names(colors) = uniqueClasses
  
  
  plot(points, bg = colors[classes], pch = 21, asp = 1,main = "Карта классификации PW. Гауссовское ядро. ", xlab = "Длина листа", ylab = "Ширина листа", col.lab = "blue") 
  
  
  step = 0.1
  ox = seq(1, 7, step)
  oy = seq(0, 3, step)
  
  for (x in ox) {
    for (y in oy) {
      x = round(x, 1) 
      y = round(y, 1) 
      u = c(x, y)
      
      
      
      distances = distances(points, u)
      names(distances) = classes
      classified = PW(distances, u, h)
      
      
      points(u[1], u[2], col = colors[classified], pch = 21) #u
    }
  }
  
  
}

#LOO
LOOPW = function(points, classes, hValues) {
  n = dim(points)[1]
  loo = rep(0, length(hValues))
  
  for (i in 1:n) {
    u = points[i,]
    sample = points[-i,]
    distances = distances(sample, u)
    names(distances) = classes[-i]
    
    for (j in 1:length(hValues)) {
      h = hValues[j]
      classified = PW(distances, u, h)
      loo[j] = loo[j] + (classified != classes[i])
    }
  }
  
  loo = loo / n
}

#Отрисовка LOO
drawLOOPW = function(points, classes, hValues) {
  loo = LOOPW(points, classes, hValues)
  
  x = hValues
  y = loo
  
  plot(x, y, type = "l", main = "LOO для PW. Гауссовское ядро.", xlab = "h", ylab = "LOO", col.lab = "blue")
  
  h = hValues[which.min(loo)]
  h.loo = round(loo[which.min(loo)], 4)
  
  points(h, h.loo, pch = 19, col = "blue")
  label = paste("h = ", h, "\n", "LOO = ", h.loo, sep = "")
  text(h, h.loo, labels = label, pos = 3, col = "blue", family = "mono", font = 2)
  
  return(h)
}

#тестируем программу

petals = iris[, 3:4]
petalNames = iris[, 5]

par(mfrow = c(1, 2), xpd = T)
drawPW(petals, petalNames, colors = c("red", "green3", "blue"), h = h)
h = drawLOOPW(petals, petalNames, hValues = seq(0.1, 2, 0.005))

