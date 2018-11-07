# ��������� ����������
euclideanDistance <- function(u, v)
{
sqrt(sum((u - v)^2))
}

## ��������� ������� �������� ���������� �� ������� z
sortObjectsByDist <- function(xl, z, metricFunction = euclideanDistance) ## ������ ������� ����������
{
l <- dim(xl)[1]
n <- dim(xl)[2] - 1

distances <- matrix(NA, l, 2)## ������ ������� ����������
for (i in 1:l)
{
distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z)) ## ������� ���������� �� ���������������� ����� �� ��������� ����� �������
}

orderedXl <- xl[order(distances[, 2]), ] ##���������
return (orderedXl);
}

##��������� ����� kNN
kNN <- function(xl_1, z, k)
xl <- iris_new[, 3:5]
{

orderedXl <- sortObjectsByDist(xl, z) ## ��������� ������� �������� ����������������� �������
n <- dim(orderedXl)[2] - 1

classes <- orderedXl[1:k, n + 1] ## �������� ������ ������ k �������
counts <- table(classes) ## ���������� ������� ������������� ������� ������
class <- names(which.max(counts)) ## ������� �����, ������� ���������� ����� ������ �������
return (class) ## ���������� �����
}

plot(NULL, NULL, type = "l", xlim = c(0, 150), ylim = c(0, 1), xlab = 'k', ylab = 'LOO') #������ �����������
Ox <- seq(from = 1, to = 150, by = 5) # ��� OX � ������� �� 1 �� 150 � ����� 5
Oy <- c() # LOO �� ��� OY

#������������� ����������� �������� loo � k
LOO_opt <- 1
k_opt <- 1

for(k in Ox) {
  R <- 0 #������� ���������� � ��������� ��������� 0, � ������� ����� ������������� ������
  for(i in 1:l) {
    iris_new <- iris[-i, ] # ���� ������� ��� i-�� ��������
    z <- iris[i, 3:4]
    if(knn(iris_new, z, k) != iris[i, 5]) { #���� �������� ������
      R <- R + 1 #����� ������ �������������
    } 
  }
#����� ���� ��� �� ��������� ��� ��������, ������� loo
  LOO <- R/l #��� R ���������� ������, � l-���������� ��������� �������
  Oy <- c(Oy, LOO)
  
#���� ��������� loo ��������� ������ ������������ ������������ ��������, �� ������� ��� ����� �������� ����������� � ������������� k ���� ����������
  if(LOO < LOO_opt) {
    LOO_opt <- LOO
    k_opt <- k
  }
}

print(Ox)
print(Oy)
print(k_opt)

lines(Ox, Oy, pch = 8, bg = "black", col = "blue")
points(k_opt, LOO_opt, pch = 22, bg = "black", col = "black")