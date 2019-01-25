
lines_draw = function(mu, cov,  title) {  
  det = det(cov)
  a = cov[1,1]
  b = cov[1,2]
  c = cov[2,1]
  d = cov[2,2]
  mu1 = mu[1]
  mu2 = mu[2]
  A = d/det
  B = (-b-c)/det
  C = a/det
  D = (-2*d*mu1 + b*mu2 + c*mu2)/det
  E = (b*mu1 + c*mu1 - 2*a*mu2)/det
  f = (d*mu1*mu1-b*mu1*mu2-c*mu1*mu2+a*mu2*mu2)/det
  
  x = y = seq(-4, 4, 0.04 )
  
  z = outer(x, y, function(x, y) 1/sqrt(2*pi*d) * exp(-1/2 * (A*x*x+B*y*x+C*y*y+D*x+E*y+f)))
  
  
  contour(x, y, z, levels=seq(0,1,0.05), main=title, drawlabels=T, asp=1)
  
}


lines_draw(c(0,0), matrix(c(1,0,0,1), nrow=2, ncol=2),  title="Признаки имеют одинаковые дисперсии")
#lines_draw(c(0,0), matrix(c(1,1,0,1), nrow=2, ncol=2),  title="Признаки коррелированы")
#lines_draw(c(0,0), matrix(c(3,0,0,1), nrow=2, ncol=2),  title="Признаки некоррелированы")
