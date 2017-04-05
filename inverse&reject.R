# 逆变换法
cauchy <- function(n){
  u <- runif(n,min=0,max=1)
  return(tan(pi*(u-0.5)))
}

cauchy_random <- cauchy(100000)

plot(density(cauchy_random[cauchy_random < 20 & cauchy_random > -20]))

# 拒绝接受法
objpdf <- function(x){
  return(6*(x-0.5)^2/7)
}
objpdf(seq(0,2,0.1))
#[1] 0.214285714 0.137142857 0.077142857 0.034285714 0.008571429 0.000000000
#[7] 0.008571429 0.034285714 0.077142857 0.137142857 0.214285714 0.308571429
#[13] 0.420000000 0.548571429 0.694285714 0.857142857 1.037142857 1.234285714
#[19] 1.448571429 1.680000000 1.928571429
# 选用U(0,2)做建议分布，则其pdf g(x)=0.5，要保证f(x) <= Mg(x), M只要f(x)最大值的2倍

M <- 3.858
n=100000
y <- runif(n,0,2)
u <- runif(n,0,1)
h <- objpdf(y)/(M*0.5)
result <- y[u<h]
plot(density(result[]))

####################
advisepdf <- function(x){
  return(0.5)
}
adviseran <- function(n){
  return(runif(n,0,2))
}
reject <- function(n,m){
  out = vector(length = n)
  count <- 1
  while(count<=n){
    y <- adviseran(1)
    u <- runif(1,0,1)
    h <- objpdf(y)/(m*advisepdf(y))
    if(u<h){
      out[count] <- y
      count = count+1
    }
  }
  return(out)
}
rejectran <- reject(10000,3.858)
plot(density(rejectran))
