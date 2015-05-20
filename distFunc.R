library(matrixStats)

observations <- 240
data <- rbind(matrix(rexp(observations/2, rate=1), ncol = 2), 
              matrix(rexp(observations/2, rate=1/20), ncol = 2))

group <- unlist(strsplit(paste(sample(c('A','B'), observations/2,rep=T),sep='',collapse=''),''))

distFunc <- function(data, group)
{
  gps <- unique(group)
  
  A <- data[group == gps[1],]
  B <- data[group == gps[2],]
  
  medA <- colMedians(A)
  medB <- colMedians(B)
  
  na <- nrow(A)
  nb <- nrow(B)
  
  vmA <- (cov(A)*pi)/(2*na)
  vmB <- (cov(B)*pi)/(2*nb)

  totalVar <- ((na-1)*vmA + (nb-1)*vmB)/(na + nb - 2)
  
  v <- medA - medB
  as.numeric(t(v) %*% solve(totalVar) %*% v)
}

realDist<-distFunc (data, sample(group))

distVector <- replicate(3000, {distFunc (data, sample(group))})

hist(distVector)
abline(v=realDist, col=2)
realDist

plot(data, col=unclass(as.factor(group))+1)


pValue <- mean(distVector>realDist)
pValue

