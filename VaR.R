(
  WD <- getwd()
)
if (!is.null(WD)) setwd(WD)

df <- read.csv("S&P500.csv",sep = ",")
#df <- df[nrow(df):1,]

stopaPR <- function(.df){
  Volatility <- NULL
  for(i in 1:(nrow(.df)-1)){
    Volatility[i] <- ((.df$Adj.Close[i+1]-.df$Adj.Close[i])/(.df$Adj.Close[i]))*100
  }
  Volatility[nrow(.df)] <- NA
  .df <- cbind(.df, Volatility)
  return(.df)
}

stopaLN <- function(.df){
  VolatilityLN <- NULL
  for(i in 1:(nrow(.df)-1)){
    VolatilityLN[i] <- log(.df$Adj.Close[i+1]/.df$Adj.Close[i])*100
  }
  VolatilityLN[nrow(.df)] <- NA
  .df <- cbind(.df, VolatilityLN)
  return(.df)
}

mChristoffersen <- function(.vector,.VaR){ # Christoffersen's back test
  a <- c(0,0,0,0)
  switch <- c(FALSE,FALSE,FALSE,FALSE)
  count <- 1
  repeat{
    if(count == 1){
      if(.vector[count] < .VaR){
        a[2] <- a[2] + 1
        switch[2] <- TRUE
        count <- count + 1
      }else{
        a[1] <- a[1] + 1
        switch[1] <- TRUE
        count <- count + 1
      }
    }else{
      if(.vector[count] < .VaR){
        if(match(TRUE, switch) == 2){
          a[4] <- a[4] + 1
          switch[match(TRUE, switch)] <- FALSE
          switch[4] <- TRUE
          count <- count + 1
        } else if(match(TRUE, switch) == 1){
          a[2] <- a[2] + 1
          switch[match(TRUE, switch)] <- FALSE
          switch[2] <- TRUE
          count <- count + 1
        } else if(match(TRUE, switch) == 3){
          a[4] <- a[4] + 1
          switch[match(TRUE, switch)] <- FALSE
          switch[4] <- TRUE
          count <- count + 1
        }else{
          a[4] <- a[4] + 1
          count <- count + 1
        }
      }else{
        if(match(TRUE, switch) == 4){
          a[3] <- a[3] + 1
          switch[match(TRUE, switch)] <- FALSE
          switch[3] <- TRUE
          count <- count + 1
        } else if(match(TRUE, switch) == 3){
          a[1] <- a[1] + 1
          switch[match(TRUE, switch)] <- FALSE
          switch[1] <- TRUE
          count <- count + 1
        } else if(match(TRUE, switch) == 2){
          switch[match(TRUE, switch)] <- FALSE
          switch[3] <- TRUE
          a[3] <- a[3] + 1
          count <- count + 1
        } else{
          a[1] <- a[1] + 1
          count <- count + 1
        }
      }
    }
    if(count > length(.vector)){
      break
    }
  }
  q0 <- ((a[1])/(a[1]+a[2]))
  q1 <- ((a[3])/(a[3]+a[4]))
  q <- ((a[1]+a[3])/(sum(a)))
  st <- ((q/q0)^a[1])*(((1-q)/(1-q0))^a[2])*((q/q1)^a[3])*(((1-q)/(1-q1))^a[4])
  final <- -2*log(st)
  return(final)
}

mKupiec <- function(.vector,.VaR){
  count <- 0
  for(i in 1:length(.vector)){
    if(.vector[i] < .VaR){
      count <- count + 1
    }
  }
  return(count)
}

mVaR <- function(.vector){
  N <- 250
  final2 <- NULL
  final5 <- NULL
  finalC2 <- NULL
  finalC5 <- NULL
  kupiec1 <- NULL
  kupiec5 <- NULL
  chris1 <- NULL
  chris5 <- NULL
  count <- 1
  repeat {
    tempVector <- sort(.vector[count:(count-1+N)],decreasing = FALSE)
    # final2[count] <- -1*tempVector[(0.01*N)]
    # final5[count] <- -1*tempVector[(0.05*N)]
    # finalC2[count] <- -1*mean(tempVector[1:(0.01*N)])
    # finalC5[count] <- -1*mean(tempVector[1:(0.05*N)])
    # kupiec1[count] <- mKupiec(-.vector[count:(count-1+N)],final2[count])
    # kupiec5[count] <- mKupiec(-.vector[count:(count-1+N)],final5[count])
    final2[count] <- tempVector[(0.01*N)]
    final5[count] <- tempVector[(0.05*N)]
    finalC2[count] <- mean(tempVector[1:(0.01*N)])
    finalC5[count] <- mean(tempVector[1:(0.05*N)])
    kupiec1[count] <- mKupiec(.vector[count:(count-1+N)],final2[count])
    kupiec5[count] <- mKupiec(.vector[count:(count-1+N)],final5[count])
    chris1[count] <- mChristoffersen(.vector[count:(count-1+N)],final2[count])
    chris5[count] <- mChristoffersen(.vector[count:(count-1+N)],final5[count])
    count <- count + 1
    if((count+N) == length(.vector)){
      break
    }
  }
  return(cbind(final2,final5,finalC2,finalC5,kupiec1,kupiec5,chris1,chris5))
}

VaRW <- function(.vector){
  N <- 250
  final2 <- NULL
  final5 <- NULL
  finalC2 <- NULL
  finalC5 <- NULL
  kupiec1 <- NULL
  kupiec5 <- NULL
  chris1 <- NULL
  chris5 <- NULL
  count <- 1
  q <- 0.96
  repeat {
    tempVector <- sort(.vector[count:(count-1+N)],decreasing = FALSE)
    temp <- rep(0,N)
    for(i in 1:N){
      if( i > 1){
        temp[i] <- temp[i-1]+((1-q)*q^(N-match(tempVector[i],.vector[count:(count-1+N)]))/(1-q^N))
      }else{
        temp[i] <- ((1-q)*q^(N-match(tempVector[i],.vector[count:(count-1+N)]))/(1-q^N))
      }
    }
    for(i in 1:length(temp)){
      if(temp[i]>=0.01){
        final2[count] <- tempVector[i]
        finalC2[count] <- mean(tempVector[1:match(final2[count],tempVector)])
        kupiec1[count] <- mKupiec(.vector[count:(count-1+N)],final2[count])
        chris1[count] <- mChristoffersen(.vector[count:(count-1+N)],final2[count])
        break
      }
    }
    for(i in 1:length(temp)){
      if(temp[i]>=0.05){
        final5[count] <- tempVector[i]
        finalC5[count] <- mean(tempVector[1:match(final5[count],tempVector)])
        kupiec5[count] <- mKupiec(.vector[count:(count-1+N)],final5[count])
        chris5[count] <- mChristoffersen(.vector[count:(count-1+N)],final5[count])
        break
      }
    }
    count <- count + 1
    if((count+N) == length(.vector)){
      break
    }
  }
  return(cbind(final2,final5,finalC2,finalC5,kupiec1,kupiec5,chris1,chris5))
}

EWMA <- function(.vector){
  weight <- 0.96
  N <- 250
  count <- 1
  final1 <- NULL
  final5 <- NULL
  ES99 <- NULL
  ES95 <- NULL
  kupiec1 <- NULL
  kupiec5 <- NULL
  chris1 <- NULL
  chris5 <- NULL
  repeat {
    tempVector <- .vector[count:(count-1+N)]
    temp <- rep(0,N)
    w <- rep(0,N)
    for(i in 1:N){
      temp[i] <- ((tempVector[i])^2)*(1-weight)*(weight^(i-1))
    }
    final1[count] <- -2.33*sqrt(sum(temp))
    final5[count] <- -1.64*sqrt(sum(temp))
    ES99[count] <- final1[count]*(exp(-(2.33^2)/2))/(sqrt(2*pi)*0.01)
    ES95[count] <- final5[count]*(exp(-(1.64^2)/2))/(sqrt(2*pi)*0.05)
    kupiec1[count] <- mKupiec(.vector[count:(count-1+N)],final1[count])
    kupiec5[count] <- mKupiec(.vector[count:(count-1+N)],final5[count])
    chris1[count] <- mChristoffersen(.vector[count:(count-1+N)],final1[count])
    chris5[count] <- mChristoffersen(.vector[count:(count-1+N)],final5[count])
    count <- count + 1
    if((count+N) == length(.vector)){
      break
    }
  }
  return(cbind(final1,final5,kupiec1,kupiec5,chris1,chris5,ES99,ES95))
}

test <- stopaPR(df)
test <- stopaLN(test)

test1 <- mVaR(test$Volatility)

plot(test$Volatility[251:7823],type="l",main = "VaR for 99% & 95%, simple HS", ylab = "Daily VaR in %", xlab = "number of observations")
lines(test1[,1], type = "l", col = "red")
lines(test1[,2], type = "l", col = "green")

plot(test$Volatility[251:7823],type="l",main = "CVaR for 99% & 95%, simple HS", ylab = "Daily VaR in %", xlab = "number of observations")
lines(test1[,3], type = "l", col = "red")
lines(test1[,4], type = "l", col = "green")

plot(test1[,5],type="p",main = "Wyniki testu Kupca dla 99% VaR", ylab = "Liczba przekroczen", xlab = "number of observations")
abline(h=8,col="red")
plot(test1[,6],type="p",main = "Wyniki testu Kupca dla 95% VaR", ylab = "Liczba przekroczen", xlab = "number of observations")
abline(h=6,col="red")
abline(h=20,col="red")

plot(test1[,5],type="p",main = "Wyniki testu wartosci rzeczywistych dla 99% VaR", ylab = "Liczba przekroczen", xlab = "number of observations")
abline(h=5,col="red")
plot(test1[,6],type="p",main = "Wyniki testu wartosci rzeczywistych dla 95% VaR", ylab = "Liczba przekroczen", xlab = "number of observations")
abline(h=7,col="red")
abline(h=20,col="red")

test2 <- VaRW(test$Volatility)

plot(test$Volatility[251:7823],type="l",main = "VaR for 99% & 95%, AWHS", ylab = "Daily VaR in %", xlab = "number of observations")
lines(test2[,1], type = "l", col = "red")
lines(test2[,2], type = "l", col = "green")

plot(test$Volatility[251:7823],type="l",main = "CVaR for 99% & 95%, AWHS", ylab = "Daily VaR in %", xlab = "number of observations")
lines(test2[,3], type = "l", col = "red")
lines(test2[,4], type = "l", col = "green")

plot(test2[,5],type="p",main = "Wyniki testu Kupca dla 99% VaR, metoda AWHS", ylab = "Liczba przekroczen", xlab = "number of observations")
abline(h=8,col="red")
plot(test2[,6],type="p",main = "Wyniki testu Kupca dla 95% VaR, metoda AWHS", ylab = "Liczba przekroczen", xlab = "number of observations")
abline(h=6,col="red")
abline(h=20,col="red")

plot(test2[,5],type="p",main = "Wyniki testu wartosci rzeczywistych dla 99% VaR, metoda AWHS", ylab = "Liczba przekroczen", xlab = "number of observations")
abline(h=5,col="red")
plot(test2[,6],type="p",main = "Wyniki testu wartosci rzeczywistych dla 95% VaR, metoda AWHS", ylab = "Liczba przekroczen", xlab = "number of observations")
abline(h=7,col="red")
abline(h=20,col="red")

test3 <- EWMA(test$VolatilityLN)

plot(test$VolatilityLN,type="l",main = "VaR for 99% & 95%, EWMA", ylab = "Daily VaR in %", xlab = "number of observations")
lines(test3[,1], type = "l", col = "red")
lines(test3[,2], type = "l", col = "green")

plot(test$VolatilityLN,type="l",main = "ES for 99% & 95%, EWMA", ylab = "Daily ES in %", xlab = "number of observations")
lines(test3[,7], type = "l", col = "red")
lines(test3[,8], type = "l", col = "green")

plot(test3[,3],type="p",main = "Wyniki testu Kupca dla 99% VaR, metoda EWMA", ylab = "Liczba przekroczen", xlab = "number of observations")
abline(h=8,col="red")
plot(test3[,4],type="p",main = "Wyniki testu Kupca dla 95% VaR, metoda EWMA", ylab = "Liczba przekroczen", xlab = "number of observations")
abline(h=6,col="red")
abline(h=20,col="red")

plot(test3[,3],type="p",main = "Wyniki testu testu wartosci rzeczywistych dla 99% VaR, metoda EWMA", ylab = "Liczba przekroczen", xlab = "number of observations")
abline(h=5,col="red")
plot(test3[,4],type="p",main = "Wyniki testu testu wartosci rzeczywistych dla 95% VaR, metoda EWMA", ylab = "Liczba przekroczen", xlab = "number of observations")
abline(h=7,col="red")
abline(h=20,col="red")
