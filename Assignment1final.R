setwd("C:/Users/USER/Desktop")
library(readr)
library(tidyverse)
library(xts)
library(dplyr)
CAN <- read.csv("CAN.csv", stringsAsFactors = FALSE)
glimpse(CAN)
coupon_rate <- c(1.5, 0.75, 0.75, 0.75, 0.5, 2.75, 1.75, 1.5, 2.25, 1.5)
YTM <- CAN[2:11]
YTM
settle_date <- as.Date.character(CAN[1]$DATE,tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))
settle_date
#calculate the yields for the first bond.
YTM[1]
ytm1 <- c()
for (i in 1:10) {
  r <- (-6)*log((YTM[[1]][i]+1.5/3)/(100+1.5/2))
  ytm1[i] <- r
}
YTM[1] <- ytm1
YTM
#Calculate the yield for the second bond

ytm2 <- c()
for (i in 1:10) {
  middle_value <- YTM[[2]][i]+0.25-(0.75/2)*exp((-1)*YTM[[1]][i])
  r <- r <- (-3/2)*log(middle_value/(0.75/2+100))
  ytm2[i] <- r
}
YTM[2] <- ytm2
YTM
#Calculate the yield for the third bond
#Function for middle value
intermedia <- function(N,Rate,Order){
  sb <- 0
  for (i in 1:(N-1)) {
  r <- as.numeric(Rate[[i]][Order])
  sb <- sb + exp(r*(-1)*(1/6+(i-1)/2))
  }
  sb
}
ytm3 <- c()

for (j in 1:10) {
  inter <- intermedia(3,YTM,j)
  inter <- as.numeric(YTM[[3]][j]) + 0.25 - inter*0.75/2
  r <- (-6/7)*log(inter/(100+0.75/2))
  ytm3[j] <- r
}
YTM[3] <- ytm3
YTM
#For the rest of them
for (k in 4:10) {
  ytm <- c()
  for (j in 1:10) {
  pizza <- intermedia(k,YTM,j) 
  C <- as.numeric(coupon_rate[k])
  pizza <- as.numeric(YTM[[k]][j]) +C/3 - pizza*C/2
  denominator <- (-1)/(1/6 + (k-1)/2)
  r <- log(pizza/(C/2 + 100))*denominator
 
  ytm[j] <- r
  }
YTM[k] <- ytm  
}

YTM_matrix <- as.matrix(YTM)
YTM_matrix
list_of_colors <- sample(colours(), length(settle_date))
list_of_colors
timeinterval <- c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)
for (n in 1:length(list_of_colors)) {
  if(n == 1){
    daydata <- as.vector(YTM_matrix[n,1:10])
    plot(timeinterval,daydata,xlim = c(0,6),ylim = c(0.014,0.025),type = 'b', col = list_of_colors[n], 
         xlab = 'Maturity period(years)', ylab = 'Yield(%)')
  }
  else{
    par(new = TRUE)
    daydata <- as.vector(YTM_matrix[n,1:10])
    plot(timeinterval,daydata,axes = FALSE,xlim = c(0,6),ylim = c(0.014,0.025),type = 'b', col = list_of_colors[n],
         xlab = "",ylab = '', main = '')
  }
}
legend('topright',legend = settle_date,
       col = list_of_colors,
       pch = 15)
title('5-year-yield curve')

#Problem
#Note for the first bond, since it has no coupon payment, the spot rate and the YTM are the same.
SPOT <- YTM
# Calculate the 2-period spot rate by data from the second bond.
#We know the 2-period-spot rate is the same as the YTM for zero coupon bond for 2 period
Bond_Price <- CAN[2:11]
Find_spot_rate <- function(period_to_maturity, market_price, coupon, yield_ytm, settletime){
  if(period_to_maturity == 0){
    sr <- as.numeric(yield_ytm[[period_to_maturity]][settletime])
    sr
  }
  fill_in_bond <- filter(CAN_data, DATE == settletime& Interval_num < period_to_maturity)
  Phi <- 0
  for (i in 1:nrow(fill_in_bond)) {
    time_int <- fill_in_bond[i:'Interval_num']
    ratevalue <- fill_in_bond[i:'rate_value']
    Phi <- Phi+ (coupon/2)*exp((-1)*((time_int+ 1/6))* ratevalue)
    
  }
  spotr <- ((market_price + coupon*(1/3))- Phi)/(0.5*coupon + 100)
  spotr <- log(spotr)*((-1)*(1/6 + period_to_maturity))
  spotr
}
CAN_data <- read.csv("CANmatrix.csv", stringsAsFactors = FALSE)
glimse(CAN_data)
CAN_date$spotr <- 0
for (i in nrow(CAN_data)) {
  CAN_data[i,'spotr'] <- Find_spot_rate(CAN_data[i,'DATE'],
                                        Bond_Price[i],
                                        coupon_rate[i],
                                        YTM[i],
                                        CAN_data[i,'settle_date']
                                        )
  
}
SPOT <- CAN_data$spotr
SPOT_matrix <- as.matrix(SPOT)
SPOT_matrix[,1]
list_of_colors <- sample(colours(), length(settle_date))
list_of_colors
timeinterval <- c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)
for (n in 1:length(list_of_colors)) {
  if(n == 1){
    daydata <- as.vector(SPOT_matrix[n,1:10])
    plot(timeinterval,daydata,xlim = c(0,6),ylim = c(0.01,0.025),type = 'l', col = list_of_colors[n], 
         xlab = 'Maturity period(years)', ylab = 'SPOT RATE(%)')
  }
  else{
    par(new = TRUE)
    daydata <- as.vector(SPOT_matrix[n,1:10])
    plot(timeinterval,daydata,axes = FALSE,xlim = c(0,6),ylim = c(0.01,0.025),type = 'l', col = list_of_colors[n],
         xlab = "",ylab = '', main = '')
  }
}
legend('topright',legend = settle_date,
       col = list_of_colors,
       pch = 15)
title('5-year-spot curve')

#Subproblem3 Calculate the forward-rate
SPOT
SPOT_matrix <- as.matrix(SPOT)
SPOT_matrix[,1]

# Now we want to construct a log-price matrix for zero coupon bond
for (i in 1:10) {
  for(j in 1:10){
   srate <- 1 + SPOT_matrix[j,i]
   realprice <- 100/(srate)^(1/6 + (i - 1)/2)
   logprice <- log(realprice)
   SPOT_matrix[j,i] <- logprice
  }
  
}
Zbond_price_matrix <- SPOT_matrix
Forward_rate_matrix <- Zbond_price_matrix
Forward_rate_matrix[1,]
#Imply the forward rate formula
for (i in 1:10) {
  forward_rate <- c()
  for (j in 1:10) {
    forward_rate_1toj <- as.numeric((-1)*(Zbond_price_matrix[i,j] - Zbond_price_matrix[i,2])/(1/6+(j-2)*0.5))
    Forward_rate_matrix[i,j] <- forward_rate_1toj
    
  }
  
}

Forward_rate_matrix
SPOT
forward_gap <- as.vector(seq(1,4.5, by = 0.5))
forward_gap

list_of_colors2 <- sample(colours(), length(forward_gap))
list_of_colors2
for (n in 1:length(list_of_colors2)) {
  if(n == 1){
    daydata <- as.vector(Forward_rate_matrix[n,3:10])
    plot(forward_gap,daydata,xlim = c(0,6),ylim = c(0.010,0.02),type = 'b', col = list_of_colors2[n], 
         xlab = 'Years', ylab = 'Forward Rate(%)')
  }
  else{
    par(new = TRUE)
    daydata <- as.vector(Forward_rate_matrix[n,3:10])
    plot(forward_gap,daydata,axes = FALSE,xlim = c(0,6),ylim = c(0.010,0.02),type = 'b', col = list_of_colors2[n],
         xlab = "",ylab = '', main = '')
  }
}
legend('topright',legend = settle_date,
       col = list_of_colors2,
       pch = 15)
title('5-year-forward_rate')

#Calculate the covariance matrix
#pick all bond whose maturity are in March

Bondmarch <- c()
selectedbond <- as.vector(seq(1,9, by = 2))
for (i in 1:5) {
  Bondmarch[i] <- YTM[selectedbond[i]]
  
}
Bondmarch[[1]][1]
comatrix_yield <- matrix(nrow = 5,ncol = 9)

for (i in 1:5) {
  for (j in 1:9) {
    ratio <- Bondmarch[[i]][j+1]/Bondmarch[[i]][j]
    comatrix_yield[i,j] <- as.numeric(log(ratio))
    
  }
  
}
#get the transpose of it
trans_matrix <- t(comatrix_yield)
covariance_matrix_yield <- comatrix_yield %*% trans_matrix
covariance_matrix_yield




comatrix_forward <- matrix(nrow = 4,ncol = 9)
for (i in 1:4) {
  for (j in 1:9) {
    ratio <- Forward_rate_matrix[j+1,i+3]/Forward_rate_matrix[j,i+3]
    comatrix_forward[i,j] <- log(ratio)
    
  }
  
}
trans_matrix <- t(comatrix_forward)
covariance_matrix_forward_rate <- comatrix_forward %*% trans_matrix
covariance_matrix_forward_rate
#Get the eigenvalue and eigenvextor for both covairance matrix.

#Get the eigenvalues for the covariance matrix for yield
ev_yield <- eigen(covariance_matrix_yield)
ev_yield
ev_forward_rate <- eigen(covariance_matrix_forward_rate)
ev_forward_rate

