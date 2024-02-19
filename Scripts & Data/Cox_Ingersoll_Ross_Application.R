#Cox-Ingersoll-Ross Application

library(pracma)

rm(list =ls())

tic()

#(Section - 1) deriving the rate model for 12-month interval 

n_12_term<-10^4
r_0<-.07
s_0<-50
k<-50
T<-1
d_12_term<-12
delta_12_term = T/d_12_term
sigma <- .13
z_12_term<-matrix(rnorm(n_12_term*(d_12_term+1)),nrow=n_12_term)
r_12_term<-matrix(numeric(n_12_term*(d_12_term+1)),nrow=n_12_term)
r_12_term[,1] = r_0
for (i in 2:(d_12_term+1))
  {
    r_12_term[,i] = r_12_term[,i-1]+0.23*(0.081-r_12_term[,i-1])*delta_12_term+0.09*sqrt(r_12_term[,i-1]*delta_12_term)*z_12_term[,i]
  }


#(Section - 2) deriving the asset price model for a 12-month interval

x_12_term<-matrix(rnorm(n_12_term*(d_12_term+1)),nrow=n_12_term)
s_12_term<-matrix(numeric(n_12_term*(d_12_term+1)),nrow=n_12_term)
s_12_term[,1] = s_0
for(i in 2:(d_12_term+1))
  {
    s_12_term[,i] <- (s_12_term[,i-1])*exp((r_12_term[,i-1]-sigma^2/2)*delta_12_term+sigma*sqrt(delta_12_term)*x_12_term[,i])
  }


#(Section - 3) finding the discounted payoff amount for a 12-month interval

rate_sum_12_term = apply(r_12_term[,1:d_12_term],1,sum)
eurocallpayoff_12_term = pmax(s_12_term[,(d_12_term+1)]-k,0)*exp(-delta_12_term*rate_sum_12_term)
eurocallpayoff_12_term


#(section - 4) find the optimal sample size and price of option for a 12-month interval

epsilon_12_term <-0.05 #absolute error tolerance
hat_sigma_12_term<-sd(eurocallpayoff_12_term) 
c_12_term<-1.1 #amplifying constant
N_12_term<-ceiling((2.58*c_12_term*hat_sigma_12_term/epsilon_12_term)^2)
N_12_term

revised_z_12_term<-matrix(rnorm(N_12_term*(d_12_term+1)),nrow=N_12_term)
revised_r_12_term<-matrix(numeric(N_12_term*(d_12_term+1)),nrow=N_12_term)
revised_r_12_term[,1] = r_0
for (i in 2:(d_12_term+1))
{
  revised_r_12_term[,i] = revised_r_12_term[,i-1]+0.23*(0.081-revised_r_12_term[,i-1])*delta_12_term+0.09*sqrt(revised_r_12_term[,i-1]*delta_12_term)*revised_z_12_term[,i]
}

revised_x_12_term<-matrix(rnorm(N_12_term*(d_12_term+1)),nrow=N_12_term)
revised_s_12_term<-matrix(numeric(N_12_term*(d_12_term+1)),nrow=N_12_term)
revised_s_12_term[,1] = s_0
for(i in 2:(d_12_term+1))
{
  revised_s_12_term[,i] <- (revised_s_12_term[,i-1])*exp((revised_r_12_term[,i-1]-sigma^2/2)*delta_12_term+sigma*sqrt(delta_12_term)*revised_x_12_term[,i])
}

revised_rate_sum_12_term = apply(revised_r_12_term[,1:d_12_term],1,sum)
revised_eurocallpayoff_12_term = pmax(revised_s_12_term[,(d_12_term+1)]-k,0)*exp(-delta_12_term*revised_rate_sum_12_term)

revised_option_price_12_term = mean(revised_eurocallpayoff_12_term)
revised_option_price_12_term
revised_Callerror_12_term<-2.58*sd(eurocallpayoff_12_term)/sqrt(N_12_term)
revised_Callerror_12_term

#(Section - 5) Find the price of an option for a 12-month interval after 1 month.

rate_sum_12_term_1_month = apply(revised_r_12_term[,1:2],1,sum)
revised_eurocallpayoff_1_month_12_term = pmax(revised_s_12_term[,2]-k,0)*exp(-delta_12_term*rate_sum_12_term_1_month)
revised_option_price_1_month_12_term = mean(revised_eurocallpayoff_1_month_12_term)
revised_option_price_1_month_12_term






#(Section - 6) deriving the rate model for 52-week interval 

n_52_term<-3
r_0<-.07
s_0<-50
k<-50
T<-1
d_52_term<-52
delta_52_term = T/d_52_term
sigma <- .13
z_52_term<-matrix(rnorm(n_52_term*(d_52_term+1)),nrow=n_52_term)
r_52_term<-matrix(numeric(n_52_term*(d_52_term+1)),nrow=n_52_term)
r_52_term[,1] = r_0
for (i in 2:(d_52_term+1))
{
  r_52_term[,i] = r_52_term[,i-1]+0.23*(0.081-r_52_term[,i-1])*delta_52_term+0.09*sqrt(r_52_term[,i-1]*delta_52_term)*z_52_term[,i]
}


#(Section - 7) deriving the asset price model for a 52-week interval

x_52_term<-matrix(rnorm(n_52_term*(d_52_term+1)),nrow=n_52_term)
s_52_term<-matrix(numeric(n_52_term*(d_52_term+1)),nrow=n_52_term)
s_52_term[,1] = s_0
for(i in 2:(d_52_term+1))
{
  s_52_term[,i] <- (s_52_term[,i-1])*exp((r_52_term[,i-1]-sigma^2/2)*delta_52_term+sigma*sqrt(delta_52_term)*x_52_term[,i])
}


#(Section - 8) finding the discounted payoff amount for a 52-week interval

rate_sum_52_term = apply(r_52_term[,1:d_52_term],1,sum)
eurocallpayoff_52_term = pmax(s_52_term[,(d_52_term+1)]-k,0)*exp(-delta_52_term*rate_sum_52_term)
eurocallpayoff_52_term


#(section - 9) find the optimal sample size and price of option for a 52-week interval

epsilon_52_term <-0.05 #absolute error tolerance
hat_sigma_52_term<-sd(eurocallpayoff_52_term) 
c_52_term<-1.1 #amplifying constant
N_52_term<-ceiling((2.58*c_52_term*hat_sigma_52_term/epsilon_52_term)^2)

revised_z_52_term<-matrix(rnorm(N_52_term*(d_52_term+1)),nrow=N_52_term)
revised_r_52_term<-matrix(numeric(N_52_term*(d_52_term+1)),nrow=N_52_term)
revised_r_52_term[,1] = r_0
for (i in 2:(d_52_term+1))
{
  revised_r_52_term[,i] = revised_r_52_term[,i-1]+0.23*(0.081-revised_r_52_term[,i-1])*delta_52_term+0.09*sqrt(revised_r_52_term[,i-1]*delta_52_term)*revised_z_52_term[,i]
}

revised_x_52_term<-matrix(rnorm(N_52_term*(d_52_term+1)),nrow=N_52_term)
revised_s_52_term<-matrix(numeric(N_52_term*(d_52_term+1)),nrow=N_52_term)
revised_s_52_term[,1] = s_0
for(i in 2:(d_52_term+1))
{
  revised_s_52_term[,i] <- (revised_s_52_term[,i-1])*exp((revised_r_52_term[,i-1]-sigma^2/2)*delta_52_term+sigma*sqrt(delta_52_term)*revised_x_52_term[,i])
}

revised_rate_sum_52_term = apply(revised_r_52_term[,1:d_52_term],1,sum)
revised_eurocallpayoff_52_term = pmax(revised_s_52_term[,(d_52_term+1)]-k,0)*exp(-delta_52_term*revised_rate_sum_52_term)

revised_option_price_52_term = mean(revised_eurocallpayoff_52_term)
revised_option_price_52_term
revised_Callerror_52_term<-2.58*sd(eurocallpayoff_52_term)/sqrt(N_52_term)
revised_Callerror_52_term

#(Section - 10) Find the price of an option for a 52-week interval after 1-week

rate_sum_52_term_1_week = apply(revised_r_52_term[,1:2],1,sum)
revised_eurocallpayoff_1_week_52_term = pmax(revised_s_52_term[,2]-k,0)*exp(-delta_52_term*rate_sum_52_term_1_week)
revised_option_price_1_week_52_term = mean(revised_eurocallpayoff_1_week_52_term)
revised_option_price_1_week_52_term






#(Section - 11) 12-month european option under GBM

n_12_GBM<-10^4
d_12_GBM<-12
MT_12_GBM<-1
delta_12_GBM<-MT_12_GBM/d_12_GBM
r_0<-.07
s_0<-50
k<-50
sigma<- .13

x_12_GBM<-matrix(rnorm(n_12_GBM*d_12_GBM),nrow=n_12_GBM)
if(d_12_GBM==1) BM_12<-sqrt(delta_12_GBM)*as.matrix(apply(x_12_GBM,1,cumsum)) else BM_12<- sqrt(delta_12_GBM)*t(apply(x_12_GBM,1,cumsum))
grid_12_GBM<-seq(delta_12_GBM,MT_12_GBM,length.out=d_12_GBM)
S_12_GBM<-s_0*exp(sweep(sigma*BM_12,MARGIN=2,(r_0-sigma^2/2)*grid_12_GBM,'+'))

EuroCallPayoff_12_GBM<-pmax(S_12_GBM[,d_12_GBM]-k,0)*exp(-r_0*MT_12_GBM)
EuroCallPrice_12_GBM<-mean(EuroCallPayoff_12_GBM)
EuroCallPrice_12_GBM

#(Section - 12) 12-month european option under GBM after 1-month

EuroCallPayoff_12_GBM_1_month<-pmax(S_12_GBM[,1]-k,0)*exp(-r_0*delta_12_GBM)
EuroCallPrice_12_GBM_1_month<-mean(EuroCallPayoff_12_GBM_1_month)
EuroCallPrice_12_GBM_1_month

#(section - 13) 52-Week european option under GBM 

n_52_GBM<-10^3
d_52_GBM<-52
MT_52_GBM<-1
delta_52_GBM<-MT_52_GBM/d_52_GBM
r_0<-.07
s_0<-50
k<-50
sigma<- .13

x_52_GBM<-matrix(rnorm(n_52_GBM*d_52_GBM),nrow=n_52_GBM)
if(d_52_GBM==1) BM_52<-sqrt(delta_52_GBM)*as.matrix(apply(x_52_GBM,1,cumsum)) else BM_52<- sqrt(delta_52_GBM)*t(apply(x_52_GBM,1,cumsum))
grid_52_GBM<-seq(delta_52_GBM,MT_52_GBM,length.out=d_52_GBM)
S_52_GBM<-s_0*exp(sweep(sigma*BM_52,MARGIN=2,(r_0-sigma^2/2)*grid_52_GBM,'+'))

EuroCallPayoff_52_GBM<-pmax(S_52_GBM[,d_52_GBM]-k,0)*exp(-r_0*MT_52_GBM)
EuroCallPrice_52_GBM<-mean(EuroCallPayoff_52_GBM)
EuroCallPrice_52_GBM

#(section 14) 52-week european option under GBM after 1-week

EuroCallPayoff_52_GBM_1_week<-pmax(S_52_GBM[,1]-k,0)*exp(-r_0*delta_52_GBM)
EuroCallPrice_52_GBM_1_week<-mean(EuroCallPayoff_52_GBM_1_week)
EuroCallPrice_52_GBM_1_week






#(section 15) summary of results:

cat("The sample size for a 12-month European call option using the Cox-Ingersoll_Ross model is",N_12_term) 

cat("The option price for a 12-month European call option using the Cox-Ingersoll_Ross model is $",revised_option_price_12_term)

cat("The option price for a 12-month European call option after 1-month using the Cox-Ingersoll_Ross model is $",revised_option_price_1_month_12_term)

cat("The sample size for a 52-month European call option using the Cox-Ingersoll_Ross model is",N_52_term)

cat("The option price for a 52-week European call option using the Cox-Ingersoll_Ross model is $",revised_option_price_52_term)

cat("The option price for a 52-week European call option after 1-week using the Cox-Ingersoll_Ross model is $",revised_option_price_1_week_52_term)

cat("The option price for a 12-month European call option using geometric brownian motion is $",EuroCallPrice_12_GBM)

cat("The option price for a 12-month European call option after 1-month using geometric brownian motion is $",EuroCallPrice_12_GBM_1_month)

cat("The option price for a 52-week European call option using geometric brownian motion is $",EuroCallPrice_52_GBM)

cat("The option price for a 52-week European call option after 1-week using geometric brownian motion is $",EuroCallPrice_52_GBM_1_week)


#(section 16) Appendix: sample paths


#(12-month) European option: first 10 paths - asset price
plot((0:d_12_term)*delta_12_term,revised_r_12_term[1,],type='l',ylim=c(0,0.15),col=1,ylab='Interest Rate',xlab='Time (12-month)',main='Table 1: (12-month) Rate Path - first 10 paths')
for (i in 1:10){
  points((0:d_12_term)*delta_12_term,revised_r_12_term[i,],type='l',col=i)
}

#(12-month) European option: first 10 paths - asset price
plot((0:d_12_term)*delta_12_term,revised_s_12_term[1,],type='l',ylim=c(30,70),col=1,ylab='Stock Price',xlab='Time (12-month)',main='Table 2: (12-month) Asset Price - first 10 paths')
for (i in 1:10){
  points((0:d_12_term)*delta_12_term,revised_s_12_term[i,],type='l',col=i)
}

#(12-month) European option: first 10 paths - asset price
plot((0:d_52_term)*delta_52_term,revised_r_52_term[1,],type='l',ylim=c(0,0.15),col=1,ylab='Interest Rate',xlab='Time (52-weeks)',main='Table 3: (52-week) Rate Path - first 10 paths')
for (i in 1:10){
  points((0:d_52_term)*delta_52_term,revised_r_52_term[i,],type='l',col=i)
}

#(52-week) European Option first 10 paths - asset price
plot((0:d_52_term)*delta_52_term,revised_s_52_term[1,],type='l',ylim=c(20,80),col=1,ylab='Stock Price',xlab='Time (52-weeks)',main ='Table 4: (52-weeks) Asset Price: first 10 paths')
for (i in 1:10){
  points((0:d_52_term)*delta_52_term,revised_s_52_term[i,],type='l',col=i)
}



toc()

