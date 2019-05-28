library(itsmr)
deaths<- itsmr::deaths

ts.plot(deaths)

# difference once at lag 12
deaths12<-diff(deaths,lag=12)
ts.plot(deaths12)

# difference once at lag 1
deaths1<-diff(deaths12,lag=1)
ts.plot(deaths1)

# get sample acvf
acf(deaths1,type="covariance")
values<-acf(deaths1,type="covariance", lag.max=20)

#acvf values
values

#sample means
mean(deaths);

mean(deaths12);

mean(deaths1);