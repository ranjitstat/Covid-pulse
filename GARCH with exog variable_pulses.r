library(rugarch)

names(mydata)
attach(mydata)
as.matrix(data1)
nrow(mydata)

dta_A <- density(DELHI, na.rm = TRUE)
dta_B <- density(MUMBAI, na.rm = TRUE)
plot(dta_A, col = "blue", ylim = c(0, max(dta_A$y,dta_B$y)))  
lines(dta_B, col = "red")

legend("topright", c("VarA","VarB"), lty = c(1,1), col = c("blue","red"))

regressor <- as.matrix(cbind(ld),nrow=len(mydata))

predictor <- as.matrix((KOLKATA),nrow=len(mydata))
lrtn=diff(log(KOLKATA))

spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1), 
                                         submodel =NULL, external.regressors = regressor,variance.targeting = F),
                   mean.model = list(armaOrder = c(1,1),arfima=F,archm=F, external.regressors = regressor), distribution.model = "norm")

garch <- ugarchfit(spec=spec,data=lrtn)
head(sigma(garch))
plot(garch,which="all")
