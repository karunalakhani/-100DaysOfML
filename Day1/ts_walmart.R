#**********************************************************************************
# Problem Statement - Walmart Sales Prediction.
# Method Used       - Time Series
# Written By        - Ankush Agrekar, Karan Katiyar, Pooja Mutreja, Sayonti Mondal
#**********************************************************************************

library(forecast)

# Loading the train.csv file
dat_train <- read.csv("train.csv", header = T)

print("Welcome to the Walmart Sales Predictor")

#Accepting User input for the model
svar <- readline(prompt = "Enter the Store Number (1-45): ")
pvar <- readline(prompt = "Enter the Number of predictions to be made: ")

svardf <- as.data.frame(svar)
colnames(svardf) <- c("Store")

cat('Extracting data for Store: ',svar,"\n")
store_dat <- merge(dat_train, svardf, by=c("Store"))

fit_ts <- function(dat){
#***********************************************************************
# Function to process and Clean data to remove negative and zero values
# and fit a seasonal arima Model.
#
# Args:
#    dat: an atomic variable containing the name of the data variable
#
# Returns:
#    the arima model
#***********************************************************************
  
  reqd.dat <- as.data.frame(get(dat))
  for (j in 1:nrow(reqd.dat)) {
    if (reqd.dat[j,4] < 0) {
      reqd.dat$Weekly_Sales[reqd.dat$Weekly_Sales <= 0] = median(reqd.dat$Weekly_Sales)
    }
  }
  logSales <- log10(reqd.dat$Weekly_Sales)
  return(Arima(logSales, order = c(6,0,0), seasonal = list(order =c(1,1,0), period = 52), method ="CSS"))
}

final.pred <- NULL
final.file <- NULL

# list of available Departments in Stores
uniqsd <- unique(store_dat[c("Store","Dept")])
for (i in 1:nrow(uniqsd)) {
    varnam <- paste("s",svar,"d",uniqsd[i,2], sep = "")
    assign(varnam, merge(store_dat, uniqsd[i,], by=c("Store","Dept")))
    
    if(nrow(get(varnam)) < 104){
      cat("Insufficient data for Department: ",uniqsd[i,2],"\n")
    }
    else{
      varfit <- paste("ars",svar,"d",uniqsd[i,2],sep = "")
      assign(varfit, fit_ts(varnam))
      varpred <- paste("pred",svar,"d",i,sep = "")
      assign(varpred, forecast.Arima(get(varfit), h=pvar))
      pred <- get(varpred)
      #summary(pred)
      pred$x <- 10^pred$x
      pred$mean <- 10^pred$mean
      #pred$lower <- 10^pred$lower
      #pred$upper <- 10^pred$upper
      final.pred <- data.frame(Store = rep(svar,pvar), Dept = rep(uniqsd[i,2],pvar), Forecasted = as.numeric(pred$mean ))
      final.file <- rbind(as.data.frame(final.file), final.pred)
    }
}

# Save the output in a file
options(max.print = 5.5E5)
z <- gzfile("submission.csv.gz")
write.csv(final.file, z)




