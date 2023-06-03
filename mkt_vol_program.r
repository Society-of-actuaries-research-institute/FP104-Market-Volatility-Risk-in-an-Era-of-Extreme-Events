# This file includes some R codes that are used when conducting the SOA sponsored research
# project "Market Volatility Risk in an Era of Extreme Events"
# The code is for eductional purpose only. It is provided 'as is' without warranty of any kind, 
# either express or implied, warranties of fitness for a purpose, or the warranty of non-infringement. 
# Although the authors try their best to test the tool, they make no warranty that
# (1) it will meet your requirements;
# (2) it will be secure or error-free;
# (3) the results that may be obtained from the use of the code will be effective, accurate or reliable;
# (4) any errors in the tool will be corrected.

# The CIA, the project oversight group and the author assume no responsibility for errors or omissions 
# in the code or related documentation. In no event shall the sponsor and the authors be liable to you 
# or any third parties for any damages of any kind arising out of or in connection with the use of the code. 

# You may contact Kailan Shang at klshang81@gmail.com for any questions about the code.

# R is a open-source statistical software and can be downloaded at www.r-project.org
# The codes have been tested using R4.0.4

############################################################################
# Market Volatility Analysis of SPX
############################################################################

# Set working directory
setwd("C:/dsge/market_vol") #change this to your local directory where you stored the data file. All result files will be populated to this directory as well

# Load historical data from an EXCEL file

daily_data <- read.csv("daily_data.csv")

n_business_days_yearly <- 252
n_business_days_monthly <- round(252/12)
daily_data$Date <- as.Date(daily_data$Date)

# Figure 1
downside_deviation<-function(data,mar=0){
	n_record <- length(data)
	data_subtract_mar <- data - mar
	data_subtract_mar <- data_subtract_mar[data_subtract_mar<0]
	return(sqrt(sum(data_subtract_mar*data_subtract_mar,na.rm=TRUE)/n_record))
}

downside_deviation(daily_data$SPX_rtn,0)

par(mfrow=c(3,1))

plot(daily_data$Date,daily_data$SPX_rtn, type = "p", col="blue", pch=20, cex=0.25, main="SPX Daily Return", xlab="Date", ylab="SPX daily return")
recession_start <- 0
recession_end <- 0
for (idx in c(1:nrow(daily_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(daily_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- daily_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- daily_data$Date[idx]
		} else {
			next
		}
	}
}
rect(daily_data$Date[14975], -1, daily_data$Date[15291], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)

moving_filter <- function(x, w, FUN, cl) {
  w = as.integer(w)
  if (w < 1) {
    stop("The length of the window must be no less than 1")
  }
  output <- x
  for (i in 1:length(x)) {
     # plus 1 because the index is inclusive with the upper_bound 'i'
    lower_bound <- i - w + 1
    if (lower_bound < 1) {
      output[i] <- NA_real_
    } else {
	  if(missing(cl)){
		output[i] <- FUN(x[lower_bound:i])
	  } else {
		output[i] <- FUN(x[lower_bound:i], cl)	  
	  }
    }
  }
  output
}

# compute daily return volatility using 21-day window
w <- n_business_days_monthly
monthly_vol_spx <- moving_filter(daily_data$SPX_rtn,w,sd)*sqrt(n_business_days_yearly)
plot(daily_data$Date,monthly_vol_spx , type = "p", col="blue", pch=20, cex=0.25, main="SPX Daily Return Volatility (Monthly Window)", xlab="Date", ylab="SPX daily return annualized volatility")
recession_start <- 0
recession_end <- 0
for (idx in c(1:nrow(daily_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(daily_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- daily_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- daily_data$Date[idx]
		} else {
			next
		}
	}
}
rect(daily_data$Date[14975], -1, daily_data$Date[15291], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)

# compute daily return downside volatility using 21-day window
w <- n_business_days_monthly
monthly_vol_spx <- moving_filter(daily_data$SPX_rtn,w,downside_deviation)*sqrt(n_business_days_yearly)
plot(daily_data$Date,monthly_vol_spx , type = "p", col="blue", pch=20, cex=0.25, main="SPX Daily Return Downside Deviation (Monthly Window)", xlab="Date", ylab="SPX daily return annualized volatility")
recession_start <- 0
recession_end <- 0
for (idx in c(1:nrow(daily_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(daily_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- daily_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- daily_data$Date[idx]
		} else {
			next
		}
	}
}
rect(daily_data$Date[14975], -1, daily_data$Date[15291], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)


# descriptive statistics, Table 1
library(moments)

create_ds <- function(dataset, cl){
	NA_counts <- sapply(dataset, function(x) sum(is.na(x)))
	record_counts <- nrow(dataset) - NA_counts
	min <- sapply(dataset, min, na.rm=TRUE)
	max <- sapply(dataset, max, na.rm=TRUE)
	mean <- sapply(dataset, mean, na.rm=TRUE)
	std_dev <- sapply(dataset, sd, na.rm=TRUE)
	skewness <- sapply(dataset, skewness, na.rm=TRUE)
	kurtosis <- sapply(dataset, kurtosis, na.rm=TRUE)
	var_left <- sapply(dataset, function(x) quantile(x, cl, na.rm=TRUE))
	cte_left <- sapply(dataset, function(x) mean(x[x<=quantile(x, cl, na.rm=TRUE)], na.rm=TRUE))
	var_right <- sapply(dataset, function(x) quantile(x, 1-cl, na.rm=TRUE))
	cte_right <- sapply(dataset, function(x) mean(x[x>quantile(x, 1-cl, na.rm=TRUE)], na.rm=TRUE))

	data.frame(record_counts, min, max, mean, std_dev, skewness, kurtosis, var_left, cte_left, var_right, cte_right)
}

daily_new <- daily_data[as.Date(daily_data$Date) >= as.Date("2020-1-1"),]
# daily_new <- daily_data[23103:23355,]
daily_fc_2008 <- daily_data[20062:20458,]
daily_great_depression <- daily_data[397:1306,]
daily_black_monday <- daily_data[14975:15291,]

perc <- 0.01

daily_all_ds <- create_ds(daily_data[, !names(daily_data) %in% c("Date")], perc)
write.csv(daily_all_ds,"daily_all_ds.csv")
daily_new_ds <- create_ds(daily_new[, !names(daily_new) %in% c("Date")], perc)
write.csv(daily_new_ds,"daily_new_ds_v2.csv")
daily_fc_2008_ds <- create_ds(daily_fc_2008[, !names(daily_fc_2008) %in% c("Date")], perc)
write.csv(daily_fc_2008_ds,"daily_fc_2008_ds.csv")
daily_great_depression_ds <- create_ds(daily_great_depression[, !names(daily_great_depression) %in% c("Date")], perc)
write.csv(daily_great_depression_ds,"daily_great_depression_ds.csv")
daily_black_monday_ds <- create_ds(daily_black_monday[, !names(daily_black_monday) %in% c("Date")], perc)
write.csv(daily_black_monday_ds,"daily_black_monday_ds.csv")


# Implied Volatility, Figure 2
iv_data <- daily_data[as.Date(daily_data$Date)>as.Date("1990-1-1"),]
plot(iv_data$Date,unlist(iv_data[, "VIXCLS"]), type = "p", col="blue", pch=20, cex=0.25, main="SPX Implied Volatility", xlab="Date", ylab="VIXCLS")

for (idx in c(1:nrow(iv_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(iv_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- iv_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- iv_data$Date[idx]
		} else {
			next
		}
	}
}

# VVIX, Figure 3
iv_data <- daily_data[as.Date(daily_data$Date)>as.Date("2006-3-1"),]
plot(iv_data$Date,unlist(iv_data[, "VVIX"]), type = "p", col="blue", pch=20, cex=0.25, main="SPX Volatility of Volatility", xlab="Date", ylab="VVIX")

for (idx in c(1:nrow(iv_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(iv_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- iv_data$Date[idx]
			rect(recession_start, -1, recession_end, 3, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- iv_data$Date[idx]
		} else {
			next
		}
	}
}

# Realized Vol of (Implied) Volatility, Figure 4
par(mfrow=c(2,1))

# compute vol of volatility using 21-day window
w <- n_business_days_monthly
monthly_vol_spx <- moving_filter(daily_data$SPX_rtn,w,sd)*(n_business_days_monthly**0.5)#((n_business_days_yearly/n_business_days_monthly)**0.5)
monthly_vol_of_vol_spx <- moving_filter(monthly_vol_spx,w,sd)*(12**0.5)#((n_business_days_yearly/n_business_days_monthly)**0.5)
plot(daily_data$Date,monthly_vol_of_vol_spx, type = "p", col="blue", pch=20, cex=0.25, main="SPX Realized Volatility of Volatility", xlab="Date", ylab="Annualized Vol of Monthly SPX Vol")

for (idx in c(1:nrow(daily_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(daily_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- daily_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- daily_data$Date[idx]
		} else {
			next
		}
	}
}
rect(daily_data$Date[14975], -1, daily_data$Date[15291], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)

# compute vol of implied volatility using 21-day window
w <- n_business_days_monthly
monthly_vol_of_vol_spx <- moving_filter(daily_data$VIXCLS,w,sd)#*((n_business_days_yearly/n_business_days_monthly)**0.5) #annualized
plot(daily_data$Date,monthly_vol_of_vol_spx, type = "p", col="blue", pch=20, cex=0.25, main="SPX Realized Volatility of Implied Volatility", xlab="Date", ylab="Vol of VIXCLS")

for (idx in c(1:nrow(daily_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(daily_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- daily_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- daily_data$Date[idx]
		} else {
			next
		}
	}
}
rect(daily_data$Date[14975], -1, daily_data$Date[15291], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)


#NASDAQ Analysis

nasdaq_data <- daily_data[as.Date(daily_data$Date)>as.Date("1971-2-1"),]
# Figure A.1

par(mfrow=c(2,1))

plot(nasdaq_data$Date,nasdaq_data$NASDAQCOM_rtn, type = "p", col="blue", pch=20, cex=0.25, main="NASDAQ Daily Return", xlab="Date", ylab="NASDAQ daily return")
recession_start <- 0
recession_end <- 0
for (idx in c(1:nrow(nasdaq_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(nasdaq_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- nasdaq_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- nasdaq_data$Date[idx]
		} else {
			next
		}
	}
}
rect(nasdaq_data$Date[14975-10764], -1, nasdaq_data$Date[15291-10764], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)

# compute daily return volatility using 21-day window
w <- n_business_days_monthly
monthly_vol_spx <- moving_filter(nasdaq_data$NASDAQCOM_rtn,w,sd)*sqrt(n_business_days_yearly)
plot(nasdaq_data$Date,monthly_vol_spx , type = "p", col="blue", pch=20, cex=0.25, main="NASDAQ Daily Return Volatility (Monthly Window)", xlab="Date", ylab="NASDAQ daily return annualized volatility")
recession_start <- 0
recession_end <- 0
for (idx in c(1:nrow(nasdaq_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(nasdaq_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- nasdaq_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- nasdaq_data$Date[idx]
		} else {
			next
		}
	}
}
rect(nasdaq_data$Date[14975-10764], -1, nasdaq_data$Date[15291-10764], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)

# Implied Volatility, Figure A.2
iv_data <- daily_data[as.Date(daily_data$Date)>as.Date("2001-2-1"),]
plot(iv_data$Date,unlist(iv_data[, "VXNCLS"]), type = "p", col="blue", pch=20, cex=0.25, main="NASDAQ Implied Volatility", xlab="Date", ylab="VXNCLS")

for (idx in c(1:nrow(iv_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(iv_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- iv_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- iv_data$Date[idx]
		} else {
			next
		}
	}
}

# Realized Vol of (Implied) Volatility, Figure A.3
par(mfrow=c(2,1))

# compute vol of volatility using 21-day window
w <- n_business_days_monthly
monthly_vol_spx <- moving_filter(nasdaq_data$NASDAQCOM_rtn,w,sd)*(n_business_days_monthly**0.5)#((n_business_days_yearly/n_business_days_monthly)**0.5)
monthly_vol_of_vol_spx <- moving_filter(monthly_vol_spx,w,sd)*(12**0.5)#((n_business_days_yearly/n_business_days_monthly)**0.5)
plot(nasdaq_data$Date,monthly_vol_of_vol_spx, type = "p", col="blue", pch=20, cex=0.25, main="NASDAQ Realized Volatility of Volatility", xlab="Date", ylab="Annualized Vol of Monthly NASDAQ Vol")

for (idx in c(1:nrow(nasdaq_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(nasdaq_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- nasdaq_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- nasdaq_data$Date[idx]
		} else {
			next
		}
	}
}
rect(nasdaq_data$Date[14975-10764], -1, nasdaq_data$Date[15291-10764], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)

# compute vol of implied volatility using 21-day window
w <- n_business_days_monthly
monthly_vol_of_vol_spx <- moving_filter(nasdaq_data$VXNCLS,w,sd)#*((n_business_days_yearly/n_business_days_monthly)**0.5) #annualized
plot(nasdaq_data$Date,monthly_vol_of_vol_spx, type = "p", col="blue", pch=20, cex=0.25, main="NASDAQCOM Realized Volatility of Implied Volatility", xlab="Date", ylab="Vol of VXNCLS")

for (idx in c(1:nrow(nasdaq_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(nasdaq_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- nasdaq_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- nasdaq_data$Date[idx]
		} else {
			next
		}
	}
}
rect(nasdaq_data$Date[14975-10764], -1, nasdaq_data$Date[15291-10764], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)




#RUSSELL2000 Analysis

# Figure A.4
russell_data <- daily_data[as.Date(daily_data$Date)>as.Date("1987-9-10"),]

par(mfrow=c(2,1))

plot(russell_data$Date,russell_data$RUT_rtn, type = "p", col="blue", pch=20, cex=0.25, main="RUSSELL Daily Return", xlab="Date", ylab="RUSSELL daily return")
recession_start <- 0
recession_end <- 0
for (idx in c(1:nrow(russell_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(russell_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- russell_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- russell_data$Date[idx]
		} else {
			next
		}
	}
}
rect(russell_data$Date[14975-14961], -1, russell_data$Date[15291-14961], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)

# compute daily return volatility using 21-day window
w <- n_business_days_monthly
monthly_vol_spx <- moving_filter(russell_data$RUT_rtn,w,sd)*sqrt(n_business_days_yearly)
plot(russell_data$Date,monthly_vol_spx , type = "p", col="blue", pch=20, cex=0.25, main="RUSSELL Daily Return Annualized Volatility (Monthly Window)", xlab="Date", ylab="RUSSELL daily return volatility")
recession_start <- 0
recession_end <- 0
for (idx in c(1:nrow(russell_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(russell_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- russell_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- russell_data$Date[idx]
		} else {
			next
		}
	}
}
rect(russell_data$Date[14975-14961], -1, russell_data$Date[15291-14961], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)

# Implied Volatility, Figure A.5
iv_data <- daily_data[as.Date(daily_data$Date)>as.Date("2004-1-1"),]
plot(iv_data$Date,unlist(iv_data[, "RVXCLS"]), type = "p", col="blue", pch=20, cex=0.25, main="RUSSELL 2000 Implied Volatility", xlab="Date", ylab="RVXCLS")

for (idx in c(1:nrow(iv_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(iv_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- iv_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- iv_data$Date[idx]
		} else {
			next
		}
	}
}

# Realized Vol of (Implied) Volatility, Figure A.6
par(mfrow=c(2,1))

# compute vol of volatility using 21-day window
w <- n_business_days_monthly
monthly_vol_spx <- moving_filter(russell_data$RUT_rtn,w,sd)*(n_business_days_monthly**0.5)#((n_business_days_yearly/n_business_days_monthly)**0.5)
monthly_vol_of_vol_spx <- moving_filter(monthly_vol_spx,w,sd)*(12**0.5)#((n_business_days_yearly/n_business_days_monthly)**0.5)
plot(russell_data$Date,monthly_vol_of_vol_spx, type = "p", col="blue", pch=20, cex=0.25, main="RUSSELL Realized Volatility of Volatility", xlab="Date", ylab="Annualized Vol of Monthly RUSSELL Vol")

for (idx in c(1:nrow(russell_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(russell_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- russell_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- russell_data$Date[idx]
		} else {
			next
		}
	}
}
rect(russell_data$Date[14975-14961], -1, russell_data$Date[15291-14961], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)

# compute vol of implied volatility using 21-day window
w <- n_business_days_monthly
monthly_vol_of_vol_spx <- moving_filter(russell_data$RVXCLS,w,sd)#*((n_business_days_yearly/n_business_days_monthly)**0.5) #annualized
plot(russell_data$Date,monthly_vol_of_vol_spx, type = "p", col="blue", pch=20, cex=0.25, main="RUSSELL Realized Volatility of Implied Volatility", xlab="Date", ylab="Vol of RVXCLS")

for (idx in c(1:nrow(russell_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(russell_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- russell_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- russell_data$Date[idx]
		} else {
			next
		}
	}
}
rect(russell_data$Date[14975-14961], -1, russell_data$Date[15291-14961], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)


#Volatility clustering

#ACF
lag_max = 20

par(mfrow=c(3,2))

acf_all <- acf(daily_data$SPX_rtn[2:nrow(daily_data)], lag.max = lag_max, plot=FALSE)
acf_new <- acf(daily_new$SPX_rtn, lag.max = lag_max, plot=FALSE)
acf_fc_2008 <- acf(daily_fc_2008$SPX_rtn, lag.max = lag_max, plot=FALSE)
acf_black_monday <- acf(daily_black_monday$SPX_rtn, lag.max = lag_max, plot=FALSE)
acf_great_depression <- acf(daily_great_depression$SPX_rtn, lag.max = lag_max, plot=FALSE)

acf_all <- acf_all[2:(lag_max+1)]
acf_new <- acf_new[2:(lag_max+1)]
acf_fc_2008 <- acf_fc_2008[2:(lag_max+1)]
acf_black_monday <- acf_black_monday[2:(lag_max+1)]
acf_great_depression <- acf_great_depression[2:(lag_max+1)]


plot(acf_all, main="ACF: S&P 500 Daily Return (Jan 1928 - Jun 2022)", ylim=c(-0.4,0.4))
plot(acf_new, main="ACF: S&P 500 Daily Return (Post 2020)", ylim=c(-0.4,0.4))
plot(acf_fc_2008, main="ACF: S&P 500 Daily Return (Dec 2007 - Jun 2009)", ylim=c(-0.4,0.4))
plot(acf_black_monday, main="ACF: S&P 500 Daily Return (Oct 1987 - Dec 1988)", ylim=c(-0.4,0.4))
plot(acf_great_depression, main="ACF: S&P 500 Daily Return (Aug 1929 - Mar 1933)", ylim=c(-0.4,0.4))

#ARMA + GARCH
library(fGarch)

p <- 1
q <- 1
g_p <-1
g_q <-1

formu <- as.formula(paste0("~ arma(",p,",",q,") + garch(",g_p,",",g_q,")"))

# Fitting the data to ARMA(1,1) and GARCH(1,1) models
gf_all <- garchFit(formu, data = daily_data$SPX_rtn[2:nrow(daily_data)], cond.dist = "sged", algorithm = "lbfgsb", trace = FALSE)
gf_all
gf_new <- garchFit(formu, data = daily_new$SPX_rtn, cond.dist = "sged", algorithm = "lbfgsb", trace = FALSE)
gf_new
gf_fc_2008 <- garchFit(formu, data = daily_fc_2008$SPX_rtn, cond.dist = "sged", algorithm = "lbfgsb", trace = FALSE)
gf_fc_2008
gf_great_depression <- garchFit(formu, data = daily_great_depression$SPX_rtn, cond.dist = "sged", algorithm = "lbfgsb", trace = FALSE)
gf_great_depression
gf_black_monday <- garchFit(formu, data = daily_black_monday$SPX_rtn, cond.dist = "sged", algorithm = "lbfgsb", trace = FALSE)
gf_black_monday


# Draw return and conditional volatility
par(mfrow=c(2,1))

plot(daily_data$Date[2:length(daily_data$Date)],daily_data$SPX_rtn[2:length(daily_data$Date)], type = "p", col="blue", pch=20, cex=0.25, main="SPX Daily Return", xlab="Date", ylab="SPX daily return")
recession_start <- 0
recession_end <- 0
for (idx in c(1:(length(daily_data$Date)-1))){
	# if (idx %% 10 == 0){print(idx)}
	if(daily_data$Contraction[idx+1]==0){
		if (recession_start != 0){
			recession_end <- daily_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- daily_data$Date[idx+1]
		} else {
			next
		}
	}
}
rect(daily_data$Date[14975], -1, daily_data$Date[15291], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)

plot(daily_data$Date[2:length(daily_data$Date)],gf_all@sigma.t*sqrt(n_business_days_yearly), type = "p", col="blue", pch=20, cex=0.25, main="SPX Conditional Volatility", xlab="Date", ylab="SPX annualized conditional volatility")
for (idx in c(1:(length(gf_all@sigma.t)-1))){
	# if (idx %% 10 == 0){print(idx)}
	if(daily_data$Contraction[idx+1]==0){
		if (recession_start != 0){
			recession_end <- daily_data$Date[idx]
			rect(recession_start, 0, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- daily_data$Date[idx+1]
		} else {
			next
		}
	}
}
rect(daily_data$Date[14975], -1, daily_data$Date[15291], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)


# compare conditional vol using different study periods
plot(daily_data$Date[2:length(daily_data$Date)],gf_all@sigma.t*sqrt(n_business_days_yearly), type = "p", col="blue", pch=20, cex=0.25, main="SPX conditional volatility (All Periods)", xlab="Date", ylab="SPX conditional volatility")
for (idx in c(1:(length(gf_all@sigma.t)-1))){
	# if (idx %% 10 == 0){print(idx)}
	if(daily_data$Contraction[idx+1]==0){
		if (recession_start != 0){
			recession_end <- daily_data$Date[idx]
			rect(recession_start, 0, recession_end, 2, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- daily_data$Date[idx+1]
		} else {
			next
		}
	}
}
rect(daily_data$Date[14975], -1, daily_data$Date[15291], 2, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)

sigma_t <- rep(NA, length(gf_all@sigma.t))

sigma_t[20061:20457] <- gf_fc_2008@sigma.t*sqrt(n_business_days_yearly)
sigma_t[396:1305] <- gf_great_depression@sigma.t*sqrt(n_business_days_yearly)
sigma_t[14974:15290] <- gf_black_monday@sigma.t*sqrt(n_business_days_yearly)
sigma_t[(length(sigma_t)-length(gf_new@sigma.t)+1):length(sigma_t)] <- gf_new@sigma.t*sqrt(n_business_days_yearly)

plot(daily_data$Date[2:length(daily_data$Date)],sigma_t, type = "p", col="blue", pch=20, cex=0.25, main="SPX conditional volatility (Extreme Periods)", xlab="Date", ylab="SPX conditional volatility")
for (idx in c(1:(length(gf_all@sigma.t)-1))){
	# if (idx %% 10 == 0){print(idx)}
	if(daily_data$Contraction[idx+1]==0){
		if (recession_start != 0){
			recession_end <- daily_data$Date[idx]
			rect(recession_start, 0, recession_end, 2, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- daily_data$Date[idx+1]
		} else {
			next
		}
	}
}
rect(daily_data$Date[14975], -1, daily_data$Date[15291], 2, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)


# QQplot comparing Gaussian and SGED
res <- gf_new@residuals/gf_new@sigma.t
# number of data
n <- length(res)
n<-500000
set.seed(123)
avg <- mean(res)
std <- sd(res)
# simulated Gaussian distribution
NormalDistribution<-rnorm(n,0,1)
par(mfrow=c(1,2))
# Q-Q plot
qqplot(res,NormalDistribution,main="Q-Q plot (Empirical v.s. Normal Distribution)",xlab = "Historical Experience Quantiles", ylab = "Normal Distribution Quantiles",xlim=range(-7.5,7.5),ylim=range(-7.5,7.5))
abline(0,1)

# simulated SGED
SGEDistribution<-rsged(n,0,1,1.592,0.664)
# Q-Q plot
qqplot(res,SGEDistribution,main="Q-Q plot (Empirical v.s. SGED)",xlab = "Historical Experience Quantiles", ylab = "Skewed Generalized Error Distribution Quantiles",xlim=range(-7.5,7.5),ylim=range(-7.5,7.5))
abline(0,1)


# VaR estimation using simulation from March 2020 for 1 year (252 trading days)
start_idx <- 40 #40, 62, 73, 93

actual_rtn <- daily_new$SPX_rtn[start_idx:(start_idx+251)]

percentile_5th <- rep(0,252)
percentile_95th <- rep(0,252)
percentile_0_5th <- rep(0,252)
percentile_99_5th <- rep(0,252)

initial_rtn <- daily_new$SPX_rtn[start_idx-1]
initial_cond_vol <- gf_new@sigma.t[start_idx-1]
initial_residual <- gf_new@residuals[start_idx-1]
c <- 2.363e-04
phi1 <- 7.049e-01
theta1 <- -8.286e-01
omega <- 4.089e-06
alpha1 <- 1.860e-01
beta1 <-  8.014e-01
lambda  <-  6.652e-01
p <-  1.519e+00

n_sim <- 100000

simulated_rtn <- matrix(0,n_sim, 252)
rtn_period <- rep(0,n_sim)
rtn_cum <- rep(1,n_sim)
vol_period <- rep(0,n_sim)
res_period <- rep(0,n_sim)

set.seed(123)
for (idx in c(1:252)){
	if (idx == 1){
		res_period <- rsged(n_sim,0,1,p,lambda)
		vol_period <- (omega+alpha1*initial_residual^2+beta1*initial_cond_vol^2)^0.5
		rtn_period <- c+phi1*initial_rtn+theta1*initial_residual+res_period*vol_period
		rtn_cum <- rtn_cum*(1+rtn_period)
		percentile_5th[idx] <- quantile(rtn_period, 0.05)
		percentile_95th[idx] <- quantile(rtn_period, 0.95)
		percentile_0_5th[idx] <- quantile(rtn_period, 0.005)
		percentile_99_5th[idx] <- quantile(rtn_period, 0.995)
	}else{
		rtn_period <- c+phi1*rtn_period+theta1*res_period*vol_period
		vol_period <- (omega+alpha1*(res_period*vol_period)^2+beta1*vol_period^2)^0.5
		res_period <- rsged(n_sim,0,1,p,lambda)
		rtn_period <- rtn_period + res_period*vol_period
		rtn_cum <- rtn_cum*(1+rtn_period)
		percentile_5th[idx] <- quantile(rtn_period, 0.05)
		percentile_95th[idx] <- quantile(rtn_period, 0.95)
		percentile_0_5th[idx] <- quantile(rtn_period, 0.005)
		percentile_99_5th[idx] <- quantile(rtn_period, 0.995)
	}
}

rtn_cum = rtn_cum - 1
annual_VaR_conditional <- quantile(rtn_cum, c(0.005, 0.05, 0.95, 0.995))
annual_VaR_unconditional <- qnorm(c(0.005, 0.05, 0.95, 0.995),0.0006092149*252,0.01602575*252^0.5)

plot(daily_new$Date[start_idx:(start_idx+252-1)],actual_rtn, type = "p", col="red", pch=20, cex=0.5, main="Conditional VaR Estimation (Mar 2020 - Feb 2021)", xlab="Date", ylab="SPX return", ylim=c(-0.15,0.15))
lines(daily_new$Date[start_idx:(start_idx+252-1)],percentile_5th, type="l", lty=2, col="green", cex=0.25)
lines(daily_new$Date[start_idx:(start_idx+252-1)],percentile_95th, type="l", lty=2, col="green", cex=0.25)
lines(daily_new$Date[start_idx:(start_idx+252-1)],percentile_0_5th, type="l", lty=3, col="black", cex=0.25)
lines(daily_new$Date[start_idx:(start_idx+252-1)],percentile_99_5th, type="l", lty=3, col="black", cex=0.25)
lines(daily_new$Date[start_idx:(start_idx+252-1)],rep(0.01602575*qnorm(0.005,0,1),252), type="l", lty=4, col="brown", cex=0.25)
lines(daily_new$Date[start_idx:(start_idx+252-1)],rep(0.01602575*qnorm(0.995,0,1),252), type="l", lty=4, col="brown", cex=0.25)
legend(daily_new$Date[start_idx+180], 0.15, legend=c("actual return", "5/95th percentile conditional", "0.5/99.5th percentile conditional", "0.5/99.5th percentile Unconditional"), col=c("red", "green", "black","brown"), pch = c(20,26,26,26), lty=c(0,2,3,4), cex=0.8)



# Jump Diffusion Calibration
install.packages("DiffusionRjgqd")
library(DiffusionRjgqd)


daily_new <- daily_data[as.Date(daily_data$Date) >= as.Date("2020-1-1"),]
daily_fc_2008 <- daily_data[20062:20458,]
daily_great_depression <- daily_data[397:1306,]
daily_black_monday <- daily_data[14975:15291,]

#post-2020 period
daily_jump_data <- daily_new

for (i in c(1:10)){
	JGQD.remove()
	G1 <- function(t){theta[1]}
	Q1 <- function(t){theta[2]*theta[2]}
	Jmu  <- function(t){theta[4]}
	Jsig <- function(t){theta[5]}
	Lam0 <- function(t){theta[3]}
	priors <- function(theta)
	{
	 dgamma(theta[3],0.001,0.001)*dgamma(1/theta[5]^2,0.001,0.001)
	}

	# Define some starting parameters and run the MCMC:
	updates <- 200000
	burns   <- 10000
	theta   <- c(0.1,0.2,10,0,0.1)
	sds     <- c(0.1,0.05,0.2,0.01,0.01)/4
	model_1 <- JGQD.mcmc(daily_jump_data$SPX/5000.0,seq(0,length(daily_jump_data$SPX)-1)/250,mesh=20,theta=theta,sds=sds,Jdist="Normal",Jtype="Mult",
                    updates=updates,burns=burns,print.output=TRUE)
	print(i)
	gc()
	if (is.na(model_1$model.info$DIC)==FALSE){break}
}

model_estimate <- JGQD.estimates(model_1,thin=100,burns, corrmat=TRUE, acf.plot=FALSE)

i <- "new"
for (name in names(model_1)){
	if (name != "dec.matrix"){
		write.csv(model_1[name], paste0("./jdm/",i, "_", name, ".csv"))
	}
}

for (name in names(model_estimate)){
	write.csv(model_estimate[name], paste0("./jdm/",i, "_estimate_", name, ".csv"))
}


#2008 financial crisis
daily_jump_data <- daily_fc_2008

for (i in c(1:10)){
	JGQD.remove()
	G1 <- function(t){theta[1]}
	Q1 <- function(t){theta[2]*theta[2]}
	Jmu  <- function(t){theta[4]}
	Jsig <- function(t){theta[5]}
	Lam0 <- function(t){theta[3]}
	priors <- function(theta)
	{
	 dgamma(theta[3],0.001,0.001)*dgamma(1/theta[5]^2,0.001,0.001)
	}

	# Define some starting parameters and run the MCMC:
	updates <- 200000
	burns   <- 10000
	theta   <- c(-0.1,0.2,10,0,0.1)
	sds     <- c(0.1,0.05,0.2,0.01,0.01)/4
	model_1 <- JGQD.mcmc(daily_jump_data$SPX/2000.0,seq(0,length(daily_jump_data$SPX)-1)/250,mesh=20,theta=theta,sds=sds,Jdist="Normal",Jtype="Mult",
                    updates=updates,burns=burns,print.output=TRUE)
	print(i)
	gc()
	if (is.na(model_1$model.info$DIC)==FALSE){break}
}

model_estimate <- JGQD.estimates(model_1,thin=100,burns, corrmat=TRUE, acf.plot=FALSE)

i <- "fc"
for (name in names(model_1)){
	if (name != "dec.matrix"){
		write.csv(model_1[name], paste0("./jdm/",i, "_", name, ".csv"))
	}
}

for (name in names(model_estimate)){
	write.csv(model_estimate[name], paste0("./jdm/",i, "_estimate_", name, ".csv"))
}

#1987 Black Monday
daily_jump_data <- daily_black_monday

for (i in c(1:10)){
	JGQD.remove()
	G1 <- function(t){theta[1]}
	Q1 <- function(t){theta[2]*theta[2]}
	Jmu  <- function(t){theta[4]}
	Jsig <- function(t){theta[5]}
	Lam0 <- function(t){theta[3]}
	priors <- function(theta)
	{
	 dgamma(theta[3],0.001,0.001)*dgamma(1/theta[5]^2,0.001,0.001)
	}

	# Define some starting parameters and run the MCMC:
	updates <- 200000
	burns   <- 10000
	theta   <- c(0.05,0.2,10,0,0.1)
	sds     <- c(0.1,0.05,0.2,0.01,0.01)/4
	model_1 <- JGQD.mcmc(daily_jump_data$SPX/500.0,seq(0,length(daily_jump_data$SPX)-1)/250,mesh=20,theta=theta,sds=sds,Jdist="Normal",Jtype="Mult",
                    updates=updates,burns=burns,print.output=TRUE)
	print(i)
	gc()
	if (is.na(model_1$model.info$DIC)==FALSE){break}
}

model_estimate <- JGQD.estimates(model_1,thin=100,burns, corrmat=TRUE, acf.plot=FALSE)

i <- "bm"
for (name in names(model_1)){
	if (name != "dec.matrix"){
		write.csv(model_1[name], paste0("./jdm/",i, "_", name, ".csv"))
	}
}

for (name in names(model_estimate)){
	write.csv(model_estimate[name], paste0("./jdm/",i, "_estimate_", name, ".csv"))
}

#1929-1933 Great Depression
daily_jump_data <- daily_great_depression

for (i in c(1:10)){
	JGQD.remove()
	G1 <- function(t){theta[1]}
	Q1 <- function(t){theta[2]*theta[2]}
	Jmu  <- function(t){theta[4]}
	Jsig <- function(t){theta[5]}
	Lam0 <- function(t){theta[3]}
	priors <- function(theta)
	{
	 dgamma(theta[3],0.001,0.001)*dgamma(1/theta[5]^2,0.001,0.001)
	}

	# Define some starting parameters and run the MCMC:
	updates <- 200000
	burns   <- 10000
	theta   <- c(-0.35,0.4,10,0,0.1)
	sds     <- c(0.1,0.05,0.2,0.01,0.01)/4
	model_1 <- JGQD.mcmc(daily_jump_data$SPX/50.0,seq(0,length(daily_jump_data$SPX)-1)/250,mesh=20,theta=theta,sds=sds,Jdist="Normal",Jtype="Mult",
                    updates=updates,burns=burns,print.output=TRUE)
	print(i)
	gc()
	if (is.na(model_1$model.info$DIC)==FALSE){break}
}

model_estimate <- JGQD.estimates(model_1,thin=100,burns, corrmat=TRUE, acf.plot=FALSE)

i <- "gd"
for (name in names(model_1)){
	if (name != "dec.matrix"){
		write.csv(model_1[name], paste0("./jdm/",i, "_", name, ".csv"))
	}
}

for (name in names(model_estimate)){
	write.csv(model_estimate[name], paste0("./jdm/",i, "_estimate_", name, ".csv"))
}



#plot jump probability

jump_probs <- rep(NA,nrow(daily_data))
jump_probs[398:1306] <- read.csv("./jdm/gd_decode.prob.csv")$decode.prob
jump_probs[14976:15291] <- read.csv("./jdm/bm_decode.prob.csv")$decode.prob
jump_probs[20063:20458] <- read.csv("./jdm/fc_decode.prob.csv")$decode.prob
jump_probs[23104:length(jump_probs)] <- read.csv("./jdm/new_decode.prob.csv")$decode.prob

par(mfrow=c(2,1))

plot(daily_data$Date,daily_data$SPX_rtn, type = "p", col="blue", pch=20, cex=0.25, main="SPX Daily Return", xlab="Date", ylab="SPX daily return")
recession_start <- 0
recession_end <- 0
for (idx in c(1:nrow(daily_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(daily_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- daily_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- daily_data$Date[idx]
		} else {
			next
		}
	}
}
rect(daily_data$Date[14975], -1, daily_data$Date[15291], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)

plot(daily_data$Date,jump_probs, type = "p", col="blue", pch=20, cex=0.25, main="SPX Jump Probability", xlab="Date", ylab="Jump Probability")
recession_start <- 0
recession_end <- 0
for (idx in c(1:nrow(daily_data))){
	# if (idx %% 10 == 0){print(idx)}
	if(daily_data$Contraction[idx]==0){
		if (recession_start != 0){
			recession_end <- daily_data$Date[idx]
			rect(recession_start, -1, recession_end, 1, density = 100, col = rgb(red=0, green=0, blue=1, alpha=0.1), border=NA)
			recession_start <- 0
			recession_end <- 0
		} else {
			next
		}
	} else {
		if (recession_start == 0) {
			recession_start <- daily_data$Date[idx]
		} else {
			next
		}
	}
}
rect(daily_data$Date[14975], -1, daily_data$Date[15291], 1, density = 100, col = rgb(red=0, green=1, blue=0, alpha=0.1), border=NA)

#calculate the percentage of jump probability greater than the threshold
threshold <- 0.5
prob_gd <- sum(read.csv("./jdm/gd_decode.prob.csv")$decode.prob>=threshold)/length(read.csv("./jdm/gd_decode.prob.csv")$decode.prob)
prob_bm <- sum(read.csv("./jdm/bm_decode.prob.csv")$decode.prob>=threshold)/length(read.csv("./jdm/bm_decode.prob.csv")$decode.prob)
prob_fc <- sum(read.csv("./jdm/fc_decode.prob.csv")$decode.prob>=threshold)/length(read.csv("./jdm/fc_decode.prob.csv")$decode.prob)
prob_new <- sum(read.csv("./jdm/new_decode.prob.csv")$decode.prob>=threshold)/length(read.csv("./jdm/new_decode.prob.csv")$decode.prob)


# Correlation Matrix

selected_vars <- c("SPX_rtn", "VIXCLS",	"T10YIE", "DFF", "DGS1", "DGS10", "BAA10Y")

daily_new <- daily_data[as.Date(daily_data$Date) >= as.Date("2020-1-1"),]
daily_fc_2008 <- daily_data[20062:20458,]
daily_great_depression <- daily_data[397:1306,]
daily_black_monday <- daily_data[14975:15291,]

# Correlation Matrices

# Daily Data
cm_all <- cor(daily_data[,(names(daily_data) %in% selected_vars)], use="pairwise.complete.obs")
round(cm_all, 2)

cm_extreme <- cor(daily_data[daily_data$Contraction == 1,(names(daily_data) %in% selected_vars)], use="pairwise.complete.obs")
round(cm_extreme, 2)

cm_new <- cor(daily_new[,(names(daily_new) %in% selected_vars)], use="pairwise.complete.obs")
round(cm_new, 2)

cm_fc_2008 <- cor(daily_fc_2008[,(names(daily_fc_2008) %in% selected_vars)], use="pairwise.complete.obs")
round(cm_fc_2008, 2)

cm_black_monday <- cor(daily_black_monday[,(names(daily_black_monday) %in% selected_vars)], use="pairwise.complete.obs")
round(cm_black_monday, 2)

# install.packages("reshape2")
library(reshape2)
library(ggplot2)
 
melted_corr_mat <- melt(round(cm_all,2))
# head(melted_corr_mat)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) + scale_y_discrete(limits = rev) +
geom_tile() + ggtitle("Correlation Matrix (Jul 1954 - Apr 2022)") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x = element_text(angle = 90)) +  
  scale_fill_distiller(palette = "RdBu",limits=c(-1,1)) + geom_text(aes(Var2, Var1, label = value), color = "green", size = 6)

melted_corr_mat <- melt(round(cm_extreme,2))
# head(melted_corr_mat)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) + scale_y_discrete(limits = rev) +
geom_tile() + ggtitle("Correlation Matrix (Recession Periods)") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x = element_text(angle = 90)) +  
  scale_fill_distiller(palette = "BrBG",limits=c(-1,1)) + geom_text(aes(Var2, Var1, label = value), color = "green", size = 6)

melted_corr_mat <- melt(round(cm_new,2))
# head(melted_corr_mat)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) + scale_y_discrete(limits = rev) +
geom_tile() + ggtitle("Correlation Matrix (Jan 2020 - Jun 2022)") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x = element_text(angle = 90)) +  
  scale_fill_distiller(palette = "BrBG",limits=c(-1,1)) + geom_text(aes(Var2, Var1, label = value), color = "green", size = 6)

melted_corr_mat <- melt(round(cm_fc_2008,2))
# head(melted_corr_mat)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) + scale_y_discrete(limits = rev) +
geom_tile() + ggtitle("Correlation Matrix (Dec 2007 – Jun 2009)") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x = element_text(angle = 90)) +  
  scale_fill_distiller(palette = "BrBG",limits=c(-1,1)) + geom_text(aes(Var2, Var1, label = value), color = "green", size = 6)



# Monthly data
monthly_data <- read.csv("monthly_data.csv")
monthly_new <- monthly_data[monthly_data$Year>=2020,]
monthly_extreme <- monthly_data[monthly_data$Contraction==1,]
monthly_fc_2008 <- monthly_data[960:978,]
monthly_great_depression <- monthly_data[20:63,]
monthly_black_monday <- monthly_data[718:732,]

cm_all <- cor(monthly_data[,(names(monthly_data) %in% selected_vars)], use="pairwise.complete.obs")
round(cm_all, 2)

cm_extreme <- cor(monthly_data[monthly_data$Contraction == 1,(names(monthly_data) %in% selected_vars)], use="pairwise.complete.obs")
round(cm_extreme, 2)

cm_new <- cor(monthly_new[,(names(monthly_new) %in% selected_vars)], use="pairwise.complete.obs")
round(cm_new, 2)

cm_fc_2008 <- cor(monthly_fc_2008[,(names(monthly_fc_2008) %in% selected_vars)], use="pairwise.complete.obs")
round(cm_fc_2008, 2)

cm_black_monday <- cor(monthly_black_monday[,(names(monthly_black_monday) %in% selected_vars)], use="pairwise.complete.obs")
round(cm_black_monday, 2)

# install.packages("reshape2")
library(reshape2)
library(ggplot2)
 
melted_corr_mat <- melt(round(cm_all,2))
# head(melted_corr_mat)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) + scale_y_discrete(limits = rev) +
geom_tile() + ggtitle("Correlation Matrix (Jul 1954 - Jun 2022)") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x = element_text(angle = 90)) +  
  scale_fill_distiller(palette = "BrBG",limits=c(-1,1)) + geom_text(aes(Var2, Var1, label = value), color = "green", size = 6)

melted_corr_mat <- melt(round(cm_extreme,2))
# head(melted_corr_mat)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) + scale_y_discrete(limits = rev) +
geom_tile() + ggtitle("Correlation Matrix (Recession Periods)") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x = element_text(angle = 90)) +  
  scale_fill_distiller(palette = "BrBG",limits=c(-1,1)) + geom_text(aes(Var2, Var1, label = value), color = "green", size = 6)

melted_corr_mat <- melt(round(cm_new,2))
# head(melted_corr_mat)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) + scale_y_discrete(limits = rev) +
geom_tile() + ggtitle("Correlation Matrix (Jan 2020 - Jun 2022)") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x = element_text(angle = 90)) +  
  scale_fill_distiller(palette = "BrBG",limits=c(-1,1)) + geom_text(aes(Var2, Var1, label = value), color = "green", size = 6)

melted_corr_mat <- melt(round(cm_fc_2008,2))
# head(melted_corr_mat)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) + scale_y_discrete(limits = rev) +
geom_tile() + ggtitle("Correlation Matrix (Dec 2007 – Jun 2009)") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.x = element_text(angle = 90)) +  
  scale_fill_distiller(palette = "BrBG",limits=c(-1,1)) + geom_text(aes(Var2, Var1, label = value), color = "green", size = 6)


# Copula Simulation
# install.packages("copula")
library(copula)

n<-2000
gaussian_cop <- normalCopula(0.85)
sim_cop<- rCopula(n, gaussian_cop)
plot(sim_cop, main="Copule de Gauss", xlab="x", ylab="y", cex = 1)

t_cop <- tCopula(0.85, df=5)
sim_cop<- rCopula(n, t_cop)
plot(sim_cop, main="Copule t", xlab="x", ylab="y", cex = 1)

gumbel_cop <- gumbelCopula(3)
sim_cop<- rCopula(n, gumbel_cop)
plot(sim_cop, main="Copule de Gumbel", xlab="x", ylab="y", cex = 1)

clayton_cop <- claytonCopula(4)
sim_cop<- rCopula(n, clayton_cop)
plot(sim_cop, main="Copule de Clayton", xlab="x", ylab="y", cex = 1)

frank_cop <- frankCopula(9.5)
sim_cop<- rCopula(n, frank_cop)
plot(sim_cop, main="Copule de Frank", xlab="x", ylab="y", cex = 1)

ind_cop<-indepCopula(dim = 2)
sim_cop<- rCopula(n, ind_cop)
plot(sim_cop, main="Copule indépendante", xlab="x", ylab="y", cex = 1)

# Copula Fitting
# Choose five variables and retain complete records
selected_vars <- c("SPX_rtn", "VIXCLS",	"T10YIE", "DFF", "DGS1", "DGS10", "BAA10Y")

selected_data <- daily_data[,selected_vars]

complete_data <- selected_data[complete.cases(selected_data), ]

u <- pobs(complete_data[,!(names(complete_data) %in% c('Date', 'Contraction'))],lower.tail = FALSE)

## Maximum pseudo-likelihood method using Gaussian Copula
gaussian_cop_mpl <- fitCopula(normalCopula(dim=7, dispstr = "un"), u, method="mpl")
write.csv(coef(gaussian_cop_mpl),"gaussian_cop_estimate.csv")
write.csv(confint(gaussian_cop_mpl),"gaussian_cop_ci_estimate.csv")

## Maximum pseudo-likelihood method using t Copula
t_cop_mpl <- fitCopula(tCopula(dim=7, dispstr = "un"), u, method="mpl")
write.csv(coef(t_cop_mpl),"t_cop_estimate.csv")
write.csv(confint(t_cop_mpl),"t_cop_ci_estimate.csv")

## Maximum pseudo-likelihood method using Gumbel Copula
gumbel_cop_mpl <- fitCopula(gumbelCopula(dim=7), u, method="mpl")
coef(gumbel_cop_mpl)
confint(gumbel_cop_mpl)

## Maximum pseudo-likelihood method using Calyton Copula
clayton_cop_mpl <- fitCopula(claytonCopula(dim=7), u, method="mpl")
coef(clayton_cop_mpl)
confint(clayton_cop_mpl)

## Maximum pseudo-likelihood method using Frank Copula
frank_cop_mpl <- fitCopula(frankCopula(dim=7), u, method="itau")
coef(frank_cop_mpl)
confint(frank_cop_mpl)

## Goodness-of-fit analysis using Cramer-von Mises statistic
set.seed(123456)
u_new <- unique(u)
for (name in colnames(u_new)){
	u_new[,name] <- u_new[,name] + rnorm(nrow(u_new),0,0.00001) #to avoid ties
}
dim_no <- length(selected_vars)

idx <- sample(seq(1, 2), size = nrow(u_new), replace = TRUE, prob = c(.99, .01))
u_new <- u_new[idx==2,]

N <- 200 #No. of bootstrapping for p-value calculation
set.seed(123)
gofCopula(normalCopula(dim=dim_no, dispstr = "un"), u_new, N=N, method="Sn", estim.method = "mpl", simulation="pb")
gofCopula(tCopula(dim=dim_no, dispstr = "un", df=12, df.fixed=TRUE), u_new, N=N, method="Sn", estim.method = "mpl", simulation="pb")
gofCopula(gumbelCopula(dim=dim_no), u_new, N=N, method="Sn", estim.method = "mpl", simulation="pb")
gofCopula(claytonCopula(dim=dim_no), u_new, N=N, method="Sn", estim.method = "mpl", simulation="pb")
gofCopula(frankCopula(dim=dim_no), u_new, N=N, method="Sn", estim.method = "itau", simulation="pb")
 

## Compare copulas between implied volatility and credit spread

## Maximum pseudo-likelihood method using Gaussian Copula

par(mfrow=c(2,3))
n<-3000
set.seed(123)
gaussian_cop <- normalCopula(0.619115656)
sim_cop<- rCopula(n, gaussian_cop)
plot(sim_cop, main="Gaussian Copula", xlab="x", ylab="y", cex = 0.5)

t_cop <- tCopula(0.6423154, df=12)
sim_cop<- rCopula(n, t_cop)
plot(sim_cop, main="t Copula", xlab="x", ylab="y", cex = 0.5)

gumbel_cop <- gumbelCopula(1.037)
sim_cop<- rCopula(n, gumbel_cop)
plot(sim_cop, main="Gumbel Copula", xlab="x", ylab="y", cex = 0.5)

clayton_cop <- claytonCopula(0.125)
sim_cop<- rCopula(n, clayton_cop)
plot(sim_cop, main="Clayton Copula", xlab="x", ylab="y", cex = 0.5)

frank_cop <- frankCopula(1.08)
sim_cop<- rCopula(n, frank_cop)
plot(sim_cop, main="Frank Copula", xlab="x", ylab="y", cex = 0.5)

sim_cop<- u[,c('VIXCLS','BAA10Y')]
plot(sim_cop, main="Empirical", xlab="x", ylab="y", cex = 0.5)

############################################################################
# Structured Model
###########################################################################

#Structured Model VAR
#install.packages("vars")
library(vars)

selected_vars <- c("SPX_rtn", "VIXCLS",	"T10YIE", "DFF", "DGS1", "DGS10", "BAA10Y")


#All complete data
selected_data <- daily_data[,selected_vars]
Traindata <- selected_data[complete.cases(selected_data), ]

p <- 5

var1 <- VAR(Traindata, p = p, type = "const")
stab1 <- stability(var1, h = 0.15, dynamic = FALSE, rescale = TRUE) #type = c("OLS-CUSUM", "Rec-CUSUM", "Rec-MOSUM","OLS-MOSUM", "RE", "ME", "Score-CUSUM", "Score-MOSUM", "fluctuation"),
plot(stab1) #stability graph to check if VAR model converges
serial.test(var1, lags.pt=10, type="PT.asymptotic")

nr <- length(selected_vars)*p + 1
varoutput <- matrix(NA,nrow=nr, ncol=length(selected_vars))
colnames(varoutput) <- selected_vars

for (i in selected_vars) {
	varoutput[,i] <- var1$varresult[i][[1]]$coefficients
}
rownames(varoutput) <- names(var1$varresult[i][[1]]$coefficients)
write.csv(t(varoutput),"varoutput.csv") #VAR(1) coefficients
write.csv(summary(var1)$corres,"var1corres.csv") #correlation matrix of residuals
write.csv(summary(var1)$covres,"var1covres.csv") #covariance matrix of residuals

#Solve stable means
tvaroutput <- t(varoutput)

A<-tvaroutput[,1:length(selected_vars)]
if (p>1){
	for (i in c(2:p)){
		A <- A+tvaroutput[,(length(selected_vars)*(i-1)+1):(length(selected_vars)*i)]
	}

}
B<-tvaroutput[,length(selected_vars)*p+1]
A<- -A
for (i in c(1:nrow(A))){
	A[i,i] <- A[i,i]+1
}
stablemeans <- solve(A,B)
write.csv(stablemeans,"stablemeans.csv")
write.csv(sapply(Traindata, mean, na.rm=TRUE),"historicalmeans.csv")

chol <- chol(summary(var1)$corres) #Cholesky decomposition of residuals
write.csv(chol,"chol.csv", row.names=FALSE)

#Pre 2020
daily_data_exclude <- daily_data[as.Date(daily_data$Date) < as.Date("2020-1-1"),]
daily_data_exclude <- daily_data[as.Date(daily_data$Date) > as.Date("2002-1-1"),]
selected_data <- daily_data_exclude[,selected_vars]
Traindata <- selected_data[complete.cases(selected_data), ]

p <- 5

var1 <- VAR(Traindata, p = p, type = "const")
stab1 <- stability(var1, h = 0.15, dynamic = FALSE, rescale = TRUE) #type = c("OLS-CUSUM", "Rec-CUSUM", "Rec-MOSUM","OLS-MOSUM", "RE", "ME", "Score-CUSUM", "Score-MOSUM", "fluctuation"),
plot(stab1) #stability graph to check if VAR model converges
serial.test(var1, lags.pt=10, type="PT.asymptotic")

nr <- length(selected_vars)*p + 1
varoutput <- matrix(NA,nrow=nr, ncol=length(selected_vars))
colnames(varoutput) <- selected_vars

for (i in selected_vars) {
	varoutput[,i] <- var1$varresult[i][[1]]$coefficients
}
rownames(varoutput) <- names(var1$varresult[i][[1]]$coefficients)
write.csv(t(varoutput),"varoutput_ex.csv") #VAR(1) coefficients
write.csv(summary(var1)$corres,"var1corres_ex.csv") #correlation matrix of residuals
write.csv(summary(var1)$covres,"var1covres_ex.csv") #covariance matrix of residuals

#Solve stable means
tvaroutput <- t(varoutput)

A<-tvaroutput[,1:length(selected_vars)]
if (p>1){
	for (i in c(2:p)){
		A <- A+tvaroutput[,(length(selected_vars)*(i-1)+1):(length(selected_vars)*i)]
	}

}
B<-tvaroutput[,length(selected_vars)*p+1]
A<- -A
for (i in c(1:nrow(A))){
	A[i,i] <- A[i,i]+1
}
stablemeans <- solve(A,B)
write.csv(stablemeans,"stablemeans_ex.csv")
write.csv(sapply(Traindata, mean, na.rm=TRUE),"historicalmeans.csv")

chol <- chol(summary(var1)$corres) #Cholesky decomposition of residuals
write.csv(chol,"chol_ex.csv", row.names=FALSE)


#post-2020 data
selected_data <- daily_new[,selected_vars]
Traindata <- selected_data[complete.cases(selected_data), ]

p <- 5

var1 <- VAR(Traindata, p = p, type = "const")
stab1 <- stability(var1, h = 0.15, dynamic = FALSE, rescale = TRUE) #type = c("OLS-CUSUM", "Rec-CUSUM", "Rec-MOSUM","OLS-MOSUM", "RE", "ME", "Score-CUSUM", "Score-MOSUM", "fluctuation"),
plot(stab1) #stability graph to check if VAR model converges
serial.test(var1, lags.pt=10, type="PT.asymptotic")

nr <- length(selected_vars)*p + 1
varoutput <- matrix(NA,nrow=nr, ncol=length(selected_vars))
colnames(varoutput) <- selected_vars

for (i in selected_vars) {
	varoutput[,i] <- var1$varresult[i][[1]]$coefficients
}
rownames(varoutput) <- names(var1$varresult[i][[1]]$coefficients)
write.csv(t(varoutput),"varoutput_new.csv") #VAR(1) coefficients
write.csv(summary(var1)$corres,"var1corres_new.csv") #correlation matrix of residuals
write.csv(summary(var1)$covres,"var1covres_new.csv") #covariance matrix of residuals

#Solve stable means
tvaroutput <- t(varoutput)

A<-tvaroutput[,1:length(selected_vars)]
if (p>1){
	for (i in c(2:p)){
		A <- A+tvaroutput[,(length(selected_vars)*(i-1)+1):(length(selected_vars)*i)]
	}

}
B<-tvaroutput[,length(selected_vars)*p+1]
A<- -A
for (i in c(1:nrow(A))){
	A[i,i] <- A[i,i]+1
}
stablemeans <- solve(A,B)
write.csv(stablemeans,"stablemeans_new.csv")
write.csv(sapply(Traindata, mean, na.rm=TRUE),"historicalmeans_new.csv")

chol <- chol(summary(var1)$corres) #Cholesky decomposition of residuals
write.csv(chol,"chol_new.csv", row.names=FALSE)

#2008 financial crisis data
selected_data <- daily_fc_2008[,selected_vars]
Traindata <- selected_data[complete.cases(selected_data), ]

p <- 5

var1 <- VAR(Traindata, p = p, type = "const")
stab1 <- stability(var1, h = 0.15, dynamic = FALSE, rescale = TRUE) #type = c("OLS-CUSUM", "Rec-CUSUM", "Rec-MOSUM","OLS-MOSUM", "RE", "ME", "Score-CUSUM", "Score-MOSUM", "fluctuation"),
plot(stab1) #stability graph to check if VAR model converges
serial.test(var1, lags.pt=10, type="PT.asymptotic")

nr <- length(selected_vars)*p + 1
varoutput <- matrix(NA,nrow=nr, ncol=length(selected_vars))
colnames(varoutput) <- selected_vars

for (i in selected_vars) {
	varoutput[,i] <- var1$varresult[i][[1]]$coefficients
}
rownames(varoutput) <- names(var1$varresult[i][[1]]$coefficients)
write.csv(t(varoutput),"varoutput_fc_2008.csv") #VAR(1) coefficients
write.csv(summary(var1)$corres,"var1corres_fc_2008.csv") #correlation matrix of residuals
write.csv(summary(var1)$covres,"var1covres_fc_2008.csv") #covariance matrix of residuals

#Solve stable means
tvaroutput <- t(varoutput)

A<-tvaroutput[,1:length(selected_vars)]
if (p>1){
	for (i in c(2:p)){
		A <- A+tvaroutput[,(length(selected_vars)*(i-1)+1):(length(selected_vars)*i)]
	}

}
B<-tvaroutput[,length(selected_vars)*p+1]
A<- -A
for (i in c(1:nrow(A))){
	A[i,i] <- A[i,i]+1
}
stablemeans <- solve(A,B)
write.csv(stablemeans,"stablemeans_fc_2008.csv")
write.csv(sapply(Traindata, mean, na.rm=TRUE),"historicalmeans_fc_2008.csv")

chol <- chol(summary(var1)$corres) #Cholesky decomposition of residuals
write.csv(chol,"chol_fc_2008.csv", row.names=FALSE)


#STL (Loess)

daily_new <- daily_data[as.Date(daily_data$Date) >= as.Date("2020-1-1"),]
daily_fc_2008 <- daily_data[20062:20458,]
daily_great_depression <- daily_data[397:1306,]
daily_black_monday <- daily_data[14975:15291,]

daily_new <- daily_data[397:1306,]
ts_new <-ts(daily_new$SPX/daily_new$SPX[1], start = 0, frequency = 250)
stl_result<-stl(ts_new, s.window = "per")

par(mfrow=c(3,1))
plot(stl_result$time.series[,1], main="STL: Great Depression (Aug. 1929 ~ Mar. 1933)", ylab ="Seasonality")
plot(stl_result$time.series[,2], ylab = "Trend")
plot(stl_result$time.series[,3], ylab = "Residual")

daily_new <- daily_data[14975:15475,]
ts_new <-ts(daily_new$SPX/daily_new$SPX[1], start = 0, frequency = 250)
stl_result<-stl(ts_new, s.window = "per")

par(mfrow=c(3,1))
plot(stl_result$time.series[,1], main="STL: Balck Monday (Oct. 1987 ~ Sept. 1989)", ylab ="Seasonality")
plot(stl_result$time.series[,2], ylab = "Trend")
plot(stl_result$time.series[,3], ylab = "Residual")

daily_new <- daily_data[20062:20562,]
ts_new <-ts(daily_new$SPX/daily_new$SPX[1], start = 0, frequency = 250)
stl_result<-stl(ts_new, s.window = "per")

par(mfrow=c(3,1))
plot(stl_result$time.series[,1], main="STL: Financial Crisis (Dec. 2007 ~ Nov. 2009)", ylab ="Seasonality")
plot(stl_result$time.series[,2], ylab = "Trend")
plot(stl_result$time.series[,3], ylab = "Residual")

daily_new <- daily_data[as.Date(daily_data$Date) >= as.Date("2020-1-1"),]
ts_new <-ts(daily_new$SPX/daily_new$SPX[1], start = 0, frequency = 250)
stl_result<-stl(ts_new, s.window = "per")

par(mfrow=c(3,1))
plot(stl_result$time.series[,1], main="STL: COVID Pandemic (Jan. 2020 ~ Apr. 2022)", ylab ="Seasonality")
plot(stl_result$time.series[,2], ylab = "Trend")
plot(stl_result$time.series[,3], ylab = "Residual")


############################################################################
# Word Cloud
############################################################################

# install.packages("wordcloud2")
library(wordcloud2)

# Set working directory
setwd("C:/dsge/market_vol")

df = read.csv("word_freq.csv")
set.seed(123) # for reproducibility 
df$freq = df$freq/100000
# df <- df[3:nrow(df),]
wordcloud2(data=df, size=1.6, color='random-dark')

############################################################################
# Attribution Analysis
############################################################################

# Fill missing values
library(zoo)
na_col_names <- names(which(colSums(is.na(daily_data))>0))

for (col in na_col_names){
	# print(col)
	start_idx <- which(!is.na(daily_data[,col]))[1]
	end_idx <- tail(which(!is.na(daily_data[,col])), n=1)
	daily_data[start_idx:end_idx,col] <- na.spline(daily_data[start_idx:end_idx,col])
}

X_vars_rtn <- c("GPDI", "PCE", "CPIAUCSL", "PMSAVE", "DSPI", "UMCSENT", "AUM_HF", "AUM_fof", "AUM_BSB", "AUM_Con", "AUM_Dis", "AUM_EM",
			"AUM_EMA", "AUM_EMLA", "AUM_EMG", "AUM_EMEE", "AUM_ELO", "AUM_ELB", "AUM_ELS", "AUM_EMN",
			"AUM_ED", "AUM_FI", "AUM_MA", "AUM_Mac", "AUM_MS", "AUM_OS", "AUM_Other", "AUM_SS", 
			"ma_debit", "ma_credit")

for (col in X_vars_rtn){
	start_idx <- which(!is.na(daily_data[,col]))[1]
	end_idx <- tail(which(!is.na(daily_data[,col])), n=1)
	daily_data[(start_idx+1):end_idx,col] <- daily_data[(start_idx+1):end_idx,col]/daily_data[(start_idx):(end_idx-1),col]-1
	daily_data[start_idx,col] <- NA
}

X_vars_gdp <- c("W994RC1Q027SBEA", "M318501Q027NBEA", "MTSDS133FMS")

for (col in X_vars_gdp){
	start_idx <- which(!is.na(daily_data[,col]))[1]
	end_idx <- tail(which(!is.na(daily_data[,col])), n=1)
	daily_data[start_idx:end_idx,col] <- daily_data[start_idx:end_idx,col]/daily_data[start_idx:end_idx,"GDP"]
	if (col == "MTSDS133FMS"){
		daily_data[start_idx:end_idx,col] <- daily_data[start_idx:end_idx,col]*1000
	}
}

daily_data[sapply(daily_data, is.infinite)] <- NA

# Autocorrelation and cross correlation analysis

Non_X_vars <- c('Date','SPX','NASDAQCOM','RUT','SPX_rtn','NASDAQ100_rtn','NASDAQCOM_rtn',
				'VXDCLS','VXNCLS','RVXCLS','VXVCLS','VIXCLS','RUT_rtn','DJI_rtn','VVIX',
				'GDP')

all_vars <- colnames(daily_data)

X_vars <- all_vars[!all_vars %in% Non_X_vars]

# X_vars <- c("T10YIE", "DFF", "DGS1", "DGS10", "Contraction" , "MICH", "CPALTT01USM657N", "GPDI",
			# "T5YIE", "PCE", "CPIAUCSL", "W994RC1Q027SBEA", "PSAVERT", "PMSAVE", "M318501Q027NBEA",
			# "MTSDS133FMS", "DSPI", "UMCSENT", "GFDEGDQ188S", "A191RL1Q225SBEA", "covid_case_us",
			# "covid_death_us", "AUM_HF", "AUM_fof", "AUM_BSB", "AUM_Con", "AUM_Dis", "AUM_EM",
			# "AUM_EMA", "AUM_EMLA", "AUM_EMG", "AUM_EMEE", "AUM_ELO", "AUM_ELB", "AUM_ELS", "AUM_EMN",
			# "AUM_ED", "AUM_FI", "AUM_MA", "AUM_Mac", "AUM_MS", "AUM_OS", "AUM_Other", "AUM_SS", 
			# "RH_MAU", "RH_AUC", "Retail_Share", "gt_ukraine", "gt_pademic", "gt_covid", "gt_market_crash",
			# "gt_inflation", "gt_job", "gt_interest_rate", "gt_stock_market", "ma_debit", "ma_credit")
			
n_x_vars <- length(X_vars)

Y_vars <- c("VIXCLS")
n_y_vars <- length(Y_vars)

n_rows <- n_x_vars * n_y_vars

n_lags <- 23

ccs <- matrix(nrow = n_rows, ncol = n_lags)

idx = 1
for (y in Y_vars){
	for (x in X_vars){
		ccs[idx,] <- ccf(daily_data[,y], daily_data[,x], na.action = na.pass, lag.max= n_lags-1)$acf[1:(n_lags)]#:(2*n_lags-1)]
		idx = idx + 1
	}
}
rownames(ccs) <- X_vars
write.csv(ccs,"ccs.csv")

daily_new <- daily_data[as.Date(daily_data$Date) >= as.Date("2000-1-1"),]
daily_new <- daily_new[as.Date(daily_new$Date) <= as.Date("2022-6-30"),]
na_col_names <- names(which(colSums(is.na(daily_new))>0))

for (col in na_col_names){
	# print(col)
	daily_new[,col] <- na.spline(daily_new[,col])
}


# c("W994RC1Q027SBEA","M318501Q027NBEA","covid_case_us","covid_death_us" ,"RH_AUC")



ccs <- matrix(nrow = n_rows, ncol = n_lags)

idx = 1
for (y in Y_vars){
	for (x in X_vars){
		ccs[idx,] <- ccf(daily_new[,y], daily_new[,x], na.action = na.pass, lag.max= n_lags-1)$acf[1:(n_lags)]#:(2*n_lags-1)]
		idx = idx + 1
	}
}
rownames(ccs) <- X_vars
write.csv(ccs,"ccs_new.csv")

daily_new <- daily_data[as.Date(daily_data$Date) >= as.Date("2020-1-1"),]
daily_new <- daily_new[as.Date(daily_new$Date) <= as.Date("2022-6-30"),]
na_col_names <- names(which(colSums(is.na(daily_new))>0))

for (col in na_col_names){
	# print(col)
	daily_new[,col] <- na.spline(daily_new[,col])
}


# c("W994RC1Q027SBEA","M318501Q027NBEA","covid_case_us","covid_death_us" ,"RH_AUC")

ccs <- matrix(nrow = n_rows, ncol = n_lags)

idx = 1
for (y in Y_vars){
	for (x in X_vars){
		ccs[idx,] <- ccf(daily_new[,y], daily_new[,x], na.action = na.pass, lag.max= n_lags-1)$acf[1:(n_lags)]#:(2*n_lags-1)]
		idx = idx + 1
	}
}
rownames(ccs) <- X_vars
write.csv(ccs,"ccs_pandemic.csv")

daily_new <- daily_data[as.Date(daily_data$Date) >= as.Date("2011-1-1"),]
daily_new <- daily_new[as.Date(daily_new$Date) <= as.Date("2022-6-30"),]
na_col_names <- names(which(colSums(is.na(daily_new))>0))

for (col in na_col_names){
	# print(col)
	daily_new[,col] <- na.spline(daily_new[,col])
}


# c("W994RC1Q027SBEA","M318501Q027NBEA","covid_case_us","covid_death_us" ,"RH_AUC")

ccs <- matrix(nrow = n_rows, ncol = n_lags)

idx = 1
for (y in Y_vars){
	for (x in X_vars){
		ccs[idx,] <- ccf(daily_new[,y], daily_new[,x], na.action = na.pass, lag.max= n_lags-1)$acf[1:(n_lags)]#:(2*n_lags-1)]
		idx = idx + 1
	}
}
rownames(ccs) <- X_vars
write.csv(ccs,"ccs_reddit.csv")




# Regression analysis
# install.packages(c("rpart","rpart.plot","FNN","gbm","randomForest","glmnet"))

library(rpart)
library(rpart.plot)
library(FNN)
library(gbm)
library(randomForest)
library(glmnet)

daily_new <- daily_data[as.Date(daily_data$Date) >= as.Date("2000-1-1"),]
daily_new <- daily_new[as.Date(daily_new$Date) <= as.Date("2022-6-30"),]
na_col_names <- names(which(colSums(is.na(daily_new))>0))

for (col in na_col_names){
	# print(col)
	daily_new[,col] <- na.spline(daily_new[,col])
}

for (x in X_vars){
	daily_new[,x] <- scale(daily_new[,x])
}

rawdata <- daily_new
Ynames <- Y_vars

# define a dataframe that will be used to store the results
modeloutput <- data.frame(y=character(),
				 RMSE_train=double(), 		#RMSE based on training data
                 R2_train=double(),   		#R-squared based on training data
                 R2Adjust_train=double(), 	#Adjusted R-squared based on training data
				 RMSE=double(),				#RMSE based on valiation data
                 R2=double(),				#R-squared based on valiation data
                 R2Adjust=double(),			#Adjusted R-squared based on validation data
				 df=integer(),				#degree of freedom
				 fvar=double(),				#total variance
				 sfvar=double(),			#residual variance
				 rvar=double(),				#recession variance
				 srvar=double(),			#residual recession variance
				 corr=double(),				#correlation coefficient
				 scorr=double(),			#correlation coefficient during recessions
                 Models=character(),		#model type
                 stringsAsFactors=FALSE)


lag <-21 # how many lags to be used for regressions. A lag of 2 means x, x(-1), and x(-2) will be used in the regression.
lags <- c(1,3,5,10,21)
residuals <- matrix(,nrow=nrow(rawdata)-lag,ncol=length(Ynames))		#store residuals of linear regression
residualsgbm <- matrix(,nrow=nrow(rawdata)-lag,ncol=length(Ynames))		#store residuals of GBM
residualsridge <- matrix(,nrow=nrow(rawdata)-lag,ncol=length(Ynames))	#store residuals of Ridge regression
residualslasso <- matrix(,nrow=nrow(rawdata)-lag,ncol=length(Ynames))	#store residuals of Lasso regression
residualsrf <- matrix(,nrow=nrow(rawdata)-lag,ncol=length(Ynames))		#store residuals of RandomForest
#set column names of these data frames that store residuals
colnames(residuals) <- Ynames											
colnames(residualsgbm) <- Ynames
colnames(residualsridge) <- Ynames
colnames(residualslasso) <- Ynames
colnames(residualsrf) <- Ynames

lambda = 0.5	#inital weight for regularization in Lasso and Ridge Regression. It is not used in the final optimized lambda value.
alpha = 0.5	#inital weight for regularization in Lasso and Ridge Regression. It is not used in the final optimized alpha value.
################################################################
# Model Calibration using training/validation split ############
################################################################

#You may see some warning messages populated for certain y variables and models. They may indicate some issues which can
#be caused by data scarcity. However, the model assessment is based on validation data and therefore the warnings may not
#be very meaningful for model selection.

for (y in Ynames) {

	# Traindata <- rawdata[,names(rawdata) %in% c(y,Xnames,"extreme_ind")]
	# Xnames1 <- names(Traindata)[!names(Traindata) %in% c("extreme_ind")] #get a list of variables to generate lagged variables
	
	Traindata <- rawdata[,names(rawdata) %in% c(y,X_vars)]
	Xnames1 <- names(Traindata) #get a list of variables to generate lagged variables

	#generate lagged variables
	if (lag > 0) {
		for (i in lags){
			for (varname in X_vars){
				Traindata[(i+1):nrow(Traindata),paste0(varname,i)] <- Traindata[1:(nrow(Traindata)-i),varname]
				Traindata[1:i,paste0(varname,i)] <- NA
			}
		}
	}
	
	#remove data records that have NA. If the lag equals 2, the first two records will be removed as x(-1) and x(-2) have no values
	Traindata <- na.omit(Traindata[(lag+1):nrow(Traindata),])
	
	# Xnames2 <- names(Traindata)[!names(Traindata) %in% c(y,"extreme_ind")]
	
	Xnames2 <- names(Traindata)[!names(Traindata) %in% c(y,Xnames1)]

	#generate the formula used for calibration
	f <-as.formula(paste(y,"~",paste(Xnames2,collapse="+")))
	print(f)

	#split the data into training (80%) and validation (20%)
	set.seed(6)
	idx <- sample(seq(1, 2), size = nrow(Traindata), replace = TRUE, prob = c(.8, .2))
	training <- Traindata[idx==1,]
	validation <- Traindata[idx==2,]

	withCallingHandlers({
		#linear regression
		mdl <- "linear regression"
		lr<-lm(f, data=training)
		lrpredict <- predict(lr,training)
		rmse_train<-sqrt(mean((training[[y]]-lrpredict)^2))
		r2_train<-1-sum((training[[y]]-lrpredict)^2)/sum((training[[y]]-mean(training[[y]]))^2)
		r2adjust_train <- r2_train-(1-r2_train)*length(Xnames2)/(nrow(training)-length(Xnames2)-1)
		lrpredict <- predict(lr,validation)
		rmse<-sqrt(mean((validation[[y]]-lrpredict)^2))
		r2<-1-sum((validation[[y]]-lrpredict)^2)/sum((validation[[y]]-mean(validation[[y]]))^2)
		r2adjust <- r2-(1-r2)*length(Xnames2)/(nrow(training)-length(Xnames2)-1)
		jpeg(file="sp_lr.jpeg")
		plot(validation[[y]], lrpredict, main="Linear Regression", xlab="True Volatility", ylab="Predicted Volatility", xlim=c(0,1),ylim=c(0,1),pch=19, col="Blue", cex=0.5)
		abline(a=0,b=1)
		dev.off()
		df<-nrow(training)-length(Xnames2)-1
		lrpredict <- predict(lr, Traindata)
		fvar <- var(lrpredict)
		sfvar <- var(lrpredict[Traindata$extreme_ind=="Y"])
		rvar <- var(Traindata[,y]-lrpredict)
		srvar <- var((Traindata[,y]-lrpredict)[Traindata$extreme_ind=="Y"])
		corr<-cor(Traindata[,y]-lrpredict,lrpredict)
		scorr<-cor((Traindata[,y]-lrpredict)[Traindata$extreme_ind=="Y"],lrpredict[Traindata$extreme_ind=="Y"])
		modeloutput[nrow(modeloutput)+1,] <- c(y, rmse_train, r2_train, r2adjust_train, rmse, r2, r2adjust, df, fvar, sfvar, rvar, srvar, corr,scorr, "LM")
		write.csv(summary(lr)$coefficients,paste0("lr_",y,".csv"))
		residuals[,y]<-c(rep(NA,nrow(residuals)-length(lr$residuals)),lr$residuals)
		
		#ridge regression
		mdl <- "ridge regression"
		ridge<-glmnet(as.matrix(training[,Xnames2]), as.matrix(training[[y]]), alpha = 0, lambda = lambda)
		# cv.out <- cv.glmnet(as.matrix(training[,Xnames2]), as.matrix(training[[y]]), alpha = 0)
		lambda <- 0.001#cv.out$lambda.min
		print(paste0(y," ridge regression: lambda = ", lambda))
		ridge<-glmnet(as.matrix(training[,Xnames2]), as.matrix(training[[y]]), alpha = 0, lambda = lambda)
		ridgepredict <- predict(ridge, s = lambda, newx = as.matrix(training[Xnames2]))
		rmse_train<-sqrt(mean((training[[y]]-ridgepredict)^2))
		r2_train<-1-sum((training[[y]]-ridgepredict)^2)/sum((training[[y]]-mean(training[[y]]))^2)
		r2adjust_train <- r2_train-(1-r2_train)*length(Xnames2)/(nrow(training)-length(Xnames2)-1)
		ridgepredict <- predict(ridge, s = lambda, newx = as.matrix(validation[Xnames2]))
		rmse<-sqrt(mean((validation[[y]]-ridgepredict)^2))
		r2<-1-sum((validation[[y]]-ridgepredict)^2)/sum((validation[[y]]-mean(validation[[y]]))^2)
		r2adjust <- r2-(1-r2)*length(Xnames2)/(nrow(training)-length(Xnames2)-1)
		jpeg(file="sp_rr.jpeg")
		plot(validation[[y]], ridgepredict, main="Ridge Regression", xlab="True Volatility", ylab="Predicted Volatility", xlim=c(0,1),ylim=c(0,1),pch=19, col="Blue", cex=0.5)
		abline(a=0,b=1)
		dev.off()
		df<-nrow(training)-length(Xnames2)-1
		ridgepredict <- predict(ridge, s = lambda, newx = as.matrix(Traindata[Xnames2]))
		fvar <- var(ridgepredict)
		sfvar <- var(ridgepredict[Traindata$extreme_ind=="Y"])
		rvar <- var(Traindata[,y]-ridgepredict)
		srvar <- var((Traindata[,y]-ridgepredict)[Traindata$extreme_ind=="Y"])
		corr<-cor(Traindata[,y]-ridgepredict,ridgepredict)
		scorr<-cor((Traindata[,y]-ridgepredict)[Traindata$extreme_ind=="Y"],ridgepredict[Traindata$extreme_ind=="Y"])
		modeloutput[nrow(modeloutput)+1,] <- c(y, rmse_train, r2_train, r2adjust_train, rmse, r2, r2adjust, df, fvar, sfvar, rvar, srvar, corr,scorr, "Ridge")
		write.csv(rbind(as.matrix(ridge$a0), as.matrix(ridge$beta)),paste0("ridge_",y,".csv"))
		residual_ridge <- Traindata[,y]-ridgepredict
		residualsridge[,y]<-c(rep(NA,nrow(residualsridge)-length(residual_ridge)),residual_ridge)

		#lasso regression
		mdl <- "lasso"
		lasso<-glmnet(as.matrix(training[,Xnames2]), as.matrix(training[[y]]), alpha = 1)
		cv.out <- cv.glmnet(as.matrix(training[,Xnames2]), as.matrix(training[[y]]), alpha = 1)
		lambda <- 0.0007477 #cv.out$lambda.min
		print(paste0(y," lasso regression: lambda = ", lambda))
		lasso<-glmnet(as.matrix(training[,Xnames2]), as.matrix(training[[y]]), alpha = 1, lambda = lambda)
		lassopredict <- predict(lasso, s = lambda, newx = as.matrix(training[Xnames2]))
		rmse_train<-sqrt(mean((training[[y]]-lassopredict)^2))
		r2_train<-1-sum((training[[y]]-lassopredict)^2)/sum((training[[y]]-mean(training[[y]]))^2)
		r2adjust_train <- r2_train-(1-r2_train)*length(Xnames2)/(nrow(training)-length(Xnames2)-1)
		lassopredict <- predict(lasso, s = lambda, newx = as.matrix(validation[Xnames2]))
		rmse<-sqrt(mean((validation[[y]]-lassopredict)^2))
		r2<-1-sum((validation[[y]]-lassopredict)^2)/sum((validation[[y]]-mean(validation[[y]]))^2)
		r2adjust <- r2-(1-r2)*length(Xnames2)/(nrow(training)-length(Xnames2)-1)
		jpeg(file="sp_lasso.jpeg")
		plot(validation[[y]], lassopredict, main="Lasso Regression", xlab="True Volatility", ylab="Predicted Volatility", xlim=c(0,1),ylim=c(0,1),pch=19, col="Blue", cex=0.5)
		abline(a=0,b=1)
		dev.off()
		df<-nrow(training)-length(Xnames2)-1
		lassopredict <- predict(lasso, s = lambda, newx = as.matrix(Traindata[Xnames2]))
		fvar <- var(lassopredict)
		sfvar <- var(lassopredict[Traindata$extreme_ind=="Y"])
		rvar <- var(Traindata[,y]-lassopredict)
		srvar <- var((Traindata[,y]-lassopredict)[Traindata$extreme_ind=="Y"])
		corr<-cor(Traindata[,y]-lassopredict,lassopredict)
		scorr<-cor((Traindata[,y]-lassopredict)[Traindata$extreme_ind=="Y"],lassopredict[Traindata$extreme_ind=="Y"])
		modeloutput[nrow(modeloutput)+1,] <- c(y, rmse_train, r2_train, r2adjust_train, rmse, r2, r2adjust, df, fvar, sfvar, rvar, srvar, corr,scorr, "lasso")
		write.csv(rbind(as.matrix(lasso$a0), as.matrix(lasso$beta)),paste0("lasso_",y,".csv"))
		residual_lasso <- Traindata[,y]-lassopredict
		residualslasso[,y]<-c(rep(NA,nrow(residualslasso)-length(residual_lasso)),residual_lasso)

		#cart
		mdl <- "CART"
		cart = rpart(f, data = training, cp = 10^(-3),minsplit = 10)
		cartpredict <- predict(cart, training)
		rmse_train<-sqrt(mean((training[[y]]-cartpredict)^2))
		r2_train<-1-sum((training[[y]]-cartpredict)^2)/sum((training[[y]]-mean(training[[y]]))^2)
		r2adjust_train <- r2_train-(1-r2_train)*(length(unique(cart$frame$var))-1)/(nrow(training)-(length(unique(cart$frame$var))-1)-1)
		cartpredict <- predict(cart, validation)
		rmse<-sqrt(mean((validation[[y]]-cartpredict)^2))
		r2<-1-sum((validation[[y]]-cartpredict)^2)/sum((validation[[y]]-mean(validation[[y]]))^2)
		r2adjust <- r2-(1-r2)*(length(unique(cart$frame$var))-1)/(nrow(training)-(length(unique(cart$frame$var))-1)-1)
		jpeg(file="sp_cart.jpeg")
		plot(validation[[y]], cartpredict, main="CART", xlab="True Volatility", ylab="Predicted Volatility", xlim=c(0,1),ylim=c(0,1),pch=19, col="Blue", cex=0.5)
		abline(a=0,b=1)
		dev.off()
		#rpart.plot(cart)
		df<-nrow(training)-(length(unique(cart$frame$var))-1)-1
		cartpredict <- predict(cart, Traindata)
		fvar <- var(cartpredict)
		sfvar <- var(cartpredict[Traindata$extreme_ind=="Y"])
		rvar <- var(Traindata[,y]-cartpredict)
		srvar <- var((Traindata[,y]-cartpredict)[Traindata$extreme_ind=="Y"])
		corr<-cor((Traindata[,y]-cartpredict),cartpredict)
		scorr<-cor((Traindata[,y]-cartpredict)[Traindata$extreme_ind=="Y"],cartpredict[Traindata$extreme_ind=="Y"])
		write.csv(cart$variable.importance,"cart_varimp.csv")
		modeloutput[nrow(modeloutput)+1,] <- c(y, rmse_train, r2_train, r2adjust_train, rmse, r2, r2adjust, df, fvar, sfvar, rvar, srvar, corr,scorr, "CART")

		#knn
		mdl <- "KNN"
		knnreg <- knn.reg(train = training[,names(validation) %in% Xnames2] , test=training[,names(training) %in% Xnames2], y=training[[y]], k=5, algorithm = "kd_tree")
		rmse_train<-sqrt(mean((training[[y]]-knnreg$pred)^2))
		r2_train<-1-sum((training[[y]]-knnreg$pred)^2)/sum((training[[y]]-mean(training[[y]]))^2)
		r2adjust_train <- r2_train-(1-r2_train)*length(Xnames2)/(nrow(training)-length(Xnames2)-1)
		knnreg <- knn.reg(train = training[,names(validation) %in% Xnames2] , test=validation[,names(validation) %in% Xnames2], y=training[[y]], k=5, algorithm = "kd_tree")
		rmse<-sqrt(mean((validation[[y]]-knnreg$pred)^2))
		r2<-1-sum((validation[[y]]-knnreg$pred)^2)/sum((validation[[y]]-mean(validation[[y]]))^2)
		r2adjust <- r2-(1-r2)*length(Xnames2)/(nrow(training)-length(Xnames2)-1)
		df<-nrow(training)-length(Xnames2)-1
		knnreg <- knn.reg(train = training[,names(validation) %in% Xnames2] , test=Traindata[,names(Traindata) %in% Xnames2], y=training[[y]], k=5, algorithm = "kd_tree")
		fvar <- var(knnreg$pred)
		sfvar <- var(knnreg$pred[Traindata$extreme_ind=="Y"])
		rvar <- var(Traindata[,y]-knnreg$pred)
		srvar <- var((Traindata[,y]-knnreg$pred)[Traindata$extreme_ind=="Y"])
		corr<-cor((Traindata[,y]-knnreg$pred),knnreg$pred)
		scorr<-cor((Traindata[,y]-knnreg$pred)[Traindata$extreme_ind=="Y"],knnreg$pred[Traindata$extreme_ind=="Y"])

		modeloutput[nrow(modeloutput)+1,] <- c(y, rmse_train, r2_train, r2adjust_train, rmse, r2, r2adjust, df, fvar, sfvar, rvar, srvar, corr,scorr,"KNN")

		#gbm
		mdl <- "GBM"
		set.seed(123) #reset random seed as gbm may use random subset
		gbmreg<-gbm(f, data=training, distribution = "gaussian", interaction.depth=6, n.minobsinnode = 2, bag.fraction=0.7, n.trees = 500)
		gbmpredict <- predict(gbmreg, training, n.trees = 100)
		rmse_train<-sqrt(mean((training[[y]]-gbmpredict)^2))
		r2_train<-1-sum((training[[y]]-gbmpredict)^2)/sum((training[[y]]-mean(training[[y]]))^2)
		r2adjust_train <- r2_train-(1-r2_train)*length(Xnames2)/(nrow(training)-length(Xnames2)-1)
		gbmpredict <- predict(gbmreg, validation, n.trees = 100)
		rmse<-sqrt(mean((validation[[y]]-gbmpredict)^2))
		r2<-1-sum((validation[[y]]-gbmpredict)^2)/sum((validation[[y]]-mean(validation[[y]]))^2)
		r2adjust <- r2-(1-r2)*length(Xnames2)/(nrow(training)-length(Xnames2)-1)
		jpeg(file="sp_gbm.jpeg")
		plot(validation[[y]], gbmpredict, main="GBM", xlab="True Volatility", ylab="Predicted Volatility", xlim=c(0,1),ylim=c(0,1),pch=19, col="Blue", cex=0.5)
		abline(a=0,b=1)
		dev.off()
		df<-nrow(training)-length(Xnames2)-1
		gbmpredict <- predict(gbmreg, Traindata, n.trees = 100)	
		fvar <- var(gbmpredict)
		sfvar <- var(gbmpredict[Traindata$extreme_ind=="Y"])
		rvar <- var(Traindata[,y]-gbmpredict)
		srvar <- var((Traindata[,y]-gbmpredict)[Traindata$extreme_ind=="Y"])
		corr<-cor((Traindata[,y]-gbmpredict),gbmpredict)
		scorr<-cor((Traindata[,y]-gbmpredict)[Traindata$extreme_ind=="Y"],gbmpredict[Traindata$extreme_ind=="Y"])
		residual_gbm <- Traindata[,y]-gbmpredict
		residualsgbm[,y]<-c(rep(NA,nrow(residualsgbm)-length(residual_gbm)),residual_gbm)
		write.csv(summary.gbm(gbmreg),'gbm_varimp.csv')
		modeloutput[nrow(modeloutput)+1,] <- c(y, rmse_train, r2_train, r2adjust_train, rmse, r2, r2adjust, df, fvar, sfvar, rvar, srvar, corr,scorr,"gbm")

		#Random Forest
		mdl <- "RF"
		set.seed(123) #reset random seed as gbm may use random subset
		rfreg<-randomForest(f, data=training, nodesize = 2, ntree = 500)
		rfpredict <- predict(rfreg, training, n.trees = 100)
		rmse_train<-sqrt(mean((training[[y]]-rfpredict)^2))
		r2_train<-1-sum((training[[y]]-rfpredict)^2)/sum((training[[y]]-mean(training[[y]]))^2)
		r2adjust_train <- r2_train-(1-r2_train)*length(Xnames2)/(nrow(training)-length(Xnames2)-1)
		rfpredict <- predict(rfreg, validation, n.trees = 100)
		rmse<-sqrt(mean((validation[[y]]-rfpredict)^2))
		r2<-1-sum((validation[[y]]-rfpredict)^2)/sum((validation[[y]]-mean(validation[[y]]))^2)
		r2adjust <- r2-(1-r2)*length(Xnames2)/(nrow(training)-length(Xnames2)-1)
		jpeg(file="sp_rf.jpeg")
		plot(validation[[y]], rfpredict, main="Random Forests", xlab="True Volatility", ylab="Predicted Volatility", xlim=c(0,1),ylim=c(0,1),pch=19, col="Blue", cex=0.5)
		abline(a=0,b=1)
		dev.off()
		df<-nrow(training)-length(Xnames2)-1
		rfpredict <- predict(rfreg, Traindata, n.trees = 100)	
		fvar <- var(rfpredict)
		sfvar <- var(rfpredict[Traindata$extreme_ind=="Y"])
		rvar <- var(Traindata[,y]-rfpredict)
		srvar <- var((Traindata[,y]-rfpredict)[Traindata$extreme_ind=="Y"])
		corr<-cor((Traindata[,y]-rfpredict),rfpredict)
		scorr<-cor((Traindata[,y]-rfpredict)[Traindata$extreme_ind=="Y"],gbmpredict[Traindata$extreme_ind=="Y"])
		residual_rf <- Traindata[,y]-rfpredict
		residualsrf[,y]<-c(rep(NA,nrow(residualsrf)-length(residual_rf)),residual_rf)
		write.csv(importance(rfreg),'rf_varimp.csv')
		modeloutput[nrow(modeloutput)+1,] <- c(y, rmse_train, r2_train, r2adjust_train, rmse, r2, r2adjust, df, fvar, sfvar, rvar, srvar, corr,scorr,"rf")

	}, warning = function(ex) { 
		message(paste0("There are some issues happened for variable: ", y, "; model: ", mdl))
		message("Here's the original warning message:")
		message(ex)
		cat("\n")
	})

}

#output results and residuals
write.csv(modeloutput,paste0("modeloutput.csv"))
write.csv(residuals,paste0("residuals1.csv"))
write.csv(residualsridge,paste0("residuals1ridge.csv"))
write.csv(residualslasso,paste0("residuals1lasso.csv"))
write.csv(residualsgbm,paste0("residuals1gbm.csv"))

############################################################################
# Asset Allocation Example
############################################################################

# Scenario generation

vol_eq <- 0.19
rtn_eq <- exp(0.095+vol_eq**2/2)-1

#Warren4.5
vol_bond <- 0.079
rtn_bond <- exp(log(1+0.0473)+vol_bond**2/2)-1

corr_eq_bond <- 0.045

time_horizon <- 10
tracking_error_sigma <- 0.01

gbm_dt <- min(1/252, time_horizon)
sv_sum_days <- round(min(252, 252*time_horizon))

init_funding_ratio <- 1.0
init_liability <- 1.0e8
payment_ratio <- 0.02
init_asset <- init_liability * init_funding_ratio

#GBM
#Single asset class

gbm <-function(mu,sigma,dt,t0,T,r0=0){
    times <- c(t0)
    rates <- c(0)
    N = round((T-t0)/dt)
    for (i in c(1:N)){
        time <- i*dt+t0
        r <- (mu-sigma^2/2)*dt+sigma*rnorm(1,0,1)*sqrt(dt)
        rates <- c(rates,r)
        times <- c(times,time)
	}
    return(data.frame(times, rates))
}

# tr <- gbm(0.1, 0.20, 0.25, 0, 5)
# # print(tr)
# plot(tr)

gbm_simulations <- function(mu,sigma,dt,t0,T,n_sims,r0=0,seed=123){
    set.seed(seed)
    N <- round((T-t0)/dt)+1
    N_Terms = 1
    scenarios <- as.data.frame(matrix(0, ncol = N_Terms+2, nrow = n_sims*N))
    for (sim in c(1:n_sims)){
        tr <- gbm(mu,sigma,dt,t0,T,r0=r0)
#         print(tr)
        for (ti in c(1:N)){
            result <- c(sim,t0+ti*dt)
            result <- c(result,tr[ti,2])
            scenarios[(sim-1)*N+ti,] <- result
		}
	}
    return(scenarios)
}

# gbm_simulations(0.1, 0.20, 0.25, 0, 5, 1000)

library(MASS)
#Two asset classes
gbm2 <- function(mus,sigmas,corr,dt,t0,T){
    N <- round((T-t0)/dt)
    times <- c()
    rates1 <- c()
    rates2 <- c()
    covs <- matrix(c(sigmas[1]^2, sigmas[1]*sigmas[2]*corr, sigmas[1]*sigmas[2]*corr, sigmas[2]^2), nrow = 2, ncol = 2, byrow = TRUE)
    rnds <- mvrnorm(N, mu = c(0,0), Sigma = covs)
    for (i in c(1:N)){
        time = i*dt+t0
        r1 <- (mus[1]-sigmas[1]^2/2)*dt+rnds[i,1]*sqrt(dt)
        r2 <- (mus[2]-sigmas[2]^2/2)*dt+rnds[i,2]*sqrt(dt)
        rates1 <- c(rates1,r1)
        rates2 <- c(rates2,r2)
        times <- c(times,time)
	}
	return(data.frame(times,rates1,rates2))
}


tr <- gbm2(c(rtn_eq,rtn_bond), c(vol_eq, vol_bond), corr_eq_bond, 1, 0, 5)
plot(tr[,1:2])

gbm2_simulations <- function(mus,sigmas,corr,dt,t0,T,n_sims,seed=123, sum_days = 1){
    set.seed(seed)
    N <- round((T-t0)/dt/sum_days)
    N_Terms <- 2
    results <- as.data.frame(matrix(0, ncol = N_Terms+2, nrow = n_sims*N))
    for (sim in c(1:n_sims)){
		if (sim %% 10 == 0){print(sim)}
        tr <- gbm2(mus,sigmas,corr,dt,t0,T)
        for (ti in c(1:N)){
            result <- c(sim,t0+ti*dt)
            result <- c(sim,t0+ti*dt*sum_days)
            y_sp <- 1
            b_sp <- 1
            for (sp in c(1:sum_days)){
                y_sp = y_sp * exp(tr[(ti-1)*sum_days+sp,2])
                b_sp = b_sp * exp(tr[(ti-1)*sum_days+sp,3])
			}
            result <- c(result,c(y_sp-1, b_sp-1))
            results[(sim-1)*N+ti,] <- result
		}
	}
	colnames(results) <- c('sim','time','equity_return','bond_return')
    return(results)
}

# ys <- gbm2_simulations(c(rtn_eq,rtn_bond), c(vol_eq, vol_bond), corr_eq_bond, gbm_dt, 0, 3, 100, sum_days=252) #gbm_dt, time_horizon
# print(paste0("equity_rtn: ",mean(ys[,3]),"bond_rtn: ",mean(ys[,4]),"equity_sd: ", sd(ys[,3]),"bond_sd: ",sd(ys[,4])))

#SVJ

svj_sim <- function(mu, kappa, theta, sigma, rho_vega, Xi, omega, epsilon, rho_lambda, alpha, delta, vega, rho_z, rtn_bond, vol_bond, corr_eq_bond, h=1/252, t =3, v_init=0, lambda_init=0){

	n_steps <- round(t/h)+1
	alpha_bar <- exp(alpha+0.5*delta^2)/(1-rho_z*vega)-1
	rnds <- rnorm((n_steps)*4,0,1)
	v_prev <- v_init
	lambda_prev <- lambda_init
	ys <- c()
	bs <- c()
	steps <- c()
	epsilon_y  <-sqrt(1-rho_vega^2-rho_lambda^2)*rnds[1] + rho_vega*rnds[2] + rho_lambda*rnds[3]
	epsilon_bond <- sqrt(1-corr_eq_bond^2)*rnds[4] + corr_eq_bond*epsilon_y
	y <- (mu-0.5*v_prev-alpha_bar*lambda_prev)*h + sqrt(max(0,v_prev)*h)*epsilon_y+0.0
	v <- v_prev + kappa*(theta-max(0,v_prev))*h + sigma*sqrt(max(0,v_prev)*h)*rnds[2]+0.0
	l <- lambda_prev + Xi*(omega-max(0,lambda_prev))*h + epsilon*sqrt(max(0,lambda_prev)*h)*rnds[3]
	b <- (rtn_bond-vol_bond^2/2)*h+vol_bond*epsilon_bond*sqrt(h)
	ys <- c(ys,y)
	bs <- c(bs,b)
	steps <- c(steps,0)
	v_prev <- v
	lambda_prev <- l
	# print(delta)
	for (step in c(2:n_steps)){
		n_t = rpois(1,lambda=max(0,lambda_prev)*h)
		# print(n_t)
		epsilon_y <- sqrt(1-rho_vega^2-rho_lambda^2)*rnds[(step-1)*4+1] + rho_vega*rnds[(step-1)*4+2] + rho_lambda*rnds[(step-1)*4+3]
		epsilon_bond <- sqrt(1-corr_eq_bond^2)*rnds[(step-1)*4+4] + corr_eq_bond*epsilon_y
		if (n_t>=1){
			if (vega>0){
				z_rnds = rexp(n_t, rate = vega)
				z_rnds_sum <- sum(z_rnds)
			} else {
				z_rnds = rep(0, n_t)
				z_rnds_sum <- sum(z_rnds)
			}
			y_rnds_sum <- 0
			for (z_rnd in z_rnds){
				# print(alpha+rho_z*z_rnd)
				y_rnd <- rnorm(1,alpha+rho_z*z_rnd,delta)
				y_rnds_sum <- y_rnds_sum + y_rnd
			}
		}
		else{
			z_rnds_sum <- 0
			y_rnds_sum <- 0	
		}
		y <- (mu-0.5*v_prev-alpha_bar*lambda_prev)*h + sqrt(max(0,v_prev)*h)*epsilon_y+y_rnds_sum
		v <- v_prev + kappa*(theta-max(0,v_prev))*h + sigma*sqrt(max(0,v_prev)*h)*rnds[(step-1)*4+2]+z_rnds_sum
		l <- lambda_prev + Xi*(omega-max(0,lambda_prev))*h + epsilon*sqrt(max(0,lambda_prev)*h)*rnds[(step-1)*4+3]
		b <- (rtn_bond-vol_bond^2/2)*h+vol_bond*epsilon_bond*sqrt(h)
		ys <- c(ys,y)
		bs <- c(bs,b)
		steps <- c(steps,(step-1)*h)
		v_prev <- v
		lambda_prev <- l    
	}
	return(data.frame(steps,ys,bs))
}

mu = 0.092575
kappa = 87.026398
theta = 0.016528#0.010473
sigma = 4.157283
rho_vega = -0.670028
Xi = 0
omega = 0
epsilon = 0
rho_lambda = 0
alpha = 0
delta = 0
vega = 0
rho_z = 0

ys = svj_sim(mu, kappa, theta, sigma, rho_vega, Xi, omega, epsilon, rho_lambda, alpha, delta, vega, rho_z, rtn_bond, vol_bond, corr_eq_bond, h=1/252, t =3, v_init=theta, lambda_init=omega)

svj_simulations<-function(mu, kappa, theta, sigma, rho_vega, Xi, omega, epsilon, rho_lambda, alpha, delta, vega, rho_z, rtn_bond, vol_bond, corr_eq_bond, h=1/252, t =3, v_init=0, lambda_init=0,n_sims=10,seed=123, sum_days=1){
    set.seed(seed)
    N <- round(t/h/sum_days)
    N_Terms <- 2
    t0 <- 0
    results <- as.data.frame(matrix(0, ncol = N_Terms+2, nrow = n_sims*N))
    for (sim in c(1:n_sims)){
		if (sim %% 10 == 0){print(sim)}
        tr <- svj_sim(mu, kappa, theta, sigma, rho_vega, Xi, omega, epsilon, rho_lambda, alpha, delta, vega, rho_z, rtn_bond, vol_bond, corr_eq_bond, h, t, v_init, lambda_init)
#         print(tr)
        for (ti in c(1:N)){
            result <- c(sim,t0+ti*h*sum_days)
            y_sp <- 1
            b_sp <- 1
            for (sp in c(1:sum_days)){
                y_sp = y_sp * exp(tr[(ti-1)*sum_days+sp,2])
                b_sp = b_sp * exp(tr[(ti-1)*sum_days+sp,3])
			}
            result <- c(result,c(y_sp-1, b_sp-1))
            results[(sim-1)*N+ti,] <- result
		}
	}
	colnames(results) <- c('sim','time','equity_return','bond_return')
    return(results)
}
# ys <- svj_simulations(mu, kappa, theta, sigma, rho_vega, Xi, omega, epsilon, rho_lambda, alpha, delta, vega, rho_z, rtn_bond, vol_bond, corr_eq_bond, h=1/252, t =3, v_init=theta, lambda_init=omega, n_sims=100,seed=123, sum_days=252)
# print(paste0("equity_rtn: ",mean(ys[,3]),"bond_rtn: ",mean(ys[,4]),"equity_sd: ", sd(ys[,3]),"bond_sd: ",sd(ys[,4])))

# Reference dependent utiltiy function

ref_dependent_util<-function(parameters, W_t, W_t_star, type = 'ratio'){
    #type can be ratio or difference
	gamma <- parameters[1]
	alpha <- parameters[2]
	lambda1 <- parameters[3]
	beta <- parameters[4]

    if (W_t == W_t_star){
        return(0)
    }else if (W_t > W_t_star && type == 'ratio'){
		return(gamma*((W_t/W_t_star)^alpha - 1))
	}else if (W_t < W_t_star && type == 'ratio'){
        return(lambda1*((W_t/W_t_star)^beta - 1))
	}else if (W_t > W_t_star){
        return(amma*(W_t-W_t_star)^alpha)
	}else{
        return(-lambda1*(W_t_star-W_t)^beta)
	}
}

plot_ref_dependent_util<-function(W_t_low,W_t_high,parameters,W_t_star=1,type = 'ratio'){
    Ws <- seq(W_t_low, W_t_high, length.out=51)
    Us = c()
    for (W in Ws){
        Us <- c(Us, ref_dependent_util(parameters, W, W_t_star, type))
	}
    return(data.frame(Ws, Us))
}

WUs <- plot_ref_dependent_util(0.2,2.2,c(1, 0.44, 4.5, 0.88))
WUs2 <- plot_ref_dependent_util(0.2,2.2,c(2, 1, 2, 1))
plot(WUs, ylim=c(-4,3),type="l",col="blue", xlab="Asset/Liability", ylab="Utility")
lines(WUs2$Ws, WUs2$Us,lty=2,col="green")
legend(0.2, 3, legend=c("reference dependent", "risk neutral"), col=c("blue", "green"), lty=1:2, cex=0.8)

ce_ref_dependent_util<-function(parameters, Ws, W_t_star=1, type = 'ratio'){
    count <- 0
    sum <- 0
    for (W in Ws){
        sum <- sum + ref_dependent_util(parameters, W, W_t_star, type = 'ratio')
        count <- count + 1
	}
    return(sum/count)
}



polygon(c(x, rev(x)), c(y2, rev(y1)),
        col = "#6BD7AF")
		
		
# Project asset and liability at end of the time horizon for all scenarios
al_proj_multi_scen<-function(init_liab, init_asset, asset_mix, scenarios, n_scen, tracking_error_sigma, t, dt, t0=0, rnd_seed=123){
    scens <- c()
    times <- c()
    assets <- c()
    liabilities <- c()
    
    set.seed(rnd_seed)
    for (scen in c(1:n_scen)){
        asset <- init_asset
        liability <- init_liab
        for (idx in c((round(t0/dt)+1):round(t/dt))){
            asset_return <- asset_mix[1]*scenarios[round((scen-1)*round(time_horizon/dt)+idx),3] + asset_mix[2]*scenarios[round((scen-1)*round(time_horizon/dt)+idx),4]
            asset <- (asset-payment_ratio*dt*init_liability)*exp(asset_return)
            liability_return <- scenarios[round((scen-1)*round(time_horizon/dt)+idx),4] * (1+tracking_error_sigma*rnorm(1,0,1)*sqrt(dt))
            liability <- (liability-payment_ratio*dt*init_liability)*exp(liability_return)
		}
        scens<-c(scens,scen)
        times<-c(times,t)
        assets<-c(assets,asset)
        liabilities<-c(liabilities,liability)
	}
	return(data.frame(scens, times, assets, liabilities))
}

# scenarios <- gbm2_simulations(c(rtn_eq,rtn_bond), c(vol_eq, vol_bond), corr_eq_bond, gbm_dt, 0, time_horizon, 10)
# al_proj_multi_scen(init_liability, init_asset, c(0.1, 0.9), scenarios, 10, tracking_error_sigma, 3, gbm_dt)

# Find the optimal plan
n_scens <- 5000
time_horizon <- 3
funding_ratios <- seq(0.7, 1.4, length.out=29)

optimize_allocation<-function(scenarios, n_scens, time_horizon){
	max_plans <- c()
	for (fr in funding_ratios){
		fr_risk <- c()
		fr_avg <- c()
		fs_avg <- c()
		fs_risk <- c()
		fs_util <- c()
		plans <- c()
		# print(fr)
		step <- 0.01
		n_plans <- round(1/step) + 1

		for (plan in c(1:n_plans)){
			# print(plan)
			plans<-c(plans,plan)
			asset_mix <- c(plan*step,1-plan*step) #global equity, hedgeing assets 
			ending_als <- al_proj_multi_scen(init_liability, init_liability*fr, asset_mix, scenarios, n_scens, tracking_error_sigma, time_horizon, 1)
			funding_ratio <- ending_als[,3]/ending_als[,4]
			funding_surplus <- ending_als[,3] - ending_als[,4]
			fr_avg <- c(fr_avg,mean(funding_ratio))
		#     fr_vol.append(np.percentile(funding_ratios,98))
			fr_risk <- c(fr_risk,sd(funding_ratio))
			fs_avg <- c(fs_avg,mean(funding_surplus))
			fs_risk <- c(fs_risk,sd(funding_surplus))
			fs_util <- c(fs_util,ce_ref_dependent_util(c(1, 0.44, 4.5, 0.88),funding_ratio))
		}
		
		# Using funding ratio
		max_util <- max(fs_util)
		max_idx <- which.max(fs_util)
		max_plan <- plans[max_idx]
		max_plans <- c(max_plans,max_plan/100.0)

	#     Rets, Risks = plot_util_scores(risk_aversion_degree, 0.9, 1.1, max_util)

		print(paste0("Funding Ratio: ", fr))
		print(paste0("Best plan: ", (max_plan-1)/100.0))
		print(paste0("mean funding ratio: ", fr_avg[max_idx], "; vol: ", fr_risk[max_idx], "; utility: ", max_util))
	}

	plot(funding_ratios, max_plans, type="l", xlab="initial funding ratio", ylab="optimal equity allocation", main="LDI strategy optimization")
	return(max_plans)
}

scenarios <- gbm2_simulations(c(rtn_eq,rtn_bond), c(vol_eq, vol_bond), corr_eq_bond, 1, 0, time_horizon, n_scens, sum_days = 1) #gbm with annual step
write.csv(optimize_allocation(scenarios, n_scens, time_horizon), paste0('max_plans_gbm_step_1yr_',time_horizon,'.csv'))
write.csv(summary(scenarios), paste0('summ_max_plans_gbm_step_1yr_',time_horizon,'.csv'))
write.csv(apply(scenarios,2,sd), paste0('sd_max_plans_gbm_step_1yr_',time_horizon,'.csv'))
write.csv(scenarios, paste0('scen_max_plans_gbm_step_1yr_',time_horizon,'.csv'))

scenarios <- gbm2_simulations(c(rtn_eq,rtn_bond), c(vol_eq+0.03, vol_bond), corr_eq_bond, 1, 0, time_horizon, n_scens, sum_days = 1) #gbm with annual step and 3% increase in equity vol
write.csv(optimize_allocation(scenarios, n_scens, time_horizon), paste0('max_plans_gbm_step_1yr_shocked',time_horizon,'.csv'))
write.csv(summary(scenarios), paste0('summ_max_plans_gbm_step_1yr_shocked',time_horizon,'.csv'))
write.csv(apply(scenarios,2,sd), paste0('sd_max_plans_gbm_step_1yr_shocked',time_horizon,'.csv'))
write.csv(scenarios, paste0('scen_max_plans_gbm_step_1yr_shocked',time_horizon,'.csv'))

scenarios <- gbm2_simulations(c(rtn_eq,rtn_bond), c(vol_eq+0.03, vol_bond), corr_eq_bond, 1/252, 0, time_horizon, n_scens, sum_days = 252) #gbm with daily step and 3% increase in equity vol
write.csv(optimize_allocation(scenarios, n_scens, time_horizon), paste0('max_plans_gbm_step_1day_shocked',time_horizon,'.csv'))
write.csv(summary(scenarios), paste0('summ_max_plans_gbm_step_1day_shocked',time_horizon,'.csv'))
write.csv(apply(scenarios,2,sd), paste0('sd_max_plans_gbm_step_1day_shocked',time_horizon,'.csv'))
write.csv(scenarios, paste0('scen_max_plans_gbm_step_1day_shocked',time_horizon,'.csv'))

mu = 0.118575
kappa = 87.026398
theta = 0.020
sigma = 4.157283
rho_vega = -0.670028
Xi = 0
omega = 0
epsilon = 0
rho_lambda = 0
alpha = 0
delta = 0
vega = 0
rho_z = 0

scenarios <- svj_simulations(mu, kappa, theta+0.03, sigma, rho_vega, Xi, omega, epsilon, rho_lambda, alpha, delta, vega, rho_z, rtn_bond, vol_bond, corr_eq_bond, h=1/252, t =time_horizon, v_init=theta, lambda_init=omega, n_sims=n_scens,seed=123, sum_days=252) #gbm with daily step and 3% increase in equity vol
write.csv(optimize_allocation(scenarios, n_scens, time_horizon), paste0('max_plans_sv_step_1day_shocked',time_horizon,'.csv'))
write.csv(summary(scenarios), paste0('summ_max_plans_sv_step_1day_shocked',time_horizon,'.csv'))
write.csv(apply(scenarios,2,sd), paste0('sd_max_plans_sv_step_1day_shocked',time_horizon,'.csv'))
write.csv(scenarios, paste0('scen_max_plans_sv_step_1day_shocked',time_horizon,'.csv'))

mu = 0.123245
kappa = 8.336307
theta = 0.020
sigma = 1.016718
rho_vega = -0.614862
Xi = 1
omega = 2.126601
epsilon = 0
rho_lambda = 0
alpha = -0.014950
delta = 0.049564
vega = 0
rho_z = 0
scenarios <- svj_simulations(mu, kappa, theta+0.03, sigma, rho_vega, Xi, omega, epsilon, rho_lambda, alpha, delta, vega, rho_z, rtn_bond, vol_bond, corr_eq_bond, h=1/252, t =time_horizon, v_init=theta, lambda_init=omega, n_sims=n_scens,seed=123, sum_days=252) #gbm with daily step and 3% increase in equity vol
write.csv(optimize_allocation(scenarios, n_scens, time_horizon), paste0('max_plans_svj_step_1day_shocked',time_horizon,'.csv'))
write.csv(summary(scenarios), paste0('summ_max_plans_svj_step_1day_shocked',time_horizon,'.csv'))
write.csv(apply(scenarios,2,sd), paste0('sd_max_plans_svj_step_1day_shocked',time_horizon,'.csv'))
write.csv(scenarios, paste0('scen_max_plans_svj_step_1day_shocked',time_horizon,'.csv'))
