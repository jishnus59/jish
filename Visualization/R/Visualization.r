## Author: Rajesh Jakhotia
## Company Name: K2 Analytics Finishing School Pvt. Ltd
## Email : ar.jakhotia@k2analytics.co.in
## Website : k2analytics.co.in

# List of Libraries
library(data.table)
library(scales)

## deciling code
#' deciling function
#'
#' Takes the data and divide in to 10 parts.
#' @author Rajesh Jakhotia
#' @param x A numeric variable
#' @return It will give back series of numbers, in which decile the observation goes to.
#' @example DEC_exam.R
#' @export
decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
    ifelse(x<deciles[2], 2,
    ifelse(x<deciles[3], 3,
    ifelse(x<deciles[4], 4,
    ifelse(x<deciles[5], 5,
    ifelse(x<deciles[6], 6,
    ifelse(x<deciles[7], 7,
    ifelse(x<deciles[8], 8,
    ifelse(x<deciles[9], 9, 10
    ))))))))))
}

## set the working directory of folder to dump the output
## compile the function

## Visualization code
#' Visualization function
#'
#' Takes the data and divid in to 10 parts and looks the target distribution.
#' @author Rajesh Jakhotia
#' @param x  varibale need to look the target distribution
#' @return It will give back you the deciled data with target disribution
#' @note set the working directory of folder to dump the output and set the variable name as col_list
#' @example vis_exam.R
#' @export

Visualization <- function(df, target, var)
{
  
  tmp <- df[, c(var , target)]
  colnames(tmp)[1] = "Xvar"
  colnames(tmp)[2] = "Target"
  
  
  tmp$deciles <- decile(tmp$Xvar)
  
  
  tmp_DT = data.table(tmp)
  
  RRate <- tmp_DT[, list(
    min_ = min(Xvar), max_ = max(Xvar), avg_ = mean(Xvar),
    cnt = length(Target), cnt_resp = sum(Target), 
    cnt_non_resp = sum(Target == 0)
  ) , 
  by=deciles][order(deciles)]
  
  RRate$range = paste(RRate$min_ , RRate$max_ , sep = " to ");
  RRate$prob <- round(RRate$cnt_resp / RRate$cnt,2);
  
  setcolorder(RRate, c(1, 8, 2:7, 9))
  
  
  RRate$cum_tot <- cumsum(RRate$cnt)
  RRate$cum_resp <- cumsum(RRate$cnt_resp)
  RRate$cum_non_resp <- cumsum(RRate$cnt_non_resp)
  RRate$cum_tot_pct <- round(RRate$cum_tot / sum(RRate$cnt),2);
  RRate$cum_resp_pct <- round(RRate$cum_resp / sum(RRate$cnt_resp),2);
  RRate$cum_non_resp_pct <- round(RRate$cum_non_resp / sum(RRate$cnt_non_resp),2);
  RRate$ks <- abs(RRate$cum_resp_pct - RRate$cum_non_resp_pct);
  
  RRate$prob = percent(RRate$prob)
  RRate$cum_tot_pct = percent(RRate$cum_tot_pct)
  RRate$cum_resp_pct = percent(RRate$cum_resp_pct)
  RRate$cum_non_resp_pct = percent(RRate$cum_non_resp_pct)
  
  View(RRate)
}



#' Power Function
#' 
#' Takes numeric in puts x & y
#' @author  Jishnu
#' @param x numeric variable
#' @param y numeric variable
#' @return x^y 
#' @example example.R
#' @export
Power<- function(x,y){
  return(x^y)
}
#' Squire Function
#' 
#' Takes numeric in puts x 
#' @author  Jishnu
#' @param x numeric variable
#' @return x^2 
#' @example power_example.R
#' @export
Squire<-function(x){
  return(x^2)
}

#' Cubic Function
#' 
#' Takes numeric in puts x 
#' @author  Jishnu
#' @param x numeric variable
#' @return x^3 
#' @example Cube.R
#' @export
Cube<-function(x){
  return(x^3)
}

