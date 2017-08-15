library(magrittr)

#Load the whole database (5 years) as provided by https://www.kaggle.com/camnugent/sandp500
data('SP500_base.rda', package= 'SP500R', envir=environment())


#' Return stocks' names
#'
#' Return the name of all available stocks
#'
#' @return List of characters, all the names available
#'
#' @examples
#' SP500_names()
#'
#' @import dplyr
#' @import magrittr
#'
#' @export
SP500_names <- function(){
  names_df <- SP500_base %>% dplyr::select(Name) %>% unique()
  as.vector(names_df$Name)}


#' Load the whole dataframe
#'
#' Simple function that will return the whole dataframe, as provided on Kaggle
#'
#' @return The data, as a dataframe
#'
#' @examples
#' SP500_names()
#'
#' @import dplyr
#'
#' @export
all_stocks <- function(){
  SP500_base
}

#' Return one stock's values
#'
#' Return one stock's values, as a dataframe. The user should provide the name of the stock, and possibly the type of value, and a starting and ending date
#'
#' @param name String value, the name of the stock. Use SP500_names for possible names
#' @param var_type String value, the name of the variabe to return, could be 'Open', 'Close', 'High', 'Low' or 'Volume'. Default is 'Open'
#' @param start Date value, the date when the serie should be started. The user should use package 'lubridate'
#' @param end Date value, the date when the serie should be ended. The user should use package 'lubridate'
#' @return This function returns the data in a dataframe with 'Date' and 'var_type' columns.
#'
#' @examples
#' one_stock.df('TSS')
#' one_stock.df('RMD','High',start=lubridate::ymd('2014-01-01'), end=lubridate::ymd('2015-01-01'))
#'
#' @import dplyr
#' @import magrittr
#'
#' @export
one_stock.df <- function(name, var_type='Open', start = NULL, end = NULL){
  #Check if the arguments provided are correct
  if(!(var_type %in% c('Open','High','Low','Close','Volume'))){
    stop("Type must be in : Open, High, Low, Close, Volume")
  }
  if(!is.null(start) & !is(start,"Date")){stop("'start' argument should be a date")}
  if(!is.null(end) & !is(end,"Date")){stop("'end' argument should be a date")}
  if(!(name %in% SP500_names())){stop("name argument is not a SP500 stocks")}
  #Filter for the right name
  tmp <- SP500_base %>% dplyr::filter(Name == name) %>% dplyr::select('Date', var_type)
  #Filter according to the starting and ending date, if provided
  if(!is.null(start)) tmp <- dplyr::filter(tmp, Date > start)
  if(!is.null(end)) tmp <- dplyr::filter(tmp, Date < end)

  return(tmp)}

#' Return one stock's values
#'
#' Return one stock's values, as a time serie. The user should provide the name of the stock, and possibly the type of value, and a starting and ending date
#'
#' @param name String value, the name of the stock. Use SP500_names for possible names
#' @param var_type String value, the name of the variabe to return, could be 'Open', 'Close', 'High', 'Low' or 'Volume'. Default is 'Open'
#' @param start Date value, the date when the serie should be started. The user should use package 'lubridate'
#' @param end Date value, the date when the serie should be ended. The user should use package 'lubridate'
#' @return This function returns the data as a time serie, with correct starting and ending time, and right frequency.
#'
#' @examples
#' one_stock.ts('TSS')
#' one_stock.ts('RMD','High',start=lubridate::ymd('2014-01-01'), end=lubridate::ymd('2015-01-01'))
#'
#' @import dplyr
#' @import magrittr
#' @import lubridate
#'
#' @export
one_stock.ts <- function(name, var_type='Open', start = NULL, end = NULL){
  stock <- one_stock.df(name, var_type, start, end)
  current_year <- lubridate::year(stock$Date[1])
  n_days_to_next_year <- days_to_next_year(stock)
  ts(stock[var_type], frequency=252,
     start=c(current_year, 252 - n_days_to_next_year + 1))}

#' Return one stock's values
#'
#' Return one stock's values and their lags, as a matrix .
#' Usefull for auto-regressive model. The first element of each row will be the stock's value, and the other elements will be the lagged values (to regress over).
#' The user should provide the name of the stock, the number of lags needed and possibly the type of value, and a starting and ending date
#'
#' @param name String value, the name of the stock. Use SP500_names for possible names
#' @param var_type String value, the name of the variabe to return, could be 'Open', 'Close', 'High', 'Low' or 'Volume'. Default is 'Open'
#' @param lags Integer value, the number of lags to include for each row. Default is zero.
#' @param start Date value, the date when the serie should be started. The user should use package 'lubridate'
#' @param end Date value, the date when the serie should be ended. The user should use package 'lubridate'
#' @return This function returns the data as a time serie, with correct starting and ending time, and right frequency.
#'
#' @examples
#' one_stock.mtx('TSS',lags=3)
#' one_stock.mtx('RMD',lags=6,'High',start=lubridate::ymd('2014-01-01'), end=lubridate::ymd('2015-01-01'))
#'
#' @import dplyr
#' @import magrittr
#' @import lubridate
#'
#' @export
one_stock.mtx <- function(name, var_type='Open', lags=0, start = NULL, end = NULL){
  if(!is(lags,'numeric')) stop(paste('Lag must be a numeric, was provided',lags))
  else if(lags %% 1 != 0) stop(paste('Lag must be an integer, was provided', lags))
  # Create the matrix of the stock's prices and their lags
  stock <- one_stock.df(name, var_type, start, end)
  tmp <- sapply(1:(nrow(stock)-lags), function(t) stock[t:(t+lags), var_type])
  # If we have lags=0, sapply will return an object with no dimention. We correct that:
  if(lags==0) dim(tmp) <- c(1,nrow(stock)-lag)
  # We transpose the matrix to respect R conventions on model's inputs
  tmp <- t(tmp)
  # Change the col and row names for more clarity
  rownames(tmp) <- as.character(stock[1:(nrow(stock)-lags), 'Date'])
  colnames(tmp) <- sapply(1:(lags+1) - 1,function(t){ifelse(t==0, "X_t", paste0('X_(t-',t ,")"))})
  return(tmp)
}

# Return how many days are left before the next year. Useful for conversion to time serie
days_to_next_year <- function(stock){
  current_year <- lubridate::year(stock$Date[1])
  tmp <- stock %>% dplyr::mutate(year = lubridate::year(Date)) %>% dplyr::group_by(year) %>%
    dplyr::summarize(nObs = n()) %>% dplyr::filter(year == current_year)
  as.numeric(tmp$nObs)
}
