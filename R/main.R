library(magrittr)

#Load the whole database (5 years) as provided by https://www.kaggle.com/camnugent/sandp500
data('SP500_base.Rda', envir=environment())


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
#' Return one stock's value, as a dataframe. The user should provide the name of the stock, and possibly the variable, and a starting and ending date
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
#' Return one stock's value, as a time serie. The user should provide the name of the stock, and possibly the variable, and a starting and ending date
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


# Return how many days are left before the next year. Useful for conversion to time serie
days_to_next_year <- function(stock){
  current_year <- lubridate::year(stock$Date[1])
  tmp <- stock %>% dplyr::mutate(year = lubridate::year(Date)) %>% dplyr::group_by(year) %>%
    dplyr::summarize(nObs = n()) %>% dplyr::filter(year == current_year)
  as.numeric(tmp$nObs)
}
