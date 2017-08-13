library(magrittr)

#Load the whole database (5 years) as provided by https://www.kaggle.com/camnugent/sandp500
data('SP500_base.Rda', envir=environment())

SP500_names <- function(){
  names_df <- SP500_base %>% dplyr::select(Name) %>% unique()
  as.vector(names_df$Name)}

all_stocks <- function(){
  SP500_base
}

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
