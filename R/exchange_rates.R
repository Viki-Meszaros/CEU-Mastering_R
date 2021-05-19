#' Returns the current exchange rate from USD to HUF
#' @param retried number of times the function already failed
#' @return number
#' @export
#' @importFrom checkmate assert_number
#' @importFrom jsonlite fromJSON
#' @importFrom logger log_info log_error
#' @examples
#' get_usdhuf()
get_usdhuf <- function(retried = 0){
  tryCatch({
    rate <- fromJSON('https://api.exchangerate.host/latest?base=USD&symbols=HUF')$rates$HUF
    assert_number(rate, lower = 250, upper = 400)
  }, error = function(e) {
    log_error(e$message)
    Sys.sleep(1 + retried ^ 2)
    get_usdhuf(retried = retried + 1)
  })

  log_info("1 USD={rate} HUF")
  rate
}


#' Returns the current exchange rate from USD to HUF
#' @param start_date date
#' @param end_date date
#' @inheritParams get_usdhuf
#' @return \code{data.table} object
#' @export
#' @importFrom checkmate assert_number
#' @importFrom jsonlite fromJSON
#' @importFrom logger log_info log_error
#' @importFrom data.table data.table
#' @importFrom httr GET content
get_usdhufs <- function(start_date = Sys.Date() - 30, end_date = Sys.Date(), retried = 0) {
tryCatch({
  response <- GET(
    'https://api.exchangerate.host/timeseries',
    query = list(
      start_date = start_date,
      end_date = end_date,
      base='USD',
      symbols='HUF'
    )
  )
  exchange_rates <- content(response)$rates

  usdhufs <- data.table(
    date = as.Date(names(exchange_rates)),
    usdhuf = as.numeric(unlist(exchange_rates)))
  assert_numeric(usdhufs$usdhuf, lower = 250, upper = 400)

  }, error = function(e) {
  log_error(e$message)
  Sys.sleep(1 + retried ^ 2)
  get_usdhufs(start_date = start_date, end_date = end_date, retried = retried + 1)
})

  usdhufs

}



#' Returns the current exchange rate between any currencies
#' @param start_date date
#' @param end_date date
#' @param base_currency string
#' @param symbol_currency string
#' @inheritParams get_usdhuf
#' @return \code{data.table} object
#' @export
#' @importFrom logger log_info log_error
#' @importFrom data.table data.table
#' @importFrom httr GET content
get_exchange_rates <- function(base_currency, symbol_currency, start_date = Sys.Date() -30, end_date = Sys.Date(), retried = 0) {
  tryCatch({
    response <- GET(
      'https://api.exchangerate.host/timeseries',
      query = list(
        start_date = start_date,
        end_date = end_date,
        base = base_currency,
        symbols = symbol_currency
      )
    )
    exchange_rates <- content(response)$rates

    rates <- data.table(
      date = as.Date(names(exchange_rates)),
      rate = as.numeric(unlist(exchange_rates)))

  }, error = function(e) {
    log_error(e$message)
    Sys.sleep(1 + retried ^ 2)
    get_exchange_rates(start_date = start_date, end_date = end_date, retried = retried + 1)
  })

  rates

}


