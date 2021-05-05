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
