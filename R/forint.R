#' Formats numbers as Hungarian Forints
#' @param x number
#' @return string
#' @export
#' @importFrom checkmate assert_numeric
#' @importFrom scales dollar
#' @examples
#' forint(42)
#' forint(10000.42365)
#' forint(1:10)
forint <- function(x){
  assert_numeric(x)
  dollar(x, prefix = "", suffix = " HUF")
}
