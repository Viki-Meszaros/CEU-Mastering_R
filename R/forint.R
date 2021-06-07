#' Formats numbers as Hungarian Forints
#' @param x number
#' @return string
#' @export
#' @importFrom checkmate assert_number
#' @importFrom scales dollar
#' @examples
#' forint(42)
#' forint(10000.42365)
forint <- function(x){
  assert_number(x)
  dollar(x, prefix = "", suffix = " HUF")
}
