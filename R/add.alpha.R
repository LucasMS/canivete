#' Add transparency to colors
#'
#' This function allows you to add transparancy to colors.
#' @keywords
#' colors, transparency, alpha
#' @export
#' @examples
#' add.alpha("black", 0.5)
#' @references
#' add.alpha funciton was made by Markus Gesmann github.com/mages, at http://www.magesblog.com/2013/04/how-to-change-alpha-value-of-colours-in.html



add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2,
        function(x)
          rgb(x[1], x[2], x[3], alpha=alpha))
}
