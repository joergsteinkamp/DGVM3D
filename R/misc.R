#' ranking without gaps with duplicate values
#'
#' @param x the input to be ranked
#' @return the ranked data
#' @author Joerg Steinkamp \email{steinkamp.joerg@@gmail.com}
#' @export
#' @examples
#' set.seed(456789)
#' x = sample(1:20, 12)
#' x = append(x, x[sample(1:12, 8, replace=TRUE)])
#' ## equal results
#' gapless.rank(x)
#' as.numeric(factor(x))
#' ## However, this here keeps the shape
#' gapless.rank(matrix(x, 4, 5))
#' as.numeric(factor(matrix(x, 4, 5)))
gapless.rank <- function(x) {
  u <- sort(unique(as.vector(x)))
  x.ranked = x
  for (i in 1:length(u)) {
    x.ranked[x==u[i]] = i
  }
  return(x.ranked)
}
