#' @title par10 Function
#'
#' @description This function reads a CSV file containing a matrix of values and modifies it as follows:
#' If a value in the matrix is 0, it is replaced by 10 times the maximum non-zero value in the corresponding row.
#'
#' @param G_file The path to the CSV file containing the matrix of values.
#' @param has_header Logical, indicating if the CSV file has a header row. Default is FALSE.
#'
#' @return A matrix where each 0 value is replaced by 10 times the maximum non-zero value in its corresponding row.
#'
#' @examples
#'
#' par10_result <- par10(system.file("extdata", "R.csv", package = "ppRank"), has_header = TRUE)
#'
#'
#' @export

par10 <- function(G_file, has_header = FALSE) {

  G <- read.csv(G_file, header = has_header)[,-1]
  names <- read.csv(G_file, header = has_header)[,1]

  m <- nrow(G)
  n <- ncol(G)

  A <- matrix(1, nrow = m, ncol = n)

  for (i in 1:m) {

    for (j in 1:n) {

      max_val <- ifelse(all(G[i,] == 0), 0, max(G[i, G[i,] != 0]))

      A[i, j] <- G[i, j]

      if (A[i, j] == 0) {
        A[i, j] <- max_val * 10
      }
    }
  }

  return(A)
}
