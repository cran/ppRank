#' Bilex Function
#'
#' This function reads two CSV files containing matrices of values (T_file and R_file) and applies a transformation to matrix R according to certain rules.
#'
#' @param R_file The filename of the CSV file containing the matrix R (located in inst/extdata).
#' @param T_file The filename of the CSV file containing the matrix T (located in inst/extdata).
#' @param has_header Logical, indicating if the CSV files have a header row. Default is FALSE.
#'
#' @return A modified matrix R according to the specified rules.
#'
#' @details
#' This function performs a transformation on matrix R based on the following rules:
#' - If a value in R is 0, it is replaced by a classification value.
#' - Classification values are determined based on comparisons between neighboring values in R and corresponding values in T.
#' - The classification of each value in R depends on the values in the same row of R, considering both numerical values and the relationship with neighboring values.
#'
#' @examples
#'
#' bilex_result <- bilex(system.file("extdata", "R.csv", package = "ppRank"),
#' system.file("extdata", "T.csv", package = "ppRank"), has_header = TRUE)
#'
#'
#' bilex_result <- bilex(system.file("extdata", "R.csv", package = "ppRank"),
#' system.file("extdata", "T.csv", package = "ppRank"))
#'
#'
#' @export

bilex <- function(R_file, T_file, has_header = FALSE) {

  T <- read.csv(T_file, header = has_header)[, -1]
  R <- read.csv(R_file, header = has_header)[, -1]

  names <- read.csv(R_file, header = has_header)[, 1]

  m <- nrow(R)
  n <- ncol(R)


  A <- matrix(1, nrow = m, ncol = n)
  rownames(A) <- names

  for (i in 1:m) {
    maior <- 1

    for (j in 2:n) {

      if (n == 1) {
        A[i, j] <- 1.0
      } else {

        if (R[i, j-1] == 0.0) {
          if (R[i, j] != 0.0) {
            maior <- maior + 1
            A[i, j] <- A[i, j-1]
            A[i, j-1] <- maior

            for (k in 1:(j-1)) {
              if (R[i, k] == 0.0) {
                A[i, k] <- maior
              }
            }
          } else {
            A[i, j] <- maior

            for (k in 1:(j-1)) {
              if (R[i, k] == 0.0) {
                A[i, k] <- maior
              }
            }
          }


        } else if (R[i, j-1] < R[i, j]) {
          maior <- maior + 1
          for (k in 1:(j-1)) {
            if (R[i, k] == 0) {
              A[i, k] <- maior
            } else if (R[i, k] == R[i, j]) {
              A[i, j] <- A[i, k]
              maior <- maior + 1
            } else if (R[i, k] > R[i, j]) {
              A[i, k] <- A[i, k] + 1
            } else if (R[i, k] < R[i, j] && R[i, k] != 0.0) {
              A[i, j] <- A[i, j] + 1
            }
          }

          if(A[i, j] == maior) {
            maior <- maior +1
            for(k in 1:j){
              if(R[i,k] == 0) {
                A[i,k] <- maior
              }
            }
          }


        } else if (R[i, j-1] == R[i, j]) {
          if (T[i, j-1] < T[i, j]) {
            A[i, j] <- A[i, j-1] + 1
          } else if (T[i, j-1] > T[i, j]) {
            A[i, j] <- A[i, j-1]
            A[i, j-1] <- A[i, j] + 1
          } else {
            A[i, j] <- A[i, j-1]
          }
          maior <- maior + 1

        } else if (R[i, j] == 0.0) {
          A[i, j] <- maior


        } else if (R[i, j-1] > R[i, j]) {

          for (k in 1:(j-1)) {
            if (R[i, k] > R[i, j]) {
              A[i, k] <- A[i, k] + 1
            } else if (R[i, k] < R[i, j]) {
              A[i, j] <- A[i, j] + 1
            } else if (R[i, k] == R[i, j]) {
              A[i, j] <- A[i, k]
            }
          }
        }

        if (j == n) {
          ranks <- rep(-1, n)
          for (k in 1:n) {
            for(l in 1:n) {
              if(A[i,k] == l) {
                ranks[l] <- ranks[l] +1
              }
            }
          }
          for(k in 1:n) {
            for(l in 1:n){
              if (A[i,k] == l && ranks[l] >= 1){
                A[i,k] <- A[i,k] + ranks[l]*0.5
              }
            }
          }
        }
      }

    }
  }

  return(A)

}
