#' F is for Fun
#'
#' @return value
#' @export
#'
f <- function() {10}

#' G is great
#'
#' @param x a number
#'
#' @return a value
#' @export
#'
g <- function(x) {return(x + f())}


#' H is the place
#'
#' @return a value
#' @export
#'
#' @importFrom foreach %dopar%
h <- function() {
    n_cores <- parallel::detectCores()

    cl <- parallel::makeCluster(n_cores - 1)
    doParallel::registerDoParallel(cl)

    output <- foreach::foreach(
        jj = 1:1000,
        .combine = dplyr::bind_rows,
        .export = c("g", "f"),
        .packages = c("tibble")

    ) %dopar% {
        tibble::tibble(iter = jj, value = g(jj))
    }

    doParallel::stopCluster(cl)
    output
}
