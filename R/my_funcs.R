#' F is for Fun
#'
#' @return value
#' @export
#'
f <- function() {10}

#' G is great
#'
#' @param x a number
#' @param ... many funcs?
#'
#' @return a value
#' @export
#'
g <- function(x, ...) {
    funcs <- list(...)
    if (length(funcs) > 0) {
        list2env(funcs, environment())
    }
    return(x + k(f = f))
}

#' K bc i and j are for iteration
#'
#' @param ... many funcs?
#'
#' @return a value
#' @export
#'
k <- function(...) {
    funcs <- list(...)
    if (length(funcs) > 0) {
        list2env(funcs, environment())
    }
    return(f())
}


#' H is the place
#'
#' @return a value
#' @export
#'
#' @importFrom foreach %dopar%
h <- function(iter = 1000, verbose = FALSE) {
    n_cores <- parallel::detectCores()

    cl <- parallel::makeCluster(n_cores - 1)
    doParallel::registerDoParallel(cl)


    output <- foreach::foreach(
        jj = seq_len(iter),
        .combine = dplyr::bind_rows,
        .multicombine = TRUE,
        .export = c("g", "f", "k"),
        .packages = c("tibble"),
        .verbose = verbose
    ) %dopar% {
        tibble::tibble(iter = jj, value = g(jj, f = f, k = k))
    }

    parallel::stopCluster(cl)
    output
}
