#' Try running code block until it succeeds or n.tries times, in which case it issues a stop()
#'
#' @param code expression (or multiple lines wrapped in {} braces) to attempt to evaluate.
#' @param n.tries how many times to attempt running the code before giving up. Default of 3
#' @param retry.delay number of seconds to delay between attempts. Defaults to a random number between 1 and 4.
#'
#' @return value produced by code
#' @export
retry_with_throttle <- function(code, n.tries = 3, retry.delay = runif(1, 1, 4)) {
  attempts <- 1

  # run the code we've been provided n.tries times before giving up
  while (attempts <= n.tries) {
    attempts <- attempts + 1

    # run code. Suppress exactly 1 warning
    #
    # this requires both tryCatch() and withCallingHandlers()
    #
    # tryCatch() will evaluate code and handle success/warning/error. However it handles it IMMEDIATELY
    # and cancels all further execution of code
    #
    # This means if we catch the "warning" then the rest of the code does not execute. That's bad
    #
    # withCallingHandlers() will evaluate the code and handle success/warning/error. However it DOES NOT EVER
    # cancel further execution of code. This means it can be used to e.g., log errors but not prevent them from
    # "bubbling up" to a program stop().
    #
    # So we combine them!
    # - use withCallingHandlers() to selectively suppress warnings using invokeRestart("muffleWarning")
    # - use tryCatch() to catch errors so that we can re-try the function
    #
    # see also
    # https://cran.r-project.org/web/packages/tryCatchLog/vignettes/tryCatchLog-intro.html
    res <- tryCatch(
      withCallingHandlers(
        code,
        warning = function(w) {
          if (w$message == "restarting interrupted promise evaluation") {
            # suppress the warning that triggered this handler
            # if not called the warning will "bubble up" and be visible to the user
            invokeRestart("muffleWarning")
          }
        }
      ),
      error = function(e) {
        e # this value has the class "simpleError"
      }
    )

    # Python code called from reticulate returns an exception with classes "Rcpp::exception", "C++Error", "error", and "condition"
    # check only for "error"
    if (!inherits(res, c("simpleError", "error"))) {
      return(res)
    } else {
      Sys.sleep(retry.delay)
    }
  }
  stop(sprintf("Failed to run after %s tries with retry.delay of %s", n.tries, retry.delay))
}
