#' Get the specified summary function for an attribute column.
#'
#' @param attr Name of the attribute.
#'
#' @param spec Specification of the summary function belonging to each
#' attribute.
#'
#' @return A function that takes a vector of attribute values as input and
#' returns a single value.
#'
#' @noRd
get_summary_function = function(attr, spec) {
  if (!is.list(spec)) {
    func = spec
  } else {
    names = names(spec)
    if (is.null(names)) {
      func = spec[[1]]
    } else {
      func = spec[[attr]]
      if (is.null(func)) {
        default = which(names == "")
        if (length(default) > 0) {
          func = spec[[default[1]]]
        } else {
          func = "ignore"
        }
      }
    }
  }
  if (is.function(func)) {
    func
  } else {
    summariser(func)
  }
}

#' @importFrom stats median
#' @importFrom utils head tail
summariser = function(name) {
  switch(
    name,
    ignore = function(x) NA,
    sum = function(x) sum(x),
    prod = function(x) prod(x),
    min = function(x) min(x),
    max = function(x) max(x),
    random = function(x) sample(x, 1),
    first = function(x) head(x, 1),
    last = function(x) tail(x, 1),
    mean = function(x) mean(x),
    median = function(x) median(x),
    concat = function(x) c(x),
    raise_unknown_summariser(name)
  )
}
