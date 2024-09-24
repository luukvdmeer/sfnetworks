#' Summarize attribute values into a single value
#'
#' @param attrs A named list with each element containing the values of an
#' attribute. To obtain this from a network object, call
#' \code{\link[igraph]{vertex_attrs}} or \code{\link[igraph]{edge_attrs}}.
#'
#' @param summary Specification of how attributes should be summarized. There
#' are several options, see \code{\link[igraph]{igraph-attribute-combination}}
#' for details.
#'
#' @param subset Integer vector specifying which rows should be summarized.
#' If \code{NULL}, all provided values are summarized.
#'
#' @returns A named list with each element containing the summarized values
#' of the provided attributes.
#'
#' @noRd
summarize_attributes = function(attrs, summary = "concat", subset = NULL) {
  if (! is.null(subset)) attrs = lapply(attrs, `[`, subset)
  names = names(attrs)
  summarizers = lapply(names, get_summary_function, summary)
  out = mapply(\(x, f) f(x), attrs, summarizers, SIMPLIFY = FALSE)
  names(out) = names
  out
}

#' Get the specified summary function for an attribute column.
#'
#' @param attr Name of the attribute.
#'
#' @param summary Specification of how attributes should be summarized. There
#' are several options, see \code{\link[igraph]{igraph-attribute-combination}}
#' for details.
#'
#' @return A function that takes a vector of attribute values as input and
#' returns a single value.
#'
#' @noRd
get_summary_function = function(attr, summary) {
  if (!is.list(summary)) {
    func = summary
  } else {
    names = names(summary)
    if (is.null(names)) {
      func = summary[[1]]
    } else {
      func = summary[[attr]]
      if (is.null(func)) {
        default = which(names == "")
        if (length(default) > 0) {
          func = summary[[default[1]]]
        } else {
          func = "ignore"
        }
      }
    }
  }
  if (is.function(func)) {
    func
  } else {
    summarizer(func)
  }
}

#' @importFrom stats median
#' @importFrom utils head tail
summarizer = function(name) {
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
    raise_unknown_summarizer(name)
  )
}
