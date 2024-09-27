#' @importFrom igraph as_edgelist is_directed
sfnetwork_to_minimal_dodgr = function(x, weights, direction = "out") {
  edgelist = as_edgelist(x, names = FALSE)
  if (inherits(weights, "dual_weights")) {
    dual = TRUE
    d = weights$reported
    w = weights$actual
  } else {
    dual = FALSE
    d = weights
  }
  if (!is_directed(x) | direction == "all") {
    x_dodgr = data.frame(
      from = as.character(c(edgelist[, 1], edgelist[, 2])),
      to = as.character(c(edgelist[, 2], edgelist[, 1])),
      d = rep(d, 2)
    )
    if (dual) x_dodgr$w = rep(w, 2)
  } else {
    if (direction == "out") {
      x_dodgr = data.frame(
        from = as.character(edgelist[, 1]),
        to = as.character(edgelist[, 2]),
        d = d
      )
    } else if (direction == "in") {
      x_dodgr = data.frame(
        from = as.character(edgelist[, 2]),
        to = as.character(edgelist[, 1]),
        d = d
      )
    } else {
      raise_unknown_input("direction", direction, c("out", "in", "all"))
    }
    if (dual) x_dodgr$w = w
  }
  x_dodgr
}