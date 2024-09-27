#' @importFrom igraph as_edgelist is_directed
sfnetwork_to_minimal_dodgr = function(x, weights, direction = "out") {
  edgelist = as_edgelist(x, names = FALSE)
  if (!is_directed(x) | direction == "all") {
    x_dodgr = data.frame(
      from = as.character(c(edgelist[, 1], edgelist[, 2])),
      to = as.character(c(edgelist[, 2], edgelist[, 1])),
      d = rep(weights, 2)
    )
  } else {
    if (direction == "out") {
      x_dodgr = data.frame(
        from = as.character(edgelist[, 1]),
        to = as.character(edgelist[, 2]),
        d = weights
      )
    } else if (direction == "in") {
      x_dodgr = data.frame(
        from = as.character(edgelist[, 2]),
        to = as.character(edgelist[, 1]),
        d = weights
      )
    } else {
      raise_unknown_input("direction", direction, c("out", "in", "all"))
    }
  }
  x_dodgr
}