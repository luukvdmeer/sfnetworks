#' @importFrom igraph ecount vcount
#' @importFrom sf st_crs
#' @importFrom tibble as_tibble
#' @importFrom tidygraph as_tbl_graph
#' @export
print.sfnetwork = function(x, ...) {
  # Define active and inactive component.
  active = attr(x, "active")
  inactive = if (active == "nodes") "edges" else "nodes"
  # Count number of nodes and edges in the network.
  nN = vcount(x) # Number of nodes in network.
  nE = ecount(x) # Number of edges in network.
  # Print header.
  cat_subtle(c("# A sfnetwork with", nN, "nodes and", nE, "edges\n"))
  cat_subtle("#\n")
  cat_subtle(c("# CRS: ", st_crs(x)$input, "\n"))
  precision = st_precision(x)
  if (precision != 0.0) {
    cat_subtle(c("# Precision: ", precision, "\n"))
  }
  cat_subtle("#\n")
  cat_subtle("#", describe_graph(as_tbl_graph(x)))
  if (has_explicit_edges(x)) {
    cat_subtle(" with spatially explicit edges\n")
  } else {
    cat_subtle(" with spatially implicit edges\n")
  }
  cat_subtle("#\n")
  # Print active data summary.
  active_data = summarise_network_element(
    data = as_tibble(x, active),
    name = substr(active, 1, 4),
    active = TRUE,
    ...
  )
  print(active_data)
  cat_subtle("#\n")
  # Print inactive data summary.
  inactive_data = summarise_network_element(
    data = as_tibble(x, inactive),
    name = substr(inactive, 1, 4),
    active = FALSE,
    ...
  )
  print(inactive_data)
  invisible(x)
}

#' @importFrom sf st_geometry
#' @importFrom tibble trunc_mat
#' @importFrom tools toTitleCase
#' @importFrom utils modifyList
summarise_network_element = function(data, name, active = TRUE,
                                     n_active = getOption("sfn_max_print_active",     6L),
                                     n_inactive = getOption("sfn_max_print_inactive", 3L),
                                     ...
                                     ) {
  # Capture ... arguments.
  args = list(...)
  # Truncate data.
  n = if (active) n_active else n_inactive
  x = do.call(trunc_mat, modifyList(args, list(x = data, n = n)))
  # Write summary.
  x$summary[1] = paste(x$summary[1], if (active) "(active)" else "")
  if (!has_sfc(data) || nrow(data) == 0) {
    names(x$summary)[1] = toTitleCase(paste(name, "data"))
  } else {
    geom = st_geometry(data)
    x$summary[2] = substr(class(geom)[1], 5, nchar(class(geom)[1]))
    x$summary[3] = class(geom[[1]])[1]
    bb = signif(attr(geom, "bbox"), options("digits")$digits)
    x$summary[4] = paste(paste(names(bb), bb[], sep = ": "), collapse = " ")
    names(x$summary) = c(
      toTitleCase(paste(name, "data")),
      "Geometry type",
      "Dimension",
      "Bounding box"
    )
  }
  x
}

#' @importFrom sf st_crs
#' @importFrom utils capture.output
#' @export
print.morphed_sfnetwork = function(x, ...) {
  x_tbg = structure(x, class = setdiff(class(x), "morphed_sfnetwork"))
  out = capture.output(print(x_tbg), ...)
  cat_subtle(gsub("tbl_graph", "sfnetwork", out[[1]]), "\n")
  cat_subtle(out[[2]], "\n")
  cat_subtle(out[[3]], "\n")
  cat_subtle(out[[4]], "\n")
  cat_subtle("# with CRS", st_crs(attr(x, ".orig_graph"))$input, "\n")
  invisible(x)
}

# nocov start

#' Describe graph function for print method
#' From: https://github.com/thomasp85/tidygraph/blob/master/R/tbl_graph.R
#' November 5, 2020
#'
#' @importFrom igraph is_simple is_directed is_bipartite is_connected is_dag
#' gorder
#' @noRd
describe_graph = function(x) {
  if (gorder(x) == 0) return("An empty graph")
  prop = list(
    simple = is_simple(x),
    directed = is_directed(x),
    bipartite = is_bipartite(x),
    connected = is_connected(x),
    tree = is_tree(x),
    forest = is_forest(x),
    DAG = is_dag(x))
  desc = c()
  if (prop$tree || prop$forest) {
    desc[1] = if (prop$directed) "A rooted"
              else "An unrooted"
    desc[2] = if (prop$tree) "tree"
              else paste0(
                "forest with ",
                count_components(x),
                " trees"
              )
  } else {
    desc[1] = if (prop$DAG) "A directed acyclic"
              else if (prop$bipartite) "A bipartite"
              else if (prop$directed) "A directed"
              else "An undirected"
    desc[2] = if (prop$simple) "simple graph"
              else "multigraph"
    n_comp = count_components(x)
    desc[3] = paste0(
      "with ", n_comp, " component",
      if (n_comp > 1) "s" else ""
    )
  }
  paste(desc, collapse = " ")
}

#' @importFrom igraph is_connected is_simple gorder gsize is_directed
is_tree = function(x) {
  is_connected(x) &&
    is_simple(x) &&
    (gorder(x) - gsize(x) == 1)
}

#' @importFrom igraph is_connected is_simple gorder gsize count_components
#' is_directed
is_forest = function(x) {
  !is_connected(x) &&
    is_simple(x) &&
    (gorder(x) - gsize(x) - count_components(x) == 0)
}

# nocov end