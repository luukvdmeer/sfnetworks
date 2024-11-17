#' @export
print.sfnetwork = function(x, ...,
                           n = getOption("sfn_max_print_active", 6),
                           n_non_active = getOption("sfn_max_print_inactive", 3)) {
  N = node_data(x, focused = FALSE)
  E = edge_data(x, focused = FALSE)
  is_explicit = is_sf(E)
  nodes_are_active = attr(x, "active") == "nodes"
  # Print header.
  cat_subtle(c("# A sfnetwork:", nrow(N), "nodes and", nrow(E), "edges\n"))
  cat_subtle("#\n")
  cat_subtle(describe_graph(x, is_explicit), "\n")
  cat_subtle("#\n")
  cat_subtle(describe_space(x, is_explicit), "\n")
  cat_subtle("#\n")
  if (is_focused(x)) {
    if (nodes_are_active) {
      n_focus = length(node_ids(x, focused = TRUE))
      cat_subtle("# Focused on ", n_focus, " nodes\n")
    } else {
      n_focus = length(edge_ids(x, focused = TRUE))
      cat_subtle("# Focused on ", n_focus, " edges\n")
    }
  }
  # Print tables.
  if (nodes_are_active) {
    active_data = N
    active_name = "Node data"
    inactive_data = E
    inactive_name = "Edge data"
  } else {
    active_data = E
    active_name = "Edge data"
    inactive_data = N
    inactive_name = "Node data"
  }
  print(as_named_tbl(active_data, active_name, " (active)"), n = n, ...)
  cat_subtle('#\n')
  print(as_named_tbl(inactive_data, inactive_name), n = n_non_active)
  invisible(x)
}

#' @importFrom utils capture.output
#' @export
print.morphed_sfnetwork = function(x, ...) {
  x_tbg = structure(x, class = setdiff(class(x), "morphed_sfnetwork"))
  out = capture.output(print(x_tbg), ...)
  cat(gsub("tbl_graph", "sfnetwork", out[[1]]), "\n")
  cat(out[[2]], "\n")
  cat(out[[3]], "\n")
  cat(out[[4]], "\n")
  invisible(x)
}

# nocov start

#' @importFrom pillar style_subtle
cat_subtle = function(...) {
  cat(style_subtle(paste0(...)))
}

#' @importFrom tibble as_tibble
as_named_tbl = function(x, name = "A tibble", suffix = "") {
  x = as_tibble(x)
  attr(x, "name") = name
  attr(x, "suffix") = suffix
  class(x) = c("named_tbl", class(x))
  x
}

#' Describe the graph structure of a sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param is_explicit Is the network spatially explicit? If \code{NULL}, this
#' will be automatically inferred from the provided network.
#'
#' @details This function is used by the print method for sfnetwork objects.
#' It is adapted from the interal describe_graph function of tidygraph.
#' See: https://github.com/thomasp85/tidygraph/blob/main/R/tbl_graph.R
#'
#' @return The description of the graph structure as a pasted character.
#'
#' @importFrom igraph is_simple is_directed is_bipartite is_connected is_dag
#' gorder count_components
#' @noRd
describe_graph = function(x, is_explicit = NULL) {
  if (gorder(x) == 0) return("# An empty graph")
  prop = list(
    simple = is_simple(x),
    directed = is_directed(x),
    bipartite = is_bipartite(x),
    connected = is_connected(x),
    tree = is_tree(x),
    forest = is_forest(x),
    DAG = is_dag(x),
    explicit = if (is.null(is_explicit)) has_explicit_edges(x) else is_explicit
  )
  n_comp = count_components(x)
  desc = c()
  if (prop$tree || prop$forest) {
    if (prop$directed) {
      desc[1] = "A rooted"
    } else {
      desc[1] = "An unrooted"
    }
    if (prop$tree) {
      desc[2] = "tree"
      if (prop$explicit) {
        desc[3] = "with spatially explicit edges"
      } else {
        desc[3] = "with spatially implicit edges"
      }
    } else {
      desc[2] = paste0("forest with ", n_comp, " trees")
      if (prop$explicit) {
        desc[3] = "and spatially explicit edges"
      } else {
        desc[3] = "and spatially implicit edges"
      }
    }
  } else {
    if (prop$DAG) {
      desc[1] = "A directed acyclic"
    } else if (prop$bipartite) {
      desc[1] = "A bipartite"
    } else if (prop$directed) {
      desc[1] = "A directed"
    } else {
      desc[1] = "An undirected"
    }
    if (prop$simple) {
      desc[2] = "simple graph"
    } else {
      desc[2] = "multigraph"
    }
    if (n_comp > 1) {
      desc[3] = paste0("with ", n_comp, " components")
    } else {
      desc[3] = paste0("with ", n_comp, " component")
    }
    if (prop$explicit) {
      desc[4] = "and spatially explicit edges"
    } else {
      desc[4] = "and spatially implicit edges"
    }
  }
  paste(c("#", desc), collapse = " ")
}

#' @importFrom igraph is_connected is_simple gorder gsize
is_tree = function(x) {
  is_connected(x) &&
    is_simple(x) &&
    (gorder(x) - gsize(x) == 1)
}

#' @importFrom igraph is_connected is_simple gorder gsize count_components
is_forest = function(x) {
  !is_connected(x) &&
    is_simple(x) &&
    (gorder(x) - gsize(x) - count_components(x) == 0)
}

#' Describe the spatial structure of a sfnetwork
#'
#' @param x An object of class \code{\link{sfnetwork}}.
#'
#' @param is_explicit Is the network spatially explicit? If \code{NULL}, this
#' will be automatically inferred from the provided network.
#'
#' @details This function is used by the print method for sfnetwork objects.
#' It is adapted from the print method for sfc objects in sf.
#' See: https://github.com/r-spatial/sf/blob/main/R/sfc.R
#'
#' @return The description of the spatial structure as a pasted character.
#'
#' @importFrom sf st_crs
#' @noRd
describe_space = function(x, is_explicit = NULL) {
  explicit = if (is.null(is_explicit)) has_explicit_edges(x) else is_explicit
  node_geom = pull_node_geom(x)
  edge_geom = if (explicit) pull_edge_geom(x) else NULL
  desc = c()
  # Dimensions.
  if (length(node_geom)) {
    desc = append(desc, paste("# Dimension:", class(node_geom[[1]])[1]))
  }
  # Bounding box.
  if (explicit) {
    box = merge_bboxes(attr(node_geom, "bbox"), attr(edge_geom, "bbox"))
  } else {
    box = attr(node_geom, "bbox")
  }
  box = signif(box, options("digits")$digits) # Round values.
  box = paste(paste(names(box), box[], sep = ": "), collapse = " ") # Unpack.
  desc = append(desc, paste("# Bounding box:", box))
  # Z range.
  if(! is.null(attr(node_geom, "z_range"))) {
    if (explicit) {
      zr = merge_zranges(attr(node_geom, "z_range"), attr(edge_geom, "z_range"))
    } else {
      zr = attr(node_geom, "z_range")
    }
    zr = signif(zr, options("digits")$digits) # Round values.
    zr = paste(paste(names(zr), zr[], sep = ": "), collapse = " ")
    desc = append(desc, paste("# Z range:", zr))
  }
  # M range.
  if(! is.null(attr(node_geom, "m_range"))) {
    if (explicit) {
      mr = merge_mranges(attr(node_geom, "m_range"), attr(edge_geom, "m_range"))
    } else {
      mr = attr(node_geom, "m_range")
    }
    mr = signif(mr, options("digits")$digits) # Round values.
    mr = paste(paste(names(mr), mr[], sep = ": "), collapse = " ")
    desc = append(desc, paste("# M range:", mr))
  }
  # CRS.
  crs = st_crs(node_geom)
  if (is.na(crs)) {
    desc = append(desc, paste("# CRS: NA"))
  } else {
    name = get_crs_name(crs)
    if (crs$IsGeographic) {
      desc = append(desc, paste("# Geodetic CRS:", name))
    }
    else {
      desc = append(desc, paste("# Projected CRS:", name))
    }
  }
  # Precision.
  prc = network_precision(x)
  if (prc < 0.0) {
    desc = append(desc, paste("# Precision: float (single precision)"))
  } else if (prc > 0.0) {
    desc = append(desc, paste("# Precision:", prc))
  }
  paste(desc, collapse = "\n")
}

get_crs_name = function(crs) {
  if (is.na(crs)) return(NA)
  name = crs$Name
  if (name == "unknown") {
    input = crs$input
    if (is.character(input) && !is.na(input) && input != "unknown") {
      name = input
    } else {
      name = crs$proj4string
    }
  }
  name
}

# nocov end