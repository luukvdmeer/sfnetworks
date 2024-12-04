igraph_adjacent_vertices_offset <- new.env(parent = emptyenv())

# An off-by-one error in igraph::adjacent_vertices() was fixed in igraph 2.1.2.
get_igraph_adjacent_vertices_offset <- function() {
  if (!is.null(igraph_adjacent_vertices_offset$offset)) {
    return(igraph_adjacent_vertices_offset$offset)
  }
  opt <- igraph::igraph_opt("return.vs.es")
  on.exit(igraph::igraph_options(return.vs.es = opt))
  igraph::igraph_options(return.vs.es = FALSE)
  test_graph <- igraph::make_graph(edges = c("a", "b"))
  adjacent <- igraph::adjacent_vertices(
    graph = test_graph,
    v = "a",
    mode = "out"
  )
  adjacent <- as.integer(adjacent)
  offset <- 2L - adjacent
  igraph_adjacent_vertices_offset$offset <- offset
  offset
}
