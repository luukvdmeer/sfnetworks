# Errors, warnings and messages that occur at multiple locations

raise_assume_constant = function(caller) {
  warning(
    caller,
    " assumes attributes are constant over geometries",
    call. = FALSE
  )
}

raise_assume_planar = function(caller) {
  warning(
    "Although coordinates are longitude/latitude, ",
    caller,
    " assumes that they are planar",
    call. = FALSE
  )
}

raise_multiple_elements = function(arg) {
  warning(
    "Although argument ",
    arg,
    " has length > 1, only the first element is used",
    call. = FALSE
  )
}

raise_na_values = function(arg) {
  stop(
    "NA values present in argument ",
    arg,
    call. = FALSE
  )
}

raise_overwrite = function(value) {
  warning(
    "Overwriting column(s): ",
    value,
    call. = FALSE
  )
}

raise_reserved_attr = function(value) {
  stop(
    "The attribute name '",
    value,
    "' is reserved",
    call. = FALSE
  )
}

raise_unknown_input = function(value) {
  stop(
    "Unknown input: ",
    value,
    call. = FALSE
  )
}

raise_invalid_sf_column = function() {
  stop(
    "Attribute 'sf_column' does not point to a geometry column.\n",
    "Did you rename it, without setting st_geometry(x) = 'newname'?",
    call. = FALSE
  )
}

#' @importFrom lifecycle deprecate_stop
deprecate_length_as_weight = function(caller) {
  switch(
    caller,
    sfnetwork = deprecate_stop(
      when = "v1.0",
      what = "sfnetwork(length_as_weight)",
      with = "sfnetwork(compute_length)"
    ),
    as_sfnetwork.sf = deprecate_stop(
      when = "v1.0",
      what = "as_sfnetwork.sf(length_as_weight)",
      with = "as_sfnetwork.sf(compute_length)",
      details = c(
        i = paste(
          "The sf method of `as_sfnetwork()` now forwards `...` to",
          "`create_from_spatial_lines()` for linestring geometries",
          "and to `create_from_spatial_points()` for point geometries."
        )
      )
    ),
    raise_unknown_input(caller)
  )
}

#' @importFrom lifecycle deprecate_stop
deprecate_edges_as_lines = function() {
  deprecate_stop(
    when = "v1.0",
    what = paste(
      "as_sfnetwork.sf(edges_as_lines = 'is deprecated for",
      "linestring geometries')"
    ),
    details = c(
      i = paste(
        "The sf method of `as_sfnetwork()` now forwards `...` to",
        "`create_from_spatial_lines()` for linestring geometries."
      ),
      i = paste(
        "An sfnetwork created from linestring geometries will now",
        "always have spatially explicit edges."
      )
    )
  )
}

#' @importFrom lifecycle deprecate_warn
deprecate_weights_is_string = function(caller) {
  deprecate_warn(
      when = "v1.0",
      what = paste(caller, "(weights = 'uses tidy evaluation')"),
      details = c(
        i = paste(
          "This means you can forward column names without quotations, e.g.",
          "`weights = length` instead of `weights = 'length'`. Quoted column",
          "names are currently still supported for backward compatibility,",
          "but this may be removed in future versions."
        )
      )
    )
}

#' @importFrom lifecycle deprecate_warn
deprecate_weights_is_null = function(caller) {
  deprecate_warn(
    when = "v1.0",
    what = paste(
      caller,
      "(weights = 'if set to NULL means no edge weights are used')"
    ),
    details = c(
      i = paste(
        "If you want to use geographic length as edge weights, use",
        "`weights = edge_length()` or provide a column in which the edge",
        "lengths are stored, e.g. `weights = length`."
      ),
      i = paste(
        "If you want to use the weight column for edge weights, specify",
        "this explicitly through `weights = weight`."
      )
    )
  )
}