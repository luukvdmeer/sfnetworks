# Errors, warnings and messages that occur at multiple locations

#' @importFrom cli cli_warn
raise_assume_constant = function(caller) {
  cli_warn(c(
    "{.fn {caller}} assumes all attributes are constant over geometries.",
    "!" = "Not all attributes are labelled as being constant.",
    "i" = "You can label attribute-geometry relations using {.fn sf::st_set_agr}."
  ))
}

#' @importFrom cli cli_warn
raise_assume_projected = function(caller) {
  cli_warn(c(
    "{.fn {caller}} assumes coordinates are projected.",
    "!" = paste(
      "The provided coordinates are geographic,",
      "which may lead to inaccurate results."
    ),
    "i" = "You can transform to a projected CRS using {.fn sf::st_transform}."
  ))
}

#' @importFrom cli cli_abort
raise_invalid_active = function(value) {
  cli_abort(c(
    "Unknown value for argument {.arg active}: {value}.",
    "i" = "Supported values are: nodes, edges."
  ))
}

#' @importFrom cli cli_abort
raise_invalid_sf_column = function() {
  cli_abort(c(
    "Attribute {.field sf_column} does not point to a geometry column.",
    "i" = paste(
      "Did you rename the geometry column without setting",
      "{.code st_geometry(x) = 'newname'}?"
    )
  ))
}

#' @importFrom cli cli_warn
raise_multiple_elements = function(arg) {
  cli_warn("Only the first element of {.arg {arg}} is used.")
}

#' @importFrom cli cli_abort
raise_na_values = function(arg) {
  cli_abort("{.arg {arg}} should not contain NA values.")
}

#' @importFrom cli cli_warn
raise_overwrite = function(value) {
  cli_warn("Overwriting column {.field {value}}.")
}

#' @importFrom cli cli_abort
raise_require_explicit = function() {
  cli_abort(c(
    "This call requires spatially explicit edges.",
    "i" = "Call {.fn tidygraph::activate} to activate nodes instead.",
    "i" = "Call {.fn sfnetworks::to_spatial_explicit} to explicitize edges."
  ))
}

#' @importFrom cli cli_abort
raise_reserved_attr = function(value) {
  cli_abort("The attribute name {.field value} is reserved.")
}

#' @importFrom cli cli_abort
raise_unknown_input = function(arg, value, options = NULL) {
  if (is.null(options)) {
    cli_abort("Unknown value for argument {.arg {arg}}: {value}.")
  } else {
    cli_abort(c(
      "Unknown value for argument {.arg {arg}}: {value}.",
      "i" = "Supported values are: {paste(options, collapse = ', ')}."
    ))
  }
}

#' @importFrom cli cli_abort
raise_unknown_summariser = function(value) {
  cli_abort(c(
    "Unknown attribute summary function: {value}.",
    "i" = "For supported values see {.fn igraph::attribute.combination}."
  ))
}

#' @importFrom cli cli_abort
raise_unsupported_arg = function(arg, replacement = NULL) {
  if (is.null(replacement)) {
    cli_abort("Setting argument {.arg {arg}} is not supported")
  } else {
    cli_abort(c(
      "Setting argument {.arg {arg}} is not supported.",
      "i" = "Use {.arg {replacement}} instead."
    ))
  }
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
    raise_unknown_input("caller", caller)
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
deprecate_weights_is_string = function() {
  deprecate_warn(
      when = "v1.0",
      what = paste0(
        "evaluate_weight_spec",
        "(weights = 'uses tidy evaluation')"
      ),
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
deprecate_weights_is_null = function() {
  deprecate_warn(
    when = "v1.0",
    what = paste0(
      "evaluate_weight_spec",
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

deprecate_from = function() {
  deprecate_warn(
    when = "v1.0",
    what = "to_spatial_neighborhood(from)",
    with = "to_spatial_neighborhood(direction)",
    details = c(
      i = paste(
        "If `from = FALSE` this will for now be automatically translated into",
        "`direction = 'in'`, but this may be removed in future versions."
      )
    )
  )
}