#' Extract datanames from list of picks
#' @keywords internal
#' @noRd
.picks_datanames <- function(x) {
  checkmate::assert_list(x, c("picks", "NULL"))
  datanames_list <- lapply(x, function(x) {
    if (is.character(x$datasets$choices)) {
      x$datasets$choices
    } else {
      NULL
    }
  })

  if (any(vapply(datanames_list, is.null, logical(1)))) {
    "all"
  } else {
    unique(unlist(datanames_list))
  }
}


#' Create a reactive that sets plot dimensions on a `teal_card`
#'
#' This is a convenience function that creates a reactive expression that
#' automatically sets the `dev.width` and `dev.height` attributes on the last
#' chunk outputs of a `teal_card` based on plot dimensions from a plot widget.
#'
#' @return A reactive expression that returns the `teal_card` with updated dimensions
#'
#' Collect unique datanames from a list of picks objects (internal).
#'
#' @param pick_slots (`list`) named list of `picks` objects (NULL entries ignored).
#'
#' @keywords internal
#'
.picks_all_datanames <- function(pick_slots) {
  pick_slots <- pick_slots[!vapply(pick_slots, is.null, logical(1))]
  if (length(pick_slots) == 0L) {
    return(character())
  }
  all_datanames <- unique(
    unlist(
      lapply(
        pick_slots,
        function(p) {
          ch <- p$datasets$choices
          if (checkmate::test_character(ch, min.len = 1L)) {
            return(unique(as.character(ch)))
          }
          sel <- p$datasets$selected
          unique(as.character(unlist(sel, recursive = FALSE, use.names = FALSE)))
        }
      ),
      use.names = FALSE
    )
  )
  all_datanames[nzchar(all_datanames) & !is.na(all_datanames)]
}

#' Assert a picks object uses single variable selection (internal).
#'
#' @param pick (`picks`)
#' @param arg_name (`character`) argument name for error messages.
#'
#' @keywords internal
.assert_picks_single_var <- function(pick, arg_name) {
  checkmate::assert_class(pick, "picks", .var.name = arg_name)
  checkmate::assert_false(
    teal.picks::is_pick_multiple(pick$variables),
    .var.name = sprintf("`%s` must use variables(..., multiple = FALSE)", arg_name)
  )
}

#' Force a picks variable selection to single or multiple selection (internal)
#'
#' Warns when `pick$variables` allows multiple selection and coerces the
#' underlying metadata to `multiple = multiple` (default false).
#'
#' @param pick (`picks`)
#' @param arg_name (`character`) argument name used in warning messages.
#' @param multiple (`logical`) whether selection shuold be multiple (TRUE) or single (FALSE)
#'
#' @return The updated `pick` object.
#'
#' @keywords internal
#' @noRd
force_pick_selection <- function(pick,
                                 arg_name = checkmate::vname(pick),
                                 which = c("variables", "values", "datasets"),
                                 multiple = FALSE) {
  checkmate::assert_class(pick, "picks", .var.name = arg_name)
  checkmate::assert_string(arg_name)
  checkmate::assert_logical(multiple)
  which <- match.arg(which)
  selection <- if (isTRUE(multiple)) "multiple" else "single"

  if (!identical(multiple, teal.picks::is_pick_multiple(pick[[which]]))) {
    warning(
      sprintf(
        "`%s` accepts only a %s variable selection. \nForcing `teal.picks::%s(multiple)` to `%s`.",
        arg_name, selection, which, multiple
      )
    )
    attr(pick[[which]], "multiple") <- multiple
  }

  pick
}

#' @keywords internal
set_chunk_dims <- function(pws, q_r, inner_classes = NULL) {
  checkmate::assert_list(pws)
  checkmate::assert_names(names(pws), must.include = "dim")
  checkmate::assert_class(pws$dim, "reactive")
  checkmate::assert_class(q_r, "reactive")
  checkmate::assert_character(inner_classes, null.ok = TRUE)

  reactive({
    pws_dim <- stats::setNames(as.list(req(pws$dim())), c("width", "height"))
    if (identical(pws_dim$width, "auto")) { # ignore non-numeric values (such as "auto")
      pws_dim$width <- NULL
    }
    if (identical(pws_dim$height, "auto")) { # ignore non-numeric values (such as "auto")
      pws_dim$height <- NULL
    }
    q <- req(q_r())
    teal.reporter::teal_card(q) <- set_chunk_attrs(
      teal.reporter::teal_card(q),
      list(dev.width = pws_dim$width, dev.height = pws_dim$height),
      inner_classes = inner_classes
    )
    q
  })
}

#' Coerce legacy `teal.transform` specs to [`teal.picks::variables()`] with deprecation
#'
#' If `x` is a legacy `choices_selected`, `filter_spec`, or `select_spec` object, it is converted
#' via [`teal.picks::as.picks()`]. Otherwise `x` must already inherit `"variables"`.
#'
#' @param x (`values`, `choices_selected` or `picks`) object.
#' @param arg_name optional (`character(1)`) argument name.
#' @param multiple optional (`logical(1)`) whether multiple values are allowed.
#' If `NULL` (default), it is not validated and inferred from the length of `selected` in the
#' `choices_selected` object.
#' @param null.ok (`logical(1)`) whether `NULL` is allowed.
#'
#' @keywords internal
#' @noRd
migrate_choices_selected_to_variables <- function(x, # nolint: object_length_linter
                                                  arg_name = checkmate::vname(x),
                                                  multiple = NULL,
                                                  null.ok = FALSE) { # nolint: object_name_linter.
  # nolint: object_name_linter.
  checkmate::assert_string(arg_name)
  checkmate::assert_flag(multiple, null.ok = TRUE)
  checkmate::assert_flag(null.ok)
  if (inherits(x, "picks")) {
    return(x)
  }

  if (isTRUE(null.ok) && is.null(x)) {
    return(x)
  }
  legacy <- c("choices_selected", "filter_spec", "select_spec")
  if (inherits(x, legacy)) {
    lifecycle::deprecate_warn(
      when = "0.5.0",
      what = I(paste0("`", arg_name, "`")),
      details = paste(
        "Pass `teal.picks::variables()` (or a full `teal.picks::picks()` chain).",
        "Support for legacy `teal.transform::choices_selected()`, `filter_spec`, and `select_spec` is deprecated."
      )
    )
    x <- teal.picks::as.picks(x, quiet = FALSE)
    attr(x, "multiple") <- (!is.null(multiple) && multiple) || (is.null(multiple) && length(x$selected) > 1L)
  } else {
    if (!is.null(multiple) && !identical(attr(x, "multiple", exact = TRUE), multiple)) {
      stop(
        sprintf("`multiple` metadata does not match the requirement for %s.", arg_name),
        sprintf(" Please set multiple = %s in the picks object.", multiple),
        call. = FALSE
      )
    }
  }
  checkmate::assert_class(
    x,
    "variables",
    null.ok = null.ok,
    .var.name = arg_name
  )
  x
}

#' Coerce legacy `choices_selected` to [`teal.picks::values()`] with deprecation
#'
#' @param x (`values`, `choices_selected`, [`teal.picks::picks()`], or [`teal.picks::variables()`]) object.
#' @param arg_name optional (`character(1)`) argument name.
#' @param multiple optional (`logical(1)`) whether multiple values are allowed.
#' If `NULL` (default), it is not validated and inferred from the length of `selected` in the
#' `choices_selected` object. If `FALSE`, the result is checked with [teal.picks::is_pick_multiple()].
#'
#' @keywords internal
#' @noRd
migrate_choices_selected_to_values <- function(x, # nolint: object_length_linter
                                               arg_name = checkmate::vname(x),
                                               multiple = NULL) {
  checkmate::assert_string(arg_name)
  checkmate::assert_flag(multiple, null.ok = TRUE)

  if (inherits(x, "picks")) {
    return(x)
  }
  if (inherits(x, "variables")) {
    return(x)
  }
  if (inherits(x, "choices_selected")) {
    lifecycle::deprecate_warn(
      when = "0.5.0",
      what = I(paste0("`", arg_name, "`")),
      details = paste(
        "Pass `teal.picks::values()`.",
        "Support for legacy `teal.transform::choices_selected()` is deprecated."
      )
    )
    if (is.null(x$choices) || inherits(x$choices, "delayed_data")) {
      stop(
        "Delayed `choices_selected` objects cannot be coerced automatically; ",
        "specify `teal.picks::values()` explicitly.",
        call. = FALSE
      )
    }
    choices <- as.character(x$choices)
    selected <- as.character(unlist(x$selected, use.names = FALSE))
    checkmate::assert_character(choices, min.len = 1L)
    if (length(selected) == 0) {
      selected <- NULL
    }
    checkmate::assert_character(selected, min.len = 1L, null.ok = TRUE)
    fixed <- isTRUE(x$fixed)
    multiple <- (!is.null(multiple) && multiple) || (is.null(multiple) && length(selected) > 1L)
    x <- teal.picks::values(choices, selected, fixed = fixed, multiple = multiple)
  }
  checkmate::assert_class(x, "values", .var.name = arg_name)
  x
}

#' Supports the creation of picks object that does not override a dataset if already exists
#' @param datasets ([`teal.picks::datasets()`] object) to use if `x` does not already have a dataset.
#' @param x (`pick` or `picks` object) to ensure has a dataset.
#' @return a `picks` object with a dataset, either from `x` or from `datasets`.
#' @keywords internal
#' @noRd
create_picks_helper <- function(datasets = NULL, x) {
  if (inherits(x, "picks") && !is.null(x$datasets)) {
    return(x)
  }
  checkmate::assert_class(datasets, "datasets", null.ok = FALSE)
  checkmate::assert_multi_class(x, c("pick", "picks"))

  if (inherits(x, "picks")) {
    picks_args <- list(datasets, x$variables, x$values)
    do.call(
      teal.picks::picks,
      picks_args[vapply(picks_args, Negate(is.null), logical(1L))],
    )
  } else if (inherits(x, "pick")) {
    teal.picks::picks(datasets, x)
  }
}

#' Coerce legacy `choices_selected`-based specs to `picks` with deprecation
#'
#' @param x (`variables`, `values`, `choices_selected` or `picks`) object.
#'   A bare [`teal.picks::variables()`] pick is returned unchanged (column selector only; value
#'   levels follow from data when the pick chain is completed with `create_picks_helper()`).
#' @param arg_name optional (`character(1)`) argument name.
#' @param multiple optional (`logical(1)`) whether multiple values are allowed.
#' If `NULL` (default), it is not validated and inferred from the length of `selected` in the
#' `choices_selected` object.
#' @param default_variable_name optional (`character(1)`) variable name to use if `x` is a bare `values` pick.
#' @param add_values (`logical(1)`) whether to add a `values` pick if `x` is a bare `variables` pick.
#' Default is `TRUE`.
#'
#' @keywords internal
#' @noRd
migrate_value_choices_to_picks <- function(x, # nolint: object_length_linter.
                                           multiple = NULL,
                                           arg_name = checkmate::vname(x),
                                           default_variable_name = NULL,
                                           add_values = TRUE) {
  checkmate::assert_flag(multiple, null.ok = TRUE)
  checkmate::assert_string(arg_name)
  checkmate::assert_flag(add_values)
  checkmate::assert_string(default_variable_name, null.ok = TRUE)

  if (inherits(x, "picks")) {
    if (!is.null(multiple) && !identical(attr(x$values, "multiple", exact = TRUE), multiple)) {
      stop(
        sprintf("`multiple` metadata does not match the requirement for %s.", arg_name),
        sprintf(" Please set multiple = %s in the picks object.", multiple),
        call. = FALSE
      )
    }

    if (add_values && is.null(x$values)) {
      x$values <- do.call(teal.picks::values, list(multiple = multiple)[!is.null(multiple)])
    }
    return(x)
  }

  if (inherits(x, "choices_selected")) {
    values <- migrate_choices_selected_to_values(x, multiple = multiple, arg_name = arg_name)
    variable_name <- attr(x$choices, "var_choices", exact = TRUE) %||% default_variable_name
    if (is.null(default_variable_name) && inherits(x, "choices_selected") && is.null(variable_name)) {
      stop(
        sprintf("When using choices_selected for %s", arg_name),
        " it should have 'var_choices' attribute specifying variable choices.",
        " Cannot convert to picks object without this information.",
        call. = FALSE
      )
    }
    return(
      teal.picks::picks(
        teal.picks::variables(variable_name, variable_name),
        values,
        check_dataset = FALSE
      )
    )
  }
  if (inherits(x, "variables")) {
    if (add_values) {
      return(
        teal.picks::picks(
          x,
          do.call(teal.picks::values, list(multiple = multiple)[!is.null(multiple)]),
          check_dataset = FALSE
        )
      )
    }
    teal.picks::picks(x, check_dataset = FALSE)
  } else if (inherits(x, "values") && !is.null(default_variable_name)) {
    teal.picks::picks(
      teal.picks::variables(default_variable_name, default_variable_name),
      x,
      check_dataset = FALSE
    )
  } else {
    stop(
      sprintf("Cannot convert object of class %s to picks for %s.", class(x)[1], arg_name),
      call. = FALSE
    )
  }
}
