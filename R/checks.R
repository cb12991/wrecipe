check_required <- function(
    x,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env(),
    ...
) {
  if (!missing(x)) {
    return(invisible(TRUE))
  }
  cli::cli_abort(
    '{.arg {arg}} is absent but must be supplied.',
    arg = arg,
    call = call
  )
}

check_length <- function(
    x,
    n,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env(),
    allow_null = FALSE,
    ...
) {
  if ((allow_null && length(x) == 0) | (length(x) %in% n)) {
    return(invisible(TRUE))
  }
  cli::cli_abort(
    '{.arg {arg}} must have size {cli::cli_vec(n, list("vec-last" = ", or ",
     "vec-sep2" = " or "))}, not size {length(x)}.',
    arg = arg,
    call = call
  )
}

check_mode <- function(
    x,
    mode,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env(),
    allow_null = TRUE,
    ...
) {
  if ((allow_null && is.null(x)) | (mode(x) == mode)) {
    return(invisible(TRUE))
  }
  cli::cli_abort(
    '{.arg {arg}} should be {.obj_type_friendly {vector(mode)}}, not
     {.obj_type_friendly {x}}.',
    arg = arg,
    call = call
  )
}

check_number_in_range <- function(
    x,
    range = c(-Inf, Inf),
    inclusive = TRUE,
    allow_na = TRUE,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env(),
    ...
) {
  lapply(
    X = list(check_mode, check_length),
    FUN = rlang::exec,
    x = range,
    n = 2,
    mode = 'numeric',
    arg = arg,
    call = call
  )
  lapply(
    X = list(check_mode, check_length),
    FUN = rlang::exec,
    x = inclusive,
    n = 1,
    mode = 'logical',
    arg = arg,
    call = call
  )
  lapply(
    X = list(check_mode, check_length),
    FUN = rlang::exec,
    x = allow_na,
    n = 1,
    mode = 'logical',
    arg = arg,
    call = call
  )

  if ((x > range[1] - inclusive & x < range[2] + inclusive) %|% allow_na) {
    return(invisible(TRUE))
  }
  cli::cli_abort(
    '{.arg {arg}} must be greater than {ifelse(inclusive, "or equal to "), ""}
     {range[1]} and less than {ifelse(inclusive, "or equal to "), ""}
     {range[2]}.',
    arg = arg,
    call = call
  )
}

check_url_connection <- function(
    x,
    call = rlang::caller_env(),
    allow_null = TRUE,
    ...
) {
  if ((is.null(x) & allow_null) || httr::GET(x)$status_code == 200) {
    return(invisible(TRUE))
  }
  cli::cli_abort(
    'URL {.url {x}} does not result in successful HTTP response.'
  )
}

check_file_exists <- function(
    x,
    call = rlang::caller_env(),
    allow_null = TRUE,
    ...
) {
  if ((is.null(x) & allow_null) || file.exists(x)) {
    return(invisible(TRUE))
  }
  cli::cli_abort(
    'Could not find file at {.file {x}}.',
    call = call
  )
}

check_same_class <- function(
    x,
    class,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env(),
    ...
) {
  x <- as.list(x)
  y <- vapply(x, inherits, logical(1), what = class)
  if (length(x) == 0 || length(x[!y]) == 0) {
    return(invisible(TRUE))
  }
  cli::cli_abort(
    'All elements of {.arg {arg}} do not inherit {.cls {class}} class.',
    arg = arg,
    call = call
  )
}
