#' @export
Equipment <- R6::R6Class(
  classname = 'Equipment',

  public = list(
    initialize = function(name, quantity = 1) {
      private$.name$value <- private$.name$validate(name)
      private$.quantity$value <- private$.quantity$validate(quantity)
    }
  ),

  active = list(
    name = function(value) {
      if (missing(value)) {
        private$.name$value
      } else {
        private$.name$value <- private$.name$validate(value)
        invisible(self)
      }
    },
    quantity = function(value) {
      if (missing(value)) {
        private$.quantity$value
      } else {
        private$.quantity$value <- private$.quantity$validate(value)
        invisible(self)
      }
    }
  ),

  private = list(
    .name = list(
      value = NULL,
      validate = function(x) {
        if (is.null(x)) {
          cli::cli_abort(
            'All {.cls Equipment} objects must at minimum have a name.'
          )
        }
        if (length(x) != 1) {
          cli::cli_abort(c(
            'Length != 1',
            i = 'Only provide a single name for the piece of equipment.'
          ))
        }
        if (!is.character(x)) {
          cli::cli_abort(
            'Is not a character vector'
          )
        }
        invisible(x)
      }
    ),
    .quantity =  list(
      value = NULL,
      validate = function(x) {
        if (!is.numeric(x)) {
          cli::cli_abort(
            'Is not a number'
          )
        }
        if (x <= 0) {
          cli::cli_abort(
            'Must be greater than or equal to 1',
          )
        }
        invisible(x)
      }
    )
  )
)
