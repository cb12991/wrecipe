#' @export
Ingredient <- R6::R6Class(
  classname = 'Ingredient',

  public = list(
    initialize = function(name, amount = NA_real_, unit = NA_character_) {
      private$.name$value <- private$.name$validate(name)
      private$.amount$value <- private$.amount$validate(amount)
      private$.unit$value <- private$.unit$validate(unit)
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
    amount = function(value) {
      if (missing(value)) {
        private$.amount$value
      } else {
        private$.amount$value <- private$.amount$validate(value)
        invisible(self)
      }
    },
    unit = function(value) {
      if (missing(value)) {
        private$.unit$value
      } else {
        private$.unit$value <- private$.unit$validate(value)
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
            'All {.cls Ingredient} objects must at minimum have a name.'
          )
        }
        if (length(x) != 1) {
          cli::cli_abort(c(
            'Length != 1',
            i = 'Only provide a single name for each ingredient.'
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
    .amount =  list(
      value = NULL,
      validate = function(x) {
        if (!is.numeric(x)) {
          cli::cli_abort(
            'Not a number'
          )
        }
        if (x <= 0) {
          cli::cli_abort(
            'Non-NA amounts must be greater than or equal to 1.',
          )
        }
        if (length(x) != 1) {
          cli::cli_abort(c(
            'Length != 1',
            '',
            i = 'Only provide a single {.field amount} value for each
                   ingredient.'
          ))
        }
        invisible(x)
      }
    ),
    .unit =  list(
      value = NULL,
      validate = function(x) {
        if (!is.character(x)) {
          cli::cli_abort(c(
            'Is not a character vector',
            '',
            i = 'Enter a common {.field unit} used in recipes, i.e., cup,
                   tablespoon, gram, etc.'
          ))
        }

        if (length(x) != 1) {
          cli::cli_abort(c(
            'Length != 1',
            '',
            i = 'Only provide a single {.field unit} value for each
                   ingredient.'
          ))
        }
        invisible(x)
      }
    )
  )
)
