#' R6 Class for a recipe ingredient
#'
#' An `Ingredient` object is an R6 object, and it can be created using
#' `Ingredient$new()`. A `name` field for the ingredient piece is required, but
#' the amount and unit of the ingredient can be omitted for a generalized
#' ingredient (e.g., canola oil).
#'
#' @field name
#'   Scalar character vector; the name of the piece of equipment.
#' @field amount
#'   Scalar numeric vector; the numeric quantity required of the ingredient in
#'   a recipe. To be used in conjunction with "unit".
#' @field unit
#'   Scalar character vector; the unit of measurement for the ingredient
#'   (e.g., cup, gallon, handful, etc.). To be used in conjunction with
#'   "amount".
#'
#' @family recipe components
#' @seealso [Recipe]
#' @export
#' @name Ingredient
NULL

Ingredient <- R6::R6Class(
  classname = 'Ingredient',

  public = list(
    initialize = function(name, amount = NA_real_, unit = NA_character_) {
      private$.name$value <- private$.name$validate(name)
      private$.amount$value <- private$.amount$validate(amount)
      private$.unit$value <- private$.unit$validate(unit)
    },

    print = function() {
      cat('<Ingredient>')
      cat(' ', private$.amount$value, private$.unit$value, private$.name$value)
      invisible(self)
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
      validate = function(name) {
        lapply(
          X = list(check_required, check_length, check_mode),
          FUN = rlang::exec,
          x = name,
          mode = 'character',
          n = 1
        )
        invisible(name)
      }
    ),
    .amount =  list(
      value = NULL,
      validate = function(amount) {
        lapply(
          X = list(check_length, check_mode, check_number_in_range),
          FUN = rlang::exec,
          x = amount,
          n = 1,
          mode = 'numeric',
          range = c(0, Inf),
          inclusive = TRUE,
          allow_na = TRUE
        )
        invisible(amount)
      }
    ),
    .unit =  list(
      value = NULL,
      validate = function(unit) {
        lapply(
          X = list(check_length, check_mode),
          FUN = rlang::exec,
          x = unit,
          mode = 'character',
          n = 1
        )
        invisible(unit)
      }
    )
  )
)
