#' R6 Class for recipe equipment
#'
#' @section Creating the equipment object:
#' An `Equipment` object is an R6 object, and it can be created using
#' `Equipment$new()`. The name of the equipment piece is required, and the
#' quantity can be omitted if desired.
#'
#' \describe{
#'   \item{name}{
#'     Scalar character vector; the name of the piece of equipment.
#'   }
#'   \item{quantity}{
#'     Scalar numeric vector; the number of equipment units needed. Defaults to
#'     1.
#'   }
#' }
#'
#' @export
#' @name Equipment
NULL

Equipment <- R6::R6Class(
  classname = 'Equipment',

  public = list(
    initialize = function(name, quantity = 1) {
      private$.name$value <- private$.name$validate(name)
      private$.quantity$value <- private$.quantity$validate(quantity)
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
      validate = function(name) {
        lapply(
          X = list(check_required, check_length, check_mode),
          FUN = rlang::exec,
          x = name,
          n = 1,
          mode = 'character'
        )
        invisible(name)
      }
    ),
    .quantity =  list(
      value = NULL,
      validate = function(quantity) {
        lapply(
          X = list(check_length, check_mode, check_number_in_range),
          FUN = rlang::exec,
          x = quantity,
          n = 1,
          mode = 'numeric',
          range = c(1, Inf)
        )
        invisible(quantity)
      }
    )
  )
)
