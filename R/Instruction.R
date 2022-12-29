#' R6 Class for recipe instructions
#'
#' An `Instruction` is an R6 object, and it can be created using
#' `Instruction$new()`. It only has one field, `text`, that contains the
#' instruction detail.
#'
#' @field text
#'   Scalar character vector; the wording of one step of a recipe.
#'
#' @family recipe components
#' @seealso [Recipe]
#' @export
#' @name Instruction
NULL

Instruction <- R6::R6Class(
  classname = 'Instruction',

  public = list(
    initialize = function(text) {
      private$.text$value <- private$.text$validate(text)
    },

    print = function() {
      cat('<Instruction>')
      cat(strwrap(private$.text$value, prefix = '  '), sep = '\n')
      invisible(self)
    }
  ),

  active = list(
    text = function(value) {
      if (missing(value)) {
        private$.text$value
      } else {
        private$.text$value <- private$.text$validate(value)
        invisible(self)
      }
    }
  ),

  private = list(
    .text = list(
      value = NULL,
      validate = function(text) {
        lapply(
          X = list(check_required, check_length, check_mode),
          FUN = rlang::exec,
          x = text,
          n = 1,
          mode = 'character'
        )
        invisible(text)
      }
    )
  )
)
