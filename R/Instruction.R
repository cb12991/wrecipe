#' R6 Class for recipe instructions
#'
#' @section Creating the instruction object:
#' An `Instruction` is an R6 object, and it can be created using
#' `Instruction$new()`. It only has one element, `text`, that contains the
#' instruction detail.
#'
#' \describe{
#'   \item{text}{
#'     Scalar character vector; the wording of one step of a recipe.
#'   }
#' }
#'
#' @export
#' @name Instruction
NULL

Instruction <- R6::R6Class(
  classname = 'Instruction',

  public = list(
    initialize = function(text) {
      private$.text$value <- private$.text$validate(name)
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
