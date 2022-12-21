#' @export
Instruction <- R6::R6Class(
  classname = 'Instruction',

  public = list(
    initialize = function(text) {
      private$.text$value <- private$.text$validate(text)
    }
  ),

  active = list(
    text = function(value) {
      if (missing(value)) {
        private$.name$value
      } else {
        private$.name$value <- private$.name$validate(value)
        invisible(self)
      }
    }
  ),

  private = list(
    .text = list(
      value = NULL,
      validate = function(x) {
        if (length(x) != 1) {
          cli::cli_abort(c(
            'Length != 1',
            '',
            i = 'Only provide a one recipe instruction per {.cls Instruction}
                 object.'
          ))
        }
        if (!is.character(x)) {
          cli::cli_abort(
            'Is not a character vector'
          )
        }
        invisible(x)
      }
    )
  )
)
