#' @export
Instruction <- R6::R6Class(
  classname = 'Instruction',
  public = list(
    step = 0,

    initialize = function(text, time, active = TRUE) {
      # assert_that(is.character(text), length(text) == 1)
      # assert_that(is.duration(time), length(time) == 1)
      # assert_that(is.logical(active), length(active) == 1)

      self$step <- self$step + 1
      self$text <- text
      self$time <- time
      self$active <- active
    }
  )
)
