#' @export
Equipment <- R6::R6Class(
  classname = 'Equipment',
  public = list(
    initialize = function(name, quantity = NA_integer_) {
      # assert_that(is.character(name), length(name) == 1)
      # assert_that(is.numeric(quantity))

      self$name <- name
      self$quantity <- quantity
    }
  )
)
