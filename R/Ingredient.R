#' @export
Ingredient <- R6::R6Class(
  classname = 'Ingredient',
  public = list(
    name = NULL,
    amount = NA_real_,
    unit = NA_character_,

    initialize = function(name, amount, unit = NA_character_) {
      # assert_that(is.character(name), length(name) == 1)
      # assert_that(is.numeric(amount), length(amount) == 1)
      # assert_that(is.character(unit), length(unit) == 1)

      self$name <- name
      self$amount <- amount
      self$unit <- unit
    },
    print = function() {
      cat(
        '<Ingredient>\n\n',
        self$amount,
        if (is.na(self$unit)) NULL else self$unit,
        self$name,
        '\n'
      )
    }
  )
)
