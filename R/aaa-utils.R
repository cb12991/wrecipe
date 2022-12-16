active_binding <- function(name) {
  name <- rlang::ensym(name)
  function(value) {
    if (missing(value)) {
      private$name$value
    } else {
      private$name$value <- private$name$validate(value)
      self
    }
  }
}
