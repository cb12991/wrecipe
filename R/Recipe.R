#' @export
Recipe <- R6::R6Class(
  classname = 'Recipe',
  private = list(
    .name = list(
      value = NULL,
      validate = function(x) {
        if (is.null(x)) {
          cli::cli_abort(
            'All {.cls Recipe} objects must at minimum have a name.'
          )
        }
        if (length(x) != 1) {
          cli::cli_abort(c(
            'Length != 1',
            i = 'Only provide a single name for the recipe.'
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
    .summary =  list(
      value = NULL,
      validate = function(x) {
        if (!is.null(x)) {
          if (!is.character(x)) {
            cli::cli_abort(
              'Not a character vector'
            )
          }
        }
        invisible(x)
      }
    ),
    .images =  list(
      value = NULL,
      validate = function(x) {
        if (length(x)) {
        if (!all(y <- vapply(x, inherits, logical(1), 'Image'))) {
          cli::cli_abort(c(
              'All elements not of class {.cls Image}',
              '',
              i = '{cli::qty(y)}Element{?s} {which(y)} have class{?es}
                   {.cls {class(x[y])}}{?, respectively}.'
            ))
          }
        }
        invisible(x)
      }
    ),
    .ingredients =  list(
      value = NULL,
      validate = function(x) {
        if (length(x)) {
          if (!all(y <- vapply(x, inherits, logical(1), 'Ingredient'))) {
            cli::cli_abort(c(
              'All elements not of class {.cls Ingredient}',
              '',
              i = '{cli::qty(y)}Element{?s} {which(y)} have class{?es}
                   {.cls {class(x[y])}}{?, respectively}.'
            ))
          }
        }
        invisible(x)
      }
    ),
    .equipment =  list(
      value = NULL,
      validate = function(x) {
        if (length(x)) {
          if (!all(y <- vapply(x, inherits, logical(1), 'Equipment'))) {
            cli::cli_abort(c(
              'All elements not of class {.cls Equipment}',
              '',
              i = '{cli::qty(y)}Element{?s} {which(y)} have class{?es}
                   {.cls {class(x[y])}}{?, respectively}.'
            ))
          }
        }
        invisible(x)
      }
    ),
    .instructions =  list(
      value = NULL,
      validate = function(x) {
        if (length(x)) {
          if (!all(y <- vapply(x, inherits, logical(1), 'Instruction'))) {
            cli::cli_abort(c(
              'All elements not of class {.cls Instruction}',
              '',
              i = '{cli::qty(y)}Element{?s} {which(y)} have class{?es}
                   {.cls {class(x[y])}}{?, respectively}.'
            ))
          }
        }
        invisible(x)
      }
    ),
    .servings =  list(
      value = 0,
      validate = function(x) {
        if (!is.numeric(x)) {
            cli::cli_abort(
              'Not a number'
            )
        }
        if (x < 0) {
          cli::cli_abort(c(
            'Must be greater than or equal to 0',
            '',
            i = '{Setting {.field servings} to 0 indicates that they have not
                 yet been determined.'
          ))
        }
        invisible(x)
      }
    )
  ),
  active = list(
    name = active_binding(.name),
    summary = active_binding(.summary),
    images = active_binding(.images),
    ingredients = active_binding(.ingredients),
    equipment = active_binding(.equipment),
    instructions = active_binding(.instructions),
    servings = active_binding(.servings)
  ),

  public = list(
    initialize = function(
      name,
      summary = NA_character_,
      images = list(),
      ingredients = list(),
      equipment = list(),
      instructions = list(),
      servings = 0
    ) {
      private$.name$value <- private$.name$validate(name)
      private$.summary$value <- private$.summary$validate(summary)
      private$.images$value <- private$.images$validate(images)
      private$.ingredients$value <- private$.ingredients$validate(ingredients)
      private$.equipment$value <- private$.equipment$validate(equipment)
      private$.instructions$value <- private$.instructions$validate(instructions)
      private$.servings$value <- private$.servings$validate(servings)
    },

    print = function() {
      cat('<Recipe>')
      cat('\n\n ', private$.name$value)
      cat('\n\n')
      cat(strwrap(private$.summary$value, prefix = '  '), sep = '\n')
      cat('\n  Image Count:', length(private$.images$value))
      cat('\n  Ingredient Count:', length(private$.ingredients$value))
      cat('\n  Equipment Count:', length(private$.equipment$value))
      cat('\n  Instruction Count:', length(private$.instructions$value))
      invisible(self)
    }
  )
)
