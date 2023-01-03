#' R6 Class providing standard format for recipes
#'
#' This class will encapsulate various recipe elements into a standard,
#' consistent format. Once the object is built, it will be easier to work with
#' when incorporating in other projects, i.e., a website.
#'
#' @section Creating the recipe object:
#' A `Recipe` is an R6 object, and it can be created using `Recipe$new()`. A
#' `Recipe` has several different fields that you could pass as arguments
#' when initializing, however, you only need to provide the `name` when first
#' initializing; other fields can be added at a later time.
#'
#' @field name
#'   Scalar character vector; the name of the recipe.
#' @field summary
#'   Character vector; a brief description of what the recipe makes.
#' @field servings
#'   Numeric vector of length 1 or 2; if only one value provided, the number of
#'   servings the recipe produces. If 2 values provided, the minimum and maximum
#'   number of servings that the recipe produces.
#' @field time
#'   (Optionally named) list of [lubridate::duration] objects (or objects that
#'   can be converted to `lubridate::duration` objects, i.e., the total number
#'   of seconds (since `lubridate::duration` objects are measured in seconds)
#'   or string representations of durations, e.g., "14 hours 20 minutes"); you
#'   can provide a list with only one element to represent the total recipe
#'   time, or you can provide a named list with elements that represent
#'   different time components, e.g., "active", "prep", etc.. Providing an
#'   unnamed lists with multiple elements will return an error because it is
#'   unclear which element corresponds to which time component.
#' @field images
#'   A list of [Image] objects associated with this `Recipe`.
#' @field ingredients
#'   A list of [Ingredient] objects associated with this `Recipe`.
#' @field equipment
#'   A list of [Equipment] objects associated with this `Recipe`.
#' @field instructions
#'   A list of [Instruction] objects associated with this `Recipe`.
#'
#'
#' @section Uses for the recipe object:
#' This class was created mainly to store recipes in a consistent format.
#' `Recipe` fields can be manually provided, but I've been webscraping and
#' adding fields that way. Once fully built, different recipe fields can be
#' accessed using the `$` operator.
#'
#' @examples
#' # An example recipe for spaghetti sauce:
#' r <- Recipe$new('Spaghetti Sauce')
#' print(r)
#'
#' # Add a summary:
#' r$summary <- paste(
#'   'This is my grandmother\'s secret recipe for her award-winning spaghetti',
#'   'sauce. It has been passed down for generations and dates back to the',
#'   '1700\'s!'
#' )
#' print(r)
#'
#' # Other elements are additional R6 classes. See the help page of each for
#' # additional information.
#'
#' @seealso [Image] [Ingredient] [Equipment] [Instruction] [lubridate::duration]
#' @export
#' @name Recipe
NULL

Recipe <- R6::R6Class(
  classname = 'Recipe',

  public = list(
    initialize = function(
      name,
      summary = NA_character_,
      servings = NA_real_,
      time = NULL,
      images = NULL,
      ingredients = NULL,
      equipment = NULL,
      instructions = NULL
    ) {
      private$.name$value <- private$.name$validate(name)
      private$.summary$value <- private$.summary$validate(summary)
      private$.servings$value <- private$.servings$validate(servings)
      private$.time$value <- private$.time$validate(time)
      private$.images$value <- private$.images$validate(images)
      private$.ingredients$value <- private$.ingredients$validate(ingredients)
      private$.equipment$value <- private$.equipment$validate(equipment)
      private$.instructions$value <- private$.instructions$validate(instructions)

    },

    print = function() {
      cat('<Recipe>')
      cat('\n\n ', private$.name$value)
      cat('\n\n')
      cat(strwrap(private$.summary$value, prefix = '  '), sep = '\n')
      cat('\n  Servings:', paste(sort(private$.servings$value), sep = '-'))
      cat(
        '\n  Total Time:',
        lubridate::as.period(Reduce(sum, private$.time$value, numeric())),
        '\n'
      )
      cat('\n  Image Count:', length(private$.images$value))
      cat('\n  Ingredient Count:', length(private$.ingredients$value))
      cat('\n  Equipment Count:', length(private$.equipment$value))
      cat('\n  Instruction Count:', length(private$.instructions$value))
      invisible(self)
    },

    export = function(file = '.') {
      saveRDS(self, file = file)
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
    summary = function(value) {
      if (missing(value)) {
        private$.summary$value
      } else {
        private$.summary$value <- private$.summary$validate(value)
        invisible(self)
      }
    },
    servings = function(value) {
      if (missing(value)) {
        private$.servings$value
      } else {
        private$.servings$value <- private$.servings$validate(value)
        invisible(self)
      }
    },
    time = function(value) {
      if (missing(value)) {
        private$.time$value
      } else {
        private$.time$value <- private$.time$validate(value)
        invisible(self)
      }
    },
    images = function(value) {
      if (missing(value)) {
        private$.images$value
      } else {
        private$.images$value <- private$.images$validate(value)
        invisible(self)
      }
    },
    ingredients = function(value) {
      if (missing(value)) {
        private$.ingredients$value
      } else {
        private$.ingredients$value <- private$.ingredients$validate(value)
        invisible(self)
      }
    },
    equipment = function(value) {
      if (missing(value)) {
        private$.equipment$value
      } else {
        private$.equipment$value <- private$.equipment$validate(value)
        invisible(self)
      }
    },
    instructions = function(value) {
      if (missing(value)) {
        private$.instructions$value
      } else {
        private$.instructions$value <- private$.instructions$validate(value)
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
    .summary =  list(
      value = NULL,
      validate = function(summary) {
        check_mode(summary, 'character')
        invisible(summary)
      }
    ),
    .servings =  list(
      value = NULL,
      validate = function(servings) {
        lapply(
          X = list(check_length, check_mode, check_number_in_range),
          FUN = rlang::exec,
          x = servings,
          n = 1:2,
          mode = 'numeric',
          range = c(0, Inf),
          inclusive = TRUE,
          allow_na = TRUE
        )
        invisible(servings)
      }
    ),
    .time =  list(
      value = NULL,
      validate = function(time) {
        time <- rlang::try_fetch(
          lapply(time, lubridate::as.duration),
          error = function(cnd) {
            rlang::abort(
              'Can\'t convert to duration object.',
              parent = cnd,
              call = rlang::caller_env()
            )
          }
        )
        lapply(
          X = time,
          FUN = check_number_in_range,
          range = c(0, Inf),
          inclusive = FALSE
        )
        invisible(time)
      }
    ),
    .images =  list(
      value = NULL,
      validate = function(images) {
        check_same_class(images, class = 'Image')
        invisible(images)
      }
    ),
    .ingredients =  list(
      value = NULL,
      validate = function(ingredients) {
        check_same_class(ingredients, class = 'Ingredient')
        invisible(ingredients)
      }
    ),
    .equipment =  list(
      value = NULL,
      validate = function(equipment) {
        check_same_class(equipment, class = 'Equipment')
        invisible(equipment)
      }
    ),
    .instructions =  list(
      value = NULL,
      validate = function(instructions) {
        check_same_class(instructions, class = 'Instruction')
        invisible(instructions)
      }
    )
  )
)
