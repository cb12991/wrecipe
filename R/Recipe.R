cbmisc::pak_load(R6, rlang, assertthat, mime, httr, xml2, jpeg, lubridate, png)

Recipe <- R6Class(
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
    name = function(value) {
      if (missing(value)) {
        private$.name$value
      } else {
        private$.name$value <- private$.name$validate(value)
        self
      }
    },
    summary = function(value) {
      if (missing(value)) {
        private$.summary$value
      } else {
        private$.summary$value <- private$.summary$validate(value)
        self
      }
    },
    images = function(value) {
      if (missing(value)) {
        private$.images$value
      } else {
        private$.images$value <- private$.images$validate(value)
        self
      }
    },
    ingredients = function(value) {
      if (missing(value)) {
        private$.ingredients$value
      } else {
        private$.ingredients$value <- private$.ingredients$validate(value)
        self
      }
    },
    equipment = function(value) {
      if (missing(value)) {
        private$.equipment$value
      } else {
        private$.equipment$value <- private$.equipment$validate(value)
        self
      }
    },
    instructions = function(value) {
      if (missing(value)) {
        private$.instructions$value
      } else {
        private$.instructions$value <- private$.instructions$validate(value)
        self
      }
    },
    servings = function(value) {
      if (missing(value)) {
        private$.servings$value
      } else {
        private$.servings$value <- private$.servings$validate(value)
        self
      }
    }
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

Image <- R6Class(
  classname = 'Image',
  private = list(
    .file = list(
      value = NULL,
      validate = function(x) {
        if (!is.null(x)) {
          if (length(x) != 1) {
            cli::cli_abort(c(
              'Length != 1',
              i = 'Only provide one file path per image.'
            ))
          }
          if (!is.character(x)) {
            cli::cli_abort(
              'Is not a character vector.'
            )
          }
          if (!file.exists(x)) {
            cli::cli_abort(
              'Could not find file at {.file {x}}.'
            )
          }
        }
        invisible(x)
      }
    ),

    .url = list(
      value = NULL,
      validate = function(x) {
        if (!is.null(x)) {
          if (length(x) != 1) {
            cli::cli_abort(c(
              'Length != 1',
              i = 'Only provide one URL path per image.'
            ))
          }
          if (!is.character(x)) {
            cli::cli_abort(
              'Is not a character vector.'
            )
          }
          if (GET(x)$status_code != 200) {
            cli::cli_abort(
              'URL {.url {url}} did not result in successful HTTP response.'
            )
          }
        }
        invisible(x)
      }
    ),

    .name = list(
      value = NULL,
      validate = function(x) {
        if (!is.null(x)) {
          if (length(x) != 1) {
            cli::cli_abort(c(
              'Length != 1',
              i = 'Only provide one name per image.'
            ))
          }
          if (!is.character(x)) {
            cli::cli_abort(
              'Is not a character vector.'
            )
          }
        }
        invisible(x)
      }
    ),

    .img_format = list(
      value = NULL,
      validate = function(x) {
        img_format <- guess_type(x)
        if (!grepl('^image', img_format)) {
          cli::cli_abort(c(
            'The following file does not appear to be an image.',
            ' ' = '{.file {x}}'
          ))
        }
        invisible(img_format)
      }
    )
  ),

  active = list(
    file = function(value) {
      if (missing(value)) {
        private$.file$value
      } else {
        private$.file$value <- private$.file$validate(value)
        self
      }
    },

    url = function(value) {
      if (missing(value)) {
        private$.url$value
      } else {
        private$.url$value <- private$.url$validate(value)
        self
      }
    },

    name = function(value) {
      if (missing(value)) {
        private$.name$value
      } else {
        private$.name$value <- private$.name$validate(value)
        self
      }
    },

    img_format = function(value) {
      if (missing(value)) {
        private$.img_format
      } else {
        private$.img_format$value <- private$.img_format$validate(value)
        self
      }
    }
  ),

  public = list(
    initialize = function(
      file = NULL,
      url = NULL,
      name = NA_character_
    ) {
      if (is.null(file) & is.null(url)) {
        cli::cli_abort('Must provide either a URL or file path to an image.')
      }

      private$.file$value <- private$.file$validate(file)
      private$.url$value <- private$.url$validate(url)
      private$.name$value <- private$.name$validate(name)
      private$.img_format$value <- private$.img_format$validate(file %||% url)

      invisible(self)
    },

    download = function() {
      private$.file$value <- download_html(private$.url$value)
      invisible(self)
    },

    plot = function(add = FALSE) {
      if (is.null(private$.file$value)) {
        self$download()
      }

      bitmap <- switch(
        private$.img_format$value,
        'image/png' = readPNG(private$.file$value, native = TRUE),
        'image/jpeg' = readJPEG(private$.file$value, native = TRUE)
      )
      res <- dim(bitmap)[2:1]

      # Initialize an empty plot area if add == FALSE.
      if (!add) {
        plot(
          1, 1, xlim = c(0, res[1]), ylim = c(0, res[2]), asp = 1, type = 'n',
          xaxs = 'i', yaxs = 'i', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '',
          bty = 'n'
        )
      }
      rasterImage(bitmap, 0, 0, res[1], res[2])
      invisible(self)
    }
  )
)

Ingredient <- R6Class(
  classname = 'Ingredient',
  public = list(
    name = NULL,
    amount = NA_real_,
    unit = NA_character_,

    initialize = function(name, amount, unit = NA_character_) {
      assert_that(is.character(name), length(name) == 1)
      # assert_that(is.numeric(amount), length(amount) == 1)
      assert_that(is.character(unit), length(unit) == 1)

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

Equipment <- R6Class(
  classname = 'Equipment',
  public = list(
    initialize = function(name, quantity = NA_integer_) {
      assert_that(is.character(name), length(name) == 1)
      assert_that(is.numeric(quantity))

      self$name <- name
      self$quantity <- quantity
    }
  )
)

Instruction <- R6Class(
  classname = 'Instruction',
  public = list(
    step = 0,

    initialize = function(text, time, active = TRUE) {
      assert_that(is.character(text), length(text) == 1)
      assert_that(is.duration(time), length(time) == 1)
      assert_that(is.logical(active), length(active) == 1)

      self$step <- self$step + 1
      self$text <- text
      self$time <- time
      self$active <- active
    }
  )
)

Serving <- R6Class(
  classname = 'Serving',
  public = list(
    initialize = function(amount, unit = 'servings') {
      assert_that(is.numeric(amount), length(amount) %in% 1:2)
      assert_that(is.character(unit), length(unit) == 1)

      self$amount <- amount
      self$unit <- unit
    }
  )
)
