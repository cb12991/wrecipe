#' @importFrom rlang %||%
#' @export
Image <- R6::R6Class(
  classname = 'Image',

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
      private$.file$value <- xml2::download_html(private$.url$value)
      invisible(self)
    },

    plot = function(add = FALSE) {
      if (is.null(private$.file$value)) {
        self$download()
      }

      bitmap <- switch(
        private$.img_format$value,
        'image/png' = png::readPNG(private$.file$value, native = TRUE),
        'image/jpeg' = jpeg::readJPEG(private$.file$value, native = TRUE)
      )
      res <- dim(bitmap)[2:1]

      # Initialize an empty plot area if add == FALSE.
      if (!add) {
        args <- rlang::exprs(
          x = 1, y = 1, xlim = !!c(0, res[1]), ylim = !!c(0, res[2]), asp = 1,
          type = 'n', xaxs = 'i', yaxs = 'i', xaxt = 'n', yaxt = 'n', xlab = '',
          ylab = '', bty = 'n'
        )
        rlang::exec(plot, !!!args)
      }
      rasterImage(bitmap, 0, 0, res[1], res[2])
      invisible(self)
    }
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
        private$.img_format$value
      } else {
        private$.img_format$value <- private$.img_format$validate(value)
        self
      }
    }
  ),

  private = list(
    .file = list(
      value = NULL,
      validate = function(x) {
        if (!is.null(x)) {
          if (length(x) != 1) {
            cli::cli_abort(c(
              'Length != 1',
              '',
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
              '',
              i = 'Only provide one URL path per image.'
            ))
          }
          if (!is.character(x)) {
            cli::cli_abort(
              'Is not a character vector.'
            )
          }
          if (httr::GET(x)$status_code != 200) {
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
        img_format <- mime::guess_type(x)
        if (!grepl('^image', img_format)) {
          cli::cli_abort(c(
            'The following file does not appear to be an image:',
            ' ' = '{.file {x}}'
          ))
        }
        invisible(img_format)
      }
    )
  )
)
