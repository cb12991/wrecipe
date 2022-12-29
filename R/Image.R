#' R6 Class for images associated with a recipe
#'
#' Recipe images are not required, but helps the viewer visualize what the
#' recipe is supposed to look like. An `Image` can either be a local file or a
#' URL that points to the file. In the case of the latter, a method is included
#' to download the image.
#'
#' This class also includes a method to "plot" the image given it is a
#' recognized image format (.jpeg or .png as of now). This helps view which
#' images to add to a [Recipe] object without having to open up the image
#' outside of RStudio.
#'
#' @section Creating the image object:
#' An `Image` is an R6 object, and it can be created using
#' `Image$new()`. A file path or URL pointing to a valid image is required
#' upon initialization; the name of the image can be determined automatically if
#' not provided.
#'
#' @field file
#'   Scalar character vector; the path to an image file. This image needs to
#'   be of JPEG or PNG format.
#' @field url
#'   Scalar character vector; the URL pointing to an image. Also needs to be
#'   of JPEG or PNG format.
#' @field name
#'   Scalar character vector; the name of the image, for reference. You can
#'   specify directly or omit and the name will be determined using the image
#'   file path or URL.
#'
#' @family recipe components
#' @seealso [Recipe]
#' @export
#' @name Image
NULL

Image <- R6::R6Class(
  classname = 'Image',

  public = list(
    initialize = function(
      file = NULL,
      url = NULL,
      name = basename(file %||% url)
    ) {
      if (is.null(file) & is.null(url)) {
        cli::cli_abort(
          'Must provide either a URL or file path to an image.',
          call = rlang::caller_env()
        )
      }
      private$.file$value <- private$.file$validate(file, rlang::caller_env())
      private$.url$value <- private$.url$validate(url, rlang::caller_env())
      private$.name$value <- private$.name$validate(name, rlang::caller_env())
    },

    print = function() {
      cat('<Image>')
      cat(strwrap(private$.name$value, prefix = '  '))
      invisible(self)
    },

    download = function() {
      if (is.null(url)) {
        cli::cli_abort('Cannot download image without providing a URL.')
      }
      private$.file$value <- xml2::download_html(private$.url$value)
      invisible(self)
    },

    plot = function(add = FALSE) {
      if (is.null(private$.file$value)) {
        self$download()
      }
      fmt <- mime::guess_type(private$.file$value %||% private$.url$value)

      if (!grepl('^image', fmt)) {
        cli::cli_abort(
          'Image format not recognized: {.field {fmt}}.',
          call = rlang::caller_env()
        )
      }

      bitmap <- switch(
        fmt,
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
        if (is.null(value) && is.null(private$.url$value)) {
          cli::cli_abort(
            'Cannot set {.field file} to {.value NULL} if {.field url} is
             {.value NULL}.',
            call = rlang::caller_env()
          )
        }
        private$.file$value <- private$.file$validate(
          value,
          error_call = rlang::caller_env()
        )
        invisible(self)
      }
    },
    url = function(value) {
      if (missing(value)) {
        private$.url$value
      } else {
        if (is.null(value) && is.null(private$.file$value)) {
          cli::cli_abort(
            'Cannot set {.field url} to {.value NULL} if {.field file} is
             {.value NULL}.',
            call = rlang::caller_env()
          )
        }
        private$.url$value <- private$.url$validate(
          value,
          error_call = rlang::caller_env()
        )
        invisible(self)
      }
    },
    name = function(value) {
      if (missing(value)) {
        private$.name$value
      } else {
        private$.name$value <- private$.name$validate(value, rlang::caller_env())
        invisible(self)
      }
    }
  ),

  private = list(
    .file = list(
      value = NULL,
      validate = function(file, error_call) {
        lapply(
          X = list(check_length, check_mode, check_file_exists),
          FUN = rlang::exec,
          x = file,
          n = 1,
          allow_null = TRUE,
          mode = 'character',
          call = error_call
        )
        invisible(file)
      }
    ),

    .url = list(
      value = NULL,
      validate = function(url, error_call) {
        lapply(
          X = list(check_length, check_mode, check_url_connection),
          FUN = rlang::exec,
          x = url,
          n = 1,
          allow_null = TRUE,
          mode = 'character',
          call = error_call
        )
        invisible(url)
      }
    ),

    .name = list(
      value = NULL,
      validate = function(name, error_call) {
        lapply(
          X = list(check_length, check_mode),
          FUN = rlang::exec,
          x = name,
          n = 1,
          mode = 'character',
          call = error_call
        )
        invisible(name)
      }
    )
  )
)
