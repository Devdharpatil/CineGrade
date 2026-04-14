# utils.R -- Internal helper functions for CineGrade.
# These functions are not exported. They provide shared image I/O,
# color-space conversion, and matrix reshaping utilities.

# ---- Image Loading ----------------------------------------------------------

#' Load an image from disk
#'
#' Reads a JPEG or PNG image file and returns it as an \code{imager} \code{cimg}
#' object, ready for pixel-level processing. Supports both absolute and
#' relative paths. Stops with an informative message if the file is not found
#' or the format is unsupported.
#'
#' @param path Character. Absolute or relative path to a JPG or PNG image file.
#'
#' @return A \code{cimg} object (from the \pkg{imager} package) with four
#'   dimensions: width x height x depth x channels.  For standard colour
#'   images, depth = 1 and channels = 3 (R, G, B), all values in \[0, 1\].
#'
#' @examples
#' \dontrun{
#'   img_path <- system.file("extdata", "sample_portrait.jpg", package = "CineGrade")
#'   img <- load_image(img_path)
#'   imager::plot(img)
#' }
#'
#' @importFrom imager load.image
#' @keywords internal
load_image <- function(path) {
  if (!is.character(path) || length(path) != 1L) {
    stop("`path` must be a single character string.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("Image file not found: ", path, call. = FALSE)
  }
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("jpg", "jpeg", "png")) {
    stop("Unsupported image format '", ext, "'. Use JPG or PNG.", call. = FALSE)
  }
  imager::load.image(path)
}


# ---- Color-Space Conversion -------------------------------------------------

#' Convert a cimg object from RGB to LAB color space
#'
#' Converts an \pkg{imager} \code{cimg} image from the standard sRGB color
#' space to the CIE L*a*b* (LAB) perceptual color space via the D65 white
#' point.  LAB is used for all color-grade clustering in CineGrade because
#' Euclidean distances in LAB correspond closely to human perception of color
#' difference, unlike RGB.
#'
#' Internally the function:
#' \enumerate{
#'   \item Extracts the R, G, B channel matrices from the \code{cimg}.
#'   \item Assembles them into a three-column numeric matrix accepted by
#'         \code{\link[colorspace]{sRGB}}.
#'   \item Converts that matrix to \code{LAB} via \pkg{colorspace}.
#'   \item Returns a new \code{cimg} with the three LAB channels packed in
#'         place of the original R, G, B channels.
#' }
#'
#' @param img A \code{cimg} object in RGB color space (channel values in
#'   \[0, 1\]).
#'
#' @return A \code{cimg} object of identical dimensions where the three colour
#'   channels now represent L* (range 0--100), a* (roughly -128 to 127), and
#'   b* (roughly -128 to 127).
#'
#' @examples
#' \dontrun{
#'   img_path <- system.file("extdata", "sample_portrait.jpg", package = "CineGrade")
#'   rgb_img  <- load_image(img_path)
#'   lab_img  <- rgb_to_lab(rgb_img)
#' }
#'
#' @importFrom imager channel as.cimg
#' @importFrom colorspace sRGB LAB coords
#' @keywords internal
rgb_to_lab <- function(img) {
  if (!inherits(img, "cimg")) {
    stop("`img` must be a cimg object.", call. = FALSE)
  }
  w <- imager::width(img)
  h <- imager::height(img)

  # Extract R, G, B channels as flat numeric vectors
  r <- as.vector(imager::channel(img, 1L))
  g <- as.vector(imager::channel(img, 2L))
  b <- as.vector(imager::channel(img, 3L))

  # Convert via colorspace: sRGB -> LAB (D65 white point)
  rgb_mat <- colorspace::sRGB(R = r, G = g, B = b)
  lab_mat  <- colorspace::coords(methods::as(rgb_mat, "LAB"))

  # Reconstruct a cimg from the LAB channel arrays
  arr <- array(0, dim = c(w, h, 1L, 3L))
  arr[, , 1L, 1L] <- matrix(lab_mat[, 1L], nrow = w, ncol = h)
  arr[, , 1L, 2L] <- matrix(lab_mat[, 2L], nrow = w, ncol = h)
  arr[, , 1L, 3L] <- matrix(lab_mat[, 3L], nrow = w, ncol = h)
  imager::as.cimg(arr)
}


#' Convert a cimg object from LAB color space back to RGB
#'
#' Inverts \code{\link{rgb_to_lab}}: takes a \code{cimg} whose three color
#' channels encode CIE L*a*b* values and returns a \code{cimg} in standard
#' sRGB, with channel values clamped to \[0, 1\].
#'
#' @param img A \code{cimg} object in LAB color space (as produced by
#'   \code{\link{rgb_to_lab}} or the internal color-transfer routines).
#'
#' @return A \code{cimg} object in sRGB space, channel values in \[0, 1\].
#'
#' @examples
#' \dontrun{
#'   img_path <- system.file("extdata", "sample_portrait.jpg", package = "CineGrade")
#'   rgb_img  <- load_image(img_path)
#'   lab_img  <- rgb_to_lab(rgb_img)
#'   back     <- lab_to_rgb(lab_img)
#' }
#'
#' @importFrom imager channel as.cimg
#' @importFrom colorspace LAB sRGB coords
#' @keywords internal
lab_to_rgb <- function(img) {
  if (!inherits(img, "cimg")) {
    stop("`img` must be a cimg object.", call. = FALSE)
  }
  w <- imager::width(img)
  h <- imager::height(img)

  # Extract L, a, b channels as flat vectors
  l_ch <- as.vector(imager::channel(img, 1L))
  a_ch <- as.vector(imager::channel(img, 2L))
  b_ch <- as.vector(imager::channel(img, 3L))

  # Convert via colorspace: LAB -> sRGB
  lab_obj <- colorspace::LAB(L = l_ch, A = a_ch, B = b_ch)
  rgb_mat  <- as.matrix(colorspace::coords(methods::as(lab_obj, "sRGB")))

  # Clamp to [0, 1] -- pmax/pmin on a matrix returns a vector, so re-wrap
  rgb_mat <- matrix(pmax(0, pmin(1, rgb_mat)), ncol = 3L)

  # Reconstruct cimg from RGB arrays
  arr <- array(0, dim = c(w, h, 1L, 3L))
  arr[, , 1L, 1L] <- matrix(rgb_mat[, 1L], nrow = w, ncol = h)
  arr[, , 1L, 2L] <- matrix(rgb_mat[, 2L], nrow = w, ncol = h)
  arr[, , 1L, 3L] <- matrix(rgb_mat[, 3L], nrow = w, ncol = h)
  imager::as.cimg(arr)
}


# ---- Matrix / Image Reshaping -----------------------------------------------

#' Flatten a cimg to an n_pixels x 3 numeric matrix
#'
#' Reshapes a \code{cimg} object (width x height x 1 x 3) into a plain
#' numeric matrix with one row per pixel and one column per color channel.
#' This is the format consumed by \code{stats::kmeans()} and other statistical
#' routines in CineGrade.
#'
#' @param img A \code{cimg} object with exactly 3 color channels (depth = 1).
#'
#' @return A numeric matrix of dimension \eqn{(W \times H) \times 3}, where W
#'   and H are the image width and height respectively.  Row ordering follows
#'   column-major (Fortran/R) convention, matching \pkg{imager}'s internal
#'   layout.  Column names are \code{c("c1", "c2", "c3")}.
#'
#' @examples
#' \dontrun{
#'   img_path <- system.file("extdata", "sample_portrait.jpg", package = "CineGrade")
#'   img      <- load_image(img_path)
#'   mat      <- pixels_to_matrix(img)
#'   dim(mat)  # (width * height) x 3
#' }
#'
#' @importFrom imager channel
#' @keywords internal
pixels_to_matrix <- function(img) {
  if (!inherits(img, "cimg")) {
    stop("`img` must be a cimg object.", call. = FALSE)
  }
  if (imager::spectrum(img) != 3L) {
    stop("`img` must have exactly 3 colour channels.", call. = FALSE)
  }
  c1 <- as.vector(imager::channel(img, 1L))
  c2 <- as.vector(imager::channel(img, 2L))
  c3 <- as.vector(imager::channel(img, 3L))
  mat <- cbind(c1, c2, c3)
  colnames(mat) <- c("c1", "c2", "c3")
  mat
}


#' Reshape a pixel matrix back into a cimg image
#'
#' Inverts \code{\link{pixels_to_matrix}}: takes an \eqn{n \times 3} numeric
#' matrix (where \eqn{n = W \times H}) and an original \code{cimg} object
#' (used only for its spatial dimensions), and returns a new \code{cimg} whose
#' pixel values come from the matrix.
#'
#' This is used after statistical transformations (e.g., shifting pixel values
#' by centroid deltas in \code{apply_grade}) to reconstruct a displayable
#' image.
#'
#' @param mat  Numeric matrix of dimension \eqn{(W \times H) \times 3}.
#'   Values should be in the valid range for the target colour space
#'   (e.g., \[0, 1\] for sRGB, or appropriate LAB ranges).
#' @param orig_img A \code{cimg} object whose width, height, and depth
#'   attributes define the output image shape.  Its pixel values are
#'   \emph{not} used.
#'
#' @return A \code{cimg} object with the same spatial dimensions as
#'   \code{orig_img} and channel values taken from \code{mat}.
#'
#' @examples
#' \dontrun{
#'   img_path <- system.file("extdata", "sample_portrait.jpg", package = "CineGrade")
#'   img      <- load_image(img_path)
#'   mat      <- pixels_to_matrix(img)
#'   # ... modify mat ...
#'   new_img  <- matrix_to_image(mat, img)
#' }
#'
#' @importFrom imager as.cimg
#' @keywords internal
matrix_to_image <- function(mat, orig_img) {
  if (!is.matrix(mat) || ncol(mat) != 3L) {
    stop("`mat` must be a numeric matrix with 3 columns.", call. = FALSE)
  }
  if (!inherits(orig_img, "cimg")) {
    stop("`orig_img` must be a cimg object.", call. = FALSE)
  }
  w <- imager::width(orig_img)
  h <- imager::height(orig_img)
  expected_n <- w * h
  if (nrow(mat) != expected_n) {
    stop(
      "`mat` has ", nrow(mat), " rows but orig_img has ",
      expected_n, " pixels (", w, " x ", h, ").",
      call. = FALSE
    )
  }
  arr <- array(0, dim = c(w, h, 1L, 3L))
  arr[, , 1L, 1L] <- matrix(mat[, 1L], nrow = w, ncol = h)
  arr[, , 1L, 2L] <- matrix(mat[, 2L], nrow = w, ncol = h)
  arr[, , 1L, 3L] <- matrix(mat[, 3L], nrow = w, ncol = h)
  imager::as.cimg(arr)
}
