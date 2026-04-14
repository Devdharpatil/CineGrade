#' Simulate how an image appears to viewers with colour vision deficiency
#'
#' Applies a standard colour vision deficiency (CVD) transformation matrix to
#' simulate how an image looks to viewers who are deuteranopic, protanopic, or
#' tritanopic.  This is an \strong{accessibility and social-impact} feature --
#' use it to check whether a colour-graded image remains informative and
#' aesthetically coherent for viewers with impaired colour vision.
#'
#' @param image Path to an image file (JPG/PNG) or an \pkg{imager} \code{cimg}
#'   object.
#' @param type Character. The type of colour vision deficiency to simulate:
#'   \describe{
#'     \item{\code{"deuteranopia"}}{Green-blind (most common form, ~6\% of
#'       males).  Red-green colours are confused.}
#'     \item{\code{"protanopia"}}{Red-blind (~2\% of males).  Reds appear dark;
#'       red-green confusion similar to deuteranopia.}
#'     \item{\code{"tritanopia"}}{Blue-yellow blind (rare, ~0.01\%).  Blues
#'       and greens are confused; reds and pinks appear similar.}
#'   }
#'
#' @details
#' The simulation uses the standard \strong{Brettel-Vi__not-Mollon (1997)} /
#' \strong{Machado-Oliveira-Fernandes (2009)} linearised RGB transformation
#' matrices, which model the loss of one cone type.  These matrices operate
#' in \emph{linear} (gamma-decoded) sRGB space:
#' \enumerate{
#'   \item Gamma-expand the clamped \eqn{[0,1]} image values:
#'         \eqn{C_{linear} = C^{2.2}}.
#'   \item Multiply each pixel \eqn{[R, G, B]^T} by the \eqn{3 \times 3}
#'         CVD matrix.
#'   \item Re-apply gamma: \eqn{C_{sRGB} = \text{clamp}(C_{linear})^{1/2.2}}.
#' }
#' Clamping to \eqn{[0,1]} is applied after both gamma steps to handle
#' numerical overflow.
#'
#' @return A \code{cimg} object with pixel values in \eqn{[0, 1]}, same
#'   dimensions as \code{image}.
#'
#' @references
#' Machado, G., Oliveira, M., & Fernandes, L. (2009). A physiologically-based
#' model for simulation of color vision deficiency. \emph{IEEE Transactions on
#' Visualization and Computer Graphics}, 15(6), 1291-1298.
#'
#' @examples
#' img_path <- system.file("extdata", "sample_portrait.jpg", package = "CineGrade")
#' if (nchar(img_path) > 0) {
#'   deut <- simulate_colorblind(img_path, type = "deuteranopia")
#'   prot <- simulate_colorblind(img_path, type = "protanopia")
#'   trit <- simulate_colorblind(img_path, type = "tritanopia")
#' }
#'
#' @seealso \code{\link{stylize}} for artistic image transformations,
#'   \code{\link{apply_grade}} for colour grading.
#'
#' @export
simulate_colorblind <- function(image,
                                type = c("deuteranopia", "protanopia", "tritanopia")) {

  type <- match.arg(type)

  # Load image
  if (inherits(image, "cimg")) {
    img <- image
  } else if (is.character(image) && length(image) == 1L) {
    img <- load_image(image)
  } else {
    stop("`image` must be a file path or a cimg object.", call. = FALSE)
  }

  # -- CVD matrices (Machado et al. 2009, severity = 1.0) ----------------------
  # Row = output channel (R, G, B), Column = input channel (R, G, B)
  # Applied in LINEAR sRGB space (after gamma expansion).
  cvd_matrices <- list(

    deuteranopia = matrix(c(
      0.367322, 0.860646, -0.227968,
      0.280085, 0.672501,  0.047413,
     -0.011820, 0.042940,  0.968881
    ), nrow = 3L, byrow = TRUE),

    protanopia = matrix(c(
      0.152286, 1.052599, -0.204898,
      0.114503, 0.786281,  0.099216,
     -0.003882, -0.048116, 1.051998
    ), nrow = 3L, byrow = TRUE),

    tritanopia = matrix(c(
      1.255528, -0.076749, -0.178779,
     -0.078411,  0.930809,  0.147602,
      0.004733,  0.691367,  0.303900
    ), nrow = 3L, byrow = TRUE)
  )

  cvd_mat <- cvd_matrices[[type]]

  # -- Pixel matrix from RGB image ----------------------------------------------
  w   <- imager::width(img)
  h   <- imager::height(img)
  pix <- pixels_to_matrix(img)   # (W*H) x 3, values in [0, 1]

  # -- Step 1: Gamma expand (sRGB -> linear RGB), clamp first -------------------
  pix_lin <- matrix(pmax(0, pmin(1, pix)), ncol = 3L)^2.2

  # -- Step 2: Apply CVD transformation matrix ----------------------------------
  # pix_lin: (n x 3), cvd_mat: (3 x 3) -> pix_cvd: (n x 3)
  pix_cvd <- pix_lin %*% t(cvd_mat)

  # -- Step 3: Gamma compress (linear -> sRGB), clamp ---------------------------
  pix_out <- matrix(pmax(0, pmin(1, pix_cvd)), ncol = 3L)^(1 / 2.2)

  # -- Repack into cimg ---------------------------------------------------------
  arr <- array(0, dim = c(w, h, 1L, 3L))
  arr[, , 1L, 1L] <- matrix(pix_out[, 1L], nrow = w, ncol = h)
  arr[, , 1L, 2L] <- matrix(pix_out[, 2L], nrow = w, ncol = h)
  arr[, , 1L, 3L] <- matrix(pix_out[, 3L], nrow = w, ncol = h)
  imager::as.cimg(arr)
}
