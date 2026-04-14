#' Apply an artistic stylization mode to an image
#'
#' Transforms an image using one of five cinematic artistic modes, all built
#' on top of \code{\link{extract_grade}} / k-means quantization.  The result
#' is a new \code{cimg} object that can be displayed, saved, or passed to
#' other CineGrade functions.
#'
#' @param image Path to an image file (JPG/PNG) or an \pkg{imager} \code{cimg}
#'   object.
#' @param mode Character. One of:
#'   \describe{
#'     \item{\code{"poster"}}{Hard k-means colour quantization in LAB space.
#'       Each pixel is replaced by its nearest cluster centroid, converted back
#'       to RGB.  Produces a flat, graphic poster look.}
#'     \item{\code{"pixel"}}{Downsamples the image to a coarse block grid
#'       (\code{block_size} pixels per block), runs k-means on the downsampled
#'       pixels, then upsamples back to the original resolution.  Creates a
#'       retro pixel-art / video-game look.}
#'     \item{\code{"anime"}}{Pushes saturation to the extreme: LAB a* and b*
#'       channels are scaled by \code{sat_scale} before k-means snapping.
#'       Produces bold, high-chroma anime-style colouring.}
#'     \item{\code{"noir"}}{Desaturates the image (sets a* = b* = 0 in LAB)
#'       then applies a high-contrast 2-tone k-means snap (forced k = 2 dark
#'       and 2 light clusters).  Produces a classic black-and-white noir look
#'       with a faint cool tint.}
#'     \item{\code{"vintage"}}{Transfers the built-in vintage colour grade
#'       preset (\code{preset_vintage.jpg}) onto the image using
#'       \code{\link{apply_grade}}.  Warm, desaturated, faded tones.}
#'   }
#' @param k Integer. Number of colour clusters for quantization modes
#'   (\code{"poster"}, \code{"pixel"}, \code{"anime"}).  Ignored for
#'   \code{"noir"} (fixed at 4) and \code{"vintage"} (uses the preset).
#'   Default \code{8}.
#' @param block_size Integer. Block size in pixels for \code{"pixel"} mode
#'   (default \code{12}).  Larger = chunkier retro look.
#' @param sat_scale Numeric. Saturation multiplier for \code{"anime"} mode
#'   (default \code{1.8}).  Values > 1 boost chroma.
#'
#' @details
#' All modes except \code{"vintage"} are pure k-means operations in LAB
#' colour space -- the same technique as \code{\link{extract_grade}} and
#' \code{\link{apply_grade}}.  The key insight is that k-means quantization
#' \emph{is} the artistic operation: fixing the per-pixel colour to the
#' nearest centroid compresses the colour palette and creates the stylized
#' look.
#'
#' @return A \code{cimg} object with pixel values in \eqn{[0, 1]}, same
#'   spatial dimensions as \code{image}.
#'
#' @examples
#' img_path <- system.file("extdata", "sample_portrait.jpg", package = "CineGrade")
#' if (nchar(img_path) > 0) {
#'   poster  <- stylize(img_path, mode = "poster",  k = 8)
#'   px      <- stylize(img_path, mode = "pixel",   k = 6, block_size = 16)
#'   an      <- stylize(img_path, mode = "anime",   k = 8, sat_scale = 2.0)
#'   nr      <- stylize(img_path, mode = "noir")
#'   vt      <- stylize(img_path, mode = "vintage")
#' }
#'
#' @seealso \code{\link{apply_grade}}, \code{\link{extract_grade}},
#'   \code{\link{simulate_colorblind}}
#'
#' @export
stylize <- function(image,
                    mode       = c("poster", "pixel", "anime", "noir", "vintage"),
                    k          = 8L,
                    block_size = 12L,
                    sat_scale  = 1.8) {

  mode <- match.arg(mode)
  if (!is.numeric(k) || k < 1L) stop("`k` must be a positive integer.", call. = FALSE)
  k <- as.integer(k)
  block_size <- as.integer(block_size)

  # Load image
  if (inherits(image, "cimg")) {
    img <- image
  } else if (is.character(image) && length(image) == 1L) {
    img <- load_image(image)
  } else {
    stop("`image` must be a file path or a cimg object.", call. = FALSE)
  }

  switch(mode,

    # -- poster: k-means snap in LAB, each pixel = nearest centroid -------------
    "poster" = {
      lab_img  <- rgb_to_lab(img)
      pix      <- pixels_to_matrix(lab_img)
      set.seed(42L)
      km       <- stats::kmeans(pix, centers = k, nstart = 3L, iter.max = 30L)
      # Replace each pixel with its centroid colour
      snapped  <- km$centers[km$cluster, , drop = FALSE]
      snapped_img <- matrix_to_image(snapped, lab_img)
      lab_to_rgb(snapped_img)
    },

    # -- pixel: downsample -> k-means snap -> upsample ----------------------------
    "pixel" = {
      w <- imager::width(img)
      h <- imager::height(img)

      # Downsample to block grid
      w_small  <- max(1L, w %/% block_size)
      h_small  <- max(1L, h %/% block_size)
      img_small <- imager::resize(img, w_small, h_small, interpolation_type = 3L)

      # k-means snap on downsampled LAB
      lab_small <- rgb_to_lab(img_small)
      pix_small <- pixels_to_matrix(lab_small)
      set.seed(42L)
      km        <- stats::kmeans(pix_small, centers = min(k, nrow(pix_small)),
                                 nstart = 3L, iter.max = 30L)
      snapped_small <- km$centers[km$cluster, , drop = FALSE]
      snapped_lab   <- matrix_to_image(snapped_small, lab_small)
      rgb_small     <- lab_to_rgb(snapped_lab)

      # Upsample back to original size (nearest-neighbour = blocky)
      imager::resize(rgb_small, w, h, interpolation_type = 1L)
    },

    # -- anime: boost saturation then k-means snap -------------------------------
    "anime" = {
      lab_img <- rgb_to_lab(img)
      pix     <- pixels_to_matrix(lab_img)

      # Scale a* and b* channels (chroma boost)
      pix_boosted        <- pix
      pix_boosted[, 2L]  <- pix[, 2L] * sat_scale
      pix_boosted[, 3L]  <- pix[, 3L] * sat_scale

      set.seed(42L)
      km      <- stats::kmeans(pix_boosted, centers = k, nstart = 3L, iter.max = 30L)
      snapped <- km$centers[km$cluster, , drop = FALSE]

      # Undo the saturation scale on the snapped centroids so the
      # quantized colours stay in a valid LAB range
      snapped[, 2L] <- snapped[, 2L] / sat_scale
      snapped[, 3L] <- snapped[, 3L] / sat_scale

      snapped_img <- matrix_to_image(snapped, lab_img)
      lab_to_rgb(snapped_img)
    },

    # -- noir: desaturate -> high-contrast 2-tone k-means snap -------------------
    "noir" = {
      lab_img <- rgb_to_lab(img)
      pix     <- pixels_to_matrix(lab_img)

      # Desaturate: zero-out a* and b*, keep only L*
      pix_grey        <- pix
      pix_grey[, 2L]  <- 0
      pix_grey[, 3L]  <- 0

      # 4-cluster snap ( 2 dark + 2 light)
      set.seed(42L)
      km       <- stats::kmeans(pix_grey, centers = 4L, nstart = 5L, iter.max = 30L)
      snapped  <- km$centers[km$cluster, , drop = FALSE]

      # Add a very subtle cool blue tint (a* slightly negative, b* slightly negative)
      snapped[, 2L] <- -4
      snapped[, 3L] <- -8

      snapped_img <- matrix_to_image(snapped, lab_img)
      lab_to_rgb(snapped_img)
    },

    # -- vintage: transfer built-in vintage preset via apply_grade ---------------
    "vintage" = {
      vintage_path <- system.file("extdata", "preset_vintage.jpg", package = "CineGrade")
      if (nchar(vintage_path) == 0L) {
        stop(
          "Built-in 'preset_vintage.jpg' not found in inst/extdata/. ",
          "Reinstall the package to restore sample data.",
          call. = FALSE
        )
      }
      apply_grade(img, vintage_path, k = k)
    }
  )
}
