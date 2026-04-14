#' Extract the color grade from an image
#'
#' Runs k-means clustering on pixel data in LAB perceptual color space
#' to extract a compact statistical signature of the image's color story.
#' LAB space is used instead of RGB because Euclidean distances in LAB
#' correspond to human perception of color difference, making the clusters
#' more visually meaningful.
#'
#' @param image Path to an image file (JPG/PNG) or an \pkg{imager} \code{cimg}
#'   object.
#' @param k Integer. Number of color clusters (default \code{8}). Higher \code{k}
#'   gives a more detailed grade capturing subtle tones; lower \code{k} gives a
#'   more stylized, posterized result.
#' @param colorspace Character. Color space used for clustering. Currently only
#'   \code{"LAB"} is supported (default). RGB clustering is intentionally not
#'   provided -- see Details.
#'
#' @details
#' \strong{Why LAB?} The RGB color model is designed for display hardware, not
#' human perception. Euclidean distances in RGB do not correlate well with how
#' different two colors look to the human eye. The CIE L*a*b* (LAB) color space
#' is \emph{perceptually uniform}: equal Euclidean distances correspond to
#' approximately equal perceived color differences. Running k-means in LAB
#' therefore produces clusters that are more semantically meaningful -- each
#' centroid represents a visually distinct "color mood" in the image.
#'
#' \strong{Algorithm:}
#' \enumerate{
#'   \item Load the image (if a path is given) using \code{\link{load_image}}.
#'   \item Convert from sRGB to LAB via \code{\link{rgb_to_lab}}.
#'   \item Flatten the image to an \eqn{(W \times H) \times 3} pixel matrix.
#'   \item Run \code{\link[stats]{kmeans}} with \code{nstart = 5} for stability.
#'   \item Return the cluster centers, per-pixel labels, \code{k}, and the
#'         color space name.
#' }
#'
#' @return A named list with four components:
#'   \describe{
#'     \item{\code{centers}}{A \eqn{k \times 3} numeric matrix of cluster centers
#'       in LAB space. Columns are \code{L} (lightness, 0-100),
#'       \code{a} (green-red axis), and \code{b} (blue-yellow axis).}
#'     \item{\code{labels}}{Integer vector of length \eqn{W \times H} giving the
#'       cluster assignment (1 to \code{k}) for each pixel, in column-major order
#'       matching \pkg{imager}'s internal layout.}
#'     \item{\code{k}}{The value of \code{k} that was used.}
#'     \item{\code{colorspace}}{Character string -- the color space used for
#'       clustering (currently always \code{"LAB"}).}
#'   }
#'
#' @examples
#' img_path <- system.file("extdata", "sample_portrait.jpg", package = "CineGrade")
#' if (nchar(img_path) > 0) {
#'   grade <- extract_grade(img_path, k = 8)
#'   print(grade$centers)
#'   cat("Pixels assigned to cluster 1:", sum(grade$labels == 1), "\n")
#' }
#'
#' @seealso \code{\link{apply_grade}} to transfer this grade to another image,
#'   \code{\link{compare_grades}} to measure similarity between two grades.
#'
#' @importFrom stats kmeans
#' @export
extract_grade <- function(image, k = 8L, colorspace = "LAB") {

  # -- Input validation --------------------------------------------------------
  if (!is.numeric(k) || length(k) != 1L || k < 1L) {
    stop("`k` must be a positive integer (e.g. 8).", call. = FALSE)
  }
  k <- as.integer(k)

  if (!identical(colorspace, "LAB")) {
    stop(
      "`colorspace` must be \"LAB\". RGB clustering is not supported -- ",
      "see ?extract_grade for the statistical rationale.",
      call. = FALSE
    )
  }

  # -- Load image --------------------------------------------------------------
  if (inherits(image, "cimg")) {
    img_rgb <- image
  } else if (is.character(image) && length(image) == 1L) {
    img_rgb <- load_image(image)
  } else {
    stop(
      "`image` must be a file path (character) or a cimg object.",
      call. = FALSE
    )
  }

  # -- Convert to LAB ----------------------------------------------------------
  img_lab <- rgb_to_lab(img_rgb)

  # -- Flatten to pixel matrix -------------------------------------------------
  pixel_mat <- pixels_to_matrix(img_lab)   # (W*H) x 3

  # -- Run k-means -------------------------------------------------------------
  # nstart = 5: run 5 random initialisations, keep the best (lowest within-SS)
  # iter.max = 30: generous iteration budget for convergence
  set.seed(42L)   # reproducibility; users can override via set.seed() before calling
  km <- stats::kmeans(pixel_mat, centers = k, nstart = 5L, iter.max = 30L)

  # -- Name the centroid columns ------------------------------------------------
  centers <- km$centers
  colnames(centers) <- c("L", "a", "b")

  # -- Return -------------------------------------------------------------------
  structure(
    list(
      centers    = centers,
      labels     = km$cluster,
      k          = k,
      colorspace = colorspace
    ),
    class = "CineGrade"
  )
}


#' Print method for CineGrade objects
#'
#' @param x A \code{CineGrade} object returned by \code{\link{extract_grade}}.
#' @param ... Further arguments (unused).
#' @return Invisibly returns \code{x}.
#' @export
print.CineGrade <- function(x, ...) {
  cat(sprintf(
    "CineGrade color grade  [k = %d, colorspace = %s]\n\n",
    x$k, x$colorspace
  ))
  cat("Cluster centers (LAB):\n")
  print(round(x$centers, 2L))
  cat(sprintf(
    "\n%d pixels total across %d clusters.\n",
    length(x$labels), x$k
  ))
  invisible(x)
}
