# Suppress R CMD CHECK NSE notes for ggplot2 column names used in aes()
utils::globalVariables(c("umap1", "umap2", "colour"))
#'
#' Subsamples \code{n_pixels} random pixels from \code{image}, embeds their
#' 3-dimensional LAB color coordinates into 2D using UMAP (Uniform Manifold
#' Approximation and Projection), and renders each pixel as a colored dot at
#' its 2D position.  The resulting plot is a visual \strong{color fingerprint}:
#' sunset photos form warm-toned gradients, forest scenes cluster in tight
#' green clouds, and noir films produce dark, tightly-packed distributions.
#'
#' @param image Path to an image file (JPG/PNG) or an \pkg{imager} \code{cimg}
#'   object.
#' @param n_pixels Integer. Number of pixels to subsample before running UMAP
#'   (default \code{5000}).  Larger values give a richer plot but take longer.
#'   Capped automatically if the image has fewer pixels than \code{n_pixels}.
#' @param n_neighbors Integer. UMAP neighbourhood size (default \code{15}).
#'   Smaller values resolve fine local colour clusters; larger values emphasise
#'   the global colour layout.
#' @param min_dist Numeric. UMAP minimum distance parameter (default
#'   \code{0.1}).  Smaller values pack points more tightly.
#' @param title Character. Optional plot title.  Defaults to
#'   \code{"Color Universe"}.
#' @param seed Integer. Random seed for reproducible UMAP and pixel sampling
#'   (default \code{42L}).
#'
#' @details
#' \strong{Why LAB for UMAP?}  UMAP looks for structure in Euclidean distance.
#' Because Euclidean distances in LAB correspond to perceived colour differences
#' (unlike RGB), running UMAP in LAB space produces a layout where nearby dots
#' genuinely look similar to the human eye.
#'
#' \strong{Pipeline:}
#' \enumerate{
#'   \item Load image and convert to LAB via \code{\link{rgb_to_lab}}.
#'   \item Flatten to a pixel matrix and subsample \code{n_pixels} rows at
#'         random (without replacement).
#'   \item Run \code{\link[uwot]{umap}} on the \eqn{n \times 3} LAB matrix to
#'         obtain 2D coordinates.
#'   \item Build a \pkg{ggplot2} scatter plot where each dot is coloured with
#'         the pixel's actual sRGB hex colour (via
#'         \code{\link[ggplot2]{scale_colour_identity}}).
#' }
#'
#' @return A \code{ggplot} object.  Print it to display, or save it with
#'   \code{\link[ggplot2]{ggsave}}.
#'
#' @examples
#' img_path <- system.file("extdata", "preset_wes.jpg", package = "CineGrade")
#' if (nchar(img_path) > 0) {
#'   p <- color_universe(img_path, n_pixels = 2000)
#'   print(p)
#' }
#'
#' @seealso \code{\link{palette_pca}} for comparing multiple images in PCA
#'   space, \code{\link{extract_grade}} for the k-means colour grade.
#'
#' @importFrom uwot umap
#' @importFrom ggplot2 ggplot aes geom_point scale_colour_identity labs
#'   theme_void theme element_text element_rect margin
#' @importFrom grDevices rgb
#' @export
color_universe <- function(image,
                           n_pixels    = 5000L,
                           n_neighbors = 15L,
                           min_dist    = 0.1,
                           title       = "Color Universe",
                           seed        = 42L) {

  # -- Input validation ---------------------------------------------------------
  if (!is.numeric(n_pixels) || length(n_pixels) != 1L || n_pixels < 10L) {
    stop("`n_pixels` must be a positive integer >= 10.", call. = FALSE)
  }
  if (!is.numeric(n_neighbors) || length(n_neighbors) != 1L || n_neighbors < 2L) {
    stop("`n_neighbors` must be an integer >= 2.", call. = FALSE)
  }
  n_pixels    <- as.integer(n_pixels)
  n_neighbors <- as.integer(n_neighbors)

  # -- Load image ---------------------------------------------------------------
  if (inherits(image, "cimg")) {
    img_rgb <- image
  } else if (is.character(image) && length(image) == 1L) {
    img_rgb <- load_image(image)
  } else {
    stop("`image` must be a file path or a cimg object.", call. = FALSE)
  }

  # -- Pixel matrices (RGB for colour, LAB for UMAP) ----------------------------
  rgb_mat <- pixels_to_matrix(img_rgb)          # (W*H) x 3, values in [0,1]
  img_lab <- rgb_to_lab(img_rgb)
  lab_mat <- pixels_to_matrix(img_lab)          # (W*H) x 3, LAB values

  total_pixels <- nrow(rgb_mat)
  n_sample     <- min(n_pixels, total_pixels)

  # -- Subsample ----------------------------------------------------------------
  set.seed(seed)
  idx         <- sample.int(total_pixels, size = n_sample, replace = FALSE)
  lab_sample  <- lab_mat[idx, , drop = FALSE]
  rgb_sample  <- rgb_mat[idx, , drop = FALSE]

  # -- Convert sampled RGB to hex colours (for ggplot2 point colours) -----------
  hex_colours <- grDevices::rgb(
    red   = rgb_sample[, 1L],
    green = rgb_sample[, 2L],
    blue  = rgb_sample[, 3L],
    maxColorValue = 1
  )

  # -- Run UMAP on LAB sample --------------------------------------------------
  set.seed(seed)
  embedding <- uwot::umap(
    X           = lab_sample,
    n_neighbors = n_neighbors,
    min_dist    = min_dist,
    n_epochs    = 200L,
    verbose     = FALSE
  )

  # -- Assemble plot data -------------------------------------------------------
  plot_df <- data.frame(
    umap1  = embedding[, 1L],
    umap2  = embedding[, 2L],
    colour = hex_colours,
    stringsAsFactors = FALSE
  )

  # -- Build ggplot2 visualisation ----------------------------------------------
  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = umap1, y = umap2, colour = colour)
  ) +
    ggplot2::geom_point(size = 0.6, alpha = 0.85, stroke = 0) +
    ggplot2::scale_colour_identity() +
    ggplot2::labs(
      title    = title,
      subtitle = paste0(
        "UMAP of ", formatC(n_sample, format = "d", big.mark = ","),
        " pixels in LAB colour space"
      ),
      x = "UMAP 1",
      y = "UMAP 2"
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "#0d0d0d", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#0d0d0d", colour = NA),
      plot.title       = ggplot2::element_text(
        colour = "#ffffff", size = 16, face = "bold",
        margin = ggplot2::margin(t = 12, b = 4)
      ),
      plot.subtitle    = ggplot2::element_text(
        colour = "#aaaaaa", size = 9,
        margin = ggplot2::margin(b = 10)
      ),
      axis.title       = ggplot2::element_text(colour = "#666666", size = 8),
      axis.title.x     = ggplot2::element_text(
        margin = ggplot2::margin(t = 8)
      ),
      axis.title.y     = ggplot2::element_text(
        margin = ggplot2::margin(r = 8), angle = 90
      ),
      plot.margin      = ggplot2::margin(12, 16, 12, 16)
    )

  p
}
