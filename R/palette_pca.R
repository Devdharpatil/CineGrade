# Suppress R CMD CHECK NSE notes for ggplot2 column names used in aes()
utils::globalVariables(c("PC1", "PC2", "label", "fill", "xend", "yend", "fname"))

#' Compare color styles across images using PCA
#'
#' Extracts the k-means colour grade from each image in \code{image_list},
#' assembles the centroid vectors into a feature matrix, and runs Principal
#' Component Analysis (\code{\link[stats]{prcomp}}) to project all images into
#' a shared 2D space.  The result is a labelled scatter plot where images with
#' similar colour styles cluster together -- you can see Wes Anderson presets
#' clustering away from noir presets, and so on.
#'
#' @param image_list A \strong{named} list of image file paths (JPG/PNG) or
#'   \code{cimg} objects.  Names are used as point labels in the plot.  At
#'   least 3 images are required for PCA to be informative.  Example:
#'   \preformatted{
#'   list(
#'     "Wes Anderson" = system.file("extdata", "preset_wes.jpg",
#'                                   package = "CineGrade"),
#'     "Noir"         = system.file("extdata", "preset_noir.jpg",
#'                                   package = "CineGrade")
#'   )
#'   }
#' @param k Integer. Number of colour clusters per image (default \code{8}).
#'   Must be the same for all images.
#' @param labels Character vector of length \code{length(image_list)}.
#'   Optional override for point labels.  If \code{NULL} (default), the names
#'   of \code{image_list} are used.
#' @param title Character. Plot title (default \code{"Palette PCA"}).
#' @param show_loadings Logical. If \code{TRUE}, draw loading arrows for the
#'   most influential feature dimensions (default \code{FALSE}).
#'
#' @details
#' \strong{Feature construction.}  Running k-means on \eqn{n} images produces
#' \eqn{n} sets of \eqn{k} centroids, but the cluster numbering is arbitrary:
#' centroid 1 from image A bears no relationship to centroid 1 from image B.
#' To make features comparable, centroids for each image are \emph{sorted by
#' L* (lightness) from dark to bright} before being concatenated.  This
#' yields a consistent \eqn{k \times 3 = 3k}-dimensional feature vector per
#' image capturing: darkest cluster colour, next-darkest, ___, brightest cluster
#' colour -- all in LAB space.
#'
#' \strong{PCA.}  \code{prcomp(scale. = TRUE)} is run on the \eqn{n \times 3k}
#' feature matrix so that the large L* range (~0-100) does not dominate the
#' smaller a*/b* ranges (~-128 to 127).
#'
#' \strong{Plot.}  Each image is shown as a coloured point whose fill is the
#' dominant (lightest-cluster) colour of the image, labelled with its name.
#' The proportion of variance explained by PC1 and PC2 is shown in the axis
#' labels.
#'
#' @return A \code{ggplot} object.  Print it to display, or save it with
#'   \code{\link[ggplot2]{ggsave}}.
#'
#' @examples
#' images <- list(
#'   wes     = system.file("extdata", "preset_wes.jpg",     package = "CineGrade"),
#'   noir    = system.file("extdata", "preset_noir.jpg",    package = "CineGrade"),
#'   vintage = system.file("extdata", "preset_vintage.jpg", package = "CineGrade"),
#'   anime   = system.file("extdata", "preset_anime.jpg",   package = "CineGrade"),
#'   portrait = system.file("extdata", "sample_portrait.jpg", package = "CineGrade")
#' )
#' if (all(nchar(unlist(images)) > 0)) {
#'   p <- palette_pca(images, k = 6)
#'   print(p)
#' }
#'
#' @seealso \code{\link{color_universe}} for single-image UMAP fingerprinting,
#'   \code{\link{extract_grade}} for the underlying k-means grade.
#'
#' @importFrom stats prcomp
#' @importFrom ggplot2 ggplot aes geom_point geom_text labs theme_minimal theme
#'   element_text element_rect element_blank element_line scale_fill_identity
#'   margin unit arrow geom_segment
#' @importFrom grDevices rgb
#' @export
palette_pca <- function(image_list,
                        k             = 8L,
                        labels        = NULL,
                        title         = "Palette PCA",
                        show_loadings = FALSE) {

  # -- Input validation ---------------------------------------------------------
  if (!is.list(image_list) || length(image_list) < 2L) {
    stop("`image_list` must be a list with at least 2 elements.", call. = FALSE)
  }
  if (!is.numeric(k) || length(k) != 1L || k < 1L) {
    stop("`k` must be a positive integer.", call. = FALSE)
  }
  k <- as.integer(k)

  # Resolve labels
  img_names <- names(image_list)
  if (is.null(img_names) || any(img_names == "")) {
    img_names <- paste0("Image", seq_along(image_list))
  }
  if (!is.null(labels)) {
    if (length(labels) != length(image_list)) {
      stop(
        "`labels` must have length ", length(image_list),
        " (one per image).", call. = FALSE
      )
    }
    img_names <- as.character(labels)
  }

  # -- Step 1: Extract k-means grade from each image ----------------------------
  feature_list <- vector("list", length(image_list))

  for (i in seq_along(image_list)) {
    img_input <- image_list[[i]]
    grade     <- extract_grade(img_input, k = k)

    # Sort centroids by L* (dark -> bright) for cross-image comparability
    L_order  <- order(grade$centers[, "L"])
    sorted_c <- grade$centers[L_order, , drop = FALSE]

    # Flatten kx3 matrix -> 3k-length vector (L1,a1,b1, L2,a2,b2, ___)
    feature_list[[i]] <- as.vector(t(sorted_c))
  }

  n_images  <- length(image_list)
  feat_mat  <- do.call(rbind, feature_list)   # n_images x 3k
  rownames(feat_mat) <- img_names

  # -- Step 2: PCA on the feature matrix ----------------------------------------
  if (nrow(feat_mat) < 2L) {
    stop("PCA requires at least 2 images.", call. = FALSE)
  }

  # scale. = TRUE: L* range (~0-100) would dominate a*/b* (~-128 to 127)
  # without scaling, so we normalise each feature to unit variance.
  pca_result <- stats::prcomp(feat_mat, center = TRUE, scale. = TRUE)

  # Percentage of variance explained by each PC
  pct_var <- round(100 * pca_result$sdev^2 / sum(pca_result$sdev^2), 1)

  # PC scores (image positions in PCA space)
  scores <- pca_result$x

  # -- Step 3: Derive a representative fill colour per image (for the points) ---
  # Use the dominant colour = the centroid with the highest L* (most visible)
  fill_cols <- vapply(feature_list, function(fv) {
    # fv is a 3k vector: (L1,a1,b1 ___ Lk,ak,bk) in ascending-L order
    # last triplet = brightest cluster -- convert that LAB to RGB for the swatch
    n3    <- length(fv)
    L_val <- fv[n3 - 2L]
    a_val <- fv[n3 - 1L]
    b_val <- fv[n3]
    lab_obj <- colorspace::LAB(L = L_val, A = a_val, B = b_val)
    rgb_mat <- colorspace::coords(methods::as(lab_obj, "sRGB"))
    rgb_mat <- matrix(pmax(0, pmin(1, rgb_mat)), ncol = 3L)
    grDevices::rgb(rgb_mat[1L, 1L], rgb_mat[1L, 2L], rgb_mat[1L, 3L])
  }, character(1L))

  # -- Step 4: Build plot data frame --------------------------------------------
  n_pc    <- min(ncol(scores), 2L)
  plot_df <- data.frame(
    PC1   = scores[, 1L],
    PC2   = if (n_pc >= 2L) scores[, 2L] else rep(0, nrow(scores)),
    label = img_names,
    fill  = fill_cols,
    stringsAsFactors = FALSE
  )

  x_lab <- paste0("PC1  (", pct_var[1L], "% variance)")
  y_lab <- if (n_pc >= 2L) {
    paste0("PC2  (", pct_var[2L], "% variance)")
  } else {
    "PC2  (0%)"
  }

  # -- Step 5: Build ggplot2 biplot ----------------------------------------------
  base_plot <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = PC1, y = PC2, label = label)
  ) +
    # Reference axes through origin
    ggplot2::geom_hline(yintercept = 0, colour = "#444444", linewidth = 0.4, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 0, colour = "#444444", linewidth = 0.4, linetype = "dashed") +
    # Points coloured by the image's dominant swatch
    ggplot2::geom_point(
      ggplot2::aes(fill = fill),
      shape  = 21,
      size   = 7,
      colour = "#ffffff",
      stroke = 1.2,
      alpha  = 0.92
    ) +
    ggplot2::scale_fill_identity() +
    # Image name labels with slight nudge
    ggplot2::geom_text(
      nudge_y  = diff(range(plot_df$PC2)) * 0.07 + 0.01,
      size     = 3.2,
      colour   = "#e0e0e0",
      fontface = "bold"
    ) +
    ggplot2::labs(
      title    = title,
      subtitle = paste0(
        "PCA of ", k, "-cluster LAB palettes \u2014 ",
        n_images, " images"
      ),
      x = x_lab,
      y = y_lab
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "#111111", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#111111", colour = NA),
      panel.grid.major = ggplot2::element_line(colour = "#2a2a2a", linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title       = ggplot2::element_text(
        colour = "#ffffff", size = 15, face = "bold",
        margin = ggplot2::margin(t = 12, b = 4)
      ),
      plot.subtitle    = ggplot2::element_text(
        colour = "#999999", size = 9,
        margin = ggplot2::margin(b = 10)
      ),
      axis.title       = ggplot2::element_text(colour = "#888888", size = 9),
      axis.text        = ggplot2::element_text(colour = "#666666", size = 8),
      plot.margin      = ggplot2::margin(12, 20, 12, 16)
    )

  # Optional loading arrows (top-5 most influential features on PC1+PC2)
  if (show_loadings && ncol(pca_result$rotation) >= 2L) {
    loadings <- pca_result$rotation[, 1L:2L, drop = FALSE]

    # rownames may be empty if all features had zero variance (dropped by scale.)
    rn <- rownames(loadings)
    if (length(rn) == 0L || nrow(loadings) == 0L) {
      message("show_loadings: no loadings available (all features may have zero variance). Skipping arrows.")
    } else {
      load_lengths <- sqrt(rowSums(loadings^2))
      max_len      <- max(load_lengths)

      if (max_len > 0) {
        n_arrows <- min(5L, nrow(loadings))
        top_idx  <- order(load_lengths, decreasing = TRUE)[seq_len(n_arrows)]

        # Scale arrows to fit within the score space
        sc_cols     <- if (ncol(scores) >= 2L) scores[, 1L:2L] else cbind(scores[, 1L], 0)
        score_range <- max(abs(sc_cols), na.rm = TRUE)
        load_scale  <- score_range * 0.7 / max_len

        load_df <- data.frame(
          xend  = loadings[top_idx, 1L, drop = TRUE] * load_scale,
          yend  = loadings[top_idx, 2L, drop = TRUE] * load_scale,
          fname = rn[top_idx],
          stringsAsFactors = FALSE
        )

        base_plot <- base_plot +
          ggplot2::geom_segment(
            data        = load_df,
            ggplot2::aes(x = 0, y = 0, xend = xend, yend = yend),
            colour      = "#f5a623",
            linewidth   = 0.6,
            arrow       = ggplot2::arrow(length = ggplot2::unit(0.18, "cm"),
                                         type   = "closed"),
            inherit.aes = FALSE
          ) +
          ggplot2::geom_text(
            data = load_df,
            ggplot2::aes(x = xend * 1.12, y = yend * 1.12, label = fname),
            colour      = "#f5a623",
            size        = 2.6,
            inherit.aes = FALSE
          )
      } else {
        message("show_loadings: all loadings are zero. Skipping arrows.")
      }
    }
  }


  base_plot
}
