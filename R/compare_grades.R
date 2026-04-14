#' Compute the statistical distance between two image color grades
#'
#' Quantifies how different two images' colour styles are by comparing their
#' k-means colour grades in LAB perceptual colour space.  Returns a single
#' numeric score -- \strong{0 means identical colour style, higher values mean
#' more different}.  Because the metric operates in LAB space, one unit
#' corresponds approximately to one \emph{just-noticeable difference} (JND)
#' as perceived by the human eye.
#'
#' @param img1 Path to an image file (JPG/PNG), an \pkg{imager} \code{cimg}
#'   object, or a pre-computed \code{CineGrade} object from
#'   \code{\link{extract_grade}}.
#' @param img2 Same as \code{img1} -- the second image or grade to compare.
#' @param k Integer. Number of colour clusters (default \code{8}).  Ignored if
#'   both inputs are already \code{CineGrade} objects.  If one input is a
#'   \code{CineGrade} object and the other is a raw image, the raw image is
#'   extracted with the same \code{k} as the pre-computed grade.
#' @param verbose Logical. If \code{TRUE} (default \code{FALSE}), prints a
#'   human-readable interpretation of the score (similar, moderate, or strongly
#'   different).
#'
#' @details
#' \strong{Algorithm -- symmetric average Hausdorff distance in LAB space:}
#' \enumerate{
#'   \item Extract (or reuse) k-means grades for both images.
#'   \item Compute the full \eqn{k \times k} pairwise Euclidean distance matrix
#'         \eqn{D} where \eqn{D_{ij} = \| c^1_i - c^2_j \|_2} in LAB 3-space.
#'   \item \strong{Forward direction} (\code{img1} -> \code{img2}): for each
#'         centroid in \code{img1}, find the nearest centroid in \code{img2}.
#'         Average those \eqn{k} minimum distances.
#'   \item \strong{Backward direction} (\code{img2} -> \code{img1}): same but
#'         reversed.
#'   \item Return the mean of the two directional averages.  This symmetric
#'         formulation ensures \eqn{d(A, B) = d(B, A)} and
#'         \eqn{d(A, A) = 0}.
#' }
#'
#' \strong{Interpreting the score (approximate LAB deltaE units):}
#' \tabular{ll}{
#'   \strong{Score} \tab \strong{Meaning} \cr
#'   0 \tab Identical colour grade \cr
#'   < 5 \tab Very similar style (subtle difference) \cr
#'   5 - 15 \tab Noticeable but related colour style \cr
#'   15 - 30 \tab Clearly different colour style \cr
#'   > 30 \tab Strongly different (e.g. noir vs. anime) \cr
#' }
#'
#' @return A named numeric scalar.  The name is \code{"delta_E"} (perceptual
#'   LAB distance units).  The object also carries two attributes:
#'   \describe{
#'     \item{\code{d_forward}}{Mean nearest-centroid distance img1 -> img2.}
#'     \item{\code{d_backward}}{Mean nearest-centroid distance img2 -> img1.}
#'   }
#'   These can be accessed with \code{attr(result, "d_forward")}.
#'
#' @examples
#' wes_path     <- system.file("extdata", "preset_wes.jpg",      package = "CineGrade")
#' noir_path    <- system.file("extdata", "preset_noir.jpg",     package = "CineGrade")
#' portrait_path<- system.file("extdata", "sample_portrait.jpg", package = "CineGrade")
#'
#' if (nchar(wes_path) > 0) {
#'   # How different is the Wes Anderson grade from noir?
#'   d1 <- compare_grades(wes_path, noir_path, k = 8)
#'   cat("Wes vs Noir distance:", round(d1, 2), "\n")
#'
#'   # Which preset is closest to the portrait?
#'   d2 <- compare_grades(portrait_path, wes_path,  k = 8)
#'   d3 <- compare_grades(portrait_path, noir_path, k = 8)
#'   cat("Portrait vs Wes:", round(d2, 2), "\n")
#'   cat("Portrait vs Noir:", round(d3, 2), "\n")
#'
#'   # Pass a pre-computed grade to avoid re-running k-means
#'   wes_grade <- extract_grade(wes_path, k = 8)
#'   d4 <- compare_grades(portrait_path, wes_grade, verbose = TRUE)
#' }
#'
#' @seealso \code{\link{extract_grade}} to obtain a reusable \code{CineGrade}
#'   object, \code{\link{apply_grade}} to perform the transfer,
#'   \code{\link{palette_pca}} to visualise multiple grades simultaneously.
#'
#' @export
compare_grades <- function(img1, img2, k = 8L, verbose = FALSE) {

  # -- Input validation ---------------------------------------------------------
  if (!is.numeric(k) || length(k) != 1L || k < 1L) {
    stop("`k` must be a positive integer.", call. = FALSE)
  }
  k <- as.integer(k)

  # -- Helper: resolve to a CineGrade object ------------------------------------
  resolve_grade <- function(x, k_use) {
    if (inherits(x, "CineGrade")) {
      return(x)
    } else if (inherits(x, "cimg")) {
      return(extract_grade(x, k = k_use))
    } else if (is.character(x) && length(x) == 1L) {
      return(extract_grade(x, k = k_use))
    } else {
      stop(
        "Each image argument must be a file path, a cimg object, or a CineGrade object.",
        call. = FALSE
      )
    }
  }

  grade1 <- resolve_grade(img1, k)
  grade2 <- resolve_grade(img2, k)

  c1 <- grade1$centers   # k1 x 3 matrix, LAB
  c2 <- grade2$centers   # k2 x 3 matrix, LAB

  k1 <- nrow(c1)
  k2 <- nrow(c2)

  # -- Pairwise Euclidean distance matrix (k1 x k2) in LAB space ---------------
  dist_mat <- matrix(0, nrow = k1, ncol = k2)
  for (i in seq_len(k1)) {
    diffs         <- sweep(c2, 2L, c1[i, ], `-`)
    dist_mat[i, ] <- sqrt(rowSums(diffs^2))
  }

  # -- Symmetric average Hausdorff distance -------------------------------------
  # Forward: for each centroid in grade1, nearest neighbour in grade2
  d_forward  <- mean(apply(dist_mat, 1L, min))   # img1 -> img2

  # Backward: for each centroid in grade2, nearest neighbour in grade1
  d_backward <- mean(apply(dist_mat, 2L, min))   # img2 -> img1

  # Symmetric score
  score <- (d_forward + d_backward) / 2

  # -- Verbose interpretation ----------------------------------------------------
  if (isTRUE(verbose)) {
    interpretation <- if (score < 5) {
      "very similar colour style"
    } else if (score < 15) {
      "noticeable but related colour styles"
    } else if (score < 30) {
      "clearly different colour styles"
    } else {
      "strongly different colour styles"
    }
    cat(sprintf(
      "Grade distance (delta_E): %.2f  [%s]\n  Forward (img1\u2192img2): %.2f | Backward (img2\u2192img1): %.2f\n",
      score, interpretation, d_forward, d_backward
    ))
  }

  # -- Return annotated numeric scalar -----------------------------------------
  result <- structure(
    score,
    names      = "delta_E",
    d_forward  = d_forward,
    d_backward = d_backward,
    k1         = k1,
    k2         = k2,
    class      = "CineGrade_dist"
  )

  result
}


#' Print method for CineGrade_dist objects
#'
#' @param x A \code{CineGrade_dist} object returned by \code{\link{compare_grades}}.
#' @param ... Further arguments (unused).
#' @return Invisibly returns \code{x}.
#' @export
print.CineGrade_dist <- function(x, ...) {
  score <- unclass(x)
  interpretation <- if (score < 5) {
    "very similar"
  } else if (score < 15) {
    "noticeable but related"
  } else if (score < 30) {
    "clearly different"
  } else {
    "strongly different"
  }
  cat(sprintf(
    "CineGrade distance  delta_E = %.4f  [%s]\n",
    score, interpretation
  ))
  cat(sprintf(
    "  Forward  (img1\u2192img2): %.4f\n  Backward (img2\u2192img1): %.4f\n  k1 = %d | k2 = %d\n",
    attr(x, "d_forward"), attr(x, "d_backward"),
    attr(x, "k1"), attr(x, "k2")
  ))
  invisible(x)
}
