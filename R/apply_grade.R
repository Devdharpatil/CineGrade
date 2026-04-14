#' Transfer the color grade of a reference image to a target image
#'
#' Applies the cinematic color grade extracted from \code{reference} onto
#' \code{target} using a centroid-shift algorithm in LAB perceptual color
#' space.  The result is a new image that has the same content as
#' \code{target} but wears the color style of \code{reference} -- making your
#' photo look like a scene from a Wes Anderson film, a noir thriller, or any
#' artistic style you choose.
#'
#' @param target   Path to the target image file (JPG/PNG) or an \pkg{imager}
#'   \code{cimg} object. This is the image that will be re-coloured.
#' @param reference Path to the reference image file (JPG/PNG) or a
#'   \code{cimg} object. This is the image whose color grade will be applied.
#'   Alternatively, a pre-computed \code{CineGrade} object returned by
#'   \code{\link{extract_grade}} can be passed directly.
#' @param k Integer. Number of color clusters used for both images (default
#'   \code{8}).  Ignored if \code{reference} is already a \code{CineGrade}
#'   object.
#'
#' @details
#' \strong{Algorithm (four steps, per PLANNING.md):}
#' \enumerate{
#'   \item \strong{Extract reference grade.}  Run k-means on the reference image
#'         in LAB space to get \eqn{k} reference centroids.
#'   \item \strong{Extract target grade.}  Run k-means on the target image in
#'         LAB space to get \eqn{k} target centroids and a per-pixel cluster
#'         label.
#'   \item \strong{Match centroids.}  For each of the \eqn{k} target centroids,
#'         find the nearest reference centroid by Euclidean distance in LAB
#'         space (1-nearest-neighbour).  This implements the \emph{kNN
#'         technique} from the course: the match minimises
#'         \eqn{\| c^{target}_j - c^{ref}_{j'} \|_2}.
#'   \item \strong{Pixel shift.}  For every pixel assigned to target cluster
#'         \eqn{j}, add the delta vector
#'         \eqn{\Delta_j = c^{ref}_{match(j)} - c^{target}_j}
#'         to its L, a, b values.  The shifted LAB image is converted back
#'         to sRGB and clamped to \eqn{[0,1]}.
#' }
#'
#' Because the shift is computed per-cluster rather than per-pixel, the
#' algorithm is \eqn{O(W \cdot H)} after the two k-means runs, making it
#' practical for large images.
#'
#' @return A \code{cimg} object in sRGB space with the same width and height
#'   as \code{target} and values in \eqn{[0, 1]}.
#'
#' @examples
#' target_path <- system.file("extdata", "sample_portrait.jpg", package = "CineGrade")
#' ref_path    <- system.file("extdata", "preset_wes.jpg",      package = "CineGrade")
#' if (nchar(target_path) > 0 && nchar(ref_path) > 0) {
#'   result <- apply_grade(target_path, ref_path, k = 8)
#'   # imager::plot(result)   # view the graded image
#' }
#'
#' @seealso \code{\link{extract_grade}} to obtain a reusable grade object,
#'   \code{\link{compare_grades}} to measure similarity between two grades,
#'   \code{\link{run_cinegrader}} for the interactive Shiny dashboard.
#'
#' @export
apply_grade <- function(target, reference, k = 8L) {

  # -- Input validation ---------------------------------------------------------
  if (!is.numeric(k) || length(k) != 1L || k < 1L) {
    stop("`k` must be a positive integer (e.g. 8).", call. = FALSE)
  }
  k <- as.integer(k)

  # -- Load target --------------------------------------------------------------
  if (inherits(target, "cimg")) {
    target_img <- target
  } else if (is.character(target) && length(target) == 1L) {
    target_img <- load_image(target)
  } else {
    stop(
      "`target` must be a file path (character) or a cimg object.",
      call. = FALSE
    )
  }

  # -- Step 1 -- Extract reference grade -----------------------------------------
  if (inherits(reference, "CineGrade")) {
    ref_grade <- reference
  } else if (inherits(reference, "cimg")) {
    ref_grade <- extract_grade(reference, k = k)
  } else if (is.character(reference) && length(reference) == 1L) {
    ref_grade <- extract_grade(reference, k = k)
  } else {
    stop(
      "`reference` must be a file path, a cimg object, or a CineGrade object.",
      call. = FALSE
    )
  }

  # -- Step 2 -- Extract target grade --------------------------------------------
  target_grade <- extract_grade(target_img, k = k)

  # -- Step 3 -- Match target centroids to reference centroids (1-NN) ------------
  # dist_mat[j, j'] = Euclidean distance between target centroid j and
  #                   reference centroid j' in 3D LAB space.
  ref_centers    <- ref_grade$centers      # k_ref x 3
  target_centers <- target_grade$centers   # k x 3

  k_target <- nrow(target_centers)
  k_ref    <- nrow(ref_centers)

  # Pairwise distance matrix: rows = target clusters, cols = ref clusters
  dist_mat <- matrix(0, nrow = k_target, ncol = k_ref)
  for (j in seq_len(k_target)) {
    # Squared Euclidean distance from target centroid j to every ref centroid
    diffs <- sweep(ref_centers, 2L, target_centers[j, ], `-`)
    dist_mat[j, ] <- rowSums(diffs^2)
  }

  # For each target centroid, the index of the closest reference centroid
  match_idx <- apply(dist_mat, 1L, which.min)   # length k_target

  # -- Step 4 -- Compute per-cluster delta vectors --------------------------------
  # delta[j,] = ref_center[match_idx[j],] - target_center[j,]
  deltas <- ref_centers[match_idx, , drop = FALSE] - target_centers  # k x 3

  # -- Apply deltas to every pixel -----------------------------------------------
  # Convert target to LAB
  target_lab <- rgb_to_lab(target_img)
  pixel_mat  <- pixels_to_matrix(target_lab)   # (W*H) x 3

  # Each pixel gets the delta of its cluster
  pixel_labels <- target_grade$labels   # integer vector, length W*H
  shift_mat    <- deltas[pixel_labels, , drop = FALSE]   # (W*H) x 3

  # Shift LAB values
  shifted_lab <- pixel_mat + shift_mat

  # -- Convert back to RGB -------------------------------------------------------
  # Rebuild a LAB cimg, convert to RGB, clamp inside lab_to_rgb
  shifted_lab_img <- matrix_to_image(shifted_lab, target_lab)
  result_rgb      <- lab_to_rgb(shifted_lab_img)

  result_rgb
}
