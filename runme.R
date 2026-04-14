# =============================================================================
#  runme.R  -  CineGrade Package Demo
#  MTH 209 - Statistical Computing, IIT Kanpur, 2025-26 Sem II
#  Author : Devdhar Patil  |  Roll: 220348
# =============================================================================
# This script installs CineGrade from the bundled tarball and walks through
# every exported function using the built-in sample images (no internet or
# external data required).
#
# HOW TO RUN:
#   1. Set your working directory to the folder containing this file and
#      'CineGrade_0.1.0.tar.gz'  (they should be in the same folder).
#   2. Source this file, OR run each section interactively block by block.
#
# PREREQUISITE (macOS only):
#   XQuartz must be installed: https://www.xquartz.org/
#   The {imager} package requires an X11 display library on macOS.
# =============================================================================


# -----------------------------------------------------------------------------
# SECTION 0 - Install dependencies and the CineGrade package
# -----------------------------------------------------------------------------

# Install CRAN dependencies if not already present
deps <- c(
  "imager", "uwot", "ggplot2", "shiny", "colorspace",
  "methods", "grDevices"
)
new_pkgs <- deps[!vapply(deps, requireNamespace, logical(1), quietly = TRUE)]
if (length(new_pkgs) > 0L) {
  message("Installing missing dependencies: ", paste(new_pkgs, collapse = ", "))
  install.packages(new_pkgs, repos = "https://cloud.r-project.org")
}

# Install CineGrade from the bundled tarball
# (assumes CineGrade_0.1.0.tar.gz is in the same directory as this script)
tarball <- "CineGrade_0.1.0.tar.gz"
if (!file.exists(tarball)) {
  stop("Cannot find '", tarball, "'. ",
    "Make sure this script and the tarball are in the same folder.",
    call. = FALSE
  )
}
install.packages(tarball, repos = NULL, type = "source")

# Load the package
library(CineGrade)
message("\nCineGrade loaded successfully. Let's begin the demo.\n")


# -----------------------------------------------------------------------------
# SECTION 1 - Built-in sample images
# -----------------------------------------------------------------------------
# The package ships 5 sample images in inst/extdata/ so the demo works
# without any external files.

portrait <- system.file("extdata", "sample_portrait.jpg", package = "CineGrade")
wes <- system.file("extdata", "preset_wes.jpg", package = "CineGrade")
noir <- system.file("extdata", "preset_noir.jpg", package = "CineGrade")
vintage <- system.file("extdata", "preset_vintage.jpg", package = "CineGrade")
anime <- system.file("extdata", "preset_anime.jpg", package = "CineGrade")

message(
  "Sample images found at:\n",
  "  portrait : ", portrait, "\n",
  "  wes      : ", wes, "\n",
  "  noir     : ", noir, "\n",
  "  vintage  : ", vintage, "\n",
  "  anime    : ", anime, "\n"
)


# -----------------------------------------------------------------------------
# SECTION 2 - extract_grade(): k-means colour grade extraction
# -----------------------------------------------------------------------------
# Runs k-means (k = 8) on pixel data in LAB perceptual colour space.
# Returns the k cluster centroids as the image's colour 'signature'.

cat("\n===  SECTION 2 : extract_grade()  ===\n\n")

wes_grade <- extract_grade(wes, k = 8)
print(wes_grade)

# The $centers matrix has one row per cluster (sorted by L* = lightness),
# with columns L, a, b (CIE LAB).  Each row is a prototype colour.
cat("\nCluster centres (LAB):\n")
print(round(wes_grade$centers, 2))


# -----------------------------------------------------------------------------
# SECTION 3 - apply_grade(): centroid-shift colour transfer
# -----------------------------------------------------------------------------
# Transfers the colour style of a reference image onto the target image.
# Algorithm:
#   1. k-means on reference  ->  k reference centroids
#   2. k-means on target     ->  k target centroids + per-pixel labels
#   3. 1-NN match each target centroid to its closest reference centroid
#   4. Shift every pixel by delta = ref_centroid - target_centroid

cat("\n===  SECTION 3 : apply_grade()  ===\n\n")

# --- Wes Anderson grade applied to the portrait ---
wes_result <- apply_grade(portrait, wes, k = 8)

# Show original vs graded side by side
par(mfrow = c(1, 2), mar = c(0, 0, 2, 0))
orig_img <- CineGrade:::load_image(portrait)
plot(orig_img, axes = FALSE)
title("Original portrait", col.main = "grey30")
plot(wes_result, axes = FALSE)
title("Wes Anderson grade (k=8)", col.main = "grey30")
par(mfrow = c(1, 1))

cat("Press [Enter] to continue to the Noir grade...\n")
readline()

# --- Noir grade ---
noir_result <- apply_grade(portrait, noir, k = 8)
par(mfrow = c(1, 2), mar = c(0, 0, 2, 0))
plot(orig_img, axes = FALSE)
title("Original portrait", col.main = "grey30")
plot(noir_result, axes = FALSE)
title("Noir grade (k=8)", col.main = "grey30")
par(mfrow = c(1, 1))


# -----------------------------------------------------------------------------
# SECTION 4 - compare_grades(): LAB Hausdorff distance metric
# -----------------------------------------------------------------------------
# Computes the symmetric average Hausdorff distance in LAB space.
# Score = 0  means identical colour style; higher = more different.
# Units = delta-E (1 unit ~ 1 just-noticeable colour difference to the eye).

cat("\n===  SECTION 4 : compare_grades()  ===\n\n")

d1 <- compare_grades(wes, noir, k = 8, verbose = TRUE)
d2 <- compare_grades(wes, vintage, k = 8, verbose = TRUE)
d3 <- compare_grades(noir, anime, k = 8, verbose = TRUE)

cat("\nSummary of distances:\n")
cat(sprintf("  Wes vs Noir    : delta-E = %.2f\n", as.numeric(d1)))
cat(sprintf("  Wes vs Vintage : delta-E = %.2f\n", as.numeric(d2)))
cat(sprintf("  Noir vs Anime  : delta-E = %.2f\n", as.numeric(d3)))


# -----------------------------------------------------------------------------
# SECTION 5 - color_universe(): UMAP colour fingerprint
# -----------------------------------------------------------------------------
# Subsamples pixels, embeds LAB coordinates in 2D via UMAP, and renders
# each pixel as a coloured dot - a visual 'fingerprint' of the colour style.

cat("\n===  SECTION 5 : color_universe()  ===\n\n")
cat("Running UMAP (takes ~5-10 seconds per image)...\n")

p_wes <- color_universe(wes, n_pixels = 3000, title = "Wes Anderson", seed = 42L)
p_noir <- color_universe(noir, n_pixels = 3000, title = "Noir", seed = 42L)

print(p_wes)
cat("Press [Enter] to see the Noir fingerprint...\n")
readline()
print(p_noir)


# -----------------------------------------------------------------------------
# SECTION 6 - palette_pca(): PCA on multi-image colour palettes
# -----------------------------------------------------------------------------
# Extracts k-means centroids from each image, sorts them by L* (lightness)
# for cross-image comparability, flattens to a 3k-dim feature vector, then
# runs prcomp().  Each image becomes a point in PCA style space.

cat("\n===  SECTION 6 : palette_pca()  ===\n\n")

images <- list(
  "Wes Anderson" = wes,
  "Noir"         = noir,
  "Vintage"      = vintage,
  "Anime"        = anime,
  "Portrait"     = portrait
)
pca_plot <- palette_pca(images, k = 6)
print(pca_plot)

cat("\nNote: images with similar colour styles cluster together in PCA space.\n")


# -----------------------------------------------------------------------------
# SECTION 7 - stylize(): artistic k-means quantization modes
# -----------------------------------------------------------------------------
# Five modes - all built on k-means in LAB space.

cat("\n===  SECTION 7 : stylize()  ===\n\n")
cat("Generating all 5 stylization modes (takes ~15 seconds)...\n")

poster <- stylize(portrait, mode = "poster", k = 8)
pixel_s <- stylize(portrait, mode = "pixel", k = 6, block_size = 16L)
anime_s <- stylize(portrait, mode = "anime", k = 8, sat_scale = 2.0)
noir_s <- stylize(portrait, mode = "noir")
vintage <- stylize(portrait, mode = "vintage")

par(mfrow = c(2, 3), mar = c(0, 0, 1.8, 0))
plot(orig_img, axes = FALSE)
title("Original")
plot(poster, axes = FALSE)
title("poster  - k-means LAB snap")
plot(pixel_s, axes = FALSE)
title("pixel   - block downsample")
plot(anime_s, axes = FALSE)
title("anime   - chroma boost")
plot(noir_s, axes = FALSE)
title("noir    - desaturated 4-tone")
plot(vintage, axes = FALSE)
title("vintage - built-in preset")
par(mfrow = c(1, 1))


# -----------------------------------------------------------------------------
# SECTION 8 - simulate_colorblind(): CVD accessibility simulation
# -----------------------------------------------------------------------------
# Applies Machado et al. (2009) colour vision deficiency matrices in
# linear sRGB space to simulate how the graded image looks to viewers
# with impaired colour vision.

cat("\n===  SECTION 8 : simulate_colorblind()  ===\n\n")

deut <- simulate_colorblind(wes_result, type = "deuteranopia")
prot <- simulate_colorblind(wes_result, type = "protanopia")

par(mfrow = c(1, 3), mar = c(0, 0, 1.8, 0))
plot(wes_result, axes = FALSE)
title("Graded (normal vision)")
plot(deut, axes = FALSE)
title("Deuteranopia (green-blind)")
plot(prot, axes = FALSE)
title("Protanopia  (red-blind)")
par(mfrow = c(1, 1))

cat("\nAccessibility check: always verify that your colour-graded output\n")
cat("remains legible for viewers with colour vision deficiency.\n")


# -----------------------------------------------------------------------------
# SECTION 9 - run_cinegrader(): interactive Shiny dashboard
# -----------------------------------------------------------------------------
# Launches a browser-based Shiny app with:
#   - Target image upload  (JPG / PNG)
#   - Built-in film style presets  OR  custom reference upload
#   - k slider (3 to 20)
#   - Live before/after split-screen preview
#   - Grade distance (delta-E) info bar
#   - Download button for the graded image

cat("\n===  SECTION 9 : run_cinegrader()  ===\n\n")
cat("Launching the Shiny dashboard in your browser...\n")
cat("(Close the browser tab or press Ctrl+C in the console to stop.)\n\n")

run_cinegrader()

# =============================================================================
#  End of runme.R
# =============================================================================
