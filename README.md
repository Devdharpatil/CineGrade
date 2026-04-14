# CineGrade

**Cinematic Colour Grading and Style Transfer for Images**

[![R](https://img.shields.io/badge/R-%3E%3D4.1-blue)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

---

CineGrade is an R package that extracts the cinematic colour grade from any reference image and mathematically transfers it to a target photo — making your photo look like a scene from a Wes Anderson film, a neon-noir thriller, or any artistic style you choose.

Every transformation is powered by a real statistical algorithm: **k-means clustering**, **PCA**, **UMAP**, and **1-nearest-neighbour matching** — all operating in **CIE LAB colour space** for perceptually accurate results.

---

## Features

- 🎨 **Grade Transfer** — Extract the colour palette of any reference image and apply it to your own photo
- 🖼️ **5 Artistic Styles** — Poster, Pixel art, Anime, Noir, and Vintage stylization modes
- 📊 **Style Analytics** — Fingerprint and compare colour palettes using UMAP and PCA
- 👁️ **Accessibility** — Simulate colour vision deficiencies (deuteranopia, protanopia, tritanopia)
- 🎬 **Shiny Dashboard** — Interactive before/after grading app with one-click download

---

## Prerequisites

CineGrade depends on `imager`, which requires the **X11 / XQuartz** graphics library on UNIX systems.

- **Windows:** No extra setup required! Just run the R code below.
- **macOS:** Install [XQuartz](https://www.xquartz.org/) first, then restart R.
- **Linux:** `sudo apt-get install libx11-dev libglu1-mesa-dev`

```r
install.packages(c("imager", "uwot", "ggplot2", "shiny", "colorspace", "devtools"))
```

---

## Installation

```r
# Install from GitHub
devtools::install_github("devdharpatil/CineGrade")

# Or install from a local clone
devtools::install("path/to/CineGrade")
```

---

## Quick Start

```r
library(CineGrade)

# Paths to built-in sample images
portrait <- system.file("extdata", "sample_portrait.jpg", package = "CineGrade")
wes      <- system.file("extdata", "preset_wes.jpg",      package = "CineGrade")

# Transfer a Wes Anderson colour grade onto your photo
result <- apply_grade(portrait, wes, k = 8)
plot(result)

# Launch the interactive Shiny dashboard
run_cinegrader()
```

---

## Function Reference

| Function | Core technique | What it does |
|:---|:---|:---|
| `extract_grade(image, k)` | k-means in LAB | Extract a compact colour signature (k centroids) from an image |
| `apply_grade(target, reference, k)` | k-means + 1-NN | Transfer the colour grade from reference to target |
| `compare_grades(img1, img2, k)` | LAB Hausdorff ΔE | Measure the perceptual distance between two colour grades |
| `color_universe(image, n_pixels)` | UMAP | Visualise colour distribution as a 2D scatter plot |
| `palette_pca(image_list, k)` | PCA | Compare colour palettes across multiple images |
| `stylize(image, mode, k)` | k-means presets | Apply artistic styles: `poster`, `pixel`, `anime`, `noir`, `vintage` |
| `simulate_colorblind(image, type)` | Machado et al. CVD | Simulate deuteranopia / protanopia / tritanopia |
| `run_cinegrader()` | Shiny | Interactive dashboard: upload → grade → download |

---

## How It Works

```mermaid
flowchart TB
    A["🎯 Target Image"]    -->|rgb_to_lab + pixels_to_matrix + kmeans| B(["k Target Centroids"])
    C["🎬 Reference Image"] -->|same pipeline| D(["k Reference Centroids"])

    B --> E["1-NN Centroid Match\n— apply_grade step 3 —"]
    D --> E

    E --> F["Δ = ref_centroid − target_centroid\nper cluster"]
    F --> G["Shift all pixels in LAB space"]
    G -->|lab_to_rgb| H[/"✅  Graded cimg Output"/]
```

All core algorithms operate in **CIE LAB** colour space because Euclidean distances in LAB correspond to human-perceived colour difference (ΔE = 1 ≈ one just-noticeable difference), making k-means clusters perceptually meaningful rather than merely numerically convenient.

---

## Built-in Sample Data

All functions work out of the box with the included sample images:

| File | Style |
|:---|:---|
| `sample_portrait.jpg` | Neutral indoor scene (default target) |
| `preset_wes.jpg` | Wes Anderson — warm pastels |
| `preset_noir.jpg` | Noir — dark teal-orange high contrast |
| `preset_vintage.jpg` | Vintage — warm, faded film look |
| `preset_anime.jpg` | Anime — bold, saturated chroma |

```r
img <- system.file("extdata", "preset_wes.jpg", package = "CineGrade")
```

---

## Examples

### Grade transfer
```r
result <- apply_grade(
  target    = system.file("extdata", "sample_portrait.jpg", package = "CineGrade"),
  reference = system.file("extdata", "preset_noir.jpg",     package = "CineGrade"),
  k         = 8
)
plot(result)
```

### Measure style similarity
```r
d <- compare_grades(
  system.file("extdata", "preset_wes.jpg",  package = "CineGrade"),
  system.file("extdata", "preset_noir.jpg", package = "CineGrade"),
  k = 8, verbose = TRUE
)
# deltaE = 19.93  [strongly different colour styles]
```

### Colour fingerprint with UMAP
```r
p <- color_universe(
  system.file("extdata", "preset_wes.jpg", package = "CineGrade"),
  n_pixels = 5000,
  title    = "Wes Anderson colour universe"
)
print(p)
```

### Compare styles in PCA space
```r
styles <- list(
  "Wes Anderson" = system.file("extdata", "preset_wes.jpg",     package = "CineGrade"),
  "Noir"         = system.file("extdata", "preset_noir.jpg",    package = "CineGrade"),
  "Vintage"      = system.file("extdata", "preset_vintage.jpg", package = "CineGrade"),
  "Anime"        = system.file("extdata", "preset_anime.jpg",   package = "CineGrade")
)
print(palette_pca(styles, k = 8))
```

### Artistic stylization
```r
portrait <- system.file("extdata", "sample_portrait.jpg", package = "CineGrade")

poster  <- stylize(portrait, mode = "poster",  k = 8)
pixel   <- stylize(portrait, mode = "pixel",   k = 6, block_size = 16)
anime_s <- stylize(portrait, mode = "anime",   sat_scale = 2.0)
noir_s  <- stylize(portrait, mode = "noir")
vintage <- stylize(portrait, mode = "vintage")
```

### Colour vision deficiency simulation
```r
result <- apply_grade(portrait, wes, k = 8)
deut   <- simulate_colorblind(result, type = "deuteranopia")
prot   <- simulate_colorblind(result, type = "protanopia")
plot(deut)
```

---

## Package Architecture

```
CineGrade/
├── R/
│   ├── utils.R           # Colour space conversions (RGB ↔ LAB)
│   ├── extract_grade.R   # k-means colour signature extraction
│   ├── apply_grade.R     # 1-NN centroid-shift colour transfer
│   ├── compare_grades.R  # Hausdorff ΔE perceptual distance
│   ├── color_universe.R  # UMAP colour fingerprint plot
│   ├── palette_pca.R     # PCA palette comparison
│   ├── stylize.R         # 5 artistic stylization modes
│   ├── colorblind.R      # CVD simulation (Machado et al. 2009)
│   └── shiny_app.R       # Interactive Shiny dashboard
├── inst/extdata/         # 5 built-in sample images
├── vignettes/            # Introduction vignette (Rmd)
├── tests/testthat/       # Unit tests
└── man/                  # Auto-generated by roxygen2
```

---

## License

MIT — see [`LICENSE`](LICENSE) file.
