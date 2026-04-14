Sample Images for CineGrade
===========================

Place the following image files in this directory before building the package:

  sample_portrait.jpg  — A neutral outdoor portrait photo (default target image)
  preset_wes.jpg       — Pastel Wes Anderson color frame (built-in grade preset)
  preset_noir.jpg      — Dark teal-orange noir frame (built-in grade preset)
  preset_vintage.jpg   — Warm desaturated vintage frame (built-in grade preset)
  preset_anime.jpg     — High-saturation anime still (built-in grade preset)

These files are loaded in package code via:

  system.file("extdata", "sample_portrait.jpg", package = "CineGrade")

Image requirements:
  - Format:     JPEG (.jpg) or PNG (.png)
  - Resolution: 400 x 600 px minimum recommended
  - Color:      sRGB, 8-bit, 3-channel (no alpha)

All images must be public-domain or CC0 licensed for CRAN / submission.
