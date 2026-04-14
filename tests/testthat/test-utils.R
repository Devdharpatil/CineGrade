test_that("load_image returns a cimg object for a valid JPEG", {
  img_path <- system.file("extdata", "sample_portrait.jpg", package = "CineGrade")
  skip_if(img_path == "", "sample_portrait.jpg not installed — run after inst/extdata is populated")
  img <- CineGrade:::load_image(img_path)
  expect_s3_class(img, "cimg")
  expect_equal(imager::spectrum(img), 3L)
})

test_that("load_image stops on a missing file", {
  expect_error(
    CineGrade:::load_image("/non/existent/path/image.jpg"),
    "Image file not found"
  )
})

test_that("load_image stops on an unsupported format", {
  tmp <- tempfile(fileext = ".bmp")
  writeLines("fake", tmp)
  expect_error(
    CineGrade:::load_image(tmp),
    "Unsupported image format"
  )
  unlink(tmp)
})

test_that("pixels_to_matrix returns a 3-column matrix", {
  img_path <- system.file("extdata", "sample_portrait.jpg", package = "CineGrade")
  skip_if(img_path == "", "sample_portrait.jpg not installed")
  img <- CineGrade:::load_image(img_path)
  mat <- CineGrade:::pixels_to_matrix(img)
  expect_true(is.matrix(mat))
  expect_equal(ncol(mat), 3L)
  expect_equal(nrow(mat), imager::width(img) * imager::height(img))
})

test_that("matrix_to_image round-trip preserves dimensions", {
  img_path <- system.file("extdata", "sample_portrait.jpg", package = "CineGrade")
  skip_if(img_path == "", "sample_portrait.jpg not installed")
  img  <- CineGrade:::load_image(img_path)
  mat  <- CineGrade:::pixels_to_matrix(img)
  img2 <- CineGrade:::matrix_to_image(mat, img)
  expect_equal(dim(img2), dim(img))
})

test_that("rgb <-> lab round-trip is approximately identity", {
  img_path <- system.file("extdata", "sample_portrait.jpg", package = "CineGrade")
  skip_if(img_path == "", "sample_portrait.jpg not installed")
  img      <- CineGrade:::load_image(img_path)
  lab_img  <- CineGrade:::rgb_to_lab(img)
  back     <- CineGrade:::lab_to_rgb(lab_img)
  orig_mat <- CineGrade:::pixels_to_matrix(img)
  back_mat <- CineGrade:::pixels_to_matrix(back)
  expect_equal(orig_mat, back_mat, tolerance = 1e-3)
})
