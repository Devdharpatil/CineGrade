library(CineGrade)

wes      <- system.file("extdata", "preset_wes.jpg",      package = "CineGrade")
noir     <- system.file("extdata", "preset_noir.jpg",     package = "CineGrade")
vintage  <- system.file("extdata", "preset_vintage.jpg",  package = "CineGrade")
portrait <- system.file("extdata", "sample_portrait.jpg", package = "CineGrade")

# Skip all tests if sample images are not installed
skip_if_no_images <- function() {
  testthat::skip_if(nchar(wes) == 0L, "Sample images not installed")
}

test_that("compare_grades returns CineGrade_dist with correct structure", {
  skip_if_no_images()
  d <- compare_grades(wes, noir, k = 6)
  expect_s3_class(d, "CineGrade_dist")
  expect_true(is.numeric(d))
  expect_equal(names(d), "delta_E")
  expect_gte(as.numeric(d), 0)
})

test_that("compare_grades returns 0 for identical inputs", {
  skip_if_no_images()
  d_self <- compare_grades(wes, wes, k = 4)
  expect_lt(as.numeric(d_self), 1e-6)
})

test_that("compare_grades is symmetric", {
  skip_if_no_images()
  d_AB <- compare_grades(wes, vintage, k = 6)
  d_BA <- compare_grades(vintage, wes, k = 6)
  expect_equal(as.numeric(d_AB), as.numeric(d_BA), tolerance = 1e-10)
})

test_that("compare_grades accepts pre-computed CineGrade objects", {
  skip_if_no_images()
  g_wes  <- extract_grade(wes,  k = 6)
  g_noir <- extract_grade(noir, k = 6)
  d <- compare_grades(g_wes, g_noir)
  expect_s3_class(d, "CineGrade_dist")
})

test_that("compare_grades accepts cimg input", {
  skip_if_no_images()
  img <- CineGrade:::load_image(wes)
  d   <- compare_grades(img, noir, k = 5)
  expect_gte(as.numeric(d), 0)
})

test_that("compare_grades attr accessors work", {
  skip_if_no_images()
  d <- compare_grades(wes, noir, k = 6)
  expect_false(is.null(attr(d, "d_forward")))
  expect_false(is.null(attr(d, "d_backward")))
})

test_that("compare_grades validates k", {
  skip_if_no_images()
  expect_error(compare_grades(wes, noir, k = 0), regexp = "positive")
})

test_that("compare_grades validates image arguments", {
  skip_if_no_images()
  expect_error(compare_grades(42L, noir))
  expect_error(compare_grades(wes, list(a = 1)))
})
