context('helpers-logo')

test_that("logo works", {
  
  expect_true(class(getLogoImage()) == "character")
  expect_true(file.exists(getLogoImage()))
  
})