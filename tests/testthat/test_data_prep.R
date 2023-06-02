test_that("prep_data() works correctly", {
  result <- NutriMeter:::prep_data(age = 18, gender = "Female")
  expect_equal(length(unique(result$nutrient)), 24)
  expect_equal(colnames(result), c("food_name","quantity","nutrient",
                                   "nutrient content", "nutrient content unit", "RDA unit", "RDA"))
  expect_equal(result[1,"RDA"], 2300)
})

