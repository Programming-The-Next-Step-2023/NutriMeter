library(NutriMeter)

test_that("calculate_nutrients_amount() works correcly", {
  result <- NutriMeter:::calculate_nutrients_amount(
        food_data = NutriMeter:::prep_data(),
        food_logged = food_logged,
        age = 18)
  expect_equal(round(result[1,"Perc_RDA"],4),0.3177)
})

# NutriMeter:::calculate_nutrients_amount(
#   NutriMeter:::prep_data(),
#   data.frame(t(c("Potatoes raw", 100, "g"))),
#   18)
