test_that("calculate_nutrients_amount() works correcly", {
  food_logged <- data.frame(Food = character(),
                            Amount = numeric(),
                            Unit = character())
  food_logged <- rbind(food_logged, data.frame(
    Food = "Potatoes raw", Amount = 100, Unit = "g"
  ))
  result <- NutriMeter:::calculate_nutrients_amount(
        food_data = NutriMeter:::prep_data(),
        food_logged = food_logged,
        age = 18)
  expect_snapshot(result)
})

