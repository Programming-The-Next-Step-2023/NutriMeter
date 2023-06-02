test_that("create_output_nutrient_table() works correctly", {
  food_logged <- data.frame(Food = character(),
                            Amount = numeric(),
                            Unit = character())
  food_logged <- rbind(food_logged, data.frame(
    Food = "Potatoes raw", Amount = 100, Unit = "g"
  ))
  ugly_table <- NutriMeter:::calculate_nutrients_amount(
    food_data = NutriMeter:::prep_data(),
    food_logged = food_logged,
    age = 18)
  result <- NutriMeter:::create_output_nutrient_table(ugly_table, NutriMeter:::prep_data())
  expect_snapshot(result)
})
