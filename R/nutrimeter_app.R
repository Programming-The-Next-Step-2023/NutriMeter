# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
#install.packages("janitor")


#read.csv("gpt_food_nutrient.csv", stringsAsFactors = FALSE)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#getwd()


#'@import shiny
#'@import usethis
#'@import dplyr
#'@import shinyWidgets


# ui = fluidPage(
#   column(4,
#          numericInput("x", "Value", 5),
#          br(),
#          actionButton("button", "Show")
#   ),
#   column(8, tableOutput("table"))
# )
# server = function(input, output) {
#   # Take an action every time button is pressed;
#   # here, we just print a message to the console
#   observeEvent(input$button, {
#     cat("Showing", input$x, "rows\n")
#   })
#   # The observeEvent() above is equivalent to:
#   # observe({
#   #    cat("Showing", input$x, "rows\n")
#   #   }) %>%
#   #   bindEvent(input$button)
#
#   # Take a reactive dependency on input$button, but
#   # not on any of the stuff inside the function
#   df <- eventReactive(input$button, {
#     head(cars, input$x)
#   })
#   output$table <- renderTable({
#     df()
#   })
# }
#

#load("./data/gpt_food_nutrient.rda")

# UI
ui <- fluidPage(
  # titlePanel("Nutrition Tracker"),
  sidebarLayout(
    sidebarPanel(
      # numericInput("x", "Value", 5),
      # actionButton("button", "Show"),
      numericInput("age", "Age:", 18, min = 9, max = 120, step = 1),
      selectInput("gender", "Gender", choices = c("Female","Male")),
      textInput("food", "Food", placeholder = "Enter food name"),
        # checkBoxGroupInout
      numericInput("amount", "Amount", value = 100, min = 0),
      selectInput("unit", "Unit", choices = c("g", "ml"), selected = "g"),
      actionButton("log_food", "Log Food")
    ),
    mainPanel(
      # tableOutput("table"),
      tableOutput("table2")
    #   h3("Food Diary"),
    #   tableOutput("food_table"),
    #   h3("Nutrition Progress"),
    #   uiOutput("progress_bars")
    )
  )
)


# Server
server <- function(input, output, session) {
  # Create logged food dataframe
  logged_food <- reactiveValues(data = data.frame(Food = character(),
                                                  Amount = numeric(),
                                                  Unit = character()))
  # Add entered foods to logged_food
  # observeEvent(input$button, {
  #   cat("Showing", input$x, "rows\n")
  # })
  # The observeEvent() above is equivalent to:
  # observe({
  #    cat("Showing", input$x, "rows\n")
  #   }) %>%
  #   bindEvent(input$button)

  # Take a reactive dependency on input$button, but
  # not on any of the stuff inside the function
  # df <- eventReactive(input$button, {
  #   head(cars, input$x)
  # })
  # output$table <- renderTable({
  #   df()
  # })



  nutrients_amount_df <- eventReactive(input$log_food, {
    # Prepare food data df with the correct RDAs
    food_data <-
      prep_data(input$age, input$gender)
      print("updated")
    print("hoi")
    cat("fg")
    food <- input$food
    amount <- input$amount
    unit <- input$unit

    print(food)
    if (!is.na(food) && amount > 0) {
      # Append the logged food to the data frame
      logged_food$data <- rbind(logged_food$data, data.frame(Food = food,
                                                             Amount = amount,
                                                             Unit = unit))
      print(logged_food$data)
      # Clear the input fields after logging the food
      updateTextInput(session, "food", value = "")
      updateNumericInput(session, "amount", value = 100)
    }
    print(str(food_data))
    calculate_nutrients_amount(food_data, logged_food$data)
    #return(logged_food$data)
  })

  output$table2 <- renderTable({
    nutrients_amount_df()
  })




  # Calculate how much raw amount of each nutrient has been consumed so far
  #nutrients_amount_df <- reactive({calculate_nutrients_amount(food_data, logged_food$data)})



  # For all food entries, calculate total percent of RDAs consumed for each nutrient




  # Calculate RDA percentage for each nutrient
  # if (FALSE) {
  # nutrient_progress <- reactive({
  #   get_nutrient_data(input$age, input$gender)
  #
  #   progress_df <- list()
  #
  #   unique_nutrients <- unique(food_data$nutrient)
  #   for (i in 1:length(unique_nutrients)) {
  #     nutrient <- unique_nutrients[i]
  #     nutrient_rda <- food_data$RDA[food_data$nutrient == nutrient][1]
  #     nutrient_content <- food_data$`nutrient content`[food_data$nutrient == nutrient][1]
  #     nutrient_content_unit <- food_data$`nutrient content unit`[food_data$nutrient == nutrient][1]
  #     nutrient_unit <- food_data$`RDA unit`[food_data$nutrient == nutrient][1]
  #
  #     nutrient_amount <- nutrient_amount()
  #
  #     if (nutrient_unit == "g" || nutrient_unit == "ml") {
  #       # Scale nutrient content based on the user-entered amount
  #       nutrient_amount <- nutrient_amount * nutrient_content * (input$amount / 100)
  #     } else {
  #       nutrient_amount <- 0
  #     }
  #
  #     progress <- tryCatch({
  #       percent <- min(nutrient_amount / nutrient_rda * 100, 100)  # Cap at 100%
  #       round(percent, digits = 1)
  #     }, error = function(e) {
  #       0  # Set progress to 0 in case of error
  #     })
  #
  #     progress_list[[i]] <- list(nutrient = nutrient, progress = progress)
  #   }
  #   return(progress_df)
  # })
  #
  #
  # # Display logged food in a table
  # output$food_table <- renderTable({
  #   logged_food$data
  # })
  #
  # # Render progress bars
  # output$progress_bars <- renderUI({
  #   progress_bars <- lapply(nutrient_progress(), function(progress) {
  #     nutrient <- progress$nutrient
  #     percent <- progress$progress
  #     progressBar(
  #       id = paste0("progress_", nutrient),
  #       value = percent,
  #       total = 100,
  #       display_pct = TRUE,
  #       title = paste(nutrient, "(", percent, "%)")
  #     )
  #   })
  #
  #   tagList(progress_bars)
  # })
  #
  # # ...
  #
  # # Update progress bars whenever logged food changes
  # observe({
  #   lapply(nutrient_progress(), function(progress) {
  #     nutrient <- progress$nutrient
  #     percent <- progress$progress
  #     updateProgressBar(session, paste0("progress_", nutrient), value = percent)
  #   })
  # })
  # }
}




### HELPER FUNCTIONS

prep_data <- function(age = 18, gender = "Female") {
  # select the right RDA column
  RDA_col_pt1 <- switch(gender,
                        "Female" = "RDA_f_",
                        "Male" = "RDA_m_",
                        "invalid")
  if(age <= 13) {RDA_col_pt2 <- "9_13"}
    else if (age >= 14 && age <= 18) {RDA_col_pt2 <- "14_18"}
    else if (age >= 19 && age <= 30) {RDA_col_pt2 <- "19_30"}
    else if (age >= 31 && age <= 50) {RDA_col_pt2 <- "31_50"}
    else if (age >= 51 && age <= 70) {RDA_col_pt2 <- "51_70"}
    else {RDA_col_pt2 <- "71_150"}
  RDA_col_name <- paste(RDA_col_pt1,RDA_col_pt2,sep = "")

  # make the food data dataframe
  food_data <- cbind(
    gpt_food_nutrient %>% select(food_name, quantity, nutrient_name, nutrient_content, unit_content, RDA_unit),
    gpt_food_nutrient[,RDA_col_name])
  colnames(food_data) <- c("food_name","quantity","nutrient","nutrient content","nutrient content unit","RDA unit","RDA")
  food_data[food_data == "µg"] <- "mcg"
  food_data$nutrient <- sub("^\\s", "", food_data$nutrient)#delete spaces at beginning of nutrient names

  return(food_data)
}

#prep_data()

# Calculate how much raw amount of each nutrient has been consumed so far
calculate_nutrients_amount <- function(food_data, food_logged) {

  print(dput(food_logged))
  # make empty dataframe with one row per food entry, and one column per nutrient
  columns <- c("food","amount","unit",unique(food_data$nutrient))
  nutrient_amount <- data.frame(matrix(nrow = nrow(food_logged), ncol = length(columns)))
  colnames(nutrient_amount) <- columns
  nutrients <- unique(food_data$nutrient)

  # for each food diary entry calculate the amount of each nutrient it contains
  for (i in 1:nrow(nutrient_amount)) {
    nutrient_amount[i,1] <- food_logged$Food[i]
    nutrient_amount[i,2] <- food_logged$Amount[i]
    nutrient_amount[i,3] <- food_logged$Unit[i]
    for (n in nutrients) {
      amount_per_100 <- food_data %>% filter(food_name == nutrient_amount[i,1], nutrient == n) %>% select(`nutrient content`) %>% pull()
      nutrient_amount[i,n] <- amount_per_100 * (nutrient_amount[i,2]/100)
    }}
  # sum the nutrient amounts over all foods
  nutrient_amount <- nutrient_amount %>%
    select(4:ncol(nutrient_amount)) %>%
    add_row(!!!summarise(., across(where(is.numeric), ~ if(all(!is.na(.))) sum(., na.rm = TRUE) else NA))) %>%
    tail(n=1)
  # return one row with total raw amounts of nutrients consumed
  return(nutrient_amount)
}

# if (FALSE) {
# food_dat <- prep_data()
# View(food_dat)
# calculate_nutrients_amount(food_data = food_dat, food_logged = test_food_diary)
#
# f1 <- c("Potatoes raw", 100, "g")
# f2 <- c("Potatoes new raw", 200, "g")
#
# test_food_diary <- as.data.frame(rbind(f1,f2))
# test_food_diary$Amount <- as.numeric(test_food_diary$Amount)
# colnames(test_food_diary) <- c("Food","Amount","Unit")
# }

#' Run the Nutrimeter app
#'
#' @return
#' @export
#'
runNutrimeterApp <- function() {
  print(server)
  shinyApp(ui = ui, server = server)
}

