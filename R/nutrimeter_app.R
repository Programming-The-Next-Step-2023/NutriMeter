# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
#install.packages("janitor")


# food_data <- read.csv("gpt_food_nutrient.csv", stringsAsFactors = FALSE)
# food_data <- food_data %>% select(food_name, quantity, nutrient_name, nutrient_content, unit_content, RDA_m_14_18, RDA_unit)
# colnames(food_data) <- c("food_name","quantity","nutrient","nutrient content","nutrient content unit","RDA","RDA unit")
# food_data[food_data == "µg"] <- "mcg"
# food_data$nutrient <- sub("^\\s", "", food_data$nutrient)
#
# food_data$`nutrient unit`[food_data$nutrient == "Potassium"][1]
#
# food_data$`RDA unit`[food_data$nutrient == "Calcium"][1]
#

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#getwd()

#library(shiny)
#library(shinyWidgets)

#'@import shiny
#'@import shinyWidgets

# UI
ui <- fluidPage(
  titlePanel("Nutrition Tracker"),
  sidebarLayout(
    sidebarPanel(
      textInput("food", "Food", placeholder = "Enter food name"),
      numericInput("amount", "Amount", value = 100, min = 0),
      selectInput("unit", "Unit", choices = c("g", "ml"), selected = "g"),
      actionButton("log_food", "Log Food")
    ),
    mainPanel(
      h3("Food Diary"),
      tableOutput("food_table"),
      h3("Nutrition Progress"),
      uiOutput("progress_bars")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Store logged food
  logged_food <- reactiveValues(data = data.frame(Food = character(),
                                                  Amount = numeric(),
                                                  Unit = character()))

  # Load food data
  food_data <- read.csv("gpt_food_nutrient.csv", stringsAsFactors = FALSE)
  food_data <- food_data %>% select(food_name, quantity, nutrient_name, nutrient_content, unit_content, RDA_m_14_18, RDA_unit)
  colnames(food_data) <- c("food_name","quantity","nutrient","nutrient content","nutrient content unit","RDA","RDA unit")
  food_data[food_data == "µg"] <- "mcg"
  food_data$nutrient <- sub("^\\s", "", food_data$nutrient)

  # Add entered foods to logged_food
  observeEvent(input$log_food, {
    food <- input$food
    amount <- input$amount
    unit <- input$unit

    if (!is.na(food) && amount > 0) {
      # Append the logged food to the data frame
      logged_food$data <- rbind(logged_food$data, data.frame(Food = food,
                                                             Amount = amount,
                                                             Unit = unit))

      # Clear the input fields after logging the food
      updateTextInput(session, "food", value = "")
      updateNumericInput(session, "amount", value = 100)
    }
  })

  # Calculate amount of each nutrient consumed (based on all the logged food)
  nutrient_amount <- reactive({
    # make empty dataframe with one row per food entry, and one column per nutrient
    columns = c("food","amount","unit",unique(food_data$nutrient))
    nutrient_amount = data.frame(matrix(nrow = nrow(logged_food$data), ncol = length(columns)))
    colnames(nutrient_amount) = columns
    nutrients <- unique(food_data$nutrient)

    for (i in 1:nrow(nutrient_amount)) {
      nutrient_amount[i,1] <- logged_food$data$Food[i]
      nutrient_amount[i,2] <- logged_food$data$Amount[i]
      nutrient_amount[i,3] <- logged_food$data$Unit[i]
      for (n in nutrients) {
        amount_per_100 <- food_data %>% filter(food_name == nutrient_amount[i,1], nutrient == n) %>% select(`nutrient content`) %>% pull()
        nutrient_amount[i,n] <- amount_per_100 * (nutrient_amount[i,2]/100)
      }
    }
    nutrient_amount <- nutrient_amount %>% select

    return(sum(nutrient_amount))
  })

  # Calculate RDA percentage for each nutrient
  nutrient_progress <- reactive({
    progress_df <- list()

    unique_nutrients <- unique(food_data$nutrient)
    for (i in 1:length(unique_nutrients)) {
      nutrient <- unique_nutrients[i]
      nutrient_rda <- food_data$RDA[food_data$nutrient == nutrient][1]
      nutrient_content <- food_data$`nutrient content`[food_data$nutrient == nutrient][1]
      nutrient_content_unit <- food_data$`nutrient content unit`[food_data$nutrient == nutrient][1]
      nutrient_unit <- food_data$`RDA unit`[food_data$nutrient == nutrient][1]

      nutrient_amount <- nutrient_amount()

      if (nutrient_unit == "g" || nutrient_unit == "ml") {
        # Scale nutrient content based on the user-entered amount
        nutrient_amount <- nutrient_amount * nutrient_content * (input$amount / 100)
      } else {
        nutrient_amount <- 0
      }

      progress <- tryCatch({
        percent <- min(nutrient_amount / nutrient_rda * 100, 100)  # Cap at 100%
        round(percent, digits = 1)
      }, error = function(e) {
        0  # Set progress to 0 in case of error
      })

      progress_list[[i]] <- list(nutrient = nutrient, progress = progress)
    }
    return(progress_df)
  })

  # Display logged food in a table
  output$food_table <- renderTable({
    logged_food$data
  })

  # Render progress bars
  output$progress_bars <- renderUI({
    progress_bars <- lapply(nutrient_progress(), function(progress) {
      nutrient <- progress$nutrient
      percent <- progress$progress
      progressBar(
        id = paste0("progress_", nutrient),
        value = percent,
        total = 100,
        display_pct = TRUE,
        title = paste(nutrient, "(", percent, "%)")
      )
    })

    tagList(progress_bars)
  })

  # ...

  # Update progress bars whenever logged food changes
  observe({
    lapply(nutrient_progress(), function(progress) {
      nutrient <- progress$nutrient
      percent <- progress$progress
      updateProgressBar(session, paste0("progress_", nutrient), value = percent)
    })
  })
}

# Run the Shiny app
#' Title
#'
#' @return
#' @export
#'
runApp <- function() {
  shinyApp(ui = ui, server = server)
}


