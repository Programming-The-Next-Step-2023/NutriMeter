# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.


# Imports
#'@import shiny
#'@import shinythemes
#'@import shinyWidgets
#'@import usethis
#'@import dplyr
#'@import testthat




# UI
ui <- fluidPage(
  theme = shinythemes::shinytheme("spacelab"),
  # shinythemes::themeSelector(),
  shiny::column(shiny::h1("NutriMeter",
                          style= "color:white"),
                style = "text-align:justify;color:black;background-color:
                            #702963;padding:15px;border-radius:10px",
                width = 12
  ),
  shiny::fluidRow(
    shiny::column(
      width = 12,
      div(
        style = "background-color: #f5f5f5; padding: 10px; border: 1px solid
        #CCCCCC; border-radius: 5px;",
        HTML("<h4 style='font-size: 18px;'>Welcome to Nutrimeter!</h4>
             <p style='font-size: 16px;'>Log all the foods you eat today in your
             food diary and see your progress on the Recommended Daily Allowance
             (RDA) of most essential nutrients!</p> <p style='font-size: 16px;'
             >If you want to know which foods are good sources of nutrients,
             check out the App Vignette!</p>")
      )
    )
  ),
  br(),
  sidebarLayout(
    sidebarPanel(
      h3("About you"),
      br(),
      numericInput("age", "Age:", 18, min = 9, max = 120, step = 1),
      selectInput("gender", "Gender", choices = c("Female","Male")),
      br(),
      h3("Add a food to your food diary"),
      br(),
      selectizeInput(
        "food",
        "Food",
        choices = NULL,
        options = list(liveSearch = TRUE)
      ),
      numericInput("amount", "Amount", value = 100, min = 0),
      selectInput("unit", "Unit", choices = c("g", "ml"), selected = "g"),
      actionButton("submit", "Submit", class = "btn-success"),
      br(),
      br(),
      h3("Today's food diary"),
      tableOutput("table1")
    ),
    mainPanel(
      fluidRow(
        column(5,
               h3("Progress Bars"),
               div(
                 style = "border-bottom: 2px solid #CCCCCC; margin-bottom: 10px;"
               ),
               br(),
               uiOutput("progressBars")
        ),
        column(7,
               h3("Progress Table"),
               div(
                 style = "border-bottom: 2px solid #CCCCCC; margin-bottom: 10px;"
               ),
               br(),
               tableOutput("table2")
        )
      )
    )
  )
)





# Server
server <- function(input, output, session) {
  observe({
    updateSelectizeInput(
      session,
      "food",
      choices = unique(food_nutrient_dat$food_name)
    )
  })
  # Create dataframe with logged foods
  logged_food <- reactiveValues(data = data.frame(Food = character(),
                                                  Amount = numeric(),
                                                  Unit = character()))
  # Initalize empty food data df with the correct RDAs
  nutrients_amount_df <- eventReactive(input$submit, {
    food_data <-
      prep_data(input$age, input$gender)
    food <- input$food
    amount <- input$amount
    unit <- input$unit

    if (!is.na(food) && amount > 0 && food != "") {
      # Append the logged food to the data frame
      logged_food$data <- rbind(logged_food$data, data.frame(Food = food,
                                                             Amount = amount,
                                                             Unit = unit))
      # Clear the input fields after logging the food
      updateTextInput(session, "food", value = "")
      updateNumericInput(session, "amount", value = 100)
    }
    calculate_nutrients_amount(food_data, logged_food$data, input$age)
  })

  ## Generate Outputs
  # food diary
  output$table1 <- renderTable({
    logged_food$data
  })
  # nutrient progress table
  output$table2 <- renderTable({
  create_output_nutrient_table(nutrients_amount_df(), food_data)
  })
  # progress bars
  output$progressBars <- renderUI({
    progressBars <- lapply(seq_along(nutrients_amount_df()$Perc_RDA), function(i) {
      nutrient_name <- nutrients_amount_df()$Nutrient[i]
      percent_value <- round(nutrients_amount_df()$Perc_RDA[i], 1)
      fluidRow(
        column(10, h4(nutrient_name)),
        column(10, progressBar(id = paste0("progress", i), value = percent_value,
                               title = paste0(percent_value, "%")))
      )
    })
    do.call(tagList, progressBars)
  })
}




# Run App Function
#' Run the Nutrimeter app
#' @description
#' Run this function to open the NutriMeter Shiny App! No arguments needed!
#' @return
#' @export
#'
runNutrimeterApp <- function() {
  shinyApp(ui = ui, server = server)
}


#------------------------------------------------------------------------------

######   HELPER FUNCTIONS    ######


###  Prepare data
prep_data <- function(age = 18, gender = "Female") {

  # select the correct RDA column based on age and gender
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
  # make food data dataframe
  food_data <- cbind(
    food_nutrient_dat %>%
      select(food_name, quantity, nutrient_name, nutrient_content, unit_content,
             RDA_unit), # except vit k
      food_nutrient_dat[,RDA_col_name]) %>%
      filter(nutrient_name != " Vitamin K total")
  colnames(food_data) <- c("food_name","quantity","nutrient","nutrient content",
                           "nutrient content unit","RDA unit","RDA")
  food_data[food_data == "µg"] <- "mcg"
  #delete spaces at beginning of nutrient names
  food_data$nutrient <- sub("^\\s", "", food_data$nutrient)
  return(food_data)
}



###  Calculate raw amounts and %RDA of nutrients consumed
calculate_nutrients_amount <- function(food_data, food_logged, age) {

  # make empty dataframe with one row per food entry, and one column per nutrient
  columns <- c("food","amount","unit",unique(food_data$nutrient))
  nutrient_amount <- data.frame(matrix(nrow = nrow(food_logged),ncol=length(columns)))
  colnames(nutrient_amount) <- columns
  nutrients <- unique(food_data$nutrient)

  # for each food diary entry calculate the amount of each nutrient it contains
  for (i in 1:nrow(nutrient_amount)) {
    nutrient_amount[i,1] <- food_logged$Food[i]
    nutrient_amount[i,2] <- food_logged$Amount[i]
    nutrient_amount[i,3] <- food_logged$Unit[i]
    for (n in nutrients) {
      amount_per_100 <- food_data %>%
        filter(food_name == nutrient_amount[i,1],
               nutrient == n) %>% select(`nutrient content`) %>% pull()
      if(is.numeric(amount_per_100) && length(amount_per_100) == 0){
        nutrient_amount[i,n] <- 0
      } else {
        nutrient_amount[i,n] <- amount_per_100 * (nutrient_amount[i,2]/100)
      }
    }}
  # sum the nutrient amounts over all foods
  nutrient_amount <- nutrient_amount %>%
    select(4:ncol(nutrient_amount)) %>%
    add_row(!!!summarise(., across(where(is.numeric), ~ if(all(!is.na(.)))
      sum(., na.rm = TRUE) else NA))) %>%
    tail(n=1)
  # add a row with RDA values
  nutrient_amount <- rbind(nutrient_amount,food_data[1:24,"RDA"])
  # rename this one sus column
  names(nutrient_amount)[names(nutrient_amount) == "Retinol  "] <- "Retinol"
  nutrient_amount %>% rename(`Retinol  ` = Retinol)
  # convert all to numeric
  nutrient_amount <- nutrient_amount %>% mutate_all(as.numeric)
  # add vitamin A column
  nutrient_amount$`Vitamin A` <- NA
  nutrient_amount$`Vitamin A`[1] <-
      nutrient_amount$`Retinol activity equialents (RAE)`[1] +
      nutrient_amount$`Retinol equivalents (RE)`[1] +
      nutrient_amount$`Retinol`[1] +
      (nutrient_amount$`Beta-carotene`[1]/12) +
      (nutrient_amount$`Alfa-carotene`[1]/24) +
      (nutrient_amount$`Beta-cryptoxanthin`[1]/24)
  nutrient_amount$`Vitamin A`[2] <- ifelse(age <= 13, 600, 800)
  # delete vitamin A component columns
  columns_to_exclude <- c("Retinol activity equialents (RAE)",
                          "Retinol equivalents (RE)", "Alfa-carotene",
                          "Retinol", "Beta-carotene", "Beta-cryptoxanthin")
  nutrient_amount <- nutrient_amount %>%
      select(-one_of(columns_to_exclude))
  # return df with raw amounts, RDA and % RDA
  nutrient_amount_fin <- data.frame(
      Nutrient = colnames(nutrient_amount),
      Raw_Amount = as.vector(unlist(nutrient_amount[1,])),
      RDA = as.vector(unlist(nutrient_amount[2,])))
  nutrient_amount_fin <- nutrient_amount_fin %>%
      mutate(Perc_RDA = Raw_Amount/RDA*100)
  custom_order <- c("Vitamin A", "Thiamin (Vit B1)", "Riboflavin (Vit B2)",
    "Niacin (Vit B3)", "Pyridoxin (Vit B6)", "Cobalamin (Vit B12)",
    "Ascorbic acid (Vit C)", "Vitamin D total", "Vitamin E total",
    "Dietary folate equivalents", "Calcium", "Copper", "Iodine", "Iron total",
    "Magnesium", "Phosphorus", "Potassium", "Selenium total", "Zinc")
  nutrient_amount_fin <- nutrient_amount_fin[match(custom_order,
                                            nutrient_amount_fin$Nutrient),]
  return(nutrient_amount_fin)
}



### Make the nutrient table pretty for the UI
create_output_nutrient_table <- function(ugly_table, food_data) {
     nice_table <- ugly_table %>%
       select(-one_of("Perc_RDA"))
      nice_table$Raw_Amount <- format(round(nice_table$Raw_Amount), nsmall = 0)
      nice_table$RDA <- format(round(nice_table$RDA), nsmall = 0)
     colnames(nice_table) <- c("Nutrient", "Consumed Amount", "RDA Target Amount")
     nice_table$Unit <- c("mcg","mg","mg","mg","mg","mcg","mg","mcg","mg","mcg",
                          "mg","mg","mcg","mg","mg","mg","mg","mcg","mg")
     return(nice_table)
}




