# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

#read.csv("gpt_food_nutrient.csv", stringsAsFactors = FALSE)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#getwd()


#'@import shiny
#'@import shinythemes
#'@import shinyWidgets
#'@import usethis
#'@import dplyr
#'@import tidyr


#load("./data/gpt_food_nutrient.rda")

# UI
ui <- fluidPage(
  shinythemes::themeSelector(),
  titlePanel("NutriMeter"),
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
      # textInput("food", "Food", placeholder = "Enter food name"),
      numericInput("amount", "Amount", value = 100, min = 0),
      selectInput("unit", "Unit", choices = c("g", "ml"), selected = "g"),
      actionButton("submit", "Submit", class = "btn-success")
    ),
    mainPanel(
      h3("Today's Food Diary"),
      tableOutput("table1"),
      br(),
      h3("Nutrient Progress"),
      tableOutput("table2"),
      uiOutput("progressBars")
    )
  )
)


# Server
server <- function(input, output, session) {
  observe({
    updateSelectizeInput(
      session,
      "food",
      choices = unique(gpt_food_nutrient$food_name)
    )
  })
  print(unique(gpt_food_nutrient$food_name))
  # Create logged food dataframe
  logged_food <- reactiveValues(data = data.frame(Food = character(),
                                                  Amount = numeric(),
                                                  Unit = character()))

  nutrients_amount_df <- eventReactive(input$submit, {
    # Prepare food data df with the correct RDAs
    food_data <-
      prep_data(input$age, input$gender)
    food <- input$food
    amount <- input$amount
    unit <- input$unit

    #print(food)
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
    # print(str(food_data))
    calculate_nutrients_amount(food_data, logged_food$data, input$age)
    #return(logged_food$data)
  })
  output$table1 <- renderTable({
    logged_food$data
  })
  output$table2 <- renderTable({
    nutrients_amount_df()
  })
  output$progressBars <- renderUI({
    progressBars <- lapply(seq_along(nutrients_amount_df()$Perc_RDA), function(i) {
      nutrient_name <- nutrients_amount_df()$Nutrient[i]
      percent_value <- round(nutrients_amount_df()$Perc_RDA[i], 1)
      fluidRow(
        column(12, h4(nutrient_name)),
        column(12, progressBar(id = paste0("progress", i), value = percent_value, title = paste0(percent_value, "%")))
      )
    })
    do.call(tagList, progressBars)
  })
}




### HELPER FUNCTIONS

#food_nutrient_dat <- read.csv("food_nutrient.csv")

#data_path <- system.file("data", "food_nutrient.csv", package = "NutriMeter")

# Read the CSV file into a dataframe
# food_nutrient <- read.csv("food_nutrient.csv")


## Prepare the data
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
  food_data$nutrient <- sub("^\\s", "", food_data$nutrient) #delete spaces at beginning of nutrient names

  return(food_data)
}


## Calculate how much raw amount of each nutrient has been consumed so far
calculate_nutrients_amount <- function(food_data, food_logged, age) {

  # print(dput(food_logged))
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
      print("amount_per_100")
      print(amount_per_100)
      nutrient_amount[i,n] <- amount_per_100 * (nutrient_amount[i,2]/100)
    }}
  # sum the nutrient amounts over all foods
  nutrient_amount <- nutrient_amount %>%
    select(4:ncol(nutrient_amount)) %>%
    add_row(!!!summarise(., across(where(is.numeric), ~ if(all(!is.na(.))) sum(., na.rm = TRUE) else NA))) %>%
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
                          "Retinol equivalents (RE)",
                          "Retinol", "Beta-carotene", "Beta-cryptoxanthin")
  nutrient_amount <- nutrient_amount %>%
      select(-one_of(columns_to_exclude))
  # return df with raw amounts, RDA and % RDA
  # print(as.vector(unlist((nutrient_amount[1,]))))
  nutrient_amount_fin <- data.frame(
      Nutrient = colnames(nutrient_amount),
      Raw_Amount = as.vector(unlist(nutrient_amount[1,])),
      RDA = as.vector(unlist(nutrient_amount[2,]))
  )
  nutrient_amount_fin <- nutrient_amount_fin %>%
      mutate(Perc_RDA = Raw_Amount/RDA*100)

  # nutrient_amount_fin$`Raw Amount` <- nutrient_amount[1,]
      #Nutrient = colnames(nutrient_amount),
      #`Raw Amount` = nutrient_amount[1,]
      #RDA = nutrient_amount[2,],
      #`% RDA` = NA
  return(nutrient_amount_fin)
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
  #print(server)
  shinyApp(ui = ui, server = server)
}




