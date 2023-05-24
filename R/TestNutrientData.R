# 2do: add the RDA columns, make the column name inside mutate() a symbol or so?

if (FALSE) {
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

library(readxl)
library(dplyr)
library(tidyr)

age <- 23
gen <- "f"



#############################
#####     LOAD DATA     #####
#############################

### (1)  key for foods (NEVO)

food_key <- read.csv("NEVO2021_Nutrienten_Nutrients.csv", row.names = NULL, sep = "|")
#View(food_key)


### (2)  foods and their nutrient contents (NEVO)

food_nutrient <- read.csv("NEVO2021_details.csv", row.names = NULL, sep = "|")
    #View(food_nutrient)

keep_nutrients <- c("CA","CU","ID","FE","MG","P","K","SE","NA","ZN","VITA_RAE",
                    "VITA_RE","RETOL","CARTBTOT","CARTA","CRYPXB","THIA","RIBF",
                    "NIA","VITB6","VITB12","VITC","VITD","VITE","VITK","FOL")

food_nutrient <- food_nutrient %>%
  select(Engelse.naam.Food.name,
         Hoeveelheid.Quantity,
         Nutrient.code,
         Component,
         Gehalte.Value,
         Eenheid.Unit) %>%
  filter(Nutrient.code %in% keep_nutrients)

food_nutrient["Gehalte.Value"] <- lapply(food_nutrient["Gehalte.Value"], function(i) as.numeric(sub(',', '.', i, fixed = TRUE)))


### (3)  RDAs (NCBI)

RDA_vit <- readxl::read_xls("RDA_vit.xls")
RDA_min <- readxl::read_xlsx("RDA_min.xlsx")
#View(RDA_vit)
#View(RDA_min)
RDA <- cbind(RDA_min,RDA_vit)
RDA <- RDA[, !duplicated(colnames(RDA))]
names(RDA)[names(RDA) == "vitD_mch"] <- "vitD_mcg"

### add RDAs to the food_nutrient dataframe

new_cols <- c("RDA_m_9_13","RDA_m_14_18","RDA_m_19_30", "RDA_m_31_50", "RDA_m_51_70","RDA_m_71_150",
              "RDA_f_9_13","RDA_f_14_18","RDA_f_19_30", "RDA_f_31_50", "RDA_f_51_70","RDA_f_71_150")
j <- 0
for (i in new_cols) {
    j = j+1
    food_nutrient <-
      food_nutrient %>%
      mutate(!!i := case_when(
        Nutrient.code == "K" ~ as.character(RDA[j, "potassium_mg"]),
        Nutrient.code == "CA" ~ as.character(RDA[j, "calcium_mg"]),  # <-- Added comma
        Nutrient.code == "P" ~ as.character(RDA[j, "phosphorus_mg"]),
        Nutrient.code == "MG" ~ as.character(RDA[j, "magnesium_mg"]),
        Nutrient.code == "FE" ~ as.character(RDA[j, "iron_mg"]),
        Nutrient.code == "CU" ~ as.character(RDA[j, "copper_mcg"]),
        Nutrient.code == "SE" ~ as.character(RDA[j, "selenium_mcg"]),
        Nutrient.code == "ZN" ~ as.character(RDA[j, "zinc_mg"]),
        Nutrient.code == "ID" ~ as.character(RDA[j, "iodine_mcg"]),
        Nutrient.code %in% c("VITA_RAE", "VITA_RE", "RETOL", "CARTBTOT", "CARTA", "CRYPXB") ~ as.character(RDA[j, "vitA_mcg"]),
        Nutrient.code == "VITD" ~ as.character(RDA[j, "vitD_mcg"]),
        Nutrient.code == "VITE" ~ as.character(RDA[j, "vitE_mg"]),
        Nutrient.code == "THIA" ~ as.character(RDA[j, "vitB1_mg"]),
        Nutrient.code == "RIBF" ~ as.character(RDA[j, "vitB2_mg"]),
        Nutrient.code == "VITB6" ~ as.character(RDA[j, "vitB6_mg"]),
        Nutrient.code == "VITB12" ~ as.character(RDA[j, "vitB12_mcg"]),
        Nutrient.code == "NIA" ~ as.character(RDA[j, "vitB3_mg"]),
        Nutrient.code == "FOL" ~ as.character(RDA[j, "folate_mcg"]),
        Nutrient.code == "VITC" ~ as.character(RDA[j, "vitC_mg"]),
        Nutrient.code == "VITK" ~ as.character(RDA[j, "vitK_mcg"])
      ))
}

food_nutrient <- food_nutrient %>% mutate(
      RDA_unit = case_when(
        Nutrient.code == "K" ~ "mg",
        Nutrient.code == "CA" ~ "mg",  # <-- Added comma
        Nutrient.code == "P" ~ "mg",
        Nutrient.code == "MG" ~ "mg",
        Nutrient.code == "FE" ~ "mg",
        Nutrient.code == "CU" ~ "mcg",
        Nutrient.code == "SE" ~ "mcg",
        Nutrient.code == "ZN" ~ "mg",
        Nutrient.code == "ID" ~ "mcg",
        Nutrient.code %in% c("VITA_RAE", "VITA_RE", "RETOL", "CARTBTOT", "CARTA", "CRYPXB") ~ "mcg",
        Nutrient.code == "VITD" ~ "mcg",
        Nutrient.code == "VITE" ~ "mg",
        Nutrient.code == "THIA" ~ "mg",
        Nutrient.code == "RIBF" ~ "mg",
        Nutrient.code == "VITB6" ~ "mg",
        Nutrient.code == "VITB12" ~ "mcg",
        Nutrient.code == "NIA" ~ "mg",
        Nutrient.code == "FOL" ~ "mcg",
        Nutrient.code == "VITC" ~ "mg",
        Nutrient.code == "VITK" ~ "mcg"
      )
  )

food_nutrient <- food_nutrient %>% mutate_at(new_cols, as.numeric)
colnames(food_nutrient)[1:6] <- c("food_name","quantity","nutrient_code","nutrient_name","nutrient_content","unit_content")

write.csv(food_nutrient, "food_nutrient.csv")


gpt_food_nutrient <- food_nutrient[1:48,]
write.csv(gpt_food_nutrient, "gpt_food_nutrient.csv")






# RDA_user <- RDA %>%
  #filter(age_LB <= age, age_UB >= age, gender == gen)


# Function that calculates the percentage of the RDA met for all nutrients
# given the food the user has consumed so far

#age <- 23
#gen <- "f"

RDA_progress <- function(gender, age) {
  current_status <- RDA_user %>% filter(age_LB <= age, age_UB >= age, gender == gen) %>%
    select(calcium_mg:folate_mcg) %>%
    # add_row(consumed_amounts) %>%
    pivot_longer(everything(),names_to = "nutrient", values_to = "RDA")
}



save(food_nutrient, )
# Vitamin A function
# calculates total vitamin A (RAE) in µg/mcg from components in µg/mcg

vit_A_total <- function(component,mcg) {
  IU <- ifelse(component == 'CRYPXB' | component == 'CARTA', mcg/24 ,
        ifelse(component == 'CARTBTOT', mcg/12,
        ifelse(component == 'RE'| component == 'RETOL' , mcg/2,
        mcg)))
  return(IU)
}






#################################
###    NUTRIENTS TO INCLUDE   ###
#################################

###  CARBOHYDRATES  ###

# CHO: Carbohydrates
#     Sugar: SUGAR
#     Starch: STARCH
#     Fiber: FIBT


###  LIPIDS  ###

# FAT: Fat total
#     FAMSCIS: Fatty acids monounsaturated cis total
#     FAPU: Fatty acids total polyunsaturated total
#         FAPUN3: Fatty acids n-3 polyunsaturated cis (Omega 3)
#         FAPUN6: Fatty acids n-6 polyunsaturated cis (Omega 6)
# FASAT: Fatty acids saturated total
# FATRS: Fatty acids trans total
# CHORL: Cholesterol


# PROTEIN:
#     PROT


###  MINERALS  ###

# CA: Calcium
# CU: Copper
# ID: Iodine
# FE: Iron
# MG: Magnesium
# Manganese
# P: Phosphorus
# K: Potassium
# SE: Selenium
# NA: Sodium
# ZN: Zinc


###  VITAMINS  ###

# VITAMIN A
#     > calculated from
#     VITA_RAE: Retinol activity equialents (RAE)
#     VITA_RE: Retinol equivalents (RE)
#     RETOL: Retinol
#     CARTBTOT: Beta-carotene
#     CARTA: Alfa-carotene
#     CRYPXB:  Beta-cryptoxanthin
#     > calculation:
#     1 IU = 0.3 mcg retinol
#     1 mcg RAE = 1 mcg retinol
#     1 mcg RAE = 2 mcg supplemental beta-carotene
#     1 mcg RAE = 12 mcg beta-carotene
#     1 mcg RAE = 24 mcg alpha-carotene
#     1 mcg RAE = 24 mcg beta-cryptoxanthin

# VITAMIN B1 (Thiamine)
#     THIA
# VITAMIN B2 (Riboflavine)
#     RIBF
# VITAMIN B3 (Niacin)
#     NIA
# VITAMIN B6 (Pyridoxin)
#     VITB6
# VITAMIN B12 (Cobalamin)
#     VITB12

# VITAMIN C
#     VITC

# VITAMIN D
#     VITD

# VITAMIN E
#     VITE

# VITAMIN K
#     VITK

# FOLATE (Dietary Folate Equivalents)
#     FOL

}



