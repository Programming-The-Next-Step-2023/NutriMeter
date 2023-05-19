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

# foods and their nutrient contents (NEVO)
food_nutrient <- read.csv("NEVO2021_details.csv", row.names = NULL, sep = "|")
    #View(food_nutrient)

# key for foods (NEVO)
food_key <- read.csv("NEVO2021_Nutrienten_Nutrients.csv", row.names = NULL, sep = "|")
    #View(food_key)

# RDAs (NCBI)
RDA_vit <- readxl::read_xls("RDA_vit.xls")
RDA_min <- readxl::read_xlsx("RDA_min.xlsx")
    #View(RDA_vit)
    #View(RDA_min)
RDA <- cbind(RDA_min,RDA_vit)
RDA <- RDA[, !duplicated(colnames(RDA))]

RDA_user <- RDA %>%
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





