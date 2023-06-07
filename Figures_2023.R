##############################################################################################################################
############ Figure(s) for Gastroschisis Random forests paper       ##########################################################
############ AUTHOR: JMP                                            ##########################################################
############ DATE: 03.21.2023                                       ##########################################################
##############################################################################################################################
#--------------------------------------------------------------------------------------------------------------------
#PREPARATION --------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------

#INSTALL AND LOAD PACKAGES
packages <- c("here", "readxl", "ggplot2", "dplyr", "forestplot")
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN')
  }
}
for (package in packages) {
  library(package, character.only=T)
}

sessionInfo()
Sys.info()

#Read in data
#if(Sys.info()["nodename"]=="BUMC-PC923434"|
#   Sys.info()["nodename"]=="DPH-LAE-013H5BK"){
#  veg <- read_excel(here("data","Histogram 2022-10-05.xlsx"),
#                    sheet = "Veg 2way Interaction RDs")
#}

# Cochrane data from the 'rmeta'-package
base_data <- tibble::tibble(mean  = c(3.42, 3.25, 2.24, 2.18, 1.97, 1.83, 1.68,
                                      1.53, 
                                      1.48, 1.42, 1.27, 1.27, 1.26, 1.20, 
                                      1.20, 
                                      1.15, 1.13, 1.10, 1.10, 1.08, 
                                      1.08, 1.05, 1.04, 1.04,
                                      1.04, 0.98, 0.97),
                            lower = c(2.94, 2.38, 1.83, 1.81, 1.68, 1.55, 1.37,
                                      1.26, 
                                      1.24, 1.16, 1.04, 1.04, 1.04, 1.00, 
                                      0.99, 
                                      0.95, 0.92, 0.92, 0.89, 0.88, 
                                      0.88, 0.85, 0.86, 0.84,
                                      0.84, 0.81, 0.79),
                            upper = c(3.99, 4.45, 2.75, 2.63, 2.31, 2.17, 2.06, 
                                      1.85, 
                                      1.75, 1.75, 1.56, 1.55, 1.54, 1.42,
                                      1.44, 
                                      1.39, 1.39, 1.33, 1.36, 1.33, 
                                      1.32, 1.29, 1.27, 1.27,
                                      1.27, 1.19, 1.18),
                            factor = c("Maternal Age", "Maternal BMI", 
                                       "Relationship Status", "Maternal Race", 
                                       "Paternal Age",  "Marijuana Use", 
                                       "Smoking", 
                                       "Oral Contraceptive Use", "Alcohol", "Parity", 
                                       "NSAID Use", 
                                       "Fast Food/Processed Food Intake", 
                                       "Polyunsaturated Fat", "Total Fat", 
                                       "Parental Education", 
                                       "Monounsaturated Fat", "Paternal Race", 
                                       "Paternal Race",  "Planned Pregnancy",
                                       "Sodium Intake", 
                                       "Genitourinary Infection", 
                                      "Annual Household Income", "Maternal Race", 
                                      "Periconceptional Folic Acid Supplementation",
                                      "Saturated Fat Intake",
                                      "Total Cholesterol", "Parental Employment"),
                            comparison = c("≤18 vs >18 Years", "<30 vs ≥30 kg/m2",
                                           "Single/Living Together vs Married/Partnered/Widowed", 
                                           "Other Races vs Black", "≤21 vs >21 Years", 
                                           "Any vs None", "Current vs Never/Past", 
                                           "Any vs None", 
                                           "Any vs None", "≤1 vs >1",
                                           "Any vs None", "Daily vs Less Often/Never",
                                           "Q1 vs Q2–Q4", "Q2–Q4 vs Q1", 
                                           "≥1 Parent Did Not Complete HS/No Parent Attended College vs More Education", 
                                           "Q1 vs Q2–Q4", "Asian vs Other Races", 
                                           "Black vs Other Races", "No vs Yes", 
                                           "Q2–Q4 vs Q1", 
                                           "Any vs None", 
                                           "≤$14,000/Unknown vs ≥$15,000/Refused", 
                                           "Asian vs Other Races", 
                                           "<400 mcg (Including 0) vs ≥400 mcg", 
                                           "Q1 vs Q2–Q4", 
                                           "Q1 vs Q2–Q4", 
                                           "Both Unemployed vs ≥1 Parent Employed"),
                            aOR_95CI = c("3.4 (2.9, 4.0)", "3.3 (2.4, 4.5)", 
                                         "2.2 (1.8, 2.8)", "2.2 (1.8, 2.6)", 
                                         "2.0 (1.7, 2.3)", "1.8 (1.6, 2.2)", 
                                         "1.7 (1.4, 2.1)", "1.5 (1.3, 1.9)", 
                                         "1.5 (1.2, 1.8)", 
                                         "1.4 (1.2, 1.8)",  
                                         "1.3 (1.0, 1.6)", 
                                         "1.3 (1.0, 1.6)", "1.3 (1.0, 1.5)",
                                         "1.2 (1.0, 1.4)", 
                                         "1.2 (0.99, 1.4)", "1.2 (0.95, 1.4)",
                                         "1.1 (0.92, 1.4)", "1.1 (0.92, 1.3)", 
                                         "1.1 (0.89, 1.4)", "1.1 (0.88, 1.3)",
                                         "1.1 (0.88, 1.3)", 
                                         "1.1 (0.86, 1.3)", "1.0 (0.85, 1.3)", 
                                         "1.0 (0.84, 1.3)",
                                         "1.0 (0.84, 1.3)", "0.98 (0.81, 1.2)", 
                                         "0.97 (0.79, 1.2)"
                                         ))


base_data |>
  forestplot(labeltext = c(factor, comparison, aOR_95CI),
             boxsize = 0.2,
             xticks = c(-.69, 0.001, 0.6931, 1.0986, 1.3863, 1.6094),
 #            clip = c(0.89, 4.1),
             align = c("l", "l", "c"),
             #could consider using a list with OR centered
             xlab = "aOR (95% CI)",
             xlog = TRUE) |>
  fp_set_style(box = "black",
               line = "black",
               summary = "black") |> 
  fp_add_header(factor = c("", "Predictor"),
                comparison = c("", "Contrast"),
                aOR_95CI = c("", "aOR (95% CI)")) |>
  fp_set_zebra_style("#EFEFEF")
