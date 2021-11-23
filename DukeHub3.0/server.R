#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(tidyverse)

course_data <- read_csv(here::here("data/course_catalog.csv"))

course_data <- course_data %>%
    rename(location = `Descr 1`,
           class_identifier = `Unique Class Identifier`,
           catalog_number = Catalog,
           enroll_cap =`Cap Enrl`,
           days = `Pat`,
           mtg_start = `Mtg Start`,
           mtg_end = `Mtg End`,
           term = `Term Descr`) %>%
    filter(location != "NA",
           Descr != "NA",
           !grepl('ML', location),
           !grepl('406 Oregon St 0114', location),
           !grepl('See Instructor/Department', location),
           !grepl('Thesis',  Descr),
           Descr != 'FIRST-YEAR SEMINAR (TOP)')

course_data <- course_data %>%
    mutate(location = case_when(
        grepl('Classroom Building', location ) ~ 'Classroom Buiding',
        grepl('Allen', location) ~ 'Allen',
        grepl('Art Building', location) ~ 'Art Building',
        grepl('Bell Tower', location) ~ 'Bell Tower',
        grepl('Biddle', location) ~ 'Biddle',
        grepl('Biological Sciences', location) ~ 'Biological Sciences',
        grepl('Bivins', location) ~ 'Bivins',
        grepl('Branson Hall', location) ~ 'Branson Hall',
        grepl('Bridges House', location) ~ 'Bridges House',
        grepl('Brodie', location) ~ 'Brodie',
        grepl('Bryan Center', location) ~ 'Bryan Center',
        grepl('Chesterfield', location) ~ 'Chesterfield',
        grepl('Crowell', location) ~ 'Crowell',
        grepl('Divinity', location) ~ 'Divinity School',
        grepl('Duke Chapel', location) ~ 'Duke Chapel',
        grepl('East Duke', location) ~ 'East Duke',
        grepl('FITZPATRICK', location) ~ 'Fitzpatrick',
        grepl('Fitzpatrick', location) ~ 'Fitzpatrick',
        grepl('Franklin Center', location) ~ 'Franklin Center',
        grepl('French Science', location) ~ 'French Science',
        grepl('Friedl Bldg', location) ~ 'Friedl',
        grepl('Fuqua', location) ~ 'Fuqua',
        grepl('Grainger Hal', location) ~ 'Grainger Hall',
        grepl('Gray', location) ~ 'Gray',
        grepl('Gross Hall', location) ~ 'Gross Hall',
        grepl('Hudson Hall', location) ~ 'Hudson Hall',
        grepl('Languages', location) ~ 'Languages',
        grepl('LSRC', location) ~ 'LSRC',
        grepl('Nanaline', location) ~ 'Nanaline',
        grepl('Nasher', location) ~ 'Nasher',
        grepl('Old Chemistry', location) ~ 'Old Chemistry',
        grepl('Page', location) ~ 'Page',
        grepl('Perkins', location) ~ 'Perkins',
        grepl('Physics', location) ~ 'Phsics',
        grepl('Reuben-Cooke', location) ~ 'Reuben-Cooke',
        grepl('Rubenstein Hall', location) ~ 'Sanford',
        grepl('Rubenstein Arts', location) ~ 'Rubenstein Arts Center',
        grepl('Sanford', location) ~ 'Sanford',
        grepl('Smith Warehouse', location) ~ 'Smith Warehouse',
        grepl('Social Sciences', location) ~ 'Social Sciences',
        grepl('Teer', location) ~ 'Teer',
        grepl('The Ark', location) ~ 'The Ark',
        grepl('Trent', location) ~ 'Trent Hall',
        grepl('West Duke', location) ~ 'West Duke',
        grepl('White', location) ~ 'White Lecture Hall',
        grepl('Wilkinson', location) ~ 'Wilkinson',
        grepl('Wilson Center', location) ~ 'Wilson Center',
        TRUE ~ location))

course_data <- course_data %>%
    mutate(Area = case_when(
        Subject %in% c("VMS", "LIT", "CINE", "ARTHIST", "MEDREN" ,
                       "ARTSVIS", "DANCE", "CLST", "THEATRST",
                       "ENGLISH", "DOCST", "LINGUIST", "MUSIC",
                       "PHIL", "RELIGION", "SES", "ROMST") ~ "Arts & Humanities",
        Subject %in% c("AEROSCI", "EVANTH", "BIOCHEM", "NEUROSCI", "CHEM",
                       "COMPSCI", "PSY", "PHYSICS", "ENVIRON", "SUSTAIN",
                       "EOS", "ECS", "MATH", "STA", "PHARM", "ISS", "MARSCI",
                       "CMAC", "LATAMER", "MGM", "DECSCI") ~ "Natural Sciences",
        Subject %in% c("CULANTH", "AMES", "AAAS", "HISTORY", "POLSCI",
                       "ICS", "ECON", "GSF", "EDUC", "SOCIOL", "PUBPOL",
                       "PJMS", "RIGHTS", "HUMANDEV", "JEWISHST", "SCISOC",
                       "MMS", "MILITSCI", "NAVALSCI", "ETHICS", "LSGS",
                       "GLHLTH", "SXL", "I&E", "CHILDPOL", "CESC", "ENERGY",
                       "EHD", "HLTHPOL") ~ "Social Sciences",
        Subject %in% c("BME", "CEE", "EGR", "ECE", "ME", "ENRGYEGR") ~ "Engineering",
        Subject %in% c("ARABIC", "CHINESE", "FRENCH", "GERMAN", "GREEK",
                       "HEBREW", "HINDI", "ITALIAN", "JPN", "KOREAN", "LATIN",
                       "RUSSIAN", "PORTUGUE", "SPANISH", "PERSIAN", "TURKISH",
                       "CREOLE", "POLISH", "KICHE", "SWAHILI") ~ "Language",
        Subject %in% c("PHYSEDU") ~ "Physical Education",
        Subject %in% c("WRITING") ~ "Writing",
        TRUE ~ Subject
    ))


shinyServer(function(session, input, output){
    observe({
        print(input$Subjects)
        x<- course_data %>% filter(Subject == input$Subjects) %>% select(catalog_number)
        updateSelectizeInput(session, "Code", "Select the Course Code", choices = unique(x))
    })
})


