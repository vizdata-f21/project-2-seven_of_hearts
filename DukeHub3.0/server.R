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
        grepl('Physics', location) ~ 'Physics',
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



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    datasetInput <- reactive({
        switch(input$dataset,
               "African American Studies" = course_data %>% filter(Subject == "AAAS") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Aerospace Studies" = course_data %>% filter(Subject == "AEROSCI") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Asian & Middle Eastern Studies" = course_data %>% filter(Subject == "AMES") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Arabic" = course_data %>% filter(Subject == "AAAS") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Art History" = course_data %>% filter(Subject == "ARTHIST") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Arts and Sciences IDEAS THEME"  =course_data %>% filter(Subject == "ARTS&SCI") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Visual Arts" = course_data %>% filter(Subject == "ARTSVIS") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Biochemistry" = course_data %>% filter(Subject == "BIOCHEM") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Biology" = course_data %>% filter(Subject == "BIOLOGY") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Biomedical Engineering" = course_data %>% filter(Subject == "BME") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Brain and Society" = course_data %>% filter(Subject == "BRAINSOC") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Civil and Environmental Engineering" = course_data %>% filter(Subject == "CEE") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Civic Engagement and Social Change" = course_data %>% filter(Subject == "CESC") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Chemistry" = course_data %>% filter(Subject == "CHEM") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Child Policy" = course_data %>% filter(Subject == "CHILDPOL") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Chinese" = course_data %>% filter(Subject == "CHINESE") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Cinema" = course_data %>% filter(Subject == "CINE") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Classical Studies" = course_data %>% filter(Subject == "CLST") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Computational Media, Arts, and Culture" = course_data %>% filter(Subject == "CMAC") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Computer Science" = course_data%>%filter(Subject == 'COMPSCI') %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Creole" = course_data %>%filter(Subject == 'CREOLE') %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Cultural Anthropology" = course_data %>% filter(Subject == "CULANTH") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Dance" = course_data %>% filter(Subject == "DANCE") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Decision Sciences Program" = course_data %>% filter(Subject == "DECSCI") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Documentary Studies" = course_data %>% filter(Subject == "DOCST") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Electrical and Computer Engineering" = course_data %>% filter(Subject == "ECE") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Economics" = course_data%>%filter(Subject == "ECON") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Earth and Climate Science" = course_data%>%filter(Subject == "ECS") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Education" = course_data%>%filter(Subject == "EDU") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Engineering"  = course_data%>%filter(Subject == "EGR") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Education and Human Development"  = course_data%>%filter(Subject == "EHD") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Energy" = course_data%>%filter(Subject == "ENERGY") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "English" = course_data%>%filter(Subject == "ENGLISH") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Energy Engineering" = course_data%>%filter(Subject == "ENRGYEGR") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Environment" = course_data%>%filter(Subject == "ENVIRON") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Earth and Ocean Sciences" = course_data%>%filter(Subject == "EOS") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
               "Ethics" = course_data%>%filter(Subject == "ETHICS") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
              "Evolutionary Anthropology" = course_data%>%filter(Subject == "EVANTH") %>%
                   select("Subject", "catalog_number", "Descr", "Section",
                          "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                          "location"),
              "French" = course_data%>%filter(Subject == "FRENCH") %>%
                  select("Subject", "catalog_number", "Descr", "Section",
                         "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                         "location"),
              "German" = course_data%>%filter(Subject == "GERMAN") %>%
                  select("Subject", "catalog_number", "Descr", "Section",
                         "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                         "location"),
              "Global Health" = course_data%>%filter(Subject == "GLHLTH") %>%
                  select("Subject", "catalog_number", "Descr", "Section",
                         "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                         "location"),
              "Greek" = course_data%>%filter(Subject == "Greek") %>%
                  select("Subject", "catalog_number", "Descr", "Section",
                         "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                         "location"),
              "Gender Sexuality & Feminist Studies" = course_data%>%filter(Subject == "GSF") %>%
                  select("Subject", "catalog_number", "Descr", "Section",
                         "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                         "location"),
              "Hebrew" = course_data%>%filter(Subject == "HEBREW") %>%
                  select("Subject", "catalog_number", "Descr", "Section",
                         "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                         "location"),
              "Hindi" = course_data%>%filter(Subject == "HINDI") %>%
                  select("Subject", "catalog_number", "Descr", "Section",
                         "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                         "location"),
              "History" = course_data%>%filter(Subject == "HISTORY") %>%
                  select("Subject", "catalog_number", "Descr", "Section",
                         "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                         "location"),
              "Health Policy" = course_data%>%filter(Subject == "HLTHPOL") %>%
                  select("Subject", "catalog_number", "Descr", "Section",
                         "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                         "location"),
             "Human Development" = course_data%>%filter(Subject == "HUMANDEV") %>%
                  select("Subject", "catalog_number", "Descr", "Section",
                         "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                         "location"),
             "Innovation & Entrepeneurship" = course_data%>%filter(Subject == "I&E") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "International Comparative Studies" = course_data%>%filter(Subject == "ICS") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Information Science + Studies" = course_data%>%filter(Subject == "ISS") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Italian" =  course_data%>%filter(Subject == "ITALIAN") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Jewish Studies" = course_data%>%filter(Subject == "JEWISHST") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Japanese" = course_data%>%filter(Subject == "JPN") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "K'iche' Maya" = course_data%>%filter(Subject == "KICHE") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Korean"  = course_data%>%filter(Subject == "KOREAN") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Latin American Studies"  = course_data%>%filter(Subject == "LATAMER") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Latin"  = course_data%>%filter(Subject == "LATIN") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Linguistics" = course_data%>%filter(Subject == "LINGUIST") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Literature" = course_data %>% filter(Subject == "LIT") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Latino Studies & Global South" = course_data %>% filter(Subject == "LSGS") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Marine Science Conservation" = course_data %>% filter(Subject == "MARSCI") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Math" = course_data %>% filter(Subject == "MATH") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Mechanical Engineering" = course_data %>% filter(Subject == "ME") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Medieval and Renaissance" = course_data %>% filter(Subject == "MEDREN") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Molec Genetics & Microbiology" = course_data %>% filter(Subject == "MGM") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Markets & Management" = course_data %>% filter(Subject == "MMS") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Military Science" = course_data %>% filter(Subject == "MILITSCI") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Music" = course_data %>% filter(Subject == "MUSIC") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Naval Science" = course_data %>% filter(Subject == "NAVALSCI") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Neuroscience" = course_data %>% filter(Subject == "NEUROSCI") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Persian" = course_data %>% filter(Subject == "PERSIAN") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Pharm & Cancer Biology" = course_data %>% filter(Subject == "PHARM") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Philosophy" = course_data %>% filter(Subject == "PHIL") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Physical Education" = course_data %>% filter(Subject == "PHYSEDU") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Physics" = course_data %>% filter(Subject == "PHYSICS") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Policy Journalism & Media Studies" = course_data %>% filter(Subject == "PJMS") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Polish" = course_data %>% filter(Subject == "POLISH") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Political Science" = course_data %>% filter(Subject == "POLSCI") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Portuguese" = course_data %>% filter(Subject == "PORTUGUE") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Psychology" = course_data %>% filter(Subject == "PSY") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Public Policy" = course_data %>% filter(Subject == "PUBPOL") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Religion" = course_data %>% filter(Subject == "RELIGION") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Human Rights" = course_data %>% filter(Subject == "RIGHTS") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Romance Studies" = course_data %>% filter(Subject == "ROMST") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Russian" = course_data %>% filter(Subject == "RUSSIAN") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Science & Society" = course_data %>% filter(Subject == "SCISOC") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Slavic and Eurasian Studies"  = course_data %>% filter(Subject == "SES") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Sociology" = course_data %>% filter(Subject == "SOCIOL") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Spanish" =  course_data %>% filter(Subject == "SPANISH") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Statistics" = course_data%>%filter(Subject == 'STA') %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Sustainability" = course_data%>%filter(Subject == 'SUSTAIN') %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Swahili" = course_data%>%filter(Subject == 'SWAHILI') %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Study of Sexualities" = course_data%>%filter(Subject == 'SXL') %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Theatre Studies" =  course_data%>%filter(Subject == 'THEATRST') %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Turkish" = course_data%>%filter(Subject == 'TURKISH') %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Visual Media Studies" = course_data%>%filter(Subject == 'VMS') %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location"),
             "Writing" = course_data%>%filter(Subject == "WRITING") %>%
                 select("Subject", "catalog_number", "Descr", "Section",
                        "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
                        "location")

               )
    })


    Dataframe2 <- reactive({
        mtcars[,input$Columns]
    })
    output$dfStr <- renderPrint({
        str(Dataframe2())
    })
    # Show the first "n" observations ----
    output$view <- renderTable({
        head(datasetInput(), n = input$obs)
    })

    })

