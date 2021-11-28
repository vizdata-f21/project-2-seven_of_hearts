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
library(DT)
library(shinythemes)
library(ggweekly)


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


course_data <- course_data %>%
  mutate(Area = case_when(
    Subject %in% c("VMS", "LIT", "CINE", "ARTHIST", "MEDREN" ,
                   "ARTSVIS", "DANCE", "CLST", "THEATRST",
                   "ENGLISH", "DOCST", "LINGUIST", "MUSIC",
                   "PHIL", "RELIGION", "SES", "ROMST") ~ "Arts & Humanities", # 17
    Subject %in% c("AEROSCI", "EVANTH", "BIOCHEM", "NEUROSCI", "CHEM",
                   "COMPSCI", "PSY", "PHYSICS", "ENVIRON", "SUSTAIN",
                   "EOS", "ECS", "MATH", "STA", "PHARM", "ISS", "MARSCI",
                   "CMAC", "LATAMER", "MGM", "DECSCI") ~ "Natural Sciences", # 21
    Subject %in% c("CULANTH", "AMES", "AAAS", "HISTORY", "POLSCI",
                   "ICS", "ECON", "GSF", "EDUC", "SOCIOL", "PUBPOL",
                   "PJMS", "RIGHTS", "HUMANDEV", "JEWISHST", "SCISOC",
                   "MMS", "MILITSCI", "NAVALSCI", "ETHICS", "LSGS",
                   "GLHLTH", "SXL", "I&E", "CHILDPOL", "CESC", "ENERGY",
                   "EHD", "HLTHPOL") ~ "Social Sciences", # 29
    Subject %in% c("BME", "CEE", "EGR", "ECE", "ME", "ENRGYEGR") ~ "Engineering", # 6
    Subject %in% c("ARABIC", "CHINESE", "FRENCH", "GERMAN", "GREEK",
                   "HEBREW", "HINDI", "ITALIAN", "JPN", "KOREAN", "LATIN",
                   "RUSSIAN", "PORTUGUE", "SPANISH", "PERSIAN", "TURKISH",
                   "CREOLE", "POLISH", "KICHE", "SWAHILI") ~ "Language", # 20
    Subject %in% c("PHYSEDU") ~ "Physical Education", # 1
    Subject %in% c("WRITING") ~ "Writing", # 1
    TRUE ~ Subject
  ))

a <- c("Subject", "catalog_number", "Descr", "Section",
       "enroll_cap",	"days",	"mtg_start", "mtg_end", "Mode",
       "location", "Area")

df <- setNames(data.frame(matrix(ncol = 11, nrow = 0)), a)

print(df)


# Define server logic required to draw a histogram
shinyServer(function(session, input, output) {
  datasetInput <- reactive({
    switch(input$dataset,
           "African American Studies" = course_data %>% filter(Subject == "AAAS") %>%
             select(a),
           "Aerospace Studies" = course_data %>% filter(Subject == "AEROSCI") %>%
             select(a),
           "Asian & Middle Eastern Studies" = course_data %>% filter(Subject == "AMES") %>%
             select(a),
           "Arabic" = course_data %>% filter(Subject == "ARABIC") %>%
             select(a),
           "Art History" = course_data %>% filter(Subject == "ARTHIST") %>%
             select(a),
           "Arts and Sciences IDEAS THEME"  =course_data %>% filter(Subject == "ARTS&SCI") %>%
             select(a),
           "Visual Arts" = course_data %>% filter(Subject == "ARTSVIS") %>%
             select(a),
           "Biochemistry" = course_data %>% filter(Subject == "BIOCHEM") %>%
             select(a),
           "Biology" = course_data %>% filter(Subject == "BIOLOGY") %>%
             select(a),
           "Biomedical Engineering" = course_data %>% filter(Subject == "BME") %>%
             select(a),
           "Brain and Society" = course_data %>% filter(Subject == "BRAINSOC") %>%
             select(a),
           "Civil and Environmental Engineering" = course_data %>% filter(Subject == "CEE") %>%
             select(a),
           "Civic Engagement and Social Change" = course_data %>% filter(Subject == "CESC") %>%
             select(a),
           "Chemistry" = course_data %>% filter(Subject == "CHEM") %>%
             select(a),
           "Child Policy" = course_data %>% filter(Subject == "CHILDPOL") %>%
             select(a),
           "Chinese" = course_data %>% filter(Subject == "CHINESE") %>%
             select(a),
           "Cinema" = course_data %>% filter(Subject == "CINE") %>%
             select(a),
           "Classical Studies" = course_data %>% filter(Subject == "CLST") %>%
             select(a),
           "Computational Media, Arts, and Culture" = course_data %>% filter(Subject == "CMAC") %>%
             select(a),
           "Computer Science" = course_data%>%filter(Subject == 'COMPSCI') %>%
             select(a),
           "Creole" = course_data %>%filter(Subject == 'CREOLE') %>%
             select(a),
           "Cultural Anthropology" = course_data %>% filter(Subject == "CULANTH") %>%
             select(a),
           "Dance" = course_data %>% filter(Subject == "DANCE") %>%
             select(a),
           "Decision Sciences Program" = course_data %>% filter(Subject == "DECSCI") %>%
             select(a),
           "Documentary Studies" = course_data %>% filter(Subject == "DOCST") %>%
             select(a),
           "Electrical and Computer Engineering" = course_data %>% filter(Subject == "ECE") %>%
             select(a),
           "Economics" = course_data%>%filter(Subject == "ECON") %>%
             select(a),
           "Earth and Climate Science" = course_data%>%filter(Subject == "ECS") %>%
             select(a),
           "Education" = course_data%>%filter(Subject == "EDU") %>%
             select(a),
           "Engineering"  = course_data%>%filter(Subject == "EGR") %>%
             select(a),
           "Education and Human Development"  = course_data%>%filter(Subject == "EHD") %>%
             select(a),
           "Energy" = course_data%>%filter(Subject == "ENERGY") %>%
             select(a),
           "English" = course_data%>%filter(Subject == "ENGLISH") %>%
             select(a),
           "Energy Engineering" = course_data%>%filter(Subject == "ENRGYEGR") %>%
             select(a),
           "Environment" = course_data%>%filter(Subject == "ENVIRON") %>%
             select(a),
           "Earth and Ocean Sciences" = course_data%>%filter(Subject == "EOS") %>%
             select(a),
           "Ethics" = course_data%>%filter(Subject == "ETHICS") %>%
             select(a),
           "Evolutionary Anthropology" = course_data%>%filter(Subject == "EVANTH") %>%
             select(a),
           "French" = course_data%>%filter(Subject == "FRENCH") %>%
             select(a),
           "German" = course_data%>%filter(Subject == "GERMAN") %>%
             select(a),
           "Global Health" = course_data%>%filter(Subject == "GLHLTH") %>%
             select(a),
           "Greek" = course_data%>%filter(Subject == "Greek") %>%
             select(a),
           "Gender Sexuality & Feminist Studies" = course_data%>%filter(Subject == "GSF") %>%
             select(a),
           "Hebrew" = course_data%>%filter(Subject == "HEBREW") %>%
             select(a),
           "Hindi" = course_data%>%filter(Subject == "HINDI") %>%
             select(a),
           "History" = course_data%>%filter(Subject == "HISTORY") %>%
             select(a),
           "Health Policy" = course_data%>%filter(Subject == "HLTHPOL") %>%
             select(a),
           "Human Development" = course_data%>%filter(Subject == "HUMANDEV") %>%
             select(a),
           "Innovation & Entrepeneurship" = course_data%>%filter(Subject == "I&E") %>%
             select(a),
           "International Comparative Studies" = course_data%>%filter(Subject == "ICS") %>%
             select(a),
           "Information Science + Studies" = course_data%>%filter(Subject == "ISS") %>%
             select(a),
           "Italian" =  course_data%>%filter(Subject == "ITALIAN") %>%
             select(a),
           "Jewish Studies" = course_data%>%filter(Subject == "JEWISHST") %>%
             select(a),
           "Japanese" = course_data%>%filter(Subject == "JPN") %>%
             select(a),
           "K'iche' Maya" = course_data%>%filter(Subject == "KICHE") %>%
             select(a),
           "Korean"  = course_data%>%filter(Subject == "KOREAN") %>%
             select(a),
           "Latin American Studies"  = course_data%>%filter(Subject == "LATAMER") %>%
             select(a),
           "Latin"  = course_data%>%filter(Subject == "LATIN") %>%
             select(a),
           "Linguistics" = course_data%>%filter(Subject == "LINGUIST") %>%
             select(a),
           "Literature" = course_data %>% filter(Subject == "LIT") %>%
             select(a),
           "Latino Studies & Global South" = course_data %>% filter(Subject == "LSGS") %>%
             select(a),
           "Marine Science Conservation" = course_data %>% filter(Subject == "MARSCI") %>%
             select(a),
           "Math" = course_data %>% filter(Subject == "MATH") %>%
             select(a),
           "Mechanical Engineering" = course_data %>% filter(Subject == "ME") %>%
             select(a),
           "Medieval and Renaissance" = course_data %>% filter(Subject == "MEDREN") %>%
             select(a),
           "Molec Genetics & Microbiology" = course_data %>% filter(Subject == "MGM") %>%
             select(a),
           "Markets & Management" = course_data %>% filter(Subject == "MMS") %>%
             select(a),
           "Military Science" = course_data %>% filter(Subject == "MILITSCI") %>%
             select(a),
           "Music" = course_data %>% filter(Subject == "MUSIC") %>%
             select(a),
           "Naval Science" = course_data %>% filter(Subject == "NAVALSCI") %>%
             select(a),
           "Neuroscience" = course_data %>% filter(Subject == "NEUROSCI") %>%
             select(a),
           "Persian" = course_data %>% filter(Subject == "PERSIAN") %>%
             select(a),
           "Pharm & Cancer Biology" = course_data %>% filter(Subject == "PHARM") %>%
             select(a),
           "Philosophy" = course_data %>% filter(Subject == "PHIL") %>%
             select(a),
           "Physical Education" = course_data %>% filter(Subject == "PHYSEDU") %>%
             select(a),
           "Physics" = course_data %>% filter(Subject == "PHYSICS") %>%
             select(a),
           "Policy Journalism & Media Studies" = course_data %>% filter(Subject == "PJMS") %>%
             select(a),
           "Polish" = course_data %>% filter(Subject == "POLISH") %>%
             select(a),
           "Political Science" = course_data %>% filter(Subject == "POLSCI") %>%
             select(a),
           "Portuguese" = course_data %>% filter(Subject == "PORTUGUE") %>%
             select(a),
           "Psychology" = course_data %>% filter(Subject == "PSY") %>%
             select(a),
           "Public Policy" = course_data %>% filter(Subject == "PUBPOL") %>%
             select(a),
           "Religion" = course_data %>% filter(Subject == "RELIGION") %>%
             select(a),
           "Human Rights" = course_data %>% filter(Subject == "RIGHTS") %>%
             select(a),
           "Romance Studies" = course_data %>% filter(Subject == "ROMST") %>%
             select(a),
           "Russian" = course_data %>% filter(Subject == "RUSSIAN") %>%
             select(a),
           "Science & Society" = course_data %>% filter(Subject == "SCISOC") %>%
             select(a),
           "Slavic and Eurasian Studies"  = course_data %>% filter(Subject == "SES") %>%
             select(a),
           "Sociology" = course_data %>% filter(Subject == "SOCIOL") %>%
             select(a),
           "Spanish" =  course_data %>% filter(Subject == "SPANISH") %>%
             select(a),
           "Statistics" = course_data%>%filter(Subject == 'STA') %>%
             select(a),
           "Sustainability" = course_data%>%filter(Subject == 'SUSTAIN') %>%
             select(a),
           "Swahili" = course_data%>%filter(Subject == 'SWAHILI') %>%
             select(a),
           "Study of Sexualities" = course_data%>%filter(Subject == 'SXL') %>%
             select(a),
           "Theatre Studies" =  course_data%>%filter(Subject == 'THEATRST') %>%
             select(a),
           "Turkish" = course_data%>%filter(Subject == 'TURKISH') %>%
             select(a),
           "Visual Media Studies" = course_data%>%filter(Subject == 'VMS') %>%
             select(a),
           "Writing" = course_data%>%filter(Subject == "WRITING") %>%
             select(a)
    )

  })

  datasetInput <- reactive({
    switch(input$area,
           "Arts & Humanities" = course_data %>% filter(Area == "Arts & Humanities") %>%
             select(a),
           "Natural Sciences" = course_data%>%filter(Area == "Natural Sciences") %>%
             select(a),
           "Social Sciences" = course_data%>%filter(Area == "Social Sciences") %>%
             select(a),
           "Engineering" = course_data%>%filter(Area == "Engineering") %>%
             select(a),
           "Language" = course_data%>%filter(Area == "Language") %>%
             select(a),
           "Physical Education" = course_data%>%filter(Area == "Physical Education") %>%
             select(a),
           "Writing" = course_data%>%filter(Area == "Writing") %>%
             select(a)
    )
  })


  observe({
    print(input$dataset)
    x <- course_data %>%
      filter(Subject == input$dataset) %>%
      # select(catalog_number) # dataframe
      pull(catalog_number) # vector
    updateSelectizeInput(session, "code", "Select the Course Code",
                         choices = unique(x))
  })



  # # Show the first "n" observations ----
  output$view <- renderDT(
    datatable(datasetInput(),
              extensions = 'Buttons',
              options = list(
                dom = 'Bfrtip',
                buttons = list(
                  "copy",
                  list(
                    extend = "collection",
                    text = 'test',
                    action = DT::JS("function ( e, dt, node, config ) {
                                    var data=oTable.rows( { selected: true }).data();
                                      Shiny.setInputValue('test', data, {priority: 'event'});
                                   }")
                  )
                )
              )
    ))




  ## Add selectd rows in the dataframe to a

  filteredTable_selected <- reactive({
    observeEvent(input$delete,{
      df <- setNames(data.frame(matrix(ncol = 11, nrow = 0)), c("Subject",
                                                                "catalog_number", "Descr",
                                                                "Section", "enroll_cap", "days", "mtg_start","mtg_end","Mode", "location",
                                                                "Area"))

    })
    ids <- input$view_rows_selected
    newRows <- datasetInput()[input$view_rows_selected, , drop = F]
    df <<- rbind(isolate(df), newRows) %>%
      distinct()

  })

  output$filteredTableSelected <- DT::renderDataTable({
    datatable(
      filteredTable_selected(),
      selection = list(mode = "none"),
      caption = "Table that gets data from unfiltered original data"
    )
  })

  observeEvent(input$save, {
    print("did you work")
    #req(input$view_rows_selected)
    # df <<- rbind(isolate(df), datasetInput()[input$view_rows_selected, , drop = F])
  })

  observeEvent(input$delete,{
    df <- setNames(data.frame(matrix(ncol = 11, nrow = 0)), c("Subject",
                                                              "catalog_number", "Descr",
                                                              "Section", "enroll_cap", "days", "mtg_start","mtg_end","Mode", "location",
                                                              "Area"))

  })

  output$plot <- renderPlot({
    cal_plot <- ggweek_planner(
      start_day = "2021-01-04",
      end_day = "2021-01-10",
      week_start_label = "week"
    )
    plot(cal_plot)
  })


  output$bardata <- DT::renderDataTable({
    datatable(
      filteredTable_selected() %>%
        mutate(course_name = paste0(Subject, " ", catalog_number)) %>%
        arrange(desc(enroll_cap)),
      caption = "You selected these courses"
    )
  })


  output$barplot <- renderPlot({
    bar_plot <- ggplot(data = filteredTable_selected() %>%
                         mutate(course_name = paste0(Subject, " ", catalog_number)) %>%
                         arrange(desc(enroll_cap)),
                       aes(x = enroll_cap, y = course_name,  # ordering not interactive
                           fill = Area)) +
      geom_col() +
      theme_minimal() +
      labs(title = "Enrollment cap of your classes",
           x = "Enrollment",
           y = "Course name") +
      scale_fill_viridis_d(option = "magma")
    # facet_wrap(~ Area, scales = "free")

    plot(bar_plot)
  })



  observeEvent(input$test, {
    if(input$test){
      print("hello")
    }
  })

})