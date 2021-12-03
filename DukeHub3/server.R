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
library(shinyalert)
library(lubridate)
library(data.table)
library(leaflet)
library(treemap)
library(tidytext)
library(ggrepel)
library(geodist)
library(geosphere)
library(thematic)

#course_data <- read_csv(here::here("data/course_catalog.csv"))
course_data <- read_csv("data/course_catalog.csv")
building_group <- read_csv("data/Building_Groups.csv")
coordinates <- read_csv("data/building_group_coordinates.csv")

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
    grepl('Classroom Building', location ) ~ 'Classroom Building',
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
    grepl('Golf', location) ~ 'Golf Course',
    grepl('Grainger Hal', location) ~ 'Grainger Hall',
    grepl('Gray', location) ~ 'Gray',
    grepl('Gross Hall', location) ~ 'Gross Hall',
    grepl('Hudson Hall', location) ~ 'Hudson Hall',
    grepl('Languages', location) ~ 'Languages',
    grepl('Lemur', location) ~ 'Lemur Center',
    grepl('LSRC', location) ~ 'LSRC',
    grepl('Nanaline', location) ~ 'Nanaline',
    grepl('Nasher', location) ~ 'Nasher',
    grepl('Old Chemistry', location) ~ 'Old Chemistry',
    grepl('Online', location) ~ 'Online',
    grepl('Page', location) ~ 'Page',
    grepl('Perkins', location) ~ 'Perkins',
    grepl('Physics', location) ~ 'Physics',
    grepl('Reuben-Cooke', location) ~ 'Reuben Cooke',
    grepl('Rubenstein Hall', location) ~ 'Sanford',
    grepl('Rubenstein Arts', location) ~ 'Rubenstein Arts Center',
    grepl('Sanford', location) ~ 'Sanford',
    grepl('Smith Warehouse', location) ~ 'Smith Warehouse',
    grepl('Social Sciences', location) ~ 'Social Sciences',
    grepl('Teer', location) ~ 'Teer',
    grepl('The Ark', location) ~ 'The Ark',
    grepl('Trent', location) ~ 'Trent Hall',
    grepl('West Campus Tennis Courts', location) ~ 'West Campus Tennis Courts',
    grepl('West Duke', location) ~ 'West Duke',
    grepl('White', location) ~ 'White Lecture Hall',
    grepl('Wilkinson', location) ~ 'Wilkinson',
    grepl('Wilson Center', location) ~ 'Wilson Center',
    TRUE ~ paste(location, 'NO MATCH')))

# conbime location data
course_data <- course_data %>%
  left_join(building_group, by = c("location" = "Location"))

print(unique(course_data$Group_Category))

course_data <- course_data %>%
  mutate(Area = case_when(
    Subject %in% c("VMS", "LIT", "CINE", "ARTHIST", "MEDREN" ,
                   "ARTSVIS", "DANCE", "CLST", "THEATRST",
                   "ENGLISH", "DOCST", "LINGUIST", "MUSIC",
                   "PHIL", "RELIGION", "SES", "ROMST") ~ "Arts & Humanities", # 17
    Subject %in% c("AEROSCI", "EVANTH", "BIOCHEM", "NEUROSCI", "CHEM",
                   "COMPSCI", "PSY", "PHYSICS", "ENVIRON", "SUSTAIN",
                   "EOS", "ECS", "MATH", "STA", "PHARM", "ISS", "MARSCI",
                   "CMAC", "LATAMER", "MGM", "DECSCI", "BIOLOGY") ~ "Natural Sciences", # 21
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
       "location", "Area", "Group_Category", "Group_Number")

df <- setNames(data.frame(matrix(ncol = 13, nrow = 0)), a)


print(df)


# Define server logic required to draw a histogram
shinyServer(function(session, input, output) {
  datasetInput <- reactive({
    switch(input$Area,
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






  # # Show the first "n" observations ----
  output$view <- renderDT(
    datatable(datasetInput()%>%
                select(-c(Mode, location, Area)) %>%
                rename("Course #" = catalog_number,
                       "Title" = Descr,
                       "Max People" = enroll_cap,
                       "Days" = days,
                       "Start" = mtg_start,
                       "End" = mtg_end,
                       "Group Category" = Group_Category,
                       "Group #" = Group_Number)))


  ## Add selectd rows in the dataframe to a

  filteredTable_selected <- reactive({
    observeEvent(input$add, {
      newRows <- datasetInput()[input$view_rows_selected, , drop = F]
       df <<- rbind(isolate(df), newRows) %>%
        distinct()
    })
  })

  #  observeEvent()

  ##BookBag
  output$filteredTableSelected <- DT::renderDataTable({
    datatable(
      df,
      selection = list(mode = "none"),
      caption = "Table that gets data from unfiltered original data"
    )
  })



  observeEvent(input$add, {
    newRows <- datasetInput()[input$view_rows_selected, , drop = F]
     df <<- rbind(isolate(df), newRows) %>%
      distinct()
    print(df)
    output$filteredTableSelected <- DT::renderDataTable({
      datatable(
        df,
        selection = list(mode = "none"),
        caption = "Table that gets data from unfiltered original data"
      )
    })

   # output$bardata <- DT::renderDataTable({ # after changing classes in the schedule builder, plot doesn't change
  #    datatable(
  #      df %>%
  #        mutate(course_name = paste0(Subject, " ", catalog_number)) %>%
  #        arrange(desc(enroll_cap)),
  #      caption = "You selected these courses"
  #    )
  #  })

    output$barplot <- renderPlot({
      bar_plot <- ggplot(data = df %>%
                           mutate(course_name = paste0(Subject, " ", catalog_number, " ", Section)) %>%
                           arrange(enroll_cap),
                         aes(x = enroll_cap, y = reorder(course_name, -enroll_cap),  # ordering not interactive
                             fill = Area)) +
        geom_col() +
        theme_minimal() +
        geom_text(aes(label = enroll_cap, color = "white"),hjust = +5, size = 5)+
        theme(panel.grid.minor = element_blank(),
              legend.position = "none",
              plot.title = element_text(face = "bold", hjust = 0.5)) +
        labs(title = "Enrollment cap of your classes",
             x = "Enrollment",
             y = "Course name") +
        scale_fill_viridis_d(option = "plasma")
      # facet_wrap(~ Area, scales = "free")

      plot(bar_plot)
    })

    output$piechart <- renderPlot({

      pie <- ggplot(data = df %>%
                      group_by(Area) %>%
                      count(Area),
                    aes(x = "",
                        y = n,
                        fill = Area)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        theme_minimal() +
        theme(axis.text = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        labs(title = "Relative Proportion of Classes by Subject Area",
             x = "",
             y = "") +
        coord_polar("y", start = 0) +
        scale_fill_viridis_d(option = "plasma")

      plot(pie, height = 500, width = 500)

    })

    output$subjectPlot <- renderPlot({

      offered_by_subject <- course_data %>%
        filter(!is.na(Subject)) %>%
        group_by(Subject) %>%
        summarise(Total = n()) %>%
        arrange(desc(Total)) %>%
        slice(1:30)

      by_subject <- ggplot(data = offered_by_subject) +
        geom_segment(aes(x = 0, y = reorder(Subject, Total),
                         xend = Total, yend = reorder(Subject, Total),
                         color = Subject %in% df$Subject),
                     size = 1) +
        geom_point(aes(x = Total, y = reorder(Subject, Total),
                       color = Subject %in% df$Subject)) +
        theme_minimal() +
        theme(legend.position = "none",
              plot.title = element_text(face = "bold", hjust = 0.5)) +
        labs(title = "The top 20 subjects that have the most classes",
             x = "Number of classes",
             y = "Subject") +
        scale_color_viridis_d()

      plot(by_subject)

    })



    output$weekdata <- DT::renderDataTable({
      datatable(
        df %>%
          mutate(course_name = paste0(Subject, " ", catalog_number)) %>%
          arrange(desc(enroll_cap)) %>%
          rename("Section #" = Section,
                 "Days" = days,
                 "Starting Time" = mtg_start,
                 "Ending Time" = mtg_end,
                 "Course Name" = course_name) %>%
          relocate("Course Name") %>%
          select(-c(Subject, catalog_number, Descr, enroll_cap, Mode, location, Area, Group_Category, Group_Number)),
        caption = "Tentative Course Schedule"
      )
    })


    weekwrangle <- reactive({
      df %>%
        mutate_at("days", str_replace, "M-F", "MTWTHF") %>%
        mutate_at("days", str_replace, "M-TH", "MTWTH") %>%
        mutate_at("days", str_replace, "TH", "D") %>%
        separate_rows(days, sep = "") %>%
        filter(days != "") %>%
        mutate(days_num = case_when(
          days == "M" ~ 1,
          days == "T" ~ 3,
          days == "W" ~ 5,
          days == "D" ~ 7,
          days == "F" ~ 9
        )) %>%
        mutate(days_num = as.numeric(days_num)) %>%
        mutate(time_start = round(hour(mtg_start)+minute(mtg_start) / 60 + second(mtg_start) / 360,2)) %>%
        mutate(time_end  = round(hour(mtg_end) + minute(mtg_end) / 60 + second(mtg_end) / 360,2)) %>%
        mutate(plotting_st = (days_num - 1)) %>%
        mutate(plotting_end = (days_num + 1)) %>%
        mutate(start_time = time_start) %>%
        mutate(end_time = time_end)  %>%
        mutate(midpoint = (time_start + time_end)/2) %>%
        mutate(post = case_when(
          time_start < 12 ~ "AM",
          time_start > 12 ~ "PM",
          time_end < 12 ~ "AM",
          time_end >= 12 ~ "PM"
        )) %>%
        mutate(head = paste0(Subject, catalog_number, " - ", Section)) %>%
        mutate(context = paste0(mtg_start," ", post ," - ", mtg_end, post))
    })



    output$week <- renderPlot({
      sched <- ggplot(data = weekwrangle(), aes(x = days_num, y = end_time)) +
        geom_rect(aes(xmin = plotting_st, xmax = plotting_end,
                      ymax = end_time, ymin = start_time, color = head, size = 0.3))+
        geom_vline(xintercept = 0, colour = "gray", linetype = "longdash", alpha = 0.4)+
        geom_vline(xintercept = 2, colour = "gray", linetype = "longdash", alpha = 0.4)+
        geom_vline(xintercept = 4, colour = "gray", linetype = "longdash", alpha = 0.4)+
        geom_vline(xintercept = 6, colour = "gray", linetype = "longdash", alpha = 0.4)+
        geom_vline(xintercept = 8, colour = "gray", linetype = "longdash", alpha = 0.4)+
        geom_text(aes(label = head, colour = "chartreuse"), nudge_y = -0.5)+
        geom_text(aes(label = context, colour = "chartreuse"), size = 2, nudge_y = -1)+
        theme_bw() +
        theme(panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(colour = "black"),
              legend.position='none',
              plot.title = element_text(face = "bold", hjust = 0.5),
              axis.title.y = element_text(angle = 0))+
        xlim(0, 10)+
        scale_x_discrete(limits=c("Monday", " ", "Tuesday", " ", "Wednesday", " ", "Thursday",  " ", "Friday"))+
        scale_y_continuous(breaks = seq(6, 22, 1))+
        scale_y_discrete(limits = c(" - "," - "," - "," - "," - ",
                                    "6AM", "7AM", "8AM", "9AM","10AM",
                                    "11AM", "12PM", "1PM", "2PM",
                                    "3PM", "4PM", "5PM", "6PM", "7PM",
                                    "8PM", "9PM", "10PM"))+
        coord_cartesian(ylim = c(6, 22))+
        labs(title = "Tentative Course Schedule", y = "Hours of the Day", x = "Days of the week")+
      scale_fill_viridis_d(option = "plasma")
      plot(sched)
    }, width = 1000,
        height = 400)





    # Do subject areas differ by location?
    output$distinfo <- renderPlot({




      dist <- course_data %>%
        filter(!is.na(Group_Category)) %>%
        filter(Area != "ARTS&SCI") %>%
        filter(Area %in% df$Area) %>%
        group_by(Area) %>%
        count(Group_Category) %>%
        arrange(desc(n),.by_group = TRUE) %>%
        mutate(perc = n/sum(n))

      dist_plot <- ggplot(data = dist, aes(y = reorder_within(Group_Category, n, Area),
                                           Group_Category)) +
        geom_segment(aes(x = 0, y = reorder_within(Group_Category, n, Area),
                         xend = n, yend = reorder_within(Group_Category, n, Area)),
                     size = 1, color = "#003087") +
        geom_point(aes(x = n, y = reorder_within(Group_Category, n, Area))) +
        scale_y_reordered() +
        facet_wrap( ~ Area, ncol = 3, scales = "free") +
        theme_minimal() +
        theme(strip.background = element_rect(fill = "#003087"),
              strip.text = element_text(colour = 'white'),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face = "bold", hjust = 0.5)) +
        labs(y = "Campus locations",
             x = "Number of classs",
             title = "Distribution of class locations by subject area")

      dist_plot
    })






  })

  observeEvent(input$calculate, {

    weekwrangle <- reactive({
      df %>%
        mutate_at("days", str_replace, "M-F", "MTWTHF") %>%
        mutate_at("days", str_replace, "M-TH", "MTWTH") %>%
        mutate_at("days", str_replace, "TH", "D") %>%
        separate_rows(days, sep = "") %>%
        filter(days != "") %>%
        mutate(days_num = case_when(
          days == "M" ~ 1,
          days == "T" ~ 3,
          days == "W" ~ 5,
          days == "D" ~ 7,
          days == "F" ~ 9
        )) %>%
        mutate(days_num = as.numeric(days_num)) %>%
        mutate(mtg_start= round(hour(mtg_start)+minute(mtg_start) / 60 + second(mtg_start) / 360,2)) %>%
        mutate(mtg_end = round(hour(mtg_end) + minute(mtg_end) / 60 + second(mtg_end) / 360,2)) %>%
        mutate(plotting_st = (days_num - 1)) %>%
        mutate(plotting_end = (days_num + 1)) %>%
        mutate(midpoint = (mtg_start + mtg_end)/2) %>%
        mutate(head = paste0(Subject, catalog_number, Section))
    })

      modified_distCosine <- function(Longitude1, Latitude1, Longitude2, Latitude2) {
      if(any(is.na(c(Longitude1, Latitude1, Longitude2, Latitude2)))) {
        0.0
      }
      else {
        distCosine(c(Longitude1, Latitude1), c(Longitude2, Latitude2))
      }
    }


    distTable <- left_join(weekwrangle(), coordinates, by = "Group_Number") %>%
      select(Subject, days, mtg_start, location, Group_Number, Latitude, Longitude)



    output$distanceTable <- DT::renderDataTable({
      datatable(
        distTable%>%
          rename("Days" = days,
                 "Total Distance" = totalDist),
        caption = "Tentative Course Schedule"
      )
    })


    distTable <- distTable %>%
        group_by(days) %>%
        arrange(desc(mtg_start)) %>%
        mutate(Distance = mapply(modified_distCosine, Longitude, Latitude, lag(Longitude), lag(Latitude)) * 0.000621371) %>%
        mutate(Distance = case_when(Distance > .10 ~ round(Distance, digits = 2),
                                    Distance < .10 ~ .10,
                                    TRUE ~ Distance))%>%
        mutate(days = case_when(days == "M" ~ "Monday",
                                days == "T" ~  "Tuesday",
                                days == "W" ~ "Wednesday",
                                days == "D" ~ "Thursday",
                                days == "F" ~ "Friday",
                                TRUE ~ days))  %>%
      summarise(totalDist = sum(Distance))








    output$location <- renderPlot({distance_plot <- ggplot(data = distTable, aes(x= days, y = totalDist))+
        geom_segment( aes(x=days, xend=days, y=0, yend=totalDist, colour= days)) +
      geom_point(aes(size = totalDist * 3.0)) +
        labs(title = "Class Commuter Distance", x = "Day", y = "Distance (miles)") + theme_minimal() +
        scale_x_discrete(limits=c("Monday", "-", "Tuesday", "-", "Wednesday", "-", "Thursday",  "-", "Friday"))+
        scale_color_viridis_d(option = "plasma") +
      geom_vline(xintercept = 2, color = "white")+
        theme(legend.position = "",
              axis.title.y = element_text(angle = 0, vjust = 1.0 ),
              plot.title = element_text(face = "bold", hjust = 0.5))+ coord_flip()
      distance_plot
    })

  })





  observeEvent(input$validate,{

    times <- df %>%
        mutate_at("days", str_replace, "M-F", "MTWTHF") %>%
        mutate_at("days", str_replace, "M-TH", "MTWTH") %>%
        mutate_at("days", str_replace, "TH", "D") %>%
        separate_rows(days, sep = "") %>%
        filter(days != "") %>%
       mutate(intervals = interval(mtg_start, mtg_end)) %>%
      group_by(days) %>%
      arrange(int_start(intervals), .by_group = TRUE) %>%
      mutate(overlap2 = map_int(intervals, ~ sum(int_overlaps(.x, intervals))) > 1)


    if(nrow(df) < 1){
      print("empty")
      shinyalert(
        title = "Invalid Schedule",
        text = "No classes selected",
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "red",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }
    if(TRUE %in% times$overlap2){

      print("wrong)")

      shinyalert(
        title = "Invalid Schedule",
        text = "Time Overlap, Please Clear Bookbag and try Again",
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "red",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )}

    if(nrow(df) > 6){

      shinyalert(
        title = "Invalid Schedule",
        text = "Only 6 courses allowed per semester",
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "red",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )}

    if((1 <= nrow(df) && nrow(df) <= 6) && !(TRUE %in% times$overlap2)){
      shinyalert(
        title = "Sucess",
        text = "Valid Course Schedule",
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "success",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )}
  }   )

  observeEvent(input$clear, {
    df <<- df[0,]
    print(df)
    output$filteredTableSelected <- DT::renderDataTable({
      datatable(
        df,
        selection = list(mode = "none"),
        caption = "Your Bookbag"
      )
    })
  })


  output$dukemap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lat = 36.000015, lng = -78.936033, zoom = 13) %>%
      addCircleMarkers(lng = coordinates$Longitude,
                       lat = coordinates$Latitude,
                       popup  = coordinates$Group_category,
                       label = coordinates$Group_category)})


  # course_catalog table
  loc <- course_data %>%
    group_by(Group_Category) %>%
    count(Group_Category) %>%
    arrange(desc(n)) %>%
    filter(!is.na(Group_Category))

  loc$fraction <- loc$n / sum(loc$n)
  loc$ymax <- cumsum(loc$fraction)
  loc$ymin <- c(0, head(loc$ymax, n = - 1))
  loc$labelPosition <- (loc$ymax + loc$ymin)/2
  loc$label <- paste0(loc$n, " classes at ", loc$Group_Category)

  loc_plot <- ggplot(loc, aes(ymax = ymax, ymin = ymin,
                              xmax = 4, xmin = 3, fill = Group_Category,
                              label = label)) +
    geom_rect() +
    geom_label_repel(x = 3.5, aes(y = labelPosition, label = label), size = 4.5) +
     # geom_text(aes(x = 3.5, y = labelPosition),
     #           check_overlap = TRUE,
     #           vjust = -1.5) +
    coord_polar(theta = "y") +
    xlim(c(2.3, 4)) +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", hjust = 0.5)) +
    labs(title = "Where are the classes located at?") +
    scale_fill_brewer(palette = "Paired")

  output$locinfo <- renderPlot({
    loc_plot
  })
})
