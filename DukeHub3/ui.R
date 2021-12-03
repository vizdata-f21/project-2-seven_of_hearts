library(shinythemes)
library(shinyalert)
library(leaflet)

ui <- navbarPage("DukeHub 3.0",
  theme = bslib::bs_theme(
    bg = "#FFFFFF", fg = "#000080", primary = "#2AA198",
    base_font = bslib::font_google("Verdana")
  ),
  tabPanel(
    "ScheduleBuilder",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "Area",
          label = "Choose a Subject Area:",
          choices = c(
            "African American Studies",
            "Aerospace Studies",
            "Asian & Middle Eastern Studies",
            "Arabic",
            "Art History",
            "Arts and Sciences IDEAS THEME",
            "Visual Arts",
            "Biochemistry",
            "Biology",
            "Biomedical Engineering",
            "Brain and Society",
            "Civil and Environmental Engineering",
            "Civic Engagement and Social Change",
            "Chemistry",
            "Child Policy",
            "Chinese",
            "Cinema",
            "Classical Studies",
            "Computational Media, Arts, and Culture",
            "Computer Science",
            "Creole",
            "Cultural Anthropology",
            "Dance",
            "Decision Sciences Program",
            "Documentary Studies",
            "Electrical and Computer Engineering",
            "Economics",
            "Earth and Climate Science",
            "Engineering",
            "Education and Human Development",
            "Energy",
            "English",
            "Energy Engineering",
            "Environment",
            "Earth and Ocean Sciences",
            "Ethics",
            "Evolutionary Anthropology",
            "French",
            "German",
            "Global Health",
            "Greek",
            "Gender Sexuality & Feminist Studies",
            "Hebrew",
            "Hindi",
            "History",
            "Health Policy",
            "Human Development",
            "Innovation & Entrepeneurship",
            "International Comparative Studies",
            "Information Science + Studies",
            "Italian",
            "Jewish Studies",
            "Japanese",
            "K'iche' Maya",
            "Korean",
            "Latin American Studies",
            "Latin",
            "Linguistics",
            "Literature",
            "Latino Studies & Global South",
            "Marine Science Conservation",
            "Math",
            "Mechanical Engineering",
            "Medieval and Renaissance",
            "Molec Genetics & Microbiology",
            "Military Science",
            "Markets & Management",
            "Music",
            "Naval Science",
            "Neuroscience",
            "Persian",
            "Pharm & Cancer Biology",
            "Philosophy",
            "Physical Education",
            "Physics",
            "Policy Journalism & Media Studies",
            "Polish",
            "Political Science",
            "Portuguese",
            "Psychology",
            "Public Policy",
            "Religion",
            "Human Rights",
            "Romance Studies",
            "Russian",
            "Science & Society",
            "Slavic and Eurasian Studies",
            "Sociology",
            "Spanish",
            "Statistics",
            "Sustainability",
            "Swahili",
            "Study of Sexualities",
            "Theatre Studies",
            "Turkish",
            "Visual Media Studies",
            "Writing"
          ),
          multiple = FALSE,
          width = "200px"
        ),
        width = 2
      ),
      mainPanel(
        h1("Schedule Builder"),
        # Output: HTML table with requested number of observations ----
        verbatimTextOutput("Select Classes to add to Schedule"),
        DT::dataTableOutput("view"),
        actionButton("add", label = "Add Course To BookBag"),
        actionButton("clear", label = "Clear BookBag"),
        useShinyalert(),
        actionButton("validate", label = "Validate Schedule"),
        br(),
        verbatimTextOutput("Selected Courses"),
        br(),
        h1("Bookbag"),
        DT::dataTableOutput("filteredTableSelected")
      )
    )
  ),
  tabPanel(
    "Weekly Calendar",
    mainPanel("Weekly calendar similar to DukeHub
                            (customizable colors for each class"),
    plotOutput("week"),
    DT::dataTableOutput("weekdata")
  ),
  tabPanel(
    "Class Info",
    mainPanel("Visualzation on the number of people in courses,
                            types of courses"),
    fluidRow(
      column(6, plotOutput("barplot")),
      column(6, plotOutput("piechart"))
    )
  ),
  tabPanel("Distance",
    mainPanel(actionButton("calculate", "Calcuate Commuter Distance",
      icon("calculator"),
      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
    )),
    width = 12,
    plotOutput("location"),
    DT::dataTableOutput("distanceTable")
  ),
  tabPanel(
    "Course Catalog Info",
    fluidRow(
      column(12, plotOutput("subjectPlot")),
      column(12, plotOutput("locinfo", height = 700)),
      column(12, plotOutput("distinfo"))
    )
  ),
  tabPanel(
    "Campus Map",
    leafletOutput("dukemap", width = "120%", height = 800)
  ),
  tabPanel(
    "Project Info",
    mainPanel(
      h1("About Our Project"),
      h2("Introduction"),
      br(),
      p("DukeHub is the academic portal used by Duke students, faculty, and advisers
to register for courses, make tuition payments, and view transcripts. Each
semester, students spend much time crafting their schedules to optimize campus
experience. The current DukeHub 2.0 has a “simple search” function to filter
courses by term and subject area and an “advanced class search” to filter by
course attributes, meeting times, instructor name, location, or the number of
units. Although DukeHub 2.0, launched in 2020, has improved user experience and
added more features compared to the previous version, our team would like to add
several features that make the course selection process more convenient. Our
DukeHub 3.0 is an R Shiny app that allows students to build their academic
schedule from 2408 courses/sections and provides additional insights to their
schedule through various visualizations. Once a student inputs their schedule,
the app will provide different visualizations such as the enrollment caps,
diversity of the subject areas they study, and the expected traveling during a
day based on the classes student select."),
      h1("Interactive functions"),
      br(),
      p("We designed 5 tabs for students to explore the nitty-gritty of their course
schedules, which will be introduced in detail in the following sections."),
      img(
        src = "tabs.jpg", height = 100, width = 1200,
        title = "overview of all the tabs in web app",
        subtitle = "overview of all the tabs in web app"
      ),
      h2("Schedule builder"),
      br(),
      p("Students can choose courses based on the specific subjects they would
like to take. Like the DukeHub 2.0, students can validate their schedule
to avoid time conflicts and overload limits (6 classes maximum), drop
classes back to the shopping cart, and clear all selections. If the
students didn’t choose any classes, the schedule builder will also
report invalid."),
      img(
        src = "invalid.jpg", height = 500, width = 700,
        title = "image when classes aren't selected",
        caption = "image when classes aren't selected"
      ),
      p("Our improvement from DukeHub 2.0 is that more information about classes
is available on the same page. Take an example of a student who wants to
choose African American Studies. In DukeHub 2.0, the dropdown list only
contains the course catalog numbers and descriptions after choosing the
subject."),
      img(
        src = "dukehub_old1.jpg", height = 500, width = 850,
        title = "preview of actual Dukehub 2.0 class",
        caption = "preview of actual Dukehub 2.0 class"
      ),
      p("Students need to click twice to see the time and location of a class,
which are difficult to find."),
      img(
        src = "dukehub_old2.jpg", height = 500, width = 800,
        title = "preview of actual Dukehub 2.0 more info window",
        caption = "preview of actual Dukehub 2.0 more info window"
      ),
      p("Our shiny app displays a comprehensive list of African American Studies
classes, including course catalog, description, enrollment cap,
location, time, teaching mode, etc. This will immediately give students
an idea of how far away this class is from their dorms, whether they
need to get up early to catch a bus, the class size, etc."),
      img(src = "newapp_aaas.jpg", height = 500, width = 800),
      h2("Weekly Calendar"),
      p("Once validated from the previous tab and step, the weekly calendar tab plots
the classes from Monday to Friday, 6AM - 10PM, colored by each individual
course. For accessibility, the classes show up in each proper rectangle with
the time interval of class start and ending time. This is so that the user
is able to get all of the information they truly need without needing to look
around the calendar too much."),
      br(),
      p("The image below is a picture of a sample course schedule. The plot covers
  Monday to Friday, even if there is no class on a certain day and from
  6AM to 10PM even if classes end earlier or later. As one can notice,
  from the schedule, the smallest width rectangle is the discussion for Hindi.
  This qualitatively confirms which class is shorter in time and helps
  the reader have a more accurate understanding of their courses without
  needing to refer to the reference course list below. To seperate the days,
  we added vertical lines. Also, each course is colored differenly. Even though
  both hindi components are in the same department and course number, they
  are still seperate course components and we wanted to reflect that. This
  comes in very handy when there is a special topics class like AAAS 190S,
  where there are multiple unique courses with the same Course Number, but
  different sections."),
      # insert schedule
      p("The goal with the schedule and all the features we added is so that
  the user can have a better experience with choosing their courses."),
      h2("Class information"),
      p("The enrollment caps of the student’s classes are represented in a bar chart
colored by subject areas. General areas include “Arts & Humanities”, “Natural
Sciences”, “Social Sciences”, “Engineering”, “Language”, “Physical Education”,
and “Writing” according to Duke academic curriculum. Students will be able to
make choices conveniently after visualizing the size of other classes. Subject
areas are shown in a pie chart to give the student an idea of how diverse their
classes are that semester."),
      br(),
      p("Here is an example of a schedule of 6 classes. We see that COMPSCI 201 is a
big introductory class with 300 enrollment cap, and the higher-level AAAS 109S
is smaller. Students might want to strike a balance regarding class sizes to
achieve a good learning experience. Although there are 3 CS classes, this
diverse schedule allows the student to explore other areas like Engineering,
Social Science, and Arts & Humanities as well."),
      br(),
      p("We hope these visualizations will help students determine on a schedule with
reasonable combination of classes sizes and subject areas and enjoy the liberal
arts education environment at Duke."),
      br(),
      img(src = "bar_chart.jpg", height = 500, width = 800),
      img(src = "pie_chart.jpg", height = 500, width = 800),
      h2("Distance"),
      br(),
      p("After understanding about one's schedule, the distance tab helps the user
  have a better idea of how much walking they can expect in a day, which is
  important when getting ready and deciding the attire that one wants to
  present themselves in."),
      br(),
      p("To calculate the cumulative distance, we assigned each location to a certain
  group based on the nearby building. For instance, the gray building, social
  sciences, and Rueben Cooke among others are assigned to group 3. Based on the
  longitude and latitude of the midpoint of each group, we were able to find
  the cumulative distance among each class for the day."),
      br(),
      # insert graph
      p("There are nuances to understand about this graph. For a day where there is
  only a single class, the bar is very short, but still present. This suggests
  that there should not be too much walking involved although it is
  still possible. We specifically decided not to include distance from the dorm
  because we can't assume that students come from a specific Duke dorm. Rather,
  they could live in an off-campus apartment or be coming from a libary or
  the Brodhead center. For our graph, there is a different color classification
  system as it is more important to color the bars by day rather than individual
  course code or subject area."),
      h2("Course Catalog Info"),
      br(),
      p("The students are able to find higher-level information about all courses
during that semester. Duke campus is huge and hard to navigate, so we want to
show the students the location distribution of all classes. From the donut
chart, we find that the places where the most classes are held are the main
quad of West Campus (888 classes) and on Science Drive (535 classes)."),
      img(src = "pie_dist.jpg"),
      p("The specific course students choose indicate their interest in that area, so
it is also useful to know the distribution of locations by these subject areas.
The example schedule in Class information contains classes from four subject
areas. The lollipop plot indicates that Arts & Humanities classes are mostly
held on the main quad of West Campus as well as places between East and West
(C-One Route), such as Rubinstein Arts Center, Nasher Museum of Art, and Smith
Warehouse; Natural Sciences classes are mostly held on Science Drive; and Social
Sciences classes are spread on both East and West campus."),
      br(),
      img(src = "dist_plot.jpg", height = 500, width = 800),
      h2("Campus Map"),
      br(),
      p("A common issue faced by first-years, based on those we've surveyed was the
  fact that it is time consuming to figure out where all of the buildings are.
  Considering that's 25% percent of our student body as well as all of the
  first-year graduate students in all programs, that is a very big issue that
  needs to be solved with a more accessible solution. Even though DukeHub has a
  Duke Map available, one would need to go to back to the login page and to the
  website to access their map. To save time, we included it as a
  different tab."),
      br(),
      img(src = "originalmap.jpg", height = 500, width = 800),
      br(),
      p("Furthermore on our Duke Map, we added highlighter circles for different
  building groups like East Campus-Main, Main Quad, Fitness, etc. Since this
  info was already given in the previous tab, it makes it easier for the user
  to find the relative group that their destination is in and scroll around
  there."),
      img(src = "newmap.jpg", height = 500, width = 800),
      br(),
      h2("Conclusion"),
      p("Ultimately, our goal was to create a better dukehub experience by offering
  new visualizations that actually matter to the user. A key tenet learned in
  the class on making good visualizations was the importance of talking to users.
  Taking this to heart, every feature and visualization we implemented was based
  on what others suggested Duke could have done better."),
      br(),
      h3("Designing for accessibility"),
      p("With the goal of creating a better DukeHub experience, designing for
  accessibility was even more crucial. To have a better understanding of the
  comparisons between the actual Dukehub 3.0 and our own rshiny web application,
  the alt text in the images of the write-up provided some really important
  description and context. For our coloring scale, we also used a colorblind
  friendly scale. We also used direct labeling for as many visualizations as
  we could in order to make it faster to read. We also used an accessibility
  tested font in Verdana.")
    )
  )
)
