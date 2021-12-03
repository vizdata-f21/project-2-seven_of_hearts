library(shinythemes)
library(shinyalert)
library(leaflet)

ui <-  navbarPage("DukeHub 3.0",
                 tabPanel("ScheduleBuilder",

                   sidebarLayout(
                     sidebarPanel (
                       selectizeInput(inputId = "dataset",
                                      label = "Choose a Subject Area:",
                                      choices = c("African American Studies",
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
                                      multiple = FALSE)

                     ),
                     mainPanel(
                       "Schedule Builder",
                       # Output: HTML table with requested number of observations ----
                       verbatimTextOutput("Select Classes to add to Schedule"),
                       DT::dataTableOutput("view"),
                       actionButton("add", label = "Add Course To BookBag"),
                       verbatimTextOutput("Selected Courses"),
                       DT::dataTableOutput("filteredTableSelected"),
                       actionButton("clear", label = "Clear BookBag"),
                       useShinyalert(),
                       actionButton("validate", label = "Validate Schedule")


                     )



                   )

                 ),
                 tabPanel("Weekly Calendar",
                          mainPanel("Weekly calendar similar to DukeHub
                            (customizable colors for each class"),
                          plotOutput("week"),
                          DT::dataTableOutput("weekdata")),
                 tabPanel("Class Info",
                          mainPanel("Visualzation on the number of people in courses,
                            types of courses"),
                          fluidRow(column(6, plotOutput("barplot")),
                                   column(6, plotOutput("piechart")))
                          ),
                 tabPanel("Distance",
                          mainPanel("Visualization based on commuter distance"),
                          DT::dataTableOutput("distanceTable"),
                          actionButton("calculate", "Calcuate Commuter Distance"),
                          plotOutput("location")),
                 tabPanel("Course Catalog Info",
                          mainPanel("An overview of the class info"),
                          fluidRow(column(12, plotOutput("subjectPlot")),
                                   column(12, plotOutput("locinfo", height = 700)),
                                   column(12, plotOutput("distinfo"))
                                   )),
                 tabPanel("Campus Map",
                          leafletOutput("dukemap", width = "120%", height = 800)
                          ),
                 tabPanel("Project Info",
                          sidebarPanel("About our Project"),
          mainPanel(
  h1("Introduction"),
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
img(src = "tab.jpg")
                          )

                          )
                 )