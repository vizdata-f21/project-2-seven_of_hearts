ui <- fluidPage(

  # App title ----
  titlePanel("DukeHub 3.0"),
  headerPanel(title = "Choose a course"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("Subjects", "Pick Department", choices = course_data$Subject),
      selectizeInput("Code", "Pick Code", choices = NULL)
    ),
    mainPanel(
        navbarPage("",
                   tabPanel("Schedule Builder",
                            tableOutput("view")),
                   tabPanel("Weekly Calendar",
                            mainPanel("Weekly calendar similar to DukeHub
                            (customizable colors for each class")),
                   tabPanel("Class Info",
                            mainPanel("Visualzation on the number of people in courses,
                            types of courses")),
                   tabPanel("Distance",
                            mainPanel("Visualization based on commuter distance")),
                   tabPanel("Reccomendations",
                            mainPanel("Recommendations on days to study and where
                            they should study and best days to go hang
                            with friends"))
    )
  )
)
)
