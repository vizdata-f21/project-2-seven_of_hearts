ui <- fluidPage(

    # App title ----
    titlePanel("DukeHub 3.0"),

    # Sidebar layout with a input and output definitions ----
    sidebarLayout(

        # Sidebar panel for inputs ----
        sidebarPanel(

            selectInput(inputId = "housing",
                        label = "Where do you live?",
                        choices  = c("East Campus", "West Campus",
                                     "9th Street (Off Campus)", "Off Campus")),
            selectInput(inputId = "credits",
                        label = "How Many classes are you taking this semester?",
                        choices = c(1, 2, 3, 4, 5, 6)),

            selectInput(inputId = "study_location",
                        label = "Where do you primarily study?",
                        choices = c("Perkins Library", "Rubenstein Library",
                        "Law School", "Sanford", "Lilly Library", "My Room"
                        )),

            # Input: Selector for choosing dataset ----
            selectInput(inputId = "dataset",
                        label = "Choose a Subject Area:",
                        choices = c("Computer Science", "Statistics", "Economics",
                                    "English", "Asian & Middle Eastern Studies",
                                    "Literature", "Cinema")),

            # Input: Numeric entry for number of obs to view ----
            numericInput(inputId = "obs",
                         label = "Number of observations to view:",
                         value = 10),

            # Show only certain columns from dataframe
          #  $selectInput("Columns","Columns",
                       # names(mtcars), multiple = TRUE)
        ),

        # Main panel for displaying outputs ----
        mainPanel(
            navbarPage("",
            tabPanel(
                "Schedule Builder",

            # Output: Verbatim text for data summary ----
            verbatimTextOutput("summary"),
            verbatimTextOutput("dfStr"),

            # Output: HTML table with requested number of observations ----
            tableOutput("view")

        ),
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
                            with friends")))
    ))
)
