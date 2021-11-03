ui <- fluidPage(

    # App title ----
    titlePanel("DukeHub 3.0"),

    # Sidebar layout with a input and output definitions ----
    sidebarLayout(

        # Sidebar panel for inputs ----
        sidebarPanel(

            # Input: Selector for choosing dataset ----
            selectInput(inputId = "dataset",
                        label = "Choose a Subject Area:",
                        choices = c("Computer Science", "Statistics", "Economics")),

            # Input: Numeric entry for number of obs to view ----
            numericInput(inputId = "obs",
                         label = "Number of observations to view:",
                         value = 10)
        ),

        # Main panel for displaying outputs ----
        mainPanel(

            # Output: Verbatim text for data summary ----
            verbatimTextOutput("summary"),

            # Output: HTML table with requested number of observations ----
            tableOutput("view")

        )
    )
)
