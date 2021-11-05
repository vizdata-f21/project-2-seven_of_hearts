DukeHub 3.0 Proposal
================
Seven of Hearts: Yihan, Kartik, Kate Straneva

### Load Packages

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(readr)
library(knitr)
```

### Load Course Data

``` r
course_data <-  read_csv(here::here("data/course_catalog.csv"))
```

    ## Rows: 2703 Columns: 13

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (9): Subject, Catalog, Descr, Section, Pat, Mode, Descr 1, Career, Term...
    ## dbl  (2): Unique Class Identifier, Cap Enrl
    ## time (2): Mtg Start, Mtg End

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
building_distance <- read_csv(here::here("data/STA 313 Team 7 data set - Building groups.csv"))
```

    ## Rows: 54 Columns: 3

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Location, Group Category
    ## dbl (1): Group Number

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
building_groups <- read_csv(here::here("data/STA 313 Team 7 data set - Building Distance.csv"))
```

    ## Rows: 10 Columns: 11

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (11): group, dist_1, dist_2, dist_3, dist_4, dist_5, dist_6, dist_7, dis...

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

### Data Cleanup

``` r
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
```

``` r
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
```

### Glimpse Data

``` r
glimpse(course_data)
```

    ## Rows: 2,408
    ## Columns: 13
    ## $ class_identifier <dbl> 1.7901e+14, 1.7901e+14, 1.7901e+14, 1.7901e+14, 1.790…
    ## $ Subject          <chr> "AMES", "LIT", "CINE", "VMS", "AAAS", "AAAS", "AAAS",…
    ## $ catalog_number   <chr> "161", "213", "255", "232", "190S", "190S", "102", "1…
    ## $ Descr            <chr> "JAPANESE CINEMA", "JAPANESE CINEMA", "JAPANESE CINEM…
    ## $ Section          <chr> "1", "1", "1", "1", "4", "1", "1", "1", "1", "1", "1"…
    ## $ enroll_cap       <dbl> 40, 40, 40, 40, 18, 13, 28, 28, 32, 32, 32, 32, 32, 1…
    ## $ days             <chr> "MW", "MW", "MW", "MW", "MW", "TH", "TTH", "TTH", "T"…
    ## $ mtg_start        <time> 15:30:00, 15:30:00, 15:30:00, 15:30:00, 19:00:00, 10…
    ## $ mtg_end          <time> 16:45:00, 16:45:00, 16:45:00, 16:45:00, 20:15:00, 12…
    ## $ Mode             <chr> "In Person", "In Person", "In Person", "In Person", "…
    ## $ location         <chr> "Classroom Buiding", "Classroom Buiding", "Classroom …
    ## $ Career           <chr> "UGRD", "UGRD", "UGRD", "UGRD", "UGRD", "UGRD", "UGRD…
    ## $ term             <chr> "2022 Spring Term", "2022 Spring Term", "2022 Spring …

``` r
glimpse(building_distance)
```

    ## Rows: 54
    ## Columns: 3
    ## $ Location         <chr> "Art Building", "Baldwin Auditorium", "Bell Tower", "…
    ## $ `Group Number`   <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3,…
    ## $ `Group Category` <chr> "EC-Back", "EC-Back", "EC-Back", "EC-Back", "EC-Back"…

``` r
glimpse(building_groups)
```

    ## Rows: 10
    ## Columns: 11
    ## $ group   <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
    ## $ dist_1  <dbl> 0.2, 0.4, 2.2, 1.6, 2.3, 1.5, 1.3, 2.8, 2.1, 4.1
    ## $ dist_2  <dbl> 0.4, 0.2, 1.8, 1.2, 1.9, 1.1, 0.9, 2.4, 1.7, 3.7
    ## $ dist_3  <dbl> 2.2, 1.8, 0.4, 0.6, 0.6, 0.4, 1.0, 1.1, 0.7, 2.5
    ## $ dist_4  <dbl> 1.6, 1.2, 0.6, 0.4, 0.5, 0.6, 0.5, 1.0, 0.5, 2.6
    ## $ dist_5  <dbl> 2.3, 1.9, 0.6, 0.5, 0.4, 0.8, 1.1, 0.5, 0.4, 1.9
    ## $ dist_6  <dbl> 1.5, 1.1, 0.4, 0.6, 0.8, 0.4, 0.7, 1.3, 0.8, 2.8
    ## $ dist_7  <dbl> 1.3, 0.9, 1.0, 0.5, 1.1, 0.7, 0.4, 1.5, 0.8, 3.0
    ## $ dist_8  <dbl> 2.8, 2.4, 1.1, 1.0, 0.5, 1.3, 1.5, 0.4, 0.7, 1.9
    ## $ dist_9  <dbl> 2.1, 1.7, 0.7, 0.2, 0.4, 0.8, 0.8, 0.7, 0.1, 2.2
    ## $ dist_10 <dbl> 4.1, 3.7, 2.5, 2.6, 1.9, 2.8, 3.0, 1.9, 2.2, 0.1

### High Level Overview

Create DukeHub 3.0, an R Shiny app that allows students to build their
academic schedule and provides additional insights to their schedule
through data visualization.

### Project Description, Goals, & Motivation

Dukehub is an academic portal used by students, faculty, and advisers to
view courses, make tuition payments, and view transcripts. Each semester
students spend countless crafting their schedules and back-up schedules.
Students can find classes using a “simple search” by term and subject
area. Students can also find classes using an “advanced class search”
inputting the course attributes, meeting times, instructor name,
location, or the number of units. Duke launched DukeHub 2.0 in 2020 to
improve user experience and add more features. DukeHub 2.0 is very
useful, but our team wishes it would provide more information beyond
schedule building. Our team’s goal is to create an improved DukeHub
using a shiny app which allows students to select courses for their
schedule and provides additional information about their schedule based
on their selection. This will include information regarding estimated
commute times, distance traveled, and recommendations for studying and
relaxing based on their selection. There are 5 tabs in our R shiny app
with different functionalities:

-   **Schedule Builder**: allows students to add classes to their
    schedule by Subject Area or Course Name.

-   **Weekly Calendar** course schedule in a calendar based on student
    selection. If there is overlapping class times, an error will occur
    (visually).

-   **Class Info**: Visualzations on the number of people in courses and
    types of courses the student is taking.

-   **Distance** : Geo-spatial visualization showing the distance
    traveled during a specific day or week; and commute times between
    buildings.

-   **Reccomendations**: Based on inputted schedule, the app will
    provide reccomendations on where and when a student should study and
    what days they are most available to get meals with friends.

Here is an image of DukeHub 3.0.

<div class="figure" style="text-align: center">

<img src="/home/guest/STA_313L_hw/project-2-seven_of_hearts/data/DukeHub3.0.png" alt="DukeHub 3.0" width="2814" />
<p class="caption">
DukeHub 3.0
</p>

</div>

To complete our project, we will be using a course catalog data set that
we requested from the Duke University Registrar’s Office (thank you Dr. 
Blalark). This data set contains 2,408 observations of 13 variables.
Each observation in the dataset represents a course which is offered to
undergraduates during the Spring 2022 term. There are 13 variables in
the dataset: `class_identifier`, `Subject`, `catalog_number`, `Descr`,
`Section`, `enroll_cap`, `days`, `mtg_start`, `mtg_end`, `Mode`,
`location`, `career`, `term`. We cleaned some of the variable names to
make them more straighforward, for example, `Descr` is renamed to
`location`, `Pat` is renamed to `days`, etc. Additionally, we filtered
coures which are reserved for seniors theses, reservered for freshmen,
take place at Duke’s marine lab, or do not have location listed,

Additionally, we are creating a distance data frame which describes the
distance between buildings. For convenience, buildings that are close to
each other are grouped together.

### Weekly Plan of Attack

-   Our team is available to meet on Tuesdays and Thursdays from 10:30
    AM to 12:00 PM.

-   Week 1 of project (week of Mon, Oct 18): Kate, Yihan, and Kartik
    contributed 3 potential ideas each and we chose one that would be
    the most interested. We decided to build a R Shiny app similar to
    Dukehub.

-   Week 2 of project (week of Mon, Oct 25):

    -   Kate: project proposal - motivation and goal; create basic R
        shiny app with UI and server files.
    -   Yihan: project proposal - introduction and description of each
        dataset
    -   Kartik: weekly plans

-   Week 3 of project (week of Mon, Nov 1):

    -   Kate: Request Course data from Registrar’s office and cleans
        data
    -   Yihan: Finish proposal and organize repo
    -   Kartik: Finish proposal and creating datasets

-   Week 4 of project (week of Mon, Nov 8): Conduct peer review on
    project proposals, and optionally, submit in an updated version of
    your proposal.

    -   All members: discuss peeer review; make any necessary
        modifications.
    -   Kate: Course builder tab
    -   Yihan: Class information tab
    -   Kartik: Weekly Calendar tab

-   Week 5 of project (week of Mon, Nov 15): Continue working on the
    project, identify issues and consult TA/professor

    -   Kate: Distance tab
    -   Yihan: Reccomendations tab
    -   Kartik: Distance tab and reccomendations tab

-   Week 6 of project (week of Mon, Nov 22): Continue working on the
    project.

    -   Kate: End user testing: scheudule builder tab and distance tab.
    -   Yihan: End user testing: class information tab, recommendation
    -   Kartik: End user testing: recommendations tab, distance tab

-   Week 7 of project (week of Mon, Nov 29): Conduct another round of
    peer review.

    -   Make any adjustments based on peer review or end user testing.
    -   Kate: UI enhancements to schedule builder tab and distance tab.
    -   Yihan: UI enhancements to class information tab, recommendation
    -   Kartik: UI enhancements to recommendations tab, distance tab,

-   Week 8 of Project(Due Date): Present our project

    -   All team members: practice presentation leading up to final
        presentation

### Organization of Project Repository:

-   README.md
    -   Introduction of our high level goal of the project
-   Data folder contains 3 data sets and a README file:
    -   course\_catalog.csv: All classes offered to undergraduates at
        Duke University in the Spring 2022 term.
    -   building\_dist.csv: Distances between each building group
    -   building\_group.csv: The groups each building belongs to
    -   README.md for data folder
    -   DukeHub3.0.png: Screenshot of our app
-   Proposal Folder :
    -   proposal.rmd
-   DukeHub3.0
    -   ui.R
    -   server.R
