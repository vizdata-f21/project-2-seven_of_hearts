
### DukeHub 3.0 Data 

## Name: course_catalog.csv

|Variable         |Data type   |Description |
|:----------------|:-----------|:--------------------|
|class_identifier| double| unique identifier for each class|
|Subject    | character|class subject area| 
|catalog_number   |chr |catalog number| 
|Descr   | character|class description| 
|Section    | character| class Section number| 
|enroll_cap   | double|class subject area| 
|days  | character| days classes meet| 
|mtg_start   | time| military start time for class| 
|mtg_end   | time| military end time for class| 
|Mode   | character | class mode |
|location   | character | building the class is in |
|Career  | character | career the class is offered in|
|term  | character | term the class is offered |


## Name: building_dist.csv
|Variable         |Data type   |Description |
|:----------------|:-----------|:--------------------|
|group| integer| group number of buildings|
|dist_1| integer| time to travel from group 1|
|dist_2| integer| time to travel from group 2|
|dist_3| integer| time to travel from group 3|
|dist_4| integer| time to travel from group 4|
|dist_5| integer| time to travel from group 5|


## Name: building_group.csv
|Variable         |Data type   |Description |
|:----------------|:-----------|:--------------------|
|group| integer| group number of building|
|location| character | the name of building|
