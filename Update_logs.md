# Update Logs

## 5.1

- Reconstruct the app frame to extend its compatibility with the different datasets.
- Add visitor mode to let the app can be executed on sample data.
- Add interface descriptions by using Markdown documents.
- Bug fixes.



## 3.22

- Updated the "README.md". Please read it if you cannot run the app because of "wrong path".
- Removed the "comma" existed in the displayed years.
- Fixed the bug that the filtered table cannot be downloaded before.
- The list of variables in the select box can be automatically formed now, so the name of the displayed variable can vary with the input data resources.
- Revised the application structure to separated frame now. All the pre-loaded functions and variables have been settled in an isolated file, so that the loading and reacting time of the application will be shorter than before.



## 1.25

- Changed UI designing package from "shiny" to "shinyBoard". Now it has a brand new UI having better performance.

- Instead of "submitButton" with "actionButton". Now the "Submit" button can control separated functions.

  ​

## 1.16

- Added date range control in the "Search Function" page.
- Renamed "FIRE_NUM" (may mix up with "Fire ID") to "FIRE_TIMES" (how many times the fire happened in that day.)
- Renamed "FIRE_SIZE" to "TOTAL_BURNED_SIZE", "HUMAN" to "HUMAN_CAUSED", "LIGHTING" to "LIGHTING_CAUSED".

## 1.15

- Bug Fixes
- Adjusted parts of layout
- Allowed user to download the filtered data table
- Added mini bar to adjust the sensitivity rate of each indicator



## 12.10

- Fixed a bug that may cause the incorrect display of `Fire_Num`
- Rewrote the algorithm related with the fire events.



## 10.23

- Replaced several "For loop" structure with "Apply function"
- Rearranged the core files

