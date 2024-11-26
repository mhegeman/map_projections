
<!-- README.md is generated from README.Rmd. Please edit that file -->

\#30DayMapChallenge

<!-- badges: start -->
<!-- badges: end -->

This is the project I used to create the maps for Day 9 (AI only) and
Day 26 (Map Projections) of the 2024 30 Day Map Challenge. The resulting
maps were created using Claude.ai. The list of the prompts I used to
create the R script that generated the maps is below.

# Map Projection Conversation Prompts

1.  “I would like to make a series of world maps in R illustrating all
    of the different map projections. Please list the map projections.”

2.  “This is a great start. I would like to make the maps a bit
    prettier. Can I make the oceans blue and the land green?”

3.  “The conical projections don’t have a blue background.”

4.  \[Error shown\] “ocean_polygon \<- st_as_sfc(ocean_bbox, crs = 4326)
    %\>% st_as_sf()”

5.  \[Warning message about size parameter\] “The `size` argument of
    `element_line()` is deprecated…”

6.  “Almost done. I want the world map to show, but be centered on North
    America. Right now it filters it to only show North America.”

7.  “Can I zoom in any closer to North America?”

8.  \[Error message\] “Error in FUN(X\[\[i\]\], …) : !anyNA(x) is not
    TRUE”

9.  “Can you create a text file that contains all of my prompts?”

This conversation shows the iterative development of R code to create
various map projections centered on North America, with improvements to
styling, visualization extent, and error handling along the way.
