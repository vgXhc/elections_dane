
library(tidyverse)
library(tidycensus)
library(tmap)
library(sf)
library(readxl)
library(janitor)



# Ward shapefile from LTSB https://data-ltsb.opendata.arcgis.com/datasets/local-redistricting-2021-municipal-wards/
wards <- read_sf("data/Wards.shp")



# download files
# Turnout https://elections.countyofdane.com/Precincts-Result/145/0002
# this is not useful at the precinct level because it lacks a denominator
# download.file(url = "https://elections.countyofdane.com/Precinct-ResultExcel/145/0002",
#               destfile = "data/turnout.xlsx", mode = "wb")


# Gov https://elections.countyofdane.com/Precincts-Result/145/0004
download.file(url = "https://elections.countyofdane.com/Precinct-ResultExcel/145/0004",
              destfile = "data/gov.xlsx", mode = "wb")
governor <- read_excel(
  "data/gov.xlsx",
  skip = 7,
  col_names = c("precinct",
                "evers",
                "michels",
                "beglinger",
                "write_in"))

# AG https://elections.countyofdane.com/Precincts-Result/145/0005
download.file(url = "https://elections.countyofdane.com/Precinct-ResultExcel/145/0005",
              destfile = "data/ag.xlsx", mode = "wb")
ag <- read_excel(
  "data/ag.xlsx",
  skip = 7,
  col_names = c("precinct",
                "kaul",
                "toney",
                "write_in"))

# SOS https://elections.countyofdane.com/Precincts-Result/145/0006
download.file(url = "https://elections.countyofdane.com/Precinct-ResultExcel/145/0006",
              destfile = "data/sos.xlsx", mode = "wb")
sos <- read_excel(
  "data/sos.xlsx",
  skip = 7,
  col_names = c("precinct",
                "la_follette",
                "loudenbeck",
                "harmon",
                "mcfarland",
                "write_in"))

# Senator https://elections.countyofdane.com/Precincts-Result/145/0008
download.file(url = "https://elections.countyofdane.com/Precinct-ResultExcel/145/0008",
              destfile = "data/senate.xlsx", mode = "wb")
senate <- read_excel(
  "data/senate.xlsx",
  skip = 7,
  col_names = c("precinct",
                "mandela_barnes",
                "ron_johnson",
                "write_in"))

# District 2 Rep https://elections.countyofdane.com/Precincts-Result/145/0009
download.file(url = "https://elections.countyofdane.com/Precinct-ResultExcel/145/0009",
              destfile = "data/house_rep.xlsx", mode = "wb")
house <- read_excel(
  "data/house_rep.xlsx",
  skip = 7,
  col_names = c("precinct",
                "pocan",
                "olsen",
                "alexander",
                "write_in"))


  r
}
results <- results %>%
  mutate(
    other = con + lib + asp + write_in,
    total_votes = biden + trump + other,
    biden_pct = biden / total_votes * 100,
    trump_pct = trump / total_votes * 100
  ) %>%
  select(-c(con, lib, asp, write_in))
results_long <- results %>%
  pivot_longer(cols = 2:4,
               names_to = "candidate",
               values_to = "votes") %>%
  filter(str_detect(precinct, "C Madison")) %>%
  mutate(precinct = as.numeric(str_sub(precinct,-3)))
```

```{
  r
}
results_joined <- wards %>%
  inner_join(results_long, by = c("WARD" = "precinct")) %>%
  filter(total_votes > 25)
tmap_mode("view")
```
```{
  r results = T
}
tm_shape(results_joined) +
  tm_polygons(
    "biden_pct",
    style = "jenks",
    title = "Joe Biden",
    popup.vars = c(
      "Vote share Bide (%)" = "biden_pct",
      "Vote share Trump (%)" = "trump_pct",
      "Total votes cast" = "total_votes"
    ),
    legend.format = list(
      fun = function(x)
        paste0(formatC(x, digits = 0, format = "f"), "%")
    )
  )
```



# Absentee ballots

Local journalist [Jason Joyce](https: /  / twitter.com / jjoyce) compiled data on absentee ballots returned in a [neat spreadsheet](
  https: /  / docs.google.com / spreadsheets / d / 15cIkjmRWpipYfBNtUbVuS4_aaVEuSIXv2DbNEx5k00I /
    edit#gid=613969698). He last updated the data on November 1. I quickly wrote some code to create maps of his data.
  
  
  
  
  ```{
    r
  }
  gs4_deauth()
  absentee <-
    read_sheet(
      "https://docs.google.com/spreadsheets/d/15cIkjmRWpipYfBNtUbVuS4_aaVEuSIXv2DbNEx5k00I/edit#gid=613969698",
      skip = 2,
      n_max = 134,
      col_types = "iiiinininin",
      col_names = c(
        "WARD",
        "not_returned",
        "returned",
        "total",
        "perc_returned",
        "reg_voters",
        "perc_reg_voters",
        "2016_voters",
        "perc_2016_voters",
        "2016_absentee",
        "perc_2016_absentee"
      )
    ) %>%
    drop_na(any_of("perc_returned"))
  ```
  
  
  
  Here is the percentage of absentee ballots returned versus requested. I filtered out wards with fewer than 15 registered voters.
  ```{
    r
  }
  absentee_sf <-
    wards %>%
    inner_join(absentee, by = "WARD") %>%
    mutate(
      perc_returned = round(perc_returned * 100, 1),
      perc_2016_voters = round(perc_2016_voters * 100, 1)
    ) %>%
    select(-OBJECTID)
  ```
  
  
  ```{
    r results = T
  }
  tmap_mode("view")
  absentee_sf %>%
    filter(reg_voters > 15) %>%
    tm_shape() +
    tm_polygons(
      "perc_returned",
      style = "jenks",
      title = "Absentee ballots returned<br> as of Nov 1",
      popup.vars = c(
        "Absentee ballots returned (%)" = "perc_returned",
        "Ward" = "WARD",
        "Number of absentee ballots returned" = "returned"
      ),
      legend.format =
        list(
          fun = function(x)
            paste0(formatC(x, digits = 0, format = "f"), "%")
        )
    )
  ```
  
  And here are the absentee ballots returned compared to total votes cast in the 2016 election. I filtered out wards with fewer than 15 total votes in 2016,
  as they messed up the scale.
  
  ```{
    r results = T
  }
  absentee_sf %>%
    filter(`2016_voters` > 15) %>%
    tm_shape("WARD") +
    tm_polygons(
      "perc_2016_voters",
      alpha = .7,
      style = "jenks",
      popup.vars = c(
        "2020 absentee returned vs 2016 total (%)" = "perc_2016_voters",
        "Ward" = "WARD",
        "Number of absentee ballots returned" = "returned"
      ),
      title = "2020 absentee ballots returned<br>compared to 2016 total votes",
      legend.format =
        list(
          fun = function(x)
            paste0(formatC(x, digits = 0, format = "f"), "%")
        )
    )
  ```
  
  Note the low percentages on and around the UW campus and the high percentages on the far west and far east sides. 