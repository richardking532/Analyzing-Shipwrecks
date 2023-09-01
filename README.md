# Analyzing the Frequency and Location of Shipwrecks

This project is inspired by a data set I found on Kaggle containing the names of shipwrecks, the year they occurred, their coordinates, and some notes about their sinking.

```{r}
library(tidyverse)
library(plotly)
Shipwrecks <- read_csv("~/GitHub/Analyzing-Shipwrecks/Shipwrecks and Sunken Ships.csv", na = '')
glimpse(Shipwrecks)
```

The data set did not appear to contain a unique identifier. Before cleaning the data, I wanted to check if any ship names were duplicated.

```{r}
Shipwrecks$Ship[duplicated(Shipwrecks$Ship)]
```

Wow! 241 ship names were repeated. It is possible that ship names have been reused throughout history. Let's see if any records are complete duplicates.

```{r}
duplicates <- duplicated(Shipwrecks)
Shipwrecks[duplicates, ]
```

It appears four records are duplicates. It will be best to remove them.

```{r}
Shipwrecks <- unique(Shipwrecks)
```

We should add a unique identifier to prevent mixing up shipwrecks with the same name.

```{r}
Shipwrecks <- rowid_to_column(Shipwrecks, "Ship_Id")
```

We want to know just the year that each shipwreck occurred. Looking at the 'Sunk date' column though, it appears the dates are in all sorts of different formats.

```{r}
Shipwrecks <- Shipwrecks %>%
  mutate(Year_Sunk = ifelse(!is.na(lubridate::dmy(Shipwrecks$`Sunk date`)), as.character(year(lubridate::dmy(Shipwrecks$`Sunk date`))),
                     ifelse(!is.na(lubridate::my(Shipwrecks$`Sunk date`)), as.character(year(lubridate::my(Shipwrecks$`Sunk date`))), NA)))
```
