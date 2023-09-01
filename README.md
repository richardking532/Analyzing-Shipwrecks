# Analyzing the Frequency and Location of Shipwrecks

This project is inspired by a data set I found on Kaggle containing the names of shipwrecks, the year they occurred, their coordinates, and some notes about their sinking.

```{r}
library(tidyverse)
library(plotly)
Shipwrecks <- read_csv("~/Programming/Portfolio/Sunken Ships/Shipwrecks and Sunken Ships.csv", na = '')
```

The data set did not appear to contain a unique ID field. Before cleaning the data, I wanted to check if any ship names were duplicated.

```{r}
Shipwrecks$Ship[duplicated(Shipwrecks$Ship)]
```
