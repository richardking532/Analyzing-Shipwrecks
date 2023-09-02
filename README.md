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

## Extracting the Year Each Shipwreck Occurred

We want to know just the year that each shipwreck occurred. Looking at the 'Sunk date' column though, it appears the dates are in all sorts of different formats. We can start by extracting years from dates in date, month, year and month, year formats. Some years don't specify the century, and the code incorrectly assumes it to be the 21st century in some records, so we need to correct that after extracting the year.

```{r}
Shipwrecks <- Shipwrecks %>%
  mutate(Year_Sunk = ifelse(!is.na(lubridate::dmy(Shipwrecks$`Sunk date`)), as.character(year(lubridate::dmy(Shipwrecks$`Sunk date`))),
                     ifelse(!is.na(lubridate::my(Shipwrecks$`Sunk date`)), as.character(year(lubridate::my(Shipwrecks$`Sunk date`))), NA)))
Shipwrecks$Year_Sunk <- ifelse(as.numeric(Shipwrecks$Year_Sunk) >= 2000, as.character(as.numeric(Shipwrecks$Year_Sunk) - 100), Shipwrecks$Year_Sunk)
```

We can now search the 'Sunk date' column for any additional records container a 4-digit number, which we can assume to be a year.

```{r}
Shipwrecks <- Shipwrecks %>%
  mutate(Year_Sunk = ifelse(is.na(Shipwrecks$Year_Sunk) & str_detect(Shipwrecks$`Sunk date`, "\\d{4}"), str_extract(Shipwrecks$`Sunk date`, "\\d{4}"), Year_Sunk))
```

We need to convert our new Year_Sunk column to numeric.

```{r}
Shipwrecks$Year_Sunk <- as.numeric(Shipwrecks$Year_Sunk)
```

Reviewing the minimum and maximum years, it seems we have a shipwreck from the year 2200 - that's obviously not possible. Looking at the record, it was originally entered as 2200 BC. We'll remove it since we aren't interested in shipwrecks that old anyways.

```{r}
Shipwrecks <- Shipwrecks %>%
  mutate(Year_Sunk = ifelse(Shipwrecks$Year_Sunk == 2200, NA, Shipwrecks$Year_Sunk))
```

I wonder what years had the most shipwrecks. Let's pull the top 10 years.

```{r}
Years_With_Most_Shipwrecks <- Shipwrecks %>%
  filter(!is.na(Year_Sunk)) %>%
  group_by(Year_Sunk) %>%
  summarise(Total_Wrecks = n()) %>%
  arrange(desc(Total_Wrecks)) %>%
  head(10)
```

Here are our results. Looks like most shipwrecks occurred during the World War I and World War II.

## Preparing Coordinates for Plotting
