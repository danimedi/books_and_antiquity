source("clean_data.R")

dat <- readr::read_csv("info2 - Sheet1.csv")

clean_data(dat$authors, dat$year)
