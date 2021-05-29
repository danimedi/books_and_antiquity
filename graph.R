# Packages ---------------
# library(googlesheets4)
library(tibble)
library(dplyr)
library(ggplot2)

# Download -----------------
# data_set <- read_sheet("https://docs.google.com/spreadsheets/d/1RVD96GNMStJdIvPzOMomu-fg9VY7L6SiAH8a1VFSWGw/edit#gid=0")
# data_set[sapply(data_set, is.character)] <- lapply(data_set[sapply(data_set, is.character)], as.factor)
# saveRDS(data_set, "data_set.rds", compress = FALSE)

# Read ----------------
data_set <- readRDS("data_set.rds")

# Process ---------------

# Separate in basic vs. clinical subjects
lookup1 <- tribble(
  ~course, ~type,
  "anatomy", "basic",
  "physiology", "basic",
  "microbiology", "basic",
  "pathology", "basic",
  "medicine", "clinical",
  "surgery", "clinical",
  "pediatrics", "clinical",
  "ginecology", "clinical"
)
data_set <- left_join(data_set, lookup1, by = "course")

# Translate courses to Spanish
lookup2 <- tribble(
  ~course, ~course_spanish,
  "anatomy", "Anatomía",
  "physiology", "Fisiología",
  "microbiology", "Microbiología",
  "pathology", "Patología",
  "medicine", "Medicina",
  "surgery", "Cirugía",
  "pediatrics", "Pediatría",
  "ginecology", "Ginecología"
)
lookup2 <- setNames(lookup2$course_spanish, lookup2$course)
data_set$course <- lookup2[data_set$course]

# Graph ------------------
ggplot(data_set) +
  geom_boxplot(aes(year, reorder(course, year, FUN = median, na.rm = TRUE))) +
  labs(x = "Año", y = "Curso")

# ggplot(data_set, aes(year, reorder(course, year, FUN = median, na.rm = TRUE))) +
#   geom_violin() +
#   geom_boxplot(width = 0.2, outlier.size = 1) +
#   geom_jitter(size = 0.6)


