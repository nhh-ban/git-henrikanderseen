library(tidyverse)
# Skeleton file 1 for Assignment 5 in BAN400. 
# -------------------------------------------

# Comments below describes briefly a set of steps that solves Problem 5.

# Read the entire data file into memory using the readLines()-function. Use the
# URL direcly or read the data from the local file that is in the repository.
lines <- readLines("suites_dw_Table1.txt")

# Identify the line number L of the separator line between the column names and
# the rest of the data table.
find_separator_line <- function(lines) {
  for(i in seq_along(lines)) {
    line <- lines[i]
    # Check if the line contains only non-alphanumeric characters
    # This will identify lines with characters like -, =, +, etc.
    if(!grepl("^\\s*$", line) &&
       grepl("^[^a-zA-Z0-9]+$", line)) {
      return(i+1)
    }
  }
  return(NULL)
}

L <- find_separator_line(lines)
L

# Save the variable descriptions (i.e. the information in lines 1:(L-2)) in a
# text-file for future reference using the cat()-function
file_name <- "variable_descriptions.txt"
descriptions <- lines[1:(L - 3)]
cat(descriptions, file = file_name, sep = "\n")

# Extract the variable names (i.e. line (L-1)), store the names in a vector.
variable_names_vector <- sapply(lines[1:(L - 3)], function(lines) {
  words <- unlist(strsplit(lines, "\\s+"))
  return(words[1])
})

variable_names_vector
# Read the data. One way to do this is to rewrite the data to a new .csv-file
# with comma-separators for instance using cat() again, with the variable names
# from the step above on the first line (see for instance paste() for collapsing
# that vector to a single string with commas as separators).
# Collapse the variable names into a single comma-separated string
header_string <- paste(variable_names_vector, collapse = ",")
csv_file_name <- "rewritten_data.csv"
cat(header_string, file = csv_file_name, sep = "\n")

shortened_lines <- lapply(lines[(L+1):length(lines)], function(line) {
  line <- gsub("\\|", "", line)
  words_or_numbers <- unlist(strsplit(line, "\\s+"))
  result_line <- paste(words_or_numbers, collapse = ",")
  sub("^,", "", result_line)
})

# Convert the list back to a character vector
shortened_lines_vector <- unlist(shortened_lines)

# Append to the .csv file without overwriting existing content
cat(shortened_lines_vector,
    file = "rewritten_data.csv",
    append = TRUE, sep = "\n")

# Read the finished .csv back into R in the normal way.
reread_df <- read.csv("rewritten_data.csv")


## Problem 3 --------------------

library(ggplot2)
reread_df$a_26 <- as.numeric(as.character(reread_df$a_26))

summary(reread_df$a_26)

# Histogram for linear diameter (a_26)
ggplot(reread_df, aes(x=a_26)) +
  geom_histogram(fill = "blue", alpha = 0.7, bins = 30) +
  labs(title = "Distribution of Galaxy Linear Diameter (a_26)",
       x = "Linear Diameter (kpc)",
       y = "Number of Galaxies") +
  theme_minimal()

#Can see that a clear majority of galaxies are on the smaller end of the scale

