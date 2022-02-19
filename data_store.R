library(googlesheets4)

tabset_id <- "page_control"

# Function to save session data on completion
table <- "responses"

save_data <- function(data) {
  # The data must be a dataframe rather than a named vector
  data <- data %>% as.list() %>% data.frame()
  # Add the data as a new row
  sheet_append(SHEET_ID, data)
}