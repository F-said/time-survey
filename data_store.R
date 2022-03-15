library(googlesheets4)

tabset_id <- "page_control"

questions <- dict()

questions$set("Question_1", "page0")$
  set("Present_0", "page1")$
  set("Present_1", "page2")$
  set("Present_2", "page3")$
  set("Present_3", "page4")$
  set("Question_2", "page5")$
  set("Question_3", "page6")$
  set("Question_4", "page7")$
  set("Question_5", "page8")$
  set("Present_4", "page9")$
  set("Present_5", "page10")$
  set("Present_6", "page11")$
  set("Present_7", "page12")$
  set("Question_6", "page13")$
  set("Question_7", "page14")$
  set("Question_8", "page15")$
  set("Question_9", "page16")$
  set("Present_8", "page17")$
  set("Present_9", "page18")$
  set("Present_10", "page19")$
  set("Question_10", "page20")$
  set("Question_11", "page21")$
  set("Question_12", "page22")$
  set("Question_13", "page23")$
  set("Present_11", "page24")$
  set("Present_12", "page25")$
  set("Present_13", "page26")$
  set("Question_14", "page27")$
  set("Question_15", "page28")$
  set("Question_16", "page29")$
  set("Question_17", "page30")$
  set("Done", "page31")

# Function to save session data on completion
table <- "responses"

save_data <- function(data) {
  # The data must be a dataframe rather than a named vector
  data <- data %>% as.list() %>% data.frame()
  # Add the data as a new row
  sheet_append(SHEET_ID, data)
}