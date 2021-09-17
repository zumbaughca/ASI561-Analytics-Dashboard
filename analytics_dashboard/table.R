library(flextable)
library(tidyverse)

create_table <- function(data, tbl_width = 1) {
  ft <- flextable(data) %>%
    theme_booktabs(bold_header = TRUE) %>%
    fontsize(size = 20, part = "header") %>%
    fontsize(size = 18, part = "body") %>%
    set_table_properties(layout = "autofit",
                         width = tbl_width) %>%
    htmltools_value()
  return(ft)
}

