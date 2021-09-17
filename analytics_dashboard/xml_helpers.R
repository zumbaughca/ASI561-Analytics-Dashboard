library(XML)
library(rvest)

get_string <- function(tag_name) {
  xml_file <- xmlParse('www/resources.xml')
  root <- xmlRoot(xml_file)
  strings <- xmlChildren(root)["STRINGS"]
  text <- strings$STRINGS[[toupper(tag_name)]][1]$text
  return(capture.output(text))
}
