library(rvest)

publication_result <- function(result) {
    pub_doc <- result %>%
        html_nodes(".facet-unit-right") %>%
        html_nodes("option") %>%
        html_text()
    pub_doc
}

summary_by_publisher <- function(x) {
    pub_vector <- get_dc_publisher(x)
    pub_values <- extract_num_papers(pub_vector)
    pub_name   <- extract_publishers(pub_vector)
    data.frame(pub_name, pub_values, stringsAsFactors = FALSE)
}

summary_by_doctype <- function(x) {
    doctype_vector <- get_dctype(x)
    doctype_value <- extract_num_papers(doctype_vector)
    doctype_name   <- extract_publishers(doctype_vector)
    data.frame(doctype_name, doctype_value, stringsAsFactors = FALSE)
}


get_dctype <- function(aList) {
    len_list <- length(aList)
    ix <- grep("All types", aList)
    aList[(ix+1):len_list]
}

get_dc_publisher <- function(aList) {
    ix_stop <- grep("All types", aList)
    aList[2:(ix_stop-1)]
}


extract_num_papers <- function(x) {
    pattern <- "(?<=\\{).+(?=\\})"
    m <- regexpr(pattern, x, perl = TRUE)
    as.numeric(gsub(",", "" , regmatches(x, m)))
}


extract_publishers <- function(x) {
    pattern <- "(\\s[{\\d}].+)"
    gsub(pattern, "", x, perl = TRUE)
}



