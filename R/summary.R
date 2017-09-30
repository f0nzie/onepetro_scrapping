library(rvest)

publication_result_right <- function(result) {
    pub_doc <- result %>%
        html_nodes(".facet-unit-right") %>%
        html_nodes("option") %>%
        html_text()
    pub_doc
}

publication_result_left <- function(result) {
    pub_doc <- result %>%
        html_nodes(".facet-unit-left") %>%
        html_nodes("option") %>%
        html_text()
    pub_doc
}

summary_by_publisher <- function(x) {
    pub_vector <- get_dc_publisher(x)
    value <- extract_num_papers(pub_vector)
    name   <- extract_publishers(pub_vector)
    data.frame(name, value, stringsAsFactors = FALSE)
}

summary_by_doctype <- function(x) {
    doctype_vector <- get_dctype(x)
    value <- extract_num_papers(doctype_vector)
    name   <- extract_publishers(doctype_vector)
    data.frame(name, value, stringsAsFactors = FALSE)
}


summary_by_dates <- function(x) {
    pub_vector <- get_dc_issued_year(x)
    value <- extract_num_papers(pub_vector)
    name   <- extract_publishers(pub_vector)
    data.frame(name, value, stringsAsFactors = FALSE)
}

summary_by_publications <- function(x) {
    pub_vector <- get_s2_parent_title(x)
    value <- extract_num_papers(pub_vector)
    name   <- extract_publishers(pub_vector)
    data.frame(name, value, stringsAsFactors = FALSE)
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

get_dc_issued_year <- function(aList) {
    ix_stop <- grep("All publications", aList)
    aList[2:(ix_stop-1)]
}

get_s2_parent_title <- function(aList) {
    len_list <- length(aList)
    ix <- grep("All publications", aList)
    aList[(ix+1):len_list]
}



extract_num_papers <- function(x) {
    pattern <- "(?<=\\{).+(?=\\})"
    m <- regexpr(pattern, x, perl = TRUE)
    as.numeric(gsub(",", "" , regmatches(x, m)))
}


extract_publishers <- function(x) {
    # pattern <- "(\\s[{\\d}].+)"
    pattern <- "\\s{([0-9].*)}"
    gsub(pattern, "", x, perl = TRUE)
}



