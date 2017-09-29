library(rvest)

publication_result <- function(result) {
    pub_doc <- result %>%
        html_nodes(".facet-unit-right") %>%
        html_nodes("option") %>%
        html_text()
    pub_doc
}

summary_by_doctype <- function(x) {
    doctype_vector <- get_dctype(x)
    doctype_value <- extract_num_papers(doctype_vector)
    doctype_name   <- extract_publishers(doctype_vector)
    data.frame(doctype_name, doctype_value, stringsAsFactors = FALSE)
}

