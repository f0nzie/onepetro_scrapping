
#Loading the rvest package
library('rvest')

# Specifying the url for desired website to be scrapped
url <- "https://www.onepetro.org/search?start=0&q=neural+networks&from_year=&peer_reviewed=&published_between=&rows=999&to_year="

form_input <- list(dummy = "dummy", query = "?q=", peer_reviewed = "peer_reviewed=", 
                   published_between = "published_between=", 
                   from_year = "from_year=",
                   to_year = "to_year=", 
                   start = "start=", 
                   rows = "rows=",
                   dc_type = "dc_type=")

# Examples:
# https://www.onepetro.org/search?q=%22data+science%22&peer_reviewed=&published_between=&from_year=&to_year=
# 

send_url <- function(url, how = "any") {
  #Reading the HTML code from the website
  read_html(url)
}


get_papers_count <- function(url) {
  result <- send_url(url)
  
  papers <- result %>%
    html_nodes("h2") %>%
    html_text()
  
  # extract the numeric part of the results
  pattern <- "[\\d,]+(?= results.)"    # a number, including comma, before " results."
  m <- regexpr(pattern, papers[1], perl = TRUE)       # matched
  as.numeric(gsub(",", "", regmatches(papers[1], m))) # remove comma first
}






make_search_url <- function(query = NULL, start = NULL, from_year = NULL, 
                            peer_reviewed = NULL, 
                            published_between = NULL, 
                            rows = NULL, 
                            to_year = NULL, 
                            dc_type = NULL,
                            how = "any") {
    
    website <- "https://www.onepetro.org"
    
    if (!is.null(start) || !is.null(rows)) {
        if (!is.null(rows) & is.null(start)) start = 0  
        stopifnot(is.numeric(start), is.numeric(rows))
    }
    
    if (!is.null(from_year) && !is.null(to_year)) {
        stopifnot(is.numeric(from_year), is.numeric(to_year))
    }
    
    if (is.null(query)) {
        stop("search words not provided")
    } else {
        split_query <- unlist(strsplit(query, " "))
        if (length(split_query) > 1) {
            query <- paste(split_query, collapse = "+")
            # use function shQuote to add extra quotes when we want how = "all"
            #query <- ifelse(how == "all", dQuote(query), query)
            query <- ifelse(how == "all", shQuote(query), query)
            # query <- ifelse(how == "all", paste0("'", query, "'"), query)
            
            # print(query)
        }
    }
    
    if (!is.null(from_year) || !is.null(to_year)) {
        # use regex to validate year is between 1900 and 2099
        pattern <- "(?:(?:19|20)[0-9]{2})"
        if (!grepl(pattern, from_year, perl = TRUE) ||
            !grepl(pattern, to_year,   perl = TRUE)) stop("year not valid")
        # if valid year then turn on published_between
        published_between = "on"
        # if any of the *from* or *to* years are null replace with empty char
        if (is.null(from_year)) {
            from_year = ""
        }
        if (is.null(to_year)) {
            to_year = ""
        }
    }
    
    # peer_reviewed=on if TRUE; blank if unslected or FALSE
    if (is.null(peer_reviewed)) {
        peer_reviewed = ""
    } else {
        if (peer_reviewed) peer_reviewed = "on"
    }
    
    # document type
    if (!is.null(dc_type)) {
        valid_options <- c("conference-paper", "journal-paper", "general")
        # stop if it is not in the options
        if (!dc_type %in% valid_options) {
            msg <- sprintf("Option unknown. It must be one of [ %s ]", 
                           paste(valid_options, collapse = ", "))
            stop(msg)
            # cat(valid_options, "\n")
        }
    }
    
    s_search  <- paste(website, "search", sep = "/")
    
    # these strings will need to join with the ampersand & at the tail
    s_query   <- paste0("?q=", query)
    s_peer    <- paste0("peer_reviewed=", peer_reviewed)
    s_publish <- paste0("published_between=", published_between)
    s_from    <- paste0("from_year=", from_year)
    s_to      <- paste0("to_year=", to_year)
    s_start   <- paste0("start=", start) 
    s_rows    <- paste0("rows=", rows)
    s_type    <- paste0("dc_type=", dc_type)
    
    # url
    s_url <- list(websearch = s_search, query = s_query, peer = s_peer, 
                  published_between = s_publish, from_year = s_from, to_year = s_to,
                  start = s_start, rows = s_rows, dc_type = s_type
    )
    
    for (i in 1:length(s_url)) {
        # cat(i, my_url[[i]], "\n")
        if (i == 1) joined <- s_url[[i]]
        if (i == 2) joined <- paste0(joined, s_url[[i]])
        if (i >=3 ) {
            if (s_url[[i]] == form_input[[i]] & i <= 6) {
                joined <- paste(joined, s_url[[i]], sep = "&")
            } else  if (s_url[[i]] != form_input[[i]]) {
                cat(i, s_url[[i]], "\n")
                joined <- paste(joined, s_url[[i]], sep = "&")
            }
        }
    } 
    
    joined
}



read_titles <- function(webpage) {
    
    # title
    # .result-link
    
    #Using CSS selectors to scrap the rankings section
    title_data_html <- html_nodes(webpage, '.result-link')
    
    # Converting the ranking data to text
    title_data <- html_text(title_data_html)
    
    # data pre-processing
    title_data <- trimws(gsub("\n", "",title_data))
    
    
    #Let's have a look at the rankings
    title_data <- data.frame(title_data, stringsAsFactors = FALSE)
    # write.csv(df, file = "3000-conf.csv")
    return(title_data)
}

read_sources <- function(webpage) {
    # year, paper id, institution, type, year
    # .result-item-source
    
    #Using CSS selectors to scrap the rankings section
    source_data_html <- html_nodes(webpage, '.result-item-source')
    
    #Converting the ranking data to text
    source_data <- html_text(source_data_html)
    
    # pre-processing. split at \n
    source_data <- data.frame(do.call('rbind', strsplit(as.character(source_data),
                                                        '\n',fixed=TRUE)), 
                              stringsAsFactors = FALSE)
    # remove blank columns
    source_data <- source_data[, 2:5]
    # rename columns
    names(source_data) <- c("paper_id", "source", "type", "year")
    # remove dash from year
    source_data$year <- gsub("-", "", source_data$year)
    
    # Let's have a look at the paper source data
    source_data
}

read_author <- function(webpage) {
    # author #1
    #Using CSS selectors to scrap the rankings section
    author1_data_html <- html_nodes(webpage, '.result-item-author:nth-child(1)')
    
    #Converting the ranking data to text
    author1_data <- html_text(author1_data_html)
    
    # data pre-processing
    author1_data <- trimws(gsub("\n", "", author1_data))
    
    #Let's have a look at the rankings
    data.frame(author1_data, stringsAsFactors = FALSE)
}


onepetro_page_to_dataframe <- function(url) {
    webpage <- read_html(url)
    df_titles  <- read_titles(webpage)
    df_sources <- read_sources(webpage)
    df_author  <- read_author(webpage)
    
    # ensure that all dataframe have the same number of rows
    if (all(dim(df_titles)[1]  == dim(df_sources)[1], 
            dim(df_sources)[1] == dim(df_author)[1], 
            dim(df_author)[1]  == dim(df_titles)[1]
                                  ))
        cbind(df_titles, df_sources, df_author)
    else
        stop("Dataframe sizes different")  # otherwise, stop
}





onepetro_allpages_to_dataframe <- function(url) {
    # webpage <- read_html(url)
    papers_count <- get_papers_count(url)
    if (papers_count > 1000) {
        num_pages <- papers_count / 1000
    } else {
        num_pages = 1
    }
    
    info <- list(papers = papers_count, pages1000 = num_pages)
    
    for (page in seq_len(num_pages)) {
        # webpage <- read_html(url)
        
    }
    info
}




create_url <- function(start = NULL, query = NULL, from_year = NULL, 
                       peer_reviewed = NULL, 
                       published_between = NULL, 
                       rows = NULL, 
                       to_year = NULL, 
                       how = "any") {
    
    website <- "https://www.onepetro.org"
    
    if (is.null(start)) {
        start = ""
    }
    if (is.null(query)) {
        stop("search words not provided")
    } else {
        split_query <- unlist(strsplit(query, " "))
        if (length(split_query) > 1) {
            query <- paste(split_query, collapse = "+")
            # use function shQuote to add extra quotes when we want how = "all"
            query <- ifelse(how == "all", shQuote(query), query)
            print(query)
        }
    }
    print(query)
    
    if (is.null(from_year)) {
        from_year = ""
    }
    if (is.null(peer_reviewed)) {
        peer_reviewed = ""
    }
    if (is.null(published_between)) {
        published_between = ""
    }
    if (is.null(rows)) {
        rows = ""
    } else {
        if(is.null(start)) start = 0
    }
    if (is.null(to_year)) {
        to_year = ""
    }
    
    s_search  <- paste(website, "search", sep = "/")
    s_q       <- paste0("?q=", query)
    s_peer    <- paste0("peer_reviewed=", peer_reviewed)
    s_publish <- paste0("published_between=", published_between)
    s_from    <- paste0("from_year=", from_year)
    s_to      <- paste0("to_year=", to_year)
    s_start   <- paste0("start=", start) 
    s_rows    <- paste0("rows=", rows)
    
    url <- paste(s_q, s_peer, s_publish, s_from, s_to, sep = "&")
    url <- paste0(s_search, url)
    url
}