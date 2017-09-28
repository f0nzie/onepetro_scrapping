# thepage = readLines("https://www.onepetro.org/search?start=0&q=neural+networks&from_year=&peer_reviewed=&published_between=&rows=100&to_year=")

# mypattern <- "data-p13n-title="
mypattern <- 'data-p13n-title=\"([^<]*)\"></div>'

datalines = grep(mypattern,thepage[500:length(thepage)],value=TRUE)
# print(datalines)
getexpr = function(s,g) substring(s,g,g+attr(g, 'match.length')-1)
gg = gregexpr(mypattern, datalines)
matches = mapply(getexpr, datalines, gg)
result = gsub(mypattern, '\\1', matches)
print(result[1:5])
