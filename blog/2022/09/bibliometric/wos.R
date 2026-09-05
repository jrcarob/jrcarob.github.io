library(bibliometrix)
library(xlsx)

### WoS test
# file <- "https://www.bibliometrix.org/datasets/wos_bibtex.bib"
# db <- convert2df(file, dbsource = "wos", format = "bibtex")
######

## importing web of science dataset
#wos <- convert2df("wos.txt")
#wos <- convert2df("wos.txt", dbsource = "wos", format = "plaintext")
wos <- convert2df("wos.bib", dbsource = "wos", format = "bibtex")

#biblioshiny()

###########################

#options(width=160)
results <- biblioAnalysis(wos)

# main findings
summary(results, k=10, pause=F, width=130)
plot(x=results, k=10, pause=F)


# most cited references
CR <- citations(scopus, field = "article", sep = ";")
cbind(CR$Cited[1:20])

# Article (References) co-citation analysis
NetMatrix <- biblioNetwork(scopus, analysis = "co-citation", network = "references", sep = ";")

net=networkPlot(NetMatrix, n = 50, Title = "Co-Citation Network", type = "fruchterman", 
                size.cex=TRUE, size=20, remove.multiple=FALSE, labelsize=1,edgesize = 10, edges.min=5)

# Journal (Source) co-citation analysis
scopus = metaTagExtraction(scopus,"CR_SO",sep=";")

# by sources

NetMatrix <- biblioNetwork(scopus, analysis = "co-citation", network = "sources", sep = ";")

net=networkPlot(NetMatrix, n = 50, Title = "Co-Citation Network", type = "auto", 
                size.cex = TRUE, size = 15, remove.multiple = FALSE, labelsize = 1, edgesize = 10, edges.min=5)

# by references

NetMatrix2 <- biblioNetwork(scopus, analysis = "co-citation", network = "references", sep = ";")

net2=networkPlot(NetMatrix2, n = 50, Title = "Co-Citation Network by references", type = "auto", 
                 size.cex = TRUE, size = 30, remove.multiple = TRUE, labelsize = 1, edgesize = 10, edges.min = 5)

# Historiograph - Direct citation linkages

