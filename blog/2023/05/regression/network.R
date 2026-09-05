# https://stats.stackexchange.com/questions/430259/what-are-the-branches-of-statistics
# https://data.stackexchange.com/stats/query/edit/1809065
# select Tags from Posts where PostTypeId = 1 and Score >2

#the sql-script saved like an sql file
network <- read.csv("network.csv", stringsAsFactors = 0)
#it looks like this:
network[1][1:5,]

l <- length(network[,1])
nk <- 1
keywords <- c("<r>") # 
M <- matrix(0,1)

for (j in 1:l) {                              # loop all lines in the text file
  s <- stringr::str_match_all(network[j,],"<.*?>")           # extract keywords
  m <- c(0)                                             
  for (is in s[[1]]) {
    if (sum(keywords == is) == 0) {           # check if there is a new keyword
      keywords <- c(keywords,is)              # add to the keywords table
      nk<-nk+1
      M <- cbind(M,rep(0,nk-1))               # expand the relation matrix with zero's
      M <- rbind(M,rep(0,nk))
    }
    m <- c(m, which(keywords == is))
    lm <- length(m)
    if (lm>2) {                               # for keywords >2 add +1 to the relations
      for (mi in m[-c(1,lm)]) {
        M[mi,m[lm]] <- M[mi,m[lm]]+1
        M[m[lm],mi] <- M[m[lm],mi]+1
      }
    }
  }
}


#getting rid of <  >
skeywords <- sub(c("<"),"",keywords)
skeywords <- sub(c(">"),"",skeywords) 


# plotting connections 

library(igraph)
library("visNetwork")

# reduces nodes and edges
Ms<-M[-1,-1]             # -1,-1 elliminates the 'r' tag which offsets the graph
Ms[which(Ms<50)] <- 0
ww <- colSums(Ms)
el <- which(ww==0)

# convert to data object for VisNetwork function
g <- graph.adjacency(Ms[-el,-el], weighted=TRUE, mode = "undirected")
data <- toVisNetworkData(g)

# adjust some plotting parameters some 
data$nodes['label'] <- skeywords[-1][-el]
data$nodes['title'] <- skeywords[-1][-el]
data$nodes['value'] <- colSums(Ms)[-el]
data$edges['width'] <- sqrt(data$edges['weight'])*1
data$nodes['font.size'] <- 20+log(ww[-el])*6
data$edges['color'] <- "#eeeeff"

#plot
visNetwork(nodes = data$nodes, edges = data$edges) %>%
  visPhysics(solver = "forceAtlas2Based", stabilization = TRUE,
             forceAtlas2Based = list(nodeDistance=70, springConstant = 0.04,
                                     springLength = 50,
                                     avoidOverlap =1)
  )
