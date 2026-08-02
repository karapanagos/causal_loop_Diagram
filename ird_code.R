#packages
library(igraph)


###  DYNAMICS OF NEONATAL MORTALITY IN UGANDA ####


# specify variables -------------------------------------------------------


variables <- c("mothers attending ANC, hospital deliveries and PNC",
               "level of awareness of MHC & NHC",
               "health of mothers",
               "care of newborn",
               "death risk of neonate",
               "safe deliveries and PNC",
               "socio-economic status",
               "resource adequacy (staffing, drugs, log & supplies)",
               "neonatal survival",
               "mothers' birth preparedness",
               "health education by health workers",
               "perceptions and belief in myths")

codes <- c("ATTEND", "AWARE", "MHLTH", "NBCARE", "NRISK", "SAFEDEL",
           "SES", "RESRC", "NSURV", "BPREP", "HEDUC", "MYTHS")
# Combined lookup table (useful for reference, export, or plotting)
var_lookup <- data.frame(code = codes, variable = variables)
var_lookup


# create the IRD matrix ---------------------------------------------------
#That naturally lives as a square matrix — rows are "from" (cause), columns are "to" (effect), with 1 for a causal link and 0 for none.
        ### step 1  (Build a zero adjacency matrix, sized from the variables vector)
n <- length(variables)
ird <- matrix(0, nrow = n, ncol = n, dimnames = list(codes, codes))
        ### step 2 (change to 1 every pair [var1,var2] where an arrow strats at var1 and points to var2)

ird["ATTEND","SAFEDEL"]<-1
ird["ATTEND","RESRC"]<-1
ird["ATTEND","HEDUC"]<-1
ird["AWARE","NBCARE"]<-1
ird["AWARE","BPREP"]<-1
ird["AWARE","MYTHS"]<-1
ird["AWARE","ATTEND"]<-1
ird["MHLTH","NBCARE"]<-1
ird["MHLTH","NRISK"]<-1
ird["MHLTH","SAFEDEL"]<-1
ird["NBCARE","NRISK"]<-1
ird["NRISK","NSURV"]<-1
ird["SAFEDEL","NRISK"]<-1
ird["SES","BPREP"]<-1
ird["SES","ATTEND"]<-1
ird["SES","MHLTH"]<-1
ird["SES","NRISK"]<-1
ird["SES","SAFEDEL"]<-1
ird["RESRC","SAFEDEL"]<-1
ird["NSURV","MYTHS"]<-1
ird["BPREP","ATTEND"]<-1
ird["HEDUC","AWARE"]<-1
ird["HEDUC","MHLTH"]<-1
ird["MYTHS","NBCARE"]<-1

# view resulting adjacency mantrix
ird
# check that there are no bidirectional relations
which(ird == 1 & t(ird) == 1, arr.ind = TRUE)

# plot the diagraph -------------------------------------------------------

# Build the directed graph from your adjacency matrix
g <- graph_from_adjacency_matrix(ird, mode = "directed", diag = FALSE)

# Basic plot using the codes as labels
plot(g,
     vertex.label = V(g)$name,
     vertex.size = 25,
     vertex.color = "lightblue",
     vertex.label.cex = 0.8,
     edge.arrow.size = 0.5,
     layout = layout_with_fr(g))  # Fruchterman-Reingold layout, good general-purpose choice

# Use full variable names as labels instead of codes
V(g)$label <- var_lookup$variable[match(V(g)$name, var_lookup$code)]

plot(g,
     vertex.label = V(g)$label,
     vertex.size = 20,
     vertex.color = "lightblue",
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     edge.arrow.size = 0.4,
     layout = layout_with_kk(g))  # Kamada-Kawai often spaces text-heavy labels better

    ### wrap labels
wrap_labels <- function(x, width = 15) sapply(x, function(s) paste(strwrap(s, width = width), collapse = "\n"))
V(g)$label <- wrap_labels(var_lookup$variable[match(V(g)$name, var_lookup$code)])

plot(g,
     vertex.label = V(g)$label,
     vertex.size = 30,
     vertex.color = "lightblue",
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     edge.arrow.size = 0.6,
     layout = layout_with_kk(g))


layout_with_fr(g)     # force-directed, good default
layout_with_kk(g)     # good for larger/more complex graphs
layout_in_circle(g)   # circular — sometimes clearer for spotting feedback loops



