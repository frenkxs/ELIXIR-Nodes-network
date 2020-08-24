# load data
cs_old <- read_csv('cir-02.csv')
cs_2019 <- read_csv('cir-2019.csv')

# merge data for 2019 and before 2019
cs_old <- cs_old[, -1]

cs_2019 <- cs_2019[1:29, ]
names(cs_2019)[1] <- "Implementation Study"

names(cs_old) == names(cs_2019)
cs <- rbind(cs_old, cs_2019)
  

cs[is.na(cs)] <- 0

cs_2019[is.na(cs_2019)] <- 0
cs_2019 <- cs_2019[, 2:ncol(cs)]

cs_2019 <- as.matrix(cs_2019)
cs_2019 <- crossprod(cs_2019)
diag(cs_2019) <- 0


cs <- cs[, 2:ncol(cs)]

# create adjacency matrix
cs <- as.matrix(cs)
cs <- crossprod(cs)
diag(cs) <- 0


library(igraph)

cs_net <- graph_from_adjacency_matrix(cs, mode = 'undirected',
                                      weighted = TRUE)
pt_net <- graph_from_adjacency_matrix(cs_2019, mode = 'undirected',
                                      weighted = TRUE)

degree(V(cs_net))

library(latentnet)
library(intergraph)

nodes_n <- asNetwork(cs_net)
nodes2019 <- asNetwork(pt_net)

network.vertex.names(nodes_n)

nodes_lpm <- ergmm(nodes_n ~ euclidean(d = 2, G = 4), verbose = TRUE)
nodes_lpm2 <- ergmm(nodes2019 ~ euclidean(d = 2), verbose = TRUE)



lay <- nodes_lpm2$mcmc.mle$Z 
   

plot(nodes_lpm, labels = TRUE)

edge.attributes(cs_net)$weight

layout <- layout_with_drl(cs_net)

igraph.options(vertex.size = strength(cs_net) * 0.3, 
               edge.width = edge.attributes(cs_net)$weight * 0.2, 
               edge.color = "grey50", 
               edge.curved = 0.1)

plot(cs_net, layout = lay, rescale = FALSE, 
     xlim = c(min(lay[, 1]), max(lay[, 1])), 
     ylim = c(min(lay[, 2]), max(lay[, 2])), asp = 0)

plot(cs_net, layout = layout, rescale = FALSE, 
     ylim = c(min(layout[, 1]), max(layout[, 1])), 
     xlim = c(min(layout[, 2]), max(layout[, 2])), asp = 0)

layout <- layout_with_fr(pt_net, niter = 1000, 
                         weights = edge.attributes(pt_net)$weight)

plot(pt_net, layout = lay, rescale = FALSE, 
     xlim = c(min(lay[, 1]), max(lay[, 1])), 
     ylim = c(min(lay[, 2]), max(lay[, 2])), asp = 0)

plot(pt_net, layout = layout)

, rescale = FALSE, 
     xlim = c(min(lay[, 1]), max(lay[, 1])), 
     ylim = c(min(lay[, 2]), max(lay[, 2])), asp = 0)

pt_net <- cs_net

vertex.attributes()
tkplot(pt_net)
is_2019 <- f
is_old <-y


is_old['Netherlands', ] + is_2019['Netherlands', ]
is_old['Finland', ] + is_2019['Finland', ]
is_old['Sweden', ] + is_2019['Sweden', ]

is_2019['Netherlands', ]
is_2019['Finland', ]
is_2019['Sweden', ]




f <- cir_2019[1:29, ]
f[is.na(f)] <- 0

g <- read_csv('cir-02.csv')
g[is.na(g)] <- 0

sum(f$Finland) + sum(g$Finland)
sum(f$Netherlands) + sum(g$Netherlands)
sum(f$Sweden) + sum(g$Sweden)


data(flo)
nflo<-network(flo)
plot(nflo,displaylabels=TRUE,boxed.labels=FALSE,label.cex=0.75)
