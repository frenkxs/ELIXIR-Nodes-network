library(tidyverse)
library(blockmodels)
library(igraph)


# load data
cs_2019 <- read_csv('cir-2019.csv')
cs_2019 <- cs_2019[1:29, ]
cs_2019[is.na(cs_2019)] <- 0



# create adjacency matrix
cs_2019 <- cs_2019[, 2:ncol(cs_2019)]
cs_2019 <- as.matrix(cs_2019)
cs_2019 <- crossprod(cs_2019)
diag(cs_2019) <- 0


cs <- graph_from_adjacency_matrix(cs_2019, mode = 'undirected', weighted = TRUE)

# remove Ireland
cs <- decompose.graph(cs)[[1]]


layout <- layout_with_fr(cs, start.temp = 344)
layout <- layout_with_drl(cs)
igraph.options(vertex.size = strength(cs) * 0.05, 
               edge.width = edge.attributes(cs)$weight * 0.2, 
               edge.color = "grey50", 
               edge.curved = 0.1)

plot(cs, layout = layout),      xlim = c(min(lay[, 1]), max(lay[, 1])), 
     ylim = c(min(lay[, 2]), max(lay[, 2])), asp = 0))
tkplot(cs)


library(latentnet)
library(intergraph)

cs_n <- asNetwork(cs)


network.vertex.names(cs_n)

cs_lpm1 <- ergmm(cs_n ~ euclidean(d = 2, G = 1), verbose = TRUE)
cs_lpm2 <- ergmm(cs_n ~ euclidean(d = 2, G = 2), verbose = TRUE)
cs_lpm3 <- ergmm(cs_n ~ euclidean(d = 2, G = 3), verbose = TRUE)

bic1 <- bic.ergmm(cs_lpm1)
bic2 <- bic.ergmm(cs_lpm2)
bic3 <- bic.ergmm(cs_lpm3)

which.max(c(bic1$overall, bic2$overall, bic3$overall))

summary(cs_lpm1)
plot(cs_lpm1, labels = TRUE)

# extract latent positions
lay <- matrix(c(cs_lpm1$mcmc.mle$Z[, 2],  cs_lpm1$mcmc.mle$Z[, 1]), nrow = 21)


igraph.options(vertex.size = strength(cs) * 0.5, 
               edge.width = edge.attributes(cs)$weight * 0.2, 
               edge.color = "grey50", 
               edge.curved = 0.1,
               vertex.label = c('BE', 'CZ', 'DA', 'EMBL-EBI',
                                'EE', 'FI', 'FR', 'DE', 'HE', 
                                'HU', 'IL', 'IT', 'LU', 'NL',
                                'NO', 'PO', 'SL', 'ES', 'SE',
                                'CH', 'UK'),
               vertex.frame.color = '#f47d20',
               vertex.color = '#f47d20',
               vertex.label.color = '#4d4848',
               vertex.label.cex = ifelse(log(strength(cs)) - 4 > 0.7, log(strength(cs)) - 4, 0.7),
               vertex.label.family = 'Helvetica')

plot(cs, layout = lay, rescale = FALSE, 
     xlim = c(min(lay[, 1]), max(lay[, 1])), 
     ylim = c(min(lay[, 2]), max(lay[, 2])))

