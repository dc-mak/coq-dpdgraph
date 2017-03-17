suppressMessages(library(RNeo4j))
suppressMessages(library(igraph))
suppressMessages(library(visNetwork))
library(tictoc)

# Connect to DB instance
cat("Connecting to database... "); tic()
graph <- startGraph(
  "http://localhost:7474/db/data/",
  username="neo4j",
  password="Neo4j")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Show distribution of PageRank values
dpp <- cypher(graph, "
  MATCH (n)
  WHERE exists(n.definition_proof_pagerank)
  RETURN n.definition_proof_pagerank AS pagerank")
plot(density(dpp$pagerank), log='xy')

# # Grab nodes...
# cat("Getting nodes (definitions and proofs) "); tic()
# nodes <- cypher(graph, "
#   MATCH (obj:definition),
# 				(m:module)-[:CONTAINS]->(obj)
#   RETURN obj.objectId AS id,
#          obj.name AS label,
#          obj.path AS title,
#          \"triangle\" AS shape,
# 				 m.objectId AS parent,
#          obj.definition_proof_pagerank AS value,
#          obj.definition_proof_modularity AS group
#   UNION
#   MATCH (obj:proof),
# 				(m:module)-[:CONTAINS]->(obj)
#   RETURN obj.objectId AS id,
#          obj.name AS label,
#          obj.path AS title,
#          \"square\" AS shape,
# 				 m.objectId AS parent,
#          obj.definition_proof_pagerank AS value,
#          obj.definition_proof_modularity AS group")
# 
# # and edges.
# cat("and edges... ")
# edges <- cypher(graph, "
#   MATCH (src)-[edge]->(dst)
#   WHERE (src:definition OR src:proof) AND (dst:definition OR dst:proof)
#   RETURN src.objectId AS from, dst.objectId AS to, edge.weight AS weight")
# time <- toc(quiet=TRUE); time <- time$toc - time$tic
# cat(sprintf("done. (%.2fs)\n", time))
# 
# # Constructing Hierachical Graph
# cat("Selecting a sample of nodes "); tic()
# # Module/def proof edges: module CONTAINS def/proof
# sample_nodes <- nodes[sample(nrow(nodes), 1000),]
# colnames(sample_nodes)[colnames(sample_nodes) == "definition_proof_pagerank"] <- "value"
# colnames(sample_nodes)[colnames(sample_nodes) == "definition_proof_modularity"] <- "group"
# 
# parent_edges <- sample_nodes[, c("id", "parent")]
# parent_edges$weight <- 1
# colnames(parent_edges)[colnames(parent_edges) == "id"] <- "to"
# colnames(parent_edges)[colnames(parent_edges) == "parent"] <- "from"
# sample_nodes <- subset(sample_nodes, select=-parent)
# 
# # Filter edges
# cat("and edges...\n")
# progressBar <- txtProgressBar(min=0,
#                               max=nrow(edges),
#                               char='=',
#                               width=80, 
#                               style=3)
# sample_edges <- data.frame(from=numeric(0), to=numeric(0), weight=numeric(0))
# for (i in 1:nrow(edges)) {
#   row = edges[i,]
#   if (row$from %in% sample_nodes$id && row$to %in% sample_nodes$id) {
#     sample_edges <- rbind(sample_edges, row)
#   }
#   setTxtProgressBar(progressBar, i)
# }
# close(progressBar)
# time <- toc(quiet=TRUE); time <- time$toc - time$tic
# cat(sprintf("Time taken:%.2fs\n", time))
# 
# # Get module CONTAINS graph
# cat("Getting module distances "); tic()
# module_nodes <- cypher(graph, "
#   MATCH (m:module)
#   RETURN m.objectId AS id,
#          m.name AS label,
#          m.path AS title,
#          \"circle\" AS shape")
# module_nodes$value <- mean(sample_nodes$value)
# module_nodes$group <- max(sample_nodes$group) + 1
# 
# cat("and edges... ")
# module_edges <- cypher(graph, "
#   MATCH (m1:module)-[:CONTAINS]->(m2:module)
#   RETURN m1.objectId AS from,
#          m2.objectId AS to,
#          1 AS weight")
# time <- toc(quiet=TRUE); time <- time$toc - time$tic
# cat(sprintf("done. (%.2fs)\n", time))
# 
# # Ouput graph
# cat("Outputting modules_modularity.html... "); tic()
# visNetwork(sample_nodes,
#            sample_edges,
#            width="100%") %>%
#   visInteraction(navigationButtons=TRUE,
#                  dragNodes=FALSE,
#                  zoomView=FALSE) %>%
#   visPhysics(stabilization=FALSE) %>%
#   visEdges(smooth=FALSE) %>%
#   visIgraphLayout() %>%
#   visSave(file = "mathcomp_pagerank_modularity.html")
# time <- toc(quiet=TRUE); time <- time$toc - time$tic
# cat(sprintf("done. (%.2fs)\n", time))
# 
# # # Take a sample of nodes because 1.7e+04^2 ~ 2.9 million
# get_mat <- function () {
# 	reduced_src <- nodes[sample(nrow(nodes), 700), c("id", "parent")]
# 	colnames(reduced_src)[colnames(reduced_src) == "id"] <- "src"
# 	colnames(reduced_src)[colnames(reduced_src) == "parent"] <- "src_parent"
# 	# and dst...
# 	reduced_dst <- reduced_src
# 	colnames(reduced_dst)[colnames(reduced_dst) == "src"] <- "dst"
# 	colnames(reduced_dst)[colnames(reduced_dst) == "src_parent"] <- "dst_parent"
# 	# Step 1: Merge on src_parent
# 	step1 <- merge(reduced_src, module_edges)
# 	# Step 2: Merge on dst_parent
# 	step2 <- merge(reduced_dst, module_edges)
# 	# Step 3: Merge on both
# 	nodes_dist <- merge(step1, step2)
# 	nodes_dist <- subset(nodes_dist, select=-c(src_parent, dst_parent))
# 	# Now construct matrix
# 	max_index <- max(nodes_dist$src, nodes_dist$dst)
# 	result <- matrix(nrow=max_index, ncol=max_index)
# 	for (i in 1:nrow(nodes_dist)) {
# 		datarow = nodes_dist[i,]
# 		d <- datarow$distance
# 		j <- datarow$src
# 		k <- datarow$dst
# 		result[j,k] <- d
# 		result[k,j] <- d
# 	}
# 	return(nodes_dist)
# }
# cat("Constructing module distance matrix... "); tic()
# module_dist_mat <- get_mat()
# time <- toc(quiet=TRUE); time <- time$toc - time$tic
# cat(sprintf("done. (%.2fs)\n", time))
#
# visNetwork(nodes, edges, width="100%") %>%
#   visSave(file = "pagerank_fast_greedy.html")
#
# print(head(nodes))
# nodes <- subset(nodes, select=-value)
# print(head(nodes))
# nodes$value <- betweenness(ig)
# print(head(nodes))
# communities <- cluster_edge_betweenness(ig)
# memb <- data.frame(id = communities$name, group = communities$membership)
# nodes <- subset(nodes, select=-group)
# nodes <- merge(nodes, memb)
# 
# visNetwork(nodes, edges, width="100%") %>%
#   visSave(file = "betweenness.html")
#
# # Compute PageRank over given labels
# # Check if has been computed before (sort then check labels)
# # If so, load result, if not, compute, set and return
# db_pagerank <- function(col, ...) {
#   sorted <- c(col, list(...))
#    
#   # Check
#   attrib <- paste(sorted, "pagerank", sep="_")
# 
# }
