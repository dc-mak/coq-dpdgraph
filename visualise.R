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

# Grab nodes...
cat("Getting nodes (definitions and proofs) "); tic()
nodes <- cypher(graph, "
  MATCH (obj:definition),
				(m:module)-[:CONTAINS]->(obj)
  RETURN obj.objectId AS id,
         obj.name AS label,
         obj.path AS title,
         \"triangle\" AS shape,
				 m.objectId AS parent
  UNION
  MATCH (obj:proof),
				(m:module)-[:CONTAINS]->(obj)
  RETURN obj.objectId AS id,
         obj.name AS label,
         obj.path AS title,
         \"square\" AS shape,
				 m.objectId AS parent")

# and edges.
cat("and edges... ")
edges <- cypher(graph, "
  MATCH (src)-[edge]->(dst)
  WHERE (src:definition OR src:proof) AND (dst:definition OR dst:proof)
  RETURN src.objectId AS from, dst.objectId AS to, edge.weight AS weight")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Constructing graph
cat("Constructing igraph... "); tic()
ig <- graph_from_data_frame(edges, directed=FALSE, nodes)
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# # Compute PageRank (proofs and definitions)
# cat("Computing PageRank (between definitions and proofs)... "); tic()
# nodes$definition_proof_pagerank <- page_rank(ig)$vector
# time <- toc(quiet=TRUE); time <- time$toc - time$tic
# cat(sprintf("done. (%.2fs)\n", time))
# 
# # Cluster (proofs and defintions)
# cat("Computing fast modularity clustering (over all definitions and proofs)... "); tic()
# communities <- cluster_fast_greedy(ig)
# memb <- data.frame(id = communities$name,
#                   definition_proof_modularity = communities$membership)
# nodes <- merge(nodes, memb)
# time <- toc(quiet=TRUE); time <- time$toc - time$tic
# cat(sprintf("done. (%.2fs)\n", time))
# 
# # Put back into database
# cat(sprintf("Setting properties:\n\t%s\n\t%s\n",
#             "definition_proof_pagerank",
#             "definition_proof_modularity.")); tic()
# set = "
#   MATCH (obj { objectId : toInt({OBJID}) })
#   SET obj.definition_proof_modularity = toInt({MODGROUP}),
#       obj.definition_proof_pagerank = toFloat({DPP}) 
# "
# transaction <- newTransaction(graph)
# progressBar <- txtProgressBar(min=0,
#                               max=nrow(nodes),
#                               char='=',
#                               width=80, 
#                               style=3)
# for (i in 1:nrow(nodes)) {
#   row <- nodes[i,]
#   appendCypher(transaction,
#                set,
#                OBJID=row$id,
#                DPP=row$definition_proof_pagerank,
#                MODGROUP=row$definition_proof_modularity)
#   setTxtProgressBar(progressBar, i)
# }
# close(progressBar)
# cat("Commiting transaction... ")
# commit(transaction)
# time <- toc(quiet=TRUE); time <- time$toc - time$tic
# cat(sprintf("done. (%.2fs)\n", time))

# Dendrogram distance
cat("Getting module distances... "); tic()
module_edges <- cypher(graph, "
  MATCH p = shortestpath((m1:module)-[:CONTAINS*..6]-(m2:module))
  WHERE m1.objectId < m2.objectId
        AND ((m1)-[:CONTAINS]->(:definition) OR (m1)-[:CONTAINS]->(:proof))
        AND ((m2)-[:CONTAINS]->(:definition) OR (m2)-[:CONTAINS]->(:proof))
  RETURN m1.objectId AS src_parent,
         m2.objectId AS dst_parent,
         length(p)/2 AS distance")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Take a sample of nodes because 1.7e+04^2 ~ 2.9 million
get_mat <- function () {
	reduced_src <- nodes[sample(nrow(nodes), 700), c("id", "parent")]
	colnames(reduced_src)[colnames(reduced_src) == "id"] <- "src"
	colnames(reduced_src)[colnames(reduced_src) == "parent"] <- "src_parent"
	# and dst...
	reduced_dst <- reduced_src
	colnames(reduced_dst)[colnames(reduced_dst) == "src"] <- "dst"
	colnames(reduced_dst)[colnames(reduced_dst) == "src_parent"] <- "dst_parent"
	# Step 1: Merge on src_parent
	step1 <- merge(reduced_src, module_edges)
	# Step 2: Merge on dst_parent
	step2 <- merge(reduced_dst, module_edges)
	# Step 3: Merge on both
	nodes_dist <- merge(step1, step2)
	nodes_dist <- subset(nodes_dist, select=-c(src_parent, dst_parent))
	# Now construct matrix
	max_index <- max(nodes_dist$src, nodes_dist$dst)
	result <- matrix(nrow=max_index, ncol=max_index)
	for (i in 1:nrow(nodes_dist)) {
		datarow = nodes_dist[i,]
		d <- datarow$distance
		j <- datarow$src
		k <- datarow$dst
		result[j,k] <- d
		result[k,j] <- d
	}
	return(result)
}
cat("Constructing module distance matrix... "); tic()
module_dist_mat <- get_mat()
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))



# # Show distribution of PageRank values
# dpp <- cypher(graph, "
#   MATCH (n)
#   WHERE exists(n.definition_proof_pagerank)
#   RETURN n.definition_proof_pagerank AS pagerank")
# plot(density(dpp$pagerank), log='x')

# Colour dendrogram of project (according to modules) with colours from clustering

# visNetwork(nodes, edges, width="100%") %>%
#   visSave(file = "pagerank_fast_greedy.html")

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
