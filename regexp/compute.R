suppressMessages(library(RNeo4j))
suppressMessages(library(igraph))
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
d_ig <- graph_from_data_frame(edges, directed=TRUE, nodes)
ig <- graph_from_data_frame(edges, directed=FALSE, nodes)
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Calculate metrics on nodes
node_metric <- function(algorithm, graph) {
  cat(sprintf("%s ... ", algorithm)); tic()

  result <- switch(algorithm,
                   PageRank = page_rank(graph)$vector,
                   Betweenness = betweenness(graph),
                   Closeness = closeness(graph))

  time <- toc(quiet=TRUE); time <- time$toc - time$tic
  cat(sprintf("done. (%.2fs)\n", time))

  return(result)
}

# Compute PageRank (proofs and definitions)
nodes$pagerank <- node_metric("PageRank", d_ig)

# Compute Betweenness (proofs and definitions)
nodes$betweenness <- node_metric("Betweenness", d_ig)

# Compute Closeness (proofs and definitions)
nodes$closeness <- node_metric("Closeness", d_ig)

# Assign a cluster to each node
assign_cluster <- function(algorithm, nodes, graph) {
  cat(sprintf("%s clustering... ", algorithm)); tic()

  label <- switch(algorithm,
                  "Fast Modularity" = "modularity",
                  Betweenness = "betweenness",
                  "Label Propagation" = "label_prop")

  communities <- switch(algorithm,
                        "Fast Modularity" = cluster_fast_greedy(graph),
                        Betweenness = cluster_edge_betweenness(graph),
                        "Label Propagation" = cluster_label_prop(graph))

  memb <- data.frame(id = communities$name)
  memb[[label]] <- communities$membership
  nodes <- merge(nodes, memb)

  time <- toc(quiet=TRUE); time <- time$toc - time$tic
  cat(sprintf("done. (%.2fs)\n", time))

  return(nodes)
}

# Cluster modularity (proofs and defintions) 
# Needs UNDIRECTED graph
nodes <- assign_cluster("Fast Modularity", nodes, ig)

# Only run on small graphs.
nodes <- assign_cluster("Betweenness", nodes, d_ig)

# Cluster label_prop (proofs and defintions)
nodes <- assign_cluster("Label Propagation", nodes, d_ig)

# Put back into database
cat(sprintf("Setting properties:\n\t%s\n\t%s\n\t%s\n\t%s\n\t%s\n\t%s\n",
            "definition_proof_pagerank",
            "definition_proof_betweenness",
            "definition_proof_closeness",
            "definition_proof_modularity",
            "definition_proof_edge_betweenness",
            "definition_proof_label_prop")); tic()
set = "
  MATCH (obj { objectId : toInt({OBJID}) })
  SET obj.definition_proof_pagerank = toFloat({PGR}), 
            obj.definition_proof_betweenness = toFloat({BTW}),
            obj.definition_proof_closeness = toFloat({CLOSE}),
            obj.definition_proof_modularity = toInt({MODGROUP}),
            obj.definition_proof_edge_betweenness = toInt({EBTW}),
            obj.definition_proof_label_prop = toInt({LBL_PRP})
"
transaction <- newTransaction(graph)
progressBar <- txtProgressBar(min=0,
                              max=nrow(nodes),
                              char='=',
                              width=80, 
                              style=3)
for (i in 1:nrow(nodes)) {
  row <- nodes[i,]
  appendCypher(transaction,
               set,
               OBJID=row$id,
               PGR=row$pagerank,
               BTW=row$betweenness,
               CLOSE=row$closeness,
               MODGROUP=row$modularity,
               EBTW=row$edge_betweenness,
               LBL_PRP=row$label_prop)
  setTxtProgressBar(progressBar, i)
}
close(progressBar)
cat("Commiting transaction... ")
commit(transaction)
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))
