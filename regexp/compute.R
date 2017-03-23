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
ig <- graph_from_data_frame(edges, directed=FALSE, nodes)
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Compute PageRank (proofs and definitions)
cat("PageRank (over definitions and proofs)... "); tic()
nodes$definition_proof_pagerank <- page_rank(ig)$vector
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Compute Betweenness (proofs and definitions)
cat("Betweenness (over definitions and proofs)... "); tic()
nodes$definition_proof_betweenness <- betweenness(ig)
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Compute Closeness (proofs and definitions)
cat("Closeness (over definitions and proofs)... "); tic()
nodes$definition_proof_closeness <- closeness(ig)
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Cluster modularity (proofs and defintions)
cat("Fast modularity clustering (over all definitions and proofs)... "); tic()
communities <- cluster_fast_greedy(ig)
memb <- data.frame(id = communities$name,
                  definition_proof_modularity = communities$membership)
nodes <- merge(nodes, memb)
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Cluster betweenness (proofs and defintions)
cat("Betweenness clustering (over all definitions and proofs)... "); tic()
communities <- cluster_edge_betweenness(ig)
memb <- data.frame(id = communities$name,
                  definition_proof_edge_betweenness = communities$membership)
nodes <- merge(nodes, memb)
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Cluster (proofs and defintions)
cat("Label propogration clustering (over all definitions and proofs)... "); tic()
communities <- cluster_label_prop(ig)
memb <- data.frame(id = communities$name,
                  definition_proof_label_prop = communities$membership)
nodes <- merge(nodes, memb)
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

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
               PGR=row$definition_proof_pagerank,
               BTW=row$definition_proof_betweenness,
               CLOSE=row$definition_proof_closeness,
               MODGROUP=row$definition_proof_modularity,
               EBTW=row$definition_proof_edge_betweenness,
               LBL_PRP=row$definition_proof_label_prop)
  setTxtProgressBar(progressBar, i)
}
close(progressBar)
cat("Commiting transaction... ")
commit(transaction)
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))
