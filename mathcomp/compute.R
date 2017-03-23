suppressMessages(library(RNeo4j))
suppressMessages(library(igraph))
suppressMessages(library(visNetwork))
library(tictoc)

# Connect to DB instance
cat("Connecting to database... "); tic()
graph <- startGraph("http://localhost:7474/db/data/",
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
         obj.path + '.' + obj.name AS title,
         \"triangle\" AS shape
  UNION
  MATCH (obj:proof),
				(m:module)-[:CONTAINS]->(obj)
  RETURN obj.objectId AS id,
         obj.path + '.' + obj.name AS title,
         \"square\" AS shape")

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
nodes$pagerank <- page_rank(ig)$vector
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Compute Betweenness (proofs and definitions)
cat("Betweenness (over definitions and proofs)... "); tic()
nodes$betweenness <- betweenness(ig)
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Compute Closeness (proofs and definitions)
cat("Closeness (over definitions and proofs)... "); tic()
nodes$closeness <- closeness(ig)
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Cluster modularity (proofs and defintions)
cat("Fast modularity clustering (over all definitions and proofs)... "); tic()
communities <- cluster_fast_greedy(ig)
memb <- data.frame(id = communities$name,
                  modularity = communities$membership)
nodes <- merge(nodes, memb)
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Only run on small graphs..?
# Cluster betweenness (proofs and defintions)

# Cluster (proofs and defintions)
cat("Label propogation clustering (over all definitions and proofs)... "); tic()
communities <- cluster_label_prop(ig)
memb <- data.frame(id = communities$name,
                  label_prop = communities$membership)
nodes <- merge(nodes, memb)
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Plain graph
cat("Outputting plain visualisation... "); tic()
visNetwork(nodes, edges, width="100%") %>%
  visInteraction(navigationButtons=TRUE,
                 dragNodes=FALSE,
                 zoomView=FALSE) %>%
  visIgraphLayout(randomSeed=1492) %>%
  visSave(file = "plain.html")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Betweenness
nodes$value <- nodes$betweenness
cat("Outputting betweenness visualisation... "); tic()
visNetwork(nodes, edges, width="100%") %>%
  visInteraction(navigationButtons=TRUE,
                 dragNodes=FALSE,
                 zoomView=FALSE) %>%
  visIgraphLayout(randomSeed=1492) %>%
  visSave(file = "betweenness.html")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Closeness
nodes$value <- nodes$closeness
cat("Outputting closeness visualisation... "); tic()
visNetwork(nodes, edges, width="100%") %>%
  visInteraction(navigationButtons=TRUE,
                 dragNodes=FALSE,
                 zoomView=FALSE) %>%
  visIgraphLayout(randomSeed=1492) %>%
  visSave(file = "closeness.html")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# PageRank 
nodes$value <- nodes$pagerank
cat("Outputting PageRank visualisation... "); tic()
visNetwork(nodes, edges, width="100%") %>%
  visInteraction(navigationButtons=TRUE,
                 dragNodes=FALSE,
                 zoomView=FALSE) %>%
  visIgraphLayout(randomSeed=1492) %>%
  visSave(file = "pagerank.html")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Label propogation 
nodes$value <- 1
nodes$group <- nodes$label_prop
cat("Outputting label propogation visualisation... "); tic()
visNetwork(nodes, edges, width="100%") %>%
  visInteraction(navigationButtons=TRUE,
                 dragNodes=FALSE,
                 zoomView=FALSE) %>%
  visIgraphLayout(randomSeed=1492) %>%
  visSave(file = "label_prop.html")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# # Edge betweenness - skipped

# Modularity
nodes$group <- nodes$modularity
cat("Outputting modularity visualisation... "); tic()
visNetwork(nodes, edges, width="100%") %>%
  visInteraction(navigationButtons=TRUE,
                 dragNodes=FALSE,
                 zoomView=FALSE) %>%
  visIgraphLayout(randomSeed=1492) %>%
  visSave(file = "modularity.html")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Hierarchical graph
nodes$value <- nodes$betweenness
cat("Outputting hierarchical visualisation... "); tic()
visNetwork(nodes, edges, width="100%") %>%
  visInteraction(navigationButtons=TRUE,
                 dragNodes=FALSE,
                 zoomView=FALSE) %>%
  visEdges(arrows="from") %>%
  visIgraphLayout(randomSeed=1492, layout="layout_with_sugiyama") %>%
  visSave(file = "hierarchical.html")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Circle graph
nodes$group <- nodes$modularity
nodes$value <- nodes$pagerank
cat("Outputting circular PageRank/modularity visualisation... "); tic()
visNetwork(nodes, edges, width="100%") %>%
  visInteraction(navigationButtons=TRUE,
                 dragNodes=FALSE,
                 zoomView=FALSE) %>%
  visEdges(arrows="from") %>%
  visIgraphLayout(randomSeed=1492, layout="layout_in_circle") %>%
  visSave(file = "circular.html")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Grid graph
nodes$group <- nodes$modularity
nodes$value <- nodes$pagerank
cat("Outputting grid PageRank/modularity visualisation... "); tic()
visNetwork(nodes, edges, width="100%") %>%
  visInteraction(navigationButtons=TRUE,
                 dragNodes=FALSE,
                 zoomView=FALSE) %>%
  visEdges(arrows="from") %>%
  visIgraphLayout(randomSeed=1492, layout="layout_on_grid") %>%
  visSave(file = "grid.html")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Force-directed
nodes$value <- nodes$betweenness
cat("Outputting label propogation visualisation... "); tic()
visNetwork(nodes, edges, width="100%") %>%
  visInteraction(navigationButtons=TRUE,
                 dragNodes=FALSE,
                 zoomView=FALSE) %>%
  visIgraphLayout(randomSeed=1492, layout="layout_with_fr") %>%
  visSave(file = "force_directed_fr.html")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))
