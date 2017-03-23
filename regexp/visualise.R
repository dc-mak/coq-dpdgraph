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
         obj.definition_proof_pagerank AS pagerank,
         obj.definition_proof_betweenness AS betweenness,
         obj.definition_proof_closeness AS closeness,
         obj.definition_proof_edge_betweenness AS edge_betweenness,
         obj.definition_proof_label_prop AS label_prop,
         obj.definition_proof_modularity AS modularity
  UNION
  MATCH (obj:proof),
				(m:module)-[:CONTAINS]->(obj)
  RETURN obj.objectId AS id,
         obj.name AS label,
         obj.path AS title,
         \"square\" AS shape,
         obj.definition_proof_pagerank AS pagerank,
         obj.definition_proof_betweenness AS betweenness,
         obj.definition_proof_closeness AS closeness,
         obj.definition_proof_edge_betweenness AS edge_betweenness,
         obj.definition_proof_label_prop AS label_prop,
         obj.definition_proof_modularity AS modularity")

# and edges.
cat("and edges... "); tic()
edges <- cypher(graph, "
  MATCH (src)-[edge]->(dst)
  WHERE (src:definition OR src:proof) AND (dst:definition OR dst:proof)
  RETURN src.objectId AS from, dst.objectId AS to, edge.weight AS weight")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Plain graph
cat("Outputting plain visualisation... "); tic()
visNetwork(nodes, edges, width="100%") %>%
  visInteraction(navigationButtons=TRUE,
                 dragNodes=FALSE,
                 zoomView=FALSE) %>%
  visEdges(arrows="from") %>%
  visIgraphLayout(randomSeed=11) %>%
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
  visEdges(arrows="from") %>%
  visIgraphLayout(randomSeed=11) %>%
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
  visEdges(arrows="from") %>%
  visIgraphLayout(randomSeed=11) %>%
  visSave(file = "closeness.html")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# PageRank 
nodes$value <- nodes$pagerank
cat("Outputting PrgeRank visualisation... "); tic()
visNetwork(nodes, edges, width="100%") %>%
  visInteraction(navigationButtons=TRUE,
                 dragNodes=FALSE,
                 zoomView=FALSE) %>%
  visEdges(arrows="from") %>%
  visIgraphLayout(randomSeed=11) %>%
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
  visEdges(arrows="from") %>%
  visIgraphLayout(randomSeed=11) %>%
  visSave(file = "label_prop.html")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Edge betweenness
nodes$group <- nodes$edge_betweenness
cat("Outputting edge betweenness visualisation... "); tic()
visNetwork(nodes, edges, width="100%") %>%
  visInteraction(navigationButtons=TRUE,
                 dragNodes=FALSE,
                 zoomView=FALSE) %>%
  visEdges(arrows="from") %>%
  visIgraphLayout(randomSeed=11) %>%
  visSave(file = "edge_betweenness.html")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Modularity
nodes$group <- nodes$modularity
cat("Outputting modularity visualisation... "); tic()
visNetwork(nodes, edges, width="100%") %>%
  visInteraction(navigationButtons=TRUE,
                 dragNodes=FALSE,
                 zoomView=FALSE) %>%
  visEdges(arrows="from") %>%
  visIgraphLayout(randomSeed=11) %>%
  visSave(file = "modularity.html")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Hierarchical graph
nodes$group <- nodes$edge_betweenness
nodes$value <- nodes$betweenness
cat("Outputting hierarchical visualisation... "); tic()
visNetwork(nodes, edges, width="100%") %>%
  visInteraction(navigationButtons=TRUE,
                 dragNodes=FALSE,
                 zoomView=FALSE) %>%
  visEdges(arrows="from") %>%
  visIgraphLayout(randomSeed=11, layout="layout_with_sugiyama") %>%
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
  visIgraphLayout(randomSeed=11, layout="layout_in_circle") %>%
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
  visIgraphLayout(randomSeed=11, layout="layout_on_grid") %>%
  visSave(file = "grid.html")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Force-directed layout graph
nodes$group <- nodes$modularity
nodes$value <- nodes$pagerank
cat("Outputting force-directed PageRank/modularity visualisation... "); tic()
visNetwork(nodes, edges, width="100%") %>%
  visInteraction(navigationButtons=TRUE,
                 dragNodes=FALSE,
                 zoomView=FALSE) %>%
  visEdges(arrows="from") %>%
  visIgraphLayout(randomSeed=11, layout="layout_with_fr") %>%
  visSave(file = "force_directed.html")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))
