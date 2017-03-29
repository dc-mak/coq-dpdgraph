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

# PageRank
nodes$pagerank <- node_metric("PageRank", d_ig)

# # Betweenness (small graphs only)
# nodes$betweenness <- node_metric("Betweenness", d_ig)

# Closeness
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

# Cluster modularity
# Needs UNDIRECTED graph
nodes <- assign_cluster("Fast Modularity", nodes, ig)

# # Cluster betweenness (small graphs only)
# nodes <- assign_cluster("Betweenness", nodes, d_ig)

# # Cluster label propagation (small graphs only)
# nodes <- assign_cluster("Label Propagation", nodes, d_ig)

# Output visualisations
visualise <- function(nodes, edges, filename, layout_opts) {
  cat(sprintf("Outputting %s... ", filename)); tic()

  g <- visNetwork(nodes, edges, width="100%", height="700px") %>%
       visInteraction(navigationButtons=TRUE,
                      dragNodes=FALSE,
                      zoomView=FALSE) %>%
       visEdges(color=list(opacity=0.1), dashes=TRUE)

  g <- do.call(visIgraphLayout, append(list(g), layout_opts))

  visSave(g, file = filename)

  time <- toc(quiet=TRUE); time <- time$toc - time$tic
  cat(sprintf("done. (%.2fs)\n", time))
}

# DrL options list
drl_opts <- list(edge.cut=1,
                 init.iterations=200,
                 init.temperature=2000,
                 init.attraction=0,
                 liquid.temperature=2000,
                 liquid.attraction=0,
                 expansion.temperature=2000,
                 expansion.attraction=0,
                 simmer.attraction=0);

# Layout options list
drl_layout <- list(randomSeed=1492,
                   options=drl_opts,
                   layout="layout_with_drl")

# Plain graph
visualise(nodes, edges, "plain.html", drl_layout)

# # Betweenness
# nodes$value <- nodes$betweenness
# visualise(nodes, edges, "betweenness.html", drl_layout)

# Closeness
nodes$value <- nodes$closeness
visualise(nodes, edges, "closeness.html", drl_layout)

# PageRank
nodes$value <- nodes$pagerank
visualise(nodes, edges, "pagerank.html", drl_layout)

# Non DrL layouts
other_layout <- list(randomSeed=1492)
 
# # Hierarchical graph
# # nodes$value <- nodes$betweenness
# nodes$value <- nodes$pagerank
# nodes$group <- nodes$modularity
# other_layout$layout <- "layout_with_sugiyama"
# visualise(nodes, edges, "hierarchical.html", other_layout)

# Circle graph
nodes$value <- nodes$pagerank
nodes$group <- nodes$modularity
other_layout$layout = "layout_in_circle"
visualise(nodes, edges, "circular.html", other_layout)

# Grid graph
nodes$value <- nodes$pagerank
nodes$group <- nodes$modularity
other_layout$layout <- "layout_on_grid"
visualise(nodes, edges, "grid.html", other_layout)

# Modularity (DrL)
# nodes$value <- nodes$betweenness
nodes$group <- nodes$modularity
visualise(nodes, edges, "modularity_direct.html", drl_layout)

# # Label propagation
# nodes$value <- 1
# nodes$group <- nodes$label_prop
# visualise(nodes, edges, "label_prop.html", drl_layout)

# Construct a hierarchical network
cat("Getting nodes (modules) "); tic()
modules <- cypher(graph, "
  MATCH (obj:module)
  RETURN obj.objectId AS id,
         obj.path + '.' + obj.name AS title,
         \"circle\" AS shape,
         NULL AS pagerank,
         NULL AS closeness,
         NULL AS modularity,
         1 AS value")

# and edges.
cat("and edges... ")
contains <- cypher(graph, "
  MATCH (src)-[edge:CONTAINS]->(dst)
  WHERE (src:module) AND (dst:module OR dst:definition OR dst:proof)
  RETURN src.objectId AS from, dst.objectId AS to, edge.weight AS weight")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Edge betweenness - skipped

# Modularity (FR, modules)
nodes$value <- nodes$betweenness
nodes$value <- nodes$pagerank
nodes$group <- nodes$modularity
modules$group <- max(nodes$group)+1
other_layout$layout <- "layout_with_fr"
visualise(rbind(nodes, modules), contains, "modularity_fr.html", other_layout)

# Modularity (DrL, modules)
drl_opts <- list(edge.cut=1,
                 init.iterations=50,
                 init.temperature=1900,
                 init.attraction=0,
                 liquid.temperature=1900,
                 liquid.attraction=0,
                 expansion.temperature=1900,
                 expansion.attraction=0,
                 simmer.attraction=0);
drl_layout <- list(randomSeed=1492, options=drl_opts, layout="layout_with_drl")

nodes$value <- nodes$betweenness
nodes$group <- nodes$modularity
modules$group <- max(nodes$group)+1
visualise(rbind(nodes, modules), contains, "modularity_drl.html", drl_layout)
