suppressMessages(library(RNeo4j))
suppressMessages(library(igraph))
library(shiny)
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
         obj.path AS module,
         \"triangle\" AS shape
  UNION
  MATCH (obj:proof),
				(m:module)-[:CONTAINS]->(obj)
  RETURN obj.objectId AS id,
         obj.path + '.' + obj.name AS title,
         obj.path AS module,
         \"square\" AS shape")

# and edges.
cat("and edges... ")
edges <- cypher(graph, "
  MATCH (src)-[edge]->(dst)
  WHERE (src:definition OR src:proof) AND (dst:definition OR dst:proof)
  RETURN src.objectId AS from, dst.objectId AS to, edge.weight AS weight")
flipped_edges <- data.frame(from=edges$to, to=edges$from, weight=edges$weight)
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

# Bucketing cluster assignments
bucket <- function(x) { return(cut(x, 10, labels=FALSE)) }

# Cluster modularity (proofs and defintions) 
# Needs UNDIRECTED graph
nodes <- assign_cluster("Fast Modularity", nodes, ig)

# # Betweenness: small graphs only
# nodes <- assign_cluster("Betweenness", nodes, d_ig)

# # Cluster label_prop (proofs and defintions)
# nodes <- assign_cluster("Label Propagation", nodes, d_ig)

# Output visualisations
visualise <- function(
    nodes, edges, filename, layout_opts,
    edge_opts=list(color=list(color="lightgray", opacity=0.1), dashes=TRUE),
    skipIgraph=FALSE,
    orange_modules=FALSE,
    select="module"
  ) {
  cat(sprintf("Outputting %s... ", filename)); tic()

  # Maybe look at scaling{min,max} later?
  g <- visNetwork(nodes, edges, width="1000px", height="800px") %>%
       visInteraction(navigationButtons=TRUE,
                      multiselect=TRUE,
                      dragNodes=FALSE,
                      zoomView=FALSE) %>%
       visOptions(selectedBy=list(variable=select, multiple=TRUE),
                  highlightNearest=TRUE,
                  nodesIdSelection=TRUE)

  if (orange_modules) {
    g <- visGroups(g, groupname=paste(max(nodes$group)), color="orange")
  }

  g <- do.call(visEdges, append(list(g), edge_opts))

  if (!skipIgraph) {
    g <- do.call(visIgraphLayout, append(list(g), layout_opts))
  } else {
    g <- do.call(visLayout, append(list(g), layout_opts))
  }

  g <- visExport(g, type="pdf", name = sprintf("%s.pdf", filename))
  visSave(g, file = filename)

  time <- toc(quiet=TRUE); time <- time$toc - time$tic
  cat(sprintf("done. (%.2fs)\n", time))

  return(g)
}

# DrL options list - painstakingly chosen
drl_opts <- list(edge.cut=1,
  # init
  init.iterations=200,
  init.temperature=200 * 6,
  init.attraction=0,
  # liquid
  liquid.iterations=200,
  # liquid.temperature=200 * i,
  liquid.attraction=0,
  # expansion
  expansion.iterations=200,
  expansion.temperature=200 * 3,
  expansion.attraction=0,
  # cooldown
  cooldown.iterations=200,
  cooldown.temperature=200 * 7,
  cooldown.attraction=1,
  # crunch
  crunch.iterations=50,
  crunch.temperature=200 * 7,
  crunch.attraction=1,
  # simmer
  simmer.iterations=100,
  simmer.attraction=0)

# Layout options list
drl_layout <- list(randomSeed=1492, options=drl_opts, layout="layout_with_drl")

# Direct DrL
nodes$value <- bucket(log(0.001 + nodes$betweenness))
nodes$group <- nodes$modularity
visualise(nodes, edges, "direct_mod.html", drl_layout,
          edge_opts=list(color=list(opacity=0.6), dashes=TRUE))
visualise(nodes, flipped_edges, "direct_flipped_mod.html", drl_layout,
          edge_opts=list(color=list(opacity=0.6), dashes=TRUE))

# Grid
nodes$value <- bucket(log(0.001 + nodes$betweenness))
nodes$group <- nodes$modularity
visualise(nodes, edges, "grid_mod.html",
          list(randomSeed=1492, layout="layout_on_grid"),
          edge_opts=list(arrows="middle", color=list(opacity=0.6), dashes=TRUE))

# Circular
nodes$value <- bucket(log(0.001 + nodes$betweenness))
nodes$group <- nodes$modularity
visualise(nodes, edges, "circular_mod.html",
          list(randomSeed=1492, layout="layout_in_circle"),
          edge_opts=list(arrows="middle", color=list(opacity=0.6), dashes=TRUE))
visualise(nodes, flipped_edges, "circular_flipped_mod.html",
          list(randomSeed=1492, layout="layout_in_circle"),
          edge_opts=list(arrows="middle", color=list(opacity=0.6), dashes=TRUE))

# Sugiyama
nodes$value <- bucket(log(0.001 + nodes$betweenness))
nodes$group <- nodes$modularity
visualise(nodes, edges, "hierarchical_mod.html",
          list(randomSeed=1492, layout="layout_with_sugiyama"),
          edge_opts=list(color=list(opacity=0.2), dashes=TRUE))
visualise(nodes, flipped_edges, "hierarchical_flipped_mod.html",
          list(randomSeed=1492, layout="layout_with_sugiyama"),
          edge_opts=list(color=list(opacity=0.2), dashes=TRUE))

# Construct a hierarchical network
cat("Getting nodes (modules) "); tic()
modules <- cypher(graph, "
  MATCH (obj:module)
  RETURN obj.objectId AS id,
         obj.path + '.' + obj.name AS title,
         obj.path AS module,
         \"circle\" AS shape,
         NULL AS pagerank,
         NULL AS betweenness,
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

# Modules: slow af but looks great
nodes$value <- bucket(log(0.001 + nodes$betweenness))
modules$value <- 10
nodes$group <- nodes$modularity
modules$group <- max(nodes$group) + 1
g <- visualise(rbind(nodes, modules), contains, "modules_mod.html",
               list(randomSeed=1492, improvedLayout=TRUE),
               edge_opts=list(color=list(color="gray", opacity=0.7), dashes=TRUE),
               skipIgraph=TRUE,
               orange_modules=TRUE)
