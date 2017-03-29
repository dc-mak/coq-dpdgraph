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
         obj.path + '.' + obj.name AS title,
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
         obj.path + '.' + obj.name AS title,
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

# Output visualisations
visualise <- function(nodes, edges, filename, layout_opts, edge_opts, skipIgraph=FALSE) {
  cat(sprintf("Outputting %s... ", filename)); tic()

  g <- visNetwork(nodes, edges, width="1600px", height="1600px") %>%
       visInteraction(navigationButtons=TRUE, dragNodes=FALSE, zoomView=FALSE)

	g <- do.call(visEdges, append(list(g), edge_opts))

  if (!skipIgraph) {
    g <- do.call(visIgraphLayout, append(list(g), layout_opts))
  } else {
    visLayout(g, randomSeed=layout_opts$randomSeed)
  }

  visSave(g, file = filename)

  time <- toc(quiet=TRUE); time <- time$toc - time$tic
  cat(sprintf("done. (%.2fs)\n", time))
  return(g)
}

# Bucket
bucket <- function(x) { return(cut(log(0.001+x), 10, labels=FALSE)) }

# Non DrL layouts
other_layout <- list(randomSeed=1492)

# Edge options
edge_opts <- list(arrows="middle", color=list(opacity=1), dashes=FALSE)

# Modularity (FR)
nodes$value <- bucket(nodes$betweenness)
nodes$group <- nodes$modularity
other_layout$layout <- "layout_with_fr"
edge_opts <- list(color=list(opacity=1), dashes=FALSE)
visualise(nodes, edges, "direct.html", other_layout, edge_opts)

# Grid graph
nodes$value <- cut(nodes$betweenness, 10, labels=FALSE)
nodes$group <- nodes$modularity
other_layout$layout <- "layout_on_grid"
edge_opts <- list(arrows="middle", color=list(opacity=0.7), dashes=TRUE)
visualise(nodes, edges, "grid.html", other_layout, edge_opts)

# Hierarchical graph
nodes$value <- bucket(nodes$betweenness)
nodes$group <- nodes$modularity
other_layout$layout <- "layout_with_sugiyama"
edge_opts <- list(color=list(opacity=1), dashes=TRUE)
visualise(nodes, edges, "hierarchical.html", other_layout, edge_opts)


# Construct a hierarchical network
cat("Getting nodes (modules) "); tic()
modules <- cypher(graph, "
  MATCH (obj:module)
  RETURN obj.objectId AS id,
         coalesce(obj.path + '.', '') + obj.name AS title,
         \"circle\" AS shape,
         NULL AS pagerank,
         NULL AS betweenness,
         NULL AS edge_betweenness,
         NULL AS closeness,
         NULL AS modularity,
         NULL AS label_prop,
         9 AS value")

# and edges.
cat("and edges... ")
contains <- cypher(graph, "
  MATCH (src)-[edge:CONTAINS]->(dst)
  WHERE (src:module) AND (dst:module OR dst:definition OR dst:proof)
  RETURN src.objectId AS from, dst.objectId AS to, 10000 * edge.weight AS weight")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Modularity (FR, modules)
nodes$value <- bucket(nodes$betweenness)
nodes$group <- nodes$modularity
modules$group <- max(nodes$group)+1
other_layout$randomSeed=440
edge_opts <- list(color=list(opacity=1), dashes=FALSE)
visualise(rbind(nodes, modules),
          contains,
          "module.html",
          other_layout,
          edge_opts,
          skipIgraph=TRUE)
