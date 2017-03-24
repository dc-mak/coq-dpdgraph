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

# Compute PageRank (proofs and definitions)
cat("PageRank (over definitions and proofs)... "); tic()
nodes$pagerank <- page_rank(d_ig)$vector
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Compute Betweenness (proofs and definitions)
cat("Betweenness (over definitions and proofs)... "); tic()
nodes$betweenness <- betweenness(d_ig)
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Compute Closeness (proofs and definitions)
cat("Closeness (over definitions and proofs)... "); tic()
nodes$closeness <- closeness(d_ig)
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
communities <- cluster_label_prop(d_ig)
memb <- data.frame(id = communities$name,
                  label_prop = communities$membership)
nodes <- merge(nodes, memb)
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

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

# Plain graph
cat("Outputting plain visualisation... "); tic()
visNetwork(nodes, edges, width="100%") %>%
  visInteraction(navigationButtons=TRUE,
                 dragNodes=FALSE,
                 zoomView=FALSE) %>%
  visEdges(color=list(color="lightgray", opacity=0.1), dashes=TRUE) %>%
  visIgraphLayout(randomSeed=1492, options=drl_opts, layout="layout_with_drl") %>%
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
  visEdges(color=list(color="lightgray", opacity=0.1), dashes=TRUE) %>%
  visIgraphLayout(randomSeed=1492, options=drl_opts, layout="layout_with_drl") %>%
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
  visEdges(color=list(color="lightgray", opacity=0.1), dashes=TRUE) %>%
  visIgraphLayout(randomSeed=1492, options=drl_opts, layout="layout_with_drl") %>%
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
  visEdges(color=list(color="lightgray", opacity=0.1), dashes=TRUE) %>%
  visIgraphLayout(randomSeed=1492, options=drl_opts, layout="layout_with_drl") %>%
  visSave(file = "pagerank.html")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Hierarchical graph
nodes$value <- nodes$betweenness
nodes$group <- nodes$modularity
cat("Outputting hierarchical visualisation... "); tic()
visNetwork(nodes, edges, width="100%") %>%
  visInteraction(navigationButtons=TRUE,
                 dragNodes=FALSE,
                 zoomView=FALSE) %>%
  visEdges(color=list(color="lightgray", opacity=0.1), dashes=TRUE) %>%
  visIgraphLayout(randomSeed=1492, layout="layout_with_sugiyama") %>%
  visSave(file = "hierarchical.html")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Circle graph
nodes$value <- nodes$pagerank
nodes$group <- nodes$modularity
cat("Outputting circular PageRank/modularity visualisation... "); tic()
visNetwork(nodes, edges, width="100%") %>%
  visInteraction(navigationButtons=TRUE,
                 dragNodes=FALSE,
                 zoomView=FALSE) %>%
  visEdges(color=list(opacity=0.25), dashes=TRUE) %>%
  visIgraphLayout(randomSeed=111, layout="layout_in_circle") %>%
  visSave(file = "circular.html")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Grid graph
nodes$value <- nodes$pagerank
nodes$group <- nodes$modularity
cat("Outputting grid PageRank/modularity visualisation... "); tic()
visNetwork(nodes, edges, width="100%") %>%
  visInteraction(navigationButtons=TRUE,
                 dragNodes=FALSE,
                 zoomView=FALSE) %>%
  visEdges(color=list(color="lightgray", opacity=0.1), dashes=TRUE) %>%
  visIgraphLayout(randomSeed=1492, layout="layout_on_grid") %>%
  visSave(file = "grid.html")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Modularity (DrL)
nodes$value <- nodes$betweenness
nodes$group <- nodes$modularity
modules$group <- max(nodes$group)+1
cat("Outputting modularity (DrL, direct) visualisation... "); tic()
visNetwork(nodes, edges, width="100%") %>%
  visInteraction(navigationButtons=TRUE,
                 dragNodes=FALSE,
                 zoomView=FALSE) %>%
  visEdges(color=list(color="lightgray", opacity=0.1), dashes=TRUE) %>%
  visIgraphLayout(randomSeed=1492, options=drl_opts, layout="layout_with_drl") %>%
  visSave(file = "modularity_direct.html")
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
  visEdges(color=list(color="lightgray", opacity=0.1), dashes=TRUE) %>%
  # visIgraphLayout(randomSeed=1492, niter=6000, layout="layout_with_fr") %>%
  visIgraphLayout(randomSeed=1492, options=drl_opts, layout="layout_with_drl") %>%
  visSave(file = "label_prop.html")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# Construct a hierarchical network
modules <- cypher(graph, "
  MATCH (obj:module)
  RETURN obj.objectId AS id,
         obj.path + '.' + obj.name AS title,
         \"circle\" AS shape,
         NULL AS pagerank,
         NULL AS betweenness,
         NULL AS closeness,
         NULL AS modularity,
         NULL AS label_prop,
         1 AS value")

# and edges.
cat("and edges... ")
contains <- cypher(graph, "
  MATCH (src)-[edge:CONTAINS]->(dst)
  WHERE (src:module) AND (dst:module OR dst:definition OR dst:proof)
  RETURN src.objectId AS from, dst.objectId AS to, edge.weight AS weight")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

# # Edge betweenness - skipped

# Modularity (FR, modules)
nodes$value <- nodes$betweenness
nodes$group <- nodes$modularity
modules$group <- max(nodes$group)+1
cat("Outputting modularity (FR, no modules) visualisation... "); tic()
visNetwork(rbind(modules, nodes), contains, width="100%") %>%
  visInteraction(navigationButtons=TRUE,
                 dragNodes=FALSE,
                 zoomView=FALSE) %>%
  visEdges(color=list(color="lightgray", opacity=0.1), dashes=TRUE) %>%
  visIgraphLayout(randomSeed=1492, niter=2000, layout="layout_with_fr") %>%
  visSave(file = "modularity_fr.html")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))

drl_opts <- list(edge.cut=1,
                 init.iterations=50,
                 init.temperature=1900,
                 init.attraction=0,
                 liquid.temperature=1900,
                 liquid.attraction=0,
                 expansion.temperature=1900,
                 expansion.attraction=0,
                 simmer.attraction=0);

# Modularity (DrL, modules)
nodes$value <- nodes$betweenness
nodes$group <- nodes$modularity
modules$group <- max(nodes$group)+1
cat("Outputting modularity (DrL, modules) visualisation... "); tic()
visNetwork(rbind(modules, nodes), contains, width="100%") %>%
  visInteraction(navigationButtons=TRUE,
                 dragNodes=FALSE,
                 zoomView=FALSE) %>%
  visEdges(color=list(color="lightgray", opacity=0.1), dashes=TRUE) %>%
  visIgraphLayout(randomSeed=1492, options=drl_opts, layout="layout_with_drl") %>%
  visSave(file = "modularity_drl.html")
time <- toc(quiet=TRUE); time <- time$toc - time$tic
cat(sprintf("done. (%.2fs)\n", time))
