library(RNeo4j)
library(igraph)
library(visNetwork)

graph = startGraph(
  "http://localhost:7474/db/data/",
  username="neo4j",
  password="Neo4j")

nodes_def = cypher(graph, "
  MATCH (obj:definition)
  RETURN obj.objectId AS id,
         obj.name AS label,
         obj.pagerank AS value,
         obj.path AS title,
         \"triangle\" AS shape")

nodes_proof = cypher(graph, "
  MATCH (obj:proof)
  RETURN obj.objectId AS id,
         obj.name AS label,
         obj.pagerank AS value,
         obj.path AS title,
         \"square\" AS shape")

edges = cypher(graph, "
  MATCH (src)-[edge]->(dst)
  WHERE (src:definition or src:proof) AND (dst:definition or dst:proof)
  RETURN src.objectId AS from, dst.objectId AS to, edge.weight AS weight")

nodes <- rbind(nodes_proof, nodes_def)

ig = graph.data.frame(edges, directed=FALSE, nodes)
communities = cluster_fast_greedy(ig)
memb = data.frame(id = communities$name, group = communities$membership)
nodes = merge(nodes, memb)

visNetwork(nodes, edges, width="100%") %>%
  visSave(file = "pagerank_fast_greedy.html")

print(head(nodes))
nodes <- subset(nodes, select=-value)
print(head(nodes))
nodes$value = betweenness(ig)
print(head(nodes))
communities = cluster_edge_betweenness(ig)
memb = data.frame(id = communities$name, group = communities$membership)
nodes <- subset(nodes, select=-group)
nodes = merge(nodes, memb)

visNetwork(nodes, edges, width="100%") %>%
  visSave(file = "betweenness.html")
