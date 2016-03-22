##list of attribute types
propertyList <- list(
  stringVal = c("neo4jid","grinnid","name","description","organism","datasource","comment"),
  listVal = c("synonyms","properties","xref")
)

##list of Cypher to query nodes
nodeList <- list(
  exactMatch = "UNWIND keyword AS x WITH x MATCH (node:label) WHERE node.property = x",
  exactCollection = "MATCH (node:label) WHERE ANY(y IN node.property WHERE y = \'keyword\')",
  regexMatch = "MATCH (node:label) WHERE lower(node.property) =~ lower(\'.*keyword.*\')",
  regexCollection = "MATCH (node:label) WHERE ANY(y IN node.property WHERE lower(y) =~ lower(\'.*keyword.*\'))"
)

##list of Cypher to map ids
idList <- list(
  exactMatch = "MATCH (node:label) WHERE node.property = \'keyword\'",
  exactCollection = "MATCH (node:label) WHERE ANY(y IN node.property WHERE y = \'keyword\')",
  regexMatch = "MATCH (node:label) WHERE lower(node.property) =~ lower(\'.*keyword.*\')",
  regexCollection = "MATCH (node:label) WHERE ANY(y IN node.property WHERE lower(y) =~ lower(\'.*keyword.*\'))"
)

##list of Cypher to query from-[relationship]->to
pathList <- list(
  from = "UNWIND keyword AS x WITH x MATCH ptw = (from:fromtype)-[r:reltype]->(to:totype) WHERE ID(from) = toInt(x) RETURN DISTINCT ptw",
  to = "UNWIND keyword AS x WITH x MATCH ptw = (from:fromtype)-[r:reltype]->(to:totype) WHERE ID(to) = toInt(x) RETURN DISTINCT ptw",
  fromto = "MATCH ptw = (from:fromtype)-[r:reltype]->(to:totype) WHERE ID(from) = toInt(\'keyfrom\') AND ID(to) = toInt(\'keyto\') RETURN DISTINCT ptw"
)

##list of Cypher to query from-[relationships]->to
pathsList <- list(
  from = "UNWIND keyword AS x WITH x MATCH ptw = relpattern WHERE ID(from) = toInt(x) RETURN DISTINCT ptw",
  to = "UNWIND keyword AS x WITH x MATCH ptw = relpattern WHERE ID(to) = toInt(x) RETURN DISTINCT ptw",
  fromto = "MATCH ptw = relpattern WHERE ID(from) = toInt(\'keyfrom\') AND ID(to) = toInt(\'keyto\') RETURN DISTINCT ptw"
)

###for enrichment analysis
#x = combGeneNetwork$edges[1:30,]
#g = graph.edgelist(as.matrix(data.frame(from=unlist(x[,1]),to=unlist(x[,2]))),directed = FALSE)
#g2 = cocitation(g)
#g2[c(2,7,25),c(3,5,8,10)]
