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

##list of Cypher to query from-[relationship]->to using GID
pathList_GID <- list(
  from = "UNWIND keyword AS x WITH x MATCH ptw = (from:fromtype)-[r:reltype]->(to:totype) WHERE from.GID = x RETURN DISTINCT ptw",
  to = "UNWIND keyword AS x WITH x MATCH ptw = (from:fromtype)-[r:reltype]->(to:totype) WHERE to.GID = x RETURN DISTINCT ptw",
  fromto = "MATCH ptw = (from:fromtype)-[r:reltype]->(to:totype) WHERE from.GID = \'keyfrom\' AND to.GID = \'keyto\' RETURN DISTINCT ptw"
)

##list of Cypher to query from-[relationships]->to using GID
pathsList_GID <- list(
  from = "UNWIND keyword AS x WITH x MATCH ptw = relpattern WHERE from.GID = x RETURN DISTINCT ptw",
  to = "UNWIND keyword AS x WITH x MATCH ptw = relpattern WHERE to.GID = x RETURN DISTINCT ptw",
  fromto = "MATCH ptw = relpattern WHERE from.GID = \'keyfrom\' AND to.GID = \'keyto\' RETURN DISTINCT ptw"
)
