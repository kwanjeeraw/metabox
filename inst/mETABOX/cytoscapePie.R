cytoscapePie <- function(nodeList=NULL, edgeList, eraList, nwpage) {
  #edgeData must have at least source and target columns
  if(nrow(edgeList) == 0 || !(all(c("source", "target") %in% colnames(edgeList)))) {
    return(NULL)
  }
  if(is.null(nodeList)){
    #format nodeList from edgeList
    #sclabel = sapply(edgeList$nodelabel, function(x) ifelse(!is.null(x),x,""))
    #trlabel = sapply(edgeList$nodelabel, function(x) ifelse(!is.null(x),x,""))
    so = data.frame(id=edgeList$source, gid=edgeList$source, nodename=edgeList$source, nodelabel="", stringsAsFactors = FALSE)
    ta = data.frame(id=edgeList$target, gid=edgeList$target, nodename=edgeList$target, nodelabel="", stringsAsFactors = FALSE)
    nodeList = unique(rbind(so,ta))
  }
  #nodeList must have at least id columns
  if(!is.null(nodeList)&&!("id" %in% names(nodeList))) {
    return(NULL)
  }
  showlabel = "nodename" #set default node label
  if(!("nodename" %in% names(nodeList))){
    showlabel = "id"
  }
  #format nodes for cytoscapeJS
  nodeEntries <- NULL
  eraListTop = na.omit(eraList[order(eraList$`p adj (non-dir.)`),][1:5,])$member #show only top 5
  colno = ncol(nodeList)
#nodeList = nodeList[nodeList$id %in% eraListTop[[1]],]
#eraListTop = list(x=c("218689","218788","218803"),y=c("218689"))
#edgeList = data.frame(source = c("218689","218788"), target = c("218803","218803"), type= c("ANNOTATION","ANNOTATION"), stringsAsFactors = FALSE)
  #preset
  nodeList$pie1 = 0
  nodeList$pie2 = 0
  nodeList$pie3 = 0
  nodeList$pie4 = 0
  nodeList$pie5 = 0
  for(i in 1:length(eraListTop)){
    nodeList[nodeList$id %in% eraListTop[[i]],colno+i] = 2
  }
  for(i in 1:nrow(nodeList)) { 
    nodeEntries <- c(nodeEntries, paste0("{ data: ", gsub('(^\\[|\\]$)', '',jsonlite::toJSON(nodeList[i,])), "}"))
  }
  nodeEntries <- paste(nodeEntries, collapse=", ")
#nodeEntries ="{ data: {\"id\":\"218689\",\"gid\":\"9750\",\"nodename\":\"L-citrulline\",\"nodelabel\":\"Compound\",\"xref\":\"9750||C00327||C0372||C7629_SIGMA||CHEBI:16349||CHEMBL444814||CIR||CTK3J1797||D07706||DB-049088||DB00155||HcwL@DpPWTf]Vyjjih{@@||HMDB00904||LS-185994||M90009539P||MT-06782\",\"pie1\":2,\"pie2\":2}}, { data: {\"id\":\"218788\",\"gid\":\"92135\",\"nodename\":\"(R)-3-Hydroxybutyric acid\",\"nodelabel\":\"Compound\",\"xref\":\"92135||C01089||CH-195||CHEBI:17066||CHEMBL1162484||CTK5E9838||H-4010||HMDB00011||HMS2230E08||LMFA01050243||MLS001333960||MolPort-003-824-844\",\"pie1\":2,\"pie2\":\"\"}}, { data: {\"id\":\"218803\",\"gid\":\"5793\",\"nodename\":\"D-Glucose\",\"nodelabel\":\"Compound\",\"xref\":\"5793||C00031||CA001722||CA012318||CHEBI:4167||CHEMBL1222250||CTK1A6945||D-(+)-Glucose||D00009||G0048||G15021LG||Glc||HMDB00122\",\"pie1\":2,\"pie2\":\"\",\"pie3\":2}}"
#showlabel = "id"
  #format edges for cytoscapeJS
  edgeEntries <- NULL
  edgeList = edgeList[!duplicated(edgeList[c("source","target","type")]), ] #remove duplicate edges
  for(i in 1:nrow(edgeList)) {   
    edgeEntries <- c(edgeEntries, paste0("{ data: ", gsub('(^\\[|\\]$)', '',jsonlite::toJSON(edgeList[i,])), "}"))
  }
  edgeEntries <- paste(edgeEntries, collapse=", ")
  
  #css for the network
  NetworkCSS <- paste0("<style>
                       #",nwpage," {
                        height: 85%;
                        width: 95%;
                        position: absolute;
                        left: 30px;
                        top: 40px;
}
</style>")
  
  #javascript for creating the network
  MainScript <- paste0("<script>
                       var cy;
                       var cyjson = 
                       {
                       nodes: [",nodeEntries,"],
                       edges: [",edgeEntries,"]
                       }
                       $(function(){ // on dom ready
                         cy = cytoscape({
                         container: document.getElementById('",nwpage,"'),
                         style: cytoscape.stylesheet()
                         .selector('node')
                         .css({
                         'content': 'data(",showlabel,")',
                         'background-color':'#A4A4A4',
                         'text-valign': 'bottom',
                         'font-size':'24',
                         'pie-size': '95%',
                         'pie-1-background-color': '#FF0000',
                         'pie-1-background-size': 'mapData(pie1, 0, 10, 0, 100)',
                         'pie-2-background-color': '#6AA3FF',
                         'pie-2-background-size': 'mapData(pie2, 0, 10, 0, 100)',
                         'pie-3-background-color': '#FFD36A',
                         'pie-3-background-size': 'mapData(pie3, 0, 10, 0, 100)',
                         'pie-4-background-color': '#7EFF6A',
                         'pie-4-background-size': 'mapData(pie4, 0, 10, 0, 100)',
                         'pie-5-background-color': '#E76AFF',
                         'pie-5-background-size': 'mapData(pie5, 0, 10, 0, 100)'
                         })
                         .selector('node[nodelabel = \"Phenotype\"]')
                         .css({
                         'shape':'octagon'
                         })
                         .selector('node[nodelabel = \"Compound\"]')
                         .css({
                         'shape':'ellipse'
                         })
                         .selector('node[nodelabel = \"Dna\"]')
                         .css({
                         'shape':'pentagon'
                         })
                         .selector('node[nodelabel = \"Gene\"]')
                         .css({
                         'shape':'hexagon'
                         })
                         .selector('node[nodelabel = \"Pathway\"]')
                         .css({
                         'shape':'diamond'
                         })
                         .selector('node[nodelabel = \"Protein\"]')
                         .css({
                         'shape':'rectangle'
                         })
                         .selector('node[nodelabel = \"Rna\"]')
                         .css({
                         'shape':'star'
                         })
                         .selector('edge')
                         .css({
                         'width': 3,
                         'target-arrow-shape': 'triangle'
                         })
                         .selector('edge[type = \"ANNOTATION\"]')
                         .css({
                         'line-color': '#8C8C8C',
                         'target-arrow-color': '#8C8C8C'
                         })
                         .selector('edge[type = \"BIOCHEMICAL_REACTION\"]')
                         .css({
                         'line-color': '#ff0000',
                         'target-arrow-color': '#ff0000'
                         })
                         .selector('edge[type = \"CATALYSIS\"]')
                         .css({
                         'line-color': '#4169E1',
                         'target-arrow-color': '#4169E1'
                         })
                         .selector('edge[type = \"CONTROL\"]')
                         .css({
                         'line-color': '#006400',
                         'target-arrow-color': '#006400'
                         })
                         .selector('edge[type = \"CONVERSION\"]')
                         .css({
                         'line-color': '#FF00FF',
                         'target-arrow-color': '#FF00FF'
                         })
                         .selector('edge[type = \"GENETIC_ASSOCIATION\"]')
                         .css({
                         'line-color': '#00BFFF',
                         'target-arrow-color': '#00BFFF'
                         })
                         .selector('edge[type = \"MOLECULAR_BINDING\"]')
                         .css({
                         'line-color': '#51ED34',
                         'target-arrow-color': '#51ED34'
                         })
                         .selector('edge[type = \"TANIMOTO_SIMILARITY\"]')
                         .css({
                         'line-color': '#662506',
                         'target-arrow-shape': 'none'
                         })
                         .selector('edge[type = \"CORRELATION\"]')
                         .css({
                         'line-color': '#3f007d',
                         'target-arrow-shape': 'none'
                         })
                         .selector('edge[type = \"PARTIAL_CORRELATION\"]')
                         .css({
                         'line-color': '#f16913',
                         'target-arrow-shape': 'none'
                         })
                         .selector(':selected')
                         .css({
                         'background-color': 'black',
                         'line-color': 'black',
                         'target-arrow-color': 'black',
                         'source-arrow-color': 'black'
                         })
                         .selector('.faded')
                         .css({
                         'opacity': 0.25,
                         'text-opacity': 0
                         }),
                         
                         elements: cyjson,
                         layout: {
                         name: 'cose',
                         padding: 10
                         }
                         });

                         cy.on('tap', 'node', function(e){
                         var node = e.cyTarget; 
                         var neighborhood = node.neighborhood().add(node);
                         cy.elements().addClass('faded');
                         neighborhood.removeClass('faded');
                         });
                         cy.on('tap', function(e){
                         if( e.cyTarget === cy ){
                         cy.elements().removeClass('faded');
                         }
                         });
                       }); // on dom ready
                       </script>")
  results <- paste0(NetworkCSS, MainScript)
  return(cat(results))
  }