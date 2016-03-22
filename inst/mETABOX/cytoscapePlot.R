cytoscapePlot <- function(nodeList=NULL, edgeList, nwpage) {
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
  for(i in 1:nrow(nodeList)) {   
    nodeEntries <- c(nodeEntries, paste0("{ data: ", gsub('(^\\[|\\]$)', '',jsonlite::toJSON(nodeList[i,])), "}"))
  }
  nodeEntries <- paste(nodeEntries, collapse=", ")
  
  #format edges for cytoscapeJS
  edgeEntries <- NULL
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
          'font-size':'24'
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
  cat(results)
}