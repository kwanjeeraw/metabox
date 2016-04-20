//@src http://jsfiddle.net/motowilliams/7rL2C/
//@function convert json to tab-delimited format
function JSONToTabConvertor(JSONData,ShowHeader) {
    //If JSONData is not an object then JSON.parse will parse the JSON string in an Object
    var arrData = typeof JSONData != 'object' ? JSON.parse(JSONData) : JSONData;
    var CSV = '';    
        //This condition will generate the Label/Header
    if (ShowHeader) {
        var row = "";
        //This loop will extract the label from 1st index of on array
        for (var index in arrData[0]) {
            //Now convert each value to string and comma-seprated
            row += index.replace(/^name$/,"nodename") + '\t';
        }
        row = row.slice(0, -1);
        //append Label row with line break
        CSV += row + '\n';
    }
    //1st loop is to extract each row
    for (var i = 0; i < arrData.length; i++) {
        var row = "";
        //2nd loop will extract each column and convert it in string comma-seprated
        for (var index in arrData[i]) {
            row += arrData[i][index] + '\t';
        }
        row = row.slice(0, -1);
        //add a line break after each row
        CSV += row + '\n';
    }
    if (CSV == '') {        
        alert("Invalid data");
    }
    return CSV;
}

//@function load one column text file and pass to textarea
function loadTxtFile(event) {
  var input = event.target;
  var reader = new FileReader();
  reader.onload = function(){
    var text = reader.result;
    $("#txtinput").val("");//clear textarea
    $("#txtinput").val(text);//assign value to textarea
  };
  reader.readAsText(input.files[0]);
};

//@function export network outputs as a zip file
function exportNwZip(nodes, edges, network){
    var zip = new JSZip();
    zip.file("node.txt", JSONToTabConvertor(nodes,true))
    zip.file("edge.txt", JSONToTabConvertor(edges,true))
    zip.file("network.png", network.replace(/^data:image\/(png|jpg);base64,/, ""), {base64: true});
    zip.generateAsync({type:"base64"})
    .then(function (content) {
      var a = document.createElement('a'); 
      a.href="data:application/zip;base64,"+content;
      a.download = "OUTPUT.zip";
      a.click();
    });    
}

//@function load spinner
function showSpinner(){
    $('#notifyBox').modal('show');
    var opts = {
      lines: 10, // The number of lines to draw
      length: 8, // The length of each line
      width: 10, // The line thickness
      radius: 20, // The radius of the inner circle
      corners: 1, // Corner roundness (0..1)
      rotate: 0, // The rotation offset
      direction: 1, // 1: clockwise, -1: counterclockwise
      color: '#0073b7', // #rgb or #rrggbb or array of colors
      speed: 1, // Rounds per second
      trail: 60, // Afterglow percentage
      shadow: false, // Whether to render a shadow
      hwaccel: false, // Whether to use hardware acceleration
      className: 'spinner', // The CSS class to assign to the spinner
      zIndex: 2e9, // The z-index (defaults to 2000000000)
      top: 'auto', // Top position relative to parent in px
      left:'auto' // Left position relative to parent in px
    };
    var spinner = new Spinner(opts).spin(document.getElementById('loading_spinner'));//assign spinner to modal box
    return spinner;
}

//@function hide spinner
function hideSpinner(spinner){
    spinner.stop();
    $('#notifyBox').modal('hide');
}

//@code get URL variables
//@param param, name of form input
//@return form values
//@based http://stackoverflow.com/questions/8460265/
function getURLVars(param)
{
    var vars = [], hash;
    var hashes = window.location.href.slice(window.location.href.indexOf('?') + 1).split('&');
    for(var i = 0; i < hashes.length; i++)
    {
        hash = hashes[i].split('=');
        if($.inArray(hash[0], vars)>-1)
        {
            vars[hash[0]]+=","+hash[1];
        }
        else
        {
            vars.push(hash[0]);
            vars[hash[0]] = hash[1];
        }
    }
    var val = vars[param];
    if(val){val = val.replace(/\+/g, ' ');//replace whitespace
    }else{val = "";}
    return val;
}

//@function format table header
function formatTableHeader(jsonData){
    var keyls = Object.keys(jsonData);//get list of keys
    var colnames = [];
    for (var i = 0; i < keyls.length; i++) {//table header = json keys
      colnames.push({title: keyls[i], data: keyls[i]});
    }
    return colnames;
}

//@function draw node table output
function drawNodeTable(objNode, xrefInd) {
    var nodeTable = $('#nodes').DataTable({
      "destroy": true,
      "columnDefs": [{
        "render": function (data, type, row) {
          return type === 'display' && data.length > 60 ?
          '<span title=\"' + data + '\">' + data.substr(0, 60) + ' ...</span>' : data;//format xref string
        },
        "targets": xrefInd
      }],
      data: objNode,
      columns: formatTableHeader(objNode[0])
    });
    return nodeTable;
}

//@function draw edge table output
function drawEdgeTable(objEdge) {
    var edgeTable = $('#edges').DataTable({
      "destroy": true,
      data: objEdge,
      columns: formatTableHeader(objEdge[0])
    });
    return edgeTable;
}

//@function draw cytoscapeJS network
function drawNetwork(objNode, objEdge){
    var nodels = [];
    var edgels = [];
    for (var i = 0; i < objNode.length; i++) {//format list of nodes for cytoscapeJS
      nodels.push({
          data: objNode[i]
      });
    }
    for (var i = 0; i < objEdge.length; i++) {//format list of edges for cytoscapeJS
      edgels.push({
          data: objEdge[i]
      });
    }
    var cy = cytoscape({//initialize cytoscapeJS
      container: document.getElementById('cy'),
      boxSelectionEnabled: false,
      autounselectify: true,
      elements: {
        nodes: nodels,
        edges: edgels
      },
      layout: {
        name: 'cose',
        padding: 10
      },
      style: cytoscape.stylesheet()
        .selector('node')
          .css({
            'content': 'data(nodename)',
            'background-color':'#A4A4A4',
            'text-valign': 'bottom',
            'font-size':'16'
          })
        .selector('node[nodelabel = "Phenotype"]')
          .css({
            'shape':'octagon'
          })
        .selector('node[nodelabel = "Compound"]')
          .css({
            'shape':'ellipse'
          })
        .selector('node[nodelabel = "Dna"]')
          .css({
            'shape':'pentagon'
          })
        .selector('node[nodelabel = "Gene"]')
          .css({
            'shape':'hexagon'
          })
        .selector('node[nodelabel = "Pathway"]')
          .css({
            'shape':'diamond'
          })
        .selector('node[nodelabel = "Protein"]')
          .css({
            'shape':'rectangle'
          })
        .selector('node[nodelabel = "Rna"]')
          .css({
            'shape':'star'
          })
        .selector('edge')
          .css({
            'width': 3
          }) 
        .selector('edge[type = "ANNOTATION"]')
          .css({
            'line-color': '#8C8C8C',
            'target-arrow-color': '#8C8C8C'
          })
          .selector('edge[type = "BIOCHEMICAL_REACTION"]')
          .css({
            'line-color': '#ff0000',
            'target-arrow-color': '#ff0000'
          })
          .selector('edge[type = "CATALYSIS"]')
          .css({
            'line-color': '#4169E1',
            'target-arrow-color': '#4169E1'
          })
          .selector('edge[type = "CONTROL"]')
          .css({
            'line-color': '#006400',
            'target-arrow-color': '#006400'
          })
          .selector('edge[type = "CONVERSION"]')
          .css({
            'line-color': '#FF00FF',
            'target-arrow-color': '#FF00FF'
          })
          .selector('edge[type = "GENETIC_ASSOCIATION"]')
          .css({
            'line-color': '#00BFFF',
            'target-arrow-color': '#00BFFF'
          })
          .selector('edge[type = "MOLECULAR_BINDING"]')
          .css({
            'line-color': '#51ED34',
            'target-arrow-color': '#51ED34'
          })
          .selector('edge[type = "TANIMOTO_SIMILARITY"]')
          .css({
            'line-color': '#662506',
            'target-arrow-shape': 'none'
          })
          .selector('edge[type = "CORRELATION"]')
          .css({
            'line-color': '#3f007d',
            'target-arrow-shape': 'none'
          })
          .selector('edge[type = "PARTIAL_CORRELATION"]')
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
          })
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
    return cy;
}