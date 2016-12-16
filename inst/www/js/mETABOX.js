//@ref http://jsfiddle.net/motowilliams/7rL2C/
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
        CSV += row + '\r\n';
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
        CSV += row + '\r\n';
    }
    if (CSV == '') {
        alert("Invalid data");
    }
    return CSV;
}

//@function convert tab-delimited format to json object
//@param tab tab-delimited input
//@param moltype type of entity e.g. PubChem, uniprot, ensembl
function tabToJSON(tab,moltype) {
	var outjson = [];
	if(tab==""){
		outjson = tab.split(/\s+/g);
	}else{
		var lines = tab.split(/\r?\n/);
		var headers = lines[0].split(/\t/);//keep header
		if(headers.length > 1){//with headers
			for(var i = 1; i < lines.length; i++){
				var obj = {};
				var currentline = lines[i].split(/\t/);
				for(var j = 0; j < headers.length; j++){
					obj[headers[j]] = currentline[j];
				}
				outjson.push(obj);
			}
		}else{//no header
			for(var i = 0; i < lines.length; i++){
				var obj = {};
				obj[moltype] = lines[i];
				outjson.push(obj);
			}
		}
	}
	return JSON.parse(JSON.stringify(outjson)); //return JSON object
}

//@function load one column text file and pass to textarea
//@param id id element
function loadTxtFile(event,id) {
  var input = event.target;
  var reader = new FileReader();
  reader.onload = function(){
    var text = reader.result;
    $(id).val("");//clear textarea
    $(id).val(text);//assign value to textarea
  };
  reader.readAsText(input.files[0]);
};

//@function load example and pass to textarea
//@param id id element
//@param example type of entities
function loadexamplelist(id,example){
	var exdat = null;
	switch (example)
	{
	   case 'compounds': exdat = [43,51,196,311,764,790,970,1005,1044,1060,1110,1188,10267,10413,27476,79025,107689,439168,439183,440641];
	   break;
	   case 'proteins': exdat = ['P18669','P15259','P51854','Q16822','O75390','P13929','P11498','P07195','P06744','P09467'];
	   break;
	   case 'compoundsnw': exdat = [311,970,1005,1060,10267,79025,107689,439168,439183,439278,439284,440641];
	   break;
	   case 'relpattern': exdat = ['(:Gene)-[:CONVERSION]->(from:Protein)-[:CATALYSIS]->(to:Compound)'];
	   break;
	   default: exdat = [43,51,196,311,764,790,970,1005,1044,1060,1110,1188,10267,10413,27476,79025,107689,439168,439183,440641];
	}
	$(id).val("");//clear textarea
	$(id).val(exdat.join("\n"));//assign value to textarea
}

var readme = "- node.txt contains node attribute.\r\n"+
"- PubChem or uniprot or ensembl column is equal to gid column.\r\n"+
"- id column is neo4j ids and used in source or target columns of edge.txt or in member column of functional analysis results.\r\n"+
"- If exists, user input data are mapped to nodes and kept in node.txt.";

//@function export network outputs as a zip file
//@param nodes, edges array of json objects
//@param img cytoscapeJS png object
function exportNwZip(nodes, edges, img){
    var zip = new JSZip();
	zip.file("README.txt", readme);
    zip.file("node.txt", JSONToTabConvertor(nodes,true));
    zip.file("edge.txt", JSONToTabConvertor(edges,true));
    zip.file("network.png", img.replace(/^data:image\/(png|jpg);base64,/, ""), {base64: true});
    zip.generateAsync({type:"base64"})
    .then(function (content) {
      var a = document.createElement('a');
      a.href="data:application/zip;base64,"+content;
      a.download = "OUTPUT.zip";
      a.click();
    });
}

//@function export enrichment outputs as a zip file
//@param nodes, edges, enrichment array of json objects
//@param img, nlegend cytoscapeJS png object
function exportEnrichmentZip(nodes, edges, enrichment, pairs, img, nlegend){
    var zip = new JSZip();
	zip.file("README.txt", readme);
    zip.file("node.txt", JSONToTabConvertor(nodes,true));
    if (edges != null) {zip.file("edge.txt", JSONToTabConvertor(edges,true));}
    zip.file("result.txt", JSONToTabConvertor(enrichment,true));
    zip.file("annotationPair.txt", JSONToTabConvertor(pairs,true));
    if (img != null) {zip.file("network.png", img.replace(/^data:image\/(png|jpg);base64,/, ""), {base64: true});}
    if (nlegend != null) {zip.file("nodeLegend.png", nlegend.replace(/^data:image\/(png|jpg);base64,/, ""), {base64: true});}
    zip.generateAsync({type:"base64"})
    .then(function (content) {
      var a = document.createElement('a');
      a.href="data:application/zip;base64,"+content;
      a.download = "OUTPUT.zip";
      a.click();
    });
}

//@function export tab-delimited text file
//@param content array of json objects
function exportTxtFile(content){
    var a = document.createElement('a');
    a.href = "data:text/plain," + escape(JSONToTabConvertor(content,true));
    a.download = "OUTPUT.txt";
    a.click();
}

//@function format cytoscape json elements
//@param network cytoscape json elements
function formatCyJSON(network){
    var nodels = [];
    var edgels = [];
    Object.keys(network.elements.nodes).forEach(function(idx) {
        nodels.push(network.elements.nodes[idx].data);
    });
    Object.keys(network.elements.edges).forEach(function(idx) {
        edgels.push(network.elements.edges[idx].data);
    });
    return {'nodes':nodels,'edges':edgels};
}

//@function load spinner
//@param txt text message
function showSpinner(txt=null){
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
    $("#notifyTxt").text(txt);
    return spinner;
}

//@function hide spinner
function hideSpinner(spinner){
    spinner.stop();
    $('#notifyBox').modal('hide');
}

//@function get URL variables
//@param param, name of variable
//@ref http://stackoverflow.com/questions/8460265/
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
//@param jsonData json object
function formatTableHeader(jsonData){
    var keyls = Object.keys(jsonData);//get list of keys
    var colnames = [];
    for (var i = 0; i < keyls.length; i++) {//table header = json keys
      colnames.push({title: keyls[i], data: keyls[i]});
    }
    return colnames;
}

//@function draw table
//@param id id element
//@param data array of json objects
function drawTable(id, data) {
	editor = new $.fn.dataTable.Editor( {
        table: id,
    } );
	 $(id).DataTable( {
		destroy: true,
	    data: data,
		"scrollX": true,
		"scrollY": "450px",
		"paging": false,
		fixedColumns: true,
		scrollCollapse: true,
		columns: formatTableHeader(data[0]),
		dom: 'Blfrtip',
		lengthChange: false,
       buttons: [
        {
            extend: 'csv',
            text: 'download',
			 name: 'csv'
        }
    ]
    } );
  /* $(id).DataTable({
      destroy: true,
      scrollX: true,
      scrollY: 400,
      scrollCollapse: true,
      data: data,
      columns: formatTableHeader(data[0]),
	  select:true,
	  buttons: [
            { extend: "create", editor: editor },
            { extend: "edit",   editor: editor },
            { extend: "remove", editor: editor },
            "selectRows",
            "selectColumns",
            "selectCells",
            "selectNone"
        ]
    });*/
}

//@function draw table
//@param id id element
//@param data array of json objects
function drawTableNw(id, data) {
    var dtTable = $(id).DataTable({
      "destroy": true,
      "scrollX": true,
      "scrollY": 400,
      "scrollCollapse": true,
      "data": data,
      "columns": formatTableHeader(data[0])
    });
    return dtTable;
}

//@function clear 2 tables e.g. network tables
//@param ndtable, edtable dataTable object
//@param nid, eid table id
//@param hasResult boolean if table contains results
function clear2Table(ndtable, edtable, nid, eid, hasResult) {
    if(hasResult){
        if(ndtable != undefined){
            ndtable.destroy();
            edtable.destroy();
        }
    }else{
        if (ndtable != undefined) {
            ndtable.clear().draw();
            edtable.clear().draw();
        }
    }
    $(nid).empty();
    $(eid).empty();
}

//@function clear 3 tables e.g. enrichment tables
//@param ndtable, edtable, pwtable dataTable object
//@param nid, eid, pid table id
//@param hasResult boolean if table contains results
function clear3Table(ndtable, edtable, pwtable, nid, eid, pid, hasResult) {
    if(hasResult){
        if(ndtable != undefined){
            ndtable.destroy();
            edtable.destroy();
            pwtable.destroy();
        }
    }else{
        if (ndtable != undefined) {
            ndtable.clear().draw();
            edtable.clear().draw();
            pwtable.clear().draw();
        }
    }
    $(nid).empty();
    $(eid).empty();
    $(pid).empty();
}

//@function clean functional analysis outputs
function cleanFnOutputs() {
    $('#consolemsgwc').empty();
    $('#wordcld').empty();
    $('#annopairs').empty();
    $('#wdc').hide();
    $('#d3tree').hide();
}

//@function toggle Mesh tree button and content
//@param anno string of annotation type
function toggleMeshTree(anno){
    $('#d3tree').hide();
    if (anno == "pathway") {
      $('#treeBtn').prop('disabled', true);
    }else{
      $('#treeBtn').prop('disabled', false);
    }
}

//@function draw table with color code
//@param id id element
//@param dt array of json objects
function drawPieTable(id, dt) {
    var pieTable = $(id).DataTable({
        "createdRow": function( row, data, index ){//format row color
          var bgc = ["#FF0000","#FFFFB3","#907FFF","#D56767","#54CCFF","#FDB462","#60DA3E","#F4B5D6","#FD49FF","#00715E"];
          if ( data.rank < 11 ) {
            $('td', row).eq(0).css({
              'background-color': bgc[data.rank-1]
            });
          }
        },
        "destroy": true,
        "scrollX": true,
        "scrollY": 400,
        "scrollCollapse": true,
        "data": dt,
        "columns": formatTableHeader(dt[0])
      });
    return pieTable;
}

//@function format node for cytoscapeJS
//@param nodeArr array of json objects
function formatNode(nodeArr) {
    var nodels = [];
    for (var i = 0; i < nodeArr.length; i++) {//format list of nodes for cytoscapeJS
      nodels.push({
        data: nodeArr[i]
      });
    }
    return nodels;
}

//@function format node for cytoscapeJS
//@param nodeArr, enrichArr array of json objects
function formatPieNode(nodeArr, enrichArr) {
    var nodels = [];
    var legendls = [];
    for (var i = 0; i < nodeArr.length; i++) {//format list of nodes for cytoscapeJS
        nodels.push({
            data: nodeArr[i]
        });
        Object.keys(enrichArr).forEach(function(idx) {
          if(enrichArr[idx].member.indexOf(nodeArr[i].id)!= -1){
              var pie = "pie" + enrichArr[idx].rank;
              nodels[i].data[pie] = 1;
          }
        });
    }
    //format list of legends for cytoscapeJS
    Object.keys(enrichArr).forEach(function(idx) {
      if(enrichArr[idx].rank <= 10){
        var legend = {id:enrichArr[idx].id,nodename:'#'+enrichArr[idx].rank+'_'+enrichArr[idx].nodename,nodelabel:"Legend"};
        var pie = "pie" + enrichArr[idx].rank;
        legend[pie] = 10;
        legendls.push({
            data: legend
        });
      }
    });
    return [nodels, legendls];
}

//@function format edge for cytoscapeJS
//@param edgeArr array of json objects
function formatEdge(edgeArr) {
    var edgels = [];
    for (var i = 0; i < edgeArr.length; i++) {//format list of edges for cytoscapeJS
      edgels.push({
          data: edgeArr[i]
      });
    }
    return edgels;
}

//@function draw cytoscapeJS network
//@param objNode, objEdge array of json objects accepted by cytoscapeJS [{data:{id:'id',name:'name'}}]
//@param pid panel id
//@param lyout network layout
function drawNetwork(objNode, objEdge, pid, lyout = "cose"){
    var cy = cytoscape({//initialize cytoscapeJS
      container: document.getElementById(pid),
      boxSelectionEnabled: true,
      autounselectify: false,
      selectionType:'additive',
      elements: {
        nodes: objNode,
        edges: objEdge
      },
      layout: {
        name: lyout,
        padding: 5
      },
      style: cytoscape.stylesheet()
        .selector('node')
          .style({
            'content': 'data(nodename)',
            'background-color':'#B3B3B3',
            'text-valign': 'bottom',
            'color':'#444444',
            'text-outline-color':'#FFFFFF',
            'text-outline-width':0.3,
            'font-size':16,
            'font-weight':'bold',
            'pie-size': '90%',
            'pie-1-background-color': '#FF0000',
            'pie-1-background-size': 'mapData(pie1, 0, 10, 0, 100)',
            'pie-2-background-color': '#FFFFB3',
            'pie-2-background-size': 'mapData(pie2, 0, 10, 0, 100)',
            'pie-3-background-color': '#907FFF',
            'pie-3-background-size': 'mapData(pie3, 0, 10, 0, 100)',
            'pie-4-background-color': '#D56767',
            'pie-4-background-size': 'mapData(pie4, 0, 10, 0, 100)',
            'pie-5-background-color': '#54CCFF',
            'pie-5-background-size': 'mapData(pie5, 0, 10, 0, 100)',
            'pie-6-background-color': '#FDB462',
            'pie-6-background-size': 'mapData(pie6, 0, 10, 0, 100)',
            'pie-7-background-color': '#60DA3E',
            'pie-7-background-size': 'mapData(pie7, 0, 10, 0, 100)',
            'pie-8-background-color': '#F4B5D6',
            'pie-8-background-size': 'mapData(pie8, 0, 10, 0, 100)',
            'pie-9-background-color': '#FD49FF',
            'pie-9-background-size': 'mapData(pie9, 0, 10, 0, 100)',
            'pie-10-background-color': '#00715E',
            'pie-10-background-size': 'mapData(pie10, 0, 10, 0, 100)'
          })
        .selector('node[nodelabel = "Legend"]')
          .style({
            'shape':'rectangle',
            'color':'#000',
            'font-size':14
          })
        .selector('node[nodelabel = "Phenotype"]')
          .style({
            'shape':'octagon'
          })
        .selector('node[nodelabel = "Compound"]')
          .style({
            'shape':'ellipse'
          })
        .selector('node[nodelabel = "Dna"]')
          .style({
            'shape':'pentagon'
          })
        .selector('node[nodelabel = "Gene"]')
          .style({
            'shape':'hexagon'
          })
        .selector('node[nodelabel = "Pathway"]')
          .style({
            'shape':'diamond'
          })
        .selector('node[nodelabel = "Protein"]')
          .style({
            'shape':'rectangle'
          })
        .selector('node[nodelabel = "Rna"]')
          .style({
            'shape':'star'
          })
        .selector('edge')
          .style({
            'width': 3
          })
        .selector('edge[type = "ANNOTATION"]')
          .style({
            'line-color': '#8C8C8C',
            'target-arrow-color': '#8C8C8C'
          })
        .selector('edge[type = "BIOCHEMICAL_REACTION"]')
          .style({
            'line-color': '#ff3333',
            'target-arrow-color': '#ff3333',
			      'target-arrow-shape': 'triangle',
			      'source-arrow-color': '#ff3333',
			      'source-arrow-shape': 'triangle'
          })
        .selector('edge[type = "CATALYSIS"]')
          .style({
            'line-color': '#577ae4',
            'target-arrow-color': '#577ae4',
            'target-arrow-shape': 'triangle'
          })
        .selector('edge[type = "CONTROL"]')
          .style({
            'line-color': '#00b100',
            'target-arrow-color': '#00b100',
			      'target-arrow-shape': 'triangle'
          })
        .selector('edge[type = "CONVERSION"]')
          .style({
            'line-color': '#ff9aff',
            'target-arrow-color': '#ff9aff',
			      'target-arrow-shape': 'triangle'
          })
        .selector('edge[type = "GENETIC_ASSOCIATION"]')
          .style({
            'line-color': '#1ac5ff',
            'target-arrow-color': '#1ac5ff'
          })
        .selector('edge[type = "MOLECULAR_BINDING"]')
          .style({
            'line-color': '#b7ef4b',
            'target-arrow-color': '#b7ef4b'
          })
        .selector('edge[type = "TANIMOTO_SIMILARITY"]')
          .style({
            'line-color': '#ad6847',
            'target-arrow-shape': 'none',
            'width': 'mapData(coef, 0, 1, 1, 10)'
          })
        .selector('edge[type = "CORRELATION"]')
          .style({
            'line-color': '#A6A5F6',
            'target-arrow-shape': 'none',
            'width': function(ele){
                var cf = Math.abs(ele.data('coef'));
                return cf * 10
            }
          })
        .selector('edge[type = "PARTIAL_CORRELATION"]')
          .style({
            'line-color': '#f3e043',
            'target-arrow-shape': 'none',
            'width': function(ele){
                var cf = Math.abs(ele.data('coef'));
                return cf * 10
            }
          })
        .selector('edge[direction = -1]')
          .style({
            'line-style': 'dotted'
          })
        .selector(':selected')
          .style({
            'background-color': 'black',
            'line-color': 'black',
            'target-arrow-color': 'black',
            'source-arrow-color': 'black'
          })
        .selector('.faded')
          .style({
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
