//@function draw circle packing
//@param meshpath path to json string
//@reference Mike Bostock - http://bl.ocks.org/mbostock/7607535
function drawCirclePack(meshpath){
    var margin = 10, diameter = 800;
    var color = d3.scale.linear()
        .domain([-1, 5])
        .range(["hsl(182,80%,80%)", "hsl(248,30%,40%)"])
        .interpolate(d3.interpolateHcl);
    var pack = d3.layout.pack()
        .padding(2)
        .size([diameter - margin, diameter - margin])
        .value(function(d) { return d.size; })
    var svg = d3.select("body").append("svg")
        .attr("width", diameter)
        .attr("height", diameter)
      .append("g")
        .attr("transform", "translate(" + diameter / 2 + "," + diameter / 2 + ")");
    d3.json(meshpath, function(error, root) {
      if (error) throw error;
      var focus = root,
          nodes = pack.nodes(root),
          view;
      var circle = svg.selectAll("circle")
          .data(nodes)
        .enter().append("circle")
          .attr("class", function(d) { return d.parent ? d.children ? "node" : "node node--leaf" : "node node--root"; })
          .style("fill", function(d) { return d.children ? color(d.depth) : null; })
          .on("click", function(d) {   if (focus !== d) zoom(d), d3.event.stopPropagation();});
      var lab = "";
      var text = svg.selectAll("text")
          .data(nodes)
        .enter().append("text")
          .attr("class", "label")
          .style("fill-opacity", function(d) { return d.parent === root ? 1 : 0; })
          .style("display", function(d) { return d.parent === root ? "inline" : "none"; })
          .text(function(d) {
            if (d.member[0].length<=25) {
              lab = d.MeshName+": "+d.member[0];
            }else{
              lab = d.MeshName+": "+d.member[0].slice(0, 25)+" ...";
            }
            return lab;
          });
      var node = svg.selectAll("circle,text");
      d3.select("body")
          .style("background", color(-1))
          .on("click", function() { zoom(root); });
      zoomTo([root.x, root.y, root.r * 2 + margin]);
      function zoom(d) {
        var focus0 = focus; focus = d;
        var transition = d3.transition()
            .duration(d3.event.altKey ? 7500 : 750)
            .tween("zoom", function(d) {
              var i = d3.interpolateZoom(view, [focus.x, focus.y, focus.r * 2 + margin]);
              return function(t) { zoomTo(i(t)); };
            });
        transition.selectAll("text")
          .filter(function(d) { return d.parent === focus || this.style.display === "inline"; })
            .style("fill-opacity", function(d) { return d.parent === focus ? 1 : 0; })
            .each("start", function(d) { if (d.parent === focus) this.style.display = "inline";  })
            .each("end", function(d) { if (d.parent !== focus) this.style.display = "none"; });
      }
      function zoomTo(v) {
        var k = diameter / v[2]; view = v;
        node.attr("transform", function(d) { return "translate(" + (d.x - v[0]) * k + "," + (d.y - v[1]) * k + ")"; });
        circle.attr("r", function(d) { return d.r * k; });
      }
    });
    d3.select(self.frameElement).style("height", diameter + "px");
}

//@function draw tree
//@param meshpath path to json string
//@reference Mike Bostock - http://bl.ocks.org/mbostock/1093025
function drawTree(meshpath){
    var margin = {top: 10, right: 10, bottom: 10, left: 10},
        width = 800 - margin.left - margin.right,
        barHeight = 20,
        barWidth = width * .8;
    var i = 0,
        duration = 400,
        root;
    var tree = d3.layout.tree()
        .nodeSize([0, 20]);
    var diagonal = d3.svg.diagonal()
        .projection(function(d) { return [d.y, d.x]; });
    var svg = d3.select("body").append("svg")
        .attr("width", width + margin.left + margin.right)
      .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");
    d3.json(meshpath, function(error, flare) {
      if (error) throw error;
      flare.x0 = 0;
      flare.y0 = 0;
      update(root = flare);
    });
    function update(source) {
      // Compute the flattened node list. TODO use d3.layout.hierarchy.
      var nodes = tree.nodes(root);
      var height = Math.max(500, nodes.length * barHeight + margin.top + margin.bottom);
      d3.select("svg").transition()
          .duration(duration)
          .attr("height", height);
      d3.select(self.frameElement).transition()
          .duration(duration)
          .style("height", height + "px");
      // Compute the "layout".
      nodes.forEach(function(n, i) {
        n.x = i * barHeight;
      });
      // Update the nodesÉ
      var node = svg.selectAll("g.node")
          .data(nodes, function(d) { return d.id || (d.id = ++i); });
      var nodeEnter = node.enter().append("g")
          .attr("class", "node")
          .attr("transform", function(d) { return "translate(" + source.y0 + "," + source.x0 + ")"; })
          .style("opacity", 1e-6);
      // Enter any new nodes at the parent's previous position.
      nodeEnter.append("rect")
          .attr("y", -barHeight / 2)
          .attr("height", barHeight)
          .attr("width", barWidth)
          .style("fill", color)
          .on("click", click);
      // Format node label, show only 25 members
      var lab = "";
      nodeEnter.append("text")
          .attr("dy", 3.5)
          .attr("dx", 5.5)
          .text(function(d) {
            lab = d.MeshName+" ("+d.size+")";
            return lab;
          });
      // Transition nodes to their new position.
      nodeEnter.transition()
          .duration(duration)
          .attr("transform", function(d) { return "translate(" + d.y + "," + d.x + ")"; })
          .style("opacity", 1);
      node.transition()
          .duration(duration)
          .attr("transform", function(d) { return "translate(" + d.y + "," + d.x + ")"; })
          .style("opacity", 1)
        .select("rect")
          .style("fill", color);
      // Transition exiting nodes to the parent's new position.
      node.exit().transition()
          .duration(duration)
          .attr("transform", function(d) { return "translate(" + source.y + "," + source.x + ")"; })
          .style("opacity", 1e-6)
          .remove();
      // Update the linksÉ
      var link = svg.selectAll("path.link")
          .data(tree.links(nodes), function(d) { return d.target.id; });
      // Enter any new links at the parent's previous position.
      link.enter().insert("path", "g")
          .attr("class", "link")
          .attr("d", function(d) {
            var o = {x: source.x0, y: source.y0};
            return diagonal({source: o, target: o});
          })
        .transition()
          .duration(duration)
          .attr("d", diagonal);
      // Transition links to their new position.
      link.transition()
          .duration(duration)
          .attr("d", diagonal);
      // Transition exiting nodes to the parent's new position.
      link.exit().transition()
          .duration(duration)
          .attr("d", function(d) {
            var o = {x: source.x, y: source.y};
            return diagonal({source: o, target: o});
          })
          .remove();
      // Stash the old positions for transition.
      nodes.forEach(function(d) {
        d.x0 = d.x;
        d.y0 = d.y;
      });
    }
    // Toggle children on click.
    function click(d) {
      if (d.children) {
        d._children = d.children;
        d.children = null;
      } else {
        d.children = d._children;
        d._children = null;
      }
      update(d);
    }
    function color(d) {
        var cscale = chroma.scale(['yellow','red','#ce0000']).domain([0, 3, 6, 9]);
        var sc = cscale(Math.abs(Math.log(d.size))).hex(); // #FF7F7F
        //return d._children ? "#953C4E" : d.children ? "#CD8392" : "#EACBD2";
        return d._children ? sc : d.children ? sc : sc;
    }
}