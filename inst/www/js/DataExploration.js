$(document).ready(function(){

var data_temp = {};
var url = getURLVars('rsess')+"R/.val/json";
$.ajax({
   url : url,
   type: 'GET',
   dataType: 'json',
   success: function (data) {
      $.each(data.factor_name,function(i,val){
        if(val){
           $("<option>").text(val).appendTo("#color");
        }
      });
data_temp = data;
makeplot(data);
   },error: function(){console.log("Ajax error: Cannot get data");}
 });


$(document).on("change","#color",function(){
//$("#demo").text(data_temp===undefined);
makeplot(data_temp);
});
function makeplot(data){
        var rep=ocpu.call("test",{
        e:data.dataset.expression,
        p:data.dataset.phenotype,
        f:data.dataset.feature,
        color:$('#color').val(),
        pie_options:"time"
      },function(session){

     session.getObject(function(obj){
       $("#demo").text(obj.scatter);
       if(obj.scatter.length===1){
        var test = document.getElementById('tester');
      Plotly.newPlot(test, eval(obj.pie[0]));
       }
      if(obj.pie.length===1){
     var test2 = document.getElementById('tester2');
      Plotly.newPlot(test2, eval(obj.scatter[0]));
      }
     });

/*var data = [{
  values: [19, 26, 55],
  labels: ['Residential', 'Non-Residential', 'Utility'],
  type: 'pie'
}];

var layout = {
  height: 400,
  width: 500
};

Plotly.newPlot('myDiv', data, layout);*/





      }).fail(function(jqXHR){
          errormsg(1 + jqXHR.responseText);
          });
}
/*test.on('plotly_selected', function(eventData) {
  var values = [];
  //var y = [];

  var temp = [];
  //for(var i = 0; i < N; i++) colors.push(color1Light);

  //console.log(eventData.points);

  eventData.points.forEach(function(pt) {
    //x.push(pt.x);
    //y.push(pt.y);
    temp[pt.pointNumber] = true;
  });
 //values
  Plotly.restyle(test2, {
    values:values,
    //xbins: {}
  }, [0]);

  //Plotly.restyle(test, 'marker.color', [colors], [0]);
});*/


/*var trace1 = {
  y: [5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5],
  mode: 'markers',
  marker: {
    size: 40,
    color: ["red","blue","red", "blue","red" ,"blue","red","blue","red","blue","red","blue","red","blue","red","blue","red","blue","red","blue","red","blue","red","blue","red","blue","red","blue","red","blue","red","blue","red","blue","red","blue","red","blue","red","blue"]
  }
};

var data = [trace1];

var layout = {
  title: 'Scatter Plot with a Color Dimension'
};

Plotly.newPlot('myDiv2', data, layout);*/


var graphDiv = document.getElementById('graph');
var N = 1000;
var color1 = '#7b3294';
var color1Light = '#c2a5cf';
var colorX = '#ffa7b5';
var colorY = '#fdae61';

function randomArray() {
  var out = new Array(N);
  for(var i = 0; i < N; i++) {
    out[i] = Math.random();
  }
  return out;
}
var x = randomArray();
var y = randomArray();

Plotly.plot(graphDiv, [{
  type: 'scatter',
  mode: 'markers',
  x: x,
  y: y,
  xaxis: 'x',
  yaxis: 'y',
  name: 'random data',
  marker: {color: color1, size: 10}
}, {
  type: 'histogram',
  x: x,
  xaxis: 'x2',
  yaxis: 'y2',
  name: 'x coord dist.',
  marker: {color: colorX}
}, {
  type: 'histogram',
  x: y,
  xaxis: 'x3',
  yaxis: 'y3',
  name: 'y coord dist.',
  marker: {color: colorY}
}], {
  title: 'Lasso around the scatter points to see sub-distributions',
  dragmode: 'lasso',
  xaxis: {
    zeroline: false,
  },
  yaxis: {
    domain: [0.55, 1],
  },
  xaxis2: {
    domain: [0, 0.45],
    anchor: 'y2',
  },
  yaxis2: {
    domain: [0, 0.45],
    anchor: 'x2'
  },
  xaxis3: {
    domain: [0.55, 1],
    anchor: 'y3'
  },
  yaxis3: {
    domain: [0, 0.45],
    anchor: 'x3'
  }
});

graphDiv.on('plotly_selected', function(eventData) {
  var x = [];
  var y = [];

  var colors = [];
  for(var i = 0; i < N; i++) colors.push(color1Light);

  console.log(eventData.points[0]);

  eventData.points.forEach(function(pt) {
    x.push(pt.x);
    y.push(pt.y);
    colors[pt.pointNumber] = color1;
  });

  Plotly.restyle(graphDiv, {
    x: [x, y],
    xbins: {}
  }, [1, 2]);

  Plotly.restyle(graphDiv, 'marker.color', [colors], [0]);
});






  function errormsg(text){
    $("#errordiv").empty().append('<div class="alert alert-danger alert-dismissable"><a href="#" class="close" data-dismiss="alert">&times;</a>' + text + '</div>');
  }

});
