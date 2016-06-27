$(document).ready(function(){
  var selected=[];
var data_temp = {};
var PCA_score_plot = Plotly.d3.select('#tester')
    .style({
        width: 55 + '%',
        height: 100 + '%'
    }).node();
var HIST_Plot = Plotly.d3.select("#hists")
      .style({
        width: 40 + "%",
        height: 100 + "%"
      }).node();
var BOX_Plot = Plotly.d3.select("#boxplots")
      .style({
        width: 100 + "%",
        height: 100 + "%"
      }).node();
var url = getURLVars('rsess')+"R/.val/json";
$.ajax({
   url : url,
   type: 'GET',
   dataType: 'json',
   success: function (data) {
data_temp = data;//for #color on change.
listSelector(data.factor_name,"#color");
listSelector(data.phenotype_colnames,"#select_infor_needed");
plotPCAScore(data);
plotHIST(data);
plotBOXPLOT(data);
},error: function(){console.log("Ajax error: Cannot get data");}
 });
$(document).on("change","#color",function(){
plotPCAScore(data_temp);
plotBOXPLOT(data_temp);
});
function listSelector(options,id){
var selector = '';
for(var i = 0;i<options.length;i++){
   selector += "<option>" + options[i] + "</option>";
}
$(id).html(selector).selectpicker('refresh');;
}

function plotPCAScore(data){
    var selection_temp;
  if($("#select_infor_needed").val()===null){
   selection_temp = data.factor_name;
    }else{
   selection_temp = $("#select_infor_needed").val();
    }
        var rep=ocpu.call("test",{
        e:data.dataset.expression,
        p:data.dataset.phenotype,
        f:data.dataset.feature,
        color:$('#color').val(),
        sample_information_selection:"Sample_name,"+selection_temp.join(",")
      },function(session){
     session.getObject(function(obj){
      Plotly.newPlot(PCA_score_plot, eval(obj.data[0]),JSON.parse(obj.layout[0]));
PCA_score_plot.on('plotly_selected',function(eventData){
  selected = [];
  eventData.points.forEach(function(pt){
    selected.push(pt.x);
  });
  plotHIST(data);
  plotBOXPLOT(data);
});});}).fail(function(jqXHR){errormsg(1 + jqXHR.responseText);});}

function plotHIST(data){
  var selection_temp;
  if($("#select_infor_needed").val()===null){
   selection_temp = data.factor_name;
    }else{
   selection_temp = $("#select_infor_needed").val();
    }
    //console.log("Sample_name,"+selection_temp.join(","));
  var rep = ocpu.call("histo",{
    p:data.dataset.phenotype,
    e:data.dataset.expression,
    sample_information_selection: selection_temp,
    x:selected
  },function(session){
    session.getObject(function(obj){
    Plotly.newPlot(HIST_Plot, eval(obj.data[0]),JSON.parse(obj.layout[0]));
    });
  }).fail(function(jqXHR){
          errormsg(2 + jqXHR.responseText);
          });
}

function plotBOXPLOT(data){
  var rep=ocpu.call("boxp",{
    e:data.dataset.expression,
    p:data.dataset.phenotype,
    color:$('#color').val(),
    selected:selected
  },function(session){
    session.getObject(function(obj){
      Plotly.newPlot(BOX_Plot,eval(obj.data[0]),JSON.parse(obj.layout[0]));
    });
  }).fail(function(jqXHR){
    errormsg(3 + jqXHR.responseText);
  });
}

$(document).on("change","#select_infor_needed",function(){
plotHIST(data_temp);
});
$(document).on("click","#select_infor_needed",function(){
plotHIST(data_temp);
});

window.onresize = function() {
    Plotly.Plots.resize(PCA_score_plot);
    Plotly.Plots.resize(HIST_Plot);
    Plotly.Plots.resize(BOX_Plot);
};







  function errormsg(text){
    $("#errordiv").empty().append('<div class="alert alert-danger alert-dismissable"><a href="#" class="close" data-dismiss="alert">&times;</a>' + text + '</div>');
  }

});
