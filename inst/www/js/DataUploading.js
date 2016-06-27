$(document).ready(function(){//wait untile the document is fully loaded.
var d;// This is unchangable untill modify on the experimental design part.
var D;
var DATA;
var eData;
var pData;
var fData;
var why_not_able;
var selected_phenotype_by_check =[];
var selected = [];
var e_after_sample_normalization = [];
var medianFoldChange = [];
var log_trans = [];
var cube_trans = [];
var auto_scaling = [];var pareto_scaling = [];var range_scaling = [];
var sample_index;
var mTICdid = Loessdid = medFCdid = BatchMediandid = false;
var mTIC = Loess = medFC = BatchMedian = [];
var mTIC_raw = Loess_raw = medFC_raw = BatchMedian_raw = [];
var e_ori = p_ori = [];
var session111;



//var first_time_remove = 0;
// load data.
$("#inputUploadData").on("change", function(){
      var req=ocpu.call("stat_load_data",{
        file: $("#inputUploadData")[0].files[0]
      },function(session){
        session.getObject(function(obj){
          DATA = obj;
          D = obj;
          d = obj;
          eData = obj.expression;
          fData = obj.feature;
          pData = obj.phenotype;
          pDataTable = drawTable('#View_pData',obj.phenotype);
          fDataTable = drawTable('#View_fData',obj.feature);
          var req2 = ocpu.call("stat_summary_data",{
            DATA:D
          },function(session2){
              session2.getObject(function(obj2){
                $("#Sample_times_Metabolites").text(obj2.number_of_sample+" X "+obj2.number_of_feature);
                $("#number_of_sample").text(obj2.number_of_sample);
                $("#number_of_feature").text(obj2.number_of_feature);
                $("#EFP").text(obj2.ncol_of_p + " and " + obj2.ncol_of_f);
                pData_column = obj2.column_names_of_pData;
                why_not_able = obj2.why_not_able;
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#independent_factor");
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#repeated_factor");
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#confounding_factor");
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#PCA_color");
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#Donut_color");
                summaryPhenotype("#Summary_Phenotype_Data",obj2.column_names_of_pData,obj2.pData_columns_num);
                summaryPhenotype("#Summary_Feature_Data",obj2.column_names_of_fData,obj2.fData_columns_num);
                e_after_sample_normalization = e_after_transformation = e_after_scaling = eData;
                plotPCAScore(e1 = eData, f1 = fData, p1 = pData);
              });
          });
        });
        session.getFile("messages.txt", function(text){
          successmsg(text);
        });

      }).fail(function(error){
      errormsg(1 + error.responseText);
    });

});// load data.

$("#Submit_To_Statistics").click(function(){
  var loadSpinner = showSpinner();

  // change the DATA. Remove the outlier.
  var req=ocpu.call("stat_rm_sample",{
    e:D.expression,p:D.phenotype,f:D.feature,sample_index:$("#Samples_To_Be_Deleted").val().split(',')
  },function(session){
    session.getObject(function(obj){
      DATA = obj;
      eData = obj.expression;
      fData = obj.feature;
      pData = obj.phenotype;
    var req = ocpu.call("stat_Study_Design",{
    DATA:DATA,
    between_factor:$("#independent_factor").val(),
    within_factor:$("#repeated_factor").val()
    },function(session2){
  session2.getStdout(function(outtxt){
   $("#Sample_Size_Table").text(outtxt);
  });
    });

    var req=ocpu.call("stat_summary_data",{
    DATA:DATA
  },function(session3){
    session3.getObject(function(obj2){
      $("#number_of_sample").text(obj2.number_of_sample);
          $("#Sample_times_Metabolites").text(obj2.number_of_sample+" X "+obj2.number_of_feature);
    });
  });

  var str = $("#Normalization_To_Be_Applied").val();
  console.log(str);
  var res = str.split(";");
  samplenormalization_method = res[0];
  datatransformation_method = res[1];
  datascaling_method = res[2];
  console.log(samplenormalization_method+datatransformation_method+datascaling_method);

  var req = ocpu.call("stat_norm",{
    e : D.expression,f : D.feature,p : D.phenotype,
  sample_index:$("#sample_to_be_removed").val().split(','),
  mTICdid : mTICdid,Loessdid:Loessdid,medFCdid:medFCdid,BatchMediandid:BatchMediandid,
  mTIC:mTIC,Loess:Loess,medFC:medFC,BatchMedian:BatchMedian,
  sample_normalization : samplenormalization_method,data_transformation:datatransformation_method,data_scaling:datascaling_method
  },function(session_norm){
    session_norm.getObject(function(obj){

    eData = obj.expression;
    fData = obj.feature;
    pData = obj.phenotype;
e_ori = obj.expression_only_rm_outlier;
p_ori = obj.phenotype_only_rm_outlier;
console.log(Object.values(eData[0])[0]);

    var req=ocpu.call("hypo_test",{
    e:eData,
    f:fData,
    p:pData,
    e_ori:e_ori,
    p_ori:p_ori,
    independent_factor_name:$("#independent_factor").val(),
    repeated_factor_name:$("#repeated_factor").val()
  },function(session4){
              hideSpinner(loadSpinner);
              session4.getObject(function(obj3){
              Stat_Result = drawTable('#Statistics_Result',obj3);
               });
              download_address = session4.getLoc() + "R/.val/csv";
              $("#download_statistical_result_button").empty();
              var r= $('<a download = "file.csv" href='+download_address+' class="btn btn-primary btn-lg active" role="button">Download Statistical Analysis Result</a>');
              $("#download_statistical_result_button").append(r);
              var r_session = session4.getLoc();

            var res_url = r_session+"files/colnames.json";
            var targeturl = "/similarity.html?varname="
            console.log(res_url);
            $.getJSON(res_url)
            		.success (function (response) {
            		  console.log(r_session);
            		var test_arr = Object.keys(response[0]).map(function (x) { if (x.match("_vs_")) {return x}}).filter(function(n){ return n != undefined }).map(function (y)                 {
            		    $("#testlist").append('<li> <a class = "btn btn-primary" href="'+targeturl+y+'" target="_blank">'+y+' </a></li>');
            		});
            		console.log(test_arr);
            });






    });
  });








  });
  });


    });










});

$(".Thefactor").on("change",function(){
    var req = ocpu.call("stat_Study_Design",{
    DATA:D,
    between_factor:$("#independent_factor").val(),
    within_factor:$("#repeated_factor").val()
    },function(session){
  session.getStdout(function(outtxt){
            $("#Sample_Size_Table").text(outtxt);
        });
    });
})




var PCA_score_plot = Plotly.d3.select('#stat_PCA_plot')
    .style({
        width: 100 + '%',
        height: 100 + '%'
    }).node();



$("#PCA_color").change(function(){
  plotPCAScore(e1 = eData, f1 = fData, p1 = pData);
});
$("#Donut_color").change(function(){
  $("#morris-donut-chart").html("");
  plotDonut(e1 = eData, f1 = fData, p1 = pData,selected);
});
// functions
function print_info_of_selected(){
var text="";

}


function listSelector(full_options,why_not_able,id,furtherlimit){
  // why_not_able = ["numeric","",]
    // selected_options = ["disabled","",...]
var selector = '';
if(furtherlimit==null){
for(var i = 0;i<full_options.length;i++){
  if(why_not_able[i].length<2){
       selector += "<option data-subtext= "+ why_not_able[i]+">" + full_options[i] + "</option>";
  }else{
    selector += "<option data-subtext="+ why_not_able[i]+" disabled>" + full_options[i] + "</option>";
  }
}
}else{
  for(var i = 0;i<full_options.length;i++){
  if(why_not_able[i].length<2){
       selector += "<option data-subtext= "+ why_not_able[i]+">" + full_options[i] + "</option>";
  }else{
    selector += "<option data-subtext="+ why_not_able[i]+" disabled>" + full_options[i] + "</option>";
  }

}
}
$(id).html(selector).selectpicker('refresh');
}

function plotPCAScore(e1,f1,p1){
    var loadSpinner = showSpinner();
  var rep=ocpu.call("stat_PCA_plot",{
        e:e1,
        f:f1,
        p:p1,
        color:$('#PCA_color').val(),
        sample_information_selection:selected_phenotype_by_check
      },function(session){
session.getObject(function(obj){

Plotly.newPlot(PCA_score_plot, eval(obj.data[0]),JSON.parse(obj.layout[0]));
     hideSpinner(loadSpinner);

PCA_score_plot.on('plotly_selected',function(eventData){
  selected = [];
  eventData.points.forEach(function(pt){
    selected.push(pt.x);
  });
  var rep=ocpu.call("stat_get_index_from_PCA",{
    e:e1,
    p:p1,
    f:f1,
    selected_sample:selected
  },function(session){
    session.getObject(function(obj){
  sample_index = obj.result;
  $("#sample_selected").text(sample_index);
    });
  });

$("#morris-donut-chart").html("");
plotDonut(e1,f1,p1,selected);
});});}).fail(function(jqXHR){errormsg(1 + jqXHR.responseText);});}




function plotDonut(e1,f1,p1,select){
  var rep=ocpu.call("stat_Donut_plot",{
e:e1,
f:f1,
        p:p1,
        p_column:$('#Donut_color').val(),
        selected_sample : select
  },function(session){
    session.getObject(function(obj){
Morris.Donut({
        element: 'morris-donut-chart',
        data: eval(obj.data[0]),
        resize: true
});
    });

  });

}




summaryPhenotype = function(id,columns,columns_num){
var text = '<ul class="list-group">';

for(var i = 0;i<columns.length;i++){
  if(i === 0){
       text += '<li class="list-group-item"><span class="badge">'+columns_num[i] +'</span> '+columns[i]+'<div style="float:left"><input class="toggleone" name="checkPhenotype" checked type="checkbox" disabled readonly></div></li>';
  }else{
       text += '<li class="list-group-item"><span class="badge">'+columns_num[i] +'</span> '+columns[i]+'<div style="float:left"><input class="toggleone" name="checkPhenotype" checked type="checkbox"></div></li>';
  }

}
text = text + '</ul>';
$(id).html(text);
$('.toggleone').bootstrapToggle({
  size:"mini",
  onstyle:"default" ,
  offstyle:"default",
  on:"<i class='fa fa-check-circle'></i>",
  off:"<i class='fa fa-times-circle'></i>"
}
);
};



$(document).on("change","input[name='checkPhenotype']",function(){
  for(var i=0;i<pData_column.length;i++){
       if(document.getElementsByName("checkPhenotype")[i].checked!==true){
             selected_phenotype_by_check[i] = false;
         why_not_able[i] = why_not_able[i]+" deleted";
       }else{
                      selected_phenotype_by_check[i] = true;
         why_not_able[i] = why_not_able[i].replace(' deleted','');
       }
     }
     listSelector(pData_column,why_not_able,"#independent_factor");
     listSelector(pData_column,why_not_able,"#repeated_factor");
     listSelector(pData_column,why_not_able,"#confounding_factor");
     listSelector(pData_column,why_not_able,"#PCA_color");
     listSelector(pData_column,why_not_able,"#Donut_color");
});


window.onresize = function() {
    Plotly.Plots.resize(PCA_score_plot);
};



function successmsg(text){
  $("#success").empty().append('<div class="alert alert-success alert-dismissable"><a href="#" class="close" data-dismiss="alert">&times;</a>' + text + '</div>');
}
function errormsg(text){
  $("#error").empty().append('<div class="alert alert-danger alert-dismissable"><a href="#" class="close" data-dismiss="alert">&times;</a>' + text + '</div>');
}

// Other Utility

// after click remove_sample, some samples would be removed for visualization.
$("#remove_sample").click(function(){
  var req = ocpu.call("stat_rm_sample",{
    e:D.expression,p:D.phenotype,f:D.feature,sample_index:$("#sample_to_be_removed").val().split(',')
  },function(session){
    session.getObject(function(obj){
apply_normalization();
$('#updatePCA').prop("disabled", false); // Element(s) are now enabled.
    });
  });
});

// Normalization


var samplenormalization_method;
var datatransformation_method;
var datascaling_method;



apply_normalization = function(){
    var loadSpinner = showSpinner();


var radios1 = document.getElementsByName('samplenormalization');
for (var i = 0, length = radios1.length; i < length; i++) {
    if (radios1[i].checked) {
      samplenormalization_method = ["None","mTIC","Loess","Median Fold Change", "Batch Median"][i];
    }
}

var radios2 = document.getElementsByName('datatransformation');
for (var i = 0, length = radios2.length; i < length; i++) {
    if (radios2[i].checked) {
      datatransformation_method = ["None","log","Cube Root"][i]
    }
}

var radios3 = document.getElementsByName('datascaling');
for (var i = 0, length = radios3.length; i < length; i++) {
    if (radios3[i].checked) {
      datascaling_method = ["None","Auto","Pareto","Range"][i]
    }
}


var req=ocpu.call("stat_norm",{
  e : D.expression,f : D.feature,p : D.phenotype,
  sample_index:$("#sample_to_be_removed").val().split(','),
  mTICdid : mTICdid,Loessdid:Loessdid,medFCdid:medFCdid,BatchMediandid:BatchMediandid,
  mTIC:mTIC,Loess:Loess,medFC:medFC,BatchMedian:BatchMedian,
  sample_normalization : samplenormalization_method,data_transformation:datatransformation_method,data_scaling:datascaling_method
},function(session){

  session.getObject(function(obj){

    mTICdid = obj.mTICdid; Loessdid = obj.Loessdid; medFCdid = obj.medFCdid; BatchMediandid = obj.BatchMediandid;
    mTIC = obj.mTIC; Loess = obj.Loess; medFC = obj.medFC; BatchMedian = obj.BatchMedian;
    eData = obj.expression;
    fData = obj.feature;
    pData = obj.phenotype;

   hideSpinner(loadSpinner);
  });
});

};





$(".normalization").change(function(event){

   $('#updatePCA').prop("disabled", false); // Element(s) are now enabled.
  apply_normalization();
});





$('#updatePCA').click(function(){
  plotPCAScore(e1 = eData, f1 = fData, p1 = pData);
  $('#updatePCA').prop("disabled", true); // Element(s) are now disabled.
});



Object.values = function(object) {
  var values = [];
  for(var property in object) {
    values.push(object[property]);
  }
  return values;
}














});
