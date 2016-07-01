$(document).ready(function(){//wait untile the document is fully loaded.
var d;// This is unchangable untill modify on the experimental design part.
var D;
var DATA;
var eData;
var pData;
var fData;
var why_not_able;
var selected_phenotype_by_check = [];
var selected_feature_by_check = [];
var selected = [];
var e_after_sample_normalization = [];
var medianFoldChange = [];
var log_trans = [];
var cube_trans = [];
var auto_scaling = [];var pareto_scaling = [];var range_scaling = [];
var sample_index;
var mTICdid = Loessdid = medFCdid = BatchMediandid = false;
var mTIC = Loess = medFC = BatchMedian = [];
var mTIC_raw = []; var Loess_raw = []; var medFC_raw = []; var BatchMedian_raw = [];
var e_ori = []; var p_ori = [];
var session111;
var fData_column;
var pData_column;
var modified_dataset_download_address = [];
var download_norm_data_adress = [];

// load data.
$("#inputUploadData").on("change", function(){
  var loadSpinner = showSpinner();
      var req=ocpu.call("stat_load_data",{
        file: $("#inputUploadData")[0].files[0],
        sheetIndex: $("#stat_load_data_para_submit").val()
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
                $("#EFP").text(obj2.ncol_of_p + " / " + obj2.ncol_of_f);
                pData_column = obj2.column_names_of_pData;
                fData_column = obj2.column_names_of_fData;
                why_not_able = obj2.why_not_able;
                decidenormalizationability(obj2.column_names_of_pData);
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#independent_factor");
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#repeated_factor");
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#confounding_factor");
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#PCA_color");
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#Donut_color");
                summaryPhenotype("#Summary_Phenotype_Data",obj2.column_names_of_pData,obj2.pData_columns_num);
                summaryFeature("#Summary_Feature_Data",obj2.column_names_of_fData,obj2.fData_columns_num);
                e_after_sample_normalization = e_after_transformation = e_after_scaling = eData;
                $("#showafterupload").collapse('show');
                $("#visualization_body").collapse('show');
              plotPCAScore(e1 = eData, f1 = fData, p1 = pData);
                          hideSpinner(loadSpinner);
              });
          });
          var req3 = ocpu.call("stat_get_modified_data",{
            DATA:D
          },function(session3){
            modified_dataset_download_address = session3.getLoc() + "R/.val/csv";
         session.getFile("messages.txt", function(text){
          if(text.length>10){
           warningmsg(text);
          }else{
           successmsg(text);
          }

$("#UploadData").collapse('toggle');
$("#StudyDesign").collapse('show');
$("#ViewData").collapse('show');

        });

          });

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


  var str = $("#Normalization_To_Be_Applied").val();

  var res = str.split(";");
  samplenormalization_method = res[0];
  datatransformation_method = res[1];
  datascaling_method = res[2];


  var req = ocpu.call("stat_norm",{
    e : DATA.expression,f : DATA.feature,p : DATA.phenotype,
  sample_index:$("#Samples_To_Be_Deleted").val().split(','),
  mTICdid : mTICdid,Loessdid:Loessdid,medFCdid:medFCdid,BatchMediandid:BatchMediandid,
  mTIC:mTIC,Loess:Loess,medFC:medFC,BatchMedian:BatchMedian,
  sample_normalization : samplenormalization_method,data_transformation:datatransformation_method,data_scaling:datascaling_method,
  selected_phenotype_by_check:selected_phenotype_by_check, selected_feature_by_check:selected_feature_by_check
  },function(session_norm){
    session_norm.getObject(function(obj){

    eData = obj.expression;
    fData = obj.feature;
    pData = obj.phenotype;
    e_ori = obj.expression_only_rm_outlier;
    p_ori = obj.phenotype_only_rm_outlier;

    var req=ocpu.call("stat_hypo_test",{
    e:eData,
    f:fData,
    p:pData,
    e_ori:e_ori,
    p_ori:p_ori,
    independent_factor_name:$("#independent_factor").val(),
    repeated_factor_name:$("#repeated_factor").val()
  },function(session4){
    $("#StudyDesign").collapse('hide');
    $("#ViewData").collapse('hide');
    $("#visualization_body").collapse('hide');
    $("#Statistics_Result_panel").collapse('show');



              hideSpinner(loadSpinner);
              session4.getObject(function(obj3){

                stat_result = obj3;

              Stat_Result = drawTable('#Statistics_Result',obj3);
               });
              download_address = session4.getLoc() + "R/.val/csv";
              $("#download_statistical_result_button").empty();
              var r= $('<a download = "file.csv" href='+download_address+' class="btn btn-primary btn-lg active" role="button">Download Statistical Analysis Result</a>');
              $("#download_statistical_result_button").append(r);
              var rsession = session4.getLoc();

            var res_url = rsession +"files/colnames.json";
            var targeturl = "/ocpu/library/mETABOX/www/similarity.html?varname="
            $.getJSON(res_url)
            		.success (function (response) {
            		if(response.indexOf("PubChem_id")!==-1 ) {
            		  var test_arr = response.map(function (x) { if (x.match("_vs_")) {return x}}).filter(function(n){ return n != undefined }).map(function (y)                 {
            		    $("#testlist").append('<li> <a class = "btn btn-primary" href="'+targeturl+y+'&rsess='+rsession+'&idtype=compound" target="_blank">'+y+' </a></li>');
            		  });
            		} else if (response.indexOf("ensembl")!==-1 ) {
            		  var test_arr = response.map(function (x) { if (x.match("_vs_")) {return x}}).filter(function(n){ return n != undefined }).map(function (y)                 {
            		    $("#testlist").append('<li> <a class = "btn btn-primary" href="'+targeturl+y+'&rsess='+rsession+'&idtype=gene" target="_blank">'+y+' </a></li>');
            		  });

            		} else if (response.indexOf("uniprot")!==-1 ) {
            		  var test_arr = response.map(function (x) { if (x.match("_vs_")) {return x}}).filter(function(n){ return n != undefined }).map(function (y)                 {
            		    $("#testlist").append('<li> <a class = "btn btn-primary" href="'+targeturl+y+'&rsess='+rsession+'&idtype=protein" target="_blank">'+y+' </a></li>');
            		  });

            		} else {
            		  alter("No ids found in the table. Cannot proceed to the next step for MetaBox workflow.");

            		}
            });


    });
  });
  });
  });
    });

});


$(".Thefactor").on("change",function(){
    var loadSpinner = showSpinner();
    var req = ocpu.call("stat_Study_Design",{
    DATA:D,
    between_factor:$("#independent_factor").val(),
    within_factor:$("#repeated_factor").val()
    },function(session){
  session.getStdout(function(outtxt){
            $("#Sample_Size_Table").text(outtxt);
                          hideSpinner(loadSpinner);
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



function decidenormalizationability(column_names_of_pData){

if($.inArray('Batch', column_names_of_pData) == -1){
    document.getElementById("samplenormalizationBatchMedian").disabled = true;
    document.getElementById("samplenormalizationloess").disabled = true;
}
if($.inArray('Sample_specific_weight', column_names_of_pData) == -1){
    document.getElementById("samplenormalizationSample_specific").disabled = true;
}

}



function plotPCAScore(e1,f1,p1){
    var loadSpinner = showSpinner();
$(".normalization").prop('disabled', true);


  var rep=ocpu.call("stat_PCA_plot",{
        e:e1,
        f:f1,
        p:p1,
        color:$('#PCA_color').val(),
        sample_information_selection:selected_phenotype_by_check
      },function(session){



session.getObject(function(obj){

Plotly.newPlot(PCA_score_plot, eval(obj.data[0]),JSON.parse(obj.layout[0]));

$(".normalization").prop('disabled', false);
decidenormalizationability(pData_column);

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
  if(columns[i] === "phenotype_index" || columns[i] === "sampleID" ||columns[i] === "Batch" ||columns[i] === "Sample_specific_weight"){
       text += '<li class="list-group-item"><span class="label label-info" style="float:right">'+columns_num[i] +'</span> '+columns[i]+'<div style="float:left"><input class="toggleone" name="checkPhenotype" checked type="checkbox" disabled readonly></div></li>';
  }else{
       text += '<li class="list-group-item"><span class="label label-info" style="float:right">'+columns_num[i] +'</span> '+columns[i]+'<div style="float:left"><input class="toggleone" name="checkPhenotype" checked type="checkbox"></div></li>';
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

summaryFeature = function(id,columns,columns_num){
var text = '<ul class="list-group">';

for(var i = 0;i<columns.length;i++){
  if(columns[i] === "feature_index" || columns[i] === "KnownorUnknown" ){
       text += '<li class="list-group-item"><span class="label label-info" style="float:right">'+columns_num[i] +'</span> '+columns[i]+'<div style="float:left"><input class="toggleone" name="checkFeature" checked type="checkbox" disabled readonly></div></li>';
  }else{
    text += '<li class="list-group-item"><span class="label label-info" style="float:right">'+columns_num[i] +'</span> '+columns[i]+'<div style="float:left"><input class="toggleone" name="checkFeature" checked type="checkbox"></div></li>';
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



$(document).on("change","input[name='checkFeature']",function(){
  for(var i=0;i<fData_column.length;i++){
       if(document.getElementsByName("checkFeature")[i].checked!==true){
             selected_feature_by_check[i] = false;

       }else{
             selected_feature_by_check[i] = true;
       }
     }

  if(typeof stat_result !== 'undefined'){
    var loadSpinner = showSpinner();
    var req=ocpu.call("stat_delete_result_column",{
      stat_result:stat_result,
      selected_feature_by_check:selected_feature_by_check
    },function(session){


              session.getObject(function(obj){
console.log(obj);
              $("#Statistics_Result").empty()
             Stat_Result = drawTable('#Statistics_Result',obj);
               });




              download_address = session.getLoc() + "R/.val/csv";
              $("#download_statistical_result_button").empty();
              var r= $('<a download = "file.csv" href='+download_address+' class="btn btn-primary btn-lg active" role="button">Download Statistical Analysis Result</a>');
              $("#download_statistical_result_button").append(r);

 hideSpinner(loadSpinner);
    })
  }

});





window.onresize = function() {
    Plotly.Plots.resize(PCA_score_plot);
};



function successmsg(text){
  $("#message").empty().append('<div class="alert alert-success alert-dismissable"><a href="#" class="close" data-dismiss="alert">&times;</a>' + text+ + '</div>');
}
function warningmsg(text){
  $("#message").empty().append('<div class="alert alert-warning alert-dismissable"><a href="#" class="close" data-dismiss="alert">&times;</a>' + text + '<a download = "file.csv" href='+modified_dataset_download_address+' role="button">downloading the data set.</a></div>');
}




function errormsg(text){
  $("#message").empty().append('<div class="alert alert-danger alert-dismissable"><a href="#" class="close" data-dismiss="alert">&times;</a>' + text + '</div>');
}

// Other Utility

// after click remove_sample, some samples would be removed for visualization.
$("#remove_sample").click(function(){
var loadSpinner = showSpinner();



  var req = ocpu.call("stat_rm_sample",{
    e:D.expression,p:D.phenotype,f:D.feature,sample_index:$("#sample_to_be_removed").val().split(',')
  },function(session){
    session.getObject(function(obj){
      apply_normalization();

//$('#updatePCA').prop("disabled", false); // Element(s) are now enabled.

// the mTICdid and others would be false because data changed.
mTICdid = Loessdid = medFCdid = BatchMediandid = false;
// hideSpinner(loadSpinner);
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
      samplenormalization_method = ["None","Sample_specific","mTIC","Loess","Median Fold Change", "Batch Median"][i];
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
  sample_normalization : samplenormalization_method,data_transformation:datatransformation_method,data_scaling:datascaling_method,
  selected_phenotype_by_check:selected_phenotype_by_check,selected_feature_by_check:selected_feature_by_check
},function(session){

  session.getObject(function(obj){

    mTICdid = obj.mTICdid; Loessdid = obj.Loessdid; medFCdid = obj.medFCdid; BatchMediandid = obj.BatchMediandid;
    mTIC = obj.mTIC; Loess = obj.Loess; medFC = obj.medFC; BatchMedian = obj.BatchMedian;
    eData = obj.expression;
    fData = obj.feature;
    pData = obj.phenotype;

    var req2=ocpu.call("stat_get_modified_data",{
      DATA:obj
    },function(session2){
        download_norm_data_adress = session2.getLoc() + "R/.val/csv";
      $("#download_norm_data").empty();
              var r= $('<a download = "file.csv" href='+download_norm_data_adress+' class="btn btn-success btn-sm btn-outline  btn-circle" role="button"><i class="glyphicon glyphicon-download-alt"></i></a>');

              $("#download_norm_data").append(r);
    })


  plotPCAScore(e1 = eData, f1 = fData, p1 = pData);
   hideSpinner(loadSpinner);
  });
});

};





$(".normalization").change(function(event){

  apply_normalization();
});







Object.values = function(object) {
  var values = [];
  for(var property in object) {
    values.push(object[property]);
  }
  return values;
}








// download boxplots.












});
