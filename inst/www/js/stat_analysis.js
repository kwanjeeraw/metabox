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
var power_trans = [];
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
var res_url;
var rsession;
var type;
var duplicatedID;
// load data.
$("#inputUploadData").on("change", function(){
  var loadSpinner = showSpinner();
      var req=ocpu.call("stat_load_data",{
        file: $("#inputUploadData")[0].files[0],
        sheetIndex: $("#stat_load_data_para_SheetIndex").val()
       // ,from_example:$('input[name="optionsRadiosInline"]:checked').val()
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
          duplicatedID = obj.duplicatedID;
          if(duplicatedID[0]){
              document.getElementById("PCA_connect_id").innerHTML = '<div class="form-group"><label>Display Trend of Each sampleID:</label><label class="checkbox-inline pull-right" ><input type="checkbox" id = "PCA_IDtrend" checked value = "TRUE"></label></div>';
          }
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
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#logparaautoindependent_factor");
                listSelector(obj2.column_names_of_fData,null,"#boxplottitle");
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#powerparaautoindependent_factor");
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#independent_factor");
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#repeated_factor");
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#confounding_factor");
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#PCA_color");
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#Donut_color");
                summaryPhenotype("#Summary_Phenotype_Data",obj2.column_names_of_pData,obj2.pData_columns_num);
                summaryFeature("#Summary_Feature_Data",obj2.column_names_of_fData,obj2.fData_columns_num);
                e_after_sample_normalization = e_after_transformation = e_after_scaling = eData;
                $("#showafterupload").collapse('show');
                $("#draw_boxplot_div").collapse('show');
                $("#visualization_body").collapse('show');

              plotPCAScore(e1 = eData, f1 = fData, p1 = pData);
                         // hideSpinner(loadSpinner);
              });
          }).fail(function(jqXHR){errormsg(1 + jqXHR.responseText);hideSpinner(loadSpinner);});
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

$(".disableafterupload").attr("disabled", true);

$(".alertToRefresh").html("If you want to upload a new file, please refresh the page first.");


$("#StudyDesign").collapse('show');
$("#ViewData").collapse('show');

        });

          }).fail(function(jqXHR){errormsg(1 + jqXHR.responseText); hideSpinner(loadSpinner);});

        });


      }).fail(function(error){
      errormsg(1 + error.responseText);hideSpinner(loadSpinner);
    });

});// load data.
// use example data.
$("input[name='optionsRadiosInline']").on("click", function(){
  document.getElementById('submit_example_data').style.visibility = 'visible';
})

$("#submit_example_data").on("click", function(){
  var loadSpinner = showSpinner();
      var req=ocpu.call("stat_load_data",{
        file: $("#inputUploadData")[0].files[0],
        sheetIndex: $("#stat_load_data_para_SheetIndex").val(),
        from_example:$('input[name="optionsRadiosInline"]:checked').val()
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
          duplicatedID = obj.duplicatedID;

          if(duplicatedID[0]){
              document.getElementById("PCA_connect_id").innerHTML = '<div class="form-group"><label>Display Trend of Each sampleID:</label><label class="checkbox-inline pull-right" ><input type="checkbox" id = "PCA_IDtrend" checked value = "TRUE"></label></div>';
          }
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
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#logparaautoindependent_factor");
                listSelector(obj2.column_names_of_fData,null,"#boxplottitle");
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#powerparaautoindependent_factor");
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#independent_factor");
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#repeated_factor");
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#confounding_factor");
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#PCA_color");
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#Donut_color");
                summaryPhenotype("#Summary_Phenotype_Data",obj2.column_names_of_pData,obj2.pData_columns_num);
                summaryFeature("#Summary_Feature_Data",obj2.column_names_of_fData,obj2.fData_columns_num);
                e_after_sample_normalization = e_after_transformation = e_after_scaling = eData;
                $("#showafterupload").collapse('show');
                $("#draw_boxplot_div").collapse('show');
                $("#visualization_body").collapse('show');
              plotPCAScore(e1 = eData, f1 = fData, p1 = pData);
                          hideSpinner(loadSpinner);
              });
          }).fail(function(jqXHR){errormsg(1 + jqXHR.responseText); hideSpinner(loadSpinner);});
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
$(".disableafterupload").attr("disabled", true);
$(".alertToRefresh").html("If you want to upload a new file, please refresh the page (F5) first.");
$("#StudyDesign").collapse('show');
$("#ViewData").collapse('show');
        });
          }).fail(function(jqXHR){errormsg(1 + jqXHR.responseText);hideSpinner(loadSpinner);});
        });
      }).fail(function(error){
      errormsg(1 + error.responseText);hideSpinner(loadSpinner);
    });

});// load data.


$("#Submit_To_Statistics").click(function(){
    // check if stat_hypo_test ca procede or not.
console.log($("#independent_factor").val());
console.log($("#repeated_factor").val());

  if($("#independent_factor").val()===null && $("#repeated_factor").val()===null){ // must have at least one factor.
    alert("You haven't selected factor yet! Go to Study Design box to select factor first.");
  }else if($("#independent_factor").val() === $("#repeated_factor").val()){ // cannot have same for independent_factor and repeated_factor
    alert("Between Subject Factor CANNOT equal to Within Subject Factor");
  }else if(!$("#independent_factor").val()===null && !$("#repeated_factor").val()===null){ // cannot handle three or more factor cases.

    if(($("#independent_factor").val().length + $("#repeated_factor").val().length)>2){
      alert("Currently, metabox can only handle two factor case or one factor case!");
    }

  }else{
      var loadSpinner = showSpinner();
  // change the DATA. Remove the outlier.
  var req=ocpu.call("stat_rm_sample",{
    e:D.expression,p:D.phenotype,f:D.feature,sample_index : ""
  },function(session){
    session.getObject(function(obj){
      DATA = obj;
      eData = obj.expression;
      fData = obj.feature;
      pData = obj.phenotype;
  var str = $("#Normalization_To_Be_Applied").val();

  samplenormalization_method = str[0];
  datatransformation_method = str[1];
  datascaling_method = str[2];

  var req = ocpu.call("stat_norm",{
    e : DATA.expression,f : DATA.feature,p : DATA.phenotype,
  sample_index:$("#Samples_To_Be_Deleted").val().split(','),
  mTICdid : mTICdid,Loessdid:Loessdid,medFCdid:medFCdid,BatchMediandid:BatchMediandid,
  mTIC:mTIC,Loess:Loess,medFC:medFC,BatchMedian:BatchMedian,
  sample_normalization : samplenormalization_method,data_transformation:datatransformation_method,data_scaling:datascaling_method,
  //selected_phenotype_by_check:selected_phenotype_by_check, selected_feature_by_check:selected_feature_by_check,

  log_para:$('input[name="logpara"]:checked').val(),independent_factor_name_log : $('#logparaautoindependent_factor').val(),
  power_para:$('input[name="powerpara"]:checked').val(),independent_factor_name_power : $('#powerparaautoindependent_factor').val()

  },function(session_norm){

    session_norm.getObject(function(obj2){

    eData = obj2.expression;
    fData = obj2.feature;
    pData = obj2.phenotype;
    e_ori = obj2.expression_only_rm_outlier;
    p_ori = obj2.phenotype_only_rm_outlier;



    var req=ocpu.call("stat_hypo_test",{
    e:eData,
    f:fData,
    p:pData,
    e_ori:e_ori,
    p_ori:p_ori,
    e_before:D.expression,
    p_before:D.phenotype,
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

                if(typeof stat_result[0]==="string"){
                  alert(stat_result);
                }else{

                $("#continue_analysis").collapse('show');


              Stat_Result = drawTable('#Statistics_Result',obj3);
                            download_address = session4.getLoc() + "R/.val/csv";
              $("#download_statistical_result_button").empty();
              var r= $('<a download = "file.csv" href='+download_address+' class="btn btn-primary btn-lg active" role="button">Download Statistical Analysis Result</a>');
              $("#download_statistical_result_button").append(r);


              rsession = session4.getLoc();
            var res_url = rsession +"files/colnames.json";
$.getJSON(res_url).success(function(obj){

                if(obj.indexOf("PubChem")!==-1 ) {
                type = "compound";

            		} else if (obj.indexOf("ensembl")!==-1 ) {
            		  type = "ensembl";


            		} else if (obj.indexOf("uniprot")!==-1 ) {
            		  type = "uniprot";

            		}else{
            		  type = "nothing"
            		}


});
// decide if it has which compound and disable certain thing!




/*!!            var targeturl = "/ocpu/library/mETABOX/www/similarity.html?varname="
            $.getJSON(res_url)
            		.success (function (response) {
            		if(response.indexOf("PubChem")!==-1 ) {
            		  var test_arr = response.map(function (x) { if (x.match("p_value")) {return x}}).filter(function(n){ return n != undefined }).map(function (y)                 {
            		    $("#testlist").append('<li> <a class = "btn btn-primary" href="'+targeturl+y+'&rsess='+rsession+'&idtype=compound" target="_blank">'+y+' </a></li>');
            		  });
            		} else if (response.indexOf("ensembl")!==-1 ) {
            		  var test_arr = response.map(function (x) { if (x.match("p_value")) {return x}}).filter(function(n){ return n != undefined }).map(function (y)                 {
            		    $("#testlist").append('<li> <a class = "btn btn-primary" href="'+targeturl+y+'&rsess='+rsession+'&idtype=gene" target="_blank">'+y+' </a></li>');
            		  });

            		} else if (response.indexOf("uniprot")!==-1 ) {
            		  var test_arr = response.map(function (x) { if (x.match("p_value")) {return x}}).filter(function(n){ return n != undefined }).map(function (y)                 {
            		    $("#testlist").append('<li> <a class = "btn btn-primary" href="'+targeturl+y+'&rsess='+rsession+'&idtype=protein" target="_blank">'+y+' </a></li>');
            		  });

            		} else {
            		  alert("No ids found in the table. Cannot proceed to the next step for MetaBox workflow.");

            		}
            });*/
                }
               });


session4.getFile("messages_hypo_test.txt", function(text){
          if(text.length>10){
           warningmsg(text);
          }


        });

    }).fail(function(jqXHR){hideSpinner(loadSpinner);alert(jqXHR.responseText); });



  });
  }).fail(function(jqXHR){ hideSpinner(loadSpinner);alert(jqXHR.responseText);});
  });
    }).fail(function(jqXHR){errormsg(1 + jqXHR.responseText); hideSpinner(loadSpinner);});
  }



});





$("#enrBtn").click(function(){
  if(type==="nothing"){
alert("No ids found in the table. Cannot proceed to the next step for MetaBox workflow.");
  }else{
      window.location = 'subnetwork.html?rsess='+rsession + '&idtype='+type;//send r object for subnetwork
  }

});


$("#overrepBtn").click(function(){
    if(type==="nothing"){
alert("No ids found in the table. Cannot proceed to the next step for MetaBox workflow.");
  }else{
      window.location = 'overrepanalysis.html?rsess='+rsession + '&idtype='+type;//send r object for subnetwork
  }
});

$("#cloudBtn").click(function(){
    if(type==="nothing"){
alert("No ids found in the table. Cannot proceed to the next step for MetaBox workflow.");
  }else{
      window.location = 'wordcloud.html?rsess='+rsession + '&idtype='+type;//send r object for subnetwork
  }

});

$("#SltBtn").click(function(){
    if(type!=="compound"){
alert("No compound id found in the table. Cannot proceed to the next step for MetaBox workflow.");
  }else{
      window.location = 'similarity.html?rsess='+rsession + '&idtype='+type;//send r object for subnetwork
  }
});




$("#PCA_plot_para_submit").click(function(){
    plotPCAScore(e1 = eData, f1 = fData, p1 = pData);
})


$(".Thefactor").on("change",function(){
    if($("#independent_factor").val()===null && $("#repeated_factor").val()===null){
    $("#Sample_Size_Table").text("Waiting user to select factor.")
  }else{
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
    }).fail(function(jqXHR){errormsg(1 + jqXHR.responseText); hideSpinner(loadSpinner);});
  }

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


function listSelector(full_options,why_not_able,id,furtherlimit){
  // why_not_able = ["numeric","",]
    // selected_options = ["disabled","",...]
var selector = '';
if(why_not_able==null){

for(var i = 0;i<full_options.length;i++){

       selector += "<option>" + full_options[i] + "</option>";

}

}else{
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
}




$(id).html(selector).selectpicker('refresh');
}



function decidenormalizationability(column_names_of_pData){

if($.inArray('batch', column_names_of_pData) == -1){

  var needBatch = document.getElementsByClassName("needBatch");


  for(var i=0; i<needBatch.length;i++){
    needBatch[i].disabled = true;
  }

    $("#why_not_able_batch").html('<i class="fa fa-question pull-right" data-toggle="tooltip" data-placement="right" data-html="true" title="Need phenotype data contains column batch" ></i>');

    $("#why_not_able_loess").html('<i class="fa fa-question pull-right" data-toggle="tooltip" data-placement="right" data-html="true" title="Need phenotype data contains column batch" ></i>');



}
if($.inArray('Sample_specific_weight', column_names_of_pData) == -1){

    var v = document.getElementsByClassName("needSample_specific");


  for(var i=0; i<v.length;i++){
    v[i].disabled = true;
  }

$("#why_not_able_Sample_specific").html('<i class="fa fa-question pull-right" data-toggle="tooltip" data-placement="right" data-html="true" title="Need phenotype data contains column Sample_specific_weight" ></i>');
if($.inArray('QC', column_names_of_pData) == -1){
  var v = document.getElementsByClassName("needQC");


  for(var i=0; i<v.length;i++){
    v[i].disabled = true;
  }
    $("#why_not_able_loess").html('<i class="fa fa-question pull-right" data-toggle="tooltip" data-placement="right" data-html="true" title="Need phenotype data contains column QC" ></i>');
}
if($.inArray('time_of_injection', column_names_of_pData) == -1){
  var v = document.getElementsByClassName("needtime_of_injection");


  for(var i=0; i<v.length;i++){
    v[i].disabled = true;
  }
    $("#why_not_able_loess").html('<i class="fa fa-question pull-right" data-toggle="tooltip" data-placement="right" data-html="true" title="Need phenotype data contains column time_of_injection" ></i>');
}

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
        sample_information_selection:selected_phenotype_by_check,
        Ellipse : $("#PCA_Ellipse").is(":checked"), dot_size : $("#PCA_dot_size").val(),
        IDtrend:$("#PCA_IDtrend").is(":checked")
      },function(session){
session.getObject(function(obj){
Plotly.newPlot(PCA_score_plot, eval(obj.data[0]),JSON.parse(obj.layout[0]));
$(".normalization").prop('disabled', false);
decidenormalizationability(pData_column);

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
  }).fail(function(jqXHR){errormsg(1 + jqXHR.responseText); hideSpinner(loadSpinner);});

$("#morris-donut-chart").html("");
plotDonut(e1,f1,p1,selected);
});});}).done(function(){
  hideSpinner(loadSpinner);
}).fail(function(jqXHR){errormsg(1 + jqXHR.responseText); hideSpinner(loadSpinner);});}




function plotDonut(e1,f1,p1,select){
  var loadSpinner = showSpinner();
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

  }).done(function(){ hideSpinner(loadSpinner);}).fail(function(jqXHR){errormsg(1 + jqXHR.responseText); hideSpinner(loadSpinner);});

}




summaryPhenotype = function(id,columns,columns_num){
var text = '<ul class="list-group">';

for(var i = 0;i<columns.length;i++){
  if(columns[i] === "phenotype_index" || columns[i] === "sampleID" ||columns[i] === "batch" ||columns[i] === "Sample_specific_weight" ||columns[i] === "QC"||
  columns[i]==="time_of_injection"||columns[i]=="rank"){
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
  if(columns[i] === "feature_index" || columns[i] === "KnownorUnknown" || columns[i]==="PubChem" || columns[i]==="ensembl"||columns[i]==="uniprot"){
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

              $("#Statistics_Result").empty()
             Stat_Result = drawTable('#Statistics_Result',obj);
               });




              download_address = session.getLoc() + "R/.val/csv";
              $("#download_statistical_result_button").empty();
              var r= $('<a download = "file.csv" href='+download_address+' class="btn btn-primary btn-lg active" role="button">Download Statistical Analysis Result</a>');
              $("#download_statistical_result_button").append(r);

 hideSpinner(loadSpinner);
    }).fail(function(jqXHR){errormsg(1 + jqXHR.responseText); hideSpinner(loadSpinner);});
  }

});





window.onresize = function() {
    Plotly.Plots.resize(PCA_score_plot);
};



function successmsg(text){
  $("#message").empty().append('<div class="alert alert-success alert-dismissable"><a href="#" class="close" data-dismiss="alert">&times;</a>' + text+ + '</div>');
}
function warningmsg(text){
  $("#message").empty().append('<div class="alert alert-warning alert-dismissable"><a href="#" class="close" data-dismiss="alert">&times;</a>' + text + '<a download = "file.csv" href='+modified_dataset_download_address+' role="button">downloading the data set with elements added.</a></div>');
}




function errormsg(text){
  $("#message").empty().append('<div class="alert alert-danger alert-dismissable"><a href="#" class="close" data-dismiss="alert">&times;</a>' + text + '</div>');
}

// Other Utility

// after click remove_sample, some samples would be removed for visualization.
/*$("#remove_sample").click(function(){
var loadSpinner = showSpinner();

  var req = ocpu.call("stat_rm_sample",{
    e:D.expression,p:D.phenotype,f:D.feature,sample_index:""
  },function(session){
    session.getObject(function(obj){
      apply_normalization();
mTICdid = Loessdid = medFCdid = BatchMediandid = false;
    });
  }).fail(function(jqXHR){errormsg(1 + jqXHR.responseText);});
});*/

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
      datatransformation_method = ["None","log","power"][i]
    }
}
var radios3 = document.getElementsByName('datascaling');
for (var i = 0, length = radios3.length; i < length; i++) {
    if (radios3[i].checked) {
      datascaling_method = ["None","Auto","Pareto","Range"][i]
    }
}
console.log(Loessdid);
console.log($("#sample_to_be_removed").val().split(','));

var req=ocpu.call("stat_norm",{
  e : D.expression,f : D.feature,p : D.phenotype,
  sample_index:$("#sample_to_be_removed").val().split(','),
  mTICdid : mTICdid,Loessdid:Loessdid,medFCdid:medFCdid,BatchMediandid:BatchMediandid,
  mTIC:mTIC,Loess:Loess,medFC:medFC,BatchMedian:BatchMedian,
  sample_normalization : samplenormalization_method,data_transformation:datatransformation_method,data_scaling:datascaling_method,
  log_para:$('input[name="logpara"]:checked').val(),independent_factor_name_log:$('#logparaautoindependent_factor').val(),
  power_para:$('input[name="powerpara"]:checked').val(),independent_factor_name_power:$('#powerparaautoindependent_factor').val()
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
 //  hideSpinner(loadSpinner);
  });
}).fail(function(jqXHR){errormsg(1 + jqXHR.responseText); hideSpinner(loadSpinner);});

};


$(".normalization").change(function(event){// able the applynormalization button.
$("#applynormalization").prop( "disabled", false);
});


$(".applynormalization").on("click",function(event){
  apply_normalization();
  $("#applynormalization").prop( "disabled", true);
});







$("#stat_norm_log_para_submit").click(function(event){
$('#datatransformationlog').attr('checked', true);
  apply_normalization();
});

$("#stat_norm_power_para_submit").click(function(event){
  $('#datatransformationpower').attr('checked', true);
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

$("#draw_boxplot").click(function(){
// print the boxplot_order_option
  if($("#independent_factor").val()===null && $("#repeated_factor").val()===null){
    alert("You haven't selected factor yet!  Go to Study Design box to select factor first.");

  }else if($("#independent_factor").val() === $("#repeated_factor").val()){
    alert("Between Subject Factor CANNOT equal to Within Subject Factor");

  }else{


if($("#repeated_factor").val() == null){
document.getElementById("boxplot_factor_order").disabled = true;
document.getElementById("boxplot_factor_order").placeholder = "Only Available for Within Subject Study Design..";
}

var req = ocpu.call("stat_get_levels",{
  e:eData,
  f:fData,
  p:pData,
  factor:$("#repeated_factor").val()[0]
},function(session){
session.getObject(function(obj){
      document.getElementById("boxplot_order_option").innerHTML = obj;
  })
})

  }
})




var title_of_boxplots;
var download_boxplot_address;
$("#draw_boxplot_submit").click(function(){
  if($("#independent_factor").val()===null && $("#repeated_factor").val()===null){
    alert("You haven't selected factor yet! Close me and go to Study Design box to select factor first.");

  }else if($("#independent_factor").val() === $("#repeated_factor").val()){
    alert("Between Subject Factor CANNOT equal to Within Subject Factor");

  }else{

  var loadSpinner = showSpinner();
  var req = ocpu.call("stat_boxplot", {
      e:D.expression,
      f:D.feature,
      p:D.phenotype,
      independent_factor_name:$("#independent_factor").val(),
      repeated_factor_name:$("#repeated_factor").val(),
      compound_name_column_index : $("#boxplottitle").val(),
      order_of_factor: $("#boxplot_factor_order").val().split(";")
  },function(session){

    download_boxplot_address = session.getLoc() + "tar";

             $("#download_boxplot").empty();
             var r= $('<a href='+download_boxplot_address+' class="active"><font color="white">Download Boxplots</font></a>');
            $("#download_boxplot").append(r);

             var temp = document.getElementById('download_boxplot');
             temp.disabled = false;


  }).fail(function(){
    alert("R returned an error: " + req.responseText);
    }).done(function(){
 hideSpinner(loadSpinner);
    });
}

})

$("#Normalization_To_Be_Applied").on("change",function(){
  var v = $("#Normalization_To_Be_Applied").val();
$( "#samplenormalizationselected" ).text( v[0] );
$( "#datatransformationselected" ).text( v[1] );
$( "#featuretransformationselected" ).text( v[2] );

})



$("input[name = 'logpara']").click(function(){
  if(document.getElementById('logparaauto').checked){
   $("#logparaautoindependent_factor_div").collapse('show');
  }else{
   $("#logparaautoindependent_factor_div").collapse('hide');
  }
})
$("input[name = 'powerpara']").click(function(){
  if(document.getElementById('powerparaauto').checked){
   $("#powerparaautoindependent_factor_div").collapse('show');
  }else{
   $("#powerparaautoindependent_factor_div").collapse('hide');
  }
})






});
