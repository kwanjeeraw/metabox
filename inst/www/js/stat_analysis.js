var editor;
var samplenormalization_method = "None";
var datatransformation_method = 'None';
var datascaling_method = "None";
var pwr = 80;
var D;// This is unchangable untill modify on the experimental design part.
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
var mTICdid = false; var Loessdid = false; var medFCdid  = false; var BatchMediandid = false;
var mTIC = Loess = medFC = BatchMedian = [];
var mTIC_raw = []; var Loess_raw = []; var medFC_raw = []; var BatchMedian_raw = [];
var e_ori = []; var p_ori = [];
var session111;
var fData_column;
var pData_column;
var pData_columns_num;
var modified_dataset_download_address = [];
var download_norm_data_adress = [];
var res_url;
var rsession;
var type;
var duplicatedID;
var guess_independent_factor;
var guess_repeated_factor;
var PCA_score_plot = []
var sample_specific_weight = "";
var sample_specific_multiplyordevide = "";
var KnownorUnknown = {value:''};
var QCIndicator = ""; var BatchIndicator = ""; var TimeIndicator = "";
var mTICunchanged = false;
var Loessunchanged = false;
var Batchunchanged = false;
var pComponents;



$(document).ready(function(){//wait untile the document is fully loaded.


PCA_score_plot = Plotly.d3.select('#stat_PCA_plot')
    .style({
        width: 100 + '%',
        height: 100 + '%'
    }).node();




$("#samplenormalizationSample_specific").click(function(){
  select_samplenormalizationSample_specific(pData_column)
})
$("#samplenormalizationmTIC").click(function(){
  select_samplenormalizationmTIC(fData_column)
})
$("#samplenormalizationloess").click(function(){
  select_samplenormalizationloess(pData_column)
})
$("#samplenormalizationBatchMedian").click(function(){
  select_samplenormalizationmBatchMedian(pData_column)
})



$("#success-message-alert").hide();
$("#warning-message-alert").hide();
$("#danger-message-alert").hide();




  $('[data-toggle="tooltip"]').tooltip();

//turn to inline mode
$.fn.editable.defaults.mode = 'inline';
$('#stat_load_data_para_SheetIndex').editable({
   emptytext: "<p class='text-muted'><strong>The <span class='text-danger'>first sheet</span> will be read as default. Click me to change.</strong></p>"
    });




$("#para_method").editable({})
$("#para_post_hoc").editable({})
$("#non_para_method").editable({})
$("#non_para_post_hoc").editable({})







// load data.
$("#inputUploadData").on("change", uploaddata)// load data.
// use example data.
$("input[name='optionsRadiosInline']").on("click", function(){
  document.getElementById('submit_example_data').style.visibility = 'visible';
})

$("#submit_example_data").on("click", uploaddata);// load data.




$('#desired_power').editable({
  title: 'Enter Power Desired (0-100)',
  success: function(response, newValue) {pwr = newValue}
});







$("#Submit_To_Statistics").click(function(){
// check normalization method available or not.


   if($("#independent_factor").val()===null){
    ind = []
  }else{
    ind = $("#independent_factor").val()
  }

  if($("#repeated_factor").val()===null){
    rep = []
  }else{
    rep = $("#repeated_factor").val()
  }



  // length should be 1 or 2

  if(ind.length + rep.length === 0){
    bootbox.alert("You haven't select factor yet. Please select at Between Subject Factor or Within Subject Factor session.");
  }else if(ind.length + rep.length > 2){
    bootbox.alert("Currently, metabox can only analysis two-way design, namely you can NOT have more than two factors intotal in Between Subject Factor and Within Subject Factor session");
  }else if(arraysEqual(ind,rep)){
    bootbox.alert("Between Subject Factor can NOT equal th Within Subject Factor.");
  }else{


applystatistics()







  }

});





$("#PCA_plot_para_submit").click(function(){
    plotPCAScore(e1 = eData, f1 = fData, p1 = pData);
})

$(".Thefactor").on("change",function(){


    if($("#independent_factor").val()===null && $("#repeated_factor").val()===null){
    $("#Sample_Size_Table").text("Waiting user to select factor.")
    $("#stat_method").text("Nothing is selected");
  }else{
    var loadSpinner = showSpinner();
                     var req = ocpu.call("stat_Study_Design",{
                    DATA:DATA,
                    between_factor:$("#independent_factor").val(),
                    within_factor:$("#repeated_factor").val()
                    },function(session4){
                  session4.getStdout(function(outtxt){
                            $("#Sample_Size_Table").text(outtxt);
                                        hideSpinner(loadSpinner);
                        });
                      session4.getFile("stat_method.txt", function(text){
console.log(text)
if(text.includes("one way ANOVA")){
  ANOVA_disc();
  console.log(ttestmethod);
}else if(text.includes("independent t test")){
  //ttestdescription();
  t_test_disc();
}else if(text.includes("two way ANOVA33")){
  twowayANOVA33_disc();
}else if(text.includes("two way ANOVA23")){
  twowayANOVA23_disc()
}else if(text.includes("two way ANOVA22")){
  twowayANOVA22_disc()
}else if(text.includes("two way ANOVA32")){
  twowayANOVA32_disc()
}else if(text.includes("one way repeated ANOVA")){
  pairedANOVA_disc();
}else if(text.includes("paired t test")){
  paired_t_test_disc();
}else if(text.includes("two way repeated anova33")){
  twowaypairedANOVA33_disc();
}else if(text.includes("two way repeated anova22")){
  twowaypairedANOVA22_disc();
}else if(text.includes("two way repeated anova32")){
  twowaypairedANOVA32_disc();
}else if(text.includes("two way repeated anova23")){
  twowaypairedANOVA23_disc();
}else if(text.includes("mixed two way anova33")){
  mixedANOVA33_disc();
}else if(text.includes("mixed two way anova22")){
  mixedANOVA22_disc();
}else if(text.includes("mixed two way anova32")){
  mixedANOVA32_disc();
}else if(text.includes("mixed two way anova23")){
  mixedANOVA23_disc();
}


loadxeditable_elements();

hideSpinner(loadSpinner);
        });
}).fail(function(jqXHR){hideSpinner(loadSpinner); bootbox.alert(jqXHR.responseText); });
  }


})






$("#PCA_color").change(function(){
  plotPCAScore(e1 = eData, f1 = fData, p1 = pData);
});
$("#Donut_color").change(function(){
  $("#morris-donut-chart").html("");
  plotDonut(e1 = eData, f1 = fData, p1 = pData,selected);
});


// functions

$("#enrBtn").click(function(){
  if(type==="nothing"){
bootbox.alert("No ids found in the table. Cannot proceed to the next step for MetaBox workflow.");
  }else{
      window.location = 'enrichmentrsess.html?rsess='+rsession + '&idtype='+type;//send r object for subnetwork
  }

});

$("#overrepBtn").click(function(){
    if(type==="nothing"){
bootbox.alert("No ids found in the table. Cannot proceed to the next step for MetaBox workflow.");
  }else{
      window.location = 'overreprsess.html?rsess='+rsession + '&idtype='+type;//send r object for subnetwork
      //var rsess1 = rsession.replace("localhost","128.120.143.234");
      //window.open('http://128.120.143.215:3104/ocpu/library/metaboxdev/www/overreprsess.html?rsess='+rsess1 + '&idtype='+type, '_blank');
  }
});

$("#cloudBtn").click(function(){
    if(type==="nothing"){
bootbox.alert("No ids found in the table. Cannot proceed to the next step for MetaBox workflow.");
  }else{
      window.location = 'wordcloudrsess.html?rsess='+rsession + '&idtype='+type;//send r object for subnetwork
  }

});

$("#SltBtn").click(function(){
    if(type!=="compound"){
bootbox.alert("No compound id found in the table. Cannot proceed to the next step for MetaBox workflow.");
  }else{
      window.location = 'similarityrsess.html?rsess='+rsession + '&idtype='+type;//send r object for subnetwork
      //window.open('http://128.120.143.208:1200/ocpu/library/MetaMapp2016/www/similarity.html?rsess='+rsession + '&idtype='+type, '_blank');
  }
});





var nobatch = false;
var noloess = false;
var nosample_specific=false;

/*function decidenormalizationability(column_names_of_pData){

if($.inArray('batch', column_names_of_pData) == -1){

  var needBatch = document.getElementsByClassName("needBatch");


  for(var i=0; i<needBatch.length;i++){
    needBatch[i].disabled = true;
  }

    $("#why_not_able_batch").html('<i class="fa fa-question pull-right" data-toggle="tooltip" data-placement="right" data-html="true" title="Need phenotype data contains column batch" ></i>');

    $("#why_not_able_loess").html('<i class="fa fa-question pull-right" data-toggle="tooltip" data-placement="right" data-html="true" title="Need phenotype data contains column batch" ></i>');

nobatch = true;
noloess = true;
}
if($.inArray('Sample_specific_weight', column_names_of_pData) == -1){

    var v = document.getElementsByClassName("needSample_specific");


  for(var i=0; i<v.length;i++){
    v[i].disabled = true;
  }

$("#why_not_able_Sample_specific").html('<i class="fa fa-question pull-right" data-toggle="tooltip" data-placement="right" data-html="true" title="Need phenotype data contains column Sample_specific_weight" ></i>');

nosample_specific = true;

if($.inArray('QC', column_names_of_pData) == -1){
  var v = document.getElementsByClassName("needQC");


  for(var i=0; i<v.length;i++){
    v[i].disabled = true;
  }
    $("#why_not_able_loess").html('<i class="fa fa-question pull-right" data-toggle="tooltip" data-placement="right" data-html="true" title="Need phenotype data contains column QC" ></i>');
    noloess = true;
}

if($.inArray('time_of_injection', column_names_of_pData) == -1){
  noloess = true;
  var v = document.getElementsByClassName("needtime_of_injection");


  for(var i=0; i<v.length;i++){
    v[i].disabled = true;
  }
    $("#why_not_able_loess").html('<i class="fa fa-question pull-right" data-toggle="tooltip" data-placement="right" data-html="true" title="Need phenotype data contains column time_of_injection" ></i>');

}

}

}*/






























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

     listSelector(pData_column,why_not_able,"#independent_factor",null,guess_independent_factor);
     listSelector(pData_column,why_not_able,"#repeated_factor",null,guess_repeated_factor);
     listSelector(pData_column,why_not_able,"#confounding_factor",null,"dfdasfdasfewrf");
     listSelector(pData_column,why_not_able,"#PCA_color",null,guess_independent_factor);
     listSelector(pData_column,why_not_able,"#Donut_color",null,"dfdasfdasfewrf");

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
             Stat_Result = drawTable('#Statistics_Result',obj, "Univariate Statistical Result","feature_index");
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







// Normalization









$(".normalization").change(function(event){// able the applynormalization button.
$("#applynormalization").removeClass("disabled");
});


$(".applynormalization").on("click",function(event){
   if ($(this).hasClass('disabled')) {
        smallerrormsg(text = '<strong>Please select a new normalization that is different from the current one.</strong>',time = 5000);
    }else{
        apply_normalization();
        $("#samplenormalizationmethod").text(samplenormalization_method);
        $("#datatransformationmethod").text(datatransformation_method);
        $("#featurenormalizationmethod").text(datascaling_method);

$( "#samplenormalizationSample_specific_column2" ).text("using " + sample_specific_weight );
$( "#mTIC_column2" ).text("Using: " + KnownorUnknown.value );
$( "#Batch_column2" ).text("Batch Indicator: " + BatchIndicator);
$( "#Loess_column2" ).text("QC: " + QCIndicator +"; Batch: "+BatchIndicator + "; Time: " +TimeIndicator +".");

if(!$("#samplenormalizationSample_specific").is(':checked')){
  $( "#samplenormalizationSample_specific_column" ).text("");  $( "#samplenormalizationSample_specific_column2" ).text("");
}
if(!$("#samplenormalizationmTIC").is(':checked')){
  $( "#mTIC_column" ).text("");$( "#mTIC_column2" ).text("");
}
if(!$("#samplenormalizationloess").is(':checked')){
  $( "#Loess_column" ).text("");$( "#Loess_column2" ).text("");
}
if(!$("#samplenormalizationBatchMedian").is(':checked')){
  $( "#Batch_column" ).text("");$( "#Batch_column2" ).text("");
}


$("#Samples_To_Be_Deleted").text("");
$("#Samples_To_Be_Deleted").text($("#sample_to_be_removed").val());
$("#applynormalization").addClass("disabled");
    }
});



/*var ex1 = {
    attr1: "initial value of attr1",
    attr2: "initial value of attr2"
};

//defining a 'watcher' for an attribute
watch(ex1, "attr1", function(){
    alert("attr1 changes!");
});

//when changing the attribute its watcher will be invoked
ex1.attr1 = "other value";*/




















































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





$("#repeated_factor").click(function(){
  if($("#repeated_factor").val() == null){
document.getElementById("boxplot_factor_order").disabled = true;
document.getElementById("boxplot_factor_order").placeholder = "Only Available for Within Subject Study Design..";
}
})


// download boxplots.

$("#draw_boxplot").click(function(){
// print the boxplot_order_option
  if($("#independent_factor").val()===null && $("#repeated_factor").val()===null){
    bootbox.alert("You haven't selected factor yet!  Go to Study Design box to select factor first.");

  }else if($("#independent_factor").val() === $("#repeated_factor").val()){
    bootbox.alert("Between Subject Factor CANNOT equal to Within Subject Factor");

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
    bootbox.alert("You haven't selected factor yet! Close me and go to Study Design box to select factor first.");

  }else if($("#independent_factor").val() === $("#repeated_factor").val()){
    bootbox.alert("Between Subject Factor CANNOT equal to Within Subject Factor");

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
    bootbox.alert("R returned an error: " + req.responseText);
    }).done(function(){
 hideSpinner(loadSpinner);
    });
}

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
