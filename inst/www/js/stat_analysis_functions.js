uploaddata = function(){
  var loadSpinner = showSpinner();
  try{var name = $("#inputUploadData")[0].files[0].name}catch(e){name = "Example Data Uploaded"}

  $("#file_name").val(name)


      var req=ocpu.call("stat_load_data",{
        file: $("#inputUploadData")[0].files[0],
        sheetIndex: $("#stat_load_data_para_SheetIndex").val(),
        from_example:$('input[name="optionsRadiosInline"]:checked').val()
      },function(session){
        session.getObject(function(obj){
          DATA = obj;
          D = obj;
          eData = obj.expression;
          fData = obj.feature;
          pData = obj.phenotype;
          e_ori = obj.expression;
          p_ori = obj.phenotype;
         drawTable('#View_pData',obj.phenotype, "Phenotype Data","phenotype_index");
         drawTable('#View_fData',obj.feature, "Feature Data","feature_index");
         drawTable('#View_eData',obj.expression, "Expression Data","feature_index");
          duplicatedID = obj.duplicatedID;
          if(duplicatedID[0]){
              document.getElementById("PCA_connect_id").innerHTML = '<div class="form-group"><label>Display Trend of Each sampleID:</label><label class="checkbox-inline pull-right" ><input type="checkbox" id = "PCA_IDtrend"  value = "TRUE"></label></div>';
          }
          var req2 = ocpu.call("stat_summary_data",{
            DATA:D
          },function(session2){


              session2.getObject(function(obj2){
                pComponents = obj2.pComponents;
                pData_column = obj2.column_names_of_pData;
                pData_columns_num = obj2.pData_columns_num;
                fData_column = obj2.column_names_of_fData;
                why_not_able = obj2.why_not_able;


                guess_independent_factor=obj2.guess_independent_factor;
                guess_repeated_factor=obj2.guess_repeated_factor;


                //decidenormalizationability(obj2.column_names_of_pData);
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#logparaautoindependent_factor",null,"dfdasfdasfewrf");
                listSelector(obj2.column_names_of_fData,null,"#boxplottitle",null,"dfdasfdasfewrf");
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#powerparaautoindependent_factor",null,"dfdasfdasfewrf");


                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#independent_factor",null,guess_independent_factor);
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#repeated_factor",null,guess_repeated_factor);
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#confounding_factor",null,"dfdasfdasfewrf");
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#PCA_color",null,guess_independent_factor);
                listSelector(obj2.column_names_of_pData,obj2.why_not_able,"#Donut_color",null,"dfdasfdasfewrf");

                     var req = ocpu.call("stat_Study_Design",{
                    DATA:D,
                    between_factor:$("#independent_factor").val(),
                    within_factor:$("#repeated_factor").val()
                    },function(session4){
                  session4.getStdout(function(outtxt){
                            $("#Sample_Size_Table").text(outtxt);
                                        hideSpinner(loadSpinner);
                        });
                      session4.getFile("stat_method.txt", function(text){


if(text.includes("one way ANOVA")){
  ANOVA_disc();
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
;

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

          if(text.indexOf('Success!') > -1){
           successmsg(text);
          }else{
           warningmsg(text);
          }

//$("#UploadData").collapse('toggle');

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


}


var Stat_Result = {}




applystatistics = function(){
  var loadSpinner = showSpinner();





    var req=ocpu.call("stat_hypo_test",{
    e:DATA.expression,f : DATA.feature,p : DATA.phenotype,
    e_ori:e_ori,
    p_ori:p_ori,
    e_before:D.expression,
    p_before:D.phenotype,
    independent_factor_name:$("#independent_factor").val(),
    repeated_factor_name:$("#repeated_factor").val(),
    need_power:document.getElementById('power_analysis_needed').checked,
    desired_power: pwr,

ttestmethod : ttestmethod,ttestcorrection : ttestcorrection,nonparattestmethod : nonparattestmethod,nonparattestcorrection : nonparattestcorrection,

ANOVAmethod : ANOVAmethod,ANOVAposthoc : ANOVAposthoc,nonparaANOVAmethod : nonparaANOVAmethod,nonparaANOVAposthoc : nonparaANOVAposthoc,

twowayANOVAmethod : twowayANOVAmethod,
maineffect1ANOVAmethod : maineffect1ANOVAmethod,maineffect1ANOVAposthoc : maineffect1ANOVAposthoc,simplemaineffect1ANOVAmethod : simplemaineffect1ANOVAmethod,simplemaineffect1ANOVAposthoc : simplemaineffect1ANOVAposthoc,
maineffect1ttestmethod : maineffect1ttestmethod,maineffect1ttestcorrection : maineffect1ttestcorrection,simplemaineffect1ttestmethod : simplemaineffect1ttestmethod,simplemaineffect1ttestcorrection : simplemaineffect1ttestcorrection,
maineffect2ANOVAmethod : maineffect2ANOVAmethod,maineffect2ANOVAposthoc : maineffect2ANOVAposthoc,simplemaineffect2ANOVAmethod : simplemaineffect2ANOVAmethod,simplemaineffect2ANOVAposthoc : simplemaineffect2ANOVAposthoc,
maineffect2ttestmethod : maineffect2ttestmethod,maineffect2ttestcorrection : maineffect2ttestcorrection,simplemaineffect2ttestmethod : simplemaineffect2ttestmethod,simplemaineffect2ttestcorrection : simplemaineffect2ttestcorrection,
nonparatwowayANOVAmethod : nonparatwowayANOVAmethod,
nonparamaineffect1ANOVAmethod : nonparamaineffect1ANOVAmethod,nonparamaineffect1ANOVAposthoc : nonparamaineffect1ANOVAposthoc,nonparasimplemaineffect1ANOVAmethod : nonparasimplemaineffect1ANOVAmethod,nonparasimplemaineffect1ANOVAposthoc : nonparasimplemaineffect1ANOVAposthoc,
nonparamaineffect1ttestmethod : nonparamaineffect1ttestmethod,nonparamaineffect1ttestcorrection : nonparamaineffect1ttestcorrection,nonparasimplemaineffect1ttestmethod : nonparasimplemaineffect1ttestmethod,nonparasimplemaineffect1ttestcorrection : nonparasimplemaineffect1ttestcorrection,
nonparamaineffect2ANOVAmethod : nonparamaineffect2ANOVAmethod,nonparamaineffect2ANOVAposthoc : nonparamaineffect2ANOVAposthoc,nonparasimplemaineffect2ANOVAmethod : nonparasimplemaineffect2ANOVAmethod,nonparasimplemaineffect2ANOVAposthoc : nonparasimplemaineffect2ANOVAposthoc,
nonparamaineffect2ttestmethod : nonparamaineffect2ttestmethod,nonparamaineffect2ttestcorrection : nonparamaineffect2ttestcorrection,nonparasimplemaineffect2ttestmethod : nonparasimplemaineffect2ttestmethod,nonparasimplemaineffect2ttestcorrection : nonparasimplemaineffect2ttestcorrection,

pairedttestmethod : pairedttestmethod,pairedttestcorrection : pairedttestcorrection,nonparapairedttestmethod : nonparapairedttestmethod,nonparapairedttestcorrection : nonparapairedttestcorrection,

pairedANOVAmethod : pairedANOVAmethod,pairedANOVAadjust : pairedANOVAadjust,pairedANOVAposthoc : pairedANOVAposthoc,nonparapairedANOVAmethod : nonparapairedANOVAmethod,nonparapairedANOVAposthoc : nonparapairedANOVAposthoc,

twowaypairedANOVAmethod : twowaypairedANOVAmethod,twowaypairedANOVAadjust : twowaypairedANOVAadjust,
maineffect1pairedANOVAmethod : maineffect1pairedANOVAmethod,maineffect1pairedANOVAadjust : maineffect1pairedANOVAadjust,maineffect1pairedANOVAposthoc : maineffect1pairedANOVAposthoc,simplemaineffect1pairedANOVAmethod : simplemaineffect1pairedANOVAmethod,simplemaineffect1pairedANOVAadjust : simplemaineffect1pairedANOVAadjust,simplemaineffect1pairedANOVAposthoc : simplemaineffect1pairedANOVAposthoc,
maineffect1pairedttestmethod : maineffect1pairedttestmethod,maineffect1pairedttestcorrection : maineffect1pairedttestcorrection,simplemaineffect1pairedttestmethod : simplemaineffect1pairedttestmethod,simplemaineffect1pairedttestcorrection : simplemaineffect1pairedttestcorrection,
maineffect2pairedANOVAmethod : maineffect2pairedANOVAmethod,maineffect2pairedANOVAadjust : maineffect2pairedANOVAadjust,maineffect2pairedANOVAposthoc : maineffect2pairedANOVAposthoc,simplemaineffect2pairedANOVAmethod : simplemaineffect2pairedANOVAmethod,simplemaineffect2pairedANOVAadjust : simplemaineffect2pairedANOVAadjust,simplemaineffect2pairedANOVAposthoc : simplemaineffect2pairedANOVAposthoc,
maineffect2pairedttestmethod : maineffect2pairedttestmethod,maineffect2pairedttestcorrection : maineffect2pairedttestcorrection,simplemaineffect2pairedttestmethod : simplemaineffect2pairedttestmethod,simplemaineffect2pairedttestcorrection : simplemaineffect2pairedttestcorrection,

nonparatwowaypairedANOVAmethod : nonparatwowaypairedANOVAmethod,
nonparamaineffect1pairedANOVAmethod : nonparamaineffect1pairedANOVAmethod,nonparamaineffect1pairedANOVAposthoc : nonparamaineffect1pairedANOVAposthoc,nonparasimplemaineffect1pairedANOVAmethod : nonparasimplemaineffect1pairedANOVAmethod,nonparasimplemaineffect1pairedANOVAposthoc : nonparasimplemaineffect1pairedANOVAposthoc,
nonparamaineffect1pairedttestmethod : nonparamaineffect1pairedttestmethod,nonparamaineffect1pairedttestcorrection : nonparamaineffect1pairedttestcorrection,nonparasimplemaineffect1pairedttestmethod : nonparasimplemaineffect1pairedttestmethod,nonparasimplemaineffect1pairedttestcorrection : nonparasimplemaineffect1pairedttestcorrection,
nonparamaineffect2pairedANOVAmethod : nonparamaineffect2pairedANOVAmethod,nonparamaineffect2pairedANOVAposthoc : nonparamaineffect2pairedANOVAposthoc,nonparasimplemaineffect2pairedANOVAmethod : nonparasimplemaineffect2pairedANOVAmethod,nonparasimplemaineffect2pairedANOVAposthoc : nonparasimplemaineffect2pairedANOVAposthoc,
nonparamaineffect2pairedttestmethod : nonparamaineffect2pairedttestmethod,nonparamaineffect2pairedttestcorrection : nonparamaineffect2pairedttestcorrection,nonparasimplemaineffect2pairedttestmethod : nonparasimplemaineffect2pairedttestmethod,nonparasimplemaineffect2pairedttestcorrection : nonparasimplemaineffect2pairedttestcorrection,

mixedANOVA : mixedANOVA,mixedANOVAadjust : mixedANOVAadjust,
nonparamixedANOVA : nonparamixedANOVA






  },function(session4){
    //$("#StudyDesign").collapse('hide');
    //$("#ViewData").collapse('hide');
    //$("#visualization_body").collapse('hide');

              session4.getObject(function(obj3){
                stat_result = obj3;

                if(typeof stat_result[0]==="string"){
                  bootbox.alert(stat_result);
                }else{

                $("#continue_analysis").collapse('show');


              session4.getObject(function(obj3){

            $("#Statistics_Result").empty()



            drawTable('#Statistics_Result',obj3, "Univariate Statistical Result","feature_index","600px");




               });

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

                }
                hideSpinner(loadSpinner);
               });


session4.getFile("messages_hypo_test.txt", function(text){
          if(text.length>10){
           warningmsg(text);
          }


        });

    }).fail(function(jqXHR){hideSpinner(loadSpinner);bootbox.alert(jqXHR.responseText); });





}






function select_samplenormalizationSample_specific(pData_column){
$("#applynormalization").removeClass("disabled");
  select_body = '<option>'+pData_column[0]+'</option>'
    select_start = '<select class="form-control" id="weight">';
        for(i=1;i<pData_column.length;i++){
          select_body = select_body+'<option>'+pData_column[i]+'</option>'
        }
    select_end = '</select>';
    select = select_start+select_body+select_end

bootbox.dialog({
   closeButton: false,
                title: "Which Column is Weight Facotr?",
                message: '<div class="row">  ' +
                    '<div class="col-md-12"> ' +
                    '<form class="form-horizontal"> ' +
                    '<div class="form-group"> ' +
                    '<label class="col-md-4 control-label" for="name">Column Name</label> ' +
                    '<div class="col-md-4">'+select+' </div> ' +
                    '</div> ' +
                    '<div class="form-group"> ' +
                    '<label class="col-md-4 control-label" for="multiplyordevide">Multiply or Devide?</label> ' +
                    '<div class="col-md-6"> <div class="radio"> <label for="multiplyordevide-0"> ' +
                    '<input type="radio" name="multiplyordevide" id="multiplyordevide-0" value="Multiply" checked="checked"> ' +
                    ' Multiply </label><span class="text-muted" style = "font-style: italic"> (after = before * weight)</span> ' +
                    '</div><div class="radio"> <label for="multiplyordevide-1"> ' +
                    '<input type="radio" name="multiplyordevide" id="multiplyordevide-1" value="Devide"> Devide </label><span  style = "font-style: italic" class="text-muted"> (after = before / weight)</span> ' +
                    '</div> ' +
                    '</div> </div>' +
                    '</form> </div>  </div>',
                buttons: {
                    success: {
                        label: "Save",
                        className: "btn-success",
                        callback: function () {
                            sample_specific_weight = $('#weight').val();
                            sample_specific_multiplyordevide = $("input[name='multiplyordevide']:checked").val()
                          $( "#samplenormalizationSample_specific_column" ).text("using " + sample_specific_weight );
console.log(samplenormalization_method)
                          samplenormalization_method = 'Sample_specific'
                          //$("#samplenormalizationmethod").text(samplenormalization_method);
                          $("#applynormalization").removeClass("disabled");
                        }
                    },
                    cancel:{
                      label:"Cancel",
                      callback:function(){
                        if(samplenormalization_method=="None"){
                          $("#samplenormalizationnone").prop("checked", true)
                        }
                        if(samplenormalization_method=="Sample_specific"){
$("#samplenormalizationSample_specific").prop("checked", true)
                        }
                        if(samplenormalization_method=="mTIC"){
$("#samplenormalizationmTIC").prop("checked", true)
                        }
                        if(samplenormalization_method=="Loess"){
$("#samplenormalizationloess").prop("checked", true)
                        }
                        if(samplenormalization_method=="Batch Median"){
$("#samplenormalizationBatchMedian").prop("checked", true)
                        }


                      }
                    }
                }
            }
        );
}


function select_samplenormalizationloess(pData_column){
$("#applynormalization").removeClass("disabled");
  select_body = '<option>'+pData_column[0]+'</option>'
    select_start = '<select class="form-control" id="QCindicator">';
        for(i=1;i<pData_column.length;i++){
          select_body = select_body+'<option>'+pData_column[i]+'</option>'
        }
    select_end = '</select>';
    select = select_start+select_body+select_end


  select_body2 = '<option>'+pData_column[0]+'</option>';
    select_start2 = '<select class="form-control" id="Batchindicator">';
        for(i=1;i<pData_column.length;i++){
          select_body2 = select_body2+'<option>'+pData_column[i]+'</option>';
        }
    select_end2 = '</select>';
    select2 = select_start2+select_body2+select_end2;


  select_body3 = '<option>'+pData_column[0]+'</option>';
    select_start3 = '<select class="form-control" id="Timeindicator">';
        for(i=1;i<pData_column.length;i++){
          select_body3 = select_body3+'<option>'+pData_column[i]+'</option>';
        }
    select_end3 = '</select>';
    select3 = select_start3+select_body3+select_end3;

bootbox.dialog({   closeButton: false,
                title: "Which Column is QC, Batch and Time Indicator?",
                message: '<div class="row">  ' +
                    '<div class="col-md-12"> ' +
                    '<form class="form-horizontal"> ' +
                    '<div class="form-group"> ' +
                    '<label class="col-md-4 control-label" for="forQC">QC: </label> ' +
                    '<div class="col-md-6">'+select+' </div> ' +
                    '</div> ' +
                    '<div class="form-group">' +
                    '<label class="col-md-4 control-label" for="forBatch">Batch: </label> ' +
                    '<div class="col-md-6">'+select2+' </div> ' +
                    '</div>' +
                    '<div class="form-group">' +
                    '<label class="col-md-4 control-label" for="forTime">Time/Order: </label> ' +
                    '<div class="col-md-6">'+select3+' </div> ' +
                    '</div>'+
                    '</form> </div>  </div>',
                buttons: {
                    success: {
                        label: "Save",
                        className: "btn-success",
                        callback: function () {
console.log($('#QCindicator').val());
                          Loessunchanged = QCIndicator===$('#QCindicator').val() && BatchIndicator===$('#Batchindicator').val() && TimeIndicator===$('#Timeindicator').val();//if later equals the KnownorKnown, this means that nothing changed so the mTIC did should stays the same.

                            QCIndicator = $('#QCindicator').val();
                            BatchIndicator = $('#Batchindicator').val();
                            TimeIndicator = $('#Timeindicator').val();
                          $( "#Loess_column" ).text("QC: " + QCIndicator +"; Batch: "+BatchIndicator + "; Time: " +TimeIndicator +".");

                          samplenormalization_method = 'Loess'
//$("#samplenormalizationmethod").text(samplenormalization_method);
$("#applynormalization").removeClass("disabled");
                        }
                    },
                    cancel:{
                      label:"Cancel",
                      callback:function(){
                        if(samplenormalization_method=="None"){
                          $("#samplenormalizationnone").prop("checked", true)
                        }
                        if(samplenormalization_method=="Sample_specific"){
$("#samplenormalizationSample_specific").prop("checked", true)
                        }
                        if(samplenormalization_method=="mTIC"){
$("#samplenormalizationmTIC").prop("checked", true)
                        }
                        if(samplenormalization_method=="Loess"){
$("#samplenormalizationloess").prop("checked", true)
                        }
                        if(samplenormalization_method=="Batch Median"){
$("#samplenormalizationBatchMedian").prop("checked", true)
                        }


                      }
                    }
                }
            }
        );
}






function select_samplenormalizationmBatchMedian(pData_column){
$("#applynormalization").removeClass("disabled");
  select_body = '<option>'+pData_column[0]+'</option>';
    select_start = '<select class="form-control" id="BatchIndicator">';
        for(i=1;i<pData_column.length;i++){
          select_body = select_body+'<option>'+pData_column[i]+'</option>';
        }
    select_end = '</select>';
    select = select_start+select_body+select_end;

bootbox.dialog({   closeButton: false,
                title: "Which Column is Batch Indicator?",
                message: '<div class="row">  ' +
                    '<div class="col-md-12"> ' +
                    '<form class="form-horizontal"> ' +
                    '<div class="form-group"> ' +
                    '<label class="col-md-4 control-label">Batch: </label> ' +
                    '<div class="col-md-4">'+select+' </div> ' +
                    '</div> ' +
                    '</form> </div>  </div>',
                buttons: {
                    success: {
                        label: "Save",
                        className: "btn-success",
                        callback: function () {
                          Batchunchanged = BatchIndicator===$('#BatchIndicator').val();//if later equals the KnownorKnown, this means that nothing changed so the mTIC did should stays the same.
                            BatchIndicator = $('#BatchIndicator').val();
                          $( "#Batch_column" ).text("Batch Indicator: " + BatchIndicator);

                          samplenormalization_method = 'Batch Median'
//$("#samplenormalizationmethod").text(samplenormalization_method);
$("#applynormalization").removeClass("disabled");
                        }
                    }
                },
                    cancel:{
                      label:"Cancel",
                      callback:function(){
                        if(samplenormalization_method=="None"){
                          $("#samplenormalizationnone").prop("checked", true)
                        }
                        if(samplenormalization_method=="Sample_specific"){
$("#samplenormalizationSample_specific").prop("checked", true)
                        }
                        if(samplenormalization_method=="mTIC"){
$("#samplenormalizationmTIC").prop("checked", true)
                        }
                        if(samplenormalization_method=="Loess"){
$("#samplenormalizationloess").prop("checked", true)
                        }
                        if(samplenormalization_method=="Batch Median"){
$("#samplenormalizationBatchMedian").prop("checked", true)
                        }


                      }
                    }
            }
        );
}


function select_samplenormalizationmTIC(fData_column){
$("#applynormalization").removeClass("disabled");
  select_body = '<option>'+fData_column[0]+'</option>'
    select_start = '<select class="form-control" id="mTICindicator">';
        for(i=1;i<fData_column.length;i++){
          select_body = select_body+'<option>'+fData_column[i]+'</option>'
        }
    select_end = '</select>';
    select = select_start+select_body+select_end

bootbox.dialog({   closeButton: false,
                title: "Which Column is Known/Unknown Indicator?",
                message: '<div class="row">  ' +
                    '<div class="col-md-12"> ' +
                    '<form class="form-horizontal"> ' +
                    '<div class="form-group"> ' +
                    '<label class="col-md-4 control-label"></label> ' +
                    '<div class="col-md-4">'+select+' </div> ' +
                    '</div> ' +
                    '</form> </div>  </div>',
                buttons: {
                    success: {
                        label: "Save",
                        className: "btn-success",
                        callback: function () {
                          mTICunchanged = KnownorUnknown.value===$('#mTICindicator').val();//if later equals the KnownorKnown, this means that nothing changed so the mTIC did should stays the same.
                            KnownorUnknown.value = $('#mTICindicator').val();
                          $( "#mTIC_column" ).text("Using: " + KnownorUnknown.value );


                          samplenormalization_method = 'mTIC'
//$("#samplenormalizationmethod").text(samplenormalization_method);
$("#applynormalization").removeClass("disabled");
                        }

                    },
                    cancel:{
                      label:"Cancel",
                      callback:function(){
                        if(samplenormalization_method=="None"){
                          $("#samplenormalizationnone").prop("checked", true)
                        }
                        if(samplenormalization_method=="Sample_specific"){
$("#samplenormalizationSample_specific").prop("checked", true)
                        }
                        if(samplenormalization_method=="mTIC"){
$("#samplenormalizationmTIC").prop("checked", true)
                        }
                        if(samplenormalization_method=="Loess"){
$("#samplenormalizationloess").prop("checked", true)
                        }
                        if(samplenormalization_method=="Batch Median"){
$("#samplenormalizationBatchMedian").prop("checked", true)
                        }


                      }
                    }
                }
            }
        );
}
















function formatTableHeader(jsonData){
    var keyls = Object.keys(jsonData);//get list of keys
    var colnames = [];
    for (var i = 0; i < keyls.length; i++) {//table header = json keys
      colnames.push({title: keyls[i], data: keyls[i], editField: keyls[i]});
    }
    return colnames;
}

function formatTableFields(jsonData){
    var keyls = Object.keys(jsonData);//get list of keys
    var colnames = [];
    for (var i = 0; i < keyls.length; i++) {//table header = json keys
      colnames.push({label: keyls[i], name: keyls[i]});
    }
    return colnames;
}


//var table = $('#Statistics_Result').DataTable();
//@function draw table
//@param id id element
//@param data array of json objects
function drawTable(id, data, filename,idSrc,tableheight="450px") {
	/*editor = new $.fn.dataTable.Editor( {
        data: data,
        table: id,
        idSrc:  idSrc,
        fields: formatTableFields(data[0])
    } );*/


  if(id === '#Statistics_Result'){
      table.destroy();
  }



$(id).empty();

	 table = $(id).DataTable( {
	   dom: 'Blfrtip',
		destroy: true,

		"scrollX": true,
		"scrollY": tableheight,
		"paging": false,
		fixedColumns: true,
		scrollCollapse: true,
    data: data,
		columns: formatTableHeader(data[0]),

		lengthChange: false,
		select:true,
       buttons: [
        {
            extend: 'csv',
            text: 'download',
			title: filename
        }
       //  ,{ extend: "create", editor: editor },
         //   { extend: "edit",   editor: editor },
        //    { extend: "remove", editor: editor },
       //     "selectRows",
       //     "selectColumns",
        //    "selectCells",
           // "selectNone"
    ]
    } );


}




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


function arraysEqual(a, b) {
  if (a === b) return true;
  if (a == null || b == null) return false;
  if (a.length != b.length) return false;

  // If you don't care about the order of the elements inside
  // the array, you should sort both arrays here.

  for (var i = 0; i < a.length; ++i) {
    if (a[i] !== b[i]) return false;
  }
  return true;
}



function listSelector(full_options,why_not_able,id,furtherlimit, default_checked){


  // why_not_able = ["numeric","",]
    // selected_options = ["disabled","",...]
var selector = '';
if(why_not_able==null){

for(var i = 0;i<full_options.length;i++){
  if(full_options[i]===default_checked[0]){
    selector += "<option selected>" + full_options[i] + "</option>";
  }else{
    selector += "<option>" + full_options[i] + "</option>";
  }

}

}else{
  if(furtherlimit==null){
for(var i = 0;i<full_options.length;i++){
  if(why_not_able[i].length<2){

  if(full_options[i]===default_checked[0]){
    selector += "<option selected>" + full_options[i] + "</option>";
  }else{
    selector += "<option data-subtext= "+ why_not_able[i]+">" + full_options[i] + "</option>";
  }


  }else{
    selector += "<option data-subtext="+ why_not_able[i]+" disabled>" + full_options[i] + "</option>";
  }
}
}else{
  for(var i = 0;i<full_options.length;i++){
  if(why_not_able[i].length<2){

    if(full_options[i]===default_checked[0]){
      selector += "<option selected>" + full_options[i] + "</option>";
    }else{
      selector += "<option data-subtext= "+ why_not_able[i]+">" + full_options[i] + "</option>";
    }



  }else{
    selector += "<option data-subtext="+ why_not_able[i]+" disabled>" + full_options[i] + "</option>";
  }

}
}
}




$(id).html(selector).selectpicker('refresh');
}



function plotPCAScore(e1,f1,p1,selected_phenotype_by_check = []){
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
//decidenormalizationability(pData_column);

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



function successmsg(text,time=20000,modified_dataset_download_address=[]){

//  $("#message").empty().append('<div class="alert alert-success alert-dismissable"><a href="#" class="close" data-dismiss="alert">&times;</a>' + text+ + '</div>');
  $("#warning-message-alert").hide();
  $("#danger-message-alert").hide()
  $("#success-message-alert-text").empty().append( text + '<a download = "file.csv" href='+modified_dataset_download_address+' role="button">Download the data uploaded.</a></div>');
           // $("#myWish").click(function showAlert() {
                $("#success-message-alert").alert();
                $("#success-message-alert").fadeTo(time, 500).slideUp(500, function(){
               $("#success-message-alert").hide();
                });
         //   });
}

function warningmsg(text,time=20000,modified_dataset_download_address=[]){
//  $("#message").empty().append('<div class="alert alert-warning alert-dismissable"><a href="#" class="close" data-dismiss="alert">&times;</a>' + text+ + '</div>');
  $("#success-message-alert").hide();
  $("#danger-message-alert").hide();
  $("#warning-message-alert-text").empty().append( text + '<a download = "file.csv" href='+modified_dataset_download_address+' role="button">Download the data uploaded.</a></div>');
           // $("#myWish").click(function showAlert() {
                $("#warning-message-alert").alert();
                $("#warning-message-alert").fadeTo(time, 500).slideUp(500, function(){
               $("#warning-message-alert").hide();
                });
         //   });
}
function smallerrormsg(text,time=20000){

//  $("#message").empty().append('<div class="alert alert-danger alert-dismissable"><a href="#" class="close" data-dismiss="alert">&times;</a>' + text+ + '</div>');
  $("#success-message-alert").hide();
  $("#warning-message-alert").hide();
  $("#danger-message-alert-text").empty().append(text);
           // $("#myWish").click(function showAlert() {
                $("#danger-message-alert").alert();
                $("#danger-message-alert").fadeTo(time, 500).slideUp(500, function(){
               $("#danger-message-alert").hide();
                });
         //   });
}

function errormsg(text){
  $("#message").empty().append('<div class="alert alert-danger alert-dismissable"><a href="#" class="close" data-dismiss="alert">&times;</a>' + text + '</div>');
}







