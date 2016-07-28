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

          pDataTable = drawTable('#View_pData',obj.phenotype, "Phenotype Data","phenotype_index");
          fDataTable = drawTable('#View_fData',obj.feature, "Feature Data","feature_index");
          fDataTable = drawTable('#View_eData',obj.expression, "Expression Data","feature_index");
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
  onewayANOVAdescription();
}else if(text.includes("independent t test")){
  ttestdescription();
}else if(text.includes("two way ANOVA")){
  twowayANOVAdescription();
}else if(text.includes("one way repeated ANOVA")){
  onewayrepeatedANOVAdescription();
}else if(text.includes("paired t test")){
  pairedttestdescription();
}else if(text.includes("two way repeated anova")){
  twowayrepeatedANOVAdescription();
}else if(text.includes("mixed two way anova")){
  mixedANOVAdescription();
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







applystatistics = function(){




















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



  var req = ocpu.call("stat_norm",{
    e : DATA.expression,f : DATA.feature,p : DATA.phenotype,
  sample_index:$("#Samples_To_Be_Deleted").val().split(','),
  mTICdid : mTICdid,Loessdid:Loessdid,medFCdid:medFCdid,BatchMediandid:BatchMediandid,
  mTIC:mTIC,Loess:Loess,medFC:medFC,BatchMedian:BatchMedian,
  sample_normalization : samplenormalization_method,data_transformation:datatransformation_method,data_scaling:datascaling_method,
  sample_specific_weight : sample_specific_weight,sample_specific_multiplyordevide : sample_specific_multiplyordevide,
  KnownorUnknown:KnownorUnknown.value,mTICunchanged:mTICunchanged,Batchunchanged:Batchunchanged,
  Loessunchanged:Loessunchanged,QCIndicator:QCIndicator,BatchIndicator:BatchIndicator,TimeIndicator:TimeIndicator,
  log_para:$('input[name="logpara"]:checked').val(),independent_factor_name_log : $('#logparaautoindependent_factor').val(),
  power_para:$('input[name="powerpara"]:checked').val(),independent_factor_name_power : $('#powerparaautoindependent_factor').val()

  },function(session_norm){

    session_norm.getObject(function(obj2){

    eData = obj2.expression;
    fData = obj2.feature;
    pData = obj2.phenotype;
    e_ori = obj2.expression_only_rm_outlier;
    p_ori = obj2.phenotype_only_rm_outlier;

console.log(ttestmethod.value+ttestFDRmethod.value+nonpara_ttestmethod.value+nonpara_ttestFDRmethod.value);

    var req=ocpu.call("stat_hypo_test",{
    e:eData,
    f:fData,
    p:pData,
    e_ori:e_ori,
    p_ori:p_ori,
    e_before:D.expression,
    p_before:D.phenotype,
    independent_factor_name:$("#independent_factor").val(),
    repeated_factor_name:$("#repeated_factor").val(),
    need_power:document.getElementById('power_analysis_needed').checked,
    desired_power: pwr,




                     onewayANOVAmethod:onewayANOVAmethod.value,nonpara_onewayANOVAmethod:nonpara_onewayANOVAmethod.value,onewayANOVAposthocmethod:onewayANOVAposthocmethod.value,nonpara_onewayANOVAposthocmethod:nonpara_onewayANOVAposthocmethod.value,

                     ttestmethod:ttestmethod.value,ttestFDRmethod:ttestFDRmethod.value,nonpara_ttestmethod:nonpara_ttestmethod.value,nonpara_ttestFDRmethod:nonpara_ttestFDRmethod.value,

                     twowayANOVAmethod:twowayANOVAmethod.value,maineffectANOVAmethod1:maineffectANOVAmethod1.value,maineffectANOVAposthocmethod1:maineffectANOVAposthocmethod1.value,maineffectANOVAmethod2:maineffectANOVAmethod2.value,maineffectANOVAposthocmethod2:maineffectANOVAposthocmethod2.value,
                     simplemaineffectANOVAmethod1:simplemaineffectANOVAmethod1.value,simplemaineffectANOVAposthocmethod1:simplemaineffectANOVAposthocmethod1.value,simplemaineffectANOVAmethod2:simplemaineffectANOVAmethod2.value,simplemaineffectANOVAposthocmethod2:simplemaineffectANOVAposthocmethod2.value,
                     nonpara_maineffectANOVAmethod1:nonpara_maineffectANOVAmethod1.value,nonpara_maineffectANOVAposthocmethod1:nonpara_maineffectANOVAposthocmethod1.value,nonpara_maineffectANOVAmethod2:nonpara_maineffectANOVAmethod2.value,nonpara_maineffectANOVAposthocmethod2:nonpara_maineffectANOVAposthocmethod2.value,
                     nonpara_simplemaineffectANOVAmethod1:nonpara_simplemaineffectANOVAmethod1.value,nonpara_simplemaineffectANOVAposthocmethod1:nonpara_simplemaineffectANOVAposthocmethod1.value,nonpara_simplemaineffectANOVAmethod2:nonpara_simplemaineffectANOVAmethod2.value,nonpara_simplemaineffectANOVAposthocmethod2:nonpara_simplemaineffectANOVAposthocmethod2.value,
                     maineffectttestmethod1:maineffectttestmethod1.value,maineffectttestposthocmethod1:maineffectttestposthocmethod1.value,maineffectttestmethod2:maineffectttestmethod2.value,maineffectttestposthocmethod2:maineffectttestposthocmethod2.value,
                     simplemaineffectttestmethod1:simplemaineffectttestmethod1.value,simplemaineffectttestposthocmethod1:simplemaineffectttestposthocmethod1.value,simplemaineffectttestmethod2:simplemaineffectttestmethod2.value,simplemaineffectttestposthocmethod2:simplemaineffectttestposthocmethod2.value,
                     nonpara_maineffectttestmethod1:nonpara_maineffectttestmethod1.value,nonpara_maineffectttestposthocmethod1:nonpara_maineffectttestposthocmethod1.value,nonpara_maineffectttestmethod2:nonpara_maineffectttestmethod2.value,nonpara_maineffectttestposthocmethod2:nonpara_maineffectttestposthocmethod2.value,
                     nonpara_simplemaineffectttestmethod1:nonpara_simplemaineffectttestmethod1.value,nonpara_simplemaineffectttestposthocmethod1:nonpara_simplemaineffectttestposthocmethod1.value,nonpara_simplemaineffectttestmethod2:nonpara_simplemaineffectttestmethod2.value,nonpara_simplemaineffectttestposthocmethod2:nonpara_simplemaineffectttestposthocmethod2.value,

                     onewayrepeatedANOVAmethod : "rANOVA",onewayrepeatedANOVAposthocmethod : "paired+bonf",onewaySpher_Corr : onewaySpher_Corr.value,
                     nonpara_onewayrepeatedANOVAmethod : "rANOVA",nonpara_onewayrepeatedANOVAposthocmethod : "nonpara_paired+bonf",

                     pairedttestmethod : "paired t test", pairedttestFDRmethod : pairedttestFDRmethod.value, nonpara_pairedttestmethod : "Wil test", nonpara_pairedttestFDRmethod : nonpara_pairedttestFDRmethod.value,

                     mainSpher_Corr1 : mainSpher_Corr1.value,mainSpher_Corr2 : mainSpher_Corr2.value,simplemainSpher_Corr1 : simplemainSpher_Corr1.value,simplemainSpher_Corr2 : simplemainSpher_Corr2.value,
                     simplemaineffectpairedttestposthocmethod1 : simplemaineffectpairedttestposthocmethod1.value, simplemaineffectpairedttestposthocmethod2 : simplemaineffectpairedttestposthocmethod2.value, nonpara_simplemaineffectpairedttestposthocmethod1 : nonpara_simplemaineffectpairedttestposthocmethod1.value,
                     nonpara_simplemaineffectpairedttestposthocmethod2 : nonpara_simplemaineffectpairedttestposthocmethod2.value,
                     maineffectpairedttestposthocmethod1 : maineffectpairedttestposthocmethod1.value,nonpara_maineffectpairedttestposthocmethod1 : nonpara_maineffectpairedttestposthocmethod1.value,

                     maineffectpairedttestposthocmethod2 : maineffectpairedttestposthocmethod2.value,nonpara_maineffectpairedttestposthocmethod2 : nonpara_maineffectpairedttestposthocmethod2.value



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
             Stat_Result = drawTable('#Statistics_Result',obj3, "Univariate Statistical Result","feature_index","600px");
allow = false;


               });

              //Stat_Result = drawTable('#Statistics_Result',obj3, "Univariate Statistical Result","feature_index");
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



  });
  }).fail(function(jqXHR){ hideSpinner(loadSpinner);bootbox.alert(jqXHR.responseText);});
  });
    }).fail(function(jqXHR){errormsg(1 + jqXHR.responseText); hideSpinner(loadSpinner);});
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

                          samplenormalization_method = 'Sample_specific'
                          if(allow){
                            applystatistics();
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

bootbox.dialog({
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
                          if(allow){
                            applystatistics();
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

bootbox.dialog({
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
                          if(allow){
                            applystatistics();
                          }
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

bootbox.dialog({
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
                          $( "#mTIC_column" ).text("Known/Unknown Indicator: " + KnownorUnknown.value );

                          samplenormalization_method = 'mTIC'
                          if(allow){
                            applystatistics();
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

//@function draw table
//@param id id element
//@param data array of json objects
function drawTable(id, data, filename,idSrc,tableheight="450px") {

	editor = new $.fn.dataTable.Editor( {
        data: data,
        table: id,
        idSrc:  idSrc,
        fields: formatTableFields(data[0])
    } );



	 $(id).DataTable( {
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








    /*$('#status').editable({
        value: 2,
        source: [
              {value: 1, text: 'Active'},
              {value: 2, text: 'Blocked'},
              {value: 3, text: 'Deleted'}
           ],
        success: function(response, newValue) {
        a = newValue//update backbone model
    }
    });
    $('#status').click(function(){
    console.log(a);
    })*/

var onewayANOVAmethod = {value : 'Welch ANOVA'};
var nonpara_onewayANOVAmethod = {value : 'H test'};
var onewayANOVAposthocmethod = {value : 'games.howell'};
var nonpara_onewayANOVAposthocmethod = {value : 'Dunn'};
var ttestmethod = {value : 'Welch t test'};
var ttestFDRmethod = {value : 'fdr'};
var nonpara_ttestmethod = {value : 'U test'};
var nonpara_ttestFDRmethod = {value : 'fdr'};
var twowayANOVAmethod = {value:"Two Way ANOVA"}
var maineffectANOVAmethod1 = {value:"Welch ANOVA"}
var maineffectANOVAmethod2 = {value:"Welch ANOVA"}
var maineffectANOVAposthocmethod1 = {value:"games.howell"}
var maineffectANOVAposthocmethod2 = {value:"games.howell"}
var simplemaineffectANOVAmethod1 = {value:"Welch ANOVA"}
var simplemaineffectANOVAmethod2 = {value:"Welch ANOVA"}
var simplemaineffectANOVAposthocmethod1 = {value:"games.howell"}
var simplemaineffectANOVAposthocmethod2 = {value:"games.howell"}
var nonpara_maineffectANOVAmethod1 = {value:"H test"}
var nonpara_maineffectANOVAmethod2 = {value:"H test"}
var nonpara_maineffectANOVAposthocmethod1 = {value:"games.howell"}
var nonpara_maineffectANOVAposthocmethod2 = {value:"games.howell"}
var nonpara_simplemaineffectANOVAmethod1 = {value:"H test"}
var nonpara_simplemaineffectANOVAmethod2 = {value:"H test"}
var nonpara_simplemaineffectANOVAposthocmethod1 = {value:"games.howell"}
var nonpara_simplemaineffectANOVAposthocmethod2 = {value:"games.howell"}
var maineffectttestmethod1 = {value:"Welch t test"}
var maineffectttestmethod2 = {value:"Welch t test"}
var maineffectttestposthocmethod1 = {value:"fdr"}
var maineffectttestposthocmethod2 = {value:"fdr"}
var simplemaineffectttestmethod1 = {value:"Welch t test"}
var simplemaineffectttestmethod2 = {value:"Welch t test"}
var simplemaineffectttestposthocmethod1 = {value:"fdr"}
var simplemaineffectttestposthocmethod2 = {value:"fdr"}
var nonpara_maineffectttestmethod1 = {value:"U test"}
var nonpara_maineffectttestmethod2 = {value:"U test"}
var nonpara_maineffectttestposthocmethod1 = {value:"fdr"}
var nonpara_maineffectttestposthocmethod2 = {value:"fdr"}
var nonpara_simplemaineffectttestmethod1 = {value:"U test"}
var nonpara_simplemaineffectttestmethod2 = {value:"U test"}
var nonpara_simplemaineffectttestposthocmethod1 = {value:"fdr"}
var nonpara_simplemaineffectttestposthocmethod2 = {value:"fdr"}

var twowayrepeatedANOVAmethod = {value:"Two Way Repeated ANOVA"}
var maineffectrepeatedANOVAmethod1 = {value:"rANOVA"}
var maineffectrepeatedANOVAmethod2 = {value:"rANOVA"}
var maineffectrepeatedANOVAposthocmethod1 = {value:"paired+bonf"}
var maineffectrepeatedANOVAposthocmethod2 = {value:"paired+bonf"}
var simplemaineffectrepeatedANOVAmethod1 = {value:"rANOVA"}
var simplemaineffectrepeatedANOVAmethod2 = {value:"rANOVA"}
var simplemaineffectrepeatedANOVAposthocmethod1 = {value:"paired+bonf"}
var simplemaineffectrepeatedANOVAposthocmethod2 = {value:"paired+bonf"}
var nonpara_maineffectrepeatedANOVAmethod1 = {value:"friedman"}
var nonpara_maineffectrepeatedANOVAmethod2 = {value:"friedman"}
var nonpara_maineffectrepeatedANOVAposthocmethod1 = {value:"nonpara_paired+bonf"}
var nonpara_maineffectrepeatedANOVAposthocmethod2 = {value:"nonpara_paired+bonf"}
var nonpara_simplemaineffectrepeatedANOVAmethod1 = {value:"friedman"}
var nonpara_simplemaineffectrepeatedANOVAmethod2 = {value:"friedman"}
var nonpara_simplemaineffectrepeatedANOVAposthocmethod1 = {value:"nonpara_paired+bonf"}
var nonpara_simplemaineffectrepeatedANOVAposthocmethod2 = {value:"nonpara_paired+bonf"}
var maineffectpairedttestmethod1 = {value:"paired t test"}
var maineffectpairedttestmethod2 = {value:"paired t test"}
var maineffectpairedttestposthocmethod1 = {value:"fdr"}
var maineffectpairedttestposthocmethod2 = {value:"fdr"}
var simplemaineffectpairedttestmethod1 = {value:"paired t test"}
var simplemaineffectpairedttestmethod2 = {value:"paired t test"}
var simplemaineffectpairedttestposthocmethod1 = {value:"fdr"}
var simplemaineffectpairedttestposthocmethod2 = {value:"fdr"}
var nonpara_maineffectpairedttestmethod1 = {value:"Wil test"}
var nonpara_maineffectpairedttestmethod2 = {value:"Wil test"}
var nonpara_maineffectpairedttestposthocmethod1 = {value:"fdr"}
var nonpara_maineffectpairedttestposthocmethod2 = {value:"fdr"}
var nonpara_simplemaineffectpairedttestmethod1 = {value:"Wil test"}
var nonpara_simplemaineffectpairedttestmethod2 = {value:"Wil test"}
var nonpara_simplemaineffectpairedttestposthocmethod1 = {value:"fdr"}
var nonpara_simplemaineffectpairedttestposthocmethod2 = {value:"fdr"}
var mainSpher_Corr1 = {value:"GG"}
var mainSpher_Corr2 = {value:"GG"}
var simplemainSpher_Corr1 = {value:"GG"}
var simplemainSpher_Corr2 = {value:"GG"}


var onewayrepeatedANOVAmethod = {value:"rANOVA"}
var onewayrepeatedANOVAposthocmethod = {value:"paired+bonf"}
var onewaySpher_Corr = {value:"GG"}
var nonpara_onewayrepeatedANOVAmethod = {value:"friedman"}
var nonpara_onewayrepeatedANOVAposthocmethod = {value:"nonpara_paired+bonf"}

var pairedttestmethod = {value:"paired t test"}
var pairedttestFDRmethod = {value:"fdr"}
var nonpara_pairedttestmethod = {value:"Wil test"}
var nonpara_pairedttestFDRmethod = {value:"fdr"}
// method specification.
// id = method_description.


onewayANOVAdescription = function(){

$("#method_description").html(
'Use <a href="#" id="onewayANOVAmethod" data-type="select"  data-title="Select ANOVA method"></a>'+
' on <strong>' +$("#independent_factor").val()+ '</strong>. '+
'<br>Use <a href="#" id="onewayANOVAposthocmethod" data-type="select"  data-title="Select post hoc method"></a>'+
' to compare two-group combination of <strong>' + pComponents[$("#independent_factor").val()[0]].join(",")+"</strong>. " +

'<br> Use '+
'<a href="#" id="nonpara_onewayANOVAmethod" data-type="select" data-title="Select non-parametric ANOVA method"></a>, a non-parametric  test, ' +
' on <strong>' +$("#independent_factor").val()+ '</strong>. '+
'<br>Use <a href="#" id="nonpara_onewayANOVAposthocmethod" data-type="select"  data-title="Select non-parametric post hoc method"></a>'+
' to compare two-group combination of <strong>' + pComponents[$("#independent_factor").val()[0]].join(",")+"</strong>. "
)




}

ttestdescription = function(){

$("#method_description").html(
'Use <a href="#" id="ttestmethod" data-type="select"  data-title="Select t test method"></a>'+
' on <strong>' +$("#independent_factor").val()+ '</strong>' +
' to compare <strong>'+
pComponents[$("#independent_factor").val()[0]].join("</strong> and <strong>")+"</strong>. " +

'<br>Use <a href="#" id="ttestFDRmethod" data-type="select"  data-title="Select FDR correction method"></a>'+
' to correct for multiple comparisons.' +

'<br> Use non-parametric  test, '+
'<a href="#" id="nonpara_ttestmethod" data-type="select" data-title="Select non-parametric t test method"></a>' +
' on <strong>' +$("#independent_factor").val()+ '</strong>'+
' to compare <strong>'+
pComponents[$("#independent_factor").val()[0]].join("</strong> and <strong>") +"</strong>. "+
'<br>Use <a href="#" id="nonpara_ttestFDRmethod" data-type="select" data-title="Select FDR correction method"></a> on non-parametric test result' +
' to correct for the multiple comparisons.'
)


}

twowayANOVAdescription = function(){



/*'Subjects were classified into <strong>'+
pComponents[$("#independent_factor").val()[0]].join("</strong> and <strong>")+"</strong>. " +*/
// Main Effect
if(pComponents[$("#independent_factor").val()[0]].length>2){
  Main_Effect1 = '<br>Use <a href="#" id="maineffectANOVAmethod1" data-type="select"  data-title="Select ANOVA method for first main effect"></a>' + ' for the main effect of <strong>' + $("#independent_factor").val()[0] + '</strong>.'+
  ' Then use post hoc analysis procedure <a href="#" id="maineffectANOVAposthocmethod1" data-type="select"  data-title="Select post hoc method for first main effect"></a>'+
'<br>Use non-parametric test, <a href="#" id="nonpara_maineffectANOVAmethod1" data-type="select"  data-title="Select non-parametric ANOVA method for first main effect"></a>' + ' on main effect of <strong>' + $("#independent_factor").val()[0] + '</strong>.'+
  ' Then use post hoc anlaysis procedure of <a href="#" id="nonpara_maineffectANOVAposthocmethod1" data-type="select"  data-title="Select non-parametric post hoc method for first main effect"></a>'
}else{
  Main_Effect1 = '<br> Use <a href="#" id="maineffectttestmethod1" data-type="select"  data-title="Select t test method for first main effect"></a>' + ' for the main effect of <strong>' + $("#independent_factor").val()[0] + '</strong>.'+
  ' Then use <a href="#" id="maineffectttestposthocmethod1" data-type="select"  data-title="Select FDR for first main effect"></a>'+' to ajust the false discovery rate (FDR).'+
'<br>Use non-parametric test <a href="#" id="nonpara_maineffectttestmethod1" data-type="select"  data-title="Select non-parametric t test method for first main effect"></a>' + ' for the main effect of <strong>' + $("#independent_factor").val()[0] + '</strong>' +
  ' Then use <a href="#" id="nonpara_maineffectttestposthocmethod1" data-type="select"  data-title="Select FDR for first main effect"></a>'+ ' to ajust the false discovery rate (FDR).'
}

if(pComponents[$("#independent_factor").val()[0]].length>2){
  Simple_Main_Effect1 = '<br> Use <a href="#" id="simplemaineffectANOVAmethod1" data-type="select"  data-title="Select ANOVA method for first simple main effect"></a>' + ' on simple main effect of <strong>' + $("#independent_factor").val()[0] + '</strong> controlling at each level of ' + $("#independent_factor").val()[1] + "," +
  ' followed by post hoc analysis procedure <a href="#" id="simplemaineffectANOVAposthocmethod1" data-type="select"  data-title="Select post hoc method for first simple main effect"></a>'+
'<br>Use non-parametric test <a href="#" id="nonpara_simplemaineffectANOVAmethod1" data-type="select"  data-title="Select non-parametric ANOVA method for first simple main effect"></a>' + 'on simple main effect of <strong>' + $("#independent_factor").val()[0] + '</strong> controlling at each level of '+$("#independent_factor").val()[1] + "." +
  ' followed by post hoc analysis procedure <a href="#" id="nonpara_simplemaineffectANOVAposthocmethod1" data-type="select"  data-title="Select non-parametric post hoc method for first simple main effect"></a>'
}else{
  Simple_Main_Effect1 = '<br> Use <a href="#" id="simplemaineffectttestmethod1" data-type="select"  data-title="Select t test method for first simple main effect"></a>' + ' for the simple main effect of <strong>' + $("#independent_factor").val()[0] + '</strong> at each level of ' +$("#independent_factor").val()[1] +
  ', followed by <a href="#" id="simplemaineffectttestposthocmethod1" data-type="select"  data-title="Select FDR for first simple main effect"></a> to adjust the false discovery rate (FDR). '+

'<br>Use non parametric test, <a href="#" id="nonpara_simplemaineffectttestmethod1" data-type="select"  data-title="Select non-parametric t test method for first simple main effect"></a>' + ' for the simple main effect of <strong>' + $("#independent_factor").val()[0] + '</strong>.' +
  ' followed by <a href="#" id="nonpara_simplemaineffectttestposthocmethod1" data-type="select"  data-title="Select FDR for first simple main effect"></a> to adjust the false discovery rate (FDR). '

}




if(pComponents[$("#independent_factor").val()[1]].length>2){
  Main_Effect2 = '<br>Use <a href="#" id="maineffectANOVAmethod2" data-type="select"  data-title="Select ANOVA method for first main effect"></a>' + ' for the main effect of <strong>' + $("#independent_factor").val()[1] + '</strong>.'+
  ' Then use post hoc analysis procedure <a href="#" id="maineffectANOVAposthocmethod2" data-type="select"  data-title="Select post hoc method for first main effect"></a>'+
'<br>Use non-parametric test, <a href="#" id="nonpara_maineffectANOVAmethod2" data-type="select"  data-title="Select non-parametric ANOVA method for first main effect"></a>' + ' on main effect of <strong>' + $("#independent_factor").val()[1] + '</strong>.'+
  ' Then use post hoc anlaysis procedure of <a href="#" id="nonpara_maineffectANOVAposthocmethod2" data-type="select"  data-title="Select non-parametric post hoc method for first main effect"></a>'
}else{
  Main_Effect2 = '<br> Use <a href="#" id="maineffectttestmethod2" data-type="select"  data-title="Select t test method for first main effect"></a>' + ' for the main effect of <strong>' + $("#independent_factor").val()[1] + '</strong>.'+
  ' Then use <a href="#" id="maineffectttestposthocmethod2" data-type="select"  data-title="Select FDR for first main effect"></a>'+' to ajust the false discovery rate (FDR).'+
'<br>Use non-parametric test <a href="#" id="nonpara_maineffectttestmethod2" data-type="select"  data-title="Select non-parametric t test method for first main effect"></a>' + ' for the main effect of <strong>' + $("#independent_factor").val()[1] + '</strong>' +
  ' Then use <a href="#" id="nonpara_maineffectttestposthocmethod2" data-type="select"  data-title="Select FDR for first main effect"></a>'+ ' to ajust the false discovery rate (FDR).'
}

if(pComponents[$("#independent_factor").val()[1]].length>2){
  Simple_Main_Effect2 = '<br> Use <a href="#" id="simplemaineffectANOVAmethod2" data-type="select"  data-title="Select ANOVA method for first simple main effect"></a>' + ' on simple main effect of <strong>' + $("#independent_factor").val()[1] + '</strong> controlling at each level of ' + $("#independent_factor").val()[1] + "," +
  ' followed by post hoc analysis procedure <a href="#" id="simplemaineffectANOVAposthocmethod2" data-type="select"  data-title="Select post hoc method for first simple main effect"></a>'+
'<br>Use non-parametric test <a href="#" id="nonpara_simplemaineffectANOVAmethod2" data-type="select"  data-title="Select non-parametric ANOVA method for first simple main effect"></a>' + ' on simple main effect of <strong>' + $("#independent_factor").val()[1] + '</strong> controlling at each level of '+$("#independent_factor").val()[1] + "." +
  ' followed by post hoc analysis procedure <a href="#" id="nonpara_simplemaineffectANOVAposthocmethod2" data-type="select"  data-title="Select non-parametric post hoc method for first simple main effect"></a>'
}else{
  Simple_Main_Effect2 = '<br> Use <a href="#" id="simplemaineffectttestmethod2" data-type="select"  data-title="Select t test method for first simple main effect"></a>' + ' for the simple main effect of <strong>' + $("#independent_factor").val()[1] + '</strong> at each level of ' +$("#independent_factor").val()[2] +
  ', followed by <a href="#" id="simplemaineffectttestposthocmethod2" data-type="select"  data-title="Select FDR for first simple main effect"></a> to adjust the false discovery rate (FDR). '+

'<br>Use non parametric test, <a href="#" id="nonpara_simplemaineffectttestmethod2" data-type="select"  data-title="Select non-parametric t test method for first simple main effect"></a>' + ' for the simple main effect of <strong>' + $("#independent_factor").val()[1] + '</strong>.' +
  ' followed by <a href="#" id="nonpara_simplemaineffectttestposthocmethod2" data-type="select"  data-title="Select FDR for first simple main effect"></a> to adjust the false discovery rate (FDR). '
}

$("#method_description").html(
'Use <a href="#" id="twowayANOVAmethod" data-type="select"  data-title="Select two way ANOVA method"></a>'+
' for <strong>Interaction term</strong> between <strong>' + $("#independent_factor").val()[0]+ '</strong> and <strong>'+$("#independent_factor").val()[1]+'</strong>. ' +
Main_Effect1 + Simple_Main_Effect1 +  Main_Effect2 + Simple_Main_Effect2)
}




twowayrepeatedANOVAdescription = function(){


/*'Subjects were classified into <strong>'+
pComponents[$("#repeated_factor").val()[0]].join("</strong> and <strong>")+"</strong>. " +*/
// Main Effect
if(pComponents[$("#repeated_factor").val()[0]].length>2){
  Main_Effect1 = '<br>Use <a href="#" id="maineffectrepeatedANOVAmethod1" data-type="select"  data-title="Select repeated ANOVA method for first main effect"></a>' + ' for the main effect of <strong>' + $("#repeated_factor").val()[0] + '</strong>.'+
  ' with a <a href="#" id="mainSpher_Corr1" data-type="select"  data-title="Correct for sphericity"></a> correction.'+
'<br>Use non-parametric test, <a href="#" id="nonpara_maineffectrepeatedANOVAmethod1" data-type="select"  data-title="Select non-parametric repeated ANOVA method for first main effect"></a>' + ' on main effect of <strong>' + $("#repeated_factor").val()[0] + '</strong>.'+
  ' Then use post hoc anlaysis procedure of <a href="#" id="nonpara_maineffectrepeatedANOVAposthocmethod1" data-type="select"  data-title="Select non-parametric post hoc method for first main effect"></a>'
}else{
  Main_Effect1 = '<br> Use <a href="#" id="maineffectpairedttestmethod1" data-type="select"  data-title="Select t test method for first main effect"></a>' + ' for the main effect of <strong>' + $("#repeated_factor").val()[0] + '</strong>.'+
  ' Then use <a href="#" id="maineffectpairedttestposthocmethod1" data-type="select"  data-title="Select FDR for first main effect"></a>'+' to ajust the false discovery rate (FDR).'+
'<br>Use non-parametric test <a href="#" id="nonpara_maineffectpairedttestmethod1" data-type="select"  data-title="Select non-parametric t test method for first main effect"></a>' + ' for the main effect of <strong>' + $("#repeated_factor").val()[0] + '</strong>' +
  ' Then use <a href="#" id="nonpara_maineffectpairedttestposthocmethod1" data-type="select"  data-title="Select FDR for first main effect"></a>'+ ' to ajust the false discovery rate (FDR).'
}

if(pComponents[$("#repeated_factor").val()[0]].length>2){
  Simple_Main_Effect1 = '<br> Use <a href="#" id="simplemaineffectrepeatedANOVAmethod1" data-type="select"  data-title="Select repeated ANOVA method for first simple main effect"></a>' + ' on simple main effect of <strong>' + $("#repeated_factor").val()[0] + '</strong> controlling at each level of ' + $("#repeated_factor").val()[1] + "," +
  ' with a <a href="#" id="simplemainSpher_Corr1" data-type="select"  data-title="Correct for sphericity"></a> correction.'+
'<br>Use non-parametric test <a href="#" id="nonpara_simplemaineffectrepeatedANOVAmethod1" data-type="select"  data-title="Select non-parametric repeated ANOVA method for first simple main effect"></a>' + ' on simple main effect of <strong>' + $("#repeated_factor").val()[0] + '</strong> controlling at each level of '+$("#repeated_factor").val()[1] + ", " +
  ' followed by post hoc analysis procedure <a href="#" id="nonpara_simplemaineffectrepeatedANOVAposthocmethod1" data-type="select"  data-title="Select non-parametric post hoc method for first simple main effect"></a>'
}else{
  Simple_Main_Effect1 = '<br> Use <a href="#" id="simplemaineffectpairedttestmethod1" data-type="select"  data-title="Select t test method for first simple main effect"></a>' + ' for the simple main effect of <strong>' + $("#repeated_factor").val()[0] + '</strong> at each level of ' +$("#repeated_factor").val()[1] +
  ', followed by <a href="#" id="simplemaineffectpairedttestposthocmethod1" data-type="select"  data-title="Select FDR for first simple main effect"></a> to adjust the false discovery rate (FDR). '+

'<br>Use non parametric test, <a href="#" id="nonpara_simplemaineffectpairedttestmethod1" data-type="select"  data-title="Select non-parametric t test method for first simple main effect"></a>' + ' for the simple main effect of <strong>' + $("#repeated_factor").val()[0] + '</strong>.' +
  ' followed by <a href="#" id="nonpara_simplemaineffectpairedttestposthocmethod1" data-type="select"  data-title="Select FDR for first simple main effect"></a> to adjust the false discovery rate (FDR). '

}




if(pComponents[$("#repeated_factor").val()[1]].length>2){
  Main_Effect2 = '<br>Use <a href="#" id="maineffectrepeatedANOVAmethod2" data-type="select"  data-title="Select repeated ANOVA method for first main effect"></a>' + ' for the main effect of <strong>' + $("#repeated_factor").val()[1] + '</strong>.'+
  ' with a <a href="#" id="mainSpher_Corr2" data-type="select"  data-title="Correct for sphericity"></a> correction.'+
'<br>Use non-parametric test, <a href="#" id="nonpara_maineffectrepeatedANOVAmethod2" data-type="select"  data-title="Select non-parametric repeated ANOVA method for first main effect"></a>' + ' on main effect of <strong>' + $("#repeated_factor").val()[1] + '</strong>.'+
  ' Then use post hoc anlaysis procedure of <a href="#" id="nonpara_maineffectrepeatedANOVAposthocmethod2" data-type="select"  data-title="Select non-parametric post hoc method for first main effect"></a>'
}else{
  Main_Effect2 = '<br> Use <a href="#" id="maineffectpairedttestmethod2" data-type="select"  data-title="Select t test method for first main effect"></a>' + ' for the main effect of <strong>' + $("#repeated_factor").val()[1] + '</strong>.'+
  ' Then use <a href="#" id="maineffectpairedttestposthocmethod2" data-type="select"  data-title="Select FDR for first main effect"></a>'+' to ajust the false discovery rate (FDR).'+
'<br>Use non-parametric test <a href="#" id="nonpara_maineffectpairedttestmethod2" data-type="select"  data-title="Select non-parametric t test method for first main effect"></a>' + ' for the main effect of <strong>' + $("#repeated_factor").val()[1] + '</strong>' +
  ' Then use <a href="#" id="nonpara_maineffectpairedttestposthocmethod2" data-type="select"  data-title="Select FDR for first main effect"></a>'+ ' to ajust the false discovery rate (FDR).'
}

if(pComponents[$("#repeated_factor").val()[1]].length>2){
  Simple_Main_Effect2 = '<br> Use <a href="#" id="simplemaineffectrepeatedANOVAmethod2" data-type="select"  data-title="Select repeated ANOVA method for first simple main effect"></a>' + ' on simple main effect of <strong>' + $("#repeated_factor").val()[1] + '</strong> controlling at each level of ' + $("#repeated_factor").val()[1] + "," +
  ' with a <a href="#" id="simplemainSpher_Corr2" data-type="select"  data-title="Correct for sphericity"></a> correction.'+
'<br>Use non-parametric test <a href="#" id="nonpara_simplemaineffectrepeatedANOVAmethod2" data-type="select"  data-title="Select non-parametric repeated ANOVA method for first simple main effect"></a>' + ' on simple main effect of <strong>' + $("#repeated_factor").val()[1] + '</strong> controlling at each level of '+$("#repeated_factor").val()[1] + ", " +
  ' followed by post hoc analysis procedure <a href="#" id="nonpara_simplemaineffectrepeatedANOVAposthocmethod2" data-type="select"  data-title="Select non-parametric post hoc method for first simple main effect"></a>'
}else{
  Simple_Main_Effect2 = '<br> Use <a href="#" id="simplemaineffectpairedttestmethod2" data-type="select"  data-title="Select t test method for first simple main effect"></a>' + ' for the simple main effect of <strong>' + $("#repeated_factor").val()[1] + '</strong> at each level of ' +$("#repeated_factor").val()[1] +
  ', followed by <a href="#" id="simplemaineffectpairedttestposthocmethod2" data-type="select"  data-title="Select FDR for first simple main effect"></a> to adjust the false discovery rate (FDR). '+

'<br>Use non parametric test, <a href="#" id="nonpara_simplemaineffectpairedttestmethod2" data-type="select"  data-title="Select non-parametric t test method for first simple main effect"></a>' + ' for the simple main effect of <strong>' + $("#repeated_factor").val()[1] + '</strong>.' +
  ' followed by <a href="#" id="nonpara_simplemaineffectpairedttestposthocmethod2" data-type="select"  data-title="Select FDR for first simple main effect"></a> to adjust the false discovery rate (FDR). '

}




$("#method_description").html(
'Use <a href="#" id="twowayrepeatedANOVAmethod" data-type="select"  data-title="Select two way repeated ANOVA method"></a>'+
' for <strong>Interaction term</strong> between <strong>' + $("#repeated_factor").val()[0]+ '</strong> and <strong>'+$("#repeated_factor").val()[1]+'</strong>. ' +
Main_Effect1 + Simple_Main_Effect1 +  Main_Effect2 + Simple_Main_Effect2)


}







pairedttestdescription = function(){

$("#method_description").html(
'Use <a href="#" id="pairedttestmethod" data-type="select"  data-title="Select paired t test method"></a>'+
' on <strong>' +$("#repeated_factor").val()+ '</strong>' +
' to compare <strong>'+
pComponents[$("#repeated_factor").val()[0]].join("</strong> and <strong>")+"</strong>. " +

'<br>Use <a href="#" id="pairedttestFDRmethod" data-type="select"  data-title="Select FDR correction method"></a>'+
' to correct for multiple comparisons.' +

'<br> Use non-parametric  test, '+
'<a href="#" id="nonpara_pairedttestmethod" data-type="select" data-title="Select non-parametric paired t test method"></a>' +
' on <strong>' +$("#repeated_factor").val()+ '</strong>'+
' to compare <strong>'+
pComponents[$("#repeated_factor").val()[0]].join("</strong> and <strong>") +"</strong>. "+
'<br>Use <a href="#" id="nonpara_pairedttestFDRmethod" data-type="select" data-title="Select FDR correction method"></a> on non-parametric test result' +
' to correct for the multiple comparisons.'
)


}
onewayrepeatedANOVAdescription = function(){
$("#method_description").html(
'Use <a href="#" id="onewayrepeatedANOVAmethod" data-type="select"  data-title="Select repeated ANOVA method"></a>'+
' on <strong>' +$("#repeated_factor").val()+ '</strong> with '+'<a href="#" id="onewaySpher_Corr" data-type="select"  data-title="Shericity Correction Method"></a>' + ' correction. Then use <a href="#" id="onewayrepeatedANOVAposthocmethod" data-type="select"  data-title="Select (repeated) post hoc method"></a>'+
' to perform paire-wise compairson.'+

'<br> Use non-parametric test, '+
'<a href="#" id="nonpara_onewayrepeatedANOVAmethod" data-type="select" data-title="Select non-parametric repeated ANOVA method"></a>, a non-parametric  test, ' +
' on <strong>' +$("#repeated_factor").val()+ '</strong>. Then use <a href="#" id="nonpara_onewayrepeatedANOVAposthocmethod" data-type="select"  data-title="Select non-parametric (repeated) post hoc method"></a>'+
' to perform paire-wise compairson.'
)
};






mixedANOVAdescription = function(){



/*'Subjects were classified into <strong>'+
pComponents[$("#independent_factor").val()[0]].join("</strong> and <strong>")+"</strong>. " +*/
// Main Effect
if(pComponents[$("#independent_factor").val()[0]].length>2){
  Main_Effect1 = '<br>Use <a href="#" id="maineffectANOVAmethod1" data-type="select"  data-title="Select ANOVA method for first main effect"></a>' + ' for the main effect of <strong>' + $("#independent_factor").val()[0] + '</strong>.'+
  ' Then use post hoc analysis procedure <a href="#" id="maineffectANOVAposthocmethod1" data-type="select"  data-title="Select post hoc method for first main effect"></a>'+
'<br>Use non-parametric test, <a href="#" id="nonpara_maineffectANOVAmethod1" data-type="select"  data-title="Select non-parametric ANOVA method for first main effect"></a>' + ' on main effect of <strong>' + $("#independent_factor").val()[0] + '</strong>.'+
  ' Then use post hoc anlaysis procedure of <a href="#" id="nonpara_maineffectANOVAposthocmethod1" data-type="select"  data-title="Select non-parametric post hoc method for first main effect"></a>'
}else{
  Main_Effect1 = '<br> Use <a href="#" id="maineffectttestmethod1" data-type="select"  data-title="Select t test method for first main effect"></a>' + ' for the main effect of <strong>' + $("#independent_factor").val()[0] + '</strong>.'+
  ' Then use <a href="#" id="maineffectttestposthocmethod1" data-type="select"  data-title="Select FDR for first main effect"></a>'+' to ajust the false discovery rate (FDR).'+
'<br>Use non-parametric test <a href="#" id="nonpara_maineffectttestmethod1" data-type="select"  data-title="Select non-parametric t test method for first main effect"></a>' + ' for the main effect of <strong>' + $("#independent_factor").val()[0] + '</strong>' +
  ' Then use <a href="#" id="nonpara_maineffectttestposthocmethod1" data-type="select"  data-title="Select FDR for first main effect"></a>'+ ' to ajust the false discovery rate (FDR).'
}

if(pComponents[$("#independent_factor").val()[0]].length>2){
  Simple_Main_Effect1 = '<br> Use <a href="#" id="simplemaineffectANOVAmethod1" data-type="select"  data-title="Select ANOVA method for first simple main effect"></a>' + ' on simple main effect of <strong>' + $("#independent_factor").val()[0] + '</strong> controlling at each level of ' + $("#independent_factor").val()[1] + "," +
  ' followed by post hoc analysis procedure <a href="#" id="simplemaineffectANOVAposthocmethod1" data-type="select"  data-title="Select post hoc method for first simple main effect"></a>'+
'<br>Use non-parametric test <a href="#" id="nonpara_simplemaineffectANOVAmethod1" data-type="select"  data-title="Select non-parametric ANOVA method for first simple main effect"></a>' + 'on simple main effect of <strong>' + $("#independent_factor").val()[0] + '</strong> controlling at each level of '+$("#independent_factor").val()[1] + "." +
  ' followed by post hoc analysis procedure <a href="#" id="nonpara_simplemaineffectANOVAposthocmethod1" data-type="select"  data-title="Select non-parametric post hoc method for first simple main effect"></a>'
}else{
  Simple_Main_Effect1 = '<br> Use <a href="#" id="simplemaineffectttestmethod1" data-type="select"  data-title="Select t test method for first simple main effect"></a>' + ' for the simple main effect of <strong>' + $("#independent_factor").val()[0] + '</strong> at each level of ' +$("#independent_factor").val()[1] +
  ', followed by <a href="#" id="simplemaineffectttestposthocmethod1" data-type="select"  data-title="Select FDR for first simple main effect"></a> to adjust the false discovery rate (FDR). '+

'<br>Use non parametric test, <a href="#" id="nonpara_simplemaineffectttestmethod1" data-type="select"  data-title="Select non-parametric t test method for first simple main effect"></a>' + ' for the simple main effect of <strong>' + $("#independent_factor").val()[0] + '</strong>.' +
  ' followed by <a href="#" id="nonpara_simplemaineffectttestposthocmethod1" data-type="select"  data-title="Select FDR for first simple main effect"></a> to adjust the false discovery rate (FDR). '

}


if(pComponents[$("#repeated_factor").val()[0]].length>2){
  Main_Effect2 = '<br>Use <a href="#" id="maineffectrepeatedANOVAmethod2" data-type="select"  data-title="Select repeated ANOVA method for first main effect"></a>' + ' for the main effect of <strong>' + $("#repeated_factor").val()[0] + '</strong>.'+
  ' with a <a href="#" id="mainSpher_Corr2" data-type="select"  data-title="Correct for sphericity"></a> correction.'+
'<br>Use non-parametric test, <a href="#" id="nonpara_maineffectrepeatedANOVAmethod2" data-type="select"  data-title="Select non-parametric repeated ANOVA method for first main effect"></a>' + ' on main effect of <strong>' + $("#repeated_factor").val()[0] + '</strong>.'+
  ' Then use post hoc anlaysis procedure of <a href="#" id="nonpara_maineffectrepeatedANOVAposthocmethod2" data-type="select"  data-title="Select non-parametric post hoc method for first main effect"></a>'
}else{
  Main_Effect2 = '<br> Use <a href="#" id="maineffectpairedttestmethod2" data-type="select"  data-title="Select t test method for first main effect"></a>' + ' for the main effect of <strong>' + $("#repeated_factor").val()[0] + '</strong>.'+
  ' Then use <a href="#" id="maineffectpairedttestposthocmethod2" data-type="select"  data-title="Select FDR for first main effect"></a>'+' to ajust the false discovery rate (FDR).'+
'<br>Use non-parametric test <a href="#" id="nonpara_maineffectpairedttestmethod2" data-type="select"  data-title="Select non-parametric t test method for first main effect"></a>' + ' for the main effect of <strong>' + $("#repeated_factor").val()[0] + '</strong>' +
  ' Then use <a href="#" id="nonpara_maineffectpairedttestposthocmethod2" data-type="select"  data-title="Select FDR for first main effect"></a>'+ ' to ajust the false discovery rate (FDR).'
}

if(pComponents[$("#repeated_factor").val()[0]].length>2){
  Simple_Main_Effect2 = '<br> Use <a href="#" id="simplemaineffectrepeatedANOVAmethod2" data-type="select"  data-title="Select repeated ANOVA method for first simple main effect"></a>' + ' on simple main effect of <strong>' + $("#repeated_factor").val()[0] + '</strong> controlling at each level of ' + $("#repeated_factor").val()[0] + "," +
  ' with a <a href="#" id="simplemainSpher_Corr2" data-type="select"  data-title="Correct for sphericity"></a> correction.'+
'<br>Use non-parametric test <a href="#" id="nonpara_simplemaineffectrepeatedANOVAmethod2" data-type="select"  data-title="Select non-parametric repeated ANOVA method for first simple main effect"></a>' + ' on simple main effect of <strong>' + $("#repeated_factor").val()[0] + '</strong> controlling at each level of '+$("#repeated_factor").val()[0] + ", " +
  ' followed by post hoc analysis procedure <a href="#" id="nonpara_simplemaineffectrepeatedANOVAposthocmethod2" data-type="select"  data-title="Select non-parametric post hoc method for first simple main effect"></a>'
}else{
  Simple_Main_Effect2 = '<br> Use <a href="#" id="simplemaineffectpairedttestmethod2" data-type="select"  data-title="Select t test method for first simple main effect"></a>' + ' for the simple main effect of <strong>' + $("#repeated_factor").val()[0] + '</strong> at each level of ' +$("#repeated_factor").val()[0] +
  ', followed by <a href="#" id="simplemaineffectpairedttestposthocmethod2" data-type="select"  data-title="Select FDR for first simple main effect"></a> to adjust the false discovery rate (FDR). '+

'<br>Use non parametric test, <a href="#" id="nonpara_simplemaineffectpairedttestmethod2" data-type="select"  data-title="Select non-parametric t test method for first simple main effect"></a>' + ' for the simple main effect of <strong>' + $("#repeated_factor").val()[0] + '</strong>.' +
  ' followed by <a href="#" id="nonpara_simplemaineffectpairedttestposthocmethod2" data-type="select"  data-title="Select FDR for first simple main effect"></a> to adjust the false discovery rate (FDR). '

}
$("#method_description").html(
'Use <a href="#" id="mixedANOVAmethod" data-type="select"  data-title="Select two way ANOVA method"></a>'+
' on <strong>Interaction term</strong> between </strong>' + $("#independent_factor").val()[0]+ '</strong> and <strong>'+$("#repeated_factor").val()[0]+'</strong>. ' +
Main_Effect1 + Simple_Main_Effect1 +  Main_Effect2 + Simple_Main_Effect2)
}



