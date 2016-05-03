$(document).ready(function(){//wait untile the document is fully loaded.


    $('input[name="uploadtype"]').click(function(){    // Decide display input one file or three files.
         if($(this).attr("value")=="upload_aggregated_datasets"){
           $(".aggregated").show("fast");
           $(".expression_feature_phenotype").hide("fast");

         }else if($(this).attr("value")=="upload_expression_feature_phenotype_datasets"){
           $(".aggregated").hide("fast");
           $(".expression_feature_phenotype").show("fast");
         }else{
           $(".aggregated").hide("fast");
           $(".expression_feature_phenotype").hide("fast");
         }
    });

    //var aggregated; //This is the session of list containing three data.frames ("expression","feature","phenotype")
    var e;
    var f;
    var p;
    aggregated = {};

    $("#InputAggregatedData").on("change", function(){

    $("#successdiv").empty();
    $("#errordiv").empty();



    $("#FilesUploaded").hide();
    $("#waitingFiles").show();
    //verify that a file is selected
    if($("#InputAggregatedData")[0].files[0]){

      //clear state
     // aggregated = null;
      var x = document.getElementById("InputAggregatedData");
      var req=ocpu.call("load_aggregated_data",{
        file: $("#InputAggregatedData")[0].files[0],
        type: x.files[0].name.indexOf('xlsx') !== -1
      },function(session){

        aggregated = session;
        aggregated.getFile("messages.txt", function(text){
          successmsg(text);
            });

        $("#waitingFiles").hide("slow");
        $("#FilesUploaded").show("slow");
      }).fail(function(jqXHR){
      errormsg(jqXHR.responseText);
    });
    }
  });

    $("#TRY").click(function(){
              aggregated.getObject(function(obj){
          e=obj.expression;
          $("#demo").text(e.length);
          f=obj.feature;
          p=obj.phenotype;
        });
    });




  function successmsg(text){
    $("#successdiv").empty().append('<div class="alert alert-success alert-dismissable"><a href="#" class="close" data-dismiss="alert">&times;</a>' + text + '</div>');
  }

  function errormsg(text){
    $("#errordiv").empty().append('<div class="alert alert-danger alert-dismissable"><a href="#" class="close" data-dismiss="alert">&times;</a>' + text + '</div>');
  }


























});
