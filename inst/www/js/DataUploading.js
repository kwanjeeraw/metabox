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

    var aggregated; //This is the session of list containing three data.frames ("expression","feature","phenotype")
    $("#InputAggregatedData").on("change", function(){
    //verify that a file is selected
    if($("#csvfile")[0].files[0]){

      //clear state
      aggregated = null;
      var x = document.getElementById("InputAggregatedData");
      document.getElementById("demo").innerHTML = x.files[0].name;
    }
  });





























});
