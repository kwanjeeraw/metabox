
$(document).ready(function(){//wait untile the document is fully loaded.

    // Decide display input one file or three files.
    $('input[name="uploadtype"]').click(function(){
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
    $("#InputAggregatedData").on("change", function(){
          if($("#InputAggregatedData")[0].files[0]){//make sure the file is loaded.
              var showall = document.getElementsByClassName("showall");
              for (var i = 0; i < showall.length; i ++) {
                      showall[i].style.display = 'none';
                  }
                  document.getElementById("data_summary_tag_heading").innerHTML = "Experimental Design";



          }
    });





































});
