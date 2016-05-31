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

    var e;
    var req=ocpu.call("summary_aggregated_data",{e:e,p:2,f:2},function(session){
      session.getObject(function(obj){
        $("#is_Waiting").text(obj.warnings);
      });
    });

    $("#FilesUploaded").hide();
    $("#waitingFiles").show();
    //var aggregated; //This is the session of list containing three data.frames ("expression","feature","phenotype")
    var eData;
    var fData;
    var pData;
    var aggregated;
    var summary;
    var options;
    var selected_ind =[];
    var selected_rep = ["none"];
    var selected_confound = ["none"];
    var selected_batch = ["none"];
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
        aggregated.getObject(function(obj){//get e f and p.
          eData=obj.expression;
          fData=obj.feature;
          pData=obj.phenotype;
          eTable = drawTable('#edata',eData);
          fTable = drawTable('#fdata',fData);
          pTable = drawTable('#pdata',pData);
        var req = ocpu.call("guess_factor",{
          p:pData
        },function(session){
          session.getObject(function(obj){// get the options for users to select which are factor which are repeated factor, etc.
            options=obj.options;
            selected_ind=obj.guess_factor_name;
            $("#factors").text(selected_ind.join(", "));
            $("#repeated_factors").text(selected_rep.join(", "));
          });
        }).fail(function(jqXHR){
              errormsg(1 + jqXHR.responseText);
          });
        });
        $("#waitingFiles").hide("slow");
        $("#FilesUploaded").show("slow");
      }).fail(function(jqXHR){
      errormsg(2 + jqXHR.responseText);
    });
    }
  });



 $("#submit_experimental_design").on("click",function(){
    var req=ocpu.call("summary_aggregated_data",{
       e:eData,
       f:fData,
       p:pData,
       factor_name: selected_ind.join(","),
       repeated_factor_name:selected_rep.join(","),
       confound: selected_confound.join(","),
       batch: selected_batch.join(","),
     },function(session){
       summary = session;
     }).fail(function(jqXHR){
      errormsg(3 + jqXHR.responseText);
    });



    $("#TRY").click(function(){
      summary.getObject(function(obj){
        $("#demo").text(summary.loc);
      });
    });
 });

  function successmsg(text){
    $("#successdiv").empty().append('<div class="alert alert-success alert-dismissable"><a href="#" class="close" data-dismiss="alert">&times;</a>' + text + '</div>');
  }

  function errormsg(text){
    $("#errordiv").empty().append('<div class="alert alert-danger alert-dismissable"><a href="#" class="close" data-dismiss="alert">&times;</a>' + text + '</div>');
  }



listCHECKBOX = function(options,selected,name){
var checkboxes = "";
for(var i = 0;i<options.length;i++){
   checkboxes += "<input type='checkbox' name="+name+" value='"+options[i]+"'/>"+options[i]+"<br>";
}
$("#option").html(checkboxes);
for(var j = 0;j<options.length;j++){
  if(selected!==undefined){
      for(var k=0; k<selected.length;k++){
    if(options[j]==selected[k]){
        document.getElementsByName(name)[j].checked=true;
    }
  }
  }
}};

// Following is for changing the ind factor.
$(document).on("click","#Edit_ind_factor",function(){
     listCHECKBOX(options,selected_ind,"ind");
     pTable_option = drawTable('#pdata_option',pData);
     document.getElementById("pdata_option").style.display = "inline";
    $("#column_options").toggle("slide");
});
$(document).on("change","input[name='ind']",function(){
      for(var i=0;i<options.length;i++){
       if(document.getElementsByName("ind")[i].checked===true){
         selected_ind[i] = options[i];
       }else{
         selected_ind[i] = "";
       }
     }
   var temp = [];
     k = 0;
     for(var j=0;j<selected_ind.length;j++){
       if(selected_ind[j].length>0){
         temp[k]=selected_ind[j];
         k=k+1;
       }
     }
     selected_ind =  temp;
     $("#factors").text(selected_ind.join(", "));
});

// Following is for changing the repeated factor.
$('#Edit_rep_factor').click(function(){
     listCHECKBOX(selected_ind,selected_rep,"rep");
     pTable_option = drawTable('#pdata_option',pData);
     $("#column_options").toggle("slow");
});
$(document).on("change","input[name='rep']",function(){
      for(var i=0;i<selected_ind.length;i++){
       if(document.getElementsByName("rep")[i].checked===true){
         selected_rep[i] = selected_ind[i];
       }else{
         selected_rep[i] = "";
       }
     }
   var temp = [];
     k = 0;
     for(var j=0;j<selected_rep.length;j++){
       if(selected_rep[j].length>0){
         temp[k]=selected_rep[j];
         k=k+1;
       }
     }
     selected_rep =  temp;
    if(selected_rep.length!==0){
      $("#repeated_factors").text(selected_rep.join(", "));
     }else{
     $("#repeated_factors").text("none");
   }
});

// Submit all the finished editing.
/*function submit_experimental_design(){
  document.getElementById("FinishUploading").submit();
}*/


    $("#Continue").click(function(){
      if(summary===undefined){
        alert("Please Load Data");
      }else{
        window.location = 'dataexplore.html?rsess='+summary.getLoc();//send r object for subnetwork
      }
    });//subnetworkBtn




});
