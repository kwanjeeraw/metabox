function choosefile(file){ //$("#display_choosefile").

  if(file=="example"){
    document.getElementById("display_choosefile").style.display ="none";
  }else if(file=="aggregated"){
    document.getElementById("display_choosefile").style.display ="block";
    document.getElementById("display_choosefile").innerHTML =
    "<p>Please read detailed explaination about the <a href='https://github.com/kwanjeeraw/mETABOX'>upload data format</a>. \
    An example dataset is also <a href='https://github.com/kwanjeeraw/mETABOX'>provided</a>.</p>\
    <label>Upload Aggregated Data</label> \
    <input id='InputData' type='file'>";
  }else if(file=="expression_feature_phenotype"){
    document.getElementById("display_choosefile").style.display ="block";
    document.getElementById("display_choosefile").innerHTML =
    "<p>Please read detailed explaination about the <a href='https://github.com/kwanjeeraw/mETABOX'>upload data format</a>. \
    If the order of features or subjects do not match, don\'t worry we can handle that but please read carefully with \
    the <a href='https://github.com/kwanjeeraw/mETABOX'>upload data format</a>.</p>\
    <p>Example datasets are also <a href='http://www.google.com'>provided</a> for expression, feature and phenotype seperately.\
    </p>\
    <div style='float:left'><label>Upload Expression Data</label> <input id='InputDataExp' type='file'></div>\
    <div style='float:left'><label>Upload Feature Data</label> <input id='InputDataExp' type='file'></div>\
    <div style='float:left'><label>Upload Phenotypen Data</label> <input id='InputDataExp' type='file'></div>";
  }else{
    return null;
  }

}
