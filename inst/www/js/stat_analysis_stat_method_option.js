var ttestmethod = 'Welch';
var ttestcorrection = 'fdr';
var nonparattestmethod = 'utest';
var nonparattestcorrection = 'fdr';



// t test.<a href="#" id="onewayANOVAmethod" data-type="select"  data-title="Select ANOVA method"></a>
t_test_disc = function(){
ttestmethod = 'Welch';
ttestcorrection = 'fdr';
nonparattestmethod = 'utest';
nonparattestcorrection = 'fdr';



  $("#method_description_para").html(
    '<h4>Parametric</h4>Use <a href="#" id="ttestmethod" data-type="select"  data-title="Select A Test"></a> on factor <strong>' + $("#independent_factor").val() + '</strong>, with a <a href="#" id="ttestcorrection" data-type="select"  data-title="Select A Adjust Method"></a> ' + ' adjustment, to compare <em>' + pComponents[$("#independent_factor").val()[0]].join("</em> and <em>") + "</em>."
  )

  $("#ttestmethod").editable({
    value: 'Welch',
    source:[
      {value: 'ttest', text: 't test'},
      {value: 'Welch', text: 'Welch\'s t test'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        ttestmethod = newValue;
    }
  })
  $("#ttestcorrection").editable({
    value:'fdr',
    source:[
      {value:'fdr', text: 'Benjamini & Hochberg (1995)'},
      {value:'holm',text: 'Holm (1979)'},
      {value:'hochberg',text:'Hochberg (1988)'},
      {value:'hommel',text:'Hommel (1988)'},
      {value:'bonferroni',text:'Bonferroni correction'},
      {value:'BY', text:'Benjamini & Yekutieli (2001)'},
      {value:'none',text:'none'}
    ],
    success: function(response, newValue){
      ttestcorrection = newValue;
    }
  })

  $("#method_description_non_para").html(
    '<h4>Non-Parametric</h4>Use <a href="#" id="nonparattestmethod" data-type="select"  data-title="Select A Test"></a> on factor <strong>' + $("#independent_factor").val() + '</strong>, with a <a href="#" id="nonparattestcorrection" data-type="select"  data-title="Select A Adjust Method"></a> ' + ' adjustment, to compare <em>' + pComponents[$("#independent_factor").val()[0]].join("</em> and <em>") + "</em>."
  )
 $("#nonparattestmethod").editable({
    value: 'utest',
    source:[
      {value: 'utest', text: 'Mann–Whitney U test'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        nonparattestmethod = newValue;
    }
  })
  $("#nonparattestcorrection").editable({
    value:'fdr',
    source:[
      {value:'fdr', text: 'Benjamini & Hochberg (1995)'},
      {value:'holm',text: 'Holm (1979)'},
      {value:'hochberg',text:'Hochberg (1988)'},
      {value:'hommel',text:'Hommel (1988)'},
      {value:'bonferroni',text:'Bonferroni correction'},
      {value:'BY', text:'Benjamini & Yekutieli (2001)'},
      {value:'none',text:'none'}
    ],
    success: function(response, newValue){
      nonparattestcorrection = newValue;
    }
  })

}

var ANOVAmethod = 'Welch';
var ANOVAposthoc = 'games.howell';
var nonparaANOVAmethod = 'htest';//Kruskal-Wallis H test
var nonparaANOVAposthoc = 'dunn';

// ANOVA
ANOVA_disc = function(){
  ANOVAmethod = 'Welch';
  ANOVAposthoc = 'games.howell';
  nonparaANOVAmethod = 'htest';//Kruskal-Wallis H test
  nonparaANOVAposthoc = 'dunn';

 var levels = pComponents[$("#independent_factor").val()[0]].slice(0,2).join("</em>, <em>")
 var lastlevel = pComponents[$("#independent_factor").val()[0]][pComponents[$("#independent_factor").val()[0]].length-1];

  $("#method_description_para").html(
    '<h4>Parameter</h4>Use <a href="#" id="ANOVAmethod" data-type = "select" data-title = "Select A Test"></a> on factor <strong>' + $("#independent_factor").val() +'</strong>, followed by <a href="#" id="ANOVAposthoc" data-type="select"  data-title="Select A post hoc analysis procedure"></a> post hoc analysis to compare between <em>' +levels+"</em> and <em>"+ lastlevel+ "</em>."
  )
  $("#ANOVAmethod").editable({
    value:'Welch',
    source:[
      {value:'ANOVA',text:'ANOVA'},
      {value:'Welch',text:'Welch\'s ANOVA'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        ANOVAmethod = newValue;
    }
  })
  $("#ANOVAposthoc").editable({
    value:'games.howell',
    source:[
      {value:'tukey',text:'Tukey'},
      {value:'games.howell',text:'Games-Howell'},
      {value: 'none', text:  'no'}
    ],
    success: function(response, newValue) {
        ANOVAposthoc = newValue;
    }
  })

   $("#method_description_non_para").html(
    '<h4>Non-Parameter</h4>Use <a href="#" id="nonparaANOVAmethod" data-type = "select" data-title = "Select A Test"></a> on factor <strong>' + $("#independent_factor").val() +'</strong>, followed by <a href="#" id="nonparaANOVAposthoc" data-type="select"  data-title="Select A post hoc analysis procedure"></a> post hoc analysis to compare between <em>' +levels+"</em> and <em>"+ lastlevel+ "</em>."
  )
  $("#nonparaANOVAmethod").editable({
    value:'htest',
    source:[
      {value:'htest',text:'Kruskal-Wallis H Test'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        nonparaANOVAmethod = newValue;
    }
  })
  $("#nonparaANOVAposthoc").editable({
    value:'dunn',
    source:[
      {value:'dunn',text:'Dunn\'s (1964) procedure with a Bonferroni adjustment'},
      {value:'utest',text:'Mann-Whitney U tests with a Bonferroni adjustment'},
      {value: 'none', text:  'no'}
    ],
    success: function(response, newValue) {
        nonparaANOVAposthoc = newValue;
    }
  })
}


var twowayANOVAmethod = 'twowayANOVA';
var maineffect1ANOVAmethod = 'Welch';
var maineffect1posthoc = 'games.howell'
var simplemaineffect1ANOVAmethod = 'Welch';
var simplemaineffect1posthoc = 'games.howell'
var maineffect2ANOVAmethod = 'Welch';
var maineffect2posthoc = 'games.howell'
var simplemaineffect2ANOVAmethod = 'Welch';
var simplemaineffect2posthoc = 'games.howell'

var nonparatwowayANOVAmethod = 'nonparatwowayANOVA';
var nonparamaineffect1ANOVAmethod = 'htest';
var nonparamaineffect1posthoc = 'dunn'
var nonparasimplemaineffect1ANOVAmethod = 'htest';
var nonparasimplemaineffect1posthoc = 'dunn'
var nonparamaineffect2ANOVAmethod = 'htest';
var nonparamaineffect2posthoc = 'dunn'
var nonparasimplemaineffect2ANOVAmethod = 'htest';
var nonparasimplemaineffect2posthoc = 'htest'

twowayANOVA33_disc = function(){

  $("#method_description_para").html(
    '<h4>Parameter</h4><strong>'+$("#independent_factor").val()[0]+' * '+$("#independent_factor").val()[1]+': </strong>Use <a href="#" id="twowayANOVAmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>interaction effect</em>.<br>' +

    '<strong>'+$("#independent_factor").val()[0]+':</strong> Use <a href="#" id="maineffect1ANOVAmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>main effect</em>, followed by <a href="#" id="maineffect1posthoc" data-type="select"  data-title="Select A post hoc analysis procedure"></a> post hoc analysis.<br>' +
    '<strong><font color="white">'+$("#independent_factor").val()[0]+':</font></strong> Use <a href="#" id="simplemaineffect1ANOVAmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>simple main effect</em> on each level of '+$("#independent_factor").val()[1]+', followed by <a href="#" id="simplemaineffect1posthoc" data-type="select"  data-title="Select A post hoc analysis procedure"></a> post hoc analysis.<br>' +

    '<strong>'+$("#independent_factor").val()[1]+':</strong> Use <a href="#" id="maineffect2ANOVAmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>main effect</em>, followed by <a href="#" id="maineffect2posthoc" data-type="select"  data-title="Select A post hoc analysis procedure"></a> post hoc analysis.<br>' +
    '<strong><font color="white">'+$("#independent_factor").val()[1]+':</font></strong> Use <a href="#" id="simplemaineffect2ANOVAmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>simple main effect</em> on each level of '+$("#independent_factor").val()[0]+', followed by <a href="#" id="simplemaineffect2posthoc" data-type="select"  data-title="Select A post hoc analysis procedure"></a> post hoc analysis.<br>'
  )



  $("#twowayANOVAmethod").editable({
    value:'twowayANOVA',
    source:[
      {value:'twowayANOVA',text:'two way ANOVA'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        twowayANOVAmethod = newValue;
    }
  })
  $("#maineffect1ANOVAmethod").editable({
    value:'Welch',
    source:[
      {value:'ANOVA',text:'ANOVA'},
      {value:'Welch',text:'Welch\'s ANOVA'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        maineffect1ANOVAmethod = newValue;
    }
  })
  $("#maineffect1posthoc").editable({
    value:'games.howell',
    source:[
      {value:'tukey',text:'Tukey'},
      {value:'games.howell',text:'Games-Howell'},
      {value: 'none', text:  'no'}
    ],
    success: function(response, newValue) {
        maineffect1posthoc = newValue;
    }
  })
  $("#simplemaineffect1ANOVAmethod").editable({
    value:'Welch',
    source:[
      {value:'ANOVA',text:'ANOVA'},
      {value:'Welch',text:'Welch\'s ANOVA'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        simplemaineffect1ANOVAmethod = newValue;
    }
  })
  $("#simplemaineffect1posthoc").editable({
    value:'games.howell',
    source:[
      {value:'tukey',text:'Tukey'},
      {value:'games.howell',text:'Games-Howell'},
      {value: 'none', text:  'no'}
    ],
    success: function(response, newValue) {
        simplemaineffect1posthoc = newValue;
    }
  })
$("#maineffect2ANOVAmethod").editable({
    value:'Welch',
    source:[
      {value:'ANOVA',text:'ANOVA'},
      {value:'Welch',text:'Welch\'s ANOVA'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        maineffect2ANOVAmethod = newValue;
    }
  })
  $("#maineffect2posthoc").editable({
    value:'games.howell',
    source:[
      {value:'tukey',text:'Tukey'},
      {value:'games.howell',text:'Games-Howell'},
      {value: 'none', text:  'no'}
    ],
    success: function(response, newValue) {
        maineffect2posthoc = newValue;
    }
  })
  $("#simplemaineffect2ANOVAmethod").editable({
    value:'Welch',
    source:[
      {value:'ANOVA',text:'ANOVA'},
      {value:'Welch',text:'Welch\'s ANOVA'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        simplemaineffect2ANOVAmethod = newValue;
    }
  })
  $("#simplemaineffect2posthoc").editable({
    value:'games.howell',
    source:[
      {value:'tukey',text:'Tukey'},
      {value:'games.howell',text:'Games-Howell'},
      {value: 'none', text:  'no'}
    ],
    success: function(response, newValue) {
        simplemaineffect2posthoc = newValue;
    }
  })

  $("#method_description_non_para").html(
    '<h4>Non-Parameter</h4><strong>'+$("#independent_factor").val()[0]+' * '+$("#independent_factor").val()[1]+':</strong> <span class="text-muted">There is no non-parametric two way ANOVA available.</span><br>' +
    //': </strong>Use <a href="#" id="nonparatwowayANOVAmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>interaction effect</em>.<br>' +

    '<strong>'+$("#independent_factor").val()[0]+':</strong> Use <a href="#" id="nonparamaineffect1ANOVAmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>main effect</em>, followed by <a href="#" id="nonparamaineffect1posthoc" data-type="select"  data-title="Select A post hoc analysis procedure"></a> post hoc analysis.<br>' +
    '<strong><font color="white">'+$("#independent_factor").val()[0]+':</font></strong> Use <a href="#" id="nonparasimplemaineffect1ANOVAmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>simple main effect</em> on each level of '+$("#independent_factor").val()[1]+', followed by <a href="#" id="nonparasimplemaineffect1posthoc" data-type="select"  data-title="Select A post hoc analysis procedure"></a> post hoc analysis.<br>' +

    '<strong>'+$("#independent_factor").val()[1]+':</strong> Use <a href="#" id="nonparamaineffect2ANOVAmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>main effect</em>, followed by <a href="#" id="nonparamaineffect2posthoc" data-type="select"  data-title="Select A post hoc analysis procedure"></a> post hoc analysis.<br>' +
    '<strong><font color="white">'+$("#independent_factor").val()[1]+':</font></strong> Use <a href="#" id="nonparasimplemaineffect2ANOVAmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>simple main effect</em> on each level of '+$("#independent_factor").val()[0]+', followed by <a href="#" id="nonparasimplemaineffect2posthoc" data-type="select"  data-title="Select A post hoc analysis procedure"></a> post hoc analysis.<br>'
  )



  $("#nonparatwowayANOVAmethod").editable({
    value:'nonparatwowayANOVA',
    source:[
      {value:'nonparatwowayANOVA',text:'non-parametric two way ANOVA'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        nonparatwowayANOVAmethod = newValue;
    }
  })
  $("#nonparamaineffect1ANOVAmethod").editable({
    value:'htest',
    source:[
      {value:'htest',text:'Kruskal-Wallis H Test'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        nonparamaineffect1ANOVAmethod = newValue;
    }
  })
  $("#nonparamaineffect1posthoc").editable({
    value:'dunn',
    source:[
      {value:'dunn',text:'Dunn\'s (1964) procedure with a Bonferroni adjustment'},
      {value:'utest',text:'Mann-Whitney U tests with a Bonferroni adjustment'},
      {value: 'none', text:  'no'}
    ],
    success: function(response, newValue) {
        nonparamaineffect1posthoc = newValue;
    }
  })
  $("#nonparasimplemaineffect1ANOVAmethod").editable({
    value:'htest',
    source:[
      {value:'htest',text:'Kruskal-Wallis H Test'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        nonparasimplemaineffect1ANOVAmethod = newValue;
    }
  })
  $("#nonparasimplemaineffect1posthoc").editable({
    value:'dunn',
    source:[
      {value:'dunn',text:'Dunn\'s (1964) procedure with a Bonferroni adjustment'},
      {value:'utest',text:'Mann-Whitney U tests with a Bonferroni adjustment'},
      {value: 'none', text:  'no'}
    ],
    success: function(response, newValue) {
        nonparasimplemaineffect1posthoc = newValue;
    }
  })
$("#nonparamaineffect2ANOVAmethod").editable({
    value:'htest',
    source:[
      {value:'htest',text:'Kruskal-Wallis H Test'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        nonparamaineffect2ANOVAmethod = newValue;
    }
  })
  $("#nonparamaineffect2posthoc").editable({
    value:'dunn',
    source:[
      {value:'dunn',text:'Dunn\'s (1964) procedure with a Bonferroni adjustment'},
      {value:'utest',text:'Mann-Whitney U tests with a Bonferroni adjustment'},
      {value: 'none', text:  'no'}
    ],
    success: function(response, newValue) {
        nonparamaineffect2posthoc = newValue;
    }
  })
  $("#nonparasimplemaineffect2ANOVAmethod").editable({
    value:'htest',
    source:[
      {value:'htest',text:'Kruskal-Wallis H Test'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        nonparasimplemaineffect2ANOVAmethod = newValue;
    }
  })
  $("#nonparasimplemaineffect2posthoc").editable({
     value:'dunn',
    source:[
      {value:'dunn',text:'Dunn\'s (1964) procedure with a Bonferroni adjustment'},
      {value:'utest',text:'Mann-Whitney U tests with a Bonferroni adjustment'},
      {value: 'none', text:  'no'}
    ],
    success: function(response, newValue) {
        nonparasimplemaineffect2posthoc = newValue;
    }
  })

}

var maineffect1ttestmethod = 'utest';
var maineffect1correction = 'fdr'
var simplemaineffect1ttestmethod = 'utest';
var simplemaineffect1correction = 'fdr'
var maineffect2ttestmethod = 'utest';
var maineffect2correction = 'fdr'
var simplemaineffect2ttestmethod = 'utest';
var simplemaineffect2correction = 'fdr'

var nonparamaineffect1ttestmethod = 'utest';
var nonparamaineffect1correction = 'fdr'
var nonparasimplemaineffect1ttestmethod = 'utest';
var nonparasimplemaineffect1correction = 'fdr'
var nonparamaineffect2ttestmethod = 'utest';
var nonparamaineffect2correction = 'fdr'
var nonparasimplemaineffect2ttestmethod = 'utest';
var nonparasimplemaineffect2correction = 'fdr'
twowayANOVA23_disc = function(){

  $("#method_description_para").html(
    '<h4>Parameter</h4><strong>'+$("#independent_factor").val()[0]+' * '+$("#independent_factor").val()[1]+': </strong>Use <a href="#" id="twowayANOVAmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>interaction effect</em>.<br>' +

    '<strong>'+$("#independent_factor").val()[0]+':</strong> Use <a href="#" id="maineffect1ttestmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>main effect</em>, with a <a href="#" id="maineffect1correction" data-type="select"  data-title="Select A Adjust Method"></a> adjustment.<br>' +
    '<strong><font color="white">'+$("#independent_factor").val()[0]+':</font></strong> Use <a href="#" id="simplemaineffect1ttestmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>simple main effect</em> on each level of '+$("#independent_factor").val()[1]+', with a <a href="#" id="simplemaineffect1correction" data-type="select"  data-title="Select A Adjust Method"></a> adjustment.<br>' +


    '<strong>'+$("#independent_factor").val()[1]+':</strong> Use <a href="#" id="maineffect2ANOVAmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>main effect</em>, followed by <a href="#" id="maineffect2posthoc" data-type="select"  data-title="Select A post hoc analysis procedure"></a> post hoc analysis.<br>' +
    '<strong><font color="white">'+$("#independent_factor").val()[1]+':</font></strong> Use <a href="#" id="simplemaineffect2ANOVAmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>simple main effect</em> on each level of '+$("#independent_factor").val()[0]+', followed by <a href="#" id="simplemaineffect2posthoc" data-type="select"  data-title="Select A post hoc analysis procedure"></a> post hoc analysis.<br>'
  )

  $("#twowayANOVAmethod").editable({
    value:'twowayANOVA',
    source:[
      {value:'twowayANOVA',text:'two way ANOVA'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        twowayANOVAmethod = newValue;
    }
  })
  $("#maineffect1ttestmethod").editable({
    value: 'Welch',
    source:[
      {value: 'ttest', text: 't test'},
      {value: 'Welch', text: 'Welch\'s t test'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        maineffect1ttestmethod = newValue;
    }
  })
  $("#maineffect1correction").editable({
    value:'fdr',
    source:[
      {value:'fdr', text: 'Benjamini & Hochberg (1995)'},
      {value:'holm',text: 'Holm (1979)'},
      {value:'hochberg',text:'Hochberg (1988)'},
      {value:'hommel',text:'Hommel (1988)'},
      {value:'bonferroni',text:'Bonferroni correction'},
      {value:'BY', text:'Benjamini & Yekutieli (2001)'},
      {value:'none',text:'none'}
    ],
    success: function(response, newValue) {
        maineffect1correction = newValue;
    }
  })
  $("#simplemaineffect1ttestmethod").editable({
    value: 'Welch',
    source:[
      {value: 'ttest', text: 't test'},
      {value: 'Welch', text: 'Welch\'s t test'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        simplemaineffect1ttestmethod = newValue;
    }
  })
  $("#simplemaineffect1correction").editable({
    value:'fdr',
    source:[
      {value:'fdr', text: 'Benjamini & Hochberg (1995)'},
      {value:'holm',text: 'Holm (1979)'},
      {value:'hochberg',text:'Hochberg (1988)'},
      {value:'hommel',text:'Hommel (1988)'},
      {value:'bonferroni',text:'Bonferroni correction'},
      {value:'BY', text:'Benjamini & Yekutieli (2001)'},
      {value:'none',text:'none'}
    ],
    success: function(response, newValue) {
        simplemaineffect1correction = newValue;
    }
  })
$("#maineffect2ANOVAmethod").editable({
    value:'Welch',
    source:[
      {value:'ANOVA',text:'ANOVA'},
      {value:'Welch',text:'Welch\'s ANOVA'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        maineffect2ANOVAmethod = newValue;
    }
  })
  $("#maineffect2posthoc").editable({
    value:'games.howell',
    source:[
      {value:'tukey',text:'Tukey'},
      {value:'games.howell',text:'Games-Howell'},
      {value: 'none', text:  'no'}
    ],
    success: function(response, newValue) {
        maineffect2posthoc = newValue;
    }
  })
  $("#simplemaineffect2ANOVAmethod").editable({
    value:'Welch',
    source:[
      {value:'ANOVA',text:'ANOVA'},
      {value:'Welch',text:'Welch\'s ANOVA'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        simplemaineffect2ANOVAmethod = newValue;
    }
  })
  $("#simplemaineffect2posthoc").editable({
    value:'games.howell',
    source:[
      {value:'tukey',text:'Tukey'},
      {value:'games.howell',text:'Games-Howell'},
      {value: 'none', text:  'no'}
    ],
    success: function(response, newValue) {
        simplemaineffect2posthoc = newValue;
    }
  })

  $("#method_description_non_para").html(
    '<h4>Non-Parameter</h4><strong>'+$("#independent_factor").val()[0]+' * '+$("#independent_factor").val()[1]+':</strong> <span class="text-muted">There is no non-parametric two way ANOVA available.</span><br>' +
    //': </strong>Use <a href="#" id="nonparatwowayANOVAmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>interaction effect</em>.<br>' +

    '<strong>'+$("#independent_factor").val()[0]+':</strong> Use <a href="#" id="nonparamaineffect1ttestmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>main effect</em>, with a <a href="#" id="nonparamaineffect1correction" data-type="select"  data-title="Select A Adjust Method"></a> adjustment.<br>' +
    '<strong><font color="white">'+$("#independent_factor").val()[0]+':</font></strong> Use <a href="#" id="nonparasimplemaineffect1ttestmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>simple main effect</em> on each level of '+$("#independent_factor").val()[1]+', with a <a href="#" id="nonparasimplemaineffect1correction" data-type="select"  data-title="Select A Adjust Method"></a> adjustment.<br>' +

    '<strong>'+$("#independent_factor").val()[1]+':</strong> Use <a href="#" id="nonparamaineffect2ANOVAmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>main effect</em>, followed by <a href="#" id="nonparamaineffect2posthoc" data-type="select"  data-title="Select A post hoc analysis procedure"></a> post hoc analysis.<br>' +
    '<strong><font color="white">'+$("#independent_factor").val()[1]+':</font></strong> Use <a href="#" id="nonparasimplemaineffect2ANOVAmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>simple main effect</em> on each level of '+$("#independent_factor").val()[0]+', followed by <a href="#" id="nonparasimplemaineffect2posthoc" data-type="select"  data-title="Select A post hoc analysis procedure"></a> post hoc analysis.<br>'
  )



  $("#nonparatwowayANOVAmethod").editable({
    value:'nonparatwowayANOVA',
    source:[
      {value:'nonparatwowayANOVA',text:'non-parametric two way ANOVA'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        nonparatwowayANOVAmethod = newValue;
    }
  })
 $("#nonparamaineffect1ttestmethod").editable({
       value: 'utest',
    source:[
      {value: 'utest', text: 'Mann–Whitney U test'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        nonparamaineffect1ttestmethod = newValue;
    }
  })
  $("#nonparamaineffect1correction").editable({
    value:'fdr',
    source:[
      {value:'fdr', text: 'Benjamini & Hochberg (1995)'},
      {value:'holm',text: 'Holm (1979)'},
      {value:'hochberg',text:'Hochberg (1988)'},
      {value:'hommel',text:'Hommel (1988)'},
      {value:'bonferroni',text:'Bonferroni correction'},
      {value:'BY', text:'Benjamini & Yekutieli (2001)'},
      {value:'none',text:'none'}
    ],
    success: function(response, newValue) {
        nonparamaineffect1correction = newValue;
    }
  })
  $("#nonparasimplemaineffect1ttestmethod").editable({
       value: 'utest',
    source:[
      {value: 'utest', text: 'Mann–Whitney U test'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        nonparasimplemaineffect1ttestmethod = newValue;
    }
  })
  $("#nonparasimplemaineffect1correction").editable({
    value:'fdr',
    source:[
      {value:'fdr', text: 'Benjamini & Hochberg (1995)'},
      {value:'holm',text: 'Holm (1979)'},
      {value:'hochberg',text:'Hochberg (1988)'},
      {value:'hommel',text:'Hommel (1988)'},
      {value:'bonferroni',text:'Bonferroni correction'},
      {value:'BY', text:'Benjamini & Yekutieli (2001)'},
      {value:'none',text:'none'}
    ],
    success: function(response, newValue) {
        nonparasimplemaineffect1correction = newValue;
    }
  })
$("#nonparamaineffect2ANOVAmethod").editable({
    value:'htest',
    source:[
      {value:'htest',text:'Kruskal-Wallis H Test'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        nonparamaineffect2ANOVAmethod = newValue;
    }
  })
  $("#nonparamaineffect2posthoc").editable({
    value:'dunn',
    source:[
      {value:'dunn',text:'Dunn\'s (1964) procedure with a Bonferroni adjustment'},
      {value:'utest',text:'Mann-Whitney U tests with a Bonferroni adjustment'},
      {value: 'none', text:  'no'}
    ],
    success: function(response, newValue) {
        nonparamaineffect2posthoc = newValue;
    }
  })
  $("#nonparasimplemaineffect2ANOVAmethod").editable({
    value:'htest',
    source:[
      {value:'htest',text:'Kruskal-Wallis H Test'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        nonparasimplemaineffect2ANOVAmethod = newValue;
    }
  })
  $("#nonparasimplemaineffect2posthoc").editable({
     value:'dunn',
    source:[
      {value:'dunn',text:'Dunn\'s (1964) procedure with a Bonferroni adjustment'},
      {value:'utest',text:'Mann-Whitney U tests with a Bonferroni adjustment'},
      {value: 'none', text:  'no'}
    ],
    success: function(response, newValue) {
        nonparasimplemaineffect2posthoc = newValue;
    }
  })

}

twowayANOVA22_disc = function(){

  $("#method_description_para").html(
    '<h4>Parameter</h4><strong>'+$("#independent_factor").val()[0]+' * '+$("#independent_factor").val()[1]+': </strong>Use <a href="#" id="twowayANOVAmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>interaction effect</em>.<br>' +

    '<strong>'+$("#independent_factor").val()[0]+':</strong> Use <a href="#" id="maineffect1ttestmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>main effect</em>, with a <a href="#" id="maineffect1correction" data-type="select"  data-title="Select A Adjust Method"></a> adjustment.<br>' +
    '<strong><font color="white">'+$("#independent_factor").val()[0]+':</font></strong> Use <a href="#" id="simplemaineffect1ttestmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>simple main effect</em> on each level of '+$("#independent_factor").val()[1]+', with a <a href="#" id="simplemaineffect1correction" data-type="select"  data-title="Select A Adjust Method"></a> adjustment.<br>' +


    '<strong>'+$("#independent_factor").val()[1]+':</strong> Use <a href="#" id="maineffect2ttestmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>main effect</em>, with a <a href="#" id="maineffect2correction" data-type="select"  data-title="Select A Adjust Method"></a> adjustment.<br>' +
    '<strong><font color="white">'+$("#independent_factor").val()[1]+':</font></strong> Use <a href="#" id="simplemaineffect2ttestmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>simple main effect</em> on each level of '+$("#independent_factor").val()[0]+', with a <a href="#" id="simplemaineffect2correction" data-type="select"  data-title="Select A Adjust Method"></a> adjustment.<br>'
  )

  $("#twowayANOVAmethod").editable({
    value:'twowayANOVA',
    source:[
      {value:'twowayANOVA',text:'two way ANOVA'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        twowayANOVAmethod = newValue;
    }
  })
  $("#maineffect1ttestmethod").editable({
    value: 'Welch',
    source:[
      {value: 'ttest', text: 't test'},
      {value: 'Welch', text: 'Welch\'s t test'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        maineffect1ttestmethod = newValue;
    }
  })
  $("#maineffect1correction").editable({
    value:'fdr',
    source:[
      {value:'fdr', text: 'Benjamini & Hochberg (1995)'},
      {value:'holm',text: 'Holm (1979)'},
      {value:'hochberg',text:'Hochberg (1988)'},
      {value:'hommel',text:'Hommel (1988)'},
      {value:'bonferroni',text:'Bonferroni correction'},
      {value:'BY', text:'Benjamini & Yekutieli (2001)'},
      {value:'none',text:'none'}
    ],
    success: function(response, newValue) {
        maineffect1correction = newValue;
    }
  })
  $("#simplemaineffect1ttestmethod").editable({
    value: 'Welch',
    source:[
      {value: 'ttest', text: 't test'},
      {value: 'Welch', text: 'Welch\'s t test'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        simplemaineffect2ttestmethod = newValue;
    }
  })
  $("#simplemaineffect1correction").editable({
    value:'fdr',
    source:[
      {value:'fdr', text: 'Benjamini & Hochberg (1995)'},
      {value:'holm',text: 'Holm (1979)'},
      {value:'hochberg',text:'Hochberg (1988)'},
      {value:'hommel',text:'Hommel (1988)'},
      {value:'bonferroni',text:'Bonferroni correction'},
      {value:'BY', text:'Benjamini & Yekutieli (2001)'},
      {value:'none',text:'none'}
    ],
    success: function(response, newValue) {
        simplemaineffect1correction = newValue;
    }
  })
$("#maineffect2ttestmethod").editable({
    value: 'Welch',
    source:[
      {value: 'ttest', text: 't test'},
      {value: 'Welch', text: 'Welch\'s t test'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        maineffect2ttestmethod = newValue;
    }
  })
  $("#maineffect2correction").editable({
    value:'fdr',
    source:[
      {value:'fdr', text: 'Benjamini & Hochberg (1995)'},
      {value:'holm',text: 'Holm (1979)'},
      {value:'hochberg',text:'Hochberg (1988)'},
      {value:'hommel',text:'Hommel (1988)'},
      {value:'bonferroni',text:'Bonferroni correction'},
      {value:'BY', text:'Benjamini & Yekutieli (2001)'},
      {value:'none',text:'none'}
    ],
    success: function(response, newValue) {
        maineffect2correction = newValue;
    }
  })
  $("#simplemaineffect2ttestmethod").editable({
    value: 'Welch',
    source:[
      {value: 'ttest', text: 't test'},
      {value: 'Welch', text: 'Welch\'s t test'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        simplemaineffect2ttestmethod = newValue;
    }
  })
  $("#simplemaineffect2correction").editable({
    value:'fdr',
    source:[
      {value:'fdr', text: 'Benjamini & Hochberg (1995)'},
      {value:'holm',text: 'Holm (1979)'},
      {value:'hochberg',text:'Hochberg (1988)'},
      {value:'hommel',text:'Hommel (1988)'},
      {value:'bonferroni',text:'Bonferroni correction'},
      {value:'BY', text:'Benjamini & Yekutieli (2001)'},
      {value:'none',text:'none'}
    ],
    success: function(response, newValue) {
        simplemaineffect2correction = newValue;
    }
  })

  $("#method_description_non_para").html(
    '<h4>Non-Parameter</h4><strong>'+$("#independent_factor").val()[0]+' * '+$("#independent_factor").val()[1]+':</strong> <span class="text-muted">There is no non-parametric two way ANOVA available.</span><br>' +
    //': </strong>Use <a href="#" id="nonparatwowayANOVAmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>interaction effect</em>.<br>' +

    '<strong>'+$("#independent_factor").val()[0]+':</strong> Use <a href="#" id="nonparamaineffect1ttestmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>main effect</em>, with a <a href="#" id="nonparamaineffect1correction" data-type="select"  data-title="Select A Adjust Method"></a> adjustment.<br>' +
    '<strong><font color="white">'+$("#independent_factor").val()[0]+':</font></strong> Use <a href="#" id="nonparasimplemaineffect1ttestmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>simple main effect</em> on each level of '+$("#independent_factor").val()[1]+', with a <a href="#" id="nonparasimplemaineffect1correction" data-type="select"  data-title="Select A Adjust Method"></a> adjustment.<br>' +

    '<strong>'+$("#independent_factor").val()[1]+':</strong> Use <a href="#" id="nonparamaineffect2ttestmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>main effect</em>, with a <a href="#" id="nonparamaineffect2correction" data-type="select"  data-title="Select A Adjust Method"></a> adjustment.<br>' +
    '<strong><font color="white">'+$("#independent_factor").val()[1]+':</font></strong> Use <a href="#" id="nonparasimplemaineffect2ttestmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>simple main effect</em> on each level of '+$("#independent_factor").val()[0]+', with a <a href="#" id="nonparasimplemaineffect2correction" data-type="select"  data-title="Select A Adjust Method"></a> adjustment.<br>'
  )



  $("#nonparatwowayANOVAmethod").editable({
    value:'nonparatwowayANOVA',
    source:[
      {value:'nonparatwowayANOVA',text:'non-parametric two way ANOVA'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        nonparatwowayANOVAmethod = newValue;
    }
  })
 $("#nonparamaineffect1ttestmethod").editable({
       value: 'utest',
    source:[
      {value: 'utest', text: 'Mann–Whitney U test'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        nonparamaineffect1ttestmethod = newValue;
    }
  })
  $("#nonparamaineffect1correction").editable({
    value:'fdr',
    source:[
      {value:'fdr', text: 'Benjamini & Hochberg (1995)'},
      {value:'holm',text: 'Holm (1979)'},
      {value:'hochberg',text:'Hochberg (1988)'},
      {value:'hommel',text:'Hommel (1988)'},
      {value:'bonferroni',text:'Bonferroni correction'},
      {value:'BY', text:'Benjamini & Yekutieli (2001)'},
      {value:'none',text:'none'}
    ],
    success: function(response, newValue) {
        nonparamaineffect1correction = newValue;
    }
  })
  $("#nonparasimplemaineffect1ttestmethod").editable({
       value: 'utest',
    source:[
      {value: 'utest', text: 'Mann–Whitney U test'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        nonparasimplemaineffect1ttestmethod = newValue;
    }
  })
  $("#nonparasimplemaineffect1correction").editable({
    value:'fdr',
    source:[
      {value:'fdr', text: 'Benjamini & Hochberg (1995)'},
      {value:'holm',text: 'Holm (1979)'},
      {value:'hochberg',text:'Hochberg (1988)'},
      {value:'hommel',text:'Hommel (1988)'},
      {value:'bonferroni',text:'Bonferroni correction'},
      {value:'BY', text:'Benjamini & Yekutieli (2001)'},
      {value:'none',text:'none'}
    ],
    success: function(response, newValue) {
        nonparasimplemaineffect1correction = newValue;
    }
  })
$("#nonparamaineffect2ttestmethod").editable({
       value: 'utest',
    source:[
      {value: 'utest', text: 'Mann–Whitney U test'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        nonparamaineffect2ttestmethod = newValue;
    }
  })
  $("#nonparamaineffect2correction").editable({
    value:'fdr',
    source:[
      {value:'fdr', text: 'Benjamini & Hochberg (1995)'},
      {value:'holm',text: 'Holm (1979)'},
      {value:'hochberg',text:'Hochberg (1988)'},
      {value:'hommel',text:'Hommel (1988)'},
      {value:'bonferroni',text:'Bonferroni correction'},
      {value:'BY', text:'Benjamini & Yekutieli (2001)'},
      {value:'none',text:'none'}
    ],
    success: function(response, newValue) {
        nonparamaineffect2correction = newValue;
    }
  })
  $("#nonparasimplemaineffect2ttestmethod").editable({
       value: 'utest',
    source:[
      {value: 'utest', text: 'Mann–Whitney U test'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        nonparasimplemaineffect2ttestmethod = newValue;
    }
  })
  $("#nonparasimplemaineffect2correction").editable({
    value:'fdr',
    source:[
      {value:'fdr', text: 'Benjamini & Hochberg (1995)'},
      {value:'holm',text: 'Holm (1979)'},
      {value:'hochberg',text:'Hochberg (1988)'},
      {value:'hommel',text:'Hommel (1988)'},
      {value:'bonferroni',text:'Bonferroni correction'},
      {value:'BY', text:'Benjamini & Yekutieli (2001)'},
      {value:'none',text:'none'}
    ],
    success: function(response, newValue) {
        nonparasimplemaineffect2correction = newValue;
    }
  })

}





twowayANOVA32_disc = function(){

  $("#method_description_para").html(
    '<h4>Parameter</h4><strong>'+$("#independent_factor").val()[0]+' * '+$("#independent_factor").val()[1]+': </strong>Use <a href="#" id="twowayANOVAmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>interaction effect</em>.<br>' +

 '<strong>'+$("#independent_factor").val()[0]+':</strong> Use <a href="#" id="maineffect1ANOVAmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>main effect</em>, followed by <a href="#" id="maineffect1posthoc" data-type="select"  data-title="Select A post hoc analysis procedure"></a> post hoc analysis.<br>' +
    '<strong><font color="white">'+$("#independent_factor").val()[0]+':</font></strong> Use <a href="#" id="simplemaineffect1ANOVAmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>simple main effect</em> on each level of '+$("#independent_factor").val()[1]+', followed by <a href="#" id="simplemaineffect1posthoc" data-type="select"  data-title="Select A post hoc analysis procedure"></a> post hoc analysis.<br>' +


    '<strong>'+$("#independent_factor").val()[1]+':</strong> Use <a href="#" id="maineffect2ttestmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>main effect</em>, with a <a href="#" id="maineffect2correction" data-type="select"  data-title="Select A Adjust Method"></a> adjustment.<br>' +
    '<strong><font color="white">'+$("#independent_factor").val()[1]+':</font></strong> Use <a href="#" id="simplemaineffect2ttestmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>simple main effect</em> on each level of '+$("#independent_factor").val()[0]+', with a <a href="#" id="simplemaineffect2correction" data-type="select"  data-title="Select A Adjust Method"></a> adjustment.<br>'
  )

 $("#twowayANOVAmethod").editable({
    value:'twowayANOVA',
    source:[
      {value:'twowayANOVA',text:'two way ANOVA'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        twowayANOVAmethod = newValue;
    }
  })
  $("#maineffect1ANOVAmethod").editable({
    value:'Welch',
    source:[
      {value:'ANOVA',text:'ANOVA'},
      {value:'Welch',text:'Welch\'s ANOVA'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        maineffect1ANOVAmethod = newValue;
    }
  })
  $("#maineffect1posthoc").editable({
    value:'games.howell',
    source:[
      {value:'tukey',text:'Tukey'},
      {value:'games.howell',text:'Games-Howell'},
      {value: 'none', text:  'no'}
    ],
    success: function(response, newValue) {
        maineffect1posthoc = newValue;
    }
  })
  $("#simplemaineffect1ANOVAmethod").editable({
    value:'Welch',
    source:[
      {value:'ANOVA',text:'ANOVA'},
      {value:'Welch',text:'Welch\'s ANOVA'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        simplemaineffect1ANOVAmethod = newValue;
    }
  })
  $("#simplemaineffect1posthoc").editable({
    value:'games.howell',
    source:[
      {value:'tukey',text:'Tukey'},
      {value:'games.howell',text:'Games-Howell'},
      {value: 'none', text:  'no'}
    ],
    success: function(response, newValue) {
        simplemaineffect1posthoc = newValue;
    }
  })
$("#maineffect2ttestmethod").editable({
    value: 'Welch',
    source:[
      {value: 'ttest', text: 't test'},
      {value: 'Welch', text: 'Welch\'s t test'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        maineffect2ttestmethod = newValue;
    }
  })
  $("#maineffect2correction").editable({
    value:'fdr',
    source:[
      {value:'fdr', text: 'Benjamini & Hochberg (1995)'},
      {value:'holm',text: 'Holm (1979)'},
      {value:'hochberg',text:'Hochberg (1988)'},
      {value:'hommel',text:'Hommel (1988)'},
      {value:'bonferroni',text:'Bonferroni correction'},
      {value:'BY', text:'Benjamini & Yekutieli (2001)'},
      {value:'none',text:'none'}
    ],
    success: function(response, newValue) {
        maineffect2correction = newValue;
    }
  })
  $("#simplemaineffect2ttestmethod").editable({
    value: 'Welch',
    source:[
      {value: 'ttest', text: 't test'},
      {value: 'Welch', text: 'Welch\'s t test'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        simplemaineffect2ttestmethod = newValue;
    }
  })
  $("#simplemaineffect2correction").editable({
    value:'fdr',
    source:[
      {value:'fdr', text: 'Benjamini & Hochberg (1995)'},
      {value:'holm',text: 'Holm (1979)'},
      {value:'hochberg',text:'Hochberg (1988)'},
      {value:'hommel',text:'Hommel (1988)'},
      {value:'bonferroni',text:'Bonferroni correction'},
      {value:'BY', text:'Benjamini & Yekutieli (2001)'},
      {value:'none',text:'none'}
    ],
    success: function(response, newValue) {
        simplemaineffect2correction = newValue;
    }
  })

  $("#method_description_non_para").html(
    '<h4>Non-Parameter</h4><strong>'+$("#independent_factor").val()[0]+' * '+$("#independent_factor").val()[1]+':</strong> <span class="text-muted">There is no non-parametric two way ANOVA available.</span><br>' +
    //': </strong>Use <a href="#" id="nonparatwowayANOVAmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>interaction effect</em>.<br>' +

    '<strong>'+$("#independent_factor").val()[0]+':</strong> Use <a href="#" id="nonparamaineffect1ANOVAmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>main effect</em>, followed by <a href="#" id="nonparamaineffect1posthoc" data-type="select"  data-title="Select A post hoc analysis procedure"></a> post hoc analysis.<br>' +
    '<strong><font color="white">'+$("#independent_factor").val()[0]+':</font></strong> Use <a href="#" id="nonparasimplemaineffect1ANOVAmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>simple main effect</em> on each level of '+$("#independent_factor").val()[1]+', followed by <a href="#" id="nonparasimplemaineffect1posthoc" data-type="select"  data-title="Select A post hoc analysis procedure"></a> post hoc analysis.<br>'  +

    '<strong>'+$("#independent_factor").val()[1]+':</strong> Use <a href="#" id="nonparamaineffect2ttestmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>main effect</em>, with a <a href="#" id="nonparamaineffect2correction" data-type="select"  data-title="Select A Adjust Method"></a> adjustment.<br>' +
    '<strong><font color="white">'+$("#independent_factor").val()[1]+':</font></strong> Use <a href="#" id="nonparasimplemaineffect2ttestmethod" data-type = "select" data-title = "Select A Test"></a> to test the <em>simple main effect</em> on each level of '+$("#independent_factor").val()[0]+', with a <a href="#" id="nonparasimplemaineffect2correction" data-type="select"  data-title="Select A Adjust Method"></a> adjustment.<br>'
  )



  $("#nonparatwowayANOVAmethod").editable({
    value:'nonparatwowayANOVA',
    source:[
      {value:'nonparatwowayANOVA',text:'non-parametric two way ANOVA'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        nonparatwowayANOVAmethod = newValue;
    }
  })
  $("#nonparamaineffect1ANOVAmethod").editable({
    value:'htest',
    source:[
      {value:'htest',text:'Kruskal-Wallis H Test'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        nonparamaineffect1ANOVAmethod = newValue;
    }
  })
  $("#nonparamaineffect1posthoc").editable({
    value:'dunn',
    source:[
      {value:'dunn',text:'Dunn\'s (1964) procedure with a Bonferroni adjustment'},
      {value:'utest',text:'Mann-Whitney U tests with a Bonferroni adjustment'},
      {value: 'none', text:  'no'}
    ],
    success: function(response, newValue) {
        nonparamaineffect1posthoc = newValue;
    }
  })
  $("#nonparasimplemaineffect1ANOVAmethod").editable({
    value:'htest',
    source:[
      {value:'htest',text:'Kruskal-Wallis H Test'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        nonparasimplemaineffect1ANOVAmethod = newValue;
    }
  })
  $("#nonparasimplemaineffect1posthoc").editable({
    value:'dunn',
    source:[
      {value:'dunn',text:'Dunn\'s (1964) procedure with a Bonferroni adjustment'},
      {value:'utest',text:'Mann-Whitney U tests with a Bonferroni adjustment'},
      {value: 'none', text:  'no'}
    ],
    success: function(response, newValue) {
        nonparasimplemaineffect1posthoc = newValue;
    }
  })
$("#nonparamaineffect2ttestmethod").editable({
       value: 'utest',
    source:[
      {value: 'utest', text: 'Mann–Whitney U test'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        nonparamaineffect2ttestmethod = newValue;
    }
  })
  $("#nonparamaineffect2correction").editable({
    value:'fdr',
    source:[
      {value:'fdr', text: 'Benjamini & Hochberg (1995)'},
      {value:'holm',text: 'Holm (1979)'},
      {value:'hochberg',text:'Hochberg (1988)'},
      {value:'hommel',text:'Hommel (1988)'},
      {value:'bonferroni',text:'Bonferroni correction'},
      {value:'BY', text:'Benjamini & Yekutieli (2001)'},
      {value:'none',text:'none'}
    ],
    success: function(response, newValue) {
        nonparamaineffect2correction = newValue;
    }
  })
  $("#nonparasimplemaineffect2ttestmethod").editable({
       value: 'utest',
    source:[
      {value: 'utest', text: 'Mann–Whitney U test'},
      {value: 'none', text:  'no test'}
    ],
    success: function(response, newValue) {
        nonparasimplemaineffect2ttestmethod = newValue;
    }
  })
  $("#nonparasimplemaineffect2correction").editable({
    value:'fdr',
    source:[
      {value:'fdr', text: 'Benjamini & Hochberg (1995)'},
      {value:'holm',text: 'Holm (1979)'},
      {value:'hochberg',text:'Hochberg (1988)'},
      {value:'hommel',text:'Hommel (1988)'},
      {value:'bonferroni',text:'Bonferroni correction'},
      {value:'BY', text:'Benjamini & Yekutieli (2001)'},
      {value:'none',text:'none'}
    ],
    success: function(response, newValue) {
        nonparasimplemaineffect2correction = newValue;
    }
  })

}
