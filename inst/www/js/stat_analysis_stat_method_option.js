// T test.<a href="#" id="onewayANOVAmethod" data-type="select"  data-title="Select ANOVA method"></a>
t_test_disc = function(){
window.ttestmethod = 'Welch';
window.ttestcorrection = 'fdr';
window.nonparattestmethod = 'utest';
window.nonparattestcorrection = 'fdr';



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
        window.ttestmethod = newValue;
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
      window.ttestcorrection = newValue;
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
        window.nonparattestmethod = newValue;
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
      window.nonparattestcorrection = newValue;
    }
  })

}


ANOVA_disc = function(){
  window.ANOVAmethod = 'Welch';
  window.ANOVAposthoc = 'games.howell';
  window.nonparaANOVAmethod = 'htest';//Kruskal-Wallis H test
  window.nonparaANOVAposthoc = 'dunn';

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
    ]
  })
  $("#ANOVAposthoc").editable({
    value:'games.howell',
    source:[
      {value:'tukey',text:'Tukey'},
      {value:'games.howell',text:'Games-Howell'},
      {value: 'none', text:  'no'}
    ]
  })

   $("#method_description_non_para").html(
    '<h4>Non-Parameter</h4>Use <a href="#" id="nonparaANOVAmethod" data-type = "select" data-title = "Select A Test"></a> on factor <strong>' + $("#independent_factor").val() +'</strong>, followed by <a href="#" id="nonparaANOVAposthoc" data-type="select"  data-title="Select A post hoc analysis procedure"></a> post hoc analysis to compare between <em>' +levels+"</em> and <em>"+ lastlevel+ "</em>."
  )
  $("#nonparaANOVAmethod").editable({
    value:'htest',
    source:[
      {value:'htest',text:'Kruskal-Wallis H Test'},
      {value: 'none', text:  'no test'}
    ]
  })
  $("#nonparaANOVAposthoc").editable({
    value:'dunn',
    source:[
      {value:'dunn',text:'Dunn\'s (1964) procedure with a Bonferroni adjustment'},
      {value:'utest',text:'Mann-Whitney U tests with a Bonferroni adjustment'},
      {value: 'none', text:  'no'}
    ]
  })
}
