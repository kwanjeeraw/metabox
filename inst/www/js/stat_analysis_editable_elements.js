var allow = false; //to determine if the pop up window should do statistics



loadxeditable_elements = function(){
$('#twowayANOVAmethod').editable({
        showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
        value: 'Two Way ANOVA',
        source: [
              {value: 'Two Way ANOVA', text: 'Two Way ANOVA'}
              //,{value: 'None', text: 'None'}
           ],
        success: function(response, newValue) {
        twowayANOVAmethod.value = newValue;
    }
    });
$('#maineffectANOVAmethod1').editable({
        showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
        value: 'Welch ANOVA',
        source: [
              {value: 'Welch ANOVA', text: 'one way Welch\'s ANOVA'},
              {value: 'ANOVA', text:'one way ANOVA'}
              //,{value: 'None', text: 'None'}
           ],
        success: function(response, newValue) {
        maineffectANOVAmethod1.value = newValue;
    }
    });
$('#maineffectANOVAmethod2').editable({
        showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
        value: 'Welch ANOVA',
        source: [
              {value: 'Welch ANOVA', text: 'one way Welch\'s ANOVA'},
              {value: 'ANOVA', text:'one way ANOVA'}
              //,{value: 'None', text: 'None'}
           ],
        success: function(response, newValue) {
        maineffectANOVAmethod2.value = newValue;
    }
    });
$('#maineffectANOVAposthocmethod1').editable({
        showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
        value: 'games.howell',
        source: [
              {value: 'games.howell', text: 'Game Howell post hoc analysis'},
              {value: 'tukey', text: 'Tukey  post hoc analysis'}
              //,{value: 'None', text: 'None'}
           ],
        success: function(response, newValue) {
        maineffectANOVAposthocmethod1.value = newValue;
    }
    });
$('#maineffectANOVAposthocmethod2').editable({
        showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
        value: 'games.howell',
        source: [
              {value: 'games.howell', text: 'Game Howell post hoc analysis'},
              {value: 'tukey', text: 'Tukey  post hoc analysis'}
              //,{value: 'None', text: 'None'}
           ],
        success: function(response, newValue) {
        maineffectANOVAposthocmethod2.value = newValue;
    }
    });
$('#simplemaineffectANOVAmethod1').editable({
        showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
        value: 'Welch ANOVA',
        source: [
              {value: 'Welch ANOVA', text: 'one way Welch\'s ANOVA'},
              {value: 'ANOVA', text:'one way ANOVA'}
              //,{value: 'None', text: 'None'}
           ],
        success: function(response, newValue) {
        simplemaineffectANOVAmethod1.value = newValue;
    }
    });
$('#simplemaineffectANOVAmethod2').editable({
        showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
        value: 'Welch ANOVA',
        source: [
              {value: 'Welch ANOVA', text: 'one way Welch\'s ANOVA'},
              {value: 'ANOVA', text:'one way ANOVA'}
              //,{value: 'None', text: 'None'}
           ],
        success: function(response, newValue) {
        simplemaineffectANOVAmethod2.value = newValue;
    }
    });
$('#simplemaineffectANOVAposthocmethod1').editable({
        showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
        value: 'games.howell',
        source: [
              {value: 'games.howell', text: 'Game Howell post hoc analysis'},
              {value: 'tukey', text: 'Tukey  post hoc analysis'}
              //,{value: 'None', text: 'None'}
           ],
        success: function(response, newValue) {
        simplemaineffectANOVAposthocmethod1.value = newValue;
    }
    });
$('#simplemaineffectANOVAposthocmethod2').editable({
        showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
        value: 'games.howell',
        source: [
              {value: 'games.howell', text: 'Game Howell post hoc analysis'},
              {value: 'tukey', text: 'Tukey  post hoc analysis'}
              //,{value: 'None', text: 'None'}
           ],
        success: function(response, newValue) {
        simplemaineffectANOVAposthocmethod2.value = newValue;
    }
    });
$('#nonpara_maineffectANOVAmethod1').editable({
        showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
        value: 'H test',
        source: [
              {value: 'H test', text: 'Krukal-Wallis H test'}
              //,{value: 'None', text: 'None'}
           ],
        success: function(response, newValue) {
        nonpara_maineffectANOVAmethod1.value = newValue;
    }
    });
$('#nonpara_maineffectANOVAmethod2').editable({
        showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
        value: 'H test',
        source: [
              {value: 'H test', text: 'Krukal-Wallis H test'}
              //,{value: 'None', text: 'None'}
           ],
        success: function(response, newValue) {
        nonpara_maineffectANOVAmethod2.value = newValue;
    }
    });
$('#nonpara_maineffectANOVAposthocmethod1').editable({
        showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
      value: 'Dunn',
      source: [
            {value: 'Dunn', text: 'Dunn\'s (1964) procedure with a Bonferroni correction'}
           //,{value: 'None', text: 'None'}
         ],
        success: function(response, newValue) {
        nonpara_maineffectANOVAposthocmethod1.value = newValue;
    }
    });
$('#nonpara_maineffectANOVAposthocmethod2').editable({
        showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
      value: 'Dunn',
      source: [
            {value: 'Dunn', text: 'Dunn\'s (1964) procedure with a Bonferroni correction'}
           //,{value: 'None', text: 'None'}
         ],
        success: function(response, newValue) {
        nonpara_maineffectANOVAposthocmethod2.value = newValue;
    }
    });
$('#nonpara_simplemaineffectANOVAmethod1').editable({
        showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
         value: 'H test',
        source: [
              {value: 'H test', text: 'Krukal-Wallis H test'}
              //,{value: 'None', text: 'None'}
           ],
        success: function(response, newValue) {
        nonpara_simplemaineffectANOVAmethod1.value = newValue;
    }
    });
$('#nonpara_simplemaineffectANOVAmethod2').editable({
        showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
         value: 'H test',
        source: [
              {value: 'H test', text: 'Krukal-Wallis H test'}
              //,{value: 'None', text: 'None'}
           ],
        success: function(response, newValue) {
        nonpara_simplemaineffectANOVAmethod2.value = newValue;
    }
    });
$('#nonpara_simplemaineffectANOVAposthocmethod1').editable({
        showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
       value: 'Dunn',
      source: [
            {value: 'Dunn', text: 'Dunn\'s (1964) procedure with a Bonferroni correction'}
           //,{value: 'None', text: 'None'}
         ],
        success: function(response, newValue) {
        nonpara_simplemaineffectANOVAposthocmethod1.value = newValue;
    }
    });
$('#nonpara_simplemaineffectANOVAposthocmethod2').editable({
        showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
        value: 'Dunn',
      source: [
            {value: 'Dunn', text: 'Dunn\'s (1964) procedure with a Bonferroni correction'}
           //,{value: 'None', text: 'None'}
         ],
        success: function(response, newValue) {
        nonpara_simplemaineffectANOVAposthocmethod2.value = newValue;
    }
    });
$('#maineffectttestmethod1').editable({
        showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
        value: 'Welch t test',
        source: [
              {value: 'Welch t test', text: 'Welch\'s t test'},
              {value: 't test', text:'t test'}
              //,{value: 'None', text: 'None'}
           ],
        success: function(response, newValue) {
        maineffectttestmethod1.value = newValue;
    }
    });
$('#maineffectttestmethod2').editable({
        showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
        value: 'Welch t test',
        source: [
              {value: 'Welch t test', text: 'Welch\'s t test'},
              {value: 't test', text:'t test'}
              //,{value: 'None', text: 'None'}
           ],
        success: function(response, newValue) {
        maineffectttestmethod2.value = newValue;
    }
    });
$('#maineffectttestposthocmethod1').editable({
        showbuttons: false,
      unsavedclass: null,
      type: 'select',
      inputclass: 'input-medium privacy-select',
      value: 'fdr',
      source: [
            {value: 'fdr', text: 'Benjamini–Hochberg procedure'},
            {value: 'holm',text: "Holm (1979)"},
            {value: 'hochberg', text:"Hochberg (1988)"},
            {value: 'hommel', text:"Hommel (1988)"},
            {value: 'BY', text:"Benjamini-Yekutieli (2001)"},
            {value: 'none', text: 'None'}
         ],
        success: function(response, newValue) {
        maineffectttestposthocmethod1.value = newValue;
    }
    });
$('#maineffectttestposthocmethod2').editable({
        showbuttons: false,
      unsavedclass: null,
      type: 'select',
      inputclass: 'input-medium privacy-select',
      value: 'fdr',
      source: [
            {value: 'fdr', text: 'Benjamini–Hochberg procedure'},
            {value: 'holm',text: "Holm (1979)"},
            {value: 'hochberg', text:"Hochberg (1988)"},
            {value: 'hommel', text:"Hommel (1988)"},
            {value: 'BY', text:"Benjamini-Yekutieli (2001)"},
            {value: 'none', text: 'None'}
         ],
        success: function(response, newValue) {
        maineffectttestposthocmethod2.value = newValue;
    }
    });
$('#simplemaineffectttestmethod1').editable({
        showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
        value: 'Welch t test',
        source: [
              {value: 'Welch t test', text: 'Welch\'s t test'},
              {value: 't test', text: 't test'}
              //,{value: 'None', text: 'None'}
           ],
        success: function(response, newValue) {
        simplemaineffectttestmethod1.value = newValue;
    }
    });
$('#simplemaineffectttestmethod2').editable({
        showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
        value: 'Welch t test',
        source: [
              {value: 'Welch t test', text: 'Welch\'s t test'},
              {value: 't test', text:'t test'}
              //,{value: 'None', text: 'None'}
           ],
        success: function(response, newValue) {
        simplemaineffectttestmethod2.value = newValue;
    }
    });
$('#simplemaineffectttestposthocmethod1').editable({
        showbuttons: false,
      unsavedclass: null,
      type: 'select',
      inputclass: 'input-medium privacy-select',
      value: 'fdr',
      source: [
            {value: 'fdr', text: 'Benjamini–Hochberg procedure'},
            {value: 'holm',text: "Holm (1979)"},
            {value: 'hochberg', text:"Hochberg (1988)"},
            {value: 'hommel', text:"Hommel (1988)"},
            {value: 'BY', text:"Benjamini-Yekutieli (2001)"},
            {value: 'none', text: 'None'}
         ],
        success: function(response, newValue) {
        simplemaineffectttestposthocmethod1.value = newValue;
    }
    });
$('#simplemaineffectttestposthocmethod2').editable({
        showbuttons: false,
      unsavedclass: null,
      type: 'select',
      inputclass: 'input-medium privacy-select',
      value: 'fdr',
      source: [
            {value: 'fdr', text: 'Benjamini–Hochberg procedure'},
            {value: 'holm',text: "Holm (1979)"},
            {value: 'hochberg', text:"Hochberg (1988)"},
            {value: 'hommel', text:"Hommel (1988)"},
            {value: 'BY', text:"Benjamini-Yekutieli (2001)"},
            {value: 'none', text: 'None'}
         ],
        success: function(response, newValue) {
        simplemaineffectttestposthocmethod2.value = newValue;
    }
    });
$('#nonpara_maineffectttestmethod1').editable({
        showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
        value: 'U test',
        source: [
              {value: 'U test', text: 'Mann-Whitney U test'}
              //,{value: 'None', text: 'None'}
           ],
        success: function(response, newValue) {
        nonpara_maineffectttestmethod1.value = newValue;
    }
    });
$('#nonpara_maineffectttestmethod2').editable({
        showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
        value: 'U test',
        source: [
              {value: 'U test', text: 'Mann-Whitney U test'}
              //,{value: 'None', text: 'None'}
           ],
        success: function(response, newValue) {
        nonpara_maineffectttestmethod2.value = newValue;
    }
    });
$('#nonpara_maineffectttestposthocmethod1').editable({
        showbuttons: false,
      unsavedclass: null,
      type: 'select',
      inputclass: 'input-medium privacy-select',
      value: 'fdr',
      source: [
            {value: 'fdr', text: 'Benjamini–Hochberg procedure'},
            {value: 'holm',text: "Holm (1979)"},
            {value: 'hochberg', text:"Hochberg (1988)"},
            {value: 'hommel', text:"Hommel (1988)"},
            {value: 'BY', text:"Benjamini-Yekutieli (2001)"},
            {value: 'none', text: 'None'}
         ],
        success: function(response, newValue) {
        nonpara_maineffectttestposthocmethod1.value = newValue;
    }
    });
$('#nonpara_maineffectttestposthocmethod2').editable({
        showbuttons: false,
      unsavedclass: null,
      type: 'select',
      inputclass: 'input-medium privacy-select',
      value: 'fdr',
      source: [
            {value: 'fdr', text: 'Benjamini–Hochberg procedure'},
            {value: 'holm',text: "Holm (1979)"},
            {value: 'hochberg', text:"Hochberg (1988)"},
            {value: 'hommel', text:"Hommel (1988)"},
            {value: 'BY', text:"Benjamini-Yekutieli (2001)"},
            {value: 'none', text: 'None'}
         ],
        success: function(response, newValue) {
        nonpara_maineffectttestposthocmethod2.value = newValue;
    }
    });
$('#nonpara_simplemaineffectttestmethod1').editable({
        showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
         value: 'U test',
        source: [
              {value: 'U test', text: 'Mann-Whitney U test'}
              //,{value: 'None', text: 'None'}
           ],
        success: function(response, newValue) {
        nonpara_simplemaineffectttestmethod1.value = newValue;
    }
    });
$('#nonpara_simplemaineffectttestmethod2').editable({
        showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
         value: 'U test',
        source: [
              {value: 'U test', text: 'Mann-Whitney U test'}
              //,{value: 'None', text: 'None'}
           ],
        success: function(response, newValue) {
        nonpara_simplemaineffectttestmethod2.value = newValue;
    }
    });
$('#nonpara_simplemaineffectttestposthocmethod1').editable({
        showbuttons: false,
      unsavedclass: null,
      type: 'select',
      inputclass: 'input-medium privacy-select',
      value: 'fdr',
      source: [
            {value: 'fdr', text: 'Benjamini–Hochberg procedure'},
            {value: 'holm',text: "Holm (1979)"},
            {value: 'hochberg', text:"Hochberg (1988)"},
            {value: 'hommel', text:"Hommel (1988)"},
            {value: 'BY', text:"Benjamini-Yekutieli (2001)"},
            {value: 'none', text: 'None'}
         ],
        success: function(response, newValue) {
        nonpara_simplemaineffectttestposthocmethod1.value = newValue;
    }
    });
$('#nonpara_simplemaineffectttestposthocmethod2').editable({
        showbuttons: false,
      unsavedclass: null,
      type: 'select',
      inputclass: 'input-medium privacy-select',
      value: 'fdr',
      source: [
            {value: 'fdr', text: 'Benjamini–Hochberg procedure'},
            {value: 'holm',text: "Holm (1979)"},
            {value: 'hochberg', text:"Hochberg (1988)"},
            {value: 'hommel', text:"Hommel (1988)"},
            {value: 'BY', text:"Benjamini-Yekutieli (2001)"},
            {value: 'none', text: 'None'}
         ],
        success: function(response, newValue) {
        nonpara_simplemaineffectttestposthocmethod2.value = newValue;
    }
    });






$('#twowayrepeatedANOVAmethod').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'Two Way Repeated ANOVA',
  source: [
    {value: 'Two Way Repeated ANOVA', text: 'Two Way Repeated ANOVA'}

    ],
  success: function(response, newValue) {
    twowayrepeatedANOVAmethod.value = newValue;
  }
});
$('#maineffectrepeatedANOVAmethod1').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'rANOVA',
  source: [
    {value: 'rANOVA', text: 'one way repeated ANOVA'}

    ],
  success: function(response, newValue) {
    maineffectrepeatedANOVAmethod1.value = newValue;
  }
});
$('#maineffectrepeatedANOVAmethod2').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'rANOVA',
  source: [
    {value: 'rANOVA', text: 'one way repeated ANOVA'}
    ],
  success: function(response, newValue) {
    maineffectrepeatedANOVAmethod2.value = newValue;
  }
});
$('#maineffectrepeatedANOVAposthocmethod1').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'GG',
  source: [
    {value: 'GG', text: 'Game Howell post hoc analysis'},
    {value: 'HF', text: 'Tukey  post hoc analysis'}
    ,{value: 'none', text: 'None'}
    ],
  success: function(response, newValue) {
    maineffectrepeatedANOVAposthocmethod1.value = newValue;
  }
});
$('#maineffectrepeatedANOVAposthocmethod2').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'GG',
  source: [
    {value: 'GG', text: 'Game Howell post hoc analysis'},
    {value: 'HF', text: 'Tukey  post hoc analysis'}
   ,{value: 'none', text: 'None'}
    ],
  success: function(response, newValue) {
    maineffectrepeatedANOVAposthocmethod2.value = newValue;
  }
});
$('#simplemaineffectrepeatedANOVAmethod1').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'rANOVA',
  source: [
    {value: 'rANOVA', text: 'one way repeated ANOVA'}
    ],
  success: function(response, newValue) {
    simplemaineffectrepeatedANOVAmethod1.value = newValue;
  }
});
$('#simplemaineffectrepeatedANOVAmethod2').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'rANOVA',
  source: [
    {value: 'rANOVA', text: 'one way repeated ANOVA'}
    ],
  success: function(response, newValue) {
    simplemaineffectrepeatedANOVAmethod2.value = newValue;
  }
});
$('#simplemaineffectrepeatedANOVAposthocmethod1').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'GG',
  source: [
    {value: 'GG', text: 'Game Howell post hoc analysis'},
    {value: 'HF', text: 'Tukey  post hoc analysis'}
    ,{value: 'none', text: 'None'}
    ],
  success: function(response, newValue) {
    simplemaineffectrepeatedANOVAposthocmethod1.value = newValue;
  }
});
$('#simplemaineffectrepeatedANOVAposthocmethod2').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'GG',
  source: [
    {value: 'GG', text: 'Game Howell post hoc analysis'},
    {value: 'HF', text: 'Tukey  post hoc analysis'},{value: 'none', text: 'None'}
    ],
  success: function(response, newValue) {
    simplemaineffectrepeatedANOVAposthocmethod2.value = newValue;
  }
});
$('#nonpara_maineffectrepeatedANOVAmethod1').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: ' friedman',
  source: [
    {value: ' friedman', text: 'Friedman\'s Test'}
    //,{value: 'none', text: 'None'}
    ],
  success: function(response, newValue) {
    nonpara_maineffectrepeatedANOVAmethod1.value = newValue;
  }
});
$('#nonpara_maineffectrepeatedANOVAmethod2').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: ' friedman',
  source: [
    {value: ' friedman', text: 'Friedman\'s Test'}
    //,{value: 'none', text: 'None'}
    ],
  success: function(response, newValue) {
    nonpara_maineffectrepeatedANOVAmethod2.value = newValue;
  }
});
$('#nonpara_maineffectrepeatedANOVAposthocmethod1').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'nonpara_paired+bonf',
  source: [
    {value: 'nonpara_paired+bonf', text: 'Wilcoxon signed-rank test with Bonferroni Correction'}
    //,{value: 'none', text: 'None'}
    ],
  success: function(response, newValue) {
    nonpara_maineffectrepeatedANOVAposthocmethod1.value = newValue;
  }
});
$('#nonpara_maineffectrepeatedANOVAposthocmethod2').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'nonpara_paired+bonf',
  source: [
    {value: 'nonpara_paired+bonf', text: 'Wilcoxon signed-rank test with Bonferroni Correction'}
    //,{value: 'none', text: 'None'}
    ],
  success: function(response, newValue) {
    nonpara_maineffectrepeatedANOVAposthocmethod2.value = newValue;
  }
});
$('#nonpara_simplemaineffectrepeatedANOVAmethod1').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: ' friedman',
  source: [
    {value: ' friedman', text: 'Friedman\'s Test'}
    //,{value: 'none', text: 'None'}
    ],
  success: function(response, newValue) {
    nonpara_simplemaineffectrepeatedANOVAmethod1.value = newValue;
  }
});
$('#nonpara_simplemaineffectrepeatedANOVAmethod2').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: ' friedman',
  source: [
    {value: ' friedman', text: 'Friedman\'s Test'}
    //,{value: 'none', text: 'None'}
    ],
  success: function(response, newValue) {
    nonpara_simplemaineffectrepeatedANOVAmethod2.value = newValue;
  }
});
$('#nonpara_simplemaineffectrepeatedANOVAposthocmethod1').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'nonpara_paired+bonf',
  source: [
    {value: 'nonpara_paired+bonf', text: 'Wilcoxon signed-rank test with Bonferroni Correction'}
    //,{value: 'none', text: 'None'}
    ],
  success: function(response, newValue) {
    nonpara_simplemaineffectrepeatedANOVAposthocmethod1.value = newValue;
  }
});
$('#nonpara_simplemaineffectrepeatedANOVAposthocmethod2').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'nonpara_paired+bonf',
  source: [
    {value: 'nonpara_paired+bonf', text: 'Wilcoxon signed-rank test with Bonferroni Correction'}
    //,{value: 'none', text: 'None'}
    ],
  success: function(response, newValue) {
    nonpara_simplemaineffectrepeatedANOVAposthocmethod2.value = newValue;
  }
});



$('#maineffectpairedttestmethod1').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'paired t test',
  source: [
    {value: 'paired t test', text: 'paired t test'}
    //,{value: 'none', text: 'None'}
    ],
  success: function(response, newValue) {
    maineffectpairedttestmethod1.value = newValue;
  }
});
$('#maineffectpairedttestmethod2').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'paired t test',
  source: [
    {value: 'paired t test', text: 'paired t test'}
    //,{value: 'none', text: 'None'}
    ],
  success: function(response, newValue) {
    maineffectpairedttestmethod2.value = newValue;
  }
});
$('#maineffectpairedttestposthocmethod1').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'fdr',
  source: [
    {value: 'fdr', text: 'Benjamini–Hochberg procedure'},
    {value: 'holm',text: "Holm (1979)"},
    {value: 'hochberg', text:"Hochberg (1988)"},
    {value: 'hommel', text:"Hommel (1988)"},
    {value: 'BY', text:"Benjamini-Yekutieli (2001)"},
    {value: 'none', text: 'None'}
    ],
  success: function(response, newValue) {
    maineffectpairedttestposthocmethod1.value = newValue;
  }
});
$('#maineffectpairedttestposthocmethod2').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'fdr',
  source: [
    {value: 'fdr', text: 'Benjamini–Hochberg procedure'},
    {value: 'holm',text: "Holm (1979)"},
    {value: 'hochberg', text:"Hochberg (1988)"},
    {value: 'hommel', text:"Hommel (1988)"},
    {value: 'BY', text:"Benjamini-Yekutieli (2001)"},
    {value: 'none', text: 'None'}
    ],
  success: function(response, newValue) {
    maineffectpairedttestposthocmethod2.value = newValue;
  }
});
$('#simplemaineffectpairedttestmethod1').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'paired t test',
  source: [
    {value: 'paired t test', text: 'paired t test'}
    //,{value: 'none', text: 'None'}
    ],
  success: function(response, newValue) {
    simplemaineffectpairedttestmethod1.value = newValue;
  }
});
$('#simplemaineffectpairedttestmethod2').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'paired t test',
  source: [
    {value: 'paired t test', text: 'paired t test'}
    //,{value: 'none', text: 'None'}
    ],
  success: function(response, newValue) {
    simplemaineffectpairedttestmethod2.value = newValue;
  }
});
$('#simplemaineffectpairedttestposthocmethod1').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'fdr',
  source: [
    {value: 'fdr', text: 'Benjamini–Hochberg procedure'},
    {value: 'holm',text: "Holm (1979)"},
    {value: 'hochberg', text:"Hochberg (1988)"},
    {value: 'hommel', text:"Hommel (1988)"},
    {value: 'BY', text:"Benjamini-Yekutieli (2001)"},
    {value: 'none', text: 'None'}
    ],
  success: function(response, newValue) {
    simplemaineffectpairedttestposthocmethod1.value = newValue;
  }
});
$('#simplemaineffectpairedttestposthocmethod2').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'fdr',
  source: [
    {value: 'fdr', text: 'Benjamini–Hochberg procedure'},
    {value: 'holm',text: "Holm (1979)"},
    {value: 'hochberg', text:"Hochberg (1988)"},
    {value: 'hommel', text:"Hommel (1988)"},
    {value: 'BY', text:"Benjamini-Yekutieli (2001)"},
    {value: 'none', text: 'None'}
    ],
  success: function(response, newValue) {
    simplemaineffectpairedttestposthocmethod2.value = newValue;
  }
});
$('#nonpara_maineffectpairedttestmethod1').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'Wil test',
  source: [
    {value: 'Wil test', text: 'Wilcoxon signed-rank test'}
    //,{value: 'none', text: 'None'}
    ],
  success: function(response, newValue) {
    nonpara_maineffectpairedttestmethod1.value = newValue;
  }
});
$('#nonpara_maineffectpairedttestmethod2').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'Wil test',
  source: [
    {value: 'Wil test', text: 'Wilcoxon signed-rank test'}
    //,{value: 'none', text: 'None'}
    ],
  success: function(response, newValue) {
    nonpara_maineffectpairedttestmethod2.value = newValue;
  }
});
$('#nonpara_maineffectpairedttestposthocmethod1').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'fdr',
  source: [
    {value: 'fdr', text: 'Benjamini–Hochberg procedure'},
    {value: 'holm',text: "Holm (1979)"},
    {value: 'hochberg', text:"Hochberg (1988)"},
    {value: 'hommel', text:"Hommel (1988)"},
    {value: 'BY', text:"Benjamini-Yekutieli (2001)"},
    {value: 'none', text: 'None'}
    ],
  success: function(response, newValue) {
    nonpara_maineffectpairedttestposthocmethod1.value = newValue;
  }
});
$('#nonpara_maineffectpairedttestposthocmethod2').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'fdr',
  source: [
    {value: 'fdr', text: 'Benjamini–Hochberg procedure'},
    {value: 'holm',text: "Holm (1979)"},
    {value: 'hochberg', text:"Hochberg (1988)"},
    {value: 'hommel', text:"Hommel (1988)"},
    {value: 'BY', text:"Benjamini-Yekutieli (2001)"},
    {value: 'none', text: 'None'}
    ],
  success: function(response, newValue) {
    nonpara_maineffectpairedttestposthocmethod2.value = newValue;
  }
});
$('#nonpara_simplemaineffectpairedttestmethod1').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'Wil test',
  source: [
    {value: 'Wil test', text: 'Wilcoxon signed-rank test'}
    //,{value: 'none', text: 'None'}
    ],
  success: function(response, newValue) {
    nonpara_simplemaineffectpairedttestmethod1.value = newValue;
  }
});
$('#nonpara_simplemaineffectpairedttestmethod2').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'Wil test',
  source: [
    {value: 'Wil test', text: 'Wilcoxon signed-rank test'}
    //,{value: 'none', text: 'None'}
    ],
  success: function(response, newValue) {
    nonpara_simplemaineffectpairedttestmethod2.value = newValue;
  }
});
$('#nonpara_simplemaineffectpairedttestposthocmethod1').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'fdr',
  source: [
    {value: 'fdr', text: 'Benjamini–Hochberg procedure'},
    {value: 'holm',text: "Holm (1979)"},
    {value: 'hochberg', text:"Hochberg (1988)"},
    {value: 'hommel', text:"Hommel (1988)"},
    {value: 'BY', text:"Benjamini-Yekutieli (2001)"},
    {value: 'none', text: 'None'}
    ],
  success: function(response, newValue) {
    nonpara_simplemaineffectpairedttestposthocmethod1.value = newValue;
  }
});
$('#nonpara_simplemaineffectpairedttestposthocmethod2').editable({
  showbuttons: false,
  unsavedclass: null,
  type: 'select',
  inputclass: 'input-medium privacy-select',
  value: 'fdr',
  source: [
    {value: 'fdr', text: 'Benjamini–Hochberg procedure'},
    {value: 'holm',text: "Holm (1979)"},
    {value: 'hochberg', text:"Hochberg (1988)"},
    {value: 'hommel', text:"Hommel (1988)"},
    {value: 'BY', text:"Benjamini-Yekutieli (2001)"},
    {value: 'none', text: 'None'}
    ],
  success: function(response, newValue) {
    nonpara_simplemaineffectpairedttestposthocmethod2.value = newValue;
  }
});










$('#onewayANOVAmethod').editable({
        showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
        value: 'Welch ANOVA',
        source: [
              {value: 'Welch ANOVA', text: 'one way Welch\'s ANOVA'},
              {value: 'ANOVA', text: 'one way ANOVA'}
              //,{value: 'None', text: 'None'}
           ],
        success: function(response, newValue) {
        onewayANOVAmethod.value = newValue;
    }
    });
$('#nonpara_onewayANOVAmethod').editable({
          showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
      value: 'H test',
      source: [
            {value: 'H test', text: 'Krukal-Wallis H test'}
           //,{value: 'None', text: 'None'}
         ],
      success: function(response, newValue) {
      nonpara_onewayANOVAmethod.value = newValue;
  }
  });
$('#onewayANOVAposthocmethod').editable({
        showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
  value: 'games.howell',
  source: [
        {value: 'games.howell', text: 'Game Howell post hoc analysis'},
        {value: 'tukey', text: 'Tukey  post hoc analysis'}
        //,{value: 'None', text: 'None'}
     ],
   success: function(response, newValue) {
  onewayANOVAposthocmethod.value = newValue;
}
});
$('#nonpara_onewayANOVAposthocmethod').editable({
          showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
      value: 'Dunn',
      source: [
            {value: 'Dunn', text: 'Dunn\'s (1964) procedure with a Bonferroni correction'}
           //,{value: 'None', text: 'None'}
         ],
      success: function(response, newValue) {
      nonpara_onewayANOVAposthocmethod.value = newValue;
  }
  });

$('#ttestmethod').editable({
          showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
      value: 'Welch t test',
      source: [
            {value: 'Welch t test', text: 'Welch\'s t test'},
            {value: 't test', text: 't test'}
            //,{value: 'None', text: 'None'}
         ],
      success: function(response, newValue) {
      ttestmethod.value = newValue;
  }
  });
$('#ttestFDRmethod').editable({
      showbuttons: false,
      unsavedclass: null,
      type: 'select',
      inputclass: 'input-medium privacy-select',
      value: 'fdr',
      source: [
            {value: 'fdr', text: 'Benjamini–Hochberg procedure'},
            {value: 'holm',text: "Holm (1979)"},
            {value: 'hochberg', text:"Hochberg (1988)"},
            {value: 'hommel', text:"Hommel (1988)"},
            {value: 'BY', text:"Benjamini-Yekutieli (2001)"},
            {value: 'none', text: 'None'}
         ],
      success: function(response, newValue) {
      ttestFDRmethod.value = newValue;
  }
  });
$('#nonpara_ttestmethod').editable({
          showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
  value: 'U test',
  source: [
        {value: 'U test', text: 'Mann–Whitney U test'}
       //,{value: 'None', text: 'None'}
     ],
   success: function(response, newValue) {
  nonpara_ttestmethod.value = newValue;
}
});
$('#nonpara_ttestFDRmethod').editable({
          showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
      value: 'fdr',
      source: [
            {value: 'fdr', text: 'Benjamini–Hochberg procedure'},
            {value: 'holm',text: "Holm (1979)"},
            {value: 'hochberg', text:"Hochberg (1988)"},
            {value: 'hommel', text:"Hommel (1988)"},
            {value: 'BY', text:"Benjamini-Yekutieli (2001)"},
            {value: 'none', text: 'None'}
         ],
      success: function(response, newValue) {
      nonpara_ttestFDRmethod.value = newValue;
  }
  });

$("#onewayrepeatedANOVAmethod").editable({
        showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
        value: 'rANOVA',
        source: [
              {value: 'rANOVA', text: 'one way repeated ANOVA'},

              //,{value: 'None', text: 'None'}
           ],
        success: function(response, newValue) {
        onewayrepeatedANOVAmethod.value = newValue;
    }
});
$("#onewaySpher_Corr").editable({
    showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
        value: 'GG',
        source: [
              {value: 'GG', text: 'Greenhouse-Geisser'},
              {value: 'HF', text: 'Huynh-Feldt'},
              {value: 'none', text: 'None'}
           ],
        success: function(response, newValue) {
        onewaySpher_Corr.value = newValue;
    }
});

$("#mainSpher_Corr1").editable({
    showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
        value: 'GG',
        source: [
              {value: 'GG', text: 'Greenhouse-Geisser'},
              {value: 'HF', text: 'Huynh-Feldt'},
              {value: 'none', text: 'None'}
           ],
        success: function(response, newValue) {
        mainSpher_Corr1.value = newValue;
    }
});
$("#mainSpher_Corr2").editable({
    showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
        value: 'GG',
        source: [
              {value: 'GG', text: 'Greenhouse-Geisser'},
              {value: 'HF', text: 'Huynh-Feldt'},
              {value: 'none', text: 'None'}
           ],
        success: function(response, newValue) {
        mainSpher_Corr2.value = newValue;
    }
});
$("#simplemainSpher_Corr2").editable({
    showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
        value: 'GG',
        source: [
              {value: 'GG', text: 'Greenhouse-Geisser'},
              {value: 'HF', text: 'Huynh-Feldt'},
              {value: 'none', text: 'None'}
           ],
        success: function(response, newValue) {
        simplemainSpher_Corr2.value = newValue;
    }
});
$("#simplemainSpher_Corr1").editable({
    showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
        value: 'GG',
        source: [
              {value: 'GG', text: 'Greenhouse-Geisser'},
              {value: 'HF', text: 'Huynh-Feldt'},
              {value: 'none', text: 'None'}
           ],
        success: function(response, newValue) {
        simplemainSpher_Corr1.value = newValue;
    }
});

$("#onewayrepeatedANOVAposthocmethod").editable({
  showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
        value: 'paired+bonf',
        source: [
              {value: 'paired+bonf', text: 'paired t test with Bonferroni Correction'}
              //,{value: 'None', text: 'None'}
           ],
        success: function(response, newValue) {
        onewayrepeatedANOVAposthocmethod.value = newValue;
    }
})
$("#nonpara_onewayrepeatedANOVAmethod").editable({
  showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
        value: 'friedman',
        source: [
              {value: 'friedman', text: 'Friedman\'s Test'},

              //,{value: 'None', text: 'None'}
           ],
        success: function(response, newValue) {
        nonpara_onewayrepeatedANOVAmethod.value = newValue;
    }
})
$("#nonpara_onewayrepeatedANOVAposthocmethod").editable({
  showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
        value: 'nonpara_paired+bonf',
        source: [
              {value: 'nonpara_paired+bonf', text: 'Wilcoxon signed-rank test with a Bonferroni Correction'}
              //,{value: 'None', text: 'None'}
           ],
        success: function(response, newValue) {
        nonpara_onewayrepeatedANOVAposthocmethod.value = newValue;
    }
})

$("#pairedttestmethod").editable({
    showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
      value: 'paired t test',
      source: [
            {value: 'paired t test', text: 'paired t test'}
            //,{value: 'None', text: 'None'}
         ],
      success: function(response, newValue) {
      pairedttestmethod.value = newValue;
  }
})
$("#pairedttestFDRmethod").editable({
      showbuttons: false,
      unsavedclass: null,
      type: 'select',
      inputclass: 'input-medium privacy-select',
      value: 'fdr',
      source: [
            {value: 'fdr', text: 'Benjamini–Hochberg procedure'},
            {value: 'holm',text: "Holm (1979)"},
            {value: 'hochberg', text:"Hochberg (1988)"},
            {value: 'hommel', text:"Hommel (1988)"},
            {value: 'BY', text:"Benjamini-Yekutieli (2001)"},
            {value: 'none', text: 'None'}
         ],
      success: function(response, newValue) {
      pairedttestFDRmethod.value = newValue;
  }
  });
$('#nonpara_pairedttestmethod').editable({
          showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
  value: 'Wil test',
  source: [
        {value: 'Wil test', text: 'Wilcoxon signed-rank test'}
       //,{value: 'None', text: 'None'}
     ],
   success: function(response, newValue) {
  nonpara_pairedttestmethod.value = newValue;
}
});
$('#nonpara_pairedttestFDRmethod').editable({
          showbuttons: false,
        unsavedclass: null,
        type: 'select',
        inputclass: 'input-medium privacy-select',
      value: 'fdr',
      source: [
            {value: 'fdr', text: 'Benjamini–Hochberg procedure'},
            {value: 'holm',text: "Holm (1979)"},
            {value: 'hochberg', text:"Hochberg (1988)"},
            {value: 'hommel', text:"Hommel (1988)"},
            {value: 'BY', text:"Benjamini-Yekutieli (2001)"},
            {value: 'none', text: 'None'}
         ],
      success: function(response, newValue) {
      nonpara_pairedttestFDRmethod.value = newValue;
  }
  });
 /* console.log(onewayANOVAmethod)
    console.log(onewayANOVAposthocmethod)
    console.log(nonpara_onewayANOVAmethod)
    console.log(nonpara_onewayANOVAposthocmethod)*/
}
