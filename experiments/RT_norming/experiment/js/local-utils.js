// general function for replacing all instances of TERM with prompt
var replaceTerms = function(stim, label){
  var prompt = stim[label];
  return prompt.replace(/FIRST/g, stim.first)
  // return prompt.replace(/NAME/g,
  //    stim.name)
};

//.replace(/SCALE/g,
    //stim.scale)

// make first letter uppercase (i.e., "capitalize")
var jsUcfirst = function(string) {
    return string.charAt(0).toUpperCase() + string.slice(1);
};
