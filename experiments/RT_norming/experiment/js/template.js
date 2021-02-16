//setting up some things
var condition = _.sample(["list1", "list2", "list1_r", "list2_r"])
var trial_counter = 0;

function make_slides(f) {
  var   slides = {};

  slides.bot = slide({
    name : "bot",
    start: function() {
      $('.err1').hide();
      $('.err2').hide();
      $('.disq').hide();
      exp.speaker = _.shuffle(["James", "John", "Robert", "Michael", "William", "David", "Richard", "Joseph", "Thomas", "Charles"])[0];
      exp.listener = _.shuffle(["Mary", "Patricia", "Jennifer", "Linda", "Elizabeth", "Barbara", "Susan", "Jessica", "Sarah", "Margaret"])[0];
      exp.lives = 0;
      var story = exp.speaker + ' says to ' + exp.listener + ': "It\'s a beautiful day, isn\'t it?"'
      var question = 'Who does ' + exp.speaker + ' talk to?';
      document.getElementById("s").innerHTML = story;
      document.getElementById("q").innerHTML = question;
    },
    button : function() {
      exp.text_input = document.getElementById("text_box").value;
      var lower = exp.listener.toLowerCase();
      var upper = exp.listener.toUpperCase();

      if ((exp.lives < 3) && ((exp.text_input.toLowerCase() == lower))){
        exp.data_trials.push({
          "slide_number": exp.phase,
          "slide_type" : "bot_check",
          "image" : exp.listener,
          "audio" : "",
          "response" : [0,exp.text_input]
        });
        exp.go();
      }
      else {
        exp.data_trials.push({
          "slide_number": exp.phase,
          "slide_type" : "bot_check",
          "image" : exp.listener,
          "audio" : "",
          "response" : [0,exp.text_input]
        });
        if (exp.lives == 0){
          $('.err1').show();
        }if (exp.lives == 1){
          $('.err1').hide();
          $('.err2').show();
        }if (exp.lives == 2){
          $('.err2').hide();
          $('.disq').show();
          $('.button').hide();
        }
        exp.lives++;
      } 
    },
  });

  slides.i0 = slide({
     name : "i0",
     start: function() {
      exp.startT = Date.now();
     }
  });

  slides.instructions = slide({
    name : "instructions",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });


  slides.trial = slide({
    name: "trial",
    present: exp.train_stims,
    present_handle: function(stim) {
      this.stim = stim;
      this.position = 0;
            
      $("#comprehension-question").hide();
      
      
      var html = "";
      
      for (var i = 0; i < stim.words.length; i++) {
        var word = stim.words[i];

        if(word.form.includes("FIRST")) {
          word.form = word.form.replace("FIRST", firsts[trial_counter]);
        }

        var masked_word = word.form.replace(/./g, "-") + " ";
        html += "<span data-form=\"" + word.form + " \" data-masked-form=\"" + masked_word + "\"  id=\"stimulus-word-" + i + "\">" +  masked_word + "</span>"
        if (word.lbr_after) {
          html += "<br>"
        }
      }
      
      
      this.response_times = [];
      
      $("#stimulus-sentence").html(html);
      
      
      var t = this;
      
      $("#comprehension-question").hide();
            
      $(document).bind("keydown", function(evt) {
        if (evt.keyCode == 32) {          
          evt.preventDefault();
          t.response_times.push(Date.now());
          if (t.position > 0) {
            var prev_idx = t.position - 1;
            $("#stimulus-word-" + prev_idx).text($("#stimulus-word-" + prev_idx).data("masked-form"));
          }
          if (t.position < t.stim.words.length) {
            $("#stimulus-word-" + t.position ).text($("#stimulus-word-" + t.position ).data("form")); 
          } else {
            $("#comprehension-question").show();
            $(document).unbind("keydown");
          }
          t.position++;
        }
        
      });
      
      $("#comprehension-question-q").text(stim.question);
      
      
      
     
     
     

    },
    button : function(response) {
      this.response_correct = response == this.stim.correct_answer;
      this.log_responses();
      _stream.apply(this);
    },

    log_responses : function() {
      for (var i = 0; i < this.stim.words.length; i++) {
        var word = this.stim.words[i];
        exp.data_trials.push({
          "trial_id": this.stim.trial_id,
          "word_idx": i,
          "first":firsts[trial_counter],
          "form": word.form,
          "region": word.region,
          "lbr_before": word.lbr_before ? 1 : 0,
          "lbr_after": word.lbr_after ? 1 : 0,
          "rt": this.response_times[i+1] - this.response_times[i], 
          "type": this.stim.type,
          "response_correct": this.response_correct ? 1 : 0,
          "trial_no": trial_counter
        }); 
      }
      trial_counter++;
    }
  });

  


  slides.subj_info =  slide({
    name : "subj_info",
    submit : function(e){
      //if (e.preventDefault) e.preventDefault(); // I don't know what this means.
      exp.subj_data = {
        language : $("#language").val(),
        name : $("#name").val(),
        gender : $('#gender').val(),
        tirednesslvl : $('#tirednesslvl').val(),
        age : $("#age").val()
      };
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.thanks = slide({
    name : "thanks",
    start : function() {
      exp.data= {
          "trials" : exp.data_trials,
          "catch_trials" : exp.catch_trials,
          "system" : exp.system,
          "condition" : exp.condition,
          "subject_information" : exp.subj_data,
          "time_in_minutes" : (Date.now() - exp.startT)/60000
      };
      setTimeout(function() {turk.submit(exp.data);}, 1000);
    }
  });

  return slides;
}

/// init ///
function init() {
  exp.condition = condition;
  exp.trials = [];
  exp.catch_trials = [];

  function build_trials() {
    if (condition == "list1") {
      return _.shuffle(list1);
    }
    
    if (condition == "list2") {
      return _.shuffle(list2);
    }
    
    if (condition == "list1_r") {
      return (_.shuffle(list1)).reverse();
    }
    
    if (condition == "list2_r") {
      return (_.shuffle(list2)).reverse();
    }
  }

  exp.train_stims = build_trials(); //can randomize between subject conditions here

  for (var i=0; i<exp.nTrials; i++) { //male names
    var f;
    f = {
      first: firsts[i],
      //scale: _.shuffle(scales)[i]
    }
    exp.train_stims.push(
      _.extend(train_stims[i], f)
      )
  };

  
  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width
    };
  //blocks of the experiment:
  exp.structure=[
  "bot",
  "i0",
  "instructions",
  "trial",
  'subj_info',
  'thanks'];

  exp.data_trials = [];
  //make corresponding slides:
  exp.slides = make_slides(exp);

  exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                    //relies on structure and slides being defined

  $('.slide').hide(); //hide everything

  //make sure turkers have accepted HIT (or you're not in mturk)
  $("#start_button").click(function() {
    // if (turk.previewMode) {
    //   $("#mustaccept").show();
    // } else {
    //   $("#start_button").click(function() {$("#mustaccept").show();});
      exp.go();
    //}
  });

  $(".response-buttons, .test-response-buttons").click(function() {
    _s.button($(this).val());
  });

  exp.go(); //show first slide
}
