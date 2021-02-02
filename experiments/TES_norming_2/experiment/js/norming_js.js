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
		start: function(){
			$('.exclusion_reminder').hide();

			document.onkeydown = checkKey;
			function checkKey(e) {
				e = e || window.event;
				if (($('.tut_instructions').is(":visible")) && (e.keyCode == 32)) {
					e = 0;
					$('.tut_instructions').hide();
					$('.exclusion_reminder').show();
				}
				if (($('.exclusion_reminder').is(":visible")) && (e.keyCode == 32)) {
					exp.go();
				}
			}
		},
	});

	slides.names = slide({
		name : "names",
		start: function(stim){
			var listOfNames = Array.from(new Set(firsts));

			var lastName = listOfNames[listOfNames.length - 1];
			var groupNames = (listOfNames.slice(0, listOfNames.length - 1)).join(", ") + ", and " + lastName;

			
			document.getElementById('name_list').innerHTML = "This story is about " + groupNames +
			", who are all close friends.";

			document.onkeydown = checkKey;
					function checkKey(e) {
						e = e || window.event;
						if (e.keyCode == 32) {
							exp.go();
						}
					}
		},
	}),
	
	slides.main_task = slide({
		name : "main_task",

		present: exp.stims,

		present_handle : function(stim){
			var prompt, utt;

			// storing this info in the slide so I can record it later?
			this.stim = stim; 

			// get index number of trial
			this.trialNum = exp.stimscopy.indexOf(stim);

			// record trial start time
			this.startTime = Date.now();

			// replace NAME from stimuli
			var reminder = stim.first;
			var story = replaceTerms(this.stim, "storyline1");
			var storyEnd = replaceTerms(this.stim, "storyline2");
			var wholeStory = story + storyEnd;

			//display story-dependent fields
			//if male
			document.getElementById('reminder_main').innerHTML = "This snippet is from a story about " + 
			reminder + ", an American man.";
			//if female
			// document.getElementById('reminder_main').innerHTML = "This snippet is from a story about " + 
			//reminder + ",  an American woman.";
			document.getElementById('output_main').innerHTML = story;
			document.getElementById('output_end').innerHTML = storyEnd;
			
			// reset exclusion boolean
			exp.exclusion = false;
			exp.firstName = stim.first;

			//if exclusion stim
			if(stim.stimType == "exclusion") {
				document.getElementById('heads_up').innerHTML = "Here, just fill in the blank with the character's name.";
				exp.exclusion = true;
			} else {
				document.getElementById('heads_up').innerHTML = "";
				exp.exclusion = false;
			}

			//displaying start of trial
			$(".err").hide();
			$('.mainQ').show();
			document.getElementById("answer_box").value = '';

			document.onkeydown = checkKey;

			exp.test_start = 0;
			exp.test_start = Date.now();

			function checkKey(e) {
				e = e || window.event;
				if ((e.keyCode == 13) && ($('.mainQ').is(":visible"))) {
					exp.word = document.getElementById("answer_box").value;
					exp.responseTime = Date.now()-exp.test_start;
					_s.button();
				}
			}
		},

		button : function() {
			if (document.getElementById("answer_box").value == "" || document.getElementById("answer_box").value == " " 
					|| exp.prevItems.includes(document.getElementById("answer_box").value.toLowerCase())) {
				$(".err").show();
			} else {
				exp.responseTime = Date.now()-exp.test_start;
				exp.word = document.getElementById("answer_box").value;
				if (exp.exclusion == true) {
					if (exp.word.toLowerCase() == exp.firstName.toLowerCase()) {
						exp.word = "pass";
					} else {
						exp.word = "fail";
					}
				}
				this.finishTime = Date.now();
				exp.prevItems.push(exp.word.toLowerCase());
				this.log_responses();
				_stream.apply(this);
		}
	},


	

	log_responses : function() {
		exp.data_trials.push({
			"trial_num" : this.trialNum,
			"response" : exp.word,
			"seconds_elapsed" : exp.responseTime / 1000,
			"first" : this.stim.first,
			"story" : this.stim.story,
			"tag": this.stim.tag,
			"list" : exp.currentList,
			"type" : this.stim.stimType
		});
	}
});

	slides.subj_info =  slide({
		name : "subj_info",
		submit : function(e){
			var raceData = new Array();
			var raceQs = document.getElementById("checkboxes");
			var chks = raceQs.getElementsByTagName("INPUT");
			for (var i = 0; i < chks.length; i++) {
				if (chks[i].checked) {
					raceData.push(chks[i].value);
				}
			};

			exp.subj_data = {
				language : $("#language").val(),
				enjoyment : $("#enjoyment").val(),
				asses : $('input[name="assess"]:checked').val(),
				age : $("#age").val(),
				gender : $("#gender").val(),
				education : $("#education").val(),
				affiliation : $("#affiliation").val(),
				race : raceData.join(", "),
				comments : $("#comments").val(),
				problems: $("#problems").val(),
				fairprice: $("#fairprice").val()
			};
			exp.go();
		}
	});

	slides.thanks = slide({
		name : "thanks",
		start : function() {
			exp.data= {
				"trials" : exp.data_trials,
				"catch_trials" : exp.catch_trials,
				"system" : exp.system,
				"condition" : exp.currentList,
				"subject_information" : exp.subj_data,
				"time_in_minutes" : (Date.now() - exp.startT)/60000
			};
			// setTimeout(function() {turk.submit(exp.data);}, 1000);
			proliferate.submit(exp.data);
		}
	});

	return slides;
}

/// init ///
function init() {
	exp.trials = [];
	exp.catch_trials = [];

	exp.nTrials = 20;

	exp.stims = [];

	// create array of respones to check for duplicates
	exp.prevItems = [];

	// determine what list to serve to participant
	exp.currentList = _.shuffle([1,2])[0];

  // filter criticals by list
  var listCriticals = criticalsM.filter(function (stim) { //for male names
  //var listCriticals = criticalsF.filter(function (stim) { //for male names
  	return stim.list == exp.currentList
  });

  //add list-less stims
  var nolistStories = criticalsM.filter(function (stim) {
  	return stim.list == "all"
  });

  var critStories = listCriticals.concat(nolistStories.filter(
  	function() { return true} ));

  // stories are the critical items for the list, plus fillers, plus exclusions

  // vacuous call to filter just converts json to javascript object
  var fillStories = critStories.concat(fillersM.filter( //for male names
  //var stories = listCriticals.concat(fillersF.filter( //for female names
  function() { return true } ));
  
  // add exclusion stims
  //exp.stims = exp.stims.concat(exclusions.filter(function() { return true } ));
  var stories = fillStories.concat(exclusions.filter(
  	function() { return true } ));

  exp.stories = stories;
//	exp.stories = listCriticals;

  for (var i=0; i<exp.nTrials; i++) { //male names
  	var f;
  	f = {
  		first: firsts[i],
      //scale: _.shuffle(scales)[i]
  }
  exp.stims.push(
  	_.extend(stories[i], f)
  	)
};


  exp.stims = _.shuffle(exp.stims);

  exp.stimscopy = exp.stims.slice(0);

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
	"names",
	"main_task",
	"subj_info",
	"thanks"
	];

	exp.data_trials = [];
	//make corresponding slides:
	exp.slides = make_slides(exp);

	exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
										//relies on structure and slides being defined

	$('.slide').hide(); //hide everything

	//make sure turkers have accepted HIT (or you're not in mturk)
	$("#start_button").click(function() {
	// 	if (turk.previewMode) {
	// 		$("#mustaccept").show();
	// 	} else {
	// 		$("#start_button").click(function() {$("#mustaccept").show();});
	exp.go();
	// 	}
});

	exp.go(); //show first slide
}