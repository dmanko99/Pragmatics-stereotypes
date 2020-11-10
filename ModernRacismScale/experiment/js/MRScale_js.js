function make_slides(f) {
	var   slides = {};

	
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

	slides.scale_test = slide({
		name: "scale_test",

		submit : function(e){
			var total_score = (
				(4 - ($("#likert1").val())) +
				$("#likert2").val() +
				(4 - ($("#likert3").val())) +
				(4 - ($("#likert4").val())) +
				(4 - ($("#likert5").val())) +
				(4 - ($("#likert6").val())) +
				(4 - ($("#likert7").val()))
			);

			exp.MRS_responses = {
				q1 : $("#likert1").val(),
				q2 : $("#likert2").val(),
				q3 : $("#likert3").val(),
				q4 : $("#likert4").val(),
				q5 : $("#likert5").val(),
				q6 : $("#likert6").val(),
				q7 : $("#likert7").val(),
				total : total_score
			};
			exp.go(); //use exp.go() if and only if there is no "present" data.
		}
	});

	// slides.main_task = slide({
	// 	name: "main_task",

	// 	present : exp.stims,

	// 	present_handle : function(stim) {
	// 		var prompt, utt;
	// 		//a bunch of stuff goes here!!!

	// 		// "uncheck" and displayradio buttons
	// 		document.getElementsByName('judgment')[0].style.visibility = 'visible';
	// 		document.getElementsByName('judgment')[1].style.visibility = 'visible';
	// 		$('input[name="judgment"]').prop('checked', false);

	// 		// hide error message
	// 		$(".err1").hide();
	// 		$(".err2").hide();

	// 		// show "data"
	// 		$(".data").show();

	// 		// get index number of trial
	// 		this.trialNum = exp.stimscopy.indexOf(stim);

	// 		// record trial start time
	// 		this.startTime = Date.now();

	// 		// storing this info in the slide so I can record it later?
	// 		this.stim = stim; 

	// 		// replace NAME from stimuli
	// 		var reminder = stim.first;
	// 		var story = replaceTerms(this.stim, "storyline")

	// 		//display story-dependent fields
	// 		if(stim.stimType != "exclusion") {
	// 			//if male
	// 			document.getElementById('reminder').innerHTML = reminder + " is an American man.";
	// 			//if female
	// 			// document.getElementById('reminder').innerHTML = reminder + " is an American woman.";
	// 		} else {
	// 			document.getElementById('reminder').innerHTML = "";
	// 		}
	// 		document.getElementById('output').innerHTML = story;

	// 		//reset slide timer
	// 		//h1.textContent = concat("00:" + answerTime + ".00");
	// 		h1.textContent = "00:10.00"
	// 			tenths = 1;
	// 			//seconds = answerTime;
	// 			seconds = 10;
	// 			minutes = 0;

	// 		clearTimeout(t);
	// 		timer();

	// 		//put auto-advancing stuff in here
	// 		clearTimeout(hideQ);
	// 		autohide();

	// 	},

		
	// 	button : function() {
	// 		if (document.getElementsByName('judgment')[0].style.visibility == 'hidden') {
	// 			exp.response = "undefined";
	// 		} else {
	// 			exp.response = $('input[name="judgment"]:checked').val();
	// 		}
	// 		if (exp.response == null) {
	// 			$(".err1").show();
	// 		} else {
	// 			this.finishTime = Date.now();
	// 			this.log_responses();

	// 			/* use _stream.apply(this); if and only if there is
	// 			"present" data. (and only *after* responses are logged) */
	// 			_stream.apply(this);
	// 		}
	// 	},

	// 	log_responses : function() {
	// 		exp.data_trials.push({
	// 			"trial_num": this.trialNum,
	// 			"response" : exp.response,
	// 			"seconds_elapsed" : (this.finishTime - this.startTime) / 1000,
	// 			"first": this.stim.first,
	// 			"story": this.stim.story,
	// 			"scale": scales[this.stim.scaleType],
	// 			"tag": this.stim.tag,
	// 			"list" : exp.currentList,
	// 			"type" : this.stim.stimType
	// 		});
	// 	}
	// });

	slides.subj_info =  slide({
		name : "subj_info",
		submit : function(e){
			//if (e.preventDefault) e.preventDefault(); // I don't know what this means.
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
			exp.go(); //use exp.go() if and only if there is no "present" data.
		}
	});

	slides.thanks = slide({
		name : "thanks",
		start : function() {
			exp.data= {
					//"trials" : exp.data_trials,
					//"catch_trials" : exp.catch_trials,
					"MRS_responses" : exp.MRS_responses,
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
		"i0", 
		"scale_test",
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
		if (turk.previewMode) {
			$("#mustaccept").show();
		} else {
			$("#start_button").click(function() {$("#mustaccept").show();});
			exp.go();
		}
	});

	exp.go(); //show first slide
}
