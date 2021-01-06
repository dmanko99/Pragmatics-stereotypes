
var wnames = ["Peter",
"Brad",
"Ethan",
"Ian",
"Cody",
"Brett",
"Paul",
"Connor",
"Jack",
"Logan",
"Roger",
"Dylan",
"Hunter",
"Dustin",
"Ryan"]

var bnames = ["Trevon",
"Tyree",
"Deion",
"Marquis",
"Jermaine",
"Lamont",
"Tyrone",
"Deandre",
"Tremayne",
"Lamar",
"Kareem",
"Hakeem",
"Jamal",
"Rasheed",
"Deshawn"]

var wFnames = ["Emily",
"Hannah",
"Lauren",
"Megan",
"Rachel",
"Rebecca",
"Samantha",
"Sarah",
"Victoria",
"Anne",
"Jill",
"Allison",
"Laurie",
"Meredith",
"Carrie"]

var bFnames = ["Aisha",
"Keisha",
"Tamika",
"Lakisha",
"Tanisha",
"Latoya",
"Kenya",
"Latonya",
"Ebony",
"Tiara",
"Monique",
"Jasmine",
"Desiree",
"Shante",
"Tanisha"]

var firsts = _.shuffle(_.sample(wnames, 10).concat(_.sample(bnames, 10)))
var firstsF = _.shuffle(_.sample(wFnames, 10).concat(_.sample(bFnames, 10)))

var criticalsM = [
	{
		"story":"gray car",
		"storyline1":"FIRST caught a glimpse of the clock and realized he was late! He hopped into his car, a gray ",
		"storyline2": " , to go meet his friends.",
		"tag":"poor",
		"stimType":"critical",
		"list":1
	},{
		"story":"blue car",
		"storyline1":"FIRST had to hold back a scream when he walked through the door and saw all of his stuff scattered across the floor. But as FIRST checked around, he realized that the only thing missing was the keys to his car, which was a blue ",
		"storyline2":" .",
		"tag":"poor",
		"stimType":"critical",
		"list":2
	},{
		"story":"practice",
		"storyline1":"FIRST got up with a start and realized that he should have left a while ago. He quickly got moving to go pick up his little cousin from ",
		"storyline2":" practice.",
		"tag":"sports",
		"stimType":"critical",
		"list":1
	},{
		"story":"lesson",
		"storyline1":"FIRST checked his phone and realized that he was going to be late. He rushed out the door to go pick up his little brother from his ",
		"storyline2":" lesson.",
		"tag":"jazz",
		"stimType":"critical",
		"list":2
	},{
		"story":"park",
		"storyline1":"It was weird to see the park empty, FIRST thought as he drove home. He remembered going to that same park as a kid after school and playing ",
		"storyline2":" with his friends.",
		"tag":"sports",
		"stimType":"critical",
		"list":2
	},{
		"story":"performing",
		"storyline1":"Even with a lot of practice, FIRST always got really nervous before performing. He knew that no matter how many times he got on stage, he would always be nervous to play the ",
		"storyline2":" in front of an audience.",
		"tag":"jazz",
		"stimType":"critical",
		"list":1
	},{
		"story":"genre",
		"storyline1":"FIRST threw his jacket to the side and sunk down into his favorite chair, happy to be home after a long day. To really relax though, he pulled his phone out and started playing some ",
		"storyline2":" , his favorite kind of music.",
		"tag":"jazz",
		"stimType":"critical",
		"list":1
	},{
		"story":"radio",
		"storyline1":"FIRST got in the car and hit the road, starting what he knew would be a long drive. Before he could go too far, he made sure to put some ",
		"storyline2":" on the car&rsquo;s radio, his favorite kind of music.",
		"tag":"genre",
		"stimType":"critical",
		"list":2
	},{
		"story":"high school team",
		"storyline1":"FIRST wasn&rsquo;t really excited for the team-building activities he had to do with his coworkers, but knew they weren&rsquo;t optional. &quot;Hopefully my experience on the ",
		"storyline2":" team in high school will help,&quot; FIRST thought.",
		"tag":"sports",
		"stimType":"critical",
		"list":1
	},{
		"story":"game with friends",
		"storyline1":"Finally done with work for the week, FIRST was excited to meet up with his friends later. He couldn&rsquo;t wait to beat them at a game of ",
		"storyline2":" .",
		"tag":"sports",
		"stimType":"critical",
		"list":2
	},{
		"story":"food",
		"storyline1":"&quot;Was that my belly just growling?&quot; thought FIRST, realizing he was actually really hungry. &quot;I could sure go for some ",
		"storyline2":" right now,&quot; he thought.",
		"tag":"fast food",
		"stimType":"critical",
		"list":1
	},{
		"story":"fruit",
		"storyline1":"FIRST was excited to go to his family&rsquo;s weekly Sunday lunch. He knew there would be fresh ",
		"storyline2":" there, which was his favorite fruit to eat.",
		"tag":"watermelon",
		"stimType":"critical",
		"list":2
	},{
		"story":"restaurant",
		"storyline1":"FIRST was looking forward to dinner. He didn&rsquo;t often eat out, but a meal tonight at ",
		"storyline2":" sounded like a nice break from him cooking something at home.",
		"tag":"fast food",
		"stimType":"critical",
		"list":1
	},{
		"story":"fancy restaurant",
		"storyline1":"&quot;Date night!&quot; thought FIRST. He had made a reservation at ",
		"storyline2":" , and was looking forward to taking his partner there.",
		"tag":"poor",
		"stimType":"critical",
		"list":2
	},{
		"story":"lazy day",
		"storyline1":"FIRST woke up with a smile, happy that it was finally the weekend. He was looking forward to having the time to ",
		"storyline2":" a lot today, which he hadn&rsquo;t had the time to in a while.",
		"tag":"lazy",
		"stimType":"critical",
		"list":1
	},{
		"story":"test",
		"storyline1":"FIRST was nervous for his interview, feeling unprepared for the part where he would be quizzed on what he knew. He hoped it wouldn&rsquo;t turn out like his worst math test from high school that he got a ",
		"storyline2":" percent on.",
		"tag":"academic",
		"stimType":"critical",
		"list":2
	}]

var criticalsF = [
	{
		"story":"gray car",
		"storyline":"FIRST caught a glimpse of the clock and realized she was late! She hopped into her car, a gray ____, to go meet her friends.",
		"tag":"poor",
		"stimType":"critical",
		"list":1
	},{
		"story":"blue car",
		"storyline":"FIRST had to hold back a scream when she walked through the door and saw all of her stuff scattered across the floor. But as FIRST checked around, she realized that the only thing missing was the keys to her car, a blue ____.",
		"tag":"poor",
		"stimType":"critical",
		"list":2
	},{
		"story":"practice",
		"storyline":"FIRST got up with a start and realized that she should have left a while ago. She quickly got moving to go pick up her little cousin from _____ practice.",
		"tag":"sports",
		"stimType":"critical",
		"list":3
	},{
		"story":"lesson",
		"storyline":"FIRST checked her phone and realized that she was going to be late. She rushed out the door to go pick up her little brother from his _____ lesson.",
		"tag":"music",
		"stimType":"critical",
		"list":3
	},{
		"story":"park",
		"storyline":"It was weird to see the park empty, FIRST thought as she drove home. She remembered going to that same park as a kid after school and playing _____ with her friends.",
		"tag":"sports",
		"stimType":"critical",
		"list":1
	},{
		"story":"performing",
		"storyline":"Even with a lot of practice, FIRST always got really nervous before performing. She knew that no matter how many times she got on stage, she would always be nervous to play the ______ in front of an audience.",
		"tag":"jazz",
		"stimType":"critical",
		"list":2
	},{
		"story":"genre",
		"storyline":"FIRST threw her jacket to the side and sunk down into her favorite chair, happy to be home after a long day. To really relax though, she pulled her phone out and started playing some ____, her favorite kind of music.",
		"tag":"jazz",
		"stimType":"critical",
		"list":1
	},{
		"story":"radio",
		"storyline":"FIRST got in the car and hit the road, starting what she knew would be a long drive. Before she could go too far, she made sure to put some ____ on the car’s radio.",
		"tag":"evoke",
		"stimType":"critical",
		"list":2
	},{
		"story":"high school team",
		"storyline":"FIRST wasn’t really excited for the team-building activities she had to do with her coworkers, but knew they weren’t optional. “Hopefully my experience on the ____ team in high school will help,” FIRST thought.",
		"tag":"sports",
		"stimType":"critical",
		"list":3
	},{
		"story":"game with friends",
		"storyline":"Finally done with work for the week, FIRST was excited to meet up with her friends later. She couldn’t wait to beat them at a game of ____.",
		"tag":"sports",
		"stimType":"critical",
		"list":3
	}]

// var exclusions = [
// 	{
// 		"story":"math1",
// 		"storyline":"5 + 4 = 9",
// 		"tag":"exclusion_right",
// 		"scaleType":9,
// 		"stimType":"exclusion",
// 		"list":"all"
// 	},{
// 		"story":"math2",
// 		"storyline":"10 - 5 = 5",
// 		"tag":"exclusion_right",
// 		"scaleType":9,
// 		"stimType":"exclusion",
// 		"list":"all"
// 	},{
// 		"story":"math3",
// 		"storyline":"10 + 2 = 13",
// 		"tag":"exclusion_wrong",
// 		"scaleType":9,
// 		"stimType":"exclusion",
// 		"list":"all"
// 	},{
// 		"story":"math4",
// 		"storyline":"5 - 3 = 1",
// 		"tag":"exclusion_wrong",
// 		"scaleType":9,
// 		"stimType":"exclusion",
// 		"list":"all"}]

var fillersM = [
	{
		"story":"B&D",
		"storyline1":"FIRST hid behind the door and when the man entered the kitchen he stabbed him in the back. He wiped the blood off the ",
		"storyline2":" and rummaged through the drawers.",
		"tag":"weapon",
		"stimType":"filler",
		"list":"all"
	},{
		"story":"sleep",
		"storyline1":"FIRST woke up with a start, shocked that his alarm was going off already. He didn&rsquo;t sleep well again, probably because of the ",
		"storyline2":" that always came from his neighbors.",
		"tag":"neighborhood",
		"stimType":"filler",
		"list":"all"
	},{
		"story":"sister",
		"storyline1":"Checking his texts, FIRST knew his sister would be mad at him so he called her immediately. &quot; ",
		"storyline2":"! I&rsquo;m sorry for missing your messages!&quot; he said, genuinely feeling bad.",
		"tag":"names",
		"stimType":"filler",
		"list":"all"
	},{
		"story":"alcohol",
		"storyline1":"FIRST was excited to meet up with his friends over the weekend. It had been a while since they all got together and drank ",
		"storyline2":" together, so he was excited to see them.",
		"tag":"food",
		"stimType":"filler",
		"list":"all"
	},{
		"story":"movies",
		"storyline1":"FIRST had bought tickets to go to the movies with his partner, and was excited. He had been wanting to see ",
		"storyline2":" for a while now.",
		"tag":"movie",
		"stimType":"filler",
		"list":"all"
	},{
		"story":"driving",
		"storyline1":"FIRST was getting a little tired on the drive, and felt his eyes getting heavy until a jolt of adrenaline woke him up. He didn&rsquo;t expect to see ",
		"storyline2":" run across the road in front of him.",
		"tag":"driving",
		"stimType":"filler",
		"list":"all"
	},{
		"story":"lost item",
		"storyline1":"After searching all around the house, FIRST was ready to give up. He couldn&rsquo;t believe he had lost his favorite ",
		"storyline2":" .",
		"tag":"item",
		"stimType":"filler",
		"list":"all"
	},{
		"story":"appointment",
		"storyline1":"FIRST groaned as he checked the time. He remembered he had a ",
		"storyline2":" appointment today, which he was dreading.",
		"tag":"appointment",
		"stimType":"filler",
		"list":"all"
	},{
		"story":"friend",
		"storyline1":"FIRST saw the name pop up on his phone and couldn&rsquo;t contain his smile. &quot; ",
		"storyline2":"&quot; he said, unable to believe it had been so long since he last talked to his friend.",
		"tag":"names",
		"stimType":"filler",
		"list":"all"
	},{
		"story":"job",
		"storyline1":"FIRST knew he had to get a lot of sleep that night. It was going to be a long day at the",
		"storyline2":" tomorrow, and he knew that without eight hours of sleep he would get really tired on the job.",
		"tag":"work",
		"stimType":"filler",
		"list":"all"
	}
	]

// var fillersF = [
// 	{
// 		"story":"sports",
// 		"storyline":"FIRST watches sports.",
// 		"tag":"filler_highbias",
// 		"scaleType":9,
// 		"stimType":"filler",
// 		"list":"all"
// 	},{
// 		"story":"dog",
// 		"storyline":"FIRST owns a dog or has owned a dog. ",
// 		"tag":"filler_highbias",
// 		"scaleType":9,
// 		"stimType":"filler",
// 		"list":"all"
// 	},{
// 		"story":"sister",
// 		"storyline":"FIRST has at least one sister.",
// 		"tag":"filler_highbias",
// 		"scaleType":9,
// 		"stimType":"filler",
// 		"list":"all"
// 	},{
// 		"story":"alcohol",
// 		"storyline":"FIRST drinks alcohol with her friends. ",
// 		"tag":"filler_highbias",
// 		"scaleType":9,
// 		"stimType":"filler",
// 		"list":"all"
// 	},{
// 		"story":"movies",
// 		"storyline":"FIRST enjoys watching movies. ",
// 		"tag":"filler_highbias",
// 		"scaleType":9,
// 		"stimType":"filler",
// 		"list":"all"
// 	},{
// 		"story":"plane crash",
// 		"storyline":"FIRST has been in a plane crash and survived.",
// 		"tag":"filler_lowbias",
// 		"scaleType":9,
// 		"stimType":"filler",
// 		"list":"all"
// 	},{
// 		"story":"lottery",
// 		"storyline":"FIRST has won the lottery twice.",
// 		"tag":"filler_lowbias",
// 		"scaleType":9,
// 		"stimType":"filler",
// 		"list":"all"
// 	},{
// 		"story":"dentist",
// 		"storyline":"FIRST looks forward to her dental appointments.",
// 		"tag":"filler_lowbias",
// 		"scaleType":9,
// 		"stimType":"filler",
// 		"list":"all"
// 	},{
// 		"story":"lightning",
// 		"storyline":"FIRST has been struck by lightning.",
// 		"tag":"filler_lowbias",
// 		"scaleType":9,
// 		"stimType":"filler",
// 		"list":"all"
// 	},{
// 		"story":"quadruplets",
// 		"storyline":"FIRST is the mother of quadruplets.",
// 		"tag":"filler_lowbias",
// 		"scaleType":9,
// 		"stimType":"filler",
// 		"list":"all"
// 	}
// ]

// var practice = [
// 	{
// 		"story":"beard",
// 		"storyline":"Brandon has a beard.",
// 		"tag":"practice",
// 		"scaleType":9,
// 		"stimType":"practice",
// 		"list":"all"
// 	}
// ]

// var scales = [
// 	{weak: "or", strong: "and"},
// 	{weak: "some", strong: "all"},
// 	{weak: "looks like", strong: "is"},
// 	{weak: "possible", strong: "certain"},
// 	{weak: "three", strong: "more than three"}
// ]

// scales[9] = "filler"