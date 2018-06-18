Experigen.initialize = function () {
	
	var conditionList = function (tests, condition) {
	    var conditions = [];
	    
	    for(var i = 0; i < tests.length; i++) {
	        if(!conditions.contains(tests[i][condition])) {
	            conditions.push(tests[i][condition]);
	        }
	    }
	    return conditions;
	}
    
    var tests = [];
	
	var items = this.resource("items");
	
	// Separate tests from fillers
	var all_tests = items.exclude("condition","filler");
	var fillers = items.subset("condition","filler");
	
	var latin_square_row = conditionList(all_tests,"condition");
	var num_conditions = latin_square_row.length;
	
	
	latin_square_row = latin_square_row.shuffle();
	
	var test_length = all_tests.length/num_conditions;
	
	// Apply Latin Square selection to each question
	for(var i = 1; i <= test_length; i += 1) {
	    tests = tests.concat(all_tests.subset("item",i.toString()).subset("condition",latin_square_row[(i % num_conditions)]));
	}
	
	tests = tests.concat(fillers);
	
	tests = tests.pairWith("view","stimulus.ejs").shuffle();

	this.addStaticScreen("intro.ejs");
	this.addStaticScreen("demographic.ejs");
	this.addStaticScreen("practice1.ejs");
	this.addStaticScreen("practice2.ejs");
	this.addStaticScreen("practice3.ejs");
	this.addStaticScreen("starttrial.ejs");
	this.addBlock(tests);
	this.addStaticScreen("finalthanks.ejs");

}