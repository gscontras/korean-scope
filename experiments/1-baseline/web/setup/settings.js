var Experigen =  {
	settings: {

        // SETUP: Enter Experiment Name
		experimentName: "HeritageEnglishExtended", // use only A-Z, a-z, 0-9
		
		databaseServer: "http://db.phonologist.org/",

		online: true,
		
		strings: {
		    // SETUP: Enter Title for Browser Window
			windowTitle:     "Chinese Scope Project - English",
			connecting:		 "Connecting...",
			loading:         "Loading...",
			soundButton:     "    ►    ",
			continueButton:  "   continue   ",
			errorMessage:    "An error occurred. We apologize for the inconvenience.",
			emptyBoxMessage: "Please supply your answer in the text box."
		},
		
		audio: true,
		
		recordResponseTimes: true,
		
		progressbar: {
			visible: true, 
			adjustWidth: 6,
			percentage: false
		},
		
		items: "resources/index.txt",
		
		otherresources: {	
		},

		folders: {
			views: "views/",
			sounds: "resources/sounds/",
			pictures: "resources/pictures/"
		},
	
		footer: "views/footer.html",
		missingview: "views/missingview.ejs"
	}	
};


