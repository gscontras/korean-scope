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

  slides.single_trial = slide({
    name: "single_trial",
    start: function() {
      $(".err").hide();
      $(".display_condition").html("You are in " + exp.condition + ".");
    },
    button : function() {
      response = $("#text_response").val();
      if (response.length == 0) {
        $(".err").show();
      } else {
        exp.data_trials.push({
          "trial_type" : "single_trial",
          "response" : response
        });
        exp.go(); //make sure this is at the *end*, after you log your data
      }
    },
  });

  slides.one_slider = slide({
    name : "one_slider",

    /* trial information for this block
     (the variable 'stim' will change between each of these values,
      and for each of these, present_handle will be run.) */
    present : _.shuffle([
      {type: "test", item: "1-leaning", aevery_surface: "1-leaning-aevery-surface", aevery_inverse: "1-leaning-aevery-inverse", everya_inverse: "1-leaning-everya-inverse"},
      {type: "test", item: "2-fishing", aevery_surface: "2-fishing-aevery-surface", aevery_inverse: "2-fishing-aevery-inverse", everya_inverse: "2-fishing-everya-inverse"},
      {type: "test", item: "3-pole", aevery_surface: "3-pole-aevery-surface", aevery_inverse: "3-pole-aevery-inverse", everya_inverse: "3-pole-everya-inverse"},
      {type: "test", item: "4-feeding", aevery_surface: "4-feeding-aevery-surface", aevery_inverse: "4-feeding-aevery-inverse", everya_inverse: "4-feeding-everya-inverse"},
      {type: "test", item: "5-bottle", aevery_surface: "5-bottle-aevery-surface", aevery_inverse: "5-bottle-aevery-inverse", everya_inverse: "5-bottle-everya-inverse"},
      {type: "test", item: "6-biting", aevery_surface: "6-biting-aevery-surface", aevery_inverse: "6-biting-aevery-inverse", everya_inverse: "6-biting-everya-inverse"},
      {type: "test", item: "7-attacking", aevery_surface: "7-attacking-aevery-surface", aevery_inverse: "7-attacking-aevery-inverse", everya_inverse: "7-attacking-everya-inverse"},
      {type: "test", item: "8-petting", aevery_surface: "8-petting-aevery-surface", aevery_inverse: "8-petting-aevery-inverse", everya_inverse: "8-petting-everya-inverse"},
      {type: "filler", item: "filler1", aevery_surface: "053", aevery_inverse: "053", everya_inverse: "053", audio: "f1"},
      {type: "filler", item: "filler2", aevery_surface: "063", aevery_inverse: "063", everya_inverse: "063", audio: "f2"},
      {type: "filler", item: "filler3", aevery_surface: "063", aevery_inverse: "063", everya_inverse: "063", audio: "f3"},
      {type: "filler", item: "filler4", aevery_surface: "075", aevery_inverse: "075", everya_inverse: "075", audio: "f4"},
      {type: "filler", item: "filler5", aevery_surface: "056", aevery_inverse: "056", everya_inverse: "056", audio: "f5"},
      {type: "filler", item: "filler6", aevery_surface: "056", aevery_inverse: "056", everya_inverse: "056", audio: "f6"},
      {type: "filler", item: "filler7", aevery_surface: "009", aevery_inverse: "009", everya_inverse: "009", audio: "f7"},
      {type: "filler", item: "filler8", aevery_surface: "009", aevery_inverse: "009", everya_inverse: "009", audio: "f8"},
      {type: "filler", item: "filler9", aevery_surface: "016", aevery_inverse: "016", everya_inverse: "016", audio: "f9"},
      {type: "filler", item: "filler10", aevery_surface: "005", aevery_inverse: "005", everya_inverse: "005", audio: "f10"},
      {type: "filler", item: "filler11", aevery_surface: "005", aevery_inverse: "005", everya_inverse: "005", audio: "f11"},
      {type: "filler", item: "filler12", aevery_surface: "054", aevery_inverse: "054", everya_inverse: "054", audio: "f12"},
      {type: "filler", item: "filler13", aevery_surface: "054", aevery_inverse: "054", everya_inverse: "054", audio: "f13"}

    ]),

    //this gets run only at the beginning of the block
    present_handle : function(stim) {
      $(".err").hide();
      $(".hidden").hide();

      this.stim = stim; //I like to store this information in the slide so I can record it later.

      scope = _.sample(["surface","inverse"])
      order = _.sample(["everya","aevery"])

      // set the image
      if (order == "aevery") {
        if (scope == "surface") {
          var image = stim.aevery_surface
        } else {
          var image = stim.aevery_inverse
        }
      } else {
        if (scope == "surface") {
          var image = stim.aevery_inverse
        } else {
          var image = stim.everya_inverse
        }
      }
      image_src = "images/" + image + ".JPG"
      document.getElementById('image').src = image_src

      // set the audio
      if (this.stim.type == "test") {
        audio_file = "audio/" + stim.item + "-" + order + "-" + exp.scrambled + ".m4a";
        document.getElementById('audio').src = audio_file;
      } else {
        audio_file = "audio/" + stim.audio  + "-" + exp.scrambled + ".m4a";
        document.getElementById('audio').src = audio_file;
      }

      // show prompt after audio plays
      document.getElementById("audio").onended = function() {right()};
      function right() {
        $(".hidden").show()
      
      }

      // initialize the sliders
      this.init_sliders();
      exp.sliderPost = null; //erase current slider value
    },

    button : function() {
      if (exp.sliderPost == null) {
        $(".err").show();
      } else {
        this.log_responses();

        /* use _stream.apply(this); if and only if there is
        "present" data. (and only *after* responses are logged) */
        _stream.apply(this);
      }
    },

    init_sliders : function() {
      utils.make_slider("#single_slider", function(event, ui) {
        exp.sliderPost = ui.value;
      });
    },

    log_responses : function() {
      exp.data_trials.push({
        "trial_type" : "one_slider",
        "response" : exp.sliderPost,
        "scope" : scope,
        "order" : order,
        "item" : this.stim.item,
        "scramble" : exp.scrambled,
        "audio" : audio_file,
        "image" : image_src,
        "type" : this.stim.type
      });
    }
  });


  slides.subj_info =  slide({
    name : "subj_info",
    submit : function(e){
      //if (e.preventDefault) e.preventDefault(); // I don't know what this means.
      exp.subj_data = {
        language : $("#language").val(),
        enjoyment : $("#enjoyment").val(),
        assess : $('input[name="assess"]:checked').val(),
        age : $("#age").val(),
        gender : $("#gender").val(),
        education : $("#education").val(),
        comments : $("#comments").val(),
        years : $("#years").val(),
        family : $("#family").val(),
        level : $("#level").val(),
        describe : $("#describe").val(),
        school : $("#school").val(),
        classes : $("#classes").val(),
        college : $("#college").val(),
        lived : $("#lived").val()
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
  repeatWorker = false;
  (function(){
      var ut_id = "korean-scope";
      if (UTWorkerLimitReached(ut_id)) {
        $('.slide').empty();
        repeatWorker = true;
        alert("You have already completed the maximum number of HITs allowed by this requester. Please click 'Return HIT' to avoid any impact on your approval rating.");
      }
  })();

  exp.trials = [];
  exp.catch_trials = [];
  exp.scrambled = _.sample(["default", "scrambled"]); //can randomize between subject conditions here
  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width
    };
  //blocks of the experiment:
  exp.structure=["i0", "instructions", "one_slider", 'subj_info', 'thanks'];
  
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