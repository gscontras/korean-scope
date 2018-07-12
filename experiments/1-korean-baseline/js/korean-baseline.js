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
    present : [
      {item: "1-leaning", aevery_surface: "1-leaning-aevery-surface", aevery_inverse: "1-leaning-aevery-inverse", everya_inverse: "1-leaning-everya-inverse"},
      {item: "2-fishing", aevery_surface: "2-fishing-aevery-surface", aevery_inverse: "2-fishing-aevery-inverse", everya_inverse: "2-fishing-everya-inverse"},
      {item: "3-fish pole", aevery_surface: "3-fishpole-aevery-surface", aevery_inverse: "3-fishpole-aevery-inverse", everya_inverse: "3-fishpole-everya-inverse"},
      {item: "4-feeding", aevery_surface: "4-feeding-aevery-surface", aevery_inverse: "4-feeding-aevery-inverse", everya_inverse: "4-feeding-everya-inverse"},
      {item: "5-bottle", aevery_surface: "5-bottle-aevery-surface", aevery_inverse: "5-bottle-aevery-inverse", everya_inverse: "5-bottle-everya-inverse"},
      {item: "6-biting", aevery_surface: "6-biting-aevery-surface", aevery_inverse: "6-biting-aevery-inverse", everya_inverse: "6-biting-everya-inverse"},
      {item: "7-attacking", aevery_surface: "7-attacking-aevery-surfac", aevery_inverse: "7-attacking-aevery-inverse", everya_inverse: "7-attacking-everya-inverse"},
      {item: "8-petting", aevery_surface: "8-petting-aevery-surface", aevery_inverse: "8-petting-aevery-inverse", everya_inverse: "8-petting-everya-inverse"}
      // {url: "audio/1-leaning-aevery-default.m4a"}
    ],

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
      audio_file = "audio/" + stim.item + "-" + order + "-" + exp.scrambled + ".m4a";
      document.getElementById('audio').src = audio_file;

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
        "image" : image_src
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
        asses : $('input[name="assess"]:checked').val(),
        age : $("#age").val(),
        gender : $("#gender").val(),
        education : $("#education").val(),
        comments : $("#comments").val(),
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
  exp.structure=["i0", "instructions", "single_trial", "one_slider", 'subj_info', 'thanks'];
  
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