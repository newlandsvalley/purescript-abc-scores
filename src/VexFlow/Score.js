"use strict";

var wrapper = function() {

  var VF = null;
  var renderer = null;
  var context = null;

  return {

    initialise : function (config) {
      return function () {
        return wrapper.init(config);
      }
    },

    newStaveImpl : function (staveConfig) {
      return function (keySignature) {
        return function () {
          return wrapper.makeStave(staveConfig, keySignature);
        }
      }
    },

    displayStave : function (stave) {
      return function () {
        return wrapper.drawStave(stave);
      }
    },

    timeSignatureImpl : function (stave) {
      return function (timeSignature) {
        return function () {
          return wrapper.drawTimeSignature(stave, timeSignature);
        }
      }
    },

    displayNotesImpl : function (stave) {
      return function (notes) {
        return function () {
          return wrapper.drawNotes(stave, notes);
        }
      }
    },

    displayAutoBeamedNotesImpl : function (timeSignature) {
      return function (stave) {
        return function (notes) {
          return function () {
            return wrapper.drawAutoBeamedNotes(timeSignature, stave, notes);
          }
        }
      }
    },

    init: function (config) {
      // console.log(config);

      VF = Vex.Flow;
      renderer = new VF.Renderer(config.canvasDivId , VF.Renderer.Backends.SVG);
      // renderer = new VF.Renderer(config.canvasDivId, VF.Renderer.Backends.CANVAS);

      // Size our svg:
      renderer.resize(config.canvasWidth, config.canvasHeight);

      context = renderer.getContext();
    },

    makeStave: function (staveConfig, keySignature) {

      // Create a stave at position 10, 40 of width 400 on the canvas.
      var stave = new VF.Stave(staveConfig.x, staveConfig.y, staveConfig.width);

      // Add a clef and key signature if it's the first bar in the stave
      if (staveConfig.barNo == 0) {
        stave.addClef("treble");
        stave.setKeySignature(keySignature);
      }

      return stave;
    },

    drawStave: function (stave) {
      stave.setContext(context).draw();
    },

    drawTimeSignature: function (stave, timeSignature) {
      var meter = timeSignature.numerator + "/" + timeSignature.denominator;
      stave.setTimeSignature(meter);
      //  stave.addTimeSignature(meter);
    },

    drawAutoBeamedNotes: function (timeSignature, stave, notesSpec) {
      console.log(notesSpec);
      var notes = notesSpec.map(wrapper.makeStaveNote);
      // notes.push (new VF.BarNote({ type: 'single' }));
      console.log(notes);

      var beams = VF.Beam.generateBeams(notes, wrapper.beamGroup(timeSignature) );
      Vex.Flow.Formatter.FormatAndDraw(context, stave, notes);
      beams.forEach(function(b) {b.setContext(context).draw()})
    },


    drawNotes: function (stave, notesSpec) {
      console.log(notesSpec);
      // var notes = notesSpec.map(new VF.StaveNote);
      var notes = notesSpec.map(wrapper.makeStaveNote);
      console.log(notes);
      // Create a voice in 6/8 and add above notes
      var voice = new VF.Voice({num_beats: 6,  beat_value: 8});
      voice.addTickables(notes);

      // Format and justify the notes to 400 pixels.
      var formatter = new VF.Formatter().joinVoices([voice]).format([voice], 400);

      // Render voice
      voice.draw(context, stave);
    },

    makeStaveNote: function (noteSpec) {
      return new VF.StaveNote(noteSpec);
    },

    // auto-beaming based on the time signature
    // not complete
    beamGroup: function (timeSignature) {
      var groups = null;
      switch (timeSignature.numerator) {
        case 3:
        case 6:
        case 9:
        case 12:
          groups = {groups: [new Vex.Flow.Fraction(3, timeSignature.denominator)] };
          break;
        default:
          groups = {groups: [new Vex.Flow.Fraction(1, timeSignature.denominator)] };
        }
      return groups;
    }
  }

}();



exports.initialise = wrapper.initialise;
exports.newStaveImpl = wrapper.newStaveImpl;
exports.displayStave = wrapper.displayStave;
exports.displayNotesImpl = wrapper.displayNotesImpl;
exports.displayAutoBeamedNotesImpl = wrapper.displayAutoBeamedNotesImpl;
exports.timeSignatureImpl = wrapper.timeSignatureImpl;
