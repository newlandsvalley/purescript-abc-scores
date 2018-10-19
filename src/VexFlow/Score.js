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

    displayBarBeginRepeat : function (stave) {
      return function () {
        stave.setBegBarType(VF.Barline.type.REPEAT_BEGIN);
      }
    },

    displayBarEndRepeat : function (stave) {
      return function () {
        stave.setEndBarType(VF.Barline.type.REPEAT_END);
      }
    },

    displayBarBothRepeat : function (stave) {
      return function () {
        stave.setBegBarType(VF.Barline.type.REPEAT_BEGIN);
        stave.setEndBarType(VF.Barline.type.REPEAT_END);
      }
    },

    displayStave : function (stave) {
      return function () {
        return wrapper.drawStave(stave);
      }
    },

    getStaveWidth : function (stave) {
      return function () {
        return stave.getWidth();
      }
    },

    timeSignatureImpl : function (stave) {
      return function (timeSignature) {
        return function () {
          return wrapper.drawTimeSignature(stave, timeSignature);
        }
      }
    },

    keySignatureImpl : function (stave) {
      return function (keySignature) {
        return function () {
          return wrapper.drawKeySignature(stave, keySignature);
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

    displayAutoBeamedNotesImpl : function (stave) {
      return function (timeSignature) {
        return function (beatsPerBeam) {
          return function (notes) {
            return function () {
              return wrapper.drawAutoBeamedNotes(stave, timeSignature, beatsPerBeam, notes);
            }
          }
        }
      }
    },

    displayTupletedNotesImpl : function (stave) {
      return function (timeSignature) {
        return function (beatsPerBeam) {
          return function (musicSpec) {
            return function () {
             return wrapper.drawTupletedNotes(stave, timeSignature, beatsPerBeam, musicSpec);
            }
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
        wrapper.drawKeySignature (stave, keySignature);
      }

      return stave;
    },

    drawStave: function (stave) {
      stave.setContext(context).draw();
    },

    drawTimeSignature: function (stave, timeSignature) {
      var meter = timeSignature.numerator + "/" + timeSignature.denominator;
      stave.setTimeSignature(meter);
    },

    drawKeySignature: function (stave, keySignature) {
      stave.addClef("treble");
      stave.setKeySignature(keySignature);
    },

    drawAutoBeamedNotes: function (stave, timeSignature, beatsPerBeam, noteSpec) {
      console.log("drawAutoBeamedNotes")
      console.log(noteSpec);
      console.log("numerator: ", timeSignature.numerator);
      var notes = noteSpec.map(wrapper.makeStaveNote);
      // notes.unshift (new VF.BarNote({ type: 'single' }));  Doesn't work
      console.log(notes);

      var beams = VF.Beam.generateBeams(notes, wrapper.beamGroup(timeSignature, beatsPerBeam) );
      Vex.Flow.Formatter.FormatAndDraw(context, stave, notes);
      beams.forEach(function(b) {b.setContext(context).draw()});
    },

    drawTupletedNotes: function (stave, timeSignature, beatsPerBeam, musicSpec) {
      console.log("drawTupletedNotes")
      console.log(musicSpec);
      var notes = musicSpec.noteSpecs.map(wrapper.makeStaveNote);
      var tuplets = musicSpec.tuplets.map(wrapper.makeTupletLayout (notes));

      var beams = VF.Beam.generateBeams(notes, wrapper.beamGroup(timeSignature, beatsPerBeam) );
      Vex.Flow.Formatter.FormatAndDraw(context, stave, notes);
      beams.forEach(function(b) {b.setContext(context).draw()});
      tuplets.forEach(function(tuplet){
        tuplet.setContext(context).draw();
      });
    },

    // not complete and maybe not useful
    drawNotes: function (stave, noteSpec) {
      console.log(noteSpec);
      // var notes = notesSpec.map(new VF.StaveNote);
      var notes = noteSpec.map(wrapper.makeStaveNote);
      console.log(notes);
      // Create a voice in 6/8 and add above notes
      var voice = new VF.Voice({num_beats: 6,  beat_value: 8});
      voice.addTickables(notes);

      // Format and justify the notes to 400 pixels.
      var formatter = new VF.Formatter().joinVoices([voice]).format([voice], 400);

      // Render voice
      voice.draw(context, stave);
    },

    // make a stave note (n.b. this can represent a single note or a chord)
    makeStaveNote: function (noteSpec) {
      var sn = new VF.StaveNote(noteSpec.vexNote);
      wrapper.addAccidentals (sn, noteSpec.accidentals);
      wrapper.addDots (sn, noteSpec.dots);
      return sn;
    },

    // make a tuplet layout
    makeTupletLayout: function (notes) {
      return function (vexTuplet) {
        return new Vex.Flow.Tuplet(notes.slice(vexTuplet.startPos, vexTuplet.endPos), {
           num_notes: vexTuplet.p, notes_occupied: vexTuplet.q
         });
      };
    },

    // auto-beaming based on the time signature
    beamGroup: function (timeSignature, beatsPerBeam) {
      return {groups: [new Vex.Flow.Fraction(beatsPerBeam, timeSignature.denominator)] };
    },


    // add the accidental(s) to the staveNote(s)
    addAccidentals: function (staveNote, accidentals) {
      accidentals.forEach (function (accidentalString, index) {
        if (accidentalString) {
          staveNote.addAccidental(index, new VF.Accidental(accidentalString));
        }
      });
    },

    // add the accidental(s) to the staveNote(s)
    // no account yet taken of double-dots
    addDots: function (staveNote, dots) {
      dots.forEach (function (dotCount, index) {
        if (dotCount > 0) {
          staveNote.addDot(index);
        }
      });
    }


  }

}();



exports.initialise = wrapper.initialise;
exports.newStaveImpl = wrapper.newStaveImpl;
exports.displayStave = wrapper.displayStave;
exports.getStaveWidth = wrapper.getStaveWidth;
exports.displayBarBeginRepeat = wrapper.displayBarBeginRepeat;
exports.displayBarEndRepeat = wrapper.displayBarEndRepeat;
exports.displayBarBothRepeat = wrapper.displayBarBothRepeat;
exports.displayNotesImpl = wrapper.displayNotesImpl;
exports.displayAutoBeamedNotesImpl = wrapper.displayAutoBeamedNotesImpl;
exports.displayTupletedNotesImpl = wrapper.displayTupletedNotesImpl;
exports.timeSignatureImpl = wrapper.timeSignatureImpl;
exports.keySignatureImpl = wrapper.keySignatureImpl;
