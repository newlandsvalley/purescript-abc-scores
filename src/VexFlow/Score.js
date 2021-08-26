"use strict";

/* const exports = require("webpack"); */

var wrapper = function() {

  var VF = null;
  /*
  var renderer = null;
  var context = null;
  */

  return {

    initialiseCanvas : function (config) {
      return function () {
        return wrapper.init(config);
      }
    },

    resizeCanvas : function (renderer) {
      return function (config) {
        return function () {
          return wrapper.reinitCanvas(renderer, config);
        }
      }
    },

    clearCanvas : function (renderer) {
      return function () {
        var context = renderer.getContext();
      context.clear();
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
      return function (message) {
        return function () {
          stave.setBegBarType(VF.Barline.type.REPEAT_BEGIN);
          if (message) {
            stave.setText(message, VF.Modifier.Position.ABOVE, 
                           { shift_y: 5, justification: VF.TextNote.Justification.LEFT });
            }
        }
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

    displayVolta : function (stave) {
      return function (volta) {
        return function () {
          return wrapper.drawVolta(stave, volta);
        }
      }
    },

    renderText : function (renderer) { 
      return function (title) {
        return function (font) {
          return function (x) {
            return function (y) {
              return function () {
                return wrapper.drawText(renderer, title, font, x, y);
              }
            }
          }
        }
      }
    },    

    renderStave : function (renderer) {
      return function (stave) {
        return function () {
          return wrapper.drawStave(renderer, stave);
        }
      }
    },

    getStaveWidth : function (stave) {
      return function () {
        return stave.getWidth();
      }
    },

    addTimeSignature : function (stave) {
      return function (timeSignature) {
        return function () {
          return wrapper.drawTimeSignature(stave, timeSignature);
        }
      }
    },

    addKeySignature : function (stave) {
      return function (keySignature) {
        return function () {
          return wrapper.drawKeySignature(stave, keySignature, false);
        }
      }
    },

    addTempoMarkingImpl : function (stave) {
      return function (tempo) {
        return function () {
          return wrapper.drawTempoMarking(stave, tempo);
        }
      }
    },

    renderBarContents : function (renderer) {
      return function (stave) {
        return function (beamSpecs) {
          return function (vexCurves) {
            return function (musicSpec) {
              return function () {
                return wrapper.drawBarContents(renderer, stave, beamSpecs, vexCurves, musicSpec);
              }
            }
          }
        }
      }
    },

    init: function (config) {
      // console.log(config);

      VF = Vex.Flow;
      var renderer

      if (config.isSVG) {
        renderer = new VF.Renderer(config.parentElementId , VF.Renderer.Backends.SVG);
      } else {
        renderer = new VF.Renderer(config.parentElementId , VF.Renderer.Backends.CANVAS);
      }

      // Size our svg:
      renderer.resize(config.width, config.height);

      var context = renderer.getContext();
      context.scale(config.scale, config.scale);

      return renderer;
    },

    reinitCanvas: function (renderer, config) {
        // Size our svg:
        renderer.resize(config.width, config.height);

        var context = renderer.getContext();
        context.scale(config.scale, config.scale);
        return renderer;
      },

    makeStave: function (staveConfig, keySignature) {

      var staveOptions = new Object();
      staveOptions.right_bar = staveConfig.hasRightBar;
      staveOptions.fill_style = staveConfig.lineColour;

      // Create a stave at the required position on the canvas.
      var stave = new VF.Stave(staveConfig.x, staveConfig.y, staveConfig.width, staveOptions);

      // create a doubled right bar line if required
      if (staveConfig.hasDoubleRightBar) {
         stave.setEndBarType(VF.Barline.type.DOUBLE);
      }

      // Add a clef and key signature if it's the first bar in the stave
      if (staveConfig.barNo == 0) {
        wrapper.drawKeySignature (stave, keySignature, true);
      }

      return stave;
    },

    drawStave: function (renderer, stave) {
      var context = renderer.getContext();
      stave.setContext(context).draw();
    },

    drawTimeSignature: function (stave, timeSignature) {
      var meter = timeSignature.numerator + "/" + timeSignature.denominator;
      stave.setTimeSignature(meter);
    },

    drawVolta: function (stave, volta) {
      // console.log("volta:")
      // console.log(volta);
      var voltaType;
      switch(volta.voltaType) {
        case 2:
          voltaType = VF.Volta.type.BEGIN;
          break;
        case 3:
          voltaType = VF.Volta.type.MID;
          break;
        case 4:
          voltaType = VF.Volta.type.END;
          break;
        case 5:
          voltaType = VF.Volta.type.BEGIN_END;
          break;
        default:
          voltaType = VF.Volta.type.NONE;
      }
      stave.setVoltaType(voltaType, volta.iteration, 30);
    },

    drawText: function (renderer, title, font, x, y) {
      console.log("title:");
      console.log(title); 
      var context = renderer.getContext();
      context.setRawFont(font);
      context.fillText (title, x, y);
    },

    drawKeySignature: function (stave, keySignature, withClef) {
      if (withClef) {
        stave.addClef("treble");
      }
      stave.setKeySignature(keySignature);
    },

    drawTempoMarking: function (stave, tempo) {
      stave.setTempo(tempo, 0);
    },

    /* draw the contents of the bar, using auto-beaming for the notes */
    drawBarContents: function (renderer, stave, beamSpecs, vexCurves, musicSpec) {
      // console.log("drawBarContents")
      // console.log(musicSpec);
      var context = renderer.getContext();
      var notes = musicSpec.noteSpecs.map(wrapper.makeStaveNote);
      var tuplets = musicSpec.tuplets.map(wrapper.makeTupletLayout (notes));
      var ties = musicSpec.ties.map(wrapper.makeTie (notes));
      // console.log("beamSpecs");
      // console.log(beamSpecs);
      var beams = beamSpecs.map(wrapper.makeBeam (notes));
      var curves = vexCurves.map(wrapper.makeCurve (notes));

      /* add repeititions to the stave (coda, segno etc.) */
      wrapper.addRepetitions (stave, musicSpec.repetitions);

      Vex.Flow.Formatter.FormatAndDraw(context, stave, notes);
      ties.forEach(function(t) {t.setContext(context).draw()})
      beams.forEach(function(b) {b.setContext(context).draw()});
      tuplets.forEach(function(tuplet){
        tuplet.setContext(context).draw();
      });
      curves.forEach(function(c) {c.setContext(context).draw()});
    },


    // make a stave note (n.b. this can represent a single note or a chord)
    makeStaveNote: function (noteSpec) {
      var sn = new VF.StaveNote(noteSpec.vexNote);
      wrapper.addAccidentals (sn, noteSpec.accidentals);
      wrapper.addDots (sn, noteSpec.dots);
      wrapper.addOrnaments (sn, noteSpec.ornaments);
      wrapper.addArticulations (sn, noteSpec.articulations);

      if (noteSpec.graceKeys.length > 0) {
        var graceNotes = noteSpec.graceKeys.map(wrapper.makeGraceNote);
        wrapper.addGraceAccidentals (graceNotes, noteSpec.graceAccidentals);
        var graceNoteGroup =  new VF.GraceNoteGroup(graceNotes, true);
        sn.addModifier(0, graceNoteGroup.beamNotes());
      }
      return sn;
    },

    makeGraceNote: function (graceKey) {
      var note = { keys: [graceKey], duration: '8' };
      return new Vex.Flow.GraceNote (note);
    },

    // make a tuplet layout
    makeTupletLayout: function (notes) {
      return function (vexTuplet) {
        return new Vex.Flow.Tuplet(notes.slice(vexTuplet.startPos, vexTuplet.endPos), {
           num_notes: vexTuplet.p, notes_occupied: vexTuplet.q
         });
      };
    },

    // make a beam between the specified notes
    makeBeam: function (notes) {
      return function (beamSpec) {
        return new Vex.Flow.Beam(notes.slice(beamSpec[0], beamSpec[1]), true);
      };
    },


    // tie a note to its successor
    makeTie: function (notes) {
      return function (noteIndex) {
        return new VF.StaveTie({
          first_note: notes[noteIndex],
          last_note: notes[noteIndex + 1],
          first_indices: [0],
          last_indices: [0]
        });
      };
    },

    // make a slur represented by a curve
    makeCurve: function (notes) {
      return function (vexCurve) {
        // the slope of the curve is just a simple heuristic
        var controlPoints = [{ x: 0, y: 5 }, { x: 0, y: 5 }]
        if (vexCurve.to - vexCurve.from > 1 ) {
          controlPoints = [{ x: 0, y: 10 }, { x: 0, y: 10 }]
        }
        return new VF.Curve(
          notes[vexCurve.from],
          notes[vexCurve.to],
          { thickness: 2,
            cps: controlPoints
        });
      };
    },

    // add the accidental(s) to the staveNote
    addAccidentals: function (staveNote, accidentals) {
      accidentals.forEach (function (accidentalString, index) {
        if (accidentalString) {
          staveNote.addAccidental(index, new VF.Accidental(accidentalString));
        }
      });
    },

    // add any accidentals to the grace notes
    addGraceAccidentals: function (graceNotes, accidentals) {
      accidentals.forEach (function (accidentalString, index) {
        if (accidentalString) {
          /* console.log("grace accidental ", accidentalString, " at ", index); */
          graceNotes[index].addAccidental(0, new VF.Accidental(accidentalString));
        }
      });
    },

    // add the dottedness to the staveNote
    addDots: function (staveNote, dots) {
      dots.forEach (function (dotCount, index) {
        if (dotCount == 2) {
          staveNote.addDot(index).addDot(index);
        }
        else if (dotCount == 1) {
          staveNote.addDot(index);
        }
      });
    },

    // add the ornamant(s) to the staveNote
    addOrnaments: function (staveNote, ornaments) {
      ornaments.forEach (function (ornament, index) {
        staveNote.addModifier(0, new VF.Ornament(ornament));
      });
    },

    // add the articulation(s) to the staveNote
    addArticulations: function (staveNote, articulations) {
      articulations.forEach (function (articulation, index) {
        staveNote.addArticulation(0, new VF.Articulation(articulation));
      });
    },

    // add the repetitions to the stave
    addRepetitions: function (stave, repetitions) {
      repetitions.forEach (function (repetition, index) {
        if (repetition.isLeft) {
          // console.log ("repetition left:", repetition.repetitionType);
          stave.setRepetitionTypeLeft(repetition.repetitionType, 25);
        }
        else {
          // console.log ("repetition right:", repetition.repetitionType);
          stave.setRepetitionTypeRight(repetition.repetitionType, 25);
        }
      });
    }

  }

}();

exports.initialiseCanvas = wrapper.initialiseCanvas;
exports.resizeCanvas = wrapper.resizeCanvas;
exports.clearCanvas = wrapper.clearCanvas;
exports.newStaveImpl = wrapper.newStaveImpl;
exports.renderStave = wrapper.renderStave;
exports.renderText = wrapper.renderText;
exports.getStaveWidth = wrapper.getStaveWidth;
exports.displayBarBeginRepeat = wrapper.displayBarBeginRepeat;
exports.displayBarEndRepeat = wrapper.displayBarEndRepeat;
exports.displayBarBothRepeat = wrapper.displayBarBothRepeat;
exports.renderBarContents = wrapper.renderBarContents;
exports.displayVolta = wrapper.displayVolta;
exports.addTimeSignature = wrapper.addTimeSignature;
exports.addKeySignature = wrapper.addKeySignature;
exports.addTempoMarkingImpl = wrapper.addTempoMarkingImpl;
