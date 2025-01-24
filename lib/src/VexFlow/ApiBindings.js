"use strict";

var wrapper = function() {

  // In a web application, we must import the vexflow JS before the transpiled PS.
  // However, in the tests, we don't have this luxury.  VF is initialised in this 
  // roundabout way to allow the tests to run (which do not make any JS calls but
  // require it to be valid)
  var VF = null;

  return {

    initializeCanvas : function (config) {
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

    resizeCanvas: function (renderer, config) {
      // Size our svg:
      renderer.resize(config.width, config.height);

      var context = renderer.getContext();
      context.scale(config.scale, config.scale);
      return renderer;
    },

    clearCanvas: function (renderer) {
      var context = renderer.getContext();
      context.clear();
    },

    makeStave: function (staveConfig, clef, keySignature) {

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
        wrapper.addKeySignature (stave, keySignature, clef);
      }

      return stave;
    }, 
    
    getStaveWidth : function (stave) {
      return stave.getWidth();
    },

    renderStave: function (renderer, stave) {
      var context = renderer.getContext();
      stave.setContext(context).draw();
    },

    addTimeSignature: function (stave, timeSignature) {
      var meter = timeSignature.numerator + "/" + timeSignature.denominator;
      stave.setTimeSignature(meter);
    },

    displayVolta: function (stave, volta) {
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
      stave.setVoltaType(voltaType, volta.iteration, 25);
    },

    renderText: function (renderer, title, font, x, y) {
      // console.log("title:");
      // console.log(title); 
      var context = renderer.getContext();
      context.setFont(font);
      context.fillText (title, x, y);
    },

    addKeySignature: function (stave, keySignature, clef) {
      if (clef) {
        stave.addClef(clef);
      }
      stave.setKeySignature(keySignature);
    },

    addTempoSignature: function (stave, tempo) {
      stave.setTempo(tempo, 0);
    },

    /* draw the contents of the bar, using explicit beaming for the notes */
    renderBarContents: function (renderer, stave, beamSpecs, vexCurves, musicSpec) {
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

      /* add repetitions to the stave (coda, segno etc.) */
      wrapper.addRepetitions (stave, musicSpec.repetitions);

      /* draw the notes.  Try to emulate thw original VexFlow 1.89 formatter */
      wrapper.formatAndDrawNotes (context, stave, notes);

      /* Vex.Flow.Formatter.FormatAndDraw(context, stave, notes); */
      ties.forEach(function(t) {t.setContext(context).draw()})
      beams.forEach(function(b) {b.setContext(context).draw()});
      tuplets.forEach(function(tuplet){
        tuplet.setContext(context).draw();
      });
      curves.forEach(function(c) {c.setContext(context).draw()});
    },

    displayBarBeginRepeat : function (stave, message) {
      stave.setBegBarType(VF.Barline.type.REPEAT_BEGIN);
      if (message) {
        stave.setText(message, VF.Modifier.Position.ABOVE, 
          { shift_y: 5, justification: VF.TextNote.Justification.LEFT });
        }
    },

    displayBarEndRepeat : function (stave) {
      stave.setEndBarType(VF.Barline.type.REPEAT_END);
    },

    displayBarBothRepeat : function (stave) {
      stave.setBegBarType(VF.Barline.type.REPEAT_BEGIN);
      stave.setEndBarType(VF.Barline.type.REPEAT_END);      
    },

    /*  This formatting appears to be sufficient for our needs.  It attempts to emulate
        the formatting we used successfully with VexFlow 1.2.89.  This went horribly wrong
        for our purposes with VexFlow 3.  The magic softmaxFactor seems to put things right.
    */
    formatAndDrawNotes: function (context, stave, notes) {
      // Create a voice and add the notes.  SOFT mode is not strict about filling the bar 
      // with enough notes to satisfy the time signature which would be disastrous.
      // Users can get it wrong, we'd need to special-case start and end bars etc.
      const voice = new VF.Voice().setMode(VF.Voice.Mode.SOFT);

      voice.addTickables(notes);
      
      // Format and justify the notes
      new VF.Formatter({ softmaxFactor: 5 }).joinVoices([voice]).format([voice]).formatToStave([voice], stave);
      
      // Render voice
      voice.draw(context, stave);
    },


    // make a stave note (n.b. this can represent a single note or a chord)
    makeStaveNote: function (noteSpec) {
      // console.log("makeStaveNote")
      // console.log(noteSpec);
      var sn = new VF.StaveNote(noteSpec.vexNote);
      wrapper.addAccidentals (sn, noteSpec.accidentals);
      wrapper.addDots (sn, noteSpec.dotCount);
      wrapper.addOrnaments (sn, noteSpec.ornaments);
      wrapper.addArticulations (sn, noteSpec.articulations);
      if (noteSpec.chordSymbol) {
        wrapper.addChordSymbol (sn, noteSpec.chordSymbol);
      }

      if (noteSpec.graceKeys.length > 0) {
        var graceNotes = noteSpec.graceKeys.map(wrapper.makeGraceNote);
        wrapper.addGraceAccidentals (graceNotes, noteSpec.graceAccidentals);
        var graceNoteGroup =  new VF.GraceNoteGroup(graceNotes, true);
        sn.addModifier(graceNoteGroup.beamNotes(), 0);
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
           num_notes: vexTuplet.p, notes_occupied: vexTuplet.q, location: VF.Tuplet.LOCATION_BOTTOM,
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
          staveNote.addModifier(new VF.Accidental(accidentalString), index);
        }
      });
    },

    // add any accidentals to the grace notes
    addGraceAccidentals: function (graceNotes, accidentals) {
      accidentals.forEach (function (accidentalString, index) {
        if (accidentalString) {
          /* console.log("grace accidental ", accidentalString, " at ", index); */
          graceNotes[index].addModifier(new VF.Accidental(accidentalString), 0);
        }
      });
    },

    // add the dottedness to the staveNote
    addDots: function (staveNote, dotCount) {
      if (dotCount == 2) {
        VF.Dot.buildAndAttach([staveNote], { all: true });
        VF.Dot.buildAndAttach([staveNote], { all: true });
      }
      else if (dotCount == 1) {
        VF.Dot.buildAndAttach([staveNote], { all: true });
      }
    },

    // add the ornament(s) to the staveNote
    addOrnaments: function (staveNote, ornaments) {
      ornaments.forEach (function (ornament, index) {
        staveNote.addModifier(new VF.Ornament(ornament), 0);
      });
    },

    // add a chord symbol above the note where it is to take effect 
    addChordSymbol: function (staveNote, chordSymbol) {
      var chord = new VF.ChordSymbol().addGlyphOrText(chordSymbol);
      staveNote.addModifier(chord, 0);
    },

    // add the articulation(s) to the staveNote
    addArticulations: function (staveNote, articulations) {
      articulations.forEach (function (articulation, index) {
        // position 3 above stave, position 4 below it
        staveNote.addModifier(new VF.Articulation(articulation).setPosition(4), 0);
      });
    },

    // add the repetitions to the stave
    addRepetitions: function (stave, repetitions) {
      repetitions.forEach (function (repetition, index) {
        // console.log ("repetition:", repetition);
        stave.setRepetitionType(repetition, 25);
      });
    }

  }

}();

export var initialiseCanvasImpl = wrapper.initializeCanvas;
export var resizeCanvasImpl = wrapper.resizeCanvas;
export var clearCanvasImpl = wrapper.clearCanvas;
export var makeStaveImpl = wrapper.makeStave;
export var renderStaveImpl = wrapper.renderStave;
export var getStaveWidthImpl = wrapper.getStaveWidth;
export var renderTextImpl = wrapper.renderText;
export var displayBarBeginRepeatImpl = wrapper.displayBarBeginRepeat;
export var displayBarEndRepeatImpl = wrapper.displayBarEndRepeat;
export var displayBarBothRepeatImpl = wrapper.displayBarBothRepeat;
export var renderBarContentsImpl = wrapper.renderBarContents;
export var displayVoltaImpl = wrapper.displayVolta;
export var addTimeSignatureImpl = wrapper.addTimeSignature;
export var addKeySignatureImpl = wrapper.addKeySignature;
export var addTempoSignatureImpl = wrapper.addTempoSignature;
