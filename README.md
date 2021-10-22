purescript-abc-scores
=====================

This is a purescript wrapper for [VexFlow](http://www.vexflow.com/) which is designed to cover just enough of the API in order to allow the engraving of music scores produced by the [ABC parser](https://github.com/newlandsvalley/purescript-abc-parser).

It is intended to be a replacement for [purescript-scores](https://github.com/newlandsvalley/purescript-scores) which itself was a wrapper for [VexTab](http://www.vexflow.com/vextab). i.e. the intention is to cut out the middle man (which is anyhow not regularly maintained).

Currently, it uses VexFlow v1.2.89

To build the library
--------------------

     spago install
     npm run build

or   

     bower install
     pulp buils

To build the small display examples
-----------------------------------
     npm run display-tests

To build the full tune example
------------------------------
     npm run full-tune

To build the example showing right-alignment of staves
------------------------------------------------------
     npm run align-staves

To build the example showing beaming
------------------------------------
     npm run beaming

To build the example showing slurs
----------------------------------
     npm run slurs

To build the example showing treble and base clefs
--------------------------------------------------
     npm run clefs

To build the example showing chord symbols
----------------------------------
     npm run chord-symbols

To build the example showing a thumbnail
----------------------------------------

(a thumbnail of the initial part of a score for use in lists)

     npm run thumbnail

VexFlow Version support
-----------------------

The library now works with the most recent version of VexFlow (Currently 3.09).  However, there is a [regression](https://github.com/0xfe/vexflow/issues/798) in this version of VexFlow which causes minims not to be formatted correctly. Consequently, we stay with the much earlier version which continues to work well.  We will migrate to the newer version when it is fixed or at least when the new approach to formatting is documented fully.

It seems that there is a good deal of recent development work on VexFlow but with very little being released.  One improvement is the introduction of support for chord symbols.  The ```chords``` branch puts basic infrastructure in place for also supporting these, but this support is currently limited to using note ```Annotations``` which means that the chord symbols are not horizontally aligned.  We will continue to monitor VexFlow progress and support these in the master branch when we can.

Limitations
-----------

*  ABC's modified key signature (for defining non-classical modes as found in, for example, some klezmer scores) is not supported.  (It is not yet supported by VexFlow.)
*  Chord symbols are not supported (intentionally).
*  Slurs are only supported if they are entirely contained within a bar.  This is partly because they are problematic in ABC (being unbalanceable) and partly because the score for the ABC is engraved on a bar-by-bar basis.  Although VexFlow does allow you to specify that the beginning and end notes live in separate bars, we cannot take advantage of it because, when processing the second bar, notes from the first are no longer in the stack frame.
*  Polyphonic scores are not properly supported.  Voice headers are ignored - there is thus no attempt to link the staves of the separate voices nor to align the bar lines.  The score is treated as a single monophonic tune.
*  Support for different voices (i.e. the ABC `Voice` header which may describe a particular clefs) is limited.  Currently, only treble and bass clefs are supported with treble being the default.  There is no attempt made to align the staves in a multi-part piece, attached by stave ties.  Rather, it is assumed that an application will split the tune into separate ABC formulations for each voice and display them separately.
