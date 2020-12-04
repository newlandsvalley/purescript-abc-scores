purescript-abc-scores
=====================

This is a purescript wrapper for [VexFlow](http://www.vexflow.com/) which is designed to cover just enough of the API in order to allow the engraving of music scores produced by the [ABC parser](https://github.com/newlandsvalley/purescript-abc-parser).

It is intended to be a replacement for [purescript-scores](https://github.com/newlandsvalley/purescript-scores) which itself was a wrapper for [VexTab](http://www.vexflow.com/vextab). i.e. the intention is to cut out the middle man (which is anyhow not regularly maintained).

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

To build the example showing a thumbnail
----------------------------------------

(a thumbnail of the initial part of a score for use in lists)

     npm run thumbnail


Limitations
-----------

*  ABC's modified key signature (for defining non-classical modes as found in, for example, some klezmer scores) is not supported.  (It is not yet supported by VexFlow.)
*  Chord symbols are not supported (intentionally).
*  Slurs are only supported if they are entirely contained within a bar.  This is partly because they are problematic in ABC (being unbalanceable) and partly because the score for the ABC is engraved on a bar-by-bar basis.  Although VexFlow does allow you to specify that the beginning and end notes live in separate bars, we cannot take advantage of it because, when processing the second bar, notes from the first are no longer in the stack frame.
*  Polyphonic scores are not properly supported.  Voice headers are ignored - there is thus no attempt to link the staves of the separate voices nor to align the bar lines.  The score is treated as a single monophonic tune.
