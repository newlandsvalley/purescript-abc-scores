purescript-abc-scores
=====================

This is a purescript wrapper for [VexFlow](http://www.vexflow.com/) which is designed to cover just enough of the API in order to allow the engraving of music scores produced by the [ABC parser](https://github.com/newlandsvalley/purescript-abc-parser).

It is intended to be a replacement for [purescript-scores](https://github.com/newlandsvalley/purescript-scores) which itself was a wrapper for [VexTab](http://www.vexflow.com/vextab). i.e. the intention is to cut out the middle man (which is anyhow not regularly maintained).

To build the library
--------------------

     bower install
     npm run build

To build the small display examples
-----------------------------------
     bower install
     npm run display-tests

To build the full tune example
------------------------------
     bower install
     npm run full-tune

To build the example showing right-alignment of staves
------------------------------------------------------
     bower install
     npm run align-staves

To build the example showing formatting errors
----------------------------------------------
          bower install
          npm run errors

These are mostly caused by the inadequacy of auto-beaming.          


Limitations
-----------

*  Beaming of notes in 4/4 is not quite right.  Some tunes require a separate beam for each beat in the bar whereas, for example, reels and hornpipes tend to beam over two beats. Currently we are biased towards the latter.
*  ABC's modified key signature (for defining non-classical modes as found in, for example, some klezmer scores) is not supported.