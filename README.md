purescript-vexflow
==================

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

To build the full tune examples
-----------------------------------
     bower install
     npm run full-tune

Limitations
-----------

*  We rely on VexFlow's auto-beaming feature which doesn't always do the right thing with (for example) tuplets, depending on the key signature.
*  ABC's modified key signature (for defining non-classical modes as found in, for example, some klezmer scores) is not supported.
*  Grace notes are supported only against individual notes (not chords, tuplets etc.).
