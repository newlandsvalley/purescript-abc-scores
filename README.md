purescript-vexflow
==================

Work in progress.

This is an attempt to write a purescript wrapper for [VexFlow](http://www.vexflow.com/) which covers just enough of the API in order to allow the engraving of music scores produced by the [ABC parser](https://github.com/newlandsvalley/purescript-abc-parser).

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
     npm run display-tests
