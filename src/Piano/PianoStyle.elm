module Piano.PianoStyle exposing (..)

{-|
Exposes a single string with the correct CSS styles for the component.

This was made an extra module to avoid long CSS styles in the main script.

@docs css

Based on javascript-piano by michaelemp
https://github.com/michaelmp/js-piano
-}


{-| CSS style
-}
css : String
css =
    """
    .piano {
      padding: 5px;
      margin: 0 auto;
      /* width: 550px; */
    }

    .piano-container, .piano-javascript {
      border-radius: 5px;
      margin: 5px;
      padding: 5px;
      white-space: nowrap;
    }

    .piano-container {
      text-align: center;
    }

    .piano-keys{
      word-spacing: 0;
      letter-spacing: 0;
      font-size: 0;
    }

    .piano-white, .piano-black {
      display: inline-block;
      position: relative;
      vertical-align: top;
      direction: ltr;
      margin: 0;
      padding: 0;
    }

    .piano-white, .piano-black-raised {
      border-radius: 2px;
      border-color: #222;
      border-style: solid;
      border-width: 1px 1px 1px 1px;
    }

    .piano-white {
      width: 24px;
      height: 100px;
      background-color: white;
      z-index: 1;
    }

    .piano-black {
      width: 0px;
      height: 100px;
      z-index: 2;
    }

    .piano-black-raised {
      width: 16px;
      height: 70px;
      position: relative;
      left: -10px;
      background-color: black;
    }

    .piano-white.pressed {
        background-color: #88FFAA;
    }

    .piano-black-raised.pressed{
        background-color: #55AA55;
    }

    .piano-white.pressed {
        background-color: #88FFAA;
    }

    .piano-black-raised.pressed{
        background-color: #55AA55;
    }
"""
