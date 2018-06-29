module Color exposing (Color, toRgb, hsl)

type alias Color =
    String

toRgb _ =
    { red = 255, green = 0, blue = 0, alpha = 255 }

hsl _ _ _ = ""
