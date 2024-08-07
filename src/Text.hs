module Text (
  createText,
  FontVariantI,

  Font,

  loadFont
) where

import Data.ByteString (pack)
import Data.Char
import Data.Maybe
import Graphics.Gloss

import Text.Font as F

type Origin = (Float, Float)
type Size = Float
type FontVariantI = Int

createText :: Font
  -> FontVariantI
  -> Color
  -> Origin
  -> String
  -> Picture
createText Font{..} iVar clr orig str =
  let bmp = fontBitmap
      bmpMeta = atlasMeta fontAtlas
      v@FontVariant{..} = variants fontAtlas ! iVar
      defGlyph = fromMaybe (defaultGlyphErr iVar) . F.lookup (ord '?')
                   . unGlyphMap $ glyphs
      sz       = size . atlasMeta $ fontAtlas
      glyphs'  = catMaybes . fst . foldl accumGlyph ([], orig) $ str

      accumGlyph (qs, o@(x, y)) c =
        let (glyph, advance) = renderGlyph bmp bmpMeta v sz clr defGlyph o c
        in (glyph:qs, (x + advance, y))

  in Pictures glyphs'
 where
  defaultGlyphErr _ = error . (msg ++) . show $ iVar
   where
    msg = "createText: Default glyph not found in atlas for font variant "

-- Render a glyph `Picture` and return the advance.
renderGlyph :: Image Pixel8
  -> FontAtlasMeta
  -> FontVariant
  -> Size
  -> Color
  -> Glyph
  -> Origin
  -> Char
  -> (Maybe Picture, Float)
renderGlyph bmp atlasMeta FontVariant{..} sz clr defGlyph (ox, oy) c =
  let Glyph{..} = fromMaybe defGlyph . F.lookup (ord c) . unGlyphMap $ glyphs
      p = glyphBitmap <$> atlasBounds <*> planeBounds
  in (p, advance * sz)
 where
  glyphBitmap :: Bounds -> Bounds -> Picture
  glyphBitmap aBounds pBounds =
    let tx = left pBounds * sz + ox
        ty = bottom pBounds * sz + oy
        w  = right pBounds * sz
        h  = top pBounds * sz

        bmpW = liftA2 (-) right left aBounds
        bmpH = liftA2 (-) top bottom aBounds

        fmt = BitmapFormat TopToBottom PxRGBA

        ps = do let r = ceiling . right $ aBounds
                    l = ceiling . left $ aBounds
                    t = if bmpYOrig == YBottom
                          then (`subtract` atlasH) . ceiling . top $ aBounds
                          else ceiling . top $ aBounds
                    b = if bmpYOrig == YBottom
                          then (`subtract` atlasH) . ceiling . bottom $ aBounds
                          else ceiling . bottom $ aBounds
                y <- [t..(b-1)]
                x <- [l..(r-1)]
                return (x, y)

        dat = pack . concatMap (colour . uncurry (pixelAt bmp)) $ ps

        bmpYOrig = yOrigin atlasMeta
        atlasH   = height atlasMeta
    in  Translate tx ty . Pictures $ [
            --Line [(0, 0), (w, 0), (w, h), (0, h), (0, 0)],
            Translate (w/2) (h/2) . bitmapOfByteString (round bmpW) (round bmpH) fmt dat $ True
          ]

  d = descender metrics * sz

  -- Return the colour for a pixel taken from the hardmask.
  -- 0 = transparent, 1 = clr
  colour p = if p > 0 then colourBytes else [0, 0, 0, 0]

  colourBytes =
    let (r, g, b, a) = rgbaOfColor clr
    in fmap floatToByte [r , g, b, a]

  floatToByte = round . (* 255)
