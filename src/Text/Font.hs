module Text.Font (
  Font(..),
  loadFont,

  FontAtlas(..),

  FontAtlasMeta(..),
  YOrigin(..),

  FontVariant(..),
  FontMetrics(..),
  GlyphMap(..),

  Glyph(..),
  Bounds(..),

  module Codec.Picture.Types,
  module Data.IntMap.Strict,
  module Data.Vector
) where

import Codec.Picture.Png
import Codec.Picture.Types
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString as BS
import Data.IntMap.Strict (IntMap, fromList, lookup)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import GHC.Generics

data Font = Font {
  fontAtlas :: FontAtlas,
  fontBitmap :: Image Pixel8
}

loadFont :: IO Font
loadFont = do
  atlas' <- fmap (either error id)
              . eitherDecodeFileStrict $ "fonts/atlas.json"
  png <- BS.readFile "fonts/glyphs.png"
  let bitmap' = either error fromImageY8 . decodePng $ png
  return $ Font atlas' bitmap'
 where
  fromImageY8 dynamicImage =
    case dynamicImage of
      ImageY8 image -> image
      _             -> error "loadFont: expecting 8-bit greyscale image."

type TextureWidth = Int;
type TextureHeight = Int;

data FontAtlas = FontAtlas {
  atlasMeta :: FontAtlasMeta,
  variants :: Vector FontVariant
} deriving (Show, Generic)

instance FromJSON FontAtlas where
  parseJSON = withObject "FontAtlas" $ \v -> FontAtlas
    <$> v .: "atlas"
    <*> v .: "variants"

data FontVariant = FontVariant {
  metrics :: FontMetrics,
  glyphs :: GlyphMap
} deriving (Show, Generic)

instance FromJSON FontVariant

data FontAtlasMeta = FontAtlasMeta {
  size :: Float,
  width :: TextureWidth,
  height :: TextureHeight,
  yOrigin :: YOrigin
} deriving (Show, Generic)

instance FromJSON FontAtlasMeta where
  parseJSON = withObject "FontAtlasMeta" $ \v -> FontAtlasMeta
    <$> v .: "size"
    <*> v .: "width"
    <*> v .: "height"
    <*> v .: "yOrigin"

data YOrigin = YBottom | YTop
  deriving (Eq, Show, Generic)

instance FromJSON YOrigin where
  parseJSON = withText "YOrigin" parseYOrigin
   where
    parseYOrigin t | t == "bottom" = return YBottom
                   | t == "top"    = return YTop
                   | otherwise     = parseFail "Unrecognised yOrigin"

data FontMetrics = FontMetrics {
  emSize :: Int,
  lineHeight :: Float,
  ascender :: Float,
  descender :: Float,
  underlineY :: Float,
  underlineThickness :: Float
} deriving (Show, Generic)

instance FromJSON FontMetrics

newtype GlyphMap = GlyphMap { unGlyphMap :: IntMap Glyph }
  deriving (Show)

instance FromJSON GlyphMap where
  parseJSON = withArray "GlyphMap" $ \a -> do
    gs <- fmap V.toList . mapM parseJSON $ a
    return . GlyphMap . fromList . fmap ((,) <$> unicode <*> id) $ gs

data Glyph = Glyph {
  unicode :: Int,
  advance :: Float,
  planeBounds :: Maybe Bounds,
  atlasBounds :: Maybe Bounds
} deriving (Show)

instance FromJSON Glyph where
  parseJSON = withObject "Glyph" $ \v -> Glyph
    <$> v .:  "unicode"
    <*> v .:  "advance"
    <*> v .:? "planeBounds"
    <*> v .:? "atlasBounds"

data Bounds = Bounds {
  top :: Float,
  right :: Float,
  bottom :: Float,
  left :: Float
} deriving (Show, Generic)

instance FromJSON Bounds
