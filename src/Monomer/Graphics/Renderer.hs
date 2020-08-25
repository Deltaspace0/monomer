{-# LANGUAGE ExistentialQuantification #-}

module Monomer.Graphics.Renderer where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Text (Text)

import Monomer.Common.Geometry
import Monomer.Common.Tree
import Monomer.Graphics.Types

data Renderer m = (Monad m) => Renderer {
  beginPath :: m (),
  closePath :: m (),
  -- Context management
  saveContext :: m (),
  restoreContext :: m (),
  -- Overlays
  createOverlay :: m () -> m (),
  renderOverlays :: m (),
  -- Scissor operations
  setScissor :: Rect -> m (),
  resetScissor :: m (),
  -- Strokes
  stroke :: m (),
  setStrokeColor :: Color -> m (),
  setStrokeWidth :: Double -> m (),
  -- Fill
  fill :: m (),
  setFillColor :: Color -> m (),
  setFillLinearGradient :: Point -> Point -> Color -> Color -> m (),
  -- Drawing
  moveTo :: Point -> m (),
  renderLine :: Point -> Point -> m (),
  renderLineTo :: Point -> m (),
  renderRect :: Rect -> m (),
  renderArc :: Point -> Double -> Double -> Double -> Winding -> m (),
  renderQuadTo :: Point -> Point -> m (),
  renderEllipse :: Rect -> m (),
  -- Text
  renderText :: Rect -> Font -> FontSize -> Align -> Text -> m Rect,
  computeTextSize :: Font -> FontSize -> Text -> Size,
  -- Image
  createImage :: Int -> Int -> ByteString -> Maybe ImageHandle,
  renderImage :: Rect -> ImageHandle -> m ()
}
