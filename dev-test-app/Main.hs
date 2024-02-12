{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Monomer
import TextShow

newtype AppModel = AppModel {
  _imageOrder :: Bool
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppStartFirst
  | AppStartSecond
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack [
      hstack [
          button "Start anim1" AppStartFirst,
          button "Start anim2" AppStartSecond
        ],
      spacer,
      imageGrid
    ] `styleBasic` [padding 10]
  imageGrid = vgrid $ zipWith makeAnimationNode nodeKeys images
  makeAnimationNode key imageNode = animSlideIn imageNode `nodeKey` key
  nodeKeys = ["anim1", "anim2"]
  images = if model ^. imageOrder
    then [image1, image2]
    else [image2, image1]
  image1 = image_ "assets/images/red-button.png" [fitEither, noDispose]
  image2 = image_ "assets/images/red-button-hover.png" [fitEither, noDispose]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  AppStartFirst -> [
      Model $ model & imageOrder %~ not,
      Message "anim1" AnimationStart
    ]
  AppStartSecond -> [
      Model $ model & imageOrder %~ not,
      Message "anim2" AnimationStart
    ]

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Dev test app",
      appWindowIcon "./assets/images/icon.png",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent AppInit,
      appModelFingerprint show
      ]
    model = AppModel False
