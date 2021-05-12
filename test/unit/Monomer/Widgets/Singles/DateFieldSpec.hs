{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.Singles.DateFieldSpec (spec) where

import Control.Lens ((&), (^.), (.~))
import Control.Lens.TH (abbreviatedFields, makeLensesWith)
import Data.Default
import Data.Text (Text)
import Data.Time
import Test.Hspec

import qualified Data.Sequence as Seq

import Monomer.Core
import Monomer.Core.Combinators
import Monomer.Event
import Monomer.TestUtil
import Monomer.TestEventUtil
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Singles.Button
import Monomer.Widgets.Singles.DateField

import qualified Monomer.Lens as L

data TestEvt
  = DateChanged Day
  | GotFocus Path
  | LostFocus Path
  deriving (Eq, Show)

data DateModel = DateModel {
  _imDateValue :: Day,
  _imDateValid :: Bool
} deriving (Eq, Show)

instance Default DateModel where
  def = DateModel {
    _imDateValue = fromGregorian 2000 01 01,
    _imDateValid = True
  }

makeLensesWith abbreviatedFields ''DateModel

spec :: Spec
spec = describe "DateField" $ do
  handleEventDate
  handleEventValueDate
  handleEventMouseDragDate
  getSizeReqDate

handleEventDate :: Spec
handleEventDate = describe "handleEventDate" $ do
  it "should remove the contents and get Nothing as model value" $ do
    modelBasic [evtKG keyA, evtK keyBackspace] ^. dateValue `shouldBe` midDate
    modelBasic [evtKG keyA, evtK keyBackspace] ^. dateValid `shouldBe` False

  it "should input '14/02/2000'" $ do
    model [evtT "14/02", evtT "/2000"] ^. dateValue `shouldBe` lowDate
    model [evtT "14/02", evtT "/2000"] ^. dateValid `shouldBe` True

  it "should input '1' (invalid date)" $ do
    model [evtKG keyA, evtT "1"] ^. dateValue `shouldBe` midDate
    model [evtKG keyA, evtT "1"] ^. dateValid `shouldBe` False

  it "should input '30/12/1999' but fail because of minValue" $ do
    model [evtT "30/12/1999"] ^. dateValue `shouldBe` midDate
    model [evtT "30/12/1999"] ^. dateValid `shouldBe` False

  it "should input '01/03/2005' but fail because of maxValue" $ do
    model [evtT "01/04/2005"] ^. dateValue `shouldBe` midDate
    model [evtT "01/04/2005"] ^. dateValid `shouldBe` False

  it "should remove one character and input '4'" $ do
    model [moveCharR, delCharL, evtT "4"] ^. dateValue `shouldBe` fromGregorian 2004 03 02
    model [moveCharR, delCharL, evtT "4"] ^. dateValid `shouldBe` True

  it "should input '14/02/2001', remove one word and input '2000'" $ do
    model [evtT "14/02/2001", delWordL, evtT "2000"] ^. dateValue `shouldBe` lowDate
    model [evtT "14/02/2001", delWordL, evtT "2000"] ^. dateValid `shouldBe` True

  it "should update the model when using the wheel" $ do
    let p = Point 100 10
    let steps1 = [WheelScroll p (Point 0 (-2000)) WheelNormal]
    let steps2 = [WheelScroll p (Point 0 (-64)) WheelNormal]
    let steps3 = [WheelScroll p (Point 0 64) WheelNormal]
    let steps4 = [WheelScroll p (Point 0 (-2000)) WheelFlipped]
    model steps1 ^. dateValue `shouldBe` minDate
    model steps2 ^. dateValue `shouldBe` fromGregorian 2001 12 28
    model steps3 ^. dateValue `shouldBe` fromGregorian 2002 05 05
    model steps4 ^. dateValue `shouldBe` maxDate

  it "should generate an event when focus is received" $
    events evtFocus `shouldBe` Seq.singleton (GotFocus emptyPath)

  it "should generate an event when focus is lost" $
    events evtBlur `shouldBe` Seq.singleton (LostFocus emptyPath)

  where
    minDate = fromGregorian 2000 01 01
    lowDate = fromGregorian 2000 02 14
    midDate = fromGregorian 2002 03 02
    maxDate = fromGregorian 2005 03 05
    wenv = mockWenv (DateModel midDate True)
    basicDateNode :: WidgetNode DateModel TestEvt
    basicDateNode = dateField_ dateValue [validInput dateValid, selectOnFocus_ False]
    dateCfg = [minValue minDate, maxValue maxDate, validInput dateValid, onFocus GotFocus, onBlur LostFocus]
    dateNode = dateField_ dateValue dateCfg
    model es = nodeHandleEventModel wenv (evtFocus : es) dateNode
    modelBasic es = nodeHandleEventModel wenv es basicDateNode
    events evt = nodeHandleEventEvts wenv [evt] dateNode

handleEventValueDate :: Spec
handleEventValueDate = describe "handleEventDate" $ do
  it "should input an '23/11/1983'" $
    evts [evtT "23/11/1983"] `shouldBe` Seq.fromList [DateChanged lowDate]

  it "should move right, delete one character and input '5'" $ do
    let steps = [moveCharR, delCharL, evtT "5"]
    lastEvt steps `shouldBe` DateChanged (fromGregorian 1985 03 02)
    model steps ^. dateValid `shouldBe` True

  it "should move right and delete one character" $ do
    let steps = [moveCharR, delCharL]
    evts steps `shouldBe` Seq.empty
    model steps ^. dateValid `shouldBe` False

  it "should input '23/11/198', input '.', 'a', then input '3'" $ do
    let steps = [evtT "23/11/198", evtT ".", evtT "a", evtT "3"]
    lastEvt steps `shouldBe` DateChanged lowDate
    model steps ^. dateValid `shouldBe` True

  it "should input '0544/03/199555'" $ do
    let steps = [evtT "05", evtT "44", evtT "/03/1995", evtT "55"]
    lastEvt steps `shouldBe` DateChanged maxDate

  where
    minDate = fromGregorian 1980 01 01
    lowDate = fromGregorian 1983 11 23
    midDate = fromGregorian 1989 03 02
    maxDate = fromGregorian 1995 03 05
    wenv = mockWenv (DateModel minDate False)
    dateNode = dateFieldV_ midDate DateChanged [minValue minDate, maxValue maxDate, selectOnFocus, validInput dateValid]
    evts es = nodeHandleEventEvts wenv (evtFocus : es) dateNode
    model es = nodeHandleEventModel wenv (evtFocus : es) dateNode
    lastIdx es = Seq.index es (Seq.length es - 1)
    lastEvt es = lastIdx (evts es)

handleEventMouseDragDate :: Spec
handleEventMouseDragDate = describe "handleEventMouseDragDate" $ do
  it "should drag upwards 100 pixels, setting the value to 10/06/1989" $ do
    let selStart = Point 50 30
    let selEnd = Point 50 (-70)
    let steps = [evtPress selStart, evtMove selEnd, evtReleaseDrag selEnd]
    model steps ^. dateValue `shouldBe` fromGregorian 1989 06 10

  it "should drag downwards 100 pixels, setting the value to 14/08/1988 (dragRate = 2)" $ do
    let selStart = Point 50 50
    let selEnd = Point 50 150
    let steps = [evtPress selStart, evtMove selEnd, evtReleaseDrag selEnd]
    model steps ^. dateValue `shouldBe` fromGregorian 1988 08 14

  it "should drag downwards 10000 pixels, staying at minDate (the minimum)" $ do
    let selStart = Point 50 50
    let selEnd = Point 50 10050
    let steps = [evtPress selStart, evtMove selEnd, evtReleaseDrag selEnd]
    model steps ^. dateValue `shouldBe` minDate

  it "should drag upwnwards 10000 pixels, staying at maxDate (the maximum)" $ do
    let selStart = Point 50 50
    let selEnd = Point 50 (-1950)
    let steps = [evtPress selStart, evtMove selEnd, evtReleaseDrag selEnd]
    model steps ^. dateValue `shouldBe` maxDate

  it "should drag downwards 30 and 20 pixels, setting the value to 11/01/1989" $ do
    let selStart = Point 50 30
    let selMid = Point 50 60
    let selEnd = Point 50 50
    let steps = [
          evtPress selStart, evtMove selMid, evtReleaseDrag selMid,
          evtPress selStart, evtMove selEnd, evtReleaseDrag selEnd
          ]
    model steps ^. dateValue `shouldBe` fromGregorian 1989 01 11

  it "should drag upwards 100 pixels, but value stay at midDate since it has focus" $ do
    let selStart = Point 50 30
    let selEnd = Point 50 (-70)
    let steps = [evtK keyTab, evtPress selStart, evtMove selEnd, evtRelease selEnd]
    model steps ^. dateValue `shouldBe` midDate

  it "should drag upwards 100 pixels, but value stay at midDate since it was double clicked on" $ do
    let selStart = Point 50 30
    let selEnd = Point 50 (-70)
    let steps = [evtDblClick selStart, evtPress selStart, evtMove selEnd, evtRelease selEnd]
    model steps ^. dateValue `shouldBe` midDate

  where
    minDate = fromGregorian 1980 01 01
    midDate = fromGregorian 1989 03 02
    maxDate = fromGregorian 1995 03 05
    wenv = mockWenv (DateModel midDate True)
    dateNode = vstack [
        button "Test" (DateChanged midDate), -- Used only to have focus
        dateField dateValue,
        dateField_ dateValue [dragRate 2, minValue minDate, maxValue maxDate]
      ]
    evts es = nodeHandleEventEvts wenv es dateNode
    model es = nodeHandleEventModel wenv es dateNode
    lastIdx es = Seq.index es (Seq.length es - 1)
    lastEvt es = lastIdx (evts es)

getSizeReqDate :: Spec
getSizeReqDate = describe "getSizeReqDate" $ do
  it "should return width = Flex 160 1" $
    sizeReqW `shouldBe` expandSize 160 1

  it "should return height = Fixed 20" $
    sizeReqH `shouldBe` fixedSize 20

  it "should return width = Flex 100 1 when resizeOnChange = True" $
    sizeReqW2 `shouldBe` expandSize 100 1

  it "should return height = Fixed 20 when resizeOnChange = True" $
    sizeReqH2 `shouldBe` fixedSize 20

  where
    wenv = mockWenvEvtUnit (def :: DateModel)
    (sizeReqW, sizeReqH) = nodeGetSizeReq wenv (dateField dateValue)
    dateResize = dateField_ dateValue [resizeOnChange]
    (sizeReqW2, sizeReqH2) = nodeGetSizeReq wenv dateResize