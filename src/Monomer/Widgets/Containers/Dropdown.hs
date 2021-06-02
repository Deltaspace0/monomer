{-|
Module      : Monomer.Widgets.Containers.Dropdown
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable

Dropdown widget, allowing selection of a single item from a collapsable list.
Both header and list content is customizable, plus its styling. In case only
text content is needed, 'Monomer.Widgets.Singles.TextDropdown' is easier to use.

Configs:

- onFocus: event to raise when focus is received.
- onFocusReq: WidgetReqest to generate when focus is received.
- onBlur: event to raise when focus is lost.
- onBlurReq: WidgetReqest to generate when focus is lost.
- onChange: event to raise when selected item changes.
- onChangeReq: WidgetRequest to generate when selected item changes.
- onChangeIdx: event to raise when selected item changes. Includes index,
- onChangeIdxReq: WidgetRequest to generate when selected item changes. Includes
index.
- maxHeight: maximum height of the list when dropdown is expanded.
- itemNormalStyle: style of an item in the list when not selected.
- itemSelectedStyle: style of the selected item in the list.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.Containers.Dropdown (
  DropdownCfg,
  DropdownItem(..),
  dropdown,
  dropdown_,
  dropdownV,
  dropdownV_,
  dropdownD_
) where

import Control.Applicative ((<|>))
import Control.Lens (ALens', (&), (^.), (^?), (^?!), (.~), (%~), (<>~), _Just, ix, non)
import Control.Monad
import Data.Default
import Data.List (foldl')
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Sequence (Seq(..), (<|), (|>))
import Data.Text (Text)
import Data.Typeable (cast)
import GHC.Generics

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container
import Monomer.Widgets.Containers.SelectList
import Monomer.Widgets.Singles.Label

import qualified Monomer.Lens as L

type DropdownItem a = SelectListItem a

data DropdownCfg s e a = DropdownCfg {
  _ddcMaxHeight :: Maybe Double,
  _ddcItemStyle :: Maybe Style,
  _ddcItemSelectedStyle :: Maybe Style,
  _ddcOnFocus :: [Path -> e],
  _ddcOnFocusReq :: [WidgetRequest s e],
  _ddcOnBlur :: [Path -> e],
  _ddcOnBlurReq :: [WidgetRequest s e],
  _ddcOnChange :: [a -> e],
  _ddcOnChangeReq :: [a -> WidgetRequest s e],
  _ddcOnChangeIdx :: [Int -> a -> e],
  _ddcOnChangeIdxReq :: [Int -> a -> WidgetRequest s e]
}

instance Default (DropdownCfg s e a) where
  def = DropdownCfg {
    _ddcMaxHeight = Nothing,
    _ddcItemStyle = Nothing,
    _ddcItemSelectedStyle = Nothing,
    _ddcOnFocus = [],
    _ddcOnFocusReq = [],
    _ddcOnBlur = [],
    _ddcOnBlurReq = [],
    _ddcOnChange = [],
    _ddcOnChangeReq = [],
    _ddcOnChangeIdx = [],
    _ddcOnChangeIdxReq = []
  }

instance Semigroup (DropdownCfg s e a) where
  (<>) t1 t2 = DropdownCfg {
    _ddcMaxHeight = _ddcMaxHeight t2 <|> _ddcMaxHeight t1,
    _ddcItemStyle = _ddcItemStyle t2 <|> _ddcItemStyle t1,
    _ddcItemSelectedStyle = _ddcItemSelectedStyle t2 <|> _ddcItemSelectedStyle t1,
    _ddcOnFocus = _ddcOnFocus t1 <> _ddcOnFocus t2,
    _ddcOnFocusReq = _ddcOnFocusReq t1 <> _ddcOnFocusReq t2,
    _ddcOnBlur = _ddcOnBlur t1 <> _ddcOnBlur t2,
    _ddcOnBlurReq = _ddcOnBlurReq t1 <> _ddcOnBlurReq t2,
    _ddcOnChange = _ddcOnChange t1 <> _ddcOnChange t2,
    _ddcOnChangeReq = _ddcOnChangeReq t1 <> _ddcOnChangeReq t2,
    _ddcOnChangeIdx = _ddcOnChangeIdx t1 <> _ddcOnChangeIdx t2,
    _ddcOnChangeIdxReq = _ddcOnChangeIdxReq t1 <> _ddcOnChangeIdxReq t2
  }

instance Monoid (DropdownCfg s e a) where
  mempty = def

instance CmbOnFocus (DropdownCfg s e a) e Path where
  onFocus fn = def {
    _ddcOnFocus = [fn]
  }

instance CmbOnFocusReq (DropdownCfg s e a) s e where
  onFocusReq req = def {
    _ddcOnFocusReq = [req]
  }

instance CmbOnBlur (DropdownCfg s e a) e Path where
  onBlur fn = def {
    _ddcOnBlur = [fn]
  }

instance CmbOnBlurReq (DropdownCfg s e a) s e where
  onBlurReq req = def {
    _ddcOnBlurReq = [req]
  }

instance CmbOnChange (DropdownCfg s e a) a e where
  onChange fn = def {
    _ddcOnChange = [fn]
  }

instance CmbOnChangeReq (DropdownCfg s e a) s e a where
  onChangeReq req = def {
    _ddcOnChangeReq = [req]
  }

instance CmbOnChangeIdx (DropdownCfg s e a) e a where
  onChangeIdx fn = def {
    _ddcOnChangeIdx = [fn]
  }

instance CmbOnChangeIdxReq (DropdownCfg s e a) s e a where
  onChangeIdxReq req = def {
    _ddcOnChangeIdxReq = [req]
  }

instance CmbMaxHeight (DropdownCfg s e a) where
  maxHeight h = def {
    _ddcMaxHeight = Just h
  }

instance CmbItemNormalStyle (DropdownCfg s e a) Style where
  itemNormalStyle style = def {
    _ddcItemStyle = Just style
  }

instance CmbItemSelectedStyle (DropdownCfg s e a) Style where
  itemSelectedStyle style = def {
    _ddcItemSelectedStyle = Just style
  }

data DropdownState = DropdownState {
  _ddsOpen :: Bool,
  _ddsOffset :: Point
} deriving (Eq, Show, Generic)

data DropdownMessage
  = forall a . DropdownItem a => OnChangeMessage Int a
  | OnListBlur

-- | Creates a dropdown using the given lens.
dropdown
  :: (Traversable t, DropdownItem a, WidgetEvent e)
  => ALens' s a             -- ^ The lens into the model.
  -> t a                    -- ^ The list of selectable items.
  -> (a -> WidgetNode s e)  -- ^ Function to create the header (always visible).
  -> (a -> WidgetNode s e)  -- ^ Function to create the list (collapsable).
  -> WidgetNode s e         -- ^ The created dropdown.
dropdown field items makeMain makeRow = newNode where
  newNode = dropdown_ field items makeMain makeRow def

-- | Creates a dropdown using the given lens. Accepts config.
dropdown_
  :: (Traversable t, DropdownItem a, WidgetEvent e)
  => ALens' s a             -- ^ The lens into the model.
  -> t a                    -- ^ The list of selectable items.
  -> (a -> WidgetNode s e)  -- ^ Function to create the header (always visible).
  -> (a -> WidgetNode s e)  -- ^ Function to create the list (collapsable).
  -> [DropdownCfg s e a]    -- ^ The config options.
  -> WidgetNode s e         -- ^ The created dropdown.
dropdown_ field items makeMain makeRow configs = newNode where
  widgetData = WidgetLens field
  newNode = dropdownD_ widgetData items makeMain makeRow configs

-- | Creates a dropdown using the given value and onChange event handler.
dropdownV
  :: (Traversable t, DropdownItem a, WidgetEvent e)
  => a                      -- ^ The current value.
  -> (Int -> a -> e)        -- ^ The event to raise on change.
  -> t a                    -- ^ The list of selectable items.
  -> (a -> WidgetNode s e)  -- ^ Function to create the header (always visible).
  -> (a -> WidgetNode s e)  -- ^ Function to create the list (collapsable).
  -> WidgetNode s e         -- ^ The created dropdown.
dropdownV value handler items makeMain makeRow = newNode where
  newNode = dropdownV_ value handler items makeMain makeRow def

-- | Creates a dropdown using the given value and onChange event handler.
-- | Accepts config.
dropdownV_
  :: (Traversable t, DropdownItem a, WidgetEvent e)
  => a                      -- ^ The current value.
  -> (Int -> a -> e)        -- ^ The event to raise on change.
  -> t a                    -- ^ The list of selectable items.
  -> (a -> WidgetNode s e)  -- ^ Function to create the header (always visible).
  -> (a -> WidgetNode s e)  -- ^ Function to create the list (collapsable).
  -> [DropdownCfg s e a]    -- ^ The config options.
  -> WidgetNode s e         -- ^ The created dropdown.
dropdownV_ value handler items makeMain makeRow configs = newNode where
  newConfigs = onChangeIdx handler : configs
  newNode = dropdownD_ (WidgetValue value) items makeMain makeRow newConfigs

-- | Creates a dropdown providing a WidgetData instance and config.
dropdownD_
  :: (Traversable t, DropdownItem a, WidgetEvent e)
  => WidgetData s a         -- ^ The WidgetData to retrieve the value from.
  -> t a                    -- ^ The list of selectable items.
  -> (a -> WidgetNode s e)  -- ^ Function to create the header (always visible).
  -> (a -> WidgetNode s e)  -- ^ Function to create the list (collapsable).
  -> [DropdownCfg s e a]    -- ^ The config options.
  -> WidgetNode s e         -- ^ The created dropdown.
dropdownD_ widgetData items makeMain makeRow configs = makeNode widget where
  config = mconcat configs
  newState = DropdownState False def
  newItems = foldl' (|>) Empty items
  widget = makeDropdown widgetData newItems makeMain makeRow config newState

makeNode :: Widget s e -> WidgetNode s e
makeNode widget = defaultWidgetNode "dropdown" widget
  & L.info . L.focusable .~ True

makeDropdown
  :: (DropdownItem a, WidgetEvent e)
  => WidgetData s a
  -> Seq a
  -> (a -> WidgetNode s e)
  -> (a -> WidgetNode s e)
  -> DropdownCfg s e a
  -> DropdownState
  -> Widget s e
makeDropdown widgetData items makeMain makeRow config state = widget where
  container = def {
    containerChildrenOffset = Just (_ddsOffset state),
    containerUseCustomCursor = True,
    containerGetBaseStyle = getBaseStyle,
    containerInit = init,
    containerFindNextFocus = findNextFocus,
    containerFindByPoint = findByPoint,
    containerMerge = merge,
    containerDispose = dispose,
    containerHandleEvent = handleEvent,
    containerHandleMessage = handleMessage,
    containerGetSizeReq = getSizeReq,
    containerResize = resize
  }
  baseWidget = createContainer state container
  widget = baseWidget {
    widgetRender = render
  }

  mainIdx = 0
  listIdx = 1
  isOpen = _ddsOpen state
  currentValue wenv = widgetDataGet (_weModel wenv) widgetData

  createDropdown wenv node newState = newNode where
    selected = currentValue wenv
    mainStyle = collectTheme wenv L.dropdownStyle
    mainNode = makeMain selected
      & L.info . L.style .~ mainStyle
    widgetId = node ^. L.info . L.widgetId
    selectListNode = makeSelectList wenv widgetData items makeRow config widgetId
    newWidget = makeDropdown widgetData items makeMain makeRow config newState
    newNode = node
      & L.widget .~ newWidget
      & L.children .~ Seq.fromList [mainNode, selectListNode]

  getBaseStyle wenv node = Just style where
    style = collectTheme wenv L.dropdownStyle

  init wenv node = resultNode $ createDropdown wenv node state

  merge wenv newNode oldNode oldState = result where
    result = resultNode $ createDropdown wenv newNode oldState

  dispose wenv node = resultReqs node reqs where
    widgetId = node ^. L.info . L.widgetId
    reqs = [ ResetOverlay widgetId | isOpen ]

  findNextFocus wenv node direction start
    | isOpen = node ^. L.children
    | otherwise = Empty

  findByPoint wenv node start point = result where
    children = node ^. L.children
    mainNode = Seq.index children mainIdx
    listNode = Seq.index children listIdx
    result
      | isOpen && isPointInNodeVp point listNode = Just listIdx
      | not isOpen && isPointInNodeVp point mainNode = Just mainIdx
      | otherwise = Nothing

  ddFocusChange evts reqs prev node = Just newResult where
    tmpResult = handleFocusChange evts reqs config prev node
    newResult = fromMaybe (resultNode node) tmpResult
      & L.requests %~ (|> IgnoreChildrenEvents)

  handleEvent wenv node target evt = case evt of
    Focus prev
      | not isOpen -> ddFocusChange _ddcOnFocus _ddcOnFocusReq prev node
    Blur next
      | not isOpen && not (seqStartsWith path focusedPath)
        -> ddFocusChange _ddcOnBlur _ddcOnBlurReq next node
    Enter{} -> Just result where
      newIcon = fromMaybe CursorHand (style ^. L.cursorIcon)
      result = resultReqs node [SetCursorIcon widgetId CursorHand]
    Move point -> result where
      mainNode = Seq.index (node ^. L.children) mainIdx
      listNode = Seq.index (node ^. L.children) listIdx
      slPoint = addPoint (negPoint (_ddsOffset state)) point
      validMainPos = isPointInNodeVp point mainNode
      validListPos = isOpen && isPointInNodeVp slPoint listNode
      validPos = validMainPos || validListPos
      isArrow = Just CursorArrow == (snd <$> wenv ^. L.cursor)
      resetRes = resultReqs node [SetCursorIcon widgetId CursorArrow]
      result
        | not validPos && not isArrow = Just resetRes
        | otherwise = Nothing
    ButtonAction _ btn BtnPressed _
      | btn == wenv ^. L.mainButton && not isOpen -> result where
        result = Just $ resultReqs node [SetFocus (node ^. L.info . L.widgetId)]
    Click point _
      | openRequired point node -> Just $ openDropdown wenv node
      | closeRequired point node -> Just $ closeDropdown wenv node
    KeyAction mode code KeyPressed
      | isKeyOpenDropdown && not isOpen -> Just $ openDropdown wenv node
      | isKeyEscape code && isOpen -> Just $ closeDropdown wenv node
      where
        activationKeys = [isKeyDown, isKeyUp, isKeySpace, isKeyReturn]
        isKeyOpenDropdown = or (fmap ($ code) activationKeys)
    _
      | not isOpen -> Just $ resultReqs node [IgnoreChildrenEvents]
      | otherwise -> Nothing
    where
      style = activeStyle wenv node
      widgetId = node ^. L.info . L.widgetId
      path = node ^. L.info . L.path
      focusedPath = wenv ^. L.focusedPath

  openRequired point node = not isOpen && inViewport where
    inViewport = pointInRect point (node ^. L.info . L.viewport)

  closeRequired point node = isOpen && not inOverlay where
    offset = _ddsOffset state
    listNode = Seq.index (node ^. L.children) listIdx
    listVp = moveRect offset (listNode ^. L.info . L.viewport)
    inOverlay = pointInRect point listVp

  openDropdown wenv node = resultReqs newNode requests where
    newState = state {
      _ddsOpen = True,
      _ddsOffset = listOffset wenv node
    }
    newNode = node
      & L.widget .~ makeDropdown widgetData items makeMain makeRow config newState
    path = node ^. L.info . L.path
    widgetId = node ^. L.info . L.widgetId
    -- selectList is wrapped by a scroll widget
    slWid = node^?! L.children. ix listIdx. L.children. ix 0. L.info. L.widgetId
    requests = [SetOverlay widgetId path, SetFocus slWid]

  closeDropdown wenv node = resultReqs newNode requests where
    widgetId = node ^. L.info . L.widgetId
    newState = state {
      _ddsOpen = False,
      _ddsOffset = def
    }
    newNode = node
      & L.widget .~ makeDropdown widgetData items makeMain makeRow config newState
    requests = [ResetOverlay widgetId, SetFocus widgetId]

  handleMessage wenv node target msg =
    cast msg >>= handleLvMsg wenv node

  handleLvMsg wenv node (OnChangeMessage idx _) =
    Seq.lookup idx items >>= \value -> Just $ onChange wenv node idx value
  handleLvMsg wenv node OnListBlur = Just result where
    tempResult = closeDropdown wenv node
    result = tempResult & L.requests %~ (|> createMoveFocusReq wenv)

  onChange wenv node idx item = result where
    WidgetResult newNode reqs = closeDropdown wenv node
    newReqs = Seq.fromList $ widgetDataSet widgetData item
      ++ fmap ($ item) (_ddcOnChangeReq config)
      ++ fmap (\fn -> fn idx item) (_ddcOnChangeIdxReq config)
    evts = RaiseEvent <$> fmap ($ item) (_ddcOnChange config)
    evtsIdx = RaiseEvent <$> fmap (\fn -> fn idx item) (_ddcOnChangeIdx config)
    newEvents = Seq.fromList (evts ++ evtsIdx)
    result = WidgetResult newNode (reqs <> newReqs <> newEvents)

  getSizeReq :: ContainerGetSizeReqHandler s e
  getSizeReq wenv node children = (newReqW, newReqH) where
    -- Main section reqs
    mainC = Seq.index children 0
    mainReqW = mainC ^. L.info . L.sizeReqW
    mainReqH = mainC ^. L.info . L.sizeReqH
    -- List items reqs
    listC = Seq.index children 1
    listReqW = listC ^. L.info . L.sizeReqW
    -- Items other than main could be wider
    -- Height only matters for the selected item, since the rest is in a scroll
    newReqW = sizeReqMergeMax mainReqW listReqW
    newReqH = mainReqH

  listHeight wenv node = maxHeight where
    Size _ winH = _weWindowSize wenv
    theme = activeTheme wenv node
    maxHeightTheme = theme ^. L.dropdownMaxHeight
    cfgMaxHeight = _ddcMaxHeight config
    -- Avoid having an invisible list if style/theme as not set
    maxHeightStyle = max 20 $ fromMaybe maxHeightTheme cfgMaxHeight
    reqHeight = case Seq.lookup 1 (node ^. L.children) of
      Just child -> sizeReqMaxBounded $ child ^. L.info . L.sizeReqH
      _ -> 0
    maxHeight = min winH (min reqHeight maxHeightStyle)

  listOffset wenv node = Point 0 newOffset where
    Size _ winH = _weWindowSize wenv
    viewport = node ^. L.info . L.viewport
    scOffset = wenv ^. L.offset
    Rect rx ry rw rh = moveRect scOffset viewport
    lh = listHeight wenv node
    newOffset
      | ry + rh + lh > winH = - (rh + lh)
      | otherwise = 0

  resize wenv node viewport children = resized where
    Rect rx ry rw rh = viewport
    !mainArea = viewport
    !listArea = viewport {
      _rY = ry + rh,
      _rH = listHeight wenv node
    }
    assignedAreas = Seq.fromList [mainArea, listArea]
    resized = (resultNode node, assignedAreas)

  render wenv node renderer = do
    drawInScissor renderer True viewport $
      drawStyledAction renderer viewport style $ \contentArea -> do
        widgetRender (mainNode ^. L.widget) wenv mainNode renderer
        renderArrow renderer style contentArea

    when isOpen $
      createOverlay renderer $
        drawInTranslation renderer totalOffset $ do
          renderOverlay renderer cwenv listOverlay
    where
      style = activeStyle wenv node
      viewport = node ^. L.info . L.viewport
      mainNode = Seq.index (node ^. L.children) mainIdx
      -- List view is rendered with an offset to accommodate for window height
      listOverlay = Seq.index (node ^. L.children) listIdx
      listOverlayVp = listOverlay ^. L.info . L.viewport
      scOffset = wenv ^. L.offset
      offset = _ddsOffset state
      totalOffset = addPoint scOffset offset
      cwenv = updateWenvOffset container wenv node
        & L.viewport .~ listOverlayVp

  renderArrow renderer style contentArea =
    drawArrowDown renderer arrowRect (_sstFgColor style)
    where
      Rect x y w h = contentArea
      size = style ^. L.text . non def . L.fontSize . non def
      arrowW = unFontSize size / 2
      dh = (h - arrowW) / 2
      arrowRect = Rect (x + w - dh) (y + dh * 1.25) arrowW (arrowW / 2)

  renderOverlay renderer wenv overlayNode = renderAction where
    widget = overlayNode ^. L.widget
    renderAction = widgetRender widget wenv overlayNode renderer

makeSelectList
  :: (DropdownItem a, WidgetEvent e)
  => WidgetEnv s e
  -> WidgetData s a
  -> Seq a
  -> (a -> WidgetNode s e)
  -> DropdownCfg s e a
  -> WidgetId
  -> WidgetNode s e
makeSelectList wenv value items makeRow config widgetId = selectListNode where
  normalTheme = collectTheme wenv L.dropdownItemStyle
  selectedTheme = collectTheme wenv L.dropdownItemSelectedStyle
  itemStyle = fromJust (Just normalTheme <> _ddcItemStyle config)
  itemSelStyle = fromJust (Just selectedTheme <> _ddcItemSelectedStyle config)
  slConfig = [
      selectOnBlur,
      onBlurReq (SendMessage widgetId OnListBlur),
      onChangeIdxReq (\idx it -> SendMessage widgetId (OnChangeMessage idx it)),
      itemNormalStyle itemStyle,
      itemSelectedStyle itemSelStyle
    ]
  slStyle = collectTheme wenv L.dropdownListStyle
  selectListNode = selectListD_ value items makeRow slConfig
    & L.info . L.style .~ slStyle
    & L.info . L.overlay .~ True

createMoveFocusReq :: WidgetEnv s e -> WidgetRequest s e
createMoveFocusReq wenv = MoveFocus Nothing direction where
  direction
    | wenv ^. L.inputStatus . L.keyMod . L.leftShift = FocusBwd
    | otherwise = FocusFwd
