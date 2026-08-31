----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE CPP               #-}
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Control.Concurrent (threadDelay)
import           Control.Monad (forever)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import           Data.List (foldl')
import           Data.Maybe (isNothing)
----------------------------------------------------------------------------
import           Miso hiding ((!!), Phase)
import qualified Miso.Html as H
import qualified Miso.Html.Property as HP
import qualified Miso.Svg as SV
import qualified Miso.Svg.Property as SP
import qualified Miso.CSS as CSS
import           Miso.String (MisoString, ms)
import           Miso.Subscription.Keyboard (keyboardSub)
----------------------------------------------------------------------------
-- Board constants
boardW, boardH, cellSz :: Int
boardW = 10
boardH = 20
cellSz = 30

previewSz :: Int
previewSz = 24
----------------------------------------------------------------------------
-- Types
data TColor
  = TCyan | TYellow | TPurple | TGreen | TRed | TBlue | TOrange
  deriving (Show, Eq)

data PType
  = IPiece | OPiece | TPiece | SPiece | ZPiece | JPiece | LPiece
  deriving (Show, Eq, Enum, Bounded)

data Piece = Piece
  { pType :: PType
  , pX    :: Int
  , pY    :: Int
  , pRot  :: Int
  } deriving (Show, Eq)

data Phase = Playing | Paused | GameOver
  deriving (Show, Eq)

type Board = [[Maybe TColor]]

data Model = Model
  { mBoard      :: Board
  , mCur        :: Maybe Piece
  , mNext       :: PType
  , mScore      :: Int
  , mLevel      :: Int
  , mLines      :: Int
  , mPhase      :: Phase
  , mTick       :: Int
  , mHeldKeys   :: IntSet
  , mKeyTimers  :: IntMap Int
  , mRng        :: Int
  } deriving (Show, Eq)

data Action
  = Tick
  | Keys IntSet
  | Restart
  | TouchLeft
  | TouchRight
  | TouchDown
  | TouchRotate
  | TouchDrop
  deriving (Show, Eq)
----------------------------------------------------------------------------
main :: IO ()
#ifdef INTERACTIVE
main = live defaultEvents app
#else
main = startApp defaultEvents app
#endif
----------------------------------------------------------------------------
#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif
----------------------------------------------------------------------------
app :: App Model Action
app = (component initModel updateModel viewModel)
  { subs = [keyboardSub Keys, tickSub] }
----------------------------------------------------------------------------
tickSub :: Sub model Action
tickSub sink = forever $ threadDelay 50000 >> sink Tick
----------------------------------------------------------------------------
-- Pure LCG for randomness
nextRng :: Int -> (Int, Int)
nextRng seed =
  let s = abs ((seed * 1664525 + 1013904223) `mod` 0x7fffffff)
  in (s `mod` 7, s)

randPType :: Int -> (PType, Int)
randPType rng =
  let (n, rng') = nextRng rng
  in ([IPiece, OPiece, TPiece, SPiece, ZPiece, JPiece, LPiece] !! n, rng')
----------------------------------------------------------------------------
emptyBoard :: Board
emptyBoard = replicate boardH (replicate boardW Nothing)

mkModel :: Int -> Model
mkModel seed =
  let (next, rng1) = randPType seed
      (pt,   rng2) = randPType rng1
  in Model
       { mBoard      = emptyBoard
       , mCur        = Just (spawnPiece pt)
       , mNext       = next
       , mScore      = 0
       , mLevel      = 1
       , mLines      = 0
       , mPhase      = Playing
       , mTick       = 0
       , mHeldKeys   = IS.empty
       , mKeyTimers  = IM.empty
       , mRng        = rng2
       }

initModel :: Model
initModel = mkModel 42
----------------------------------------------------------------------------
-- Piece shapes: (row, col) offsets from (pY, pX)
pieceShape :: PType -> Int -> [(Int, Int)]
pieceShape IPiece 0 = [(1,0),(1,1),(1,2),(1,3)]
pieceShape IPiece 1 = [(0,2),(1,2),(2,2),(3,2)]
pieceShape IPiece 2 = [(2,0),(2,1),(2,2),(2,3)]
pieceShape IPiece 3 = [(0,1),(1,1),(2,1),(3,1)]
pieceShape OPiece _ = [(0,1),(0,2),(1,1),(1,2)]
pieceShape TPiece 0 = [(0,1),(1,0),(1,1),(1,2)]
pieceShape TPiece 1 = [(0,1),(1,1),(1,2),(2,1)]
pieceShape TPiece 2 = [(1,0),(1,1),(1,2),(2,1)]
pieceShape TPiece 3 = [(0,1),(1,0),(1,1),(2,1)]
pieceShape SPiece 0 = [(0,1),(0,2),(1,0),(1,1)]
pieceShape SPiece 1 = [(0,1),(1,1),(1,2),(2,2)]
pieceShape SPiece 2 = [(1,1),(1,2),(2,0),(2,1)]
pieceShape SPiece 3 = [(0,0),(1,0),(1,1),(2,1)]
pieceShape ZPiece 0 = [(0,0),(0,1),(1,1),(1,2)]
pieceShape ZPiece 1 = [(0,2),(1,1),(1,2),(2,1)]
pieceShape ZPiece 2 = [(1,0),(1,1),(2,1),(2,2)]
pieceShape ZPiece 3 = [(0,1),(1,0),(1,1),(2,0)]
pieceShape JPiece 0 = [(0,0),(1,0),(1,1),(1,2)]
pieceShape JPiece 1 = [(0,1),(0,2),(1,1),(2,1)]
pieceShape JPiece 2 = [(1,0),(1,1),(1,2),(2,2)]
pieceShape JPiece 3 = [(0,1),(1,1),(2,0),(2,1)]
pieceShape LPiece 0 = [(0,2),(1,0),(1,1),(1,2)]
pieceShape LPiece 1 = [(0,1),(1,1),(2,1),(2,2)]
pieceShape LPiece 2 = [(1,0),(1,1),(1,2),(2,0)]
pieceShape LPiece 3 = [(0,0),(0,1),(1,1),(2,1)]
pieceShape _      r = pieceShape IPiece (r `mod` 4)

pieceCells :: Piece -> [(Int, Int)]
pieceCells p =
  [ (pY p + dr, pX p + dc)
  | (dr, dc) <- pieceShape (pType p) (pRot p `mod` 4)
  ]

pieceColorHex :: PType -> MisoString
pieceColorHex IPiece = "#00f0f0"
pieceColorHex OPiece = "#f0f000"
pieceColorHex TPiece = "#a000f0"
pieceColorHex SPiece = "#00d000"
pieceColorHex ZPiece = "#f00000"
pieceColorHex JPiece = "#2020f0"
pieceColorHex LPiece = "#f0a000"

pieceToColor :: PType -> TColor
pieceToColor IPiece = TCyan
pieceToColor OPiece = TYellow
pieceToColor TPiece = TPurple
pieceToColor SPiece = TGreen
pieceToColor ZPiece = TRed
pieceToColor JPiece = TBlue
pieceToColor LPiece = TOrange

colorHex :: TColor -> MisoString
colorHex TCyan   = "#00f0f0"
colorHex TYellow = "#f0f000"
colorHex TPurple = "#a000f0"
colorHex TGreen  = "#00d000"
colorHex TRed    = "#f00000"
colorHex TBlue   = "#2020f0"
colorHex TOrange = "#f0a000"
----------------------------------------------------------------------------
spawnPiece :: PType -> Piece
spawnPiece pt = Piece { pType = pt, pX = 3, pY = 0, pRot = 0 }

isValid :: Board -> Piece -> Bool
isValid board p = all cellOk (pieceCells p)
  where
    cellOk (r, c) =
      c >= 0 && c < boardW &&
      r < boardH &&
      (r < 0 || isNothing (board !! r !! c))

lockPiece :: Board -> Piece -> Board
lockPiece board p =
  foldl' setCell board (pieceCells p)
  where
    col = pieceToColor (pType p)
    setCell b (r, c)
      | r < 0    = b
      | otherwise =
          let (top, row:bot) = splitAt r b
              (left, _:right) = splitAt c row
          in top ++ (left ++ Just col : right) : bot

clearLines :: Board -> (Board, Int)
clearLines board =
  let kept    = filter (any isNothing) board
      cleared = boardH - length kept
      newRows = replicate cleared (replicate boardW Nothing)
  in (newRows ++ kept, cleared)

lineScore :: Int -> Int -> Int
lineScore lvl n = lvl * case n of
  1 -> 100; 2 -> 300; 3 -> 500; 4 -> 800; _ -> 0

ticksPerDrop :: Int -> Int
ticksPerDrop lvl = max 1 (12 - lvl)

hardDrop :: Board -> Piece -> Piece
hardDrop board p =
  let p' = p { pY = pY p + 1 }
  in if isValid board p' then hardDrop board p' else p
----------------------------------------------------------------------------
-- Update
updateModel :: Action -> Effect context props Model Action
updateModel = \case

  Restart -> modify $ \m -> mkModel (mRng m * 6364136 + 1442695)

  Keys newKeys -> modify $ \m ->
    let prev        = mHeldKeys m
        justPressed = IS.difference newKeys prev
        m1          = m { mHeldKeys = newKeys }
    in case mPhase m1 of
         GameOver ->
           if IS.member 82 justPressed  -- R to restart
             then mkModel (mRng m * 6364136 + 1442695)
             else m1
         Paused ->
           if IS.member 82 justPressed then mkModel (mRng m * 6364136 + 1442695)
           else if IS.member 80 justPressed then m1 { mPhase = Playing }
           else m1
         Playing ->
           foldl' applyInstantKey m1 (IS.toList justPressed)

  Tick -> modify $ \m ->
    case mPhase m of
      Playing -> processTick m
      _       -> m

  TouchLeft   -> modify $ whenPlaying $ \m -> tryMove m (-1) 0
  TouchRight  -> modify $ whenPlaying $ \m -> tryMove m 1 0
  TouchDown   -> modify $ whenPlaying $ \m -> tryMove m 0 1
  TouchRotate -> modify $ whenPlaying tryRotate
  TouchDrop   -> modify $ whenPlaying doHardDrop

whenPlaying :: (Model -> Model) -> Model -> Model
whenPlaying f m
  | mPhase m == Playing = f m
  | otherwise           = m

applyInstantKey :: Model -> Int -> Model
applyInstantKey m = \case
  32 -> doHardDrop m
  80 -> m { mPhase = Paused }
  82 -> mkModel (mRng m * 6364136 + 1442695)
  _  -> m

tryMove :: Model -> Int -> Int -> Model
tryMove m dx dy = case mCur m of
  Nothing -> m
  Just p  ->
    let p' = p { pX = pX p + dx, pY = pY p + dy }
    in if isValid (mBoard m) p' then m { mCur = Just p' } else m

tryRotate :: Model -> Model
tryRotate m = case mCur m of
  Nothing -> m
  Just p  ->
    let r' = (pRot p + 1) `mod` 4
        rotated = p { pRot = r' }
        kicks = [rotated, rotated { pX = pX rotated + 1 }, rotated { pX = pX rotated - 1 }]
        valid = filter (isValid (mBoard m)) kicks
    in case valid of
         (p':_) -> m { mCur = Just p' }
         []     -> m

doHardDrop :: Model -> Model
doHardDrop m = case mCur m of
  Nothing -> m
  Just p  -> lockAndSpawn m (hardDrop (mBoard m) p)

-- DAS: 6 ticks (~300ms) initial delay, then auto-repeat
dasDelay :: Int
dasDelay = 6

-- True on tick 1 (immediate) and then every `arr` ticks after the DAS delay
firesMove :: Int -> Int -> Bool
firesMove arr t = t == 1 || (t > dasDelay && (t - dasDelay - 1) `mod` arr == 0)

processTick :: Model -> Model
processTick m =
  let keys      = mHeldKeys m
      oldTimers = mKeyTimers m
      newTimers = IM.fromList
        [ (k, 1 + IM.findWithDefault 0 k oldTimers)
        | k <- IS.toList keys
        ]
      fire arr k = maybe False (firesMove arr) (IM.lookup k newTimers)
      m1 = if fire    2 37 then tryMove   m  (-1) 0 else m   -- left:   100ms ARR
      m2 = if fire    2 39 then tryMove   m1   1  0 else m1  -- right:  100ms ARR
      m3 = if fire    1 40 then tryMove   m2   0  1 else m2  -- down:    50ms ARR
      m4 = if fire    2 38 then tryRotate m3        else m3  -- rotate: 100ms ARR
      m5 = m4 { mKeyTimers = newTimers }
      t  = mTick m5 + 1
  in if t < ticksPerDrop (mLevel m5)
       then m5 { mTick = t }
       else case mCur m5 of
              Nothing -> spawnNext m5 { mTick = 0 }
              Just p  ->
                let p' = p { pY = pY p + 1 }
                in if isValid (mBoard m5) p'
                     then m5 { mCur = Just p', mTick = 0 }
                     else lockAndSpawn m5 p

lockAndSpawn :: Model -> Piece -> Model
lockAndSpawn m p =
  let board1       = lockPiece (mBoard m) p
      (board2, n)  = clearLines board1
      score'       = mScore m + lineScore (mLevel m) n
      lines'       = mLines m + n
      level'       = 1 + lines' `div` 10
      (next', rng') = randPType (mRng m)
      newPiece     = spawnPiece (mNext m)
  in if not (isValid board2 newPiece)
       then m { mBoard = board2, mCur = Nothing, mPhase = GameOver
              , mScore = score', mLines = lines', mLevel = level' }
       else m { mBoard  = board2
              , mCur    = Just newPiece
              , mNext   = next'
              , mScore  = score'
              , mLines  = lines'
              , mLevel  = level'
              , mTick   = 0
              , mRng    = rng'
              }

spawnNext :: Model -> Model
spawnNext m =
  let (next', rng') = randPType (mRng m)
      newPiece      = spawnPiece (mNext m)
  in if not (isValid (mBoard m) newPiece)
       then m { mPhase = GameOver }
       else m { mCur = Just newPiece, mNext = next', mRng = rng' }
----------------------------------------------------------------------------
-- View
viewModel :: context -> props -> Model -> View context Model Action
viewModel _ _ m =
  H.div_
    [ HP.class_ "game-root" ]
    [ H.h1_
        [ HP.class_ "game-title" ]
        [ H.a_
            [ HP.href_ "https://github.com/haskell-miso/miso-tetris"
            , CSS.style_
                [ CSS.color (CSS.hex "00f0f0")
                , CSS.textShadow "0 0 20px #00f0f0"
                , CSS.textDecoration "none"
                ]
            ]
            [ text "miso tetris \x1F35C" ]
        ]
    , H.div_
        [ HP.class_ "game-main" ]
        [ viewBoard m
        , viewPanel m
        ]
    , touchControls
    , H.div_
        [ HP.class_ "help-text" ]
        [ text "\8592\8594 move  \8593 rotate  \8595 soft drop  SPC hard drop  P pause  R restart" ]
    ]

viewBoard :: Model -> View context Model Action
viewBoard m =
  let w     = boardW * cellSz
      h     = boardH * cellSz
      ghost = case (mPhase m, mCur m) of
                (Playing, Just p) -> Just (hardDrop (mBoard m) p)
                _                 -> Nothing
  in SV.svg_
       [ HP.width_  (ms w)
       , HP.height_ (ms h)
       , SP.viewBox_ ("0 0 " <> ms w <> " " <> ms h)
       , HP.class_ "board-svg"
       ]
       ( bgCells
      ++ boardCells (mBoard m)
      ++ maybe [] ghostCells ghost
      ++ maybe [] activeCells (mCur m)
      ++ overlayViews m
       )

bgCells :: [View context Model Action]
bgCells =
  [ SV.rect_
      [ SP.x_ (ms (c * cellSz))
      , SP.y_ (ms (r * cellSz))
      , HP.width_  (ms cellSz)
      , HP.height_ (ms cellSz)
      , SP.fill_ (if even (r + c) then "#1c1c3c" else "#232350")
      , SP.stroke_ "#2e2e60"
      , SP.strokeWidth_ "1"
      ]
  | r <- [0..boardH-1], c <- [0..boardW-1]
  ]

boardCells :: Board -> [View context Model Action]
boardCells board =
  [ SV.rect_
      [ SP.x_ (ms (c * cellSz + 1))
      , SP.y_ (ms (r * cellSz + 1))
      , HP.width_  (ms (cellSz - 2))
      , HP.height_ (ms (cellSz - 2))
      , SP.fill_        (colorHex col)
      , SP.stroke_      (colorHex col <> "cc")
      , SP.strokeWidth_ "1"
      , SP.rx_          "3"
      ]
  | (r, row) <- zip [0..] board
  , (c, Just col) <- zip [0..] row
  ]

ghostCells :: Piece -> [View context Model Action]
ghostCells p =
  [ SV.rect_
      [ SP.x_ (ms (c * cellSz + 3))
      , SP.y_ (ms (r * cellSz + 3))
      , HP.width_  (ms (cellSz - 6))
      , HP.height_ (ms (cellSz - 6))
      , SP.fill_        "none"
      , SP.stroke_      (pieceColorHex (pType p))
      , SP.strokeWidth_ "2"
      , SP.opacity_     "0.55"
      , SP.rx_          "2"
      ]
  | (r, c) <- pieceCells p, r >= 0
  ]

activeCells :: Piece -> [View context Model Action]
activeCells p =
  [ SV.rect_
      [ SP.x_ (ms (c * cellSz + 1))
      , SP.y_ (ms (r * cellSz + 1))
      , HP.width_  (ms (cellSz - 2))
      , HP.height_ (ms (cellSz - 2))
      , SP.fill_        (pieceColorHex (pType p))
      , SP.stroke_      "#ffffff88"
      , SP.strokeWidth_ "1"
      , SP.rx_          "3"
      ]
  | (r, c) <- pieceCells p, r >= 0
  ]

overlayViews :: Model -> [View context Model Action]
overlayViews m =
  let w = boardW * cellSz
      h = boardH * cellSz
  in case mPhase m of
       GameOver ->
         [ SV.rect_
             [ SP.x_ "0", SP.y_ "0"
             , HP.width_ (ms w), HP.height_ (ms h)
             , SP.fill_ "rgba(0,0,0,0.72)"
             ]
         , svgLabel (ms (w `div` 2)) (ms (h `div` 2 - 22)) "28" "#ff4444" "GAME OVER"
         , svgLabel (ms (w `div` 2)) (ms (h `div` 2 + 14)) "15" "#8888aa" "Press R to restart"
         ]
       Paused ->
         [ SV.rect_
             [ SP.x_ "0", SP.y_ "0"
             , HP.width_ (ms w), HP.height_ (ms h)
             , SP.fill_ "rgba(0,0,0,0.62)"
             ]
         , svgLabel (ms (w `div` 2)) (ms (h `div` 2)) "28" "#00f0f0" "PAUSED"
         ]
       Playing -> []

svgLabel :: MisoString -> MisoString -> MisoString -> MisoString -> MisoString -> View context Model Action
svgLabel x y sz col txt =
  SV.text_
    [ SP.x_ x, SP.y_ y
    , SP.textAnchor_ "middle"
    , SP.fill_       col
    , SP.fontSize_   sz
    , SP.fontWeight_ "bold"
    , SP.fontFamily_ "'Courier New', monospace"
    ]
    [ text txt ]

viewPanel :: Model -> View context Model Action
viewPanel m =
  H.div_
    [ HP.class_ "side-panel" ]
    [ infoCard "NEXT"  (nextPieceView (mNext m))
    , infoCard "SCORE" (statLabel (ms (mScore m)))
    , infoCard "LEVEL" (statLabel (ms (mLevel m)))
    , infoCard "LINES" (statLabel (ms (mLines m)))
    , restartBtn
    ]

infoCard :: MisoString -> View context Model Action -> View context Model Action
infoCard label inner =
  H.div_
    [ CSS.style_
        [ CSS.background "#1c1c3a"
        , CSS.border "1px solid #3a3a70"
        , CSS.borderRadius "6px"
        , CSS.padding "10px 14px"
        , CSS.boxShadow "0 2px 12px rgba(0,0,0,0.5)"
        ]
    ]
    [ H.p_
        [ CSS.style_
            [ CSS.fontSize "0.65rem"
            , CSS.letterSpacing "0.2em"
            , CSS.color (CSS.hex "9999cc")
            , CSS.margin "0 0 6px 0"
            ]
        ]
        [ text label ]
    , inner
    ]

statLabel :: MisoString -> View context Model Action
statLabel val =
  H.p_
    [ CSS.style_
        [ CSS.fontSize (CSS.rem 1.5)
        , CSS.fontWeight "bold"
        , CSS.color (CSS.hex "00f0f0")
        , CSS.margin "0"
        , CSS.textShadow "0 0 12px rgba(0,240,240,0.7)"
        ]
    ]
    [ text val ]

nextPieceView :: PType -> View context Model Action
nextPieceView pt =
  let preview   = Piece { pType = pt, pX = 0, pY = 0, pRot = 0 }
      cells     = pieceCells preview
      maxR      = maximum (fmap fst cells) + 1
      maxC      = maximum (fmap snd cells) + 1
      svgW      = maxC * previewSz
      svgH      = maxR * previewSz
  in SV.svg_
       [ HP.width_  (ms svgW)
       , HP.height_ (ms svgH)
       , SP.viewBox_ ("0 0 " <> ms svgW <> " " <> ms svgH)
       ]
       [ SV.rect_
           [ SP.x_ (ms (c * previewSz + 1))
           , SP.y_ (ms (r * previewSz + 1))
           , HP.width_  (ms (previewSz - 2))
           , HP.height_ (ms (previewSz - 2))
           , SP.fill_        (pieceColorHex pt)
           , SP.stroke_      "#ffffff33"
           , SP.strokeWidth_ "1"
           , SP.rx_          "2"
           ]
       | (r, c) <- cells
       ]

-- On-screen controls, shown on touch devices via CSS (pointer: coarse)
touchControls :: View context Model Action
touchControls =
  H.div_
    [ HP.class_ "touch-controls" ]
    [ touchBtn "\8592"  TouchLeft    -- ←
    , touchBtn "\8595"  TouchDown    -- ↓
    , touchBtn "\8594"  TouchRight   -- →
    , touchBtn "\10227" TouchRotate  -- ⟳
    , touchBtn "\10515" TouchDrop    -- ⤓
    ]

touchBtn :: MisoString -> Action -> View context Model Action
touchBtn label act =
  H.button_
    [ SV.onClick act
    , HP.class_ "touch-btn"
    ]
    [ text label ]

restartBtn :: View context Model Action
restartBtn =
  H.button_
    [ SV.onClick Restart
    , CSS.style_
        [ CSS.background "#1c1c3a"
        , CSS.border "1px solid #3a3a70"
        , CSS.borderRadius "6px"
        , CSS.color (CSS.hex "bbbbdd")
        , CSS.padding "10px"
        , CSS.fontSize (CSS.rem 0.8)
        , CSS.letterSpacing "0.1em"
        , CSS.cursor "pointer"
        , CSS.width "100%"
        , CSS.fontFamily "'Courier New',monospace"
        ]
    ]
    [ text "RESTART" ]
----------------------------------------------------------------------------
