module Tetris (main) where
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Control.Monad.Trans (liftIO)
import Control.Monad (forM_, when)
import Data.Text (unpack)
import Data.IORef
import System.Random (randomR, getStdRandom)

-- ゲーム状態を表す型
data GameStatus = GameStatus {
                    getBoard :: Board,
                    getMino  :: Mino
                  } deriving Show

-- テトリスのブロックを表す型 (EはEmpty, Gはボード周囲の番兵用)
data Block = I | O | S | Z | J | L | T | E | G deriving (Show, Eq)

-- テトリミノを表す型
data Mino = Mino { getBlock     :: Block
                 , getPos       :: Pos
                 , getShape     :: [Pos]
                 , getRotate    :: Int
                 , getRotateMax :: Int
                 } deriving Show

-- テトリミノの移動を表す型
data Move = DOWN | LEFT | RIGHT | ROTATE | NONE deriving (Show, Eq)

-- ボードを表す型
type Board = [[Block]]

-- 位置を表す型
type Pos = (Int, Int)

-- ミノの初期状態
i_Mino = Mino I (5,2) [(0,0),(0,-1),(0,-2),(0,1)] 0 2  -- I-ミノ
o_Mino = Mino O (5,2) [(0,0),(0,1),(1,0),(1,1)]   0 1  -- O-ミノ
s_Mino = Mino S (5,2) [(0,0),(1,0),(0,1),(-1,1)]  0 2  -- S-ミノ
z_Mino = Mino Z (5,2) [(0,0),(-1,0),(0,1),(1,1)]  0 2  -- Z-ミノ
j_Mino = Mino J (5,2) [(0,0),(0,-1),(0,1),(-1,1)] 0 4  -- J-ミノ
l_Mino = Mino L (5,2) [(0,0),(0,-1),(0,1),(1,1)]  0 4  -- L-ミノ
t_Mino = Mino T (5,2) [(0,0),(0,-1),(-1,0),(1,0)] 0 4  -- T-ミノ

-- ボードの初期状態
initBoard :: Board
initBoard = [mid | _ <- [1..22]] ++ [last]
  where mid  = [G,E,E,E,E,E,E,E,E,E,E,G]
        last = [G,G,G,G,G,G,G,G,G,G,G,G]

-- ブロックの色を返す
blockColor :: Block -> Color
blockColor b = case b of
  I -> Color (175*255) (223*255) (228*255) -- aqua
  O -> Color (255*255) (255*255) 0         -- yellow
  S -> Color 0         (255*255) 0         -- green
  Z -> Color (255*255) 0         0         -- red
  J -> Color 0         0         (255*255) -- blue
  L -> Color (243*255) (152*255) 0         -- orange
  T -> Color (167*255) (87*255)  (168*255) -- purple
  E -> Color (180*255) (180*255) (180*255) -- gray
  G -> Color 0         0         0         -- black

-- リストの位置を指定して新しい値に置き換える
replace :: [a] -> Int -> a -> [a]
replace xs n v = ys ++ [v] ++ zs
  where ys = take n xs
        zs = tail $ drop n xs

-- ボードの位置を指定して新しいブロックで置き換える
putBlock :: Block -> Pos -> Board -> Board
putBlock b (x,y) board = board'
  where xs = replace (board!!y) x b
        board' = replace board y xs

-- ボードの複数の位置を指定して新しいブロックで置き換える
putBlocks :: Block -> [Pos] -> Board -> Board
putBlocks b ps board = foldr (putBlock b) board ps

-- 位置リストを回転する
rotate :: [Pos] -> Int -> [Pos]
rotate xs 0 = xs
rotate xs n = rotate xs' (n-1)
  where xs' = map (\(x,y) -> (-y,x)) xs

-- テトリミノの各ブロックの具体的な位置を計算する
getPosList :: Mino -> [Pos]
getPosList mino = map (\(sx,sy) -> (sx+x,sy+y)) shape
  where (x,y) = getPos mino
        r =  getRotate mino `mod` getRotateMax mino
        shape = rotate (getShape mino) r

-- ボードにテトリミノを置く
putMino :: Mino -> Board -> Board
putMino mino = putBlocks (getBlock mino) (getPosList mino)

-- テトリミノを移動する
moveMino :: Move -> Mino -> Mino
moveMino move mino = case move of
    DOWN   -> mino {getPos = (x,y+1)}
    LEFT   -> mino {getPos = (x-1,y)}
    RIGHT  -> mino {getPos = (x+1,y)}
    ROTATE -> mino {getRotate = r+1}
    NONE   -> mino
  where (x,y) = getPos mino
        r = getRotate mino

-- テトリミノを配置可能か判定する
canPut :: Board -> Mino -> Bool
canPut board mino = all check (getPosList mino)
  where check (x,y) = board!!y!!x == E

-- テトリミノをランダムに選択する
getRandomMino :: IO Mino
getRandomMino = do
  i <- getStdRandom $ randomR (0,6)
  return $ [i_Mino,o_Mino,s_Mino,z_Mino,j_Mino,l_Mino,t_Mino]!!i

-- テトリミノの自動落下
autoDown :: IORef GameStatus -> IO ()
autoDown gameStatusRef = do
  -- 現在のボードとテトリミノを取得する
  gameStatus <- readIORef gameStatusRef
  let board = getBoard gameStatus
  let mino = getMino gameStatus

  -- テトリミノを落下させる
  let downMino = moveMino DOWN mino

  if canPut board downMino then
    -- ボードに置ける場合、落下テトリミノを現在のテトリミノとして確定する
    writeIORef gameStatusRef gameStatus { getMino = downMino }
  else do
    -- ボードに置けない場合、落下前のテトリミノを固定化したボードを作る
    let fixedBoard = putMino mino board
    -- 新しいテトリミノをランダムに選択する
    newMino <- getRandomMino
    -- 現在のボードを更新し、新しいテトリミノを出現させる
    writeIORef gameStatusRef gameStatus { getBoard = fixedBoard
                                        , getMino = newMino }

-- キーボード操作を取得する
getMove :: EventM EKey Move
getMove = do
  keyval <- eventKeyVal
  let move = case unpack (keyName keyval) of
               "Down"  -> DOWN
               "Left"  -> LEFT
               "Right" -> RIGHT
               "Up"    -> ROTATE
               _       -> NONE
  return move

-- ボードを描画するイベントハンドラ
updateWindow :: Board -> EventM EExpose Bool
updateWindow board = do
  win <- eventWindow
  liftIO $ do
    gc <- gcNew win

    -- 2重ループで各ブロックを描画する。以下のループインデックスに注意
    -- iは1〜10 (0と11は番兵なので表示しない)
    -- jは2〜21 (0と1は見えない領域、22は番兵なので表示しない)
    forM_ [1..10] $ \i -> do
      forM_ [2..21] $ \j -> do

        -- ブロックの描画x,y座標を求める(20はブロックの大きさ)
        let w = 20
        let h = 20
        let x = w * (i-1)
        let y = h * (j-2)

        -- ブロックの色をgcに設定する
        gcSetValues gc newGCValues {
          foreground = blockColor $ board!!j!!i
        }
        -- ブロックを描画する(塗りつぶし)
        drawRectangle win gc True x y w h
        -- ブロックの枠線色(白)をgcに設定する
        gcSetValues gc newGCValues {
          foreground = Color 65535 65535 65535
        }
        -- ブロックの枠線を少し内側に描画する
        drawRectangle win gc False x y (w-2) (h-2)
  return True

-- メイン関数
main = do
  -- ゲーム状態の初期値を生成
  gameStatusRef <- newIORef $ GameStatus {
                                getBoard = initBoard,
                                getMino  = t_Mino
                              }
  initGUI
  window <- windowNew

  -- ウインドウクローズイベント処理
  window `on` deleteEvent $ do
    liftIO mainQuit
    return False

  -- 描画イベント処理
  window `on` exposeEvent $ do
    gameStatus <- liftIO $ readIORef gameStatusRef
    -- テトリミノをボードに置く
    let board = putMino (getMino gameStatus) (getBoard gameStatus)
    -- ボードを描画する
    updateWindow board

  -- キーボード操作イベント処理
  window `on` keyPressEvent $ do
    move <- getMove
    liftIO $ do
      gameStatus <- readIORef gameStatusRef
      -- 移動後の新しいテトリミノを取得
      let newMino = moveMino move (getMino gameStatus)
      -- 新しいテトリミノを配置できる場合にゲーム状態を更新
      when (canPut (getBoard gameStatus) newMino) $
         writeIORef gameStatusRef $ gameStatus { getMino = newMino }
      -- 描画イベントをキック
      widgetQueueDraw window
    return True

  -- タイムアウトイベント処理(1秒間隔)
  timeoutAdd (do
    autoDown gameStatusRef -- 自動落下
    widgetQueueDraw window -- 描画イベントをキック
    return True) 1000

  -- ウインドウタイトル設定
  windowSetTitle window "Tetris"
  -- ウインドウの大きさを設定(200x400)
  widgetSetSizeRequest window (20*10) (20*20)
  -- ウインドウサイズ変更を許可しない
  windowSetResizable window False
  widgetShowAll window
  mainGUI
