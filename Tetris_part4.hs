module Tetris (main) where
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Control.Monad.Trans (liftIO)
import Control.Monad (forM_)

-- テトリスのブロックを表す型 (EはEmpty, Gはボード周囲の番兵用)
data Block = I | O | S | Z | J | L | T | E | G deriving (Show, Eq)

-- ボードを表す型
type Board = [[Block]]

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

-- メイン関数
main = do
  initGUI
  window <- windowNew

  -- ウインドウクローズイベント処理
  window `on` deleteEvent $ do
    liftIO mainQuit
    return False

  -- 描画イベント処理
  window `on` exposeEvent $ do
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
            foreground = blockColor $ initBoard!!j!!i
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

  -- ウインドウタイトル設定
  windowSetTitle window "Tetris"
  -- ウインドウの大きさを設定(200x400)
  widgetSetSizeRequest window (20*10) (20*20)
  -- ウインドウサイズ変更を許可しない
  windowSetResizable window False
  widgetShowAll window
  mainGUI