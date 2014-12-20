
-- テトリスのブロックを表す型 (EはEmpty, Gはボード周囲の番兵用)
data Block = I | O | S | Z | J | L | T | E | G deriving (Show, Eq)

-- ボードを表す型
type Board = [[Block]]

-- ボードの初期状態
initBoard :: Board
initBoard = [mid | _ <- [1..22]] ++ [last]
  where mid  = [G,E,E,E,E,E,E,E,E,E,E,G]
        last = [G,G,G,G,G,G,G,G,G,G,G,G]