import Data.Char (isNumber)

type Location = (Char, Int)

data Player = White | Black deriving (Show, Eq)

data Piece = P Location | N Location | K Location | Q Location | R Location | B Location deriving (Show, Eq)

type Board = (Player, [Piece], [Piece])

-- a) setBoard

setBoard :: Board
setBoard =
  ( White,
    [ R ('h', 1),
      N ('g', 1),
      B ('f', 1),
      K ('e', 1),
      Q ('d', 1),
      B ('c', 1),
      N ('b', 1),
      R ('a', 1),
      P ('h', 2),
      P ('g', 2),
      P ('f', 2),
      P ('e', 2),
      P ('d', 2),
      P ('c', 2),
      P ('b', 2),
      P ('a', 2)
    ],
    [ R ('h', 8),
      N ('g', 8),
      B ('f', 8),
      K ('e', 8),
      Q ('d', 8),
      B ('c', 8),
      N ('b', 8),
      R ('a', 8),
      P ('h', 7),
      P ('g', 7),
      P ('f', 7),
      P ('e', 7),
      P ('d', 7),
      P ('c', 7),
      P ('b', 7),
      P ('a', 7)
    ]
  )

-- b) visualizeBoard

generateBoardList =
  [ ["8", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["7", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["6", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["5", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["4", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["3", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["2", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "],
    ["1", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  "]
  ]

stringToInt :: String -> Int
stringToInt = read

tra :: Char -> Int
tra c | c == 'a' = 1 | c == 'b' = 2 | c == 'c' = 3 | c == 'd' = 4 | c == 'e' = 5 | c == 'f' = 6 | c == 'g' = 7 | c == 'h' = 8

replace :: Char -> Piece -> [[String]] -> [[String]]
replace c (P (col, row)) ((h : t1) : t)
  | row == stringToInt h = replaceHelper c (P (col, row)) (h : t1) (tra col) : t
  | otherwise = (h : t1) : replace c (P (col, row)) t
replace c (N (col, row)) ((h : t1) : t)
  | row == stringToInt h = replaceHelper c (N (col, row)) (h : t1) (tra col) : t
  | otherwise = (h : t1) : replace c (N (col, row)) t
replace c (K (col, row)) ((h : t1) : t)
  | row == stringToInt h = replaceHelper c (K (col, row)) (h : t1) (tra col) : t
  | otherwise = (h : t1) : replace c (K (col, row)) t
replace c (Q (col, row)) ((h : t1) : t)
  | row == stringToInt h = replaceHelper c (Q (col, row)) (h : t1) (tra col) : t
  | otherwise = (h : t1) : replace c (Q (col, row)) t
replace c (R (col, row)) ((h : t1) : t)
  | row == stringToInt h = replaceHelper c (R (col, row)) (h : t1) (tra col) : t
  | otherwise = (h : t1) : replace c (R (col, row)) t
replace c (B (col, row)) ((h : t1) : t)
  | row == stringToInt h = replaceHelper c (B (col, row)) (h : t1) (tra col) : t
  | otherwise = (h : t1) : replace c (B (col, row)) t

replaceHelper :: Char -> Piece -> [String] -> Int -> [String]
replaceHelper c (P (col, row)) (h : t) n
  | n == 0 = ("P" ++ [c]) : t
  | otherwise = h : replaceHelper c (P (col, row)) t (n - 1)
replaceHelper c (N (col, row)) (h : t) n
  | n == 0 = ("N" ++ [c]) : t
  | otherwise = h : replaceHelper c (N (col, row)) t (n - 1)
replaceHelper c (K (col, row)) (h : t) n
  | n == 0 = ("K" ++ [c]) : t
  | otherwise = h : replaceHelper c (K (col, row)) t (n - 1)
replaceHelper c (Q (col, row)) (h : t) n
  | n == 0 = ("Q" ++ [c]) : t
  | otherwise = h : replaceHelper c (Q (col, row)) t (n - 1)
replaceHelper c (R (col, row)) (h : t) n
  | n == 0 = ("R" ++ [c]) : t
  | otherwise = h : replaceHelper c (R (col, row)) t (n - 1)
replaceHelper c (B (col, row)) (h : t) n
  | n == 0 = ("B" ++ [c]) : t
  | otherwise = h : replaceHelper c (B (col, row)) t (n - 1)

updateBoardList :: [[String]] -> [Piece] -> [Piece] -> [[String]]
updateBoardList boardList [] [] = boardList
updateBoardList boardList [] (bh : bt) = replace 'B' bh (updateBoardList boardList [] bt)
updateBoardList boardList (wh : wt) blackPieces = replace 'W' wh (updateBoardList boardList wt blackPieces)

visualizeHelper :: [[String]] -> String
visualizeHelper [] = ""
visualizeHelper ([] : t) = visualizeHelper t
visualizeHelper ((h : t1) : t)
  | h == "8" || h == "7" || h == "6" || h == "5" || h == "4" || h == "3" || h == "2" || h == "1" = "\n" ++ h ++ "|" ++ visualizeHelper (t1 : t)
  | otherwise = h ++ "|" ++ visualizeHelper (t1 : t)

visualizeBoard (player, whitePieces, blackPieces) =
  putStr
    ( "  a  b  c  d  e  f  g  h"
        ++ visualizeHelper (updateBoardList generateBoardList whitePieces blackPieces)
        ++ "\n\nTurn: "
        ++ show player
    )

-- c) isLegal

isLegal piece (player, whitePieces, blackPieces) location
  | getPlayer piece whitePieces blackPieces == White =
      checkInBoard location
        && checkNotOccupied location whitePieces
        && checkPieceMove whitePieces blackPieces White piece location
        && checkInGame piece whitePieces
  | getPlayer piece whitePieces blackPieces == Black =
      checkInBoard location
        && checkNotOccupied location blackPieces
        && checkPieceMove whitePieces blackPieces Black piece location
        && checkInGame piece blackPieces
  | otherwise = False

getPlayer piece whitePieces blackPieces = if piece `elem` whitePieces then White else Black

checkInGame piece pieces = elem piece pieces

checkInBoard (col, row)
  | col < 'a' || col > 'h' || row < 1 || row > 8 = False
  | otherwise = True

checkNotOccupied (col, row) [] = True
checkNotOccupied (col, row) ((P (col1, row1)) : t)
  | col == col1 && row == row1 = False
  | otherwise = checkNotOccupied (col, row) t
checkNotOccupied (col, row) ((N (col1, row1)) : t)
  | col == col1 && row == row1 = False
  | otherwise = checkNotOccupied (col, row) t
checkNotOccupied (col, row) ((K (col1, row1)) : t)
  | col == col1 && row == row1 = False
  | otherwise = checkNotOccupied (col, row) t
checkNotOccupied (col, row) ((Q (col1, row1)) : t)
  | col == col1 && row == row1 = False
  | otherwise = checkNotOccupied (col, row) t
checkNotOccupied (col, row) ((R (col1, row1)) : t)
  | col == col1 && row == row1 = False
  | otherwise = checkNotOccupied (col, row) t
checkNotOccupied (col, row) ((B (col1, row1)) : t)
  | col == col1 && row == row1 = False
  | otherwise = checkNotOccupied (col, row) t

checkPieceMove wp bp White (P (col1, 2)) (col2, row2)
  | row2 == 3 && col1 == col2 = checkNotOccupied (col2, 3) bp && checkNotOccupied (col2, 3) wp
  | row2 == 3 && ((tra col1) + 1 == tra col2 || (tra col1) - 1 == tra col2) && not (checkNotOccupied (col2, row2) bp) = True
  | row2 == 4 && col1 == col2 = checkNotOccupied (col2, 3) bp && checkNotOccupied (col2, 3) wp && checkNotOccupied (col2, 4) bp && checkNotOccupied (col2, 4) wp
checkPieceMove wp bp Black (P (col1, 7)) (col2, row2)
  | row2 == 6 && col1 == col2 = checkNotOccupied (col2, 6) bp && checkNotOccupied (col2, 6) wp
  | row2 == 6 && ((tra col1) + 1 == tra col2 || (tra col1) - 1 == tra col2) && not (checkNotOccupied (col2, row2) wp) = True
  | row2 == 5 && col1 == col2 = checkNotOccupied (col2, 6) wp && checkNotOccupied (col2, 6) bp && checkNotOccupied (col2, 5) wp && checkNotOccupied (col2, 5) bp
checkPieceMove wp bp White (P (col1, row1)) (col2, row2)
  | row2 == row1 + 1 && tra col2 == tra col1 && checkNotOccupied (col2, row2) bp = True
  | tra col2 == tra col1 + 1 && row2 == row1 + 1 && not (checkNotOccupied (col2, row2) bp) = True
  | tra col2 == tra col1 - 1 && row2 == row1 + 1 && not (checkNotOccupied (col2, row2) bp) = True
  | otherwise = False
checkPieceMove wp bp Black (P (col1, row1)) (col2, row2)
  | row2 == row1 - 1 && tra col2 == tra col1 && checkNotOccupied (col2, row2) wp = True
  | tra col2 == tra col1 + 1 && row2 == row1 - 1 && not (checkNotOccupied (col2, row2) wp) = True
  | tra col2 == tra col1 - 1 && row2 == row1 - 1 && not (checkNotOccupied (col2, row2) wp) = True
  | otherwise = False
checkPieceMove wp bp _ (N (col1, row1)) (col2, row2) =
  tra col2 == tra col1 + 1 && row2 == row1 + 2
    || tra col2 == tra col1 + 1 && row2 == row1 - 2
    || tra col2 == tra col1 - 1 && row2 == row1 + 2
    || tra col2 == tra col1 - 1 && row2 == row1 - 2
    || tra col2 == tra col1 + 2 && row2 == row1 + 1
    || tra col2 == tra col1 + 2 && row2 == row1 - 1
    || tra col2 == tra col1 - 2 && row2 == row1 + 1
    || tra col2 == tra col1 - 2 && row2 == row1 - 1
checkPieceMove wp bp _ (K (col1, row1)) (col2, row2) =
  tra col2 == tra col1 + 1 && row2 == row1 + 1
    || tra col2 == tra col1 + 1 && row2 == row1 - 1
    || tra col2 == tra col1 - 1 && row2 == row1 + 1
    || tra col2 == tra col1 - 1 && row2 == row1 - 1
    || tra col2 == tra col1 + 1 && row2 == row1
    || tra col2 == tra col1 - 1 && row2 == row1
    || tra col2 == tra col1 && row2 == row1 + 1
    || tra col2 == tra col1 && row2 == row1 - 1
checkPieceMove wp bp _ (Q (col1, row1)) (col2, row2) =
  ( tra col1 == tra col2 && row2 - row1 /= 0
      || row1 == row2 && tra col2 - tra col1 /= 0
      || tra col2 - tra col1 == row2 - row1 && tra col2 - tra col1 /= 0
      || tra col2 - tra col1 == -(row2 - row1) && tra col2 - tra col1 /= 0
  )
    && checkUninterruptedMove wp bp (Q (col1, row1)) (col2, row2)
checkPieceMove wp bp _ (R (col1, row1)) (col2, row2) =
  (tra col1 == tra col2 && row2 - row1 /= 0 || row1 == row2 && tra col2 - tra col1 /= 0)
    && checkUninterruptedMove wp bp (R (col1, row1)) (col2, row2)
checkPieceMove wp bp _ (B (col1, row1)) (col2, row2) =
  ( tra col2 - tra col1 == row2 - row1 && tra col2 - tra col1 /= 0
      || tra col2 - tra col1 == -(row2 - row1) && tra col2 - tra col1 /= 0
  )
    && checkUninterruptedMove wp bp (B (col1, row1)) (col2, row2)

checkUninterruptedMove wp bp (Q (col1, row1)) (col2, row2)
  | tra col1 == tra col2 && row2 - 1 > row1 = checkNotOccupied (col1, row1 + 1) wp && checkNotOccupied (col1, row1 + 1) bp && checkUninterruptedMove wp bp (Q (col1, row1 + 1)) (col2, row2)
  | tra col1 == tra col2 && row2 + 1 < row1 = checkNotOccupied (col1, row1 - 1) wp && checkNotOccupied (col1, row1 - 1) bp && checkUninterruptedMove wp bp (Q (col1, row1 - 1)) (col2, row2)
  | row1 == row2 && tra col2 - 1 > tra col1 = checkNotOccupied (succ col1, row1) wp && checkNotOccupied (succ col1, row1) bp && checkUninterruptedMove wp bp (Q (succ col1, row1)) (col2, row2)
  | row1 == row2 && tra col2 + 1 < tra col1 = checkNotOccupied (pred col1, row1) wp && checkNotOccupied (pred col1, row1) bp && checkUninterruptedMove wp bp (Q (pred col1, row1)) (col2, row2)
  | tra col1 < tra col2 - 1 && row1 > row2 + 1 = checkNotOccupied (succ col1, row1 - 1) wp && checkNotOccupied (succ col1, row1 - 1) bp && checkUninterruptedMove wp bp (Q (succ col1, row1 - 1)) (col2, row2)
  | tra col1 < tra col2 - 1 && row1 < row2 - 1 = checkNotOccupied (succ col1, row1 + 1) wp && checkNotOccupied (succ col1, row1 + 1) bp && checkUninterruptedMove wp bp (Q (succ col1, row1 + 1)) (col2, row2)
  | tra col1 > tra col2 + 1 && row1 > row2 + 1 = checkNotOccupied (pred col1, row1 - 1) wp && checkNotOccupied (pred col1, row1 - 1) bp && checkUninterruptedMove wp bp (Q (pred col1, row1 - 1)) (col2, row2)
  | tra col1 > tra col2 + 1 && row1 < row2 - 1 = checkNotOccupied (pred col1, row1 + 1) wp && checkNotOccupied (pred col1, row1 + 1) bp && checkUninterruptedMove wp bp (Q (pred col1, row1 + 1)) (col2, row2)
  | otherwise = True
checkUninterruptedMove wp bp (R (col1, row1)) (col2, row2)
  | tra col1 == tra col2 && row2 - 1 > row1 = checkNotOccupied (col1, row1 + 1) wp && checkNotOccupied (col1, row1 + 1) bp && checkUninterruptedMove wp bp (R (col1, row1 + 1)) (col2, row2)
  | tra col1 == tra col2 && row2 + 1 < row1 = checkNotOccupied (col1, row1 - 1) wp && checkNotOccupied (col1, row1 - 1) bp && checkUninterruptedMove wp bp (R (col1, row1 - 1)) (col2, row2)
  | row1 == row2 && tra col2 - 1 > tra col1 = checkNotOccupied (succ col1, row1) wp && checkNotOccupied (succ col1, row1) bp && checkUninterruptedMove wp bp (R (succ col1, row1)) (col2, row2)
  | row1 == row2 && tra col2 + 1 < tra col1 = checkNotOccupied (pred col1, row1) wp && checkNotOccupied (pred col1, row1) bp && checkUninterruptedMove wp bp (R (pred col1, row1)) (col2, row2)
  | otherwise = True
checkUninterruptedMove wp bp (B (col1, row1)) (col2, row2)
  | tra col1 < tra col2 - 1 && row1 > row2 + 1 = checkNotOccupied (succ col1, row1 - 1) wp && checkNotOccupied (succ col1, row1 - 1) bp && checkUninterruptedMove wp bp (B (succ col1, row1 - 1)) (col2, row2)
  | tra col1 < tra col2 - 1 && row1 < row2 - 1 = checkNotOccupied (succ col1, row1 + 1) wp && checkNotOccupied (succ col1, row1 + 1) bp && checkUninterruptedMove wp bp (B (succ col1, row1 + 1)) (col2, row2)
  | tra col1 > tra col2 + 1 && row1 > row2 + 1 = checkNotOccupied (pred col1, row1 - 1) wp && checkNotOccupied (pred col1, row1 - 1) bp && checkUninterruptedMove wp bp (B (pred col1, row1 - 1)) (col2, row2)
  | tra col1 > tra col2 + 1 && row1 < row2 - 1 = checkNotOccupied (pred col1, row1 + 1) wp && checkNotOccupied (pred col1, row1 + 1) bp && checkUninterruptedMove wp bp (B (pred col1, row1 + 1)) (col2, row2)
  | otherwise = True

-- d) suggestMove

suggestMoveHelper :: [Location]
suggestMoveHelper =
  [ ('a', 1),
    ('a', 2),
    ('a', 3),
    ('a', 4),
    ('a', 5),
    ('a', 6),
    ('a', 7),
    ('a', 8),
    ('b', 1),
    ('b', 2),
    ('b', 3),
    ('b', 4),
    ('b', 5),
    ('b', 6),
    ('b', 7),
    ('b', 8),
    ('c', 1),
    ('c', 2),
    ('c', 3),
    ('c', 4),
    ('c', 5),
    ('c', 6),
    ('c', 7),
    ('c', 8),
    ('d', 1),
    ('d', 2),
    ('d', 3),
    ('d', 4),
    ('d', 5),
    ('d', 6),
    ('d', 7),
    ('d', 8),
    ('e', 1),
    ('e', 2),
    ('e', 3),
    ('e', 4),
    ('e', 5),
    ('e', 6),
    ('e', 7),
    ('e', 8),
    ('f', 1),
    ('f', 2),
    ('f', 3),
    ('f', 4),
    ('f', 5),
    ('f', 6),
    ('f', 7),
    ('f', 8),
    ('g', 1),
    ('g', 2),
    ('g', 3),
    ('g', 4),
    ('g', 5),
    ('g', 6),
    ('g', 7),
    ('g', 8),
    ('h', 1),
    ('h', 2),
    ('h', 3),
    ('h', 4),
    ('h', 5),
    ('h', 6),
    ('h', 7),
    ('h', 8)
  ]

suggestMove2 piece board [] = []
suggestMove2 piece board (hCons : tCons)
  | isLegal piece board hCons = hCons : suggestMove2 piece board tCons
  | otherwise = suggestMove2 piece board tCons

suggestMove piece (player, whitePieces, blackPieces) = suggestMove2 piece (player, whitePieces, blackPieces) suggestMoveHelper

-- e) move

updateSameList (player, wp, bp) (P (col1, row1)) (col2, row2)
  | player == White = removeItem (col1, row1) wp ++ [P (col2, row2)]
  | player == Black = removeItem (col1, row1) bp ++ [P (col2, row2)]
updateSameList (player, wp, bp) (N (col1, row1)) (col2, row2)
  | player == White = removeItem (col1, row1) wp ++ [N (col2, row2)]
  | player == Black = removeItem (col1, row1) bp ++ [N (col2, row2)]
updateSameList (player, wp, bp) (K (col1, row1)) (col2, row2)
  | player == White = removeItem (col1, row1) wp ++ [K (col2, row2)]
  | player == Black = removeItem (col1, row1) bp ++ [K (col2, row2)]
updateSameList (player, wp, bp) (Q (col1, row1)) (col2, row2)
  | player == White = removeItem (col1, row1) wp ++ [Q (col2, row2)]
  | player == Black = removeItem (col1, row1) bp ++ [Q (col2, row2)]
updateSameList (player, wp, bp) (R (col1, row1)) (col2, row2)
  | player == White = removeItem (col1, row1) wp ++ [R (col2, row2)]
  | player == Black = removeItem (col1, row1) bp ++ [R (col2, row2)]
updateSameList (player, wp, bp) (B (col1, row1)) (col2, row2)
  | player == White = removeItem (col1, row1) wp ++ [B (col2, row2)]
  | player == Black = removeItem (col1, row1) bp ++ [B (col2, row2)]

getLocation (P (col, row)) = (col, row)
getLocation (N (col, row)) = (col, row)
getLocation (K (col, row)) = (col, row)
getLocation (Q (col, row)) = (col, row)
getLocation (R (col, row)) = (col, row)
getLocation (B (col, row)) = (col, row)

emptyPieceList :: [Piece] -> Bool
emptyPieceList [] = True
emptyPieceList _ = False

removeItem _ [] = []
removeItem location (y : ys)
  | location == getLocation y = removeItem location ys
  | otherwise = y : removeItem location ys

updateBoard (White, whitePieces, blackPieces) piece (col2, row2) = (White, whitePieces1, blackPieces1)
  where
    whitePieces1 = removeItem (col2, row2) whitePieces
    blackPieces1 = updateSameList (Black, whitePieces, blackPieces) piece (col2, row2)
updateBoard (Black, whitePieces, blackPieces) piece (col2, row2) = (Black, whitePieces1, blackPieces1)
  where
    blackPieces1 = removeItem (col2, row2) blackPieces
    whitePieces1 = updateSameList (White, whitePieces, blackPieces) piece (col2, row2)

checkPlayer piece (player, whitePieces, blackPieces)
  | player == White && elem piece whitePieces = True
  | player == Black && elem piece blackPieces = True
  | player == White = error ("This is White player's turn, Black can't move")
  | player == Black = error ("This is Black player's turn, White can't move")

move :: Piece -> (Char, Int) -> (Player, [Piece], [Piece]) -> (Player, [Piece], [Piece])
move piece location (player, whitePieces, blackPieces)
  | player == White && isLegal piece (White, whitePieces, blackPieces) location && checkPlayer piece (player, whitePieces, blackPieces) = updateBoard (Black, whitePieces, blackPieces) piece location
  | player == Black && isLegal piece (Black, whitePieces, blackPieces) location && checkPlayer piece (player, whitePieces, blackPieces) = updateBoard (White, whitePieces, blackPieces) piece location
  | otherwise = error ("Illegal move for piece " ++ show piece)