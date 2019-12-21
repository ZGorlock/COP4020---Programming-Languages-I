module Chess where

-- See https://en.wikipedia.org/wiki/Chess for more details
-- We only consider the situation where there is only a single
-- piece on the board

-- see Rules - Set up for basic definitions

type File     = Char         -- column index
                             -- valid files are 'a','b',...,'h'
type Rank     = Int          -- row index
                             -- valid ranks are 1,2,...8
type Position = (File,Rank)   

data Color =
  Black | White
  deriving (Eq,Show)

data Piece =
  King | Queen | Rook | Bishop | Knight | Pawn
  deriving (Eq,Show)

isLegalPosition :: Position -> Bool
isLegalPosition (f,r) =
    (if ((f >= 'a' && f <= 'h') &&
         (r >= 1 && r <= 8)) then True
     else False)

-- see Rules - Movement for legal movements 

isLegalMove :: Color -> Piece -> Position -> Position -> Bool
isLegalMove c p pi pf = 
    (if (isLegalPosition pi && isLegalPosition pf && --legal position check
         (pi /= pf)) --cant move to the same location
     then isLegalMoveHelper c p pi pf
     else False)

     
isLegalMoveHelper:: Color -> Piece -> Position -> Position -> Bool
isLegalMoveHelper _ King (f1,r1) (f2,r2) =
    (if ((abs(fromEnum(f1) - fromEnum(f2)) <= 1) && --not more than one space horizontally
         (abs(r1 - r2) <= 1)) --not more than one space vertically
     then True
     else False)

isLegalMoveHelper _ Queen (f1,r1) (f2,r2) =
    (if (((f1 == f2) || (r1 == r2)) ||    --can move horizontal or vertical
        (abs(fromEnum(f1) - fromEnum(f2)) == 
         abs(r1 - r2))) --or can move the same number of spaces vertically and horizontally
     then True
     else False)

isLegalMoveHelper _ Rook (f1,r1) (f2,r2) =
    (if ((f1 == f2) || (r1 == r2)) --can move horizontal or vertical but not both
     then True
     else False)

isLegalMoveHelper _ Bishop (f1,r1) (f2,r2) =
    (if (abs(fromEnum(f1) - fromEnum(f2)) ==
         abs(r1 - r2)) --must move the same number of spaces vertically and horizontally
     then True
     else False)

isLegalMoveHelper _ Knight (f1,r1) (f2,r2) =
    (if ((abs(fromEnum(f1) - fromEnum(f2)) == 2 && abs(r1 - r2) == 1) || --can move 2 over 1 up 
         (abs(fromEnum(f1) - fromEnum(f2)) == 1 && abs(r1 - r2) == 2)) --can move 1 over 2 up
     then True
     else False)

isLegalMoveHelper Black Pawn (f1,r1) (f2,r2) =
    (if (r1 == 7) --initial rank
     then (if ((f1 == f2) && (((r1 - r2) == 1) || ((r1 - r2) == 2)) && (r1 /= 8)) --can move 1 or 2 forward
           then True
           else False)
     else (if ((f1 == f2) && ((r1 - r2) == 1) && (r1 /= 8)) --can move 1 forward
           then True
           else False))
isLegalMoveHelper White Pawn (f1,r1) (f2,r2) =
    (if (r1 == 2) --initial rank
     then (if ((f1 == f2) && (((r2 - r1) == 1) || ((r2 - r1) == 2)) && (r1 /= 1)) --can move 1 or 2 forward
           then True
           else False)
     else (if ((f1 == f2) && ((r2 - r1) == 1) && (r1 /= 1)) --can move 1 forward
           then True
           else False))