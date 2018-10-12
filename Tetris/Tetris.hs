-- | The Tetris game (main module)
module Main where
import ConsoleGUI       -- cabal install ansi-terminal
--import CodeWorldGUI     -- cabal install codeworld-api
import Shapes
import Test.QuickCheck
import Data.List

--------------------------------------------------------------------------------
-- * The code that puts all the piece together

main = runGame tetrisGame

tetrisGame = Game { startGame     = startTetris,
                    stepGame      = stepTetris,
                    drawGame      = drawTetris,
                    gameInfo      = defaultGameInfo prop_Tetris,
                    tickDelay     = defaultDelay,
                    gameInvariant = prop_Tetris }

--------------------------------------------------------------------------------
-- * The various parts of the Tetris game implementation


-- | The state of the game
data Tetris = Tetris (Vector,Shape) Shape [Shape]
-- The state consists of three parts:
--   * The position and shape of the falling piece
--   * The well (the playing field), where the falling pieces pile up
--   * An infinite supply of random shapes

-- ** Positions and sizes

type Vector = (Int,Int)

-- | The size of the well
wellSize :: (Int,Int)
wellSize = (wellWidth,wellHeight)
wellWidth = 10
wellHeight = 20

-- | Starting position for falling pieces
startPosition :: Vector
startPosition = (wellWidth `div` 2 - 1, 0)

-- | Vector addition
vAdd :: Vector -> Vector -> Vector
(x1,y1) `vAdd` (x2,y2) = (x1+x2,y1+y2)

-- | Move the falling piece into position
place :: (Vector,Shape) -> Shape
place (v,s) = shiftShape v s

-- ** B4
-- | An invariant that startTetris and stepTetris should uphold
prop_Tetris :: Tetris -> Bool
prop_Tetris (Tetris (_,s) w _) = w == (emptyShape wellSize) && prop_Shape s

-- ** B5
-- | Add black walls around a shape
addWalls :: Shape -> Shape
addWalls s = S ((map (Just Black:) (shape))
 ++ ([replicate (n+2) (Just Black)]))
  where n = length $ head $ rows s
        hor = replicate n (Just Black):(rows s)
        shape = map (++[Just Black]) (hor)

-- ** B6
-- | Visualize the current game state. This is what the user will see
-- when playing the game.
drawTetris :: Tetris -> Shape
drawTetris (Tetris (v,p) w _) = addWalls(w) `combine` (shiftShape v p)

-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition,shape1) (emptyShape wellSize) supply
  where
    shape1:supply = repeat (allShapes!!1) -- incomplete !!!

test :: Int -> Int -> Int -> Int
test x y z = z+y+x 

-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int,Tetris)
stepTetris MoveDown t = tick t
stepTetris MoveLeft t = Just (0, movePiece (-1) t)
stepTetris MoveRight t = Just (0, movePiece 1 t)
stepTetris Rotate t = Just (0, rotatePiece t)
stepTetris _ t = tick t

-- ** B7
move :: Vector -> Tetris -> Tetris
move v (Tetris (v1,p) w shapeList) = Tetris (v1 `vAdd` v, p) w shapeList

rotate :: Tetris -> Tetris
rotate (Tetris (v,p) w shapeList) = Tetris (v, rotateShape $ p) w shapeList

-- ** B8
tick :: Tetris -> Maybe (Int,Tetris)
tick t =  Just (0, corrState)
   where 
      corrState = collisiontick (collision $ newstate) newstate t
      newstate= move (0,1) t

collisiontick :: Bool -> Tetris -> Tetris -> Tetris
collisiontick True  _ old = old
collisiontick False new _ = new
--collisiontick(collision $ newstate newstate t)

collision :: Tetris -> Bool
collision (Tetris (v,s) w _) = or [(fst $ v) -1 <0,
    (fst $ v) -1 + (fst $ shapeSize s)> (fst $ shapeSize $ w),
    (snd $ v) -1 + (snd $ shapeSize s)> (snd $ shapeSize $ w),
    overlaps s w]

--C3
movePiece :: Int -> Tetris -> Tetris
movePiece i t = collisiontick (collision $ newstate) newstate t 
   where
   	newstate= move (i,0) t

rotatePiece :: Tetris -> Tetris
rotatePiece t = collisiontick (collision $ newstate) newstate t 
   where
   	newstate= rotate t

dropNewPiece :: Tetris -> Maybe (Int,Tetris)
dropNewPiece (Tetris (v,s) w shapeList) = Just (0, Tetris (v,shape1) (combine w s) shapeList)
 where
    shape1:supply = repeat (allShapes!!1) -- incomplete !!!

