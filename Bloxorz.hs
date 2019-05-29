{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}


module Bloxorz where

import ProblemState

import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.Maybe as M

{-
    Caracterele ce vor fi printate pentru fiecare tip de obiect din joc 
    Puteți înlocui aceste caractere cu orice, în afară de '\n'.
-}

hardTile :: Char
hardTile = '▒'

softTile :: Char
softTile = '='

block :: Char
block = '▓'

switch :: Char
switch = '±'

emptySpace :: Char
emptySpace = ' '

winningTile :: Char
winningTile = '*'

{-
    Sinonim de tip de date pentru reprezetarea unei perechi (int, int)
    care va reține coordonatele de pe tabla jocului
-}

type Position = (Int, Int)

{-
    Direcțiile în care se poate mișcă blocul de pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Tip de date care va reprezenta plăcile care alcătuiesc harta și switch-urile
-}

data Cell = Hard | Soft | Block | Switch | Empty | Winning 
	deriving (Eq, Ord)

instance Show Cell
	where
		show Hard = [hardTile]
		show Soft = [softTile]
		show Block = [block]
		show Switch = [switch]
		show Empty = [emptySpace]
		show Winning = [winningTile]
	

{-
    Tip de date pentru reprezentarea nivelului curent
-}

data Level = MyLevel { arrayGame :: (A.Array Position Cell)
					, switchPairs :: [(Position, [Position])]
					, blockPositions :: [(Position, Cell)]} deriving (Eq, Ord)

{-
    *** Opțional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară, 
    instantiati explicit clasele Eq și Ord pentru Level. 
    În cazul acesta, eliminați deriving (Eq, Ord) din Level. 
-}

-- instance Eq Level where
--     (==) = undefined

-- instance Ord Level where
--     compare = undefined

{-
    Instantiati Level pe Show. 

    Atenție! String-ul returnat va fi urmat și precedat de un rând nou. 
    În cazul în care jocul este câștigat, la sfârșitul stringului se va mai
    concatena mesajul "Congrats! You won!\n". 
    În cazul în care jocul este pierdut, se va mai concatena "Game Over\n". 
-}

-- http://hackage.haskell.org/package/split-0.2.3.3/docs/src/Data-List-Split-Internals.html#chunksOf

build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

myChunksOf :: Int -> [e] -> [[e]]
myChunksOf i ls = map (take i) (build (splitter ls))
	where
		splitter :: [e] -> ([e] -> a -> a) -> a -> a
		splitter [] _ n = n
		splitter l c n  = l `c` splitter (drop i l) c n


showArr :: Level -> String
showArr (MyLevel level _ _) = L.intercalate "\n" . map concat . map (map show) $ myChunksOf upperBound $ (A.elems $ level)
	where 
		((_, _), (_, endY)) = A.bounds level
		upperBound = endY + 1
		
putMessage :: Level -> String
putMessage (MyLevel _ _ blockPosition)
	| (length blockPosition) == 1 && ((snd $ head $ blockPosition) == Empty || (snd $ head $ blockPosition) == Soft ) = "Game Over\n"
	| (length blockPosition) == 2 && (((snd $ head $ blockPosition) == Empty) || ((snd $ head $ tail $ blockPosition) == Empty)) = "Game Over\n"
	| (length blockPosition) == 1 && ((snd $ head $ blockPosition) == Winning) = "Congrats! You won!\n"
	| (length blockPosition) == 2 && (((snd $ head $ blockPosition) == Winning) || ((snd $ head $ tail $ blockPosition) == Winning)) = "Congrats! You won!\n"
	| otherwise = ""

	
instance Show Level
	where
		show level = "\n" ++ showArr level ++ "\n" ++ (putMessage level)
		
{-
    Primește coordonatele colțului din dreapta jos a hârtii și poziția inițială a blocului.
    Întoarce un obiect de tip Level gol.
    Implicit, colțul din stânga sus este (0, 0).
-}


emptyLevel :: Position -> Position -> Level
emptyLevel positionRightDown positionBlock = MyLevel finalArray [] [(positionBlock, Block)]
	where 
		initialArray = A.array ((0, 0),((fst positionRightDown), (snd positionRightDown )))
			([((i, j), Empty) | i <- [0..(fst positionRightDown)], j <- [0..(snd positionRightDown)]])
		finalArray = initialArray A.// [(positionBlock, Block)]

{-
    *** TODO ***

    Adaugă o celulă de tip Tile în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        'H' pentru tile hard 
        'S' pentru tile soft 
        'W' pentru winning tile 
-}

addTile :: Char -> Position -> Level -> Level
addTile charType position (MyLevel level switchPairss blockPosition) = MyLevel updatedArray switchPairss blockPositionn
	where
		updatedArray = if charType == 'H' && position /= (fst (head blockPosition)) then level A.// [(position, Hard)]
										else if charType == 'S' && position /= (fst (head blockPosition)) then level A.// [(position, Soft)]
										else if charType == 'W' && position /= (fst (head blockPosition)) then level A.// [(position, Winning)]
										else level
										
		blockPositionn = if charType == 'H' && position == (fst (head blockPosition)) then [(position, Hard)]
										else if charType == 'S' && position == (fst (head blockPosition)) then [(position, Soft)]
										else if charType == 'W' && position == (fst (head blockPosition)) then [(position, Winning)]
										else blockPosition

{-
    *** TODO ***

    Adaugă o celulă de tip Swtich în nivelul curent.
    Va primi poziția acestuia și o listă de Position
    ce vor desemna pozițiile în care vor apărea sau 
    dispărea Hard Cells în momentul activării/dezactivării
    switch-ului.
-}

addSwitch :: Position -> [Position] -> Level -> Level
addSwitch position positionList (MyLevel level switchPairss blockPosition) = MyLevel updatedArray updatedSwitchPairs blockPosition
	where
		updatedArray = level A.// [(position, Switch)]
		updatedSwitchPairs = switchPairss ++ [(position, positionList)]

{-
    === MOVEMENT ===
-}

{-
    *** TODO ***

    Activate va verifica dacă mutarea blocului va activa o mecanică specifică. 
    În funcție de mecanica activată, vor avea loc modificări pe hartă. 
-}

activateOne :: Cell -> Level -> Level
activateOne cell (MyLevel level switchPairss blockPosition)
	| cell == Switch = MyLevel updatedArray switchPairss blockPosition
	| cell /= Switch = MyLevel level switchPairss blockPosition
	where 
		updatedArray = level A.// [(position1, myCell1)]
		position1 = head $ M.fromJust $ L.lookup (fst (head blockPosition)) $ switchPairss
		myCell1 =  if Hard == (M.fromJust $ L.lookup position1 $ A.assocs $ level) then Empty
					else Hard
				
activateTwo :: Cell -> Level -> Level
activateTwo cell (MyLevel level switchPairss blockPosition)
	| cell == Switch = MyLevel updatedArray switchPairss blockPosition
	| cell /= Switch = MyLevel level switchPairss blockPosition
	where 
		updatedArray = (level A.// [(position1, myCell1)]) A.// [(position2, myCell2)]
		position1 = head $ M.fromJust $ L.lookup (fst (head blockPosition)) $ switchPairss
		myCell1 =  if Hard == (M.fromJust $ L.lookup position1 $ A.assocs $ level) then Empty
					else Hard
		position2 = head $ tail $ M.fromJust $ L.lookup (fst (head blockPosition)) $ switchPairss
		myCell2 = if Hard == (M.fromJust $ L.lookup position2 $ A.assocs $ level) then Empty
					else Hard
		 
activateThem :: Cell -> Level -> Level
activateThem cell (MyLevel level switchPairss blockPosition)
	| my_len == 1 = activateOne cell (MyLevel level switchPairss blockPosition)
	| my_len == 2 = activateTwo cell (MyLevel level switchPairss blockPosition)
	where
		my_len = length $ M.fromJust $ L.lookup cellPos $ switchPairss
		cellPos = (fst (head blockPosition))
		
		 
activate :: Cell -> Level -> Level
activate cell (MyLevel level switchPairss blockPosition)
	| cell == Switch = activateThem cell (MyLevel level switchPairss blockPosition)
	| cell /= Switch = MyLevel level switchPairss blockPosition
	
{-
    *** TODO ***

    Mișcarea blocului în una din cele 4 direcții 
    Hint: Dacă jocul este deja câștigat sau pierdut, puteți lăsa nivelul neschimbat.
-}


move4 :: Directions -> Level -> Level
move4 direction (MyLevel level switchPairss blockPosition) = MyLevel (arrayGame $ newestLevel2) switchPairss [(moveOn2, cell2), (moveOn1, cell1)]
	where
		removedPos = fst $ head $ blockPosition
		removedCell = snd $ head $ blockPosition
		newestLevel2 = activate cell2 newLevel2
		newLevel2 = MyLevel (arrayGame $ newestLevel1) (switchPairs $ newestLevel1) newBlockPos2
		newBlockPos2 = [(moveOn2, cell2), (moveOn1, cell1)]
		newestLevel1 = activate cell1 newLevel1
		newLevel1 = MyLevel updatedArray switchPairss newBlockPos1
		newBlockPos1 = [(moveOn1, cell1)]
		updatedArray = (((level A.// [(moveOn1, Block)]) A.// [(moveOn2, Block)]) A.// [(removedPos, removedCell)])
		
		moveOn1 = if direction == North then ((fst $ fst $ head $ blockPosition) - 1, (snd $ fst $ head $ blockPosition))
					else if direction == South  then ((fst $ fst $ head $ blockPosition) + 1, (snd $ fst $ head $ blockPosition))
					else if direction == West then ((fst $ fst $ head $ blockPosition), (snd $ fst $ head $ blockPosition) - 1)
					else if direction == East then ((fst $ fst $ head $ blockPosition), (snd $ fst $ head $ blockPosition) + 1)
					else ((fst $ fst $ head $ blockPosition), (snd $ fst $ head $ blockPosition) + 1)
					
		moveOn2 = if direction == North then ((fst $ fst $ head $ blockPosition) - 2, (snd $ fst $ head $ blockPosition))
					else if direction == South then ((fst $ fst $ head $ blockPosition) + 2, (snd $ fst $ head $ blockPosition))
					else if direction == West then ((fst $ fst $ head $ blockPosition), (snd $ fst $ head $ blockPosition) - 2)
					else if direction == East then ((fst $ fst $ head $ blockPosition), (snd $ fst $ head $ blockPosition) + 2)
					else ((fst $ fst $ head $ blockPosition), (snd $ fst $ head $ blockPosition) + 2)
					
		cell1 = M.fromJust $ L.lookup moveOn1 $ A.assocs $ level
		cell2 = M.fromJust $ L.lookup moveOn2 $ A.assocs $ level
		
move8 :: Directions -> Level -> Level
move8 direction (MyLevel level switchPairss blockPosition) = MyLevel (arrayGame $ newestLevel2) switchPairss [(moveOn2, cell2), (moveOn1, cell1)]
	where
		removedCell1 = snd $ head $ blockPosition
		removedCell2 = snd $ head $ tail $ blockPosition
		removedPos1 = fst $ head $ blockPosition
		removedPos2 = fst $ head $ tail $ blockPosition
		moveOn1 = if direction == North then ((fst $ fst $ head $ blockPosition) - 1, (snd $ fst $ head $ blockPosition))
					else if direction == South then ((fst $ fst $ head $ blockPosition) + 1, (snd $ fst $ head $ blockPosition))
					else if direction == West  then ((fst $ fst $ head $ blockPosition), (snd $ fst $ head $ blockPosition) - 1)
					else if direction == East then ((fst $ fst $ head $ blockPosition), (snd $ fst $ head $ blockPosition) + 1)
					else ((fst $ fst $ head $ blockPosition), (snd $ fst $ head $ blockPosition) + 1)
		moveOn2 = if direction == North then ((fst $ fst $ head $ tail $ blockPosition) - 1, (snd $ fst $ head $ tail $ blockPosition))
					else if direction == South then ((fst $ fst $ head $ tail $ blockPosition) + 1, (snd $ fst $ head $ tail $ blockPosition))
					else if direction == West  then ((fst $ fst $ head $ tail $ blockPosition), (snd $ fst $ head $ blockPosition) - 1)
					else if direction == East then ((fst $ fst $ head $ tail $ blockPosition), (snd $ fst $ head $ tail $ blockPosition) + 1)
					else ((fst $ fst $ head $ tail $ blockPosition), (snd $ fst $ head $ tail $ blockPosition) + 1)
		cell1 = M.fromJust $ L.lookup moveOn1 $ A.assocs $ level
		cell2 = M.fromJust $ L.lookup moveOn2 $ A.assocs $ level			
		newestLevel2 = activate cell2 newLevel2
		newLevel2 = MyLevel (arrayGame $ newestLevel1) (switchPairs $ newestLevel1) newBlockPos2
		newBlockPos2 = [(moveOn2, cell2), (moveOn1, cell1)]
		newestLevel1 = activate cell1 newLevel1
		newLevel1 = MyLevel updatedArray switchPairss newBlockPos1
		newBlockPos1 = [(moveOn1, cell1)]
		updatedArray = ((((level A.// [(moveOn1, Block)]) A.// [(moveOn2, Block)]) A.// [(removedPos1, removedCell1)]) A.// [(removedPos2, removedCell2)])
		
		
move12 :: Directions -> Level -> Level
move12 direction (MyLevel level switchPairss blockPosition) = MyLevel ((((arrayGame $ newestLevel1) A.// [(moveOn1, Block)]) A.// [(removedPos1, removedCell1)]) A.// [(removedPos2, removedCell2)]) switchPairss newBlockPos2
	where
		removedCell1 = snd $ head $ blockPosition
		removedCell2 = snd $ head $ tail $ blockPosition
		removedPos1 = fst $ head $ blockPosition
		removedPos2 = fst $ head $ tail $ blockPosition
		minLine = if (fst $ fst $ head $ blockPosition) < (fst $ fst $ head $ tail $ blockPosition) then (fst $ fst $ head $ blockPosition) else (fst $ fst $ head $ tail $ blockPosition)
		maxLine = if (fst $ fst $ head $ blockPosition) > (fst $ fst $ head $ tail $ blockPosition) then (fst $ fst $ head $ blockPosition) else (fst $ fst $ head $ tail $ blockPosition)
		minCol = if (snd $ fst $ head $ blockPosition) < (snd $ fst $ head $ tail $ blockPosition) then (snd $ fst $ head $ blockPosition) else (snd $ fst $ head $ tail $ blockPosition)
		maxCol = if (snd $ fst $ head $ blockPosition) > (snd $ fst $ head $ tail $ blockPosition) then (snd $ fst $ head $ blockPosition) else (snd $ fst $ head $ tail $ blockPosition)
		moveOn1 = if direction == North then (minLine - 1, (snd $ fst $ head $ blockPosition))
					else if direction == South then (maxLine + 1, (snd $ fst $ head $ blockPosition))
					else if direction == West  then ((fst $ fst $ head $ blockPosition), minCol - 1)
					else if direction == East then ((fst $ fst $ head $ blockPosition), maxCol + 1)
					else ((fst $ fst $ head $ blockPosition), (snd $ fst $ head $ blockPosition) + 1)
		cell1 = M.fromJust $ L.lookup moveOn1 $ A.assocs $ level	
		newBlockPos2 = take 1 $ (blockPositions $ newestLevel1)
		newestLevel1 = activate cell1 newLevel1
		newLevel1 = MyLevel level switchPairss newBlockPos1
		newBlockPos1 = [(moveOn1, cell1)] ++ blockPosition

move :: Directions -> Level -> Level
move direction (MyLevel level switchPairss blockPosition)
	| (length blockPosition) == 1 = move4 direction (MyLevel level switchPairss blockPosition)
	| (length blockPosition) == 2 && (snd $ fst $ head $ blockPosition) == (snd $ fst $ head $ tail $ blockPosition)  && (direction == East || direction == West) = move8 direction (MyLevel level switchPairss blockPosition)
	| (length blockPosition) == 2 && (fst $ fst $ head $ blockPosition) == (fst $ fst $ head $ tail $ blockPosition)  && (direction == North || direction == South) = move8 direction (MyLevel level switchPairss blockPosition)
	| (length blockPosition) == 2 && (snd $ fst $ head $ blockPosition) == (snd $ fst $ head $ tail $ blockPosition)  && (direction == North || direction == South) = move12 direction (MyLevel level switchPairss blockPosition)
	| (length blockPosition) == 2 && (fst $ fst $ head $ blockPosition) == (fst $ fst $ head $ tail $ blockPosition)  && (direction == East || direction == West) = move12 direction (MyLevel level switchPairss blockPosition)
			
{-
    *** TODO ***

    Va returna True dacă jocul nu este nici câștigat, nici pierdut.
    Este folosită în cadrul Interactive.
-}


continueGame :: Level -> Bool
continueGame (MyLevel _ _ blockPosition)
	| (length blockPosition) == 1 && ((snd $ head $ blockPosition) == Soft) = True
	| (length blockPosition) == 1 && ((snd $ head $ blockPosition) == Empty) = False
	| (length blockPosition) == 2 && (((snd $ head $ blockPosition) == Empty) && ((snd $ head $ tail $ blockPosition) == Empty)) = False
	| (length blockPosition) == 1 && ((snd $ head $ blockPosition) == Winning) = False
	| otherwise = True


{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru. 
  
    Hint: Un level câștigat nu are succesori! 
    De asemenea, puteți ignora succesorii care 
    duc la pierderea unui level.
-}
successorsHelper :: Level -> Bool
successorsHelper (MyLevel _ _ blockPosition)
	| (length blockPosition) == 1 && ((snd $ head $ blockPosition) == Soft) = True
	| (length blockPosition) == 1 && ((snd $ head $ blockPosition) == Empty) = True
	| (length blockPosition) == 2 && (((snd $ head $ blockPosition) == Empty) && ((snd $ head $ tail $ blockPosition) == Empty)) = True 
    | (length blockPosition) == 2 && (((snd $ head $ blockPosition) == Soft) && ((snd $ head $ tail $ blockPosition) == Soft)) = True
	| (length blockPosition) == 1 && ((snd $ head $ blockPosition) == Winning) = False 
	| (length blockPosition) == 2 && (((snd $ head $ blockPosition) == Winning) &&((snd $ head $ tail $ blockPosition) == Winning)) = False 
	| otherwise = False


isWon :: Level -> Bool
isWon (MyLevel _ _ blockPosition)
	| (length blockPosition) == 1 && ((snd $ head $ blockPosition) == Winning) = True
	| (length blockPosition) == 2 && (((snd $ head $ blockPosition) == Winning) && ((snd $ head $ tail $ blockPosition) == Winning)) = True
	| otherwise = False

instance ProblemState Level Directions where
    successors (MyLevel level switchPairss blockPosition) = filter filter_function [(North, move North my_level), 
																					(South, move South my_level),
																					(East, move East my_level), 
																					(West, move West my_level)]
		where
			my_level = MyLevel level switchPairss blockPosition
			filter_function = (\(_, lvl)-> (successorsHelper lvl) == False || (isGoal lvl == True))
    isGoal (MyLevel level switchPairss blockPosition) = isWon (MyLevel level switchPairss blockPosition)

    -- Doar petru BONUS
    -- heuristic = undefined