{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState

import Data.Maybe
import Data.List


{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = Node {
                        state :: s,
                        action :: Maybe a,
                        parent :: Maybe (Node s a),
                        depth :: Int,
						nodeSuccessors :: [Node s a]
                     } deriving (Eq, Show)

{-
    *** TODO ***

    Întoarce starea stocată într-un nod.
-}

nodeState :: Node s a -> s
nodeState n = state n

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor 
    Primește starea inițială și creează nodul corespunzător acestei stări, 
    având drept copii nodurile succesorilor stării curente.
-}

createStateSpace :: (Eq a, Eq s, ProblemState s a) => s -> Node s a
createStateSpace root = my_stateSpace
	where
		my_stateSpace = Node root Nothing Nothing 0 nodeSuccessorss
		my_successors = successors root
		nodeSuccessorss = nextLvlSuccesors [] 0 my_successors my_stateSpace

nextLvlSuccesors successorsList depthh succs parentt = if succs == [] then successorsList else nextLvlSuccesors newSuccessorsList depthh (tail succs) parentt
	where
		node = Node st (Just act) (Just parentt) (depthh + 1) children
		children = nextLvlSuccesors [] (depthh + 1) (successors st) node
		st = snd $ head $ succs
		act = fst $ head $ succs
		newSuccessorsList = (successorsList ++ [node])

{-
    *** TODO PENTRU BONUS ***

    Ordonează întreg spațiul stărilor după euristica din ProblemState. 
    Puteți folosi `sortBy` din Data.List.
-}

orderStateSpace :: (ProblemState s a) => Node s a -> Node s a
orderStateSpace = undefined


{-
    *** TODO ***

    Întoarce lista nodurilor rezultate prin parcurgerea limitată în adâncime
    a spațiului stărilor, pornind de la nodul dat ca parametru.

    Pentru reținerea stărilor vizitate, recomandăm Data.Set. Constrângerea
    `Ord s` permite utilizarea tipului `Set`.
-}

-- https://stackoverflow.com/questions/48696720/checking-to-see-if-an-element-exists-in-a-list-in-haskell
isElement :: (Eq a, Eq s) => Node s a -> [Node s a] -> Bool
isElement _ [] = False
isElement node (x:xs)
  | xs == [] = False
  | (nodeState node) == (nodeState x) = True
  | otherwise = isElement node xs

limitedDfs :: (Eq a, Eq s) => (ProblemState s a, Ord s)
           => Node s a    -- Nodul stării inițiale
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri

limitedDfsHelper :: (Eq a, Eq s) => [Node s a] -> [Node s a] -> Int -> [Node s a]
limitedDfsHelper [] visited _ = visited
limitedDfsHelper nodesList visited maxDepth
	| isElement x visited = limitedDfsHelper xs visited maxDepth
	| depth x == maxDepth = limitedDfsHelper xs (x : visited) maxDepth
	| otherwise = limitedDfsHelper (my_nodeSuccessors ++ xs) (x : visited) maxDepth
	where
		my_nodeSuccessors = nodeSuccessors x
		x = head nodesList
		xs = tail nodesList
		   
limitedDfs node maxDepth = reverse $ result
	where 
		result = limitedDfsHelper [node] [] maxDepth
	

{-
    *** TODO ***

    Explorează în adâncime spațiul stărilor, utilizând adâncire iterativă,
    pentru determinarea primei stări finale întâlnite.

    Întoarce o perche între nodul cu prima stare finală întâlnită și numărul
    de stări nefinale vizitate până în acel moment.
-}

iterativeDeepening :: (Eq a, Eq s) => (ProblemState s a, Ord s)
    => Node s a         -- Nodul stării inițiale
    -> (Node s a, Int)  -- (Nod cu prima stare finală,
                        --  număr de stări nefinale vizitate)
						
		
iterativeDeepeningHelper :: (Eq a, Eq s) => (ProblemState s a, Ord s) => (Node s a) -> (Node s a, Int)
iterativeDeepeningHelper node = pereche
	where
		dfss = map (\new_maxdepth -> (limitedDfs node new_maxdepth)) [1,2..]
		filtereddfss = map (filter (\t -> isGoal (state t))) dfss
		okdepth = length (takeWhile (== []) filtereddfss)
		firstsVid = sum (map length (take okdepth dfss))
		listaBuna = head $ drop okdepth dfss 
		lungimeListaBuna = length listaBuna
		pereche = ((fromJust (find (\t -> isGoal (state t)) listaBuna)), firstsVid + lungimeListaBuna - 1)
		
iterativeDeepening node = iterativeDeepeningHelper node

{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

extractPath :: (Eq a, Eq s) => Node s a -> [(a, s)]
extractPath (Node st ac pa _ _)
    | pa == Nothing = []
    | otherwise = my_path ++ [(fromJust ac, st)]
	where
		my_path = (extractPath (Node (state (fromJust pa)) (action (fromJust pa)) (parent (fromJust pa)) (depth (fromJust pa)) (nodeSuccessors (fromJust pa))))

{-
    *** TODO ***

    Pornind de la o stare inițială, se folosește de iterativeDeepening pentru 
    a găsi prima stare finală și reface calea către nodul inițial folosind 
    extractPath. 
  
    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

solve :: (Eq a, Eq s) => (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește 
      -> Bool       -- Dacă să folosească sau nu euristica dată 
      -> [(a, s)]   -- Lista perechilor
solve s _ = extractPath node
	where
		node = fst $ iterativeDeepening $ createStateSpace s

{-
    Poate fi utilizată pentru afișarea fiecărui element al unei liste
    pe o linie separată.
-}

printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))