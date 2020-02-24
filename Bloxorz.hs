{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}


module Bloxorz where

import ProblemState

import qualified Data.Array as A

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
    *** TODO ***

    Tip de date care va reprezenta plăcile care alcătuiesc harta și switch-urile
-}


data Cell = HardTile | SoftTile | Switch [Position]| Block | EmptySpace | WinningTile deriving(Eq,Ord)
instance Show Cell where
    show HardTile = [hardTile]
    show SoftTile = [softTile]
    show (Switch l)= [switch]
    show Block = [block]
    show EmptySpace = [emptySpace]
    show WinningTile = [winningTile]


{-    *** TODO ***

    Tip de date pentru reprezentarea nivelului curent
-}

data Level = Level (A.Array (Int,Int) Cell) (Position,Position) Bool deriving (Eq, Ord)

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
    *** TODO ***

    Instantiati Level pe Show. 

    Atenție! String-ul returnat va fi urmat și precedat de un rând nou. 
    În cazul în care jocul este câștigat, la sfârșitul stringului se va mai
    concatena mesajul "Congrats! You won!\n". 
    În cazul în care jocul este pierdut, se va mai concatena "Game Over\n". 
-}

instance Show Level where
      show (Level harta (pos1,pos2) moved)=concat["\n"++concat[show (if(i,j)/=pos1 && (i,j)/=pos2 then(harta A.! (i, j))else Block) | j <- [0..snd.snd$A.bounds harta]] | i <- [0..fst.snd$A.bounds harta]] ++ (if continueGame(Level harta (pos1,pos2) moved) == False && moved == True 
									then if(harta A.! pos1) == WinningTile then "\nCongrats! You won!\n"
													       else "\nGame Over\n"
									else "\n") 
															

{-
    *** TODO ***

    Primește coordonatele colțului din dreapta jos a hârtii și poziția inițială a blocului.
    Întoarce un obiect de tip Level gol.
    Implicit, colțul din stânga sus este (0, 0).
-}

emptyLevel :: Position -> Position -> Level
emptyLevel dim bl_pos = Level (A.array((0,0),dim) [((i,j),EmptySpace) | i <-[0..fst dim], j <- [0..snd dim]]) (bl_pos,bl_pos) False


{-
    *** TODO ***

    Adaugă o celulă de tip Tile în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        'H' pentru tile hard 
        'S' pentru tile soft 
        'W' pentru winning tile 
-}

addTile :: Char -> Position -> Level -> Level
addTile ch pos (Level harta bl_pos m) = Level(harta A.//[(pos,case ch of 'H' -> HardTile 
	               	            				         'S' -> SoftTile
							                 'W' -> WinningTile)]) bl_pos m					
{-
    *** TODO ***

    Adaugă o celulă de tip Swtich în nivelul curent.
    Va primi poziția acestuia și o listă de Position
    ce vor desemna pozițiile în care vor apărea sau 
    dispărea Hard Cells în momentul activării/dezactivării
    switch-ului.
-}

addSwitch :: Position -> [Position] -> Level -> Level
addSwitch pos lst (Level harta bl_pos m) = Level(harta A.//[(pos,Switch lst)]) bl_pos m

{-
    === MOVEMENT ===
-}

{-
    *** TODO ***

    Activate va verifica dacă mutarea blocului va activa o mecanică specifică. 
    În funcție de mecanica activată, vor avea loc modificări pe hartă. 
-}

activate :: Cell -> Level -> Level
activate tile (Level harta pos m) = case tile of (Switch l) -> Level(harta A.//[(index,if(harta A.! index)==EmptySpace then HardTile else EmptySpace) | index<-l]) pos m 
					         HardTile -> Level harta pos m
					         WinningTile -> Level harta pos m
					         EmptySpace -> Level harta pos m
					         SoftTile -> if(fst pos)==(snd pos) then Level(harta A.//[((fst pos),EmptySpace)]) pos m
										  else Level harta pos m
{-
    *** TODO ***

    Mișcarea blocului în una din cele 4 direcții 
    Hint: Dacă jocul este deja câștigat sau pierdut, puteți lăsa nivelul neschimbat.
-}

move :: Directions -> Level -> Level
move dir (Level harta ((i1,j1),(i2,j2)) m) =(if(i1,j1)==(i2,j2) then case dir of North->activate(harta A.!(i2-1,j2))(activate(harta A.!(i1-2,j1))(Level harta((i1-2,j1),(i2-1,j2))True)) 
								                 South->activate(harta A.!(i2+2,j2))(activate(harta A.!(i1+1,j1))(Level harta((i1+1,j1),(i2+2,j2))True))
									         East->activate(harta A.!(i2,j2+2))(activate(harta A.!(i1,j1+1))(Level harta((i1,j1+1),(i2,j2+2))True))
									         West->activate(harta A.!(i2,j2-1))(activate(harta A.!(i1,j1-2))(Level harta((i1,j1-2),(i2,j2-1))True))
					  else if(i1,j1)==(i2-1,j2) then case dir of North->activate(harta A.!(i1-1,j1))(Level harta((i1-1,j1),(i2-2,j2))True)
										     South->activate(harta A.!(i1+2,j1))(Level harta((i1+2,j1),(i2+1,j2))True)
										     East->activate(harta A.!(i2,j2+1))(activate(harta A.!(i1,j1+1))(Level harta((i1,j1+1),(i2,j2+1))True))
										     West->activate(harta A.!(i2,j2-1))(activate(harta A.!(i1,j1-1))(Level harta((i1,j1-1),(i2,j2-1))True)) 
						   else case dir of North->activate(harta A.!(i2-1,j2))(activate(harta A.!(i1-1,j1))(Level harta((i1-1,j1),(i2-1,j2))True))
							            South->activate(harta A.!(i2+1,j2))(activate(harta A.!(i1+1,j1))(Level harta((i1+1,j1),(i2+1,j2))True))
								    East->activate(harta A.!(i1,j1+2))(Level harta((i1,j1+2),(i2,j2+1))True)
								    West->activate(harta A.!(i1,j1-1))(Level harta((i1,j1-1),(i2,j2-2))True))   
{-
    *** TODO ***

    Va returna True dacă jocul nu este nici câștigat, nici pierdut.
    Este folosită în cadrul Interactive.
-}

continueGame :: Level -> Bool
continueGame (Level harta (pos1,pos2) m)= if(pos1==pos2 && (harta A.! pos1) == WinningTile) || 
					  ((harta A.! pos1) == EmptySpace || (harta A.! pos2) == EmptySpace) then False
													     else True

{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru. 
  
    Hint: Un level câștigat nu are succesori! 
    De asemenea, puteți ignora succesorii care 
    duc la pierderea unui level.
-}

instance ProblemState Level Directions where
    successors (Level harta pos m) = let mapN = move North (Level harta pos m)
					 mapS = move South (Level harta pos m)
					 mapE = move East (Level harta pos m)
					 mapW = move West (Level harta pos m)
				     in filter (\x -> isGoal (snd x) == True || continueGame (snd x) == True) 
									[(East,mapE),(West,mapW),(South,mapS),(North,mapN)]
    isGoal (Level harta (pos1,pos2) m) = if continueGame(Level harta (pos1,pos2) m) == False && (harta A.! pos1) == WinningTile then True
																else False 

    -- Doar petru BONUS
    -- heuristic = undefined
continuu :: Int -> [Int]
continuu x = x : (continuu (x + 1))
