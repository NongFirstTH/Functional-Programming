import Data.List

type Label = Int
type Index = Int
type ColumnLength = Int
type Area = Int
type Perimeter = Int

calculateColumnLength :: String -> Int
calculateColumnLength [] = 0
calculateColumnLength ('\n' : _) = 0
calculateColumnLength (_ : xs) = 1 + calculateColumnLength xs

readF = readFile "input2.txt"

ptrInput = do
  input <- readF
  putStrLn $ show $ length input

-- input = "RRRRIICCFF\nRRRRIICCCF\nVVRRRCCFFF\nVVRCCCJFFF\nVVVVCJJCFE\nVVIVCCJJEE\nVVIIICJJEE\nMIIIIIJJEE\nMIIISIJEEE\nMMMISSJEEE"

-- main :: IO Int
main = do
  input <- readF
  let (result, equivalentTable) = processGarden input 0 (calculateColumnLength input) 0 [] []
  let equivalentTable' = sort equivalentTable
  -- putStrLn $ show $ (result, equivalentTable')
  -- putStrLn $ show $ processEquivalentTable equivalentTable' result

  -- return $ processResult $ processEquivalentTable equivalentTable' result
  -- return $ processEquivalentTable equivalentTable' result
  -- putStrLn $ show $ processEquivalentTable equivalentTable' result
  putStrLn $ show $ processResult $ processEquivalentTable equivalentTable' result

processGarden :: [Char] -> Index -> ColumnLength -> Label -> [(Label, Char)] -> [(Area, Perimeter)] -> ([(Area, Perimeter)], [(Label, Label)])
processGarden [] _ _ _ _ result = (result, [])
processGarden ('\n' : xs) i columnLen k garden result = processGarden xs i columnLen k garden result
processGarden (x : xs) i columnLen k garden result =
  let u = findWithIndex garden (columnLen - 1)
      l = if i `mod` columnLen == 0
          then (-1, '\0')
          else findWithIndex garden 0
      p = if xs == []
          then 2
          else
            if i `mod` columnLen == columnLen - 1 || length xs < columnLen
            then 1
            else 0
  in case (x == snd l, x == snd u) of
    (True, True) -> 
      let (result', equivalentTable) = processGarden xs (i+1) columnLen k ((fst l, x) : garden) (addAreaOfLabel (fst l) 1 $ addPeremiterOfLabel (fst l) p $ result)
      in if fst l /= fst u
        then (result', (min (fst l) (fst u), max (fst l) (fst u)) : equivalentTable)
        else (result', equivalentTable)
    (True, False) -> processGarden xs (i+1) columnLen k ((fst l, x) : garden) (addAreaOfLabel (fst l) 1 $ addPeremiterOfLabel (fst l) (1+p) $ addPeremiterOfLabel (fst u) 1 $ result)
    (False, True) -> processGarden xs (i+1) columnLen k ((fst u, x) : garden) (addAreaOfLabel (fst u) 1 $ addPeremiterOfLabel (fst l) 1 $ addPeremiterOfLabel (fst u) (1+p) $ result)
    (False, False) -> processGarden xs (i+1) columnLen (k+1) ((k, x) : garden) (addAreaOfLabel k 1 $ addPeremiterOfLabel (fst l) 1 $ addPeremiterOfLabel (fst u) 1 $ addPeremiterOfLabel k (2+p) $ result)

processEquivalentTable :: [(Label, Label)] -> [(Area, Perimeter)] -> [(Area, Perimeter)]
processEquivalentTable [] result = result
-- processEquivalentTable ((l1, r1) : (l2, r2) : equivalentTable) result =
--   let 
--     resultL1 = result !! l1
--     resultR1 = result !! r1
--     resultL1R1 = (fst resultL1 + fst resultR1, snd resultL1 + snd resultR1)
--     target = if l1 == l2 
--              then r2
--              else r1
--   in
--     processEquivalentTable ((l2, r2) : equivalentTable) $ addAreaOfLabel target (fst resultL1R1) $ addPeremiterOfLabel target (snd resultL1R1) $ resetLabel l1  $ resetLabel r1 $ result
processEquivalentTable ((l, r) : equivalentTable) result =
  let 
    resultL = result !! l
    resultR = if (l==r) then (0, 0) else result !! r
    resultLR = (fst resultL + fst resultR, snd resultL + snd resultR)
  in
    processEquivalentTable (map (\(x, y) -> let f = \x -> if x == l then r else x in (f x, f y)) equivalentTable) $ replaceLabel r resultLR $ resetLabel l $ result

processResult :: [(Area, Perimeter)] -> Int
processResult [] = 0
processResult ((a, p) : xs) = a * p + processResult xs

addAreaOfLabel :: Label -> Area -> [(Area, Perimeter)] -> [(Area, Perimeter)]
addAreaOfLabel (-1) _ xs = xs
addAreaOfLabel 0 a [] = [(a, 0)]
addAreaOfLabel n a [] = (0, 0) : addAreaOfLabel (n - 1) a []
addAreaOfLabel 0 a ((area, perimeter) : xs) = (area + a, perimeter) : xs
addAreaOfLabel n a (x : xs) = x : addAreaOfLabel (n - 1) a xs

addPeremiterOfLabel :: Label -> Perimeter -> [(Area, Perimeter)] -> [(Area, Perimeter)]
addPeremiterOfLabel (-1) _ xs = xs
addPeremiterOfLabel 0 p [] = [(0, p)]
addPeremiterOfLabel n p [] = (0, 0) : addPeremiterOfLabel (n - 1) p []
addPeremiterOfLabel 0 p ((area, perimeter) : xs) = (area, perimeter + p) : xs
addPeremiterOfLabel n p (x : xs) = x : addPeremiterOfLabel (n - 1) p xs

resetLabel :: Label -> [(Area, Perimeter)] -> [(Area, Perimeter)]
resetLabel 0 [] = [(0, 0)]
resetLabel n [] = (0, 0) : resetLabel (n - 1) []
resetLabel 0 (_ : xs) = (0, 0) : xs
resetLabel n (x : xs) = x : resetLabel (n - 1) xs

replaceLabel :: Label -> (Area, Perimeter) -> [(Area, Perimeter)] -> [(Area, Perimeter)]
replaceLabel 0 x [] = [x]
replaceLabel n x [] = (0, 0) : replaceLabel (n - 1) x []
replaceLabel 0 x (_ : xs) = x : xs
replaceLabel n x (x' : xs) = x' : replaceLabel (n - 1) x xs

findWithIndex :: [(Label, Char)] -> Int -> (Label, Char)
findWithIndex xs i
  | i < 0 || i >= length xs = (-1, '\0')
  -- | i < 0 || i >= length xs = undefined
  | otherwise = (xs !! i)