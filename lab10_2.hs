data Month = January  | February | March
  | April | May | June
  | July  | August    | September 
  |October| November  | December deriving (Show, Enum, Eq)

daysInMonth :: Month -> Integer
daysInMonth m = case m of
  January    -> 31
  February   -> 28
  March      -> 31
  April      -> 30
  May        -> 31
  June       -> 30
  July       -> 31
  August     -> 31
  September  -> 30
  October    -> 31
  November   -> 30
  December   -> 31

nextDay :: Integer -> Month -> (Integer, Month)
nextDay d m = 
    if (d < 0) || (d > (daysInMonth m)) then error("Invalid day in month")
    else if d /= (daysInMonth m) then (d+1, m)
    else if m == December then (1,January)  
    else (1,succ m)
