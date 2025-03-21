type CourseID = Int
type Capacity = Int
type StudentID = Int

data CourseInfo' = Course'
  { cid :: CourseID
  , cap :: Capacity
  , roster :: [StudentID]
  } deriving (Show)

-- enroll student in the course if
-- possible and return updated course
-- otherwise, Nothing
enroll'' :: CourseInfo' -> StudentID -> Either String CourseInfo'
enroll'' c sid
  | sid `elem` rs =
      Left "student already registered"
  | length rs >= seats =
      Left "course full"
  | otherwise = Right $
      Course' (cid c) seats (sid:rs)
  where seats = cap c
        rs = roster c

-- register student in a given course and
-- return updated registration information
-- if possible
register'' :: [CourseInfo'] -> CourseID -> StudentID -> Either String [CourseInfo']
register'' [] _ _ = Left "no such course"
register'' (c : cs) cid' sid
  | cid c == cid' =
      case enroll'' c sid of
        Left msg -> Left msg
        Right c' -> Right (c' : cs)
  | otherwise = 
      case register'' cs cid' sid of
        Left msg -> Left msg
        Right cs' -> Right (c : cs')

reg = [Course' 261406 2 [600610752, 600610764], Course' 261111 2 [600610111]]
res = register'' reg 261111 600610765

maybeAp :: Maybe (a -> b) -> Maybe a -> Maybe b
maybeAp Nothing _ = Nothing
maybeAp _ Nothing = Nothing
maybeAp (Just f) (Just x) = Just (f x) 

initMaybe :: a -> Maybe a
initMaybe a = Just a

listAp :: [a -> b] -> [a] -> [b]
listAp f x = [f' x' | f' <- f , x' <- x]

initList :: a -> [a]
initList a = [a]

-- fmap (*3) (+100)
-- เป็น fmap ที่รอรับ ตัวเลข type Num แล้วเอามา บวก 100 จากนั้นจำไปคูณ 3 ซึ่งผลลัพธ์ที่ได้ก็จะเป็น