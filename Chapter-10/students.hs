data MyData = Name String | Rollno Integer deriving (Eq, Show, Ord)

shubham :: [MyData]
shubham = [Name "Shubham", Rollno 18116] 

isName :: MyData -> Bool
isName (Name x) = True
isName _ = False
