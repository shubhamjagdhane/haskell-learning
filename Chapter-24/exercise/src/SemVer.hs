module SemVer where

import Text.Trifecta

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving Show

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving Show
instance Eq SemVer where
  (==) (SemVer x y z _ _) (SemVer x' y' z' _ _) = x==x' && y==y' && z==z'

instance Ord SemVer where
  (SemVer x y z _ _) <= (SemVer x' y' z' _ _) = x <= x' && y <= y' && z <= z'

-- -x.7.z.92
parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  _ <- char '.'
  minor <- integer 
  _ <- char '.'
  patch <- integer 
  _ <- char '-'
  n1 <- some letter
  _  <- char '.'
  n2 <- integer
  _  <- char '.'
  n3 <- some letter
  _  <- char '.'
  n4 <- integer
  return (SemVer major minor patch [NOSS n1, NOSI n2, NOSS n3, NOSI n4] [])
