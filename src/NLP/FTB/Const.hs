{-# LANGUAGE OverloadedStrings #-}


-- Parsing French treebank phrase structure files.


module NLP.FTB.Const where


import           Control.Applicative ((<*), (*>), (<|>))

-- import qualified Data.Tree as R
import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.Either as E
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified Data.Attoparsec.Text.Lazy as A
import qualified Pipes as P


-- | Default text type.
type Text = T.Text


-- | Attribute-value matrix.
type AVM = M.Map Text Text


-- | An FTB constituency tree.
data FTree
  = INode
    { label :: Text
      -- ^ Phrasal category
    , subTrees :: [FTree]
    }
  | FNode
    { label :: Text
      -- ^ POS category
    , avm :: AVM
      -- ^ Morphological information
    , orth :: Text
      -- ^ Orthographic form
    } deriving (Show, Eq, Ord)


-- readFile :: FilePath


-- | Parse an FTB file with a list of syntactic trees, each tree
-- in a separate line.
parseFile :: L.Text -> [FTree]
parseFile = E.rights . map parseTree . L.lines


-- | Parse tree if possible.  Return the error otherwise.
parseTree :: L.Text -> Either String FTree
parseTree = A.eitherResult . A.parse topP
-- parseTree = A.eitherResult . A.parse treeP


-- | FTB top-level tree parser.
topP :: A.Parser FTree
topP = do
  A.char '(' *> A.many1 A.space
  treeP <* A.char ')'


-- | Tree parser.
treeP :: A.Parser FTree
treeP = do
  A.char '('
  t <- nodeP <|> leafP
  A.char ')'
  return t


-- | Internal node parser.
nodeP :: A.Parser FTree
nodeP = do
  x <- labelP
  A.space
  xs <- treeP `A.sepBy1` A.space
  return $ INode x xs


-- | Leaf parser.
leafP :: A.Parser FTree
leafP = do
  x <- labelP
  a <- avmP
  A.space
  o <- orthP
  return $ FNode x a o


-- | AVM parser.
avmP :: A.Parser AVM
avmP = do
  A.string "##"
  xs <- attrValP `A.sepBy` A.char '|'
  A.string "##"
  return $ M.fromList xs


-- | Attribute-value pair parser.
attrValP :: A.Parser (Text, Text)
attrValP = do
  x <- labelP
  A.char '='
  y <- labelP
  return (x, y)


-- | Label parser.
labelP :: A.Parser Text
labelP = A.takeTill $ \c ->
  C.isSpace c || c `elem` ['|', '#', '=']


-- | Orthographic form parser.
orthP :: A.Parser Text
orthP = A.takeTill $ \c ->
  C.isSpace c || c `elem` ['|', '#', ')']
