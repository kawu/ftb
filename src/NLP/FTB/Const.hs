{-# LANGUAGE OverloadedStrings #-}


-- Parsing French treebank phrase structure files.  We do not assume any
-- specific symbol/category/pos values so that the parser should work also
-- if new values are introduced to FTB.



module NLP.FTB.Const where


import           Control.Applicative ((<*), (*>), (<|>), (<$>),
                    (<$))
import           Control.Monad (guard)

-- import qualified Data.Tree as R
import qualified Data.Char as C
import           Data.Maybe (catMaybes)
import qualified Data.Map as M
import qualified Data.Either as E
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified Data.Attoparsec.Text.Lazy as A
-- import qualified Pipes as P


import           NLP.FTB.Tree


-- | Default text type.
type Text = T.Text


-- | Attribute-value matrix.
type AVM = [(Text, Text)]


-- | A syntagmatic symbol of the form SYMBOL or SYMBOL-FUNCTIONTAG
-- (e.g. "NP" or "NP-SUJ").
data Sym
  = Sym
    { syn :: Text
      -- ^ Syntagmatic symbol (e.g. "NP")
    , fun :: Maybe Text
      -- ^ Functional tag (e.g. "SUJ")
    } deriving (Show, Eq ,Ord)


-- | A part-of-speech symbol are of the form:
-- POS-FCT##FEAT1=VAL1|FEAT2=VAL2|...##.
data Word
  = Word
    { pos :: Sym
      -- ^ Part-of-speech tag
    , avm :: AVM
      -- ^ Attribute-value matrix with morphological features
    , orth :: Text
      -- ^ Orthographic value
    } deriving (Show, Eq ,Ord)


-- | Multi-word expression.
data MWE
  = MWE
    { mweSyn :: Sym
      -- ^ Syntagmatic symbol of MWE
    , mweBody :: [Word]
      -- ^ Elements of MWE
    } deriving (Show, Eq, Ord)


-- | FTB syntactic tree.
type FTree = Tree Sym (Either Word MWE)


-- | Parse an FTB file with a list of syntactic trees, each tree
-- in a separate line.
-- readFile :: FilePath -> P.Producer FTree IO ()
readFTB :: FilePath -> IO [FTree]
readFTB path = parseFTB <$> L.readFile path


-- | Parse an FTB file contents with a list of syntactic trees,
-- each tree in a separate line.
parseFTB :: L.Text -> [FTree]
-- parseFTB = E.rights . map parseTree . L.lines
parseFTB = map f . L.lines
  where
    f line = case parseTree line of
      Left err -> error $ show line
      Right x -> x


-- | Parse tree if possible.  Return the error otherwise.
parseTree :: L.Text -> Either String FTree
parseTree = A.eitherResult . A.parse topP


-- | FTB top-level tree parser.
topP :: A.Parser FTree
topP = do
  A.char '(' *> A.space
  treeP <* A.char ')'


-- | Tree parser.
treeP :: A.Parser FTree
treeP = nodeP
    <|> (FNode . Left <$> wordP)
    <|> (FNode . Right <$> mweP)


-- | Internal node parser.
nodeP :: A.Parser FTree
nodeP = do
  A.char '('
  x  <- symP <* A.space
  xs <- treeP `A.sepBy1` A.space
  A.char ')'
  return $ INode x xs


-- | Word parser.
wordP :: A.Parser Word
wordP = do
  A.char '('
  x <- symP
  a <- avmP
  A.space
  o <- orthP
  A.char ')'
  return $ Word x a o


-- | MWE parser.
mweP :: A.Parser MWE
mweP = do
  A.char '('
  x  <- mweSymP <* A.space
  xs <- wordP `A.sepBy1` A.space
  A.char ')'
  return $ MWE x xs


-- | AVM parser.
avmP :: A.Parser AVM
avmP = do
  A.string "##"
  let avP = (Just <$> attrValP) <|>
            (Nothing <$ A.char '_')
  xs <- avP `A.sepBy` A.char '|'
  A.string "##"
  -- return $ M.fromList xs
  return $ catMaybes xs


-- | Attribute-value pair parser.
attrValP :: A.Parser (Text, Text)
attrValP = do
  x <- labP
  A.char '='
  y <- valP
  return (x, y)
  where
    valP = A.takeTill (`elem` ['|', '#'])


-- | Regular (non-MWE) symbol parser.
symP :: A.Parser Sym
symP = do
  x <- labP
  guard $ T.last x /= '+'
  return $ mkS x


-- | MWE (with '+' at the end) symbol parser.
mweSymP :: A.Parser Sym
mweSymP = do
  x <- labP
  guard $ T.last x == '+'
  return $ mkS x


-- | Label (and not a freely structured string) parser.
labP :: A.Parser Text
labP = A.takeWhile1 $ \c ->
  C.isAlphaNum c || c `elem` ['-', '+', '_']


-- | Orthographic form parser.
orthP :: A.Parser Text
-- Orthographic form is always at the end of a word description,
-- thus is it always followed by a closing parenthesis.
orthP = A.takeTill (==')')


-- | Make a proper `Sym` from a string.
mkS :: Text -> Sym
mkS x = case T.split (== '-') x of
  [y]-> Sym y Nothing
  (y:ys) -> Sym y $ Just $ T.intercalate "-" ys
