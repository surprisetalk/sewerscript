
module Main where

-- import Control.Monad (void)
import Data.Void
import qualified Data.Map as Map
import Data.Complex (Complex(..))
import Data.Scientific (Scientific)
import Numeric.Natural (Natural)
import Data.Ratio (Rational,(%))
import Text.Megaparsec
import Text.Megaparsec.Char
-- import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import System.Environment

import Text.Pretty.Simple (pPrint)

-------------------------------------------------------------------------------

type Parser = Parsec Void String

-------------------------------------------------------------------------------

type Prg
  = [Exp]

data Exp
  = ExpOTC Exp
  | ExpORS Exp
  | ExpOCP Exp
  | ExpLst Lst
  | ExpDct Dct
  | ExpPrn Lst
  | ExpTup Lst (Int   )
  | ExpRec Dct -- (Set String)
  | ExpQut Exp
  | ExpSym Sym
  | ExpTyp Exp
  | ExpNmb Nmb
  | ExpTxt Txt
  deriving (Show)

data Txt
  = TxtStr String
  | TxtChr Char
  -- TODO: Path, URI, etc.
  deriving (Show)

type Sym
  = String

type Dct
  = [(Exp,Exp)]
  
type Lst
  = [Exp]
  
data Nmb
  -- TODO: make notation for 0+n 0-n, 1+n, 1-n (for guarding against negatives and zeroes)
  = NmbNat NmbBase  Natural
  | NmbInt NmbBase  Int
  | NmbFlt NmbBase  Float
  | NmbFrc NmbBase  Rational
  | NmbSci NmbBase  Scientific
  | NmbCIn NmbBase (Currency Int)
  | NmbCFl NmbBase (Currency Float)
  | NmbPIn NmbBase (Complex Int)
  | NmbPFl NmbBase (Complex Float)
  deriving (Show)

type NmbBase = Natural

data Currency a
  = CurCur a
  deriving (Show)

-- data Exp
--   = ExpQut Exp
--   | ExpLam Exp
--   | ExpPii Exp
--   | ExpNmb Nmb
--   | ExpTxt Txt
--   | ExpTyp Exp
--   | ExpSub Exp
--   | ExpSym Sym
--   | ExpLst [Exp]
--   | ExpArr [Exp]
--   | ExpTup [Exp] (Int)
--   | ExpDct (Dct)
--   | ExpRec (Dct) -- [String]
--   deriving (Show)


-------------------------------------------------------------------------------

sc :: Parser ()
sc  = skipMany spaceChar

sc1 :: Parser ()
sc1  = L.space (skipSome spaceChar) (L.skipLineComment "--") empty

integer :: Parser Int
integer = L.decimal

nongroup :: Parser Char
nongroup  = noneOf " \t\n(){}[]`'\""

-- specialChar :: Parser Char
-- specialChar = oneOf ",.!#$%&|*+-/:<=>?@^_~\\"

currencySymbol :: Parser Char
currencySymbol = oneOf "$¢Ƀ£¤¥฿₣₤€₹₽"


-------------------------------------------------------------------------------

parseRational :: Parser Rational
parseRational = do
  numer <- L.decimal
  _     <- char '/'
  denom <- L.decimal
  return $ numer % denom

parseComplex :: Parser a -> Parser (Complex a)
parseComplex p = do
  real <- p
  _    <- oneOf "+-"
  comp <- p
  _    <- char 'i'
  return $ real :+ comp

parseCurrency :: Parser a -> Parser (Currency a)
parseCurrency p = do
  _ <- currencySymbol
  n <- p
  return $ CurCur $ n

parseNmb :: Parser Nmb
parseNmb  = L.lexeme sc1
          $ NmbInt 10 <$> try (L.signed empty $ L.decimal              )
        <|> NmbFlt 10 <$> try (L.signed empty $ L.float                )
        <|> NmbSci 10 <$> try (L.signed empty $ L.scientific           )
        <|> NmbFrc 10 <$> try (L.signed empty $ parseRational          )
        <|> NmbPIn 10 <$> try (                 parseComplex  L.decimal)
        <|> NmbPFl 10 <$> try (L.signed empty $ parseComplex  L.float  )
        <|> NmbCIn 10 <$> try (                 parseCurrency L.decimal)
        <|> NmbCFl 10 <$> try (                 parseCurrency L.float  )

parseTxt :: Parser Txt
parseTxt  = L.lexeme sc1
          $ TxtChr <$> between (char '\'') (char '\'')       (noneOf  "'")
        <|> TxtStr <$> between (char  '"') (char  '"') (many (noneOf "\""))

grouper :: String -> String -> Parser a -> Parser [a]
grouper s e p = between (L.symbol sc s) (L.symbol sc e) $ sepEndBy p sc1

parseGrp :: Parser Exp
parseGrp  =  do grp <- grouper "(" ")" parseExp
                return $ ExpPrn $ grp
         <|> do grp <- grouper "[" "]" parseExp
                return $ ExpLst $ grp
         <|> do grp <- grouper "=[" "]" parseExp
                return $ ExpTup grp $ length grp
         <|> do grp <- grouper "{" "}" parsePair
                return $ ExpDct $ grp
         <|> do grp <- grouper "={" "}" parsePair
                return $ ExpRec grp
         -- <|> do grp <- grouper "&(" ")" parseExp
         --        return $ ExpGrp $ GrpLst $ grp

parsePair :: Parser (Exp,Exp)
parsePair = do
  a <- parseExp
  _ <- sc1
  b <- parseExp
  return $ (a,b)

parseQut :: Parser Exp
parseQut = L.lexeme sc1 $ do
  _   <- char '#'
  sym <- parseExp
  return $ ExpQut $ sym

parseTyp :: Parser Exp
parseTyp = L.lexeme sc1 $ do
  _   <- char ':'
  sym <- parseExp
  return $ ExpTyp $ sym

parseSym :: Parser Exp
parseSym  = do
  sym <- L.lexeme sc1 $ some nongroup
  return $ ExpSym $ sym
  
parseOp :: String -> Parser Exp
parseOp s = do
  _ <- L.symbol sc1 s
  b <- parseExp
  return $ b

parseExp :: Parser Exp
parseExp = parseQut
       <|> ExpOTC <$> parseOp "|="
       <|> ExpORS <$> parseOp "|>"
       <|> ExpOCP <$> parseOp "|-"
       <|> parseTyp
       <|> parseGrp
       <|> ExpNmb <$> try parseNmb
       <|> ExpTxt <$>     parseTxt
       <|> parseSym

parsePrg :: Parser Prg
parsePrg = between sc eof $ sepEndBy parseExp sc1
-- BUG: Does this mean that your program can't start with a comment?

-------------------------------------------------------------------------------

type Env = Map.Map String Exp

type State = ( Env, String -> String )

initEnv :: Env
initEnv = Map.fromList
  [
  ]
  -- [ ( "."  , lam "label" $ lam "body"     $ ExpLst [ ExpSub (ExpSym "label"), (ExpSym "body"     ) ] )
  -- , ( ":"  , lam "label" $ lam "subtypes" $ ExpLst [ ExpSub (ExpSym "label"), (ExpSym "subtypes" ) ] )
  -- , ( "\\" , lam "var"   $ lam "body"     $ ExpLst [ ExpSub (ExpSym "var"  ), (ExpSym "body"     ) ] )
  -- ]

get :: String -> Env -> Exp
get sym env = case Map.lookup sym env of
  Just x  -> x
  Nothing -> error $ "could not find " ++ show sym ++ " in env"
-- get sym = Map.findWithDefault (ExpSym sym) sym

set :: String -> Exp -> Env -> Env
set = Map.insert

-- compile :: Main.State -> Exp -> Fun
-- compile (env,f) (ExpQut      ex   ) = f . 
-- compile (env,_) (ExpSym     sym   ) = get sym env
-- compile (_  ,_) (ExpLst (    [] ) ) = ExpLst []
-- compile (env,_) (ExpLst (  x:xs ) ) = apply env x xs
-- compile (_  ,_) (ExpTyp     typ   ) = ExpTyp $ typ
-- compile (env,_) (ExpLst      xs   ) = ExpLst $ eval env <$> xs
-- compile (env,_) (ExpTup      xs n ) = ExpTup ( eval env <$> xs ) $ n
-- compile (env,_) (ExpDct      xs   ) = ExpDct $ (\(k,v) -> (eval env k, eval env v)) <$> xs 
-- compile (env,_) (ExpRec      xs   ) = ExpRec $ (\(k,v) -> (eval env k, eval env v)) <$> xs 
  
--  = ExpLst Lst
--  | ExpDct Dct
--  | ExpPrn Lst
--  | ExpTup Lst (Int   )
--  | ExpRec Dct -- (Set String)
--  | ExpQut Exp
--  | ExpSym Sym
--  | ExpTyp Exp
--  | ExpNmb Nmb
--  | ExpTxt Txt

-- typecheck :: [Exp] -> Exp -> [Exp]
-- typecheck xs x = xs ++ [ x ]

-------------------------------------------------------------------------------

main :: IO ()
main = do
  (filePath:_) <- getArgs
  fileContent <- readFile filePath
  -- _   <- parseTest' parsePrg txt
  case parse parsePrg filePath fileContent of
    Left err -> putStr $ parseErrorPretty err
    Right xs -> pPrint $ xs
    -- Right xs -> pPrint $ fold compile (initState,id) $ fold typecheck [] xs
    -- Right xs -> print  (eval initEnv xs)
    -- TODO: On the next iteration, each character should be a function, and we'll just compose them all together.
    -- TODO: Compose a parser onto the begninning.
    -- TODO: Compose a printer onto the end.
