{
module Parse where
import Data.Char
}

%name parser
%tokentype { Token }

%token
	'*' { TimesToken }
	'+' { PlusToken }
	'-' { MinusToken }
	'/' { DivideToken }
	'^' { PowerToken }
	'(' { OpenToken }
	')'	{ CloseToken }
	int { IntToken $$ }
	str { VarToken $$}
	'sin' { SinToken }
	'cos' { CosToken }
	'd/dx' {DeriveToken}
%%

Exp :: { Exp }
Exp : Exp '+' Exp { PlusE $1 $3 }
	| Exp '-' Exp { MinusE $1 $3 }
	| 'sin' Exp { SinE $2 }
	| 'cos' Exp { CosE $2 }
	| 'd/dx' Exp { DerE $2 }
	| Term { Term $1 }
Term : Term '*' Term { TimesE $1 $3 }
	| Term '/' Term { DivE $1 $3 }
	| Factor { Factor $1 }
Factor : int { Int $1 }
	| str { VarE $1 }
	| '(' Exp ')' { Brack $2 }
	| Exp '^' int { PowerE $1 $3}

{
	
happyError :: [Token] -> a
happyError _ = error ("Parse error\n")

data Exp = PlusE Exp Exp | MinusE Exp Exp | SinE Exp | CosE Exp | DerE Exp | Term Term deriving (Eq, Show, Ord)
data Term = TimesE Term Term | DivE Term Term| Factor Factor deriving (Eq, Show, Ord)
data Factor = Int Int | VarE String | Brack Exp | PowerE Exp Int deriving (Eq, Show, Ord)

data Token = PlusToken | MinusToken | TimesToken | DivideToken | PowerToken | DeriveToken | SinToken |
    CosToken | OpenToken | CloseToken | IntToken Int | VarToken String deriving (Eq, Show, Ord)


lexer :: String -> [Token]
lexer []                   = []
lexer ('+'      : restStr) = PlusToken      : lexer restStr
lexer ('-'      : restStr) = MinusToken     : lexer restStr 
lexer ('*'      : restStr) = TimesToken     : lexer restStr
lexer ('/'      : restStr) = DivideToken    : lexer restStr
lexer ('^'      : restStr) = PowerToken     : lexer restStr
lexer ('('      : restStr) = OpenToken      : lexer restStr 
lexer (')'      : restStr) = CloseToken     : lexer restStr
lexer (chr : restStr) 
    | isSpace chr = lexer restStr
lexer str@(chr : _) 
    | isDigit chr = IntToken (stringToInt digitStr) : lexer restStr1
    | isLetter chr = lexLetters varStr : lexer restStr2
    where
        (digitStr, restStr1) = break (not . isDigit) str
        stringToInt :: String -> Int
        stringToInt = foldl (\acc chr -> 10 * acc + digitToInt chr) 0
        (varStr, restStr2) = break (not . isLetter) str     
lexer (_ : restString) 
  = error ("lexer: unexpected character")
  
lexLetters "sin" = SinToken
lexLetters "cos" = CosToken
lexLetters "d/dx" = DeriveToken
lexLetters x = VarToken x

}