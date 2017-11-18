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
Exp : E1	{ E1 $1 }
E1 : Factor { Factor $1 }	
	| E1 '+' E1 { PlusE $1 $3 } 
	| E1 '-' E1 { MinusE $1 $3 }
	| E1 '*' E1 { TimesE $1 $3 } 
	| E1 '/' E1 { DivE $1 $3 }
	| 'sin' E1 { SinE $2 } 
	| 'cos' E1 { CosE $2 }
	| 'd/dx' E1 { DerE $2 }
	| E1 '^' E1 { PowerE $1 $3}
Factor : int			{ Int $1 }
	| str			{ Varr $1 }
	| '(' Exp ')'		{ Brack $2 }

{

happyError :: [Token] -> a
happyError _ = error ("Parse error\n")

data Exp  = E1 E1 deriving (Eq, Show, Ord)
data E1 = PlusE E1 E1 | MinusE E1 E1 | TimesE E1 E1 | DivE E1 E1 | SinE E1 | CosE E1 | DerE E1 | PowerE E1 E1 | Factor Factor deriving (Eq, Show, Ord)
data Factor = Int Int | Varr String | Brack Exp deriving (Eq, Show, Ord)

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