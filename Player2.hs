module Payer2 where


import Data.List
import Control.Lens
import Network.Wreq
import Network.HTTP.Types.Header
import qualified Data.ByteString.Char8 as BytCh
import qualified Data.ByteString.Lazy.Char8 as BytChLazy
import qualified System.IO as SysIO
import qualified Data.CaseInsensitive as CI

type Cord = (String, String, String, String)
type PirmMap = [(Cord)]
type Ats = (String, String)

message :: String
message = "[{\"x\": 0, \"y\": 0, \"v\": \"x\"}, {\"x\": 2, \"y\": 1, \"v\": \"o\"}, {\"x\": 0, \"y\": 2, \"v\": \"x\"}, {\"x\": 2, \"y\": 2, \"v\": \"o\"}, {\"x\": 0, \"y\": 1, \"v\": \"x\"}, {\"x\": 1, \"y\": 1, \"v\": \"o\"}, {\"x\": 1, \"y\": 2, \"v\": \"x\"}, {\"x\": 2, \"y\": 0, \"v\": \"o\"}]"

xar :: String
xar = "012"

yar :: String
yar = "012"

--Main funkcija kvietimui
main :: IO ()
main = do
	getA
	SysIO.putStrLn "GG"

url1 = "http://tictactoe.homedir.eu/game/mako1416/player/2"

toHeaderName :: String -> HeaderName
toHeaderName header = CI.mk (BytCh.pack header)

postA msg = do
    let opts = defaults & header (toHeaderName "Content-Type") .~ [BytCh.pack "application/json+map"]
    let msg' = answer msg
    r <- postWith opts url1 $ BytCh.pack (msg')
    if msg' == "GG"
        then return()
        else getA

getA = do
    let opts2 = defaults & header (toHeaderName "Accept") .~ [BytCh.pack "application/json+map"]
    r <- getWith opts2 url1
    let msg = BytChLazy.unpack(r ^. responseBody)
    if answer msg == "GG"
        then return()
        else postA msg



--patikrinam ar duotas stringas ne tuscias jei ne kvieciam funkcija kas rastu ats
answer :: String -> String
answer "{}" = "{\"0\": {\"x\": 0, \"y\": 0, \"v\": \"o\"}}"
answer d  = 
	let
		call = addToList(removeBrackets(removeSpaces d)) []
		last = findlast call
		(ats, ats2) = perx call xar
		lastnumber = findlastnr call
		ilgis = (length d) - 1
		b = take ilgis d
		at = concat [b, ", \"", lastnumber, "\": {\"x\": ", ats, ", \"y\": ", ats2, ", \"v\": \"", last, "\"}}"]
	in if (ats == "n" && ats2 == "n")
	then "GG"
	else at

--funkcija pašalinti laužtinius skliaustus(brackets)
removeBrackets :: String -> String
removeBrackets move = 
	let 
		bb = drop 1 move	
	in bb

--pasalina tarpus
removeSpaces :: String -> String
removeSpaces str = filter(/=' ') str	

first :: Cord -> String
first (x,_,_,_) = x

second :: Cord -> String
second (_,x,_,_) = x

third :: Cord -> String
third (_,_,x,_) = x

fourth :: Cord -> String
fourth (_,_,_,x) = x


addToList :: String -> PirmMap -> PirmMap
addToList [] list = list
addToList "}" list = list
addToList ad list =
	let
        (move, rest) = readMoves ad
    in addToList rest (move : list)
	
	
readMoves :: String -> (Cord, String) -- (ejimas, likusi stringo dalis)
readMoves mo = 
	let
	mov = takeWhile (/= '}') mo
	--tailas = drop 19 mo
	mov1 = drop 1 mo
	nr = [mov1 !! 0]
	mov2 = drop 8 mov1
	pirm = [mov2 !! 0]
	mov3 = drop 6 mov2
	antr = [mov3 !! 0]
	mov4 = drop 7 mov3
	trec = [mov4 !! 0]
	tailas = drop 4 mov4
	in ((pirm, antr, trec, nr), tailas)	
	


findlast :: PirmMap -> String
findlast [] = ""
findlast a =
		let
		pask = head a
		laste = third pask
		in if (laste == "o") 
		then "x"
		else "o"


findlastnr :: PirmMap -> String
findlastnr [] = ""
findlastnr a =
			let
			pask = head a
			laste = fourth pask
			in if (laste == "0") then "1"
			else if (laste == "1") then "2"
			else if (laste == "2") then "3"
			else if (laste == "3") then "4"
			else if (laste == "4") then "5"
			else if (laste == "5") then "6"
			else if (laste == "6") then "7"
			else if (laste == "7") then "8"
			else "8"
		
		
perx :: PirmMap -> String -> Ats 
perx a "" = ("n", "n")
perx a b =
			let
			ordx = [b !! 0]
			tailas = tail b
			z = pery a yar ordx
			in if (z /= "n") 
				then (ordx, z)
			else perx a tailas
			
pery :: PirmMap -> String -> String -> String
pery a "" x = "n"
pery a b x =
			let
			ordy = [b !! 0]
			tailas = tail b
			z = pereitiPerMapa a x ordy
			in if (z == True) 
				then ordy
				else pery a tailas x
		
		
	
pereitiPerMapa :: PirmMap -> String -> String -> Bool
pereitiPerMapa [] x y = True
pereitiPerMapa map x y = 
		let
			pirmas = head map
			tailas = tail map
			xas = x
			yas = y
			
		in if ((xas == first pirmas) && (yas == second pirmas))
				then False
				else pereitiPerMapa tailas xas yas	