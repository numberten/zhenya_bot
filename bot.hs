{-# Language ScopedTypeVariables #-}
import Network
import Control.Arrow
import Control.Monad.Reader
import Control.Exception
import Prelude hiding (catch)
import System.IO
import Text.Printf
import Data.List
import System.Exit
import System.Time
import Data.Char
import System.Random

server = "crafting.dangerbear.in"
port = 6667
chan = "#greatestguys"
nick = "zhenya_bot"

type Log = [(String, String)]
type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle, starttime :: ClockTime, events :: Log}

main :: IO ()
main = bracket connect disconnect loop
	where
		--disconnect :: Bot -> IO ()
		disconnect = hClose . socket
		--loop :: Bot -> IO ()
		loop st    = catch (runReaderT run st) (\(e :: SomeException) -> return ()) --(const $ return ())

connect :: IO Bot
connect = notify $ do
	t <- getClockTime
	h <- connectTo server (PortNumber (fromIntegral port))
	hSetBuffering h NoBuffering
	return (Bot h t [])
	where
		notify a = bracket_ (printf "Connecting to %s ... " server >> hFlush stdout) (putStrLn "done.") a

run :: Net ()
run = do
	write "NICK" nick
	write "USER" (nick++" 0 * :Greatest Guys bot")
	write "JOIN" chan
	asks socket >>= listen

listen :: Handle -> Net ()
listen h = forever $ do
	s <- init `fmap` io (hGetLine h)
	io (putStrLn s)
	if ping s then pong s else (if isQuit s then goodbye else (if isSeen s then doSeen s else eval (clean s)))
	where
		forever a = a >> forever a
		clean ss  = (drop 1 . dropWhile (/= ':') . drop 1 $ ss, drop 1 . takeWhile (/= '!') $ ss)
		ping x    = "PING :" `isPrefixOf` x
		pong x    = write "PONG" (':' : drop 6 x)

isQuit :: String -> Bool
isQuit = elem "QUIT" . words

isSeen :: String -> Bool
isSeen s = take 39 s == ":crafting.dangerbear.in 317 zhenya_bot "

doSeen :: String -> Net ()
doSeen s = privmsg $ seen name secs
   where name = takeWhile (not . isSpace) . drop 39 $ s 
         secs = read . takeWhile (isDigit) . drop 1 . dropWhile (not . isSpace) . drop 39 $ s :: Integer

isPrivMsg :: String -> Bool
isPrivMsg = elem "PRIVMSG" . words


addToLogs :: String -> Log -> Log
addToLogs s l = (source, message):l
	where 	source = (takeWhile (/='!') . drop 1 $ s)
		message = (dropWhile (/=':') . drop 1 $ s)

--log :: String -> Net ()
--log x =
eval :: (String, String) -> Net ()
eval ("!quit", _)                    = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval ("!uptime", _)                  = uptime >>= privmsg 
eval ("!ascend", person)             = giveop person >>  privmsg ("Welcome to the ranks of the Ascended, Brother " ++ person ++ ".")
eval ("!ding", person)               = giveop person
eval (x, _) | "!id " `isPrefixOf` x  = privmsg (drop 4 x)
            | "!roll" `isPrefixOf` x = io (randomI . takeWhile (isDigit) . dropWhile (not . isDigit) $ x) >>= privmsg
	    | "zhenya_bot" `isPrefixOf` x = privmsg "hm?"
            | "!seen " `isPrefixOf` x     = whois (drop 6 x)
 	    | match x pigup 0.3      = privmsg "Jolly good pigups, jolly good."
            | match x bug 0.5        = privmsg "Noted."
	    | match x meat 0.2       = privmsg "Can we go to DQ?"
eval (_, _)                          = return ()

pigup = "pigups"
meat = "cheap meat"
bug  = "you can't dry a bug!"

goodbye = privmsg "And now his watch is ended."

match :: String -> String -> Float -> Bool
match input s f = (fromIntegral $ edist ni si) <= ((fromIntegral $ length si) * f)
	where	ni = map toLower . trim isLetter $ input
		si = trim isLetter s

trim :: (Char -> Bool) -> String -> String
trim f ""     = ""
trim f (x:xs) = if f x then x:(trim f xs) else trim f xs

randomI :: String -> IO String
randomI [] = return "Pigup Pigup Pigup"
randomI i  = (fmap (randomR (1, (read i :: Int))) newStdGen) >>= (\x -> return $ show $ fst x) 

giveop :: String -> Net ()
giveop s = write "MODE" (chan ++ " +o " ++ s)

whois :: String -> Net ()
whois s = write "WHOIS" (" " ++ (dropWhile (isSpace) s))

privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

write :: String -> String -> Net ()
write s t = do
	h <- asks socket
	io $ hPrintf h "%s %s\r\n" s t
	io $ printf    "> %s %s\n" s t

io :: IO a -> Net a
io = liftIO

uptime :: Net String
uptime = do
	now <- io getClockTime
	zero <- asks starttime
	return . pretty $ diffClockTimes now zero

seen :: String -> Integer -> String
seen s i = "Last saw "++ s ++ " " ++ (pretty $ diffClockTimes (TOD i 0) (TOD 0 0)) ++ " ago."

pretty :: TimeDiff -> String
pretty td = unwords $ map (uncurry (++) . first show) $ if null diffs then [(0, "s ")] else diffs
	where
		merge (tot,acc) (sec,typ) = let (sec', tot') = divMod tot sec in (tot', (sec', typ):acc)
		metrics = [(86400,"d"),(3600,"h"),(60,"m"),(1,"s")]
		diffs = filter ((/=0) . fst) $ reverse $ snd $ foldl' merge (tdSec td, []) metrics


edist :: Eq a => [a] -> [a] -> Int
edist a b 
    = last (if lab == 0 then mainDiag
	    else if lab > 0 then lowers !! (lab - 1)
		 else{- < 0 -}   uppers !! (-1 - lab))
    where mainDiag = oneDiag a b (head uppers) (-1 : head lowers)
	  uppers = eachDiag a b (mainDiag : uppers) -- upper diagonals
	  lowers = eachDiag b a (mainDiag : lowers) -- lower diagonals
	  eachDiag a [] diags = []
	  eachDiag a (bch:bs) (lastDiag:diags) = oneDiag a bs nextDiag lastDiag : eachDiag a bs diags
	      where nextDiag = head (tail diags)
	  oneDiag a b diagAbove diagBelow = thisdiag
	      where doDiag [] b nw n w = []
		    doDiag a [] nw n w = []
		    doDiag (ach:as) (bch:bs) nw n w = me : (doDiag as bs me (tail n) (tail w))
			where me = if ach == bch then nw else 1 + min3 (head w) nw (head n)
		    firstelt = 1 + head diagBelow
		    thisdiag = firstelt : doDiag a b firstelt diagAbove (tail diagBelow)
	  lab = length a - length b
          min3 x y z = if x < y then x else min y z
