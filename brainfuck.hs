{-# LANGUAGE TemplateHaskell #-}
import Data.Char
import Control.Lens
import Control.Lens.TH
import Control.Monad.State
import System.Environment
import System.Exit

data Mem = Mem [Int] Int [Int]
data Instructions = Instr String Char String
data ST = ST {_mem :: Mem, _instr :: Instructions}
type Op = StateT ST IO

makeLenses ''ST

main = do
    args <- getArgs
    case args of
      [file] -> do (i:is) <- readFile file
                   runStateT interpret (ST (Mem [] 0 []) (Instr [] i is))
                   return ()
      _ -> putStrLn "Provide an bf file"

interpret :: Op ()
interpret = do
    inst@(Instr _ i _) <- use instr
    (Mem pm m nm) <- use mem
    case i of
        '>' -> case nm of
                [] -> mem ^= Mem (m:pm) 0 []
                (nextM:nextMs) -> mem ^= Mem (m:pm) nextM nextMs
        '<' -> case pm of
                [] -> undefined
                (prevM:prevMs) -> mem ^= Mem prevMs prevM (m:nm)
        '+' -> mem %= modMem (+1)
        '-' -> mem %= modMem (\x -> x-1)
        ',' -> do v <- liftIO getChar
                  mem %= modMem (\x -> ord v)
        '.' -> (liftIO . putChar . chr) m
        '[' -> when (m == 0) $ instr ^= moveForward inst (-1)
        ']' -> when (m /= 0) $ instr ^= moveBackward inst (-1)
        _   -> return ()
    stepInstr
    interpret

stepInstr :: Op ()
stepInstr = do
    (Instr prev i next) <- use instr
    case next of
     [] -> liftIO exitSuccess
     (nextI:nextIs) -> instr ^= Instr (i:prev) nextI nextIs

moveForward :: Instructions -> Int -> Instructions
moveForward instr@(Instr prevI i (nextI:nextIs)) n
 | n == 0 && i == ']' = instr
 | i == ']' = moveForward step (n-1)
 | i == '[' = moveForward step (n+1)
 | otherwise = moveForward step n
 where step = Instr (i:prevI) nextI nextIs

moveBackward :: Instructions -> Int -> Instructions
moveBackward instr@(Instr (prevI:prevIs) i nextI) n
 | n == 0 && i == '[' = instr
 | i == '[' = moveBackward step (n-1)
 | i == ']' = moveBackward step (n+1)
 | otherwise = moveBackward step n
 where step = Instr prevIs prevI (i:nextI)

modMem f (Mem pm m nm) = Mem pm (f m) nm

