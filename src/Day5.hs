module Day5 where

import qualified Data.Vector as V

data MachineState = Running | Stopped
  deriving Show

data Machine = Machine (V.Vector Int) Int MachineState
  deriving Show

executeInstruction :: Machine -> Machine
executeInstruction (Machine is ip _) =
  let instruction     = is V.! ip
      nextIP          = ip + instruction
      newInstructions = V.update is (V.singleton (ip, instruction+1))
  in
    case is V.!? nextIP of
      Just _    -> Machine newInstructions nextIP Running
      Nothing   -> Machine newInstructions nextIP Stopped

runMachine :: Machine -> (Machine, Int)
runMachine m =
  runner m 0
  where
    runner mm@(Machine _ _ Running) c = runner (executeInstruction mm) (c + 1)
    runner mm@(Machine _ _ Stopped) c = (mm, c)

withInput :: FilePath -> IO Machine
withInput path = do
  f <- readFile path
  let is = V.fromList $ map (\l -> read l :: Int) $ lines f
  return (Machine is 0 Running)


day05 :: IO ()
day05 =
  withInput "data/Day5.txt" >>= \machine -> do
    putStrLn "Part 1"
    print $ snd $ runMachine machine
