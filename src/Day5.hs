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
      newInstructions = V.update is $! V.singleton (ip, instruction+1)
  in
    case is V.!? nextIP of
      Just _    -> Machine newInstructions nextIP Running
      Nothing   -> Machine newInstructions nextIP Stopped

executeInstruction2 :: Machine -> Machine
executeInstruction2 (Machine is ip _) =
  let instruction     = is V.! ip
      nextIP          = ip + instruction
      increment       = if instruction >= 3 then (-1) else 1
      newInstructions = V.update is $! V.singleton (ip, instruction + increment)
  in
    case is V.!? nextIP of
      Just _    -> Machine newInstructions nextIP Running
      Nothing   -> Machine newInstructions nextIP Stopped

runMachine :: Machine -> (Machine, Int)
runMachine m =
  runner m 0
  where
    runner mm@(Machine _ _ Stopped) c = (mm, c)
    runner mm@(Machine _ _ Running) c = runner (executeInstruction mm) $! (c + 1)

runMachine' :: Machine -> (Machine, Int)
runMachine' m =
  runner m 0
  where
    runner :: Machine -> Int -> (Machine, Int)
    runner !mm !c = case mm of
      Machine _ _ Stopped -> (mm, c)
      Machine _ _ Running -> runner (executeInstruction mm) $! (c+1)

runMachine2 :: Machine -> (Machine, Int)
runMachine2 m =
  runner m 0
  where
    runner !mm@(Machine _ _ Stopped) !c = (mm, c)
    runner !mm@(Machine _ _ Running) !c = runner (executeInstruction2 mm) $! (c + 1)

runMachine2' :: Machine -> (Machine, Int)
runMachine2' m =
  runner m 0
  where
    runner :: Machine -> Int -> (Machine, Int)
    runner !mm !c = case mm of
      Machine _ _ Stopped -> (mm, c)
      Machine _ _ Running -> runner (executeInstruction2 mm) $! (c+1)

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
    putStrLn "Part 2"
    print $ snd $ runMachine2 machine
