import Control.Monad.State

import Emulate8080.Types (mkComputer)
import Emulate8080.Compute (runComputer)

-- Emulate an 8080 Intel processor for the following usage:
-- 1. Write Instructions in a Haskell DSL (assembly like)
-- 2. Compile instructions to bytecode
-- 3. Load bytecode to memory
-- 4. Run emulation of CPU with memory until HALT
-- 5. Return computer (CPU and RAM state)

main = do
  let computer = mkComputer 0x00FF
      -- Load computer with program here
      result = runComputer computer
  print result
  return ()

