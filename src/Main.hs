import Control.Monad.State

import Emulate8080.Types (mkComputer)
import Emulate8080.Compute (runComputer, loadProgram)

-- Emulate an 8080 Intel processor for the following usage:
-- 1. Write Instructions in a Haskell DSL (assembly like)
-- 2. Compile instructions to bytecode
-- 3. Load bytecode to memory
-- 4. Run emulation of CPU with memory until HALT
-- 5. Return computer (CPU and RAM state)

main = do
  let computer = mkComputer 0x01FF
      computer' = loadProgram [0x06, 0x10, 0x0E, 0xA0,
                               0x16, 0x20, 0x1E, 0xF0,
                               0x79, 0x93, 0x4F, 0x78, 0x9A, 0x47] computer
      result = runComputer computer'
  print result
  return ()

