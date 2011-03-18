import Control.Monad.State

import Emulate8080.Types (mkComputer)
import Emulate8080.Compute (runComputer, loadProgram)

-- Emulate an 8080 Intel processor for the following usage:
-- 1. Write program as a list of bytes (yes, pure bytecode programming!)
-- 2. Load bytes to memory
-- 3. Run emulation of CPU with memory until HALT
-- 4. Return computer (CPU and RAM state)

main = do
  let program = [0x3E, 134, 0x06, 3, 0x0E, 6, 0xB8, 0x04, 0x37, 0x1F,
                 0xC5, 0xC5, 0xE3, 0xC1, 0xFB, 0x76]
      computer = loadProgram 0x0000 program $ mkComputer 0x01FF
      result = runComputer computer
  print result
  return ()
