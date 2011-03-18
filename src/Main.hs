import Hamu8080.Types (mkComputer)
import Hamu8080.Compute (runComputer, loadProgram)

-- Emulate an 8080 Intel processor for the following usage:
-- 1. Write program as a list of bytes (yes, pure machine code
--    programming!)
-- 2. Create a computer (CPU & Memory)
-- 3. Load program into memory
-- 4. Run computer emulation until a HLT instruction is reached
-- 5. Inspect final state of computer

main :: IO ()
main = do
  let program = [0x3E, 0x9B, 0x27, 0x76]
      computer = loadProgram 0x0000 program $ mkComputer 0x01FF
      result = runComputer computer
  print result
