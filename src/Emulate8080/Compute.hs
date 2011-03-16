module Emulate8080.Compute (
  runComputer
  ) where

import Control.Monad.State (execState, gets, modify, when)

import Emulate8080.Types

runComputer :: Computer -> Computer
runComputer = execState process

process :: Computation ()
process = do
  setMem 0x0000 0x32 -- Load single opcode program into memory...
  continue <- doOp =<< readNext
  when continue process

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

modifyCPU :: (CPU -> CPU) -> Computation ()
modifyCPU f = modify $ \(Computer c m) -> Computer (f c) m

modifyRAM :: (VRAM -> VRAM) -> Computation ()
modifyRAM f = modify $ \(Computer c m) -> Computer c (f m)

setReg :: Register -> Byte -> Computation ()
setReg A byte = modifyCPU $ \c -> c { cpuA = byte }
setReg B byte = modifyCPU $ \c -> c { cpuB = byte }
setReg C byte = modifyCPU $ \c -> c { cpuC = byte }
setReg D byte = modifyCPU $ \c -> c { cpuD = byte }
setReg E byte = modifyCPU $ \c -> c { cpuE = byte }
setReg H byte = modifyCPU $ \c -> c { cpuH = byte }
setReg L byte = modifyCPU $ \c -> c { cpuL = byte }

getReg :: Register -> Computation Byte
getReg A = gets $ cpuA . compCPU
getReg B = gets $ cpuB . compCPU
getReg C = gets $ cpuC . compCPU
getReg D = gets $ cpuD . compCPU
getReg E = gets $ cpuE . compCPU
getReg H = gets $ cpuH . compCPU
getReg L = gets $ cpuL . compCPU

setSP :: Address -> Computation ()
setSP addr = modifyCPU $ \c -> c { cpuSP = addr }

getSP :: Computation Address
getSP = gets $ cpuSP . compCPU

setPC :: Address -> Computation ()
setPC addr = modifyCPU $ \c -> c { cpuPC = addr }

getPC :: Computation Address
getPC = gets $ cpuPC . compCPU

incPC :: Computation ()
incPC = modifyCPU $ \c -> c { cpuPC = 1 + cpuPC c }

setMem :: Address -> Byte -> Computation ()
setMem addr byte = modifyRAM (putByte addr byte)

getMem :: Address -> Computation Byte
getMem addr = gets $ getByte addr . compRAM

readNext :: Computation Byte
readNext = do
  op <- getPC >>= getMem
  incPC
  return op

halt :: Computation Bool
halt = return False

continue :: Computation Bool
continue = return True

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

doOp :: OpCode -> Computation Bool

doOp 0x32 = do -- LDA A [xxxx] WRONG
  setMem 0x000F 0x34
  setMem 0x00F3 0x38
  setReg A 0x11
  continue
doOp 0x76 = halt -- HLT
doOp x = halt
