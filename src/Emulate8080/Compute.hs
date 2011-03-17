module Emulate8080.Compute (
  runComputer, loadProgram, addc
  ) where

import Control.Monad (liftM2, liftM3)
import Data.Bits (shift, complement, testBit)
import Control.Monad.State (execState, gets, modify, when, unless)

import Emulate8080.Types

loadProgram :: [Byte] -> Computer -> Computer
loadProgram program (Computer c m) = Computer c (putBytes 0x000 program m)

runComputer :: Computer -> Computer
runComputer = execState process

process :: Computation ()
process = do
  op <- readNextByte
  unless (halting op) $ do
    doOp op
    process

halting :: Byte -> Bool
halting 0x76 = True
halting 0x00 = True
halting x = False

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

getRegPair :: RegisterPair -> Computation Address
getRegPair BC = liftM2 bytesToAddress (getReg C) (getReg B)
getRegPair DE = liftM2 bytesToAddress (getReg E) (getReg D)
getRegPair HL = liftM2 bytesToAddress (getReg L) (getReg H)

getF :: Flag -> Computation Bool
getF x = gets $ (getFlag x) . cpuPSW . compCPU

setF :: Flag -> Bool -> Computation ()
setF x set = modifyCPU $ \c -> c { cpuPSW = setFlag x set (cpuPSW c) }

setSP :: Address -> Computation ()
setSP addr = modifyCPU $ \c -> c { cpuSP = addr }

getSP :: Computation Address
getSP = gets $ cpuSP . compCPU

setPC :: Address -> Computation ()
setPC addr = modifyCPU $ \c -> c { cpuPC = addr }

getPC :: Computation Address
getPC = gets $ cpuPC . compCPU

incPC :: Computation ()
incPC = modifyCPU $ \c -> c { cpuPC = cpuPC c + 1}

setMem :: Address -> Byte -> Computation ()
setMem addr byte = modifyRAM (putByte addr byte)

getMem :: Address -> Computation Byte
getMem addr = gets $ getByte addr . compRAM

readNextByte :: Computation Byte
readNextByte = do
  op <- getPC >>= getMem
  incPC
  return op

readNextAddress :: Computation Address
-- ^ Read next two bytes little endian
readNextAddress = do
  low <- getPC >>= getMem
  incPC
  high <- getPC >>= getMem
  return (bytesToAddress low high)

halt :: Computation Bool
halt = return False

continue :: Computation Bool
continue = return True

--------------------------------------------------------------------------------
-- Arithmetic helper functions
--------------------------------------------------------------------------------

parityBit :: Byte -> Bool
parityBit byte = even (sum [if testBit byte i then 1 else 0 | i <- [0..7]])

setResult :: (Byte, Bool) -> Computation ()
setResult (byte, carry) = do
  setReg A byte
  setF Sign (testBit byte 7)
  setF Zero (byte == 0)
  setF Parity (parityBit byte)
  setF Carry carry

add :: Byte -> Byte -> (Byte, Bool)
add a b = addc False a b

addc :: Bool -> Byte -> Byte -> (Byte, Bool)
-- ^ Add with carry
addc cin a b = (fromIntegral sum :: Byte, sum > 255)
  where sum = (if cin then 1 else 0) + toInteger a + toInteger b

sub :: Byte -> Byte -> (Byte, Bool)
sub a b = subb False a b

subb :: Bool -> Byte -> Byte -> (Byte, Bool)
-- ^ Subtract with borrow
subb bin a b = (sum, not carry)
  where (sum, carry) = addc (not bin) a (complement b)

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------
-- Direct addressing: LDA A, [aaaa]
-- Indexed addressing: MOV B, [HL]
-- Immediate addressing: MVI B, 37h

doOp :: OpCode -> Computation ()

doOp 0x02 = getRegPair BC >>= \addr -> getReg A >>= setMem addr -- STAX [BC], A
doOp 0x12 = getRegPair DE >>= \addr -> getReg A >>= setMem addr -- STAX [DE], A
doOp 0x0A = getRegPair BC >>= getMem >>= setReg A -- LDAX A, [BC]
doOp 0x1A = getRegPair DE >>= getMem >>= setReg A -- LDAX A, [DE]

doOp 0x06 = readNextByte >>= setReg B -- MVI B, xx
doOp 0x0E = readNextByte >>= setReg C -- MVI C, xx
doOp 0x16 = readNextByte >>= setReg D -- MVI D, xx
doOp 0x1E = readNextByte >>= setReg E -- MVI E, xx
doOp 0x26 = readNextByte >>= setReg H -- MVI H, xx
doOp 0x2E = readNextByte >>= setReg L -- MVI L, xx
doOp 0x36 = getRegPair HL >>= \addr -> readNextByte >>= setMem addr -- MVI [HL], xx
doOp 0x3E = readNextByte >>= setReg A -- MVI A, xx

doOp 0x32 = readNextAddress >>= \addr -> getReg A >>= setMem addr -- STA [aaaa], A
doOp 0x3A = readNextAddress >>= getMem >>= setReg A -- LDA A, [aaaa]

doOp 0x37 = setF Carry True -- STC Set Carry flag to 1
doOp 0x3F = getF Carry >>= \set -> setF Carry (not set) -- CMC Complement Carry flag

doOp 0x40 = setReg B =<< getReg B -- MOV B, B
doOp 0x41 = setReg B =<< getReg C -- MOV B, C
doOp 0x42 = setReg B =<< getReg D -- MOV B, D
doOp 0x43 = setReg B =<< getReg E -- MOV B, E
doOp 0x44 = setReg B =<< getReg H -- MOV B, H
doOp 0x45 = setReg B =<< getReg L -- MOV B, L
doOp 0x46 = setReg B =<< getMem =<< getRegPair HL -- MOV B, [HL]
doOp 0x47 = setReg B =<< getReg A -- MOV B, A

doOp 0x48 = setReg C =<< getReg B -- MOV C, B
doOp 0x49 = setReg C =<< getReg C -- MOV C, C
doOp 0x4A = setReg C =<< getReg D -- MOV C, D
doOp 0x4B = setReg C =<< getReg E -- MOV C, E
doOp 0x4C = setReg C =<< getReg H -- MOV C, H
doOp 0x4D = setReg C =<< getReg L -- MOV C, L
doOp 0x4E = setReg C =<< getMem =<< getRegPair HL -- MOV C, [HL]
doOp 0x4F = setReg C =<< getReg A -- MOV C, A

doOp 0x50 = setReg D =<< getReg B -- MOV D, B
doOp 0x51 = setReg D =<< getReg C -- MOV D, C
doOp 0x52 = setReg D =<< getReg D -- MOV D, D
doOp 0x53 = setReg D =<< getReg E -- MOV D, E
doOp 0x54 = setReg D =<< getReg H -- MOV D, H
doOp 0x55 = setReg D =<< getReg L -- MOV D, L
doOp 0x56 = setReg D =<< getMem =<< getRegPair HL -- MOV D, [HL]
doOp 0x57 = setReg D =<< getReg A -- MOV D, A

doOp 0x58 = setReg E =<< getReg B -- MOV E, B
doOp 0x59 = setReg E =<< getReg C -- MOV E, C
doOp 0x5A = setReg E =<< getReg D -- MOV E, D
doOp 0x5B = setReg E =<< getReg E -- MOV E, E
doOp 0x5C = setReg E =<< getReg H -- MOV E, H
doOp 0x5D = setReg E =<< getReg L -- MOV E, L
doOp 0x5E = setReg E =<< getMem =<< getRegPair HL -- MOV E, [HL]
doOp 0x5F = setReg E =<< getReg A -- MOV E, A

doOp 0x60 = setReg H =<< getReg B -- MOV H, B
doOp 0x61 = setReg H =<< getReg C -- MOV H, C
doOp 0x62 = setReg H =<< getReg D -- MOV H, D
doOp 0x63 = setReg H =<< getReg E -- MOV H, E
doOp 0x64 = setReg H =<< getReg H -- MOV H, H
doOp 0x65 = setReg H =<< getReg L -- MOV H, L
doOp 0x66 = setReg H =<< getMem =<< getRegPair HL -- MOV H, [HL]
doOp 0x67 = setReg H =<< getReg A -- MOV H, A

doOp 0x68 = setReg L =<< getReg B -- MOV L, B
doOp 0x69 = setReg L =<< getReg C -- MOV L, C
doOp 0x6A = setReg L =<< getReg D -- MOV L, D
doOp 0x6B = setReg L =<< getReg E -- MOV L, E
doOp 0x6C = setReg L =<< getReg H -- MOV L, H
doOp 0x6D = setReg L =<< getReg L -- MOV L, L
doOp 0x6E = setReg L =<< getMem =<< getRegPair HL -- MOV L, [HL]
doOp 0x6F = setReg L =<< getReg A -- MOV L, A

doOp 0x70 = getRegPair HL >>= \addr -> getReg B >>= setMem addr -- MOV [HL], B
doOp 0x71 = getRegPair HL >>= \addr -> getReg C >>= setMem addr -- MOV [HL], C
doOp 0x72 = getRegPair HL >>= \addr -> getReg D >>= setMem addr -- MOV [HL], D
doOp 0x73 = getRegPair HL >>= \addr -> getReg E >>= setMem addr -- MOV [HL], E
doOp 0x74 = getRegPair HL >>= \addr -> getReg H >>= setMem addr -- MOV [HL], H
doOp 0x75 = getRegPair HL >>= \addr -> getReg L >>= setMem addr -- MOV [HL], L
doOp 0x76 = return ()                                           -- HLT
doOp 0x77 = getRegPair HL >>= \addr -> getReg A >>= setMem addr -- MOV [HL], A

doOp 0x78 = setReg A =<< getReg B -- MOV A, B
doOp 0x79 = setReg A =<< getReg C -- MOV A, C
doOp 0x7A = setReg A =<< getReg D -- MOV A, D
doOp 0x7B = setReg A =<< getReg E -- MOV A, E
doOp 0x7C = setReg A =<< getReg H -- MOV A, H
doOp 0x7D = setReg A =<< getReg L -- MOV A, L
doOp 0x7E = setReg A =<< getMem =<< getRegPair HL -- MOV A, [HL]
doOp 0x7F = setReg A =<< getReg A -- MOV A, A

doOp 0x80 = liftM2 add (getReg A) (getReg B) >>= setResult -- ADD A, B
doOp 0x81 = liftM2 add (getReg A) (getReg C) >>= setResult -- ADD A, C
doOp 0x82 = liftM2 add (getReg A) (getReg D) >>= setResult -- ADD A, D
doOp 0x83 = liftM2 add (getReg A) (getReg E) >>= setResult -- ADD A, E
doOp 0x84 = liftM2 add (getReg A) (getReg H) >>= setResult -- ADD A, H
doOp 0x85 = liftM2 add (getReg A) (getReg L) >>= setResult -- ADD A, L
doOp 0x86 = liftM2 add (getReg A) (getRegPair HL >>= getMem) >>= setResult -- ADD A, [HL]
doOp 0x87 = liftM2 add (getReg A) (getReg A) >>= setResult -- ADD A, A

doOp 0x88 = liftM3 addc (getF Carry) (getReg A) (getReg B) >>= setResult -- ADC A, B
doOp 0x89 = liftM3 addc (getF Carry) (getReg A) (getReg C) >>= setResult -- ADC A, C
doOp 0x8A = liftM3 addc (getF Carry) (getReg A) (getReg D) >>= setResult -- ADC A, D
doOp 0x8B = liftM3 addc (getF Carry) (getReg A) (getReg E) >>= setResult -- ADC A, E
doOp 0x8C = liftM3 addc (getF Carry) (getReg A) (getReg H) >>= setResult -- ADC A, H
doOp 0x8D = liftM3 addc (getF Carry) (getReg A) (getReg L) >>= setResult -- ADC A, L
doOp 0x8E = liftM3 addc (getF Carry) (getReg A) (getRegPair HL >>= getMem) >>= setResult -- ADC A, [HL]
doOp 0x8F = liftM3 addc (getF Carry) (getReg A) (getReg A) >>= setResult -- ADC A, A

doOp 0x90 = liftM2 sub (getReg A) (getReg B) >>= setResult -- SUB A, B
doOp 0x91 = liftM2 sub (getReg A) (getReg C) >>= setResult -- SUB A, C
doOp 0x92 = liftM2 sub (getReg A) (getReg D) >>= setResult -- SUB A, D
doOp 0x93 = liftM2 sub (getReg A) (getReg E) >>= setResult -- SUB A, E
doOp 0x94 = liftM2 sub (getReg A) (getReg H) >>= setResult -- SUB A, H
doOp 0x95 = liftM2 sub (getReg A) (getReg L) >>= setResult -- SUB A, L
doOp 0x96 = liftM2 sub (getReg L) (getRegPair HL >>= getMem) >>= setResult -- SUB A, [HL]
doOp 0x97 = liftM2 sub (getReg A) (getReg A) >>= setResult -- SUB A, A

doOp 0x98 = liftM3 subb (getF Carry) (getReg A) (getReg B) >>= setResult -- SBB A, B
doOp 0x99 = liftM3 subb (getF Carry) (getReg A) (getReg C) >>= setResult -- SBB A, C
doOp 0x9A = liftM3 subb (getF Carry) (getReg A) (getReg D) >>= setResult -- SBB A, D
doOp 0x9B = liftM3 subb (getF Carry) (getReg A) (getReg E) >>= setResult -- SBB A, E
doOp 0x9C = liftM3 subb (getF Carry) (getReg A) (getReg H) >>= setResult -- SBB A, H
doOp 0x9D = liftM3 subb (getF Carry) (getReg A) (getReg L) >>= setResult -- SBB A, L
doOp 0x9E = liftM3 subb (getF Carry) (getReg B) (getRegPair HL >>= getMem) >>= setResult -- SBB A, B
doOp 0x9F = liftM3 subb (getF Carry) (getReg A) (getReg A) >>= setResult -- SBB A, A

doOp 0x00 = return () -- NOP
doOp x = return () -- NOP* Catch all undefined
