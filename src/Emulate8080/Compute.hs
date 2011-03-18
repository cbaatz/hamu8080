module Emulate8080.Compute (
  runComputer, loadProgram
  ) where

import Control.Monad (liftM, liftM2, liftM3)
import Data.Bits (shift, complement, rotate, setBit, clearBit, testBit,
                  (.&.), (.|.), xor)
import Control.Monad.State (execState, gets, modify, when, unless)

import Emulate8080.Types

loadProgram :: Address -> [Byte] -> Computer -> Computer
loadProgram addr program (Computer c m) = Computer c (putBytes addr program m)

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
halting x = False

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

modifyCPU :: (CPU -> CPU) -> Computation ()
modifyCPU f = modify $ \(Computer c m) -> Computer (f c) m

modifyRAM :: (VRAM -> VRAM) -> Computation ()
modifyRAM f = modify $ \(Computer c m) -> Computer c (f m)

setReg :: Register -> Byte -> Computation ()
setReg PSW byte = modifyCPU $ \c -> c { cpuPSW = byte }
setReg A byte = modifyCPU $ \c -> c { cpuA = byte }
setReg B byte = modifyCPU $ \c -> c { cpuB = byte }
setReg C byte = modifyCPU $ \c -> c { cpuC = byte }
setReg D byte = modifyCPU $ \c -> c { cpuD = byte }
setReg E byte = modifyCPU $ \c -> c { cpuE = byte }
setReg H byte = modifyCPU $ \c -> c { cpuH = byte }
setReg L byte = modifyCPU $ \c -> c { cpuL = byte }

getReg :: Register -> Computation Byte
getReg PSW = gets $ cpuPSW . compCPU
getReg A = gets $ cpuA . compCPU
getReg B = gets $ cpuB . compCPU
getReg C = gets $ cpuC . compCPU
getReg D = gets $ cpuD . compCPU
getReg E = gets $ cpuE . compCPU
getReg H = gets $ cpuH . compCPU
getReg L = gets $ cpuL . compCPU

getRegPair :: RegisterPair -> Computation Address
getRegPair PC = gets $ cpuPC . compCPU
getRegPair SP = gets $ cpuSP . compCPU
getRegPair BC = liftM2 bytesToAddress (getReg C) (getReg B)
getRegPair DE = liftM2 bytesToAddress (getReg E) (getReg D)
getRegPair HL = liftM2 bytesToAddress (getReg L) (getReg H)
getRegPair PSWA = liftM2 bytesToAddress (getReg A) (getReg PSW)

setRegPair :: RegisterPair -> Address -> Computation ()
setRegPair PC addr = modifyCPU $ \c -> c { cpuPC = addr }
setRegPair SP addr = modifyCPU $ \c -> c { cpuSP = addr }
setRegPair BC addr = setReg C l >> setReg B h
  where (l, h) = addressToBytes addr
setRegPair DE addr = setReg E l >> setReg D h
  where (l, h) = addressToBytes addr
setRegPair HL addr = setReg L l >> setReg H h
  where (l, h) = addressToBytes addr
setRegPair PSWA addr = setReg A l >> setReg PSW h
  where (l, h) = addressToBytes addr

getF :: Flag -> Computation Bool
getF x = gets $ (getFlag x) . cpuPSW . compCPU

setF :: Flag -> Bool -> Computation ()
setF x set = modifyCPU $ \c -> c { cpuPSW = setFlag x set (cpuPSW c) }

incPC :: Computation ()
incPC = modifyCPU $ \c -> c { cpuPC = cpuPC c + 1}

setMem :: Address -> Byte -> Computation ()
setMem addr byte = modifyRAM (putByte addr byte)

getMem :: Address -> Computation Byte
getMem addr = gets $ getByte addr . compRAM

readNextByte :: Computation Byte
readNextByte = do
  op <- getRegPair PC >>= getMem
  incPC
  return op

readNextAddress :: Computation Address
-- ^ Read next two bytes little endian
readNextAddress = do
  low <- getRegPair PC >>= getMem
  incPC
  high <- getRegPair PC >>= getMem
  return (bytesToAddress low high)

halt :: Computation Bool
halt = return False

continue :: Computation Bool
continue = return True

--------------------------------------------------------------------------------
-- Branching and stack helper functions
--------------------------------------------------------------------------------

ret :: Computation ()
ret = popStack >>= setRegPair PC

jmp :: Computation ()
jmp = readNextAddress >>= setRegPair PC

call :: Computation ()
call = getRegPair PC >>= pushStack >> jmp

pushStack :: Address -> Computation ()
pushStack bytes = do
  let (l, h) = addressToBytes bytes
  addr <- getRegPair SP
  setMem (addr - 1) h -- Store higher byte
  setMem (addr - 2) l -- Store lower byte
  setRegPair SP (addr - 2)

popStack :: Computation Address
popStack = do
  addr <- getRegPair SP
  setRegPair SP (addr + 2)
  liftM2 bytesToAddress (getMem addr) (getMem (addr + 1))

--------------------------------------------------------------------------------
-- Arithmetic helper functions
--------------------------------------------------------------------------------

parityBit :: Byte -> Bool
parityBit byte = even (sum [if testBit byte i then 1 else 0 | i <- [0..7]])

setFlags :: (Byte, Bool) -> Computation ()
setFlags (byte, carry) = do
  setF Sign (testBit byte 7)
  setF Zero (byte == 0)
  setF Parity (parityBit byte)
  setF Carry carry

setResult :: Byte -> Computation ()
setResult byte = setResultC (byte, False)

setResultC :: (Byte, Bool) -> Computation ()
setResultC (byte, carry) = setFlags (byte, carry) >> setReg A byte

add :: Byte -> Byte -> (Byte, Bool)
add a b = addc False a b

addc :: Bool -> Byte -> Byte -> (Byte, Bool)
-- ^ Add with carry
addc cin a b = (fromIntegral sum, sum > 0xFF)
  where sum = (if cin then 1 else 0) + toInteger a + toInteger b

addxc :: Bool -> Address -> Address -> (Address, Bool)
-- ^ Add with carry
addxc cin a b = (fromIntegral sum, sum > 0xFFFF)
  where sum = (if cin then 1 else 0) + toInteger a + toInteger b

sub :: Byte -> Byte -> (Byte, Bool)
sub a b = subb False a b

subb :: Bool -> Byte -> Byte -> (Byte, Bool)
-- ^ Subtract with borrow
subb bin a b = (sum, not carry)
  where (sum, carry) = addc (not bin) a (complement b)

incr :: Computation Byte -> (Byte -> Computation ()) -> Computation ()
incr getByte setByte = liftM (+1) getByte >>= \b -> setFlags (b, False) >> setByte b

decr :: Computation Byte -> (Byte -> Computation ()) -> Computation ()
decr getByte setByte = liftM (subtract 1) getByte >>= \b -> setFlags (b, False) >> setByte b

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------
-- See http://www.pastraiser.com/cpu/i8080/i8080_opcodes.html
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

doOp 0x80 = liftM2 add (getReg A) (getReg B) >>= setResultC -- ADD A, B
doOp 0x81 = liftM2 add (getReg A) (getReg C) >>= setResultC -- ADD A, C
doOp 0x82 = liftM2 add (getReg A) (getReg D) >>= setResultC -- ADD A, D
doOp 0x83 = liftM2 add (getReg A) (getReg E) >>= setResultC -- ADD A, E
doOp 0x84 = liftM2 add (getReg A) (getReg H) >>= setResultC -- ADD A, H
doOp 0x85 = liftM2 add (getReg A) (getReg L) >>= setResultC -- ADD A, L
doOp 0x86 = liftM2 add (getReg A) (getRegPair HL >>= getMem) >>= setResultC -- ADD A, [HL]
doOp 0x87 = liftM2 add (getReg A) (getReg A) >>= setResultC -- ADD A, A

doOp 0x88 = liftM3 addc (getF Carry) (getReg A) (getReg B) >>= setResultC -- ADC A, B
doOp 0x89 = liftM3 addc (getF Carry) (getReg A) (getReg C) >>= setResultC -- ADC A, C
doOp 0x8A = liftM3 addc (getF Carry) (getReg A) (getReg D) >>= setResultC -- ADC A, D
doOp 0x8B = liftM3 addc (getF Carry) (getReg A) (getReg E) >>= setResultC -- ADC A, E
doOp 0x8C = liftM3 addc (getF Carry) (getReg A) (getReg H) >>= setResultC -- ADC A, H
doOp 0x8D = liftM3 addc (getF Carry) (getReg A) (getReg L) >>= setResultC -- ADC A, L
doOp 0x8E = liftM3 addc (getF Carry) (getReg A) (getRegPair HL >>= getMem) >>= setResultC -- ADC A, [HL]
doOp 0x8F = liftM3 addc (getF Carry) (getReg A) (getReg A) >>= setResultC -- ADC A, A

doOp 0x90 = liftM2 sub (getReg A) (getReg B) >>= setResultC -- SUB A, B
doOp 0x91 = liftM2 sub (getReg A) (getReg C) >>= setResultC -- SUB A, C
doOp 0x92 = liftM2 sub (getReg A) (getReg D) >>= setResultC -- SUB A, D
doOp 0x93 = liftM2 sub (getReg A) (getReg E) >>= setResultC -- SUB A, E
doOp 0x94 = liftM2 sub (getReg A) (getReg H) >>= setResultC -- SUB A, H
doOp 0x95 = liftM2 sub (getReg A) (getReg L) >>= setResultC -- SUB A, L
doOp 0x96 = liftM2 sub (getReg L) (getRegPair HL >>= getMem) >>= setResultC -- SUB A, [HL]
doOp 0x97 = liftM2 sub (getReg A) (getReg A) >>= setResultC -- SUB A, A

doOp 0x98 = liftM3 subb (getF Carry) (getReg A) (getReg B) >>= setResultC -- SBB A, B
doOp 0x99 = liftM3 subb (getF Carry) (getReg A) (getReg C) >>= setResultC -- SBB A, C
doOp 0x9A = liftM3 subb (getF Carry) (getReg A) (getReg D) >>= setResultC -- SBB A, D
doOp 0x9B = liftM3 subb (getF Carry) (getReg A) (getReg E) >>= setResultC -- SBB A, E
doOp 0x9C = liftM3 subb (getF Carry) (getReg A) (getReg H) >>= setResultC -- SBB A, H
doOp 0x9D = liftM3 subb (getF Carry) (getReg A) (getReg L) >>= setResultC -- SBB A, L
doOp 0x9E = liftM3 subb (getF Carry) (getReg B) (getRegPair HL >>= getMem) >>= setResultC -- SBB A, B
doOp 0x9F = liftM3 subb (getF Carry) (getReg A) (getReg A) >>= setResultC -- SBB A, A

doOp 0xA0 = liftM2 (.&.) (getReg A) (getReg B) >>= setResult -- AND A, B
doOp 0xA1 = liftM2 (.&.) (getReg A) (getReg C) >>= setResult -- AND A, C
doOp 0xA2 = liftM2 (.&.) (getReg A) (getReg D) >>= setResult -- AND A, D
doOp 0xA3 = liftM2 (.&.) (getReg A) (getReg E) >>= setResult -- AND A, E
doOp 0xA4 = liftM2 (.&.) (getReg A) (getReg H) >>= setResult -- AND A, H
doOp 0xA5 = liftM2 (.&.) (getReg A) (getReg L) >>= setResult -- AND A, L
doOp 0xA6 = liftM2 (.&.) (getReg A) (getRegPair HL >>= getMem) >>= setResult -- AND A, [HL]
doOp 0xA7 = liftM2 (.&.) (getReg A) (getReg A) >>= setResult -- AND A, A

doOp 0xA8 = liftM2 xor (getReg A) (getReg B) >>= setResult -- XOR A, B
doOp 0xA9 = liftM2 xor (getReg A) (getReg C) >>= setResult -- XOR A, C
doOp 0xAA = liftM2 xor (getReg A) (getReg D) >>= setResult -- XOR A, D
doOp 0xAB = liftM2 xor (getReg A) (getReg E) >>= setResult -- XOR A, E
doOp 0xAC = liftM2 xor (getReg A) (getReg H) >>= setResult -- XOR A, H
doOp 0xAD = liftM2 xor (getReg A) (getReg L) >>= setResult -- XOR A, L
doOp 0xAE = liftM2 xor (getReg A) (getRegPair HL >>= getMem) >>= setResult -- XOR A, [HL]
doOp 0xAF = liftM2 xor (getReg A) (getReg A) >>= setResult -- XOR A, A

doOp 0xB0 = liftM2 (.|.) (getReg A) (getReg B) >>= setResult -- OR A, B
doOp 0xB1 = liftM2 (.|.) (getReg A) (getReg C) >>= setResult -- OR A, C
doOp 0xB2 = liftM2 (.|.) (getReg A) (getReg D) >>= setResult -- OR A, D
doOp 0xB3 = liftM2 (.|.) (getReg A) (getReg E) >>= setResult -- OR A, E
doOp 0xB4 = liftM2 (.|.) (getReg A) (getReg H) >>= setResult -- OR A, H
doOp 0xB5 = liftM2 (.|.) (getReg A) (getReg L) >>= setResult -- OR A, L
doOp 0xB6 = liftM2 (.|.) (getReg A) (getRegPair HL >>= getMem) >>= setResult -- OR A, [HL]
doOp 0xB7 = liftM2 (.|.) (getReg A) (getReg A) >>= setResult -- OR A, A

doOp 0xB8 = liftM2 sub (getReg A) (getReg B) >>= setFlags -- CMP A, B
doOp 0xB9 = liftM2 sub (getReg A) (getReg C) >>= setFlags -- CMP A, C
doOp 0xBA = liftM2 sub (getReg A) (getReg D) >>= setFlags -- CMP A, D
doOp 0xBB = liftM2 sub (getReg A) (getReg E) >>= setFlags -- CMP A, E
doOp 0xBC = liftM2 sub (getReg A) (getReg H) >>= setFlags -- CMP A, H
doOp 0xBD = liftM2 sub (getReg A) (getReg L) >>= setFlags -- CMP A, L
doOp 0xBE = liftM2 sub (getReg A) (getRegPair HL >>= getMem) >>= setFlags -- CMP A, [HL]
doOp 0xBF = liftM2 sub (getReg A) (getReg A) >>= setFlags -- CMP A, A

doOp 0xC6 = liftM2 add (getReg A) readNextByte >>= setResultC -- ADI A, xx
doOp 0xCE = liftM3 addc (getF Carry) (getReg A) readNextByte >>= setResultC -- ACI A, xx
doOp 0xD6 = liftM2 sub (getReg A) readNextByte >>= setResultC -- SUI A, xx
doOp 0xDE = liftM3 subb (getF Carry) (getReg A) readNextByte >>= setResultC -- SBI A, xx
doOp 0xE6 = liftM2 (.&.) (getReg A) readNextByte >>= setResult -- ANI A, xx
doOp 0xEE = liftM2 xor (getReg A) readNextByte >>= setResult -- XRI A, xx
doOp 0xF6 = liftM2 (.|.) (getReg A) readNextByte >>= setResult -- ORI A, xx
doOp 0xFE = liftM2 sub (getReg A) readNextByte >>= setFlags -- CPI A, xx

doOp 0x27 = return () -- TODO: DAA Decimal Adjust Accumulator
doOp 0x2F = liftM complement (getReg A) >>= setReg A -- CMA Complement Accumulator

-- INR and DCR
doOp 0x04 = incr (getReg B) (setReg B) -- INR B
doOp 0x05 = decr (getReg B) (setReg B) -- DCR B
doOp 0x0C = incr (getReg C) (setReg C) -- INR C
doOp 0x0D = decr (getReg C) (setReg C) -- DCR C
doOp 0x14 = incr (getReg D) (setReg D) -- INR D
doOp 0x15 = decr (getReg D) (setReg D) -- DCR D
doOp 0x1C = incr (getReg E) (setReg E) -- INR E
doOp 0x1D = decr (getReg E) (setReg E) -- DCR E
doOp 0x34 = incr (getRegPair HL >>= getMem) (\b -> getRegPair HL >>= flip setMem b) -- INR [HL]
doOp 0x35 = decr (getRegPair HL >>= getMem) (\b -> getRegPair HL >>= flip setMem b) -- DCR [HL]
doOp 0x3C = incr (getReg A) (setReg A) -- INR A
doOp 0x3D = decr (getReg A) (setReg A) -- DCR A

-- Rotate accumulator
doOp 0x07 = do -- RLC Rotate Accumulator left
  byte <- getReg A
  setF Carry (testBit byte 7)
  setReg A (rotate byte 1)
doOp 0x0F = do -- RRC Rotate Accumulator right
  byte <- getReg A
  setF Carry (testBit byte 0)
  setReg A (rotate byte (-1))
doOp 0x17 = do -- RAL Rotate Accumulator left through carry
  byte <- getReg A
  carry <- getF Carry
  let byte' = (if carry then setBit else clearBit) (rotate byte 1) 0
      carry' = testBit byte 7
  setF Carry carry'
  setReg A byte'
doOp 0x1F = do -- RAR Rotate Accumulator right through carry
  byte <- getReg A
  carry <- getF Carry
  let byte' = (if carry then setBit else clearBit) (rotate byte (-1)) 7
      carry' = testBit byte 0
  setF Carry carry'
  setReg A byte'

-- PUSH
doOp 0xC5 = getRegPair BC >>= pushStack -- PUSH BC
doOp 0xD5 = getRegPair DE >>= pushStack -- PUSH DE
doOp 0xE5 = getRegPair HL >>= pushStack -- PUSH HL
doOp 0xF5 = getRegPair PSWA >>= pushStack -- PUSH PSW

-- POP
doOp 0xC1 = popStack >>= setRegPair BC -- POP BC
doOp 0xD1 = popStack >>= setRegPair DE -- POP DE
doOp 0xE1 = popStack >>= setRegPair HL -- POP HL
doOp 0xF1 = popStack >>= setRegPair PSWA -- POP PSW

-- LXI
doOp 0x01 = readNextAddress >>= setRegPair BC -- LXI BC, xxxx
doOp 0x11 = readNextAddress >>= setRegPair DE -- LXI DE, xxxx
doOp 0x21 = readNextAddress >>= setRegPair HL -- LXI HL, xxxx
doOp 0x31 = readNextAddress >>= setRegPair SP -- LXI SP, xxxx

-- INX and DCX
doOp 0x03 = liftM (+1) (getRegPair BC) >>= setRegPair BC -- INX BC
doOp 0x13 = liftM (+1) (getRegPair DE) >>= setRegPair DE -- INX DE
doOp 0x23 = liftM (+1) (getRegPair HL) >>= setRegPair HL -- INX DE
doOp 0x33 = liftM (+1) (getRegPair SP) >>= setRegPair SP -- INX SP

doOp 0x0B = liftM (subtract 1) (getRegPair BC) >>= setRegPair BC -- DCX BC
doOp 0x1B = liftM (subtract 1) (getRegPair DE) >>= setRegPair DE -- DCX DE
doOp 0x2B = liftM (subtract 1) (getRegPair HL) >>= setRegPair HL -- DCX HL
doOp 0x3B = liftM (subtract 1) (getRegPair SP) >>= setRegPair SP -- DCX SP

-- DAD
doOp 0x09 = do -- DAD HL, BC
  (sum, carry) <- liftM2 (addxc False) (getRegPair HL) (getRegPair BC)
  setRegPair HL sum
  setF Carry carry
doOp 0x19 = do -- DAD HL, DE
  (sum, carry) <- liftM2 (addxc False) (getRegPair HL) (getRegPair DE)
  setRegPair HL sum
  setF Carry carry
doOp 0x29 = do -- DAD HL, HL
  (sum, carry) <- liftM2 (addxc False) (getRegPair HL) (getRegPair HL)
  setRegPair HL sum
  setF Carry carry
doOp 0x39 = do -- DAD HL, SP
  (sum, carry) <- liftM2 (addxc False) (getRegPair HL) (getRegPair SP)
  setRegPair HL sum
  setF Carry carry

doOp 0x22 = do -- SHLD [aaaa], HL Store HL Direct
  addr <- readNextAddress
  getReg L >>= setMem addr
  getReg H >>= setMem (addr + 1)
doOp 0x2A = do -- LHLD HL, [aaaa] Load HL Direct
  addr <- readNextAddress
  getMem addr >>= setReg L
  getMem (addr + 1) >>= setReg H
doOp 0xE9 = getRegPair HL >>= setRegPair PC -- PCHL PC, HL Load PC from HL
doOp 0xF9 = getRegPair HL >>= setRegPair SP -- PCHL SP, HL Load SP from HL
doOp 0xE3 = do -- XTHL HL, [SP]
  sp <- getRegPair SP
  l <- getReg L
  h <- getReg H
  getMem sp >>= setReg L
  getMem (sp + 1) >>= setReg H
  setMem sp l
  setMem (sp + 1) h
doOp 0xEB = do -- XCHG HL, DE
  hl <- getRegPair HL
  getRegPair DE >>= setRegPair HL
  setRegPair DE hl

-- Return, Call, and Jump instructions

doOp 0xC9 = ret -- RET
doOp 0xC3 = jmp -- JMP aaaa
doOp 0xCD = call -- CALL aaaa

doOp 0xC0 = getF Zero >>= \z -> when (not z) ret -- RNZ
doOp 0xC2 = getF Zero >>= \z -> when (not z) jmp -- JNZ
doOp 0xC4 = getF Zero >>= \z -> when (not z) call -- CNZ

doOp 0xC8 = getF Zero >>= \z -> when z ret -- RZ
doOp 0xCA = getF Zero >>= \z -> when z jmp -- JZ
doOp 0xCC = getF Zero >>= \z -> when z call -- CZ

doOp 0xD0 = getF Carry >>= \c -> when (not c) ret -- RNC
doOp 0xD2 = getF Carry >>= \c -> when (not c) jmp -- JNC
doOp 0xD4 = getF Carry >>= \c -> when (not c) call -- CNC

doOp 0xD8 = getF Carry >>= \c -> when c ret -- RC
doOp 0xDA = getF Carry >>= \c -> when c jmp -- JC
doOp 0xDC = getF Carry >>= \c -> when c call -- CC

doOp 0xE0 = getF Parity >>= \p -> when (not p) ret -- RPO
doOp 0xE2 = getF Parity >>= \p -> when (not p) jmp -- JPO
doOp 0xE4 = getF Parity >>= \p -> when (not p) call -- CPO

doOp 0xE8 = getF Parity >>= \p -> when p ret -- RPE
doOp 0xEA = getF Parity >>= \p -> when p jmp -- JPE
doOp 0xEC = getF Parity >>= \p -> when p call -- CPE

doOp 0xF0 = getF Sign >>= \s -> when (not s) ret -- RP
doOp 0xF2 = getF Sign >>= \s -> when (not s) jmp -- JP
doOp 0xF4 = getF Sign >>= \s -> when (not s) call -- CP

doOp 0xF8 = getF Sign >>= \s -> when s ret -- RM
doOp 0xFA = getF Sign >>= \s -> when s jmp -- JM
doOp 0xFC = getF Sign >>= \s -> when s call -- CM

doOp 0xD3 = return () -- OUT pp TODO
doOp 0xDB = return () -- IN pp TODO
doOp 0xF3 = modifyCPU $ \c -> c { cpuINTE = False } -- DI Disable Interrupts
doOp 0xFB = modifyCPU $ \c -> c { cpuINTE = True } -- EI Enable Interrupts

-- Restart
doOp 0xC7 = getRegPair PC >>= pushStack >> setRegPair PC 0x0000 -- RST 0
doOp 0xCF = getRegPair PC >>= pushStack >> setRegPair PC 0x0008 -- RST 1
doOp 0xD7 = getRegPair PC >>= pushStack >> setRegPair PC 0x0010 -- RST 2
doOp 0xDF = getRegPair PC >>= pushStack >> setRegPair PC 0x0018 -- RST 3
doOp 0xE7 = getRegPair PC >>= pushStack >> setRegPair PC 0x0020 -- RST 4
doOp 0xEF = getRegPair PC >>= pushStack >> setRegPair PC 0x0028 -- RST 5
doOp 0xF7 = getRegPair PC >>= pushStack >> setRegPair PC 0x0030 -- RST 6
doOp 0xFF = getRegPair PC >>= pushStack >> setRegPair PC 0x0038 -- RST 7

-- Alternative codes
doOp 0xCB = doOp 0xC3 -- *JMP
doOp 0xD9 = doOp 0xC9 -- *RET
doOp 0xDD = doOp 0xCD -- *CALL
doOp 0xED = doOp 0xCD -- *CALL
doOp 0xFD = doOp 0xCD -- *CALL

-- NOP
doOp 0x00 = return () -- NOP
doOp x = return () -- *NOP Catch all undefined
