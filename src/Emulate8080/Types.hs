module Emulate8080.Types (
  Address, Byte, OpCode, bytesToAddress, addressToBytes,
  Accumulator, ProgramCounter, StackPointer,
  Computer(..), mkComputer,
  CPU(..), Register(..), RegisterPair(..), Flag(..), getFlag, setFlag,
  RAM(..), VRAM(..)
) where

import Data.Array.Unboxed
import Data.Bits (testBit, setBit, clearBit, (.&.), shift)
import Data.Word (Word8, Word16)
import Text.Printf

type Address = Word16
type Byte = Word8
type OpCode = Byte

bytesToAddress :: Byte -> Byte -> Address
bytesToAddress low high = fromIntegral $ (toInteger low) + 0x0100 * (toInteger high)

addressToBytes :: Address -> (Byte, Byte)
addressToBytes addr = (low, high)
  where low = fromIntegral $ addr .&. 0x00FF
        high = fromIntegral $ shift (addr .&. 0xFF00) (-8)

type Accumulator = Byte
type ProgramCounter = Address
type StackPointer = Address

--------------------------------------------------------------------------------
-- Computer
--------------------------------------------------------------------------------

data Computer = Computer {
  compCPU :: CPU,
  compRAM :: VRAM
  } deriving (Eq)

mkComputer :: Word16 -> Computer
mkComputer size = Computer mkCPU (mkRAM size)

instance Show Computer where
  show cs = concat [
    show (compCPU cs),
    "\n",
    show (compRAM cs)]

--------------------------------------------------------------------------------
-- CPU
--------------------------------------------------------------------------------

data Register = PSW | A | B | C | D | E | H | L deriving (Show, Eq)
-- ^ The Register type is used to reference CPU registers

data RegisterPair = PC | SP | BC | DE | HL | PSWA deriving (Show, Eq)
-- ^ The RegisterPair type is used to reference CPU register pairs

data Flag = Sign | Zero | AuxCarry | Parity | Carry deriving (Show, Eq)
-- ^ The Flag type is used to reference CPU flags.

getFlag :: Flag -> Byte -> Bool
-- ^ Flag bits: SZ0A0P1C (Sign, Zero, 0, Aux Carry, 0, Parity, 1, Carry)
getFlag Sign byte = testBit byte 7
getFlag Zero byte = testBit byte 6
getFlag AuxCarry byte = testBit byte 4
getFlag Parity byte = testBit byte 2
getFlag Carry byte = testBit byte 0

setFlag :: Flag -> Bool -> Byte -> Byte
-- ^ Flag bits: SZ0A0P1C (Sign, Zero, 0, Aux Carry, 0, Parity, 1, Carry)
setFlag Sign set byte = (if set then setBit else clearBit) byte 7
setFlag Zero set byte = (if set then setBit else clearBit) byte 6
setFlag AuxCarry set byte = (if set then setBit else clearBit) byte 4
setFlag Parity set byte = (if set then setBit else clearBit) byte 2
setFlag Carry set byte = (if set then setBit else clearBit) byte 0

data CPU = CPU {
  cpuPC :: ProgramCounter,
  cpuSP :: StackPointer,
  cpuPSW :: Byte,
  cpuINTE :: Bool,
  cpuA :: Accumulator,
  cpuB :: Byte,
  cpuC :: Byte,
  cpuD :: Byte,
  cpuE :: Byte,
  cpuH :: Byte,
  cpuL :: Byte
  } deriving (Eq)

mkCPU :: CPU
mkCPU = CPU 0 0x0200 2 False 0 0 0 0 0 0 0

instance Show CPU where
  show cpu = unlines [
    printf "PC:   0x%04X %5d" (cpuPC cpu) (cpuPC cpu),
    printf "SP:   0x%04X %5d" (cpuSP cpu) (cpuSP cpu),
    printf "PSW:  SZ0A0P1C",
    printf "      %d%d%d%d%d%d%d%d" (f 7) (f 6) (f 5) (f 4) (f 3) (f 2) (f 1) (f 0),
    printf "INTE: %s" (show (cpuINTE cpu)),
    printf "A:    0x%02X   %5d" (cpuA cpu) (cpuA cpu),
    printf "B:    0x%02X   %5d" (cpuB cpu) (cpuB cpu),
    printf "C:    0x%02X   %5d" (cpuC cpu) (cpuC cpu),
    printf "D:    0x%02X   %5d" (cpuD cpu) (cpuD cpu),
    printf "E:    0x%02X   %5d" (cpuE cpu) (cpuE cpu),
    printf "H:    0x%02X   %5d" (cpuH cpu) (cpuH cpu),
    printf "L:    0x%02X   %5d" (cpuL cpu) (cpuL cpu)]
    where f bit = if testBit (cpuPSW cpu) bit then 1 else 0 :: Int

--------------------------------------------------------------------------------
-- RAM
--------------------------------------------------------------------------------

class RAM m where
  -- ^ Class for 64k memories. Functions to read and write.
  bytes :: m -> [Byte]
  getByte :: Address -> m -> Byte
  putByte :: Address -> Byte -> m -> m
  putBytes :: Address -> [Byte] -> m -> m

newtype VRAM = VRAM (UArray Address Byte) deriving (Eq)

mkRAM :: Word16 -> VRAM
mkRAM size = VRAM $ listArray (0x0000, size) [0 | i <- [0x0000..size]]

instance RAM VRAM where
  bytes (VRAM mem) = elems mem
  getByte addr (VRAM mem) = mem ! addr
  putByte addr byte (VRAM mem) = VRAM (mem // [(addr, byte)])
  putBytes addr bytes (VRAM mem) = VRAM (mem // zip (iterate (+1) addr) bytes)

box :: Int -> [a] -> [[a]]
box i ls = take i ls : if length ls > i then box i (drop i ls) else []

instance Show VRAM where
  show ram = unlines $ map showRow (zip [0,lineWidth..] rows)
    where rows = box lineWidth (fmap (printf "%02X") (bytes ram))
          showRow (addr, ds) = printf "0x%04X %s" (addr :: Int) (unwords ds)
          lineWidth = 16
