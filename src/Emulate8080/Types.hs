module Emulate8080.Types (
  Address, Byte, OpCode, bytesToAddress,
  Accumulator, ProgramCounter, StackPointer,
  Computation,
  Computer(..), mkComputer,
  CPU(..), Register(..), RegisterPair(..),
  RAM(..), VRAM(..)
) where

import Control.Monad.State (State)
import Data.Array.Unboxed
import Data.Word (Word8, Word16)
import Text.Printf

type Address = Word16
type Byte = Word8
type OpCode = Byte

-- TODO: Cleaner way?
bytesToAddress :: Byte -> Byte -> Address
bytesToAddress low high = fromIntegral $ (toInteger low) + 0x0100 * (toInteger high)

type Accumulator = Byte
type ProgramCounter = Address
type StackPointer = Address

type Computation = State Computer

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

data Register = A | B | C | D | E | H | L deriving (Show, Eq)
-- ^ The Register type is used to reference CPU registers

data RegisterPair = BC | DE | HL deriving (Show, Eq)
-- ^ The RegisterPair type is used to reference CPU register pairs

data CPU = CPU {
  cpuPC :: ProgramCounter,
  cpuSP :: StackPointer,
  cpuA :: Accumulator,
  cpuB :: Byte,
  cpuC :: Byte,
  cpuD :: Byte,
  cpuE :: Byte,
  cpuH :: Byte,
  cpuL :: Byte
  } deriving (Eq)

mkCPU :: CPU
mkCPU = CPU 0 0 0 0 0 0 0 0 0

instance Show CPU where
  show cpu = unlines [
    printf "PC:  0x%04X %5d" (cpuPC cpu) (cpuPC cpu),
    printf "SP:  0x%04X %5d" (cpuSP cpu) (cpuSP cpu),
    printf "A:   0x%02X   %5d" (cpuA cpu) (cpuA cpu),
    printf "B:   0x%02X   %5d" (cpuB cpu) (cpuB cpu),
    printf "C:   0x%02X   %5d" (cpuC cpu) (cpuC cpu),
    printf "D:   0x%02X   %5d" (cpuD cpu) (cpuD cpu),
    printf "E:   0x%02X   %5d" (cpuE cpu) (cpuE cpu),
    printf "H:   0x%02X   %5d" (cpuH cpu) (cpuH cpu),
    printf "L:   0x%02X   %5d" (cpuL cpu) (cpuL cpu)]

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
