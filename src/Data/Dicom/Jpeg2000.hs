module Data.Dicom.Jpeg2000 where

import qualified Data.ByteString as B
import Data.Binary.Strict.Get
import Data.Word
import Data.Bits

-- Delimiting markers and marker segments
-- Start of Codestream
j2K_SOC :: Word16
j2K_SOC = 0xff4f

-- Start of Tile
j2K_SOT :: Word16
j2K_SOT = 0xff90

-- Start of Data
j2K_SOD :: Word16
j2K_SOD = 0xff93

-- End of Codestream
j2K_EOC :: Word16
j2K_EOC = 0xffd9

-- Fixed information marker segments
-- Image and Tile Size
j2K_SIZ :: Word16
j2K_SIZ = 0xff51

-- No capability flags
j2K_RSIZ_BASELINE :: Word16
j2K_RSIZ_BASELINE = 0x0000

-- Error resilience flag
j2K_RSIZ_ER_FLAG :: Word16
j2K_RSIZ_ER_FLAG = 0x0001

-- ROI present flag
j2K_RSIZ_ROI :: Word16
j2K_RSIZ_ROI = 0x0002

-- Functional marker segments
-- Coding style Default
j2K_COD :: Word16
j2K_COD = 0xff52

-- Coding style Component
j2K_COC :: Word16
j2K_COC = 0xff53

-- Quantization Default
j2K_QCD :: Word16
j2K_QCD = 0xff5c

-- Quantization Component
j2K_QCC :: Word16
j2K_QCC = 0xff5d

-- Quantization guard bits shift 
j2K_QCX_GB_SHIFT :: Int
j2K_QCX_GB_SHIFT = 5

-- Quantization guard bits mask 
j2K_QCX_GB_MASK :: Word8
j2K_QCX_GB_MASK = 7

-- Quantization type - Reversible
j2K_QCX_NO_QUANTIZATION :: Word8
j2K_QCX_NO_QUANTIZATION = 0

-- Quantization type - Scalar derived
j2K_QCX_SCALAR_DERIVED :: Word8
j2K_QCX_SCALAR_DERIVED = 1

-- Quantization type - Scalar exponential
j2K_QCX_SCALAR_EXPONENTIAL :: Word8
j2K_QCX_SCALAR_EXPONENTIAL = 2

-- Progression Types
-- Layer/Resolution/Component/Position progressive
j2k_PROG_LY_RES_COMP_POS :: Word8
j2k_PROG_LY_RES_COMP_POS = 0

-- Resolution/Layer/Component/Position progressive
j2k_PROG_RES_LY_COMP_POS :: Word8
j2k_PROG_RES_LY_COMP_POS = 1

-- Resolution/Position/Component/Layer progressive
j2k_PROG_RES_POS_COMP_LY :: Word8
j2k_PROG_RES_POS_COMP_LY = 2

-- Position/Component/Resolution/Layer progressive
j2k_PROG_POS_COMP_RES_LY :: Word8
j2k_PROG_POS_COMP_RES_LY = 3

-- Component/Position/Resolution/Layer progressive
j2k_PROG_COMP_POS_RES_LY :: Word8
j2k_PROG_COMP_POS_RES_LY = 4

-- Data types

-- 
data DicomJpeg2000 = DicomJpeg2000 {
  j2kSoc :: B.ByteString,
  j2kSiz :: DicomJ2kSiz,
  j2kCod :: DicomJ2kCod,
  j2kQcd :: DicomJ2kQcd,
  j2kSot :: DicomJ2kSot,
  j2kData :: DicomJ2kData,
  j2kEoc :: DicomJ2kEoc
  }

-- COD marker segment.
data DicomJ2kCod = DicomJ2kCod {
  j2kCodMarker :: B.ByteString,
  j2kCodLength :: Word16,
  j2kCodBlockStyle :: Word8,
  j2kCodProgOrder :: Word8,
  j2kCodNumLayers :: Word16,
  j2kCodMct :: Word8,
  j2kCodDecompLevels :: Word8,
  j2kCodBlockWidth :: Word8,
  j2kCodBlockHeight :: Word8,
  j2kCodPassStyle :: Word8,
  j2kCodRest :: B.ByteString
  }

-- SOD marker
data DicomJ2kData = DicomJ2kData {
  j2kDataMarker :: B.ByteString,
  j2kDataLength :: Word32,
  j2kDataBody :: B.ByteString
  }

-- EOC marker
data DicomJ2kEoc = DicomJ2kEoc {
  j2kEocMarker :: B.ByteString
  }

-- QCD marker segment.
data DicomJ2kQcd = DicomJ2kQcd {
  j2kQcdMarker :: B.ByteString,
  j2kQcdLength :: Word16,
  j2kQcdQuantStyle :: Word8,
  j2kQcdSubBands :: B.ByteString
  }

-- SIZ marker segment. Required in main header immediately after SOC marker
data DicomJ2kSiz = DicomJ2kSiz {
  j2kSizMarker :: B.ByteString,
  j2kSizLength :: Word16,
  j2kSizCap :: Word16,
  j2kImageXSize :: Word32,
  j2kImageYSize :: Word32,
  j2kImageXOffset :: Word32,
  j2kImageYOffset :: Word32,
  j2kTileXSize :: Word32,
  j2kTileYSize :: Word32,
  j2kTileXOffset :: Word32,
  j2kTileYOffset :: Word32,
  j2kNComp :: Word16,
  j2kComponents :: DicomJ2kSizComp
  }

data DicomJ2kSizComp = DicomJ2kSizComp {
  j2kSizBitDepth :: Word8,
  j2kSizXRSiz :: Word8,
  j2kSizYRSiz :: Word8
  }

-- SOT marker segment.
data DicomJ2kSot = DicomJ2kSot {
  j2kSotMarker :: B.ByteString,
  j2kSotLength :: Word16,
  j2kSotTileIndex :: Word16,
  j2kSotTileLength :: Word32,
  j2kSotTilePart :: Word8,
  j2kSotTileNParts :: Word8
  }

-- 
data DicomJ2kSegment = DicomJ2kSegment {
  j2kSegMarker :: B.ByteString,
  j2kSegLength :: Word16,
  j2kSegData :: B.ByteString
  }

-- Parsing functions

-- JPEG2000
getJpeg2000 :: Get DicomJpeg2000
getJpeg2000 = do
  j2kSoc <- getByteString 2
  j2kSiz <- getJ2kSiz
  j2kCod <- getJ2kCod
  j2kQcd <- getJ2kQcd j2kCod
  -- Header ends when SOT marker segment found
  j2kSot <- getJ2kSot
  j2kData <- getJ2kData j2kSot
  j2kEoc <- getJ2kEoc
  return (DicomJpeg2000 j2kSoc j2kSiz j2kCod j2kQcd j2kSot j2kData j2kEoc)

-- COD segment
getJ2kCod :: Get DicomJ2kCod
getJ2kCod = do
  marker <- getByteString 2
  len <- getWord16be
  blockStyle <- getWord8
  progOrder <- getWord8
  numLayers <- getWord16be
  mct <- getWord8
  decompLevels <- getWord8
  blockWidth <- getWord8
  blockHeight <- getWord8
  passStyle <- getWord8
  rest <- getByteString 1
  return (DicomJ2kCod marker len blockStyle progOrder numLayers mct decompLevels
    blockWidth blockHeight passStyle rest)

-- Data segment
getJ2kData :: DicomJ2kSot -> Get DicomJ2kData
getJ2kData sot = do
  marker <- getByteString 2
  let len = j2kSotTileLength sot
  -- SOT length is always 10 and tile length is from start of SOT
  raw <- getByteString $ fromIntegral (len - 14)
  return (DicomJ2kData marker (fromIntegral len) raw)

-- EOC
getJ2kEoc ::Get DicomJ2kEoc
getJ2kEoc = do
  marker <- getByteString 2
  return (DicomJ2kEoc marker)

-- QCD segment
getJ2kQcd :: DicomJ2kCod -> Get DicomJ2kQcd
getJ2kQcd cod = do
  marker <- getByteString 2
  len <- getWord16be
  startByte <- bytesRead
  quantStyle <- getWord8
  -- See HeaderDecoder line ~776 for subband info
  currByte <- bytesRead
  rest <- getByteString $ fromIntegral (len - 2) - (currByte - startByte)
  return (DicomJ2kQcd marker len quantStyle rest)

-- SIZ segment
getJ2kSiz :: Get DicomJ2kSiz
getJ2kSiz = do
  sizMarker <- getByteString 2
  sizLen <- getWord16be
  sizCap <- getWord16be
  sizXSize <- getWord32be
  sizYSize <- getWord32be
  sizXOffset <- getWord32be
  sizYOffset <- getWord32be
  sizTileXSize <- getWord32be
  sizTileYSize <- getWord32be
  sizTileXOffset <- getWord32be
  sizTileYOffset <- getWord32be
  sizNComp <- getWord16be
  sizComps <- getJ2kSizComp
  return (DicomJ2kSiz sizMarker sizLen sizCap sizXSize sizYSize sizXOffset sizYOffset
    sizTileXSize sizTileYSize sizTileXOffset sizTileYOffset sizNComp sizComps)

-- SIZ components
getJ2kSizComp :: Get DicomJ2kSizComp
getJ2kSizComp = do
  sizDepth <- getWord8
  sizX <- getWord8
  sizY <- getWord8
  return (DicomJ2kSizComp sizDepth sizX sizY)

-- SOT segment
getJ2kSot :: Get DicomJ2kSot
getJ2kSot = do
  marker <- getByteString 2
  len <- getWord16be
  -- See FileBitStreamReaderAgent line ~609
  startByte <- bytesRead
  tileIndex <- getWord16be
  tileLength <- getWord32be
  tilePart <- getWord8
  tileNParts <- getWord8
  return (DicomJ2kSot marker len tileIndex tileLength tilePart tileNParts)

-- 
getJ2kSegment :: Get DicomJ2kSegment
getJ2kSegment = do
  segMarker <- getByteString 2
  segLen <- getWord16be
  segData <- getByteString $ fromIntegral (segLen - 2)
  return (DicomJ2kSegment segMarker segLen segData)

-- Public interface
-- QCD
quantType :: Word8 -> Word8
quantType style = 
  style .&. (complement $ shiftL j2K_QCX_GB_MASK j2K_QCX_GB_SHIFT)
    
