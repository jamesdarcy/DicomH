{-
Copyright James d'Arcy 2010

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of James d'Arcy nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

module Data.Dicom
  ( -- * Basic Types
    EncapDicomObject(..),
    DicomObject(..),
    DicomElement(..),
    DicomValue(..),
    DicomFragment,
    DicomElementMap,
    getTag,
    getEncapDicomObject,
    -- * Generic accessors
    getBytes,
    getInt16,
    getInt32,
    getString,
    getWord16,
    getWord32,
    extractInt16s
  ) where

import System.IO
import Text.Printf
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import Data.Binary.Strict.Get
import Data.Word
import Data.Int
import Data.Bits
import qualified Data.Map as Map
import Control.Monad

import Data.Dicom.Dictionary as Dict
import Data.Dicom.Tag
import Data.Dicom.UID

data EncapDicomObject = EncapDicomObject {
  preamble :: B.ByteString,
  magic :: String,
  metadata :: DicomObject,
  dicom :: DicomObject
  } deriving (Eq)

data DicomObject = DicomObject {
  contents :: DicomElementMap
  } deriving (Eq)

data DicomElement = DicomElement {
  group :: Word16,
  element :: Word16,
  vr :: String,
  valueLength :: Word32,
  value :: DicomValue
  } deriving (Eq)

data DicomValue = DicomAE String
                | DicomAS String
                | DicomAT B.ByteString
                | DicomCS String
                | DicomDA String
                | DicomDS String
                | DicomDT String
                | DicomFL Float
                | DicomFLList [Float]
                | DicomFD Double
                | DicomFDList [Double]
                | DicomIS String
                | DicomLO String
                | DicomLT String
                | DicomOB B.ByteString
                | DicomOF B.ByteString
                | DicomOW B.ByteString
                | DicomPN String
                | DicomSH String
                | DicomSL Int32
                | DicomSLList [Int32]
                | DicomSQ [DicomObject]
                | DicomSS Int16
                | DicomSSList [Int16]
                | DicomST String
                | DicomTM String
                | DicomUI String
                | DicomUL Word32
                | DicomULList [Word32]
                | DicomUN B.ByteString
                | DicomUS Word16
                | DicomUSList [Word16]
                | DicomUT String
                | DicomFRAG [DicomFragment]
                | DicomERR B.ByteString
                  deriving (Eq)

type DicomElementMap = Map.Map DicomTag DicomElement

type DicomFragment = B.ByteString

tagDict = getTagDictionary

getTag :: DicomElement -> DicomTag
getTag de = getTagFromGroupElement (group de) (element de)

getTagFromGroupElement :: Word16 -> Word16 -> DicomTag
getTagFromGroupElement group element =
  (shiftL (fromIntegral group) 16) .|. (fromIntegral element)

getEncapDicomObject :: Get EncapDicomObject
getEncapDicomObject = do
  preamble <- getByteString 128
  magic <- liftM BC8.unpack (getByteString 4)
  metadata <- getDicomObject getDicomExpLeElement isMetadata
  let getElement = getSyntaxElementFn $ contents metadata
  dicom <- getDicomObject getElement true
  return (EncapDicomObject preamble magic metadata dicom)

getDicomObject :: Get DicomElement -> Get Bool -> Get DicomObject
getDicomObject getElement validate = do
  contents <- getDicomElementMap getElement validate
  return (DicomObject contents)

getDicomElementMap :: Get DicomElement -> Get Bool -> Get DicomElementMap
getDicomElementMap getElement validate = do
  moreElements <- hasMoreElements
  if not moreElements
    then return Map.empty
    else do
      valid <- validate 
      if not valid
        then return Map.empty
        else do
          e <- getElement
          rest <- getDicomElementMap getElement validate
          let singleton = (Map.insert (getTag e) e) $ Map.empty
          return $ Map.union singleton rest

getDicomSequence :: Word32 -> Get DicomElement -> Get DicomValue
getDicomSequence length getElement = do
  dcm <- getDicomSQItemList length getElement
  return (DicomSQ dcm)

getDicomSQItem :: Word32 -> Get DicomElement -> Get DicomObject
getDicomSQItem length getElement = do
  itemAhead <- isSQItemAhead
  if not itemAhead
    then return (DicomObject Map.empty)
    else do
      skip 4
      itemLength <- getWord32le
      case itemLength of
        0          -> return (DicomObject Map.empty)
        0xffffffff -> getDicomObject getElement true
        otherwise  -> do 
          bytes <- bytesRead
          getDicomObject getElement (inBounds (bytes+(fromIntegral itemLength)))

getDicomSQItemList :: Word32 -> Get DicomElement -> Get [DicomObject]
getDicomSQItemList length getElement = do
  moreItems <- hasMoreItems
  if not moreItems
    then return []
    else do
      dcm <- getDicomSQItem length getElement
      rest <- getDicomSQItemList length getElement
      return (dcm : rest)

hasMoreElements :: Get Bool
hasMoreElements = do
  empty <- isEmpty
  if empty
    then return False
    else do
      endSQItem <- isSQItemDelimAhead
      if endSQItem
        then do
          skip 8
          return False
        else return True
  
hasMoreFragments :: Get Bool
hasMoreFragments = do
  empty <- isEmpty
  if empty
    then return False
    else do
      endFrag <- isFragmentEndAhead
      if endFrag
        then do
          skip 8
          return False
        else isFragmentAhead
  
hasMoreItems :: Get Bool
hasMoreItems = do
  empty <- isEmpty
  if empty
    then return False
    else do
      endSQ <- isSQEndAhead
      if endSQ
        then do
          skip 8
          return False
        else isSQItemAhead
  
isFragmentAhead :: Get Bool
isFragmentAhead = isSQItemAhead

isFragmentEndAhead :: Get Bool
isFragmentEndAhead = isSQEndAhead

isMetadata :: Get Bool
isMetadata = liftM (== 2) $ lookAhead getWord16le

inBounds :: Int -> Get Bool
inBounds bound = liftM (< bound) $ bytesRead

isSQItemAhead :: Get Bool
isSQItemAhead =
  liftM (== B.pack [0xfe,0xff,0x00,0xe0]) $ lookAhead (getByteString 4)

isSQItemDelimAhead :: Get Bool
isSQItemDelimAhead =
  liftM (== B.pack [0xfe,0xff,0x0d,0xe0]) $ lookAhead (getByteString 4)

isSQEndAhead :: Get Bool
isSQEndAhead =
  liftM (== B.pack [0xfe,0xff,0xdd,0xe0]) $ lookAhead (getByteString 4)

true :: Get Bool
true = return True

getSyntaxElementFn :: DicomElementMap -> Get DicomElement
getSyntaxElementFn tags =
  case Map.lookup tRANSFER_SYNTAX_UID tags of
    Nothing -> fail $ "No transfer syntax found"
    Just de ->
      let syntax = getStringValue $ value de in
      case syntax of
        "1.2.840.10008.1.2.4.91" -> getDicomJpeg2000Element -- jPEG_2000
        "1.2.840.10008.1.2.1"    -> getDicomExpLeElement    -- eXPLICIT_VR_LE
        "1.2.840.10008.1.2"      -> getDicomImpLeElement    -- iMPLICIT_VR_LE
        _ -> fail $ "Unsupported transfer syntax: " ++ syntax

getDicomExpLeElement :: Get DicomElement
getDicomExpLeElement = do
  group <- getWord16le
  element <- getWord16le
  vr <- liftM BC8.unpack (getByteString 2)
  valueLength <- getExpLeValueLength vr
  value <- getDicomValue vr valueLength getDicomExpLeElement
  return (DicomElement group element vr valueLength value)

getExpLeValueLength :: String -> Get Word32
getExpLeValueLength vr
  | vr == "OB" = skipToValueLength
  | vr == "OW" = skipToValueLength
  | vr == "OF" = skipToValueLength
  | vr == "SQ" = skipToValueLength
  | vr == "UT" = skipToValueLength
  | vr == "UN" = skipToValueLength
  | otherwise  = liftM fromIntegral getWord16le

getDicomJpeg2000Element :: Get DicomElement
getDicomJpeg2000Element = do
  group <- getWord16le
  element <- getWord16le
  vr <- liftM BC8.unpack (getByteString 2)
  valueLength <- getExpLeValueLength vr
  if ((group == 0x7fe0) && (element == 0x0010) && (valueLength == 0xffffffff))
    then do
      -- Pixel data is encapsulated
      bytesSoFar <- bytesRead
      fragList <- getDicomFragmentList
      bytesAfterFrags <- bytesRead
      return (DicomElement group element "FR" (fromIntegral (bytesAfterFrags - bytesSoFar)) (DicomFRAG fragList))
    else do
      value <- getDicomValue vr valueLength getDicomJpeg2000Element
      return (DicomElement group element vr valueLength value)

getDicomFragment :: Get DicomFragment
getDicomFragment = do
  skip 4
  fragLength <- getWord32le
  bytesRemaining <- remaining
  if fragLength > (fromIntegral bytesRemaining)
    then fail "Fragment length too long"
    else getByteString $ fromIntegral fragLength

getDicomFragmentList :: Get [DicomFragment]
getDicomFragmentList = do
  moreFrags <- hasMoreFragments
  if not moreFrags
    then return []
    else do
      frag <- getDicomFragment
      rest <- getDicomFragmentList
      return (frag : rest)

skipToValueLength :: Get Word32
skipToValueLength = skip 2 >> getWord32le
  
getDicomImpLeElement :: Get DicomElement
getDicomImpLeElement = do
  group <- getWord16le
  element <- getWord16le
  let vr = getVrFromTag tagDict $ getTagFromGroupElement group element
  valueLength <- getWord32le
  value <- getDicomValue vr (fromIntegral valueLength) getDicomImpLeElement
  return (DicomElement group element vr valueLength value)

getVrFromTag :: TagDictionary -> DicomTag -> String
getVrFromTag tagDict tag = go $ Map.lookup tag tagDict
  where
    go de =
      case de of
        Nothing -> "UN"
        Just x  -> Dict.vr x
        
getDicomValue :: String -> Word32 -> Get DicomElement -> Get DicomValue
getDicomValue vr length getElement
  | vr == "AE" = do
    bs <- getByteString $ fromIntegral length
    return (DicomAE (trimDicomString bs))
  | vr == "AS" = do
    bs <- getByteString $ fromIntegral length
    return (DicomAS (trimDicomString bs))
  | vr == "AT" = do
    bs <- getByteString $ fromIntegral length
    return (DicomAT bs)
  | vr == "CS" = do
    bs <- getByteString $ fromIntegral length
    return (DicomCS (trimDicomString bs))
  | vr == "DA" = do
    bs <- getByteString $ fromIntegral length
    return (DicomDA (trimDicomString bs))
  | vr == "DS" = do
    bs <- getByteString $ fromIntegral length
    return (DicomDS (trimDicomString bs))
  | vr == "DT" = do
    bs <- getByteString $ fromIntegral length
    return (DicomDT (trimDicomString bs))
  | vr == "FL" = getDicomFLValue $ fromIntegral length
  | vr == "FD" = getDicomFDValue $ fromIntegral length
  | vr == "IS" = do
    bs <- getByteString $ fromIntegral length
    return (DicomIS (trimDicomString bs))
  | vr == "LO" = do
    bs <- getByteString $ fromIntegral length
    return (DicomLO (trimDicomString bs))
  | vr == "LT" = do
    bs <- getByteString $ fromIntegral length
    return (DicomLT (trimDicomString bs))
  | vr == "OB" = do
    bs <- getByteString $ fromIntegral length
    return (DicomOB bs)
  | vr == "OF" = do
    bs <- getByteString $ fromIntegral length
    return (DicomOF bs)
  | vr == "OW" = do
    bs <- getByteString $ fromIntegral length
    return (DicomOW bs)
  | vr == "PN" = do
    bs <- getByteString $ fromIntegral length
    return (DicomPN (trimDicomString bs))
  | vr == "SH" = do
    bs <- getByteString $ fromIntegral length
    return (DicomSH (trimDicomString bs))
  | vr == "SL" = getDicomSLValue $ fromIntegral length
  | vr == "SQ" = getDicomSequence length getElement
  | vr == "SS" = getDicomSSValue $ fromIntegral length
  | vr == "ST" = do
    bs <- getByteString $ fromIntegral length
    return (DicomST (trimDicomString bs))
  | vr == "TM" = do
    bs <- getByteString $ fromIntegral length
    return (DicomTM (trimDicomString bs))
  | vr == "UI" = do
    bs <- getByteString $ fromIntegral length
    return (DicomUI (trimDicomString bs))
  | vr == "UL" = getDicomULValue $ fromIntegral length
  | vr == "UN" = do
    bs <- getByteString $ fromIntegral length
    return (DicomUN bs)
  | vr == "US" = getDicomUSValue $ fromIntegral length
  | vr == "UT" = do
    bs <- getByteString $ fromIntegral length
    return (DicomUT (trimDicomString bs))
  | otherwise  = do
    bs <- getByteString $ fromIntegral length
    return (DicomERR bs)

getDicomFDValue :: Int -> Get DicomValue
getDicomFDValue length
  | length == 8 = do
    fd <- getFloat64host -- Fix!!! Assumes LE host
    return (DicomFD fd)
  | length `mod` 8 == 0 = do
    fdList <- getDicomFDList length
    return (DicomFDList fdList)
  | otherwise = fail $ "Non-integral VM for FD. Length=" ++ show length

getDicomFDList :: Int -> Get [Double]
getDicomFDList length
  | length == 0 = return []
  | otherwise   = do
    fd <- getFloat64host -- Fix!!! Assumes LE host
    rest <- getDicomFDList (length-8)
    return (fd:rest)

getDicomFLValue :: Int -> Get DicomValue
getDicomFLValue length
  | length == 4 = do
    fl <- getFloat32host -- Fix!!! Assumes LE host
    return (DicomFL fl)
  | length `mod` 4 == 0 = do
    flList <- getDicomFLList length
    return (DicomFLList flList)
  | otherwise = fail $ "Non-integral VM for FL. Length=" ++ show length

getDicomFLList :: Int -> Get [Float]
getDicomFLList length
  | length == 0 = return []
  | otherwise   = do
    fl <- getFloat32host -- Fix!!! Assumes LE host
    rest <- getDicomFLList (length-4)
    return (fl:rest)

getDicomSLValue :: Int -> Get DicomValue
getDicomSLValue length
  | length == 4 = do
    sl <- getWord32le -- Fix!!! Assumes LE
    return (DicomSL (fromIntegral sl))
  | length `mod` 4 == 0 = do
    slList <- getDicomSLList length
    return (DicomSLList slList)
  | otherwise = fail $ "Non-integral VM for SL. Length=" ++ show length

getDicomSLList :: Int -> Get [Int32]
getDicomSLList length
  | length == 0 = return []
  | otherwise   = do
    sl <- liftM fromIntegral getWord32le -- Fix!!! Assumes LE
    rest <- getDicomSLList (length-4)
    return (sl:rest)

getDicomSSValue :: Int -> Get DicomValue
getDicomSSValue length
  | length == 2 = do
    ss <- getWord16le -- Fix!!! Assumes LE
    return (DicomSS (fromIntegral ss))
  | length `mod` 2 == 0 = do
    ssList <- getDicomSSList length
    return (DicomSSList ssList)
  | otherwise = fail $ "Non-integral VM for SS. Length=" ++ show length

getDicomSSList :: Int -> Get [Int16]
getDicomSSList length
  | length == 0 = return []
  | otherwise   = do
    ss <- liftM fromIntegral getWord16le -- Fix!!! Assumes LE
    rest <- getDicomSSList (length-2)
    return (ss:rest)

getDicomUSValue :: Int -> Get DicomValue
getDicomUSValue length
  | length == 2 = do
    us <- getWord16le -- Fix!!! Assumes LE
    return (DicomUS us)
  | length `mod` 2 == 0 = do
    usList <- getDicomUSList length
    return (DicomUSList usList)
  | otherwise = fail $ "Non-integral VM for US. Length=" ++ show length

getDicomUSList :: Int -> Get [Word16]
getDicomUSList length
  | length == 0 = return []
  | otherwise   = do
    us <- getWord16le -- Fix!!! Assumes LE
    rest <- getDicomUSList (length-2)
    return (us:rest)

getDicomULValue :: Int -> Get DicomValue
getDicomULValue length
  | length == 4 = do
    ul <- getWord32le -- Fix!!! Assumes LE
    return (DicomUL ul)
  | length `mod` 4 == 0 = do
    ulList <- getDicomULList length
    return (DicomULList ulList)
  | otherwise = fail $ "Non-integral VM for UL. Length=" ++ show length

getDicomULList :: Int -> Get [Word32]
getDicomULList length
  | length == 0 = return []
  | otherwise   = do
    ul <- getWord32le -- Fix!!! Assumes LE
    rest <- getDicomULList (length-4)
    return (ul:rest)

stripWhiteSpace :: String -> String
stripWhiteSpace s = reverse(dropWhile (== ' ') $ reverse(dropWhile (== ' ') s))

trimDicomString :: B.ByteString -> String
trimDicomString bs =
  if (B.length bs > 0) && (B.last bs == 0)
    then stripWhiteSpace $ BC8.unpack $ B.init bs
    else stripWhiteSpace $ BC8.unpack bs

getStringValue :: DicomValue -> String
getStringValue (DicomUI ui) = ui
getStringValue _ = error "Wrong type"

--
getBytes :: DicomTag -> DicomObject -> Maybe B.ByteString
getBytes tag dcm = do
  let maybeElement = Map.lookup tag $ contents dcm
  case maybeElement of
    Nothing -> Nothing
    Just dcmElement -> do
      let dcmValue = value dcmElement
      case dcmValue of
        DicomOB dcmOb -> Just dcmOb
        DicomOW dcmOw -> Just dcmOw
        _             -> Nothing

--
getInt16 :: DicomTag -> DicomObject -> Maybe Int16
getInt16 tag dcm = do
  let maybeElement = Map.lookup tag $ contents dcm
  case maybeElement of
    Nothing -> Nothing
    Just dcmElement -> do
      let dcmValue = value dcmElement
      case dcmValue of
        DicomSS dcmSs -> Just dcmSs
        _             -> Nothing

getInt16List :: Get [Int16]
getInt16List = do
  empty <- isEmpty
  if empty
    then return []
    else do
      x <- liftM fromIntegral getWord16le -- Fix!!! Assumes LE
      xs <- getInt16List
      return (x : xs)

--
extractInt16s :: B.ByteString -> [Int16]
extractInt16s bytes = do
  let result = runGet getInt16List bytes
  case result of
    (Left errorMessage, _) -> []
    (Right intList, _)     -> intList

--
getInt32 :: DicomTag -> DicomObject -> Maybe Int32
getInt32 tag dcm = do
  let maybeElement = Map.lookup tag $ contents dcm
  case maybeElement of
    Nothing -> Nothing
    Just dcmElement -> do
      let dcmValue = value dcmElement
      case dcmValue of
        DicomSL dcmSl -> Just dcmSl
        _             -> Nothing

--
getString :: DicomTag -> DicomObject -> Maybe String
getString tag dcm = do
  let maybeElement = Map.lookup tag $ contents dcm
  case maybeElement of
    Nothing -> Nothing
    Just dcmElement -> do
      let dcmValue = value dcmElement
      case dcmValue of
        DicomAE dcmAe -> Just dcmAe
        DicomAS dcmAs -> Just dcmAs
        DicomCS dcmCs -> Just dcmCs
        DicomDA dcmDa -> Just dcmDa
        DicomDS dcmDs -> Just dcmDs
        DicomDT dcmDt -> Just dcmDt
        DicomIS dcmIs -> Just dcmIs
        DicomLO dcmLo -> Just dcmLo
        DicomLT dcmLt -> Just dcmLt
        DicomPN dcmPn -> Just dcmPn
        DicomSH dcmSh -> Just dcmSh
        DicomST dcmSt -> Just dcmSt
        DicomTM dcmTm -> Just dcmTm
        DicomUI dcmUi -> Just dcmUi
        DicomUT dcmUt -> Just dcmUt
        _             -> Nothing

--
getWord16 :: DicomTag -> DicomObject -> Maybe Word16
getWord16 tag dcm = do
  let maybeElement = Map.lookup tag $ contents dcm
  case maybeElement of
    Nothing -> Nothing
    Just dcmElement -> do
      let dcmValue = value dcmElement
      case dcmValue of
        DicomUS dcmUs -> Just dcmUs
        _             -> Nothing

--
getWord32 :: DicomTag -> DicomObject -> Maybe Word32
getWord32 tag dcm = do
  let maybeElement = Map.lookup tag $ contents dcm
  case maybeElement of
    Nothing -> Nothing
    Just dcmElement -> do
      let dcmValue = value dcmElement
      case dcmValue of
        DicomUL dcmUl -> Just dcmUl
        _             -> Nothing

