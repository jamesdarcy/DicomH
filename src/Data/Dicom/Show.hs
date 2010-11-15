
module Data.Dicom.Show where

import Data.Dicom
import qualified Data.Dicom.Dictionary as Dict
import Text.PrettyPrint.HughesPJ
import Text.Printf
import qualified Data.ByteString as B
import qualified Data.Map as Map
import Data.Word

renderEncapDicom :: EncapDicomObject -> Doc
renderEncapDicom (EncapDicomObject preamble magic metadata dicom) =
  text "\nPreamble: " <> renderByteString preamble <> line <>
  text "Magic:   " <> text magic <> line <>
  text "Metadata:\n" <> renderDicom 0 metadata <>
  text "DICOM:\n" <> renderDicom 0 dicom

renderDicom :: Int -> DicomObject -> Doc
renderDicom level dcm = go $ Map.elems $ contents dcm
  where
    go []     = empty
    go (e:es) = renderElement level e <> go es

renderElement :: Int -> DicomElement -> Doc
renderElement level de =
  let rep = vr de in
  case rep of
    "SQ" -> renderElement2 level de <> line <> renderSQ (level+1) de
    _    -> renderElement2 level de <> line

renderElement2 :: Int -> DicomElement -> Doc
renderElement2 level de =
  indent level <> renderTag de <+> text (vr de) <+>
      renderValueLength de <+> renderValue (value de) <> text " - " <>
      renderDescription de

renderSQ :: Int -> DicomElement -> Doc
renderSQ level de =
  let (DicomSQ sq) = value de in
  goSQ sq where
    goSQ []     = empty
    goSQ (d:ds) = renderDicom level d <> goSQ ds

renderTag :: DicomElement -> Doc
renderTag de =
   lparen <> text (printf "%04x" (group de)) <> comma <>
   text (printf "%04x" (element de)) <> rparen

renderValueLength :: DicomElement -> Doc
renderValueLength de = lbrack <> int (fromIntegral (valueLength de)) <>
  rbrack

renderDescription :: DicomElement -> Doc
renderDescription de = go $ Map.lookup (getTag de) Dict.getTagDictionary
  where
    go (Just x) = text $ Dict.desc x
    go _        = text "Unknown"

renderByteString :: B.ByteString -> Doc
renderByteString bs
  | B.null bs = empty
  | otherwise = text (concat $ map (printf "%02x") $ B.unpack (B.take 32 bs))

renderValue :: DicomValue -> Doc
renderValue (DicomAE bs) = text bs
renderValue (DicomAS bs) = text bs
renderValue (DicomAT bs) = renderByteString bs
renderValue (DicomCS bs) = text bs
renderValue (DicomDA bs) = text bs
renderValue (DicomDS bs) = text bs
renderValue (DicomDT bs) = text bs
renderValue (DicomFL bs) = float bs
renderValue (DicomFLList flList) = text (show flList)
renderValue (DicomFD bs) = double bs
renderValue (DicomFDList fdList) = text (show fdList)
renderValue (DicomIS bs) = text bs
renderValue (DicomLO bs) = text bs
renderValue (DicomLT bs) = text bs
renderValue (DicomOB bs) = renderByteString bs
renderValue (DicomOF bs) = renderByteString bs
renderValue (DicomOW bs) = renderByteString bs
renderValue (DicomPN bs) = text bs
renderValue (DicomSH bs) = text bs
renderValue (DicomSL sl) = text (show sl)
renderValue (DicomSLList slList) = text (show slList)
renderValue (DicomSQ sq) = empty
renderValue (DicomSS ss) = text (show ss)
renderValue (DicomSSList ssList) = text (show ssList)
renderValue (DicomST bs) = text bs
renderValue (DicomTM bs) = text bs
renderValue (DicomUI bs) = text bs
renderValue (DicomUL ul) = text (show ul)
renderValue (DicomULList ulList) = text (show ulList)
renderValue (DicomUN bs) = renderByteString bs
renderValue (DicomUS us) = text (show us)
renderValue (DicomUSList usList) = text (show usList)
renderValue (DicomUT bs) = text bs
renderValue (DicomERR bs) = renderByteString bs
renderValue (DicomFRAG frags) = text (concat $ map ("\n * Fragment: " ++) (map (render . renderByteString) frags))

line :: Doc
line = text "\n"

indent :: Int -> Doc
indent 0     = empty
indent level = text $ concat $ replicate level "> "

pretty :: Doc -> String
pretty doc = render doc

instance Show EncapDicomObject where
  show edo = pretty $ renderEncapDicom edo

instance Show DicomObject where
  show dcm = render $ renderDicom 0 dcm

instance Show DicomElement where
  show (DicomElement group element vr valueLength value) =
    "    (" ++ printf "%04x" group ++ "," ++ printf "%04x" element ++ ") " ++
    vr ++ " [" ++ show valueLength ++ "] " ++
    (Prelude.take 256 $ show value) ++ "\n"

instance Show DicomValue where
  show (DicomAE bs) = bs
  show (DicomAS bs) = bs
  show (DicomAT bs) = render $ renderByteString bs
  show (DicomCS bs) = bs
  show (DicomDA bs) = bs
  show (DicomDS bs) = bs
  show (DicomDT bs) = bs
  show (DicomFL fl) = show fl
  show (DicomFLList flList) = show flList
  show (DicomFD fd) = show fd
  show (DicomFDList fdList) = show fdList
  show (DicomIS bs) = bs
  show (DicomLO bs) = bs
  show (DicomLT bs) = bs
  show (DicomOB bs) = render $ renderByteString bs
  show (DicomOF bs) = render $ renderByteString bs
  show (DicomOW bs) = render $ renderByteString bs
  show (DicomPN bs) = bs
  show (DicomSH bs) = bs
  show (DicomSL sl) = show sl
  show (DicomSLList slList) = show slList
  show (DicomSQ sq) = show sq
  show (DicomSS ss) = show ss
  show (DicomSSList ssList) = show ssList
  show (DicomST bs) = bs
  show (DicomTM bs) = bs
  show (DicomUI bs) = bs
  show (DicomUL ul) = show ul
  show (DicomULList ulList) = show ulList
  show (DicomUN bs) = render $ renderByteString bs
  show (DicomUS us) = show us
  show (DicomUSList usList) = show usList
  show (DicomUT bs) = bs
  show (DicomERR bs) = render $ renderByteString bs


