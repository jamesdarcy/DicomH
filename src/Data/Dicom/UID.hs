module Data.Dicom.UID
  ( -- * Transfer Syntax
    iMPLICIT_VR_LE,
    eXPLICIT_VR_LE,
    eXPLICIT_VR_BE
  ) where

-- | Implicit VR Little Endian
iMPLICIT_VR_LE :: String
iMPLICIT_VR_LE = "1.2.840.10008.1.2"

-- | Explicit VR Little Endian
eXPLICIT_VR_LE :: String
eXPLICIT_VR_LE = "1.2.840.10008.1.2.1"

-- | Explicit VR Big Endian
eXPLICIT_VR_BE :: String
eXPLICIT_VR_BE = "1.2.840.10008.1.2.2"

-- | JPEG2000
jPEG_2000 :: String
jPEG_2000 = "1.2.840.10008.1.2.4.91"