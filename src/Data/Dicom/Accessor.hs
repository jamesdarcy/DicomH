module Data.Dicom.Accessor
  (
    -- * Accessors for 0x0002
    getTransferSyntaxUid,
    -- * Accessors for 0x0008
    getSopClassUid,
    getSopInstanceUid,
    getStudyDate,
    getSeriesDate,
    getModality,
    getStudyDescription,
    getSeriesDescription,
    -- * Accessors for 0x0010
    getPatientName,
    -- * Accessors for 0x0020
    getStudyInstanceUid,
    getSeriesInstanceUid,
    getStudyId,
    getSeriesNumber,
    -- * Accessors for 0x0028
    getNumberOfFrames,
    getRows,
    getColumns
  ) where

import Data.Word
import Data.Int

import Data.Dicom
import Data.Dicom.Tag

-- | Group 0x0002 - Metadata
getTransferSyntaxUid :: DicomObject -> Maybe String
getTransferSyntaxUid = getString tRANSFER_SYNTAX_UID

-- | Group 0x0008
getSopClassUid :: DicomObject -> Maybe String
getSopClassUid = getString sOP_CLASS_UID

getSopInstanceUid :: DicomObject -> Maybe String
getSopInstanceUid = getString sOP_INSTANCE_UID

getStudyDate :: DicomObject -> Maybe String
getStudyDate = getString sTUDY_DATE

getSeriesDate :: DicomObject -> Maybe String
getSeriesDate = getString sERIES_DATE

getModality :: DicomObject -> Maybe String
getModality = getString mODALITY

getStudyDescription :: DicomObject -> Maybe String
getStudyDescription = getString sTUDY_DESCRIPTION

getSeriesDescription :: DicomObject -> Maybe String
getSeriesDescription = getString sERIES_DESCRIPTION

-- | Group 0x0010
getPatientName :: DicomObject -> Maybe String
getPatientName = getString pATIENT_NAME

-- | Group 0x0020
getStudyInstanceUid :: DicomObject -> Maybe String
getStudyInstanceUid = getString sTUDY_INSTANCE_UID

getSeriesInstanceUid :: DicomObject -> Maybe String
getSeriesInstanceUid = getString sERIES_INSTANCE_UID

getStudyId :: DicomObject -> Maybe String
getStudyId = getString sTUDY_ID

getSeriesNumber :: DicomObject -> Maybe String
getSeriesNumber = getString sERIES_NUMBER

-- | Group 0x0028
getNumberOfFrames :: DicomObject -> Maybe String
getNumberOfFrames = getString nUMBER_OF_FRAMES

getRows :: DicomObject -> Maybe Word16
getRows = getWord16 rOWS

getColumns :: DicomObject -> Maybe Word16
getColumns = getWord16 cOLUMNS

