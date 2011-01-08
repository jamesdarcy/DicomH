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

module Data.Dicom.Dictionary
  (
    TagDictEntry(..),
    TagDictionary,
    UidDictionary,
    getTagDictionary,
    getUidDictionary
  ) where

import qualified Data.Map as Map
import Data.Word

import Data.Dicom.Tag
import Data.Dicom.UID

-- | UID type
data TagDictEntry = TagDictEntry {
  vr :: String,
  desc :: String
  } deriving (Eq, Show)

-- | UID type
type TagDictionary = Map.Map Word32 TagDictEntry

-- | UID type
type UidDictionary = Map.Map UID String

-- | Tag dictionary
getTagDictionary :: TagDictionary
getTagDictionary =
  Map.insert 0x00020000 (TagDictEntry "UL" "File Meta Information Group Length") .
  Map.insert 0x00020001 (TagDictEntry "OB" "File Meta Information Version") .
  Map.insert 0x00020002 (TagDictEntry "UI" "Media Storage SOP Class UID") .
  Map.insert 0x00020003 (TagDictEntry "UI" "Media Storage SOP Instance UID") .
  Map.insert 0x00020010 (TagDictEntry "UI" "Transfer Syntax UID") .
  Map.insert 0x00020012 (TagDictEntry "UI" "Implentation Class UID") .
  Map.insert 0x00020013 (TagDictEntry "SH" "Implementation Version Name") .
  Map.insert 0x00020016 (TagDictEntry "AE" "Source Application Entity Title") .
  Map.insert 0x00020100 (TagDictEntry "UI" "Private Information Creator UID") .
  Map.insert 0x00020102 (TagDictEntry "OB" "Private Information") .
  Map.insert 0x00041130 (TagDictEntry "CS" "File-set ID") .
  Map.insert 0x00041141 (TagDictEntry "CS" "File-set Descriptor File ID") .
  Map.insert 0x00041142 (TagDictEntry "CS" "Specific Character Set of File-set Descriptor File") .
  Map.insert 0x00041200 (TagDictEntry "UL" "Offset of the First Directory Record of the Root Directory Entity") .
  Map.insert 0x00041202 (TagDictEntry "UL" "Offset of the Last Directory Record of the Root Directory Entity") .
  Map.insert 0x00041212 (TagDictEntry "US" "File-set Consistency Flag") .
  Map.insert 0x00041220 (TagDictEntry "SQ" "Directory Record Sequence") .
  Map.insert 0x00041400 (TagDictEntry "UL" "Offset of the Next Directory Record") .
  Map.insert 0x00041410 (TagDictEntry "US" "Record In-use Flag") .
  Map.insert 0x00041420 (TagDictEntry "UL" "Offset of Referenced Lower-Level Directory Entity") .
  Map.insert 0x00041430 (TagDictEntry "CS" "Directory Record Type") .
  Map.insert 0x00041432 (TagDictEntry "UI" "Private Record UID") .
  Map.insert 0x00041500 (TagDictEntry "CS" "Referenced File ID") .
  Map.insert 0x00041510 (TagDictEntry "UI" "Referenced SOP Class UID in File") .
  Map.insert 0x00041511 (TagDictEntry "UI" "Referenced SOP Instance UID in File") .
  Map.insert 0x00041512 (TagDictEntry "UI" "Referenced Transfer Syntax UID in File") .
  Map.insert 0x0004151a (TagDictEntry "UI" "Referenced Related General SOP Class UID in File") .
  Map.insert 0x00080005 (TagDictEntry "CS" "Specific Character Set") .
  Map.insert 0x00080006 (TagDictEntry "SQ" "Language Code Sequence") .
  Map.insert 0x00080008 (TagDictEntry "CS" "Image Type") .
  Map.insert 0x00080012 (TagDictEntry "DA" "Instance Creation Date") .
  Map.insert 0x00080013 (TagDictEntry "TM" "Instance Creation Time") .
  Map.insert 0x00080014 (TagDictEntry "UI" "Instance Creator UID") .
  Map.insert 0x00080016 (TagDictEntry "UI" "SOP Class UID") .
  Map.insert 0x00080018 (TagDictEntry "UI" "SOP Instance UID") .
  Map.insert 0x00080020 (TagDictEntry "DA" "Study Date") .
  Map.insert 0x00080021 (TagDictEntry "DA" "Series Date") .
  Map.insert 0x00080022 (TagDictEntry "DA" "Acquisition Date") .
  Map.insert 0x00080023 (TagDictEntry "DA" "Content Date") .
  Map.insert 0x0008002a (TagDictEntry "DT" "Acquisition DateTime") .
  Map.insert 0x00080030 (TagDictEntry "TM" "Study Time") .
  Map.insert 0x00080031 (TagDictEntry "TM" "Series Time") .
  Map.insert 0x00080032 (TagDictEntry "TM" "Acquisition Time") .
  Map.insert 0x00080033 (TagDictEntry "TM" "Content Time") .
  Map.insert 0x00080050 (TagDictEntry "SH" "Accession Number") .
  Map.insert 0x00080051 (TagDictEntry "SQ" "Issuer of Accession Number Sequence") .
  Map.insert 0x00080060 (TagDictEntry "CS" "Modality") .
  Map.insert 0x00080070 (TagDictEntry "LO" "Manufacturer") .
  Map.insert 0x00080080 (TagDictEntry "LO" "Institution Name") .
  Map.insert 0x00080081 (TagDictEntry "ST" "Institution Address") .
  Map.insert 0x00080082 (TagDictEntry "SQ" "Institution Code Sequence") .
  Map.insert 0x00080090 (TagDictEntry "PN" "Referring Physician Name") .
  Map.insert 0x00080096 (TagDictEntry "SQ" "Referring Physician Identification Sequence") .
  Map.insert 0x00080100 (TagDictEntry "SH" "Code Value") .
  Map.insert 0x00080102 (TagDictEntry "SH" "Coding Scheme Designator") .
  Map.insert 0x00080103 (TagDictEntry "SH" "Coding Scheme Version") .
  Map.insert 0x00080104 (TagDictEntry "LO" "Code Meaning") .
  Map.insert 0x00080105 (TagDictEntry "CS" "Mapping Resource") .
  Map.insert 0x00080106 (TagDictEntry "DT" "Context Group Version") .
  Map.insert 0x0008010b (TagDictEntry "CS" "Context Group Extension Flag") .
  Map.insert 0x0008010d (TagDictEntry "UI" "Context Group Extension Creator UID") .
  Map.insert 0x0008010f (TagDictEntry "CS" "Context Identifier") .
  Map.insert 0x00080110 (TagDictEntry "SQ" "Coding Scheme Identification Sequence") .
  Map.insert 0x00081010 (TagDictEntry "SH" "Station Name") .
  Map.insert 0x00081030 (TagDictEntry "LO" "Study Description") .
  Map.insert 0x00081032 (TagDictEntry "SQ" "Procedure Code Sequence") .
  Map.insert 0x0008103e (TagDictEntry "LO" "Series Description") .
  Map.insert 0x0008103f (TagDictEntry "SQ" "Series Description Code Sequence") .
  Map.insert 0x00081040 (TagDictEntry "LO" "Institutional Department Name") .
  Map.insert 0x00081048 (TagDictEntry "PN" "Physician(s) of Record") .
  Map.insert 0x00081049 (TagDictEntry "SQ" "Physician(s) of Record Identification Sequence") .
  Map.insert 0x00081050 (TagDictEntry "PN" "Performing Physician Name") .
  Map.insert 0x00081052 (TagDictEntry "SQ" "Performing Physician Identification Sequence") .
  Map.insert 0x00081062 (TagDictEntry "SQ" "Physician(s) Reading Study Identification Sequence") .
  Map.insert 0x00081070 (TagDictEntry "PN" "Operator Name") .
  Map.insert 0x00081072 (TagDictEntry "SQ" "Operator Identification Sequence") .
  Map.insert 0x00081080 (TagDictEntry "LO" "Admitting Diagnoses Description") .
  Map.insert 0x00081084 (TagDictEntry "SQ" "Admitting Diagnoses Code Sequence") .
  Map.insert 0x00081090 (TagDictEntry "LO" "Manufacturer Model Name") .
  Map.insert 0x00081110 (TagDictEntry "SQ" "Referenced Study Sequence") .
  Map.insert 0x00081111 (TagDictEntry "SQ" "Referenced Performed Procedure Step Sequence") .
  Map.insert 0x00081115 (TagDictEntry "SQ" "Referenced Series Sequence") .
  Map.insert 0x00081120 (TagDictEntry "SQ" "Referenced Patient Sequence") .
  Map.insert 0x00081125 (TagDictEntry "SQ" "Referenced Visit Sequence") .
  Map.insert 0x00081134 (TagDictEntry "SQ" "Referenced Stereometric Instance Sequence") .
  Map.insert 0x0008113a (TagDictEntry "SQ" "Referenced Waveform Sequence") .
  Map.insert 0x00081140 (TagDictEntry "SQ" "Referenced Image Sequence") .
  Map.insert 0x0008114a (TagDictEntry "SQ" "Referenced Instance Sequence") .
  Map.insert 0x0008114b (TagDictEntry "SQ" "Referenced Real World Value Mapping Instance Sequence") .
  Map.insert 0x00081150 (TagDictEntry "UI" "Referenced SOP Class UID") .
  Map.insert 0x00081155 (TagDictEntry "UI" "Referenced SOP Instance UID") .
  Map.insert 0x00081160 (TagDictEntry "IS" "Referenced SOP Class UID") .
  Map.insert 0x00081164 (TagDictEntry "SQ" "Frame Extraction Sequence") .
  Map.insert 0x00081198 (TagDictEntry "SQ" "Failed SOP Sequence") .
  Map.insert 0x00081199 (TagDictEntry "SQ" "Referenced SOP Sequence") .
  Map.insert 0x00081200 (TagDictEntry "SQ" "Studies Containing Other Referenced Instances Sequence") .
  Map.insert 0x00081250 (TagDictEntry "SQ" "Related Series Sequence") .
  Map.insert 0x00082111 (TagDictEntry "ST" "Derivation Description") .
  Map.insert 0x00082112 (TagDictEntry "SQ" "Source Image Sequence") .
  Map.insert 0x00082133 (TagDictEntry "SQ" "Event Timer Sequence") .
  Map.insert 0x00082135 (TagDictEntry "SQ" "Event Code Sequence") .
  Map.insert 0x00082218 (TagDictEntry "SQ" "Anatomic Region Sequence") .
  Map.insert 0x00082220 (TagDictEntry "SQ" "Anatomic Region Modifier Sequence") .
  Map.insert 0x00082228 (TagDictEntry "SQ" "Primary Anatomic Structure Sequence") .
  Map.insert 0x00082229 (TagDictEntry "SQ" "Anatomic Structure, Space or Region Sequence") .
  Map.insert 0x00082230 (TagDictEntry "SQ" "Primary Anatomic Structure Modifier Sequence") .
  Map.insert 0x00083001 (TagDictEntry "SQ" "Alternate Representation Sequence") .
  Map.insert 0x00089007 (TagDictEntry "CS" "Frame Type") .
  Map.insert 0x00089092 (TagDictEntry "SQ" "Referenced Image Evidence Sequence") .
  Map.insert 0x00089121 (TagDictEntry "SQ" "Referenced Raw Data Sequence") .
  Map.insert 0x00089123 (TagDictEntry "UI" "Creator-Version UID") .
  Map.insert 0x00089124 (TagDictEntry "SQ" "Derivation Image Sequence") .
  Map.insert 0x00089154 (TagDictEntry "SQ" "Source Image Evidence Sequence") .
  Map.insert 0x00089205 (TagDictEntry "CS" "Pixel Presentation") .
  Map.insert 0x00089206 (TagDictEntry "CS" "Volumetric Properties") .
  Map.insert 0x00089207 (TagDictEntry "CS" "Volume Based Calculation Technique") .
  Map.insert 0x00089208 (TagDictEntry "CS" "Complex Image Component") .
  Map.insert 0x00089209 (TagDictEntry "CS" "Acquisition Contrast") .
  Map.insert 0x00089215 (TagDictEntry "SQ" "Derivation Code Sequence") .
  Map.insert 0x00089237 (TagDictEntry "SQ" "Referenced Presentation State Sequence") .
  Map.insert 0x00089410 (TagDictEntry "SQ" "Referenced Other Plane Sequence") .
  Map.insert 0x00089458 (TagDictEntry "SQ" "Frame Display Sequence") .
  Map.insert 0x00100010 (TagDictEntry "PN" "Patient Name") .
  Map.insert 0x00100020 (TagDictEntry "DS" "Patient's Size") .
  Map.insert 0x00100024 (TagDictEntry "SQ" "Issuer of Patient ID Qualifiers Sequence") .
  Map.insert 0x00100030 (TagDictEntry "DA" "Patient Birth Date") .
  Map.insert 0x00100032 (TagDictEntry "TM" "Patient Birth Time") .
  Map.insert 0x00100040 (TagDictEntry "CS" "Patient Sex") .
  Map.insert 0x00100050 (TagDictEntry "SQ" "Patient's Insurance Plan Code Sequence") .
  Map.insert 0x00100101 (TagDictEntry "SQ" "Patient's Primary Language Code Sequence") .
  Map.insert 0x00100102 (TagDictEntry "SQ" "Patient's Primary Language Modifier Code Sequence") .
  Map.insert 0x00101000 (TagDictEntry "LO" "Other Patient IDs") .
  Map.insert 0x00101002 (TagDictEntry "SQ" "Other Patient IDs Sequence") .
  Map.insert 0x00101010 (TagDictEntry "AS" "Patient Age") .
  Map.insert 0x00101020 (TagDictEntry "AS" "Patient Age") .
  Map.insert 0x00101030 (TagDictEntry "DS" "Patient Weight") .
  Map.insert 0x00102152 (TagDictEntry "LO" "Region of Residence") .
  Map.insert 0x001021c0 (TagDictEntry "US" "Pregnancy Status") .
  Map.insert 0x00102202 (TagDictEntry "SQ" "Patient Species Code Sequence") .
  Map.insert 0x00102293 (TagDictEntry "SQ" "Patient Breed Code Sequence") .
  Map.insert 0x00102294 (TagDictEntry "SQ" "Breed Registration Sequence") .
  Map.insert 0x00102296 (TagDictEntry "SQ" "Breed Registry Code Sequence") .
  Map.insert 0x00104000 (TagDictEntry "LT" "Patient Comments") .
  Map.insert 0x00120064 (TagDictEntry "SQ" "De-identification Method Code Sequence") .
  Map.insert 0x00120083 (TagDictEntry "SQ" "Consent for Clinical Trial Use Sequence") .
  Map.insert 0x00180010 (TagDictEntry "LO" "Contrast/Bolus Agent") .
  Map.insert 0x00180012 (TagDictEntry "SQ" "Contrast/Bolus Agent Sequence") .
  Map.insert 0x00180014 (TagDictEntry "SQ" "Contrast/Bolus Administration Route Sequence") .
  Map.insert 0x00180015 (TagDictEntry "CS" "Body Part Examined") .
  Map.insert 0x00180020 (TagDictEntry "CS" "Scanning Sequence") .
  Map.insert 0x00180021 (TagDictEntry "CS" "Sequence Variant") .
  Map.insert 0x00180022 (TagDictEntry "CS" "Scan Options") .
  Map.insert 0x00180023 (TagDictEntry "CS" "MR Acquisition Type") .
  Map.insert 0x00180024 (TagDictEntry "SH" "Sequence Name") .
  Map.insert 0x00180025 (TagDictEntry "CS" "Angio Flag") .
  Map.insert 0x00180026 (TagDictEntry "SQ" "Intervention Drug Information Sequence") .
  Map.insert 0x00180029 (TagDictEntry "SQ" "Intervention Drug Code Sequence") .
  Map.insert 0x0018002a (TagDictEntry "SQ" "Additional Drug Sequence") .
  Map.insert 0x00180036 (TagDictEntry "SQ" "Intervention Sequence") .
  Map.insert 0x00180050 (TagDictEntry "DS" "Slice Thickness") .
  Map.insert 0x00180060 (TagDictEntry "DS" "KVP") .
  Map.insert 0x00180080 (TagDictEntry "DS" "Repetition Time") .
  Map.insert 0x00180081 (TagDictEntry "DS" "Echo Time") .
  Map.insert 0x00180082 (TagDictEntry "DS" "Inversion Time") .
  Map.insert 0x00180083 (TagDictEntry "DS" "Number of Averages") .
  Map.insert 0x00180084 (TagDictEntry "DS" "Imaging Frequency") .
  Map.insert 0x00180085 (TagDictEntry "SH" "Imaged Nucleus") .
  Map.insert 0x00180086 (TagDictEntry "IS" "Echo Numbers") .
  Map.insert 0x00180087 (TagDictEntry "DS" "Magnetic Field Strength") .
  Map.insert 0x00180088 (TagDictEntry "DS" "Spacing Between Slices") .
  Map.insert 0x00180089 (TagDictEntry "IS" "Number of Phase Encoding Steps") .
  Map.insert 0x00180090 (TagDictEntry "DS" "Data Collection Diameter") .
  Map.insert 0x00180091 (TagDictEntry "IS" "Echo Train Length") .
  Map.insert 0x00180093 (TagDictEntry "DS" "Percent Sampling") .
  Map.insert 0x00180094 (TagDictEntry "DS" "Percent Phase Field of View") .
  Map.insert 0x00180095 (TagDictEntry "DS" "Pixel Bandwidth") .
  Map.insert 0x00181000 (TagDictEntry "LO" "Device Serial Number") .
  Map.insert 0x00181004 (TagDictEntry "LO" "Plate ID") .
  Map.insert 0x00181020 (TagDictEntry "LO" "Software Version") .
  Map.insert 0x00181030 (TagDictEntry "LO" "Protocol Name") .
  Map.insert 0x00181041 (TagDictEntry "DS" "Contrast/Bolus Volume") .
  Map.insert 0x00181042 (TagDictEntry "TM" "Contrast/Bolus Start Time") .
  Map.insert 0x00181044 (TagDictEntry "DS" "Contrast/Bolus Total Dose") .
  Map.insert 0x00181046 (TagDictEntry "DS" "Contrast Flow Rate") .
  Map.insert 0x00181047 (TagDictEntry "DS" "Contrast Flow Duration") .
  Map.insert 0x00181049 (TagDictEntry "DS" "Contrast/Bolus Ingredient Concentration") .
  Map.insert 0x00181072 (TagDictEntry "TM" "Radiopharmaceutical Start Time") .
  Map.insert 0x00181074 (TagDictEntry "DS" "Radionuclide Total Dose") .
  Map.insert 0x00181075 (TagDictEntry "DS" "Radionuclide Half Life") .
  Map.insert 0x00181076 (TagDictEntry "DS" "Radionuclide Positron Fraction") .
  Map.insert 0x00181081 (TagDictEntry "IS" "Low R-R Value") .
  Map.insert 0x00181082 (TagDictEntry "IS" "High R-R Value") .
  Map.insert 0x00181083 (TagDictEntry "IS" "Intervals Acquired") .
  Map.insert 0x00181084 (TagDictEntry "IS" "Intervals Rejected") .
  Map.insert 0x00181088 (TagDictEntry "IS" "Heart Rate") .
  Map.insert 0x00181100 (TagDictEntry "DS" "Reconstruction Diameter") .
  Map.insert 0x00181110 (TagDictEntry "DS" "Distance Source to Detector") .
  Map.insert 0x00181111 (TagDictEntry "DS" "Distance Source to Patient") .
  Map.insert 0x00181120 (TagDictEntry "DS" "Detector/Gantry Tilt") .
  Map.insert 0x00181130 (TagDictEntry "DS" "Table Height") .
  Map.insert 0x00181140 (TagDictEntry "CS" "Rotation Direction") .
  Map.insert 0x00181150 (TagDictEntry "IS" "Exposure Time") .
  Map.insert 0x00181151 (TagDictEntry "IS" "X-Ray Tube Current") .
  Map.insert 0x00181152 (TagDictEntry "IS" "Exposure") .
  Map.insert 0x00181155 (TagDictEntry "CS" "Radiation Setting") .
  Map.insert 0x00181160 (TagDictEntry "SH" "Filter Type") .
  Map.insert 0x00181162 (TagDictEntry "DS" "Intensifier Size") .
  Map.insert 0x00181164 (TagDictEntry "DS" "Imager Pixel Spacing") .
  Map.insert 0x00181170 (TagDictEntry "SH" "Generator Power") .
  Map.insert 0x00181181 (TagDictEntry "CS" "Collimator Type") .
  Map.insert 0x00181190 (TagDictEntry "DS" "Focal Spots") .
  Map.insert 0x00181200 (TagDictEntry "DA" "Date of Last Calibration") .
  Map.insert 0x00181201 (TagDictEntry "TM" "Time of Last Calibration") .
  Map.insert 0x00181210 (TagDictEntry "SH" "Convolution Kernel") .
  Map.insert 0x00181242 (TagDictEntry "IS" "Actual Frame Duration") .
  Map.insert 0x00181250 (TagDictEntry "SH" "Receive Coil Name") .
  Map.insert 0x00181251 (TagDictEntry "SH" "Transmit Coil Name") .
  Map.insert 0x00181260 (TagDictEntry "SH" "Plate Type") .
  Map.insert 0x00181310 (TagDictEntry "US" "Acquisition Matrix") .
  Map.insert 0x00181312 (TagDictEntry "CS" "In-plane Phase Encoding Direction") .
  Map.insert 0x00181314 (TagDictEntry "DS" "Flip Angle") .
  Map.insert 0x00181315 (TagDictEntry "CS" "Variable Flip Angle Flag") .
  Map.insert 0x00181316 (TagDictEntry "DS" "SAR") .
  Map.insert 0x00181318 (TagDictEntry "DS" "dB/dt") .
  Map.insert 0x00181400 (TagDictEntry "LO" "Acquisition Device Processing Description") .
  Map.insert 0x00181401 (TagDictEntry "LO" "Acquisition Device Processing Code") .
  Map.insert 0x00181402 (TagDictEntry "CS" "Cassette Orientation") .
  Map.insert 0x00181403 (TagDictEntry "CS" "Cassette Size") .
  Map.insert 0x00181404 (TagDictEntry "US" "Exposures on Plate") .
  Map.insert 0x00181510 (TagDictEntry "DS" "Positioner Primary Angle") .
  Map.insert 0x00181511 (TagDictEntry "DS" "Positioner Secondary Angle") .
  Map.insert 0x00185100 (TagDictEntry "CS" "Patient Position") .
  Map.insert 0x00185101 (TagDictEntry "CS" "View Position") .
  Map.insert 0x00185104 (TagDictEntry "SQ" "Projection Eponymous Name Code Sequence") .
  Map.insert 0x00186000 (TagDictEntry "DS" "Sensitivity") .
  Map.insert 0x00186011 (TagDictEntry "SQ" "Sequence of Ultrasound Regions") .
  Map.insert 0x00186012 (TagDictEntry "US" "Region Spatial Format") .
  Map.insert 0x00186014 (TagDictEntry "US" "Region Data Type") .
  Map.insert 0x00186016 (TagDictEntry "UL" "Region Flags") .
  Map.insert 0x00186018 (TagDictEntry "UL" "Region Location Min X0") .
  Map.insert 0x0018601a (TagDictEntry "UL" "Region Location Min Y0") .
  Map.insert 0x0018601c (TagDictEntry "UL" "Region Location Max X1") .
  Map.insert 0x0018601e (TagDictEntry "UL" "Region Location Max Y1") .
  Map.insert 0x00186020 (TagDictEntry "SL" "Reference Pixel X0") .
  Map.insert 0x00186022 (TagDictEntry "SL" "Reference Pixel Y0") .
  Map.insert 0x00186024 (TagDictEntry "US" "Physical Units X Direction") .
  Map.insert 0x00186026 (TagDictEntry "US" "Physical Units Y Direction") .
  Map.insert 0x00186028 (TagDictEntry "FD" "Reference Pixel Physical Value X") .
  Map.insert 0x0018602a (TagDictEntry "FD" "Reference Pixel Physical Value Y") .
  Map.insert 0x0018602c (TagDictEntry "FD" "Physical Delta X") .
  Map.insert 0x0018602e (TagDictEntry "FD" "Physical Delta Y") .
  Map.insert 0x00186030 (TagDictEntry "UL" "Transducer Frequency") .
  Map.insert 0x00189004 (TagDictEntry "CS" "Content Qualification") .
  Map.insert 0x00189005 (TagDictEntry "SH" "Pulse Sequence Name") .
  Map.insert 0x00189006 (TagDictEntry "SQ" "MR Imaging Modifier Sequence") .
  Map.insert 0x00189008 (TagDictEntry "CS" "Echo Pulse Sequence") .
  Map.insert 0x00189009 (TagDictEntry "CS" "Inversion Recovery") .
  Map.insert 0x00189010 (TagDictEntry "CS" "Flow Compensation") .
  Map.insert 0x00189011 (TagDictEntry "CS" "Multiple Spin Echo") .
  Map.insert 0x00189012 (TagDictEntry "CS" "Multi-planar Excitation") .
  Map.insert 0x00189014 (TagDictEntry "CS" "Phase Contrast") .
  Map.insert 0x00189015 (TagDictEntry "CS" "Time of Flight Contrast") .
  Map.insert 0x00189016 (TagDictEntry "CS" "Spoiling") .
  Map.insert 0x00189017 (TagDictEntry "CS" "Steady State Pulse Sequence") .
  Map.insert 0x00189018 (TagDictEntry "CS" "Echo Planar Pulse Sequence") .
  Map.insert 0x00189020 (TagDictEntry "CS" "Magnetization Transfer") .
  Map.insert 0x00189021 (TagDictEntry "CS" "T2 Preparation") .
  Map.insert 0x00189022 (TagDictEntry "CS" "Blood Signal Nulling") .
  Map.insert 0x00189024 (TagDictEntry "CS" "Saturation Recovery") .
  Map.insert 0x00189025 (TagDictEntry "CS" "Spectrally Selected Suppression") .
  Map.insert 0x00189026 (TagDictEntry "CS" "Spectrally Selected Excitation") .
  Map.insert 0x00189027 (TagDictEntry "CS" "Spatial Pre-saturation") .
  Map.insert 0x00189028 (TagDictEntry "CS" "Tagging") .
  Map.insert 0x00189029 (TagDictEntry "CS" "Oversampling Phase") .
  Map.insert 0x00189032 (TagDictEntry "CS" "Geometry of k-Space Traversal") .
  Map.insert 0x00189033 (TagDictEntry "CS" "Segmented k-Space Traversal") .
  Map.insert 0x00189034 (TagDictEntry "CS" "Rectilinear Phase Encode Reordering") .
  Map.insert 0x00189035 (TagDictEntry "FD" "Tag Thickness") .
  Map.insert 0x00189036 (TagDictEntry "CS" "Partial Fourier Direction") .
  Map.insert 0x00189037 (TagDictEntry "CS" "Cardiac Synchronization Technique") .
  Map.insert 0x00189041 (TagDictEntry "LO" "Receive Coil Manufacturer Name") .
  Map.insert 0x00189042 (TagDictEntry "SQ" "MR Receive Coil Sequence") .
  Map.insert 0x00189043 (TagDictEntry "CS" "Receive Coil Type") .
  Map.insert 0x00189044 (TagDictEntry "CS" "Quadrature Receive Coil") .
  Map.insert 0x00189045 (TagDictEntry "SQ" "Multi-Coil Definition Sequence") .
  Map.insert 0x00189046 (TagDictEntry "LO" "Multi-Coil Configuration") .
  Map.insert 0x00189047 (TagDictEntry "SH" "Multi-Coil Element Name") .
  Map.insert 0x00189048 (TagDictEntry "CS" "Multi-Coil Element Used") .
  Map.insert 0x00189049 (TagDictEntry "SQ" "MR Transmit Coil Sequence") .
  Map.insert 0x00189050 (TagDictEntry "LO" "Transmit Coil Manufacturer Name") .
  Map.insert 0x00189051 (TagDictEntry "CS" "Transmit Coil Type") .
  Map.insert 0x00189053 (TagDictEntry "FD" "Chemical Shift Reference") .
  Map.insert 0x00189058 (TagDictEntry "US" "MR Acquisition Frequency Encoding Steps") .
  Map.insert 0x00189059 (TagDictEntry "CS" "De-coupling") .
  Map.insert 0x00189064 (TagDictEntry "CS" "k-space Filtering") .
  Map.insert 0x00189069 (TagDictEntry "FD" "Parallel Reduction Factor In-plane") .
  Map.insert 0x00189073 (TagDictEntry "FD" "Acquisition Duration") .
  Map.insert 0x00189074 (TagDictEntry "DT" "Frame Acquisition DateTime") .
  Map.insert 0x00189076 (TagDictEntry "SQ" "Diffusion Gradient Direction Sequence") .
  Map.insert 0x00189077 (TagDictEntry "CS" "Parallel Acquisition") .
  Map.insert 0x00189080 (TagDictEntry "ST" "Metabolite Map Description") .
  Map.insert 0x00189081 (TagDictEntry "CS" "Partial Fourier") .
  Map.insert 0x00189082 (TagDictEntry "FD" "Effective Echo Time") .
  Map.insert 0x00189083 (TagDictEntry "SQ" "Metabolite Map Code Sequence") .
  Map.insert 0x00189084 (TagDictEntry "SQ" "Chemical Shift Sequence") .
  Map.insert 0x00189090 (TagDictEntry "FD" "Velocity Encoding Direction") .
  Map.insert 0x00189091 (TagDictEntry "FD" "Velocity Encoding Minimum Value") .
  Map.insert 0x00189093 (TagDictEntry "US" "Number of k-Space Trajectories") .
  Map.insert 0x00189094 (TagDictEntry "CS" "Coverage of k-Space") .
  Map.insert 0x00189098 (TagDictEntry "FD" "Transmitter Frequency") .
  Map.insert 0x00189100 (TagDictEntry "CS" "Resonant Nucleus") .
  Map.insert 0x00189101 (TagDictEntry "CS" "Frequency Correction") .
  Map.insert 0x00189103 (TagDictEntry "SQ" "MR Spectroscopy FOV/Geometry Sequence") .
  Map.insert 0x00189107 (TagDictEntry "SQ" "MR Spatial Saturation Sequence") .
  Map.insert 0x00189112 (TagDictEntry "SQ" "MR Timing and Related Parameters Sequence") .
  Map.insert 0x00189114 (TagDictEntry "SQ" "MR Echo Sequence") .
  Map.insert 0x00189115 (TagDictEntry "SQ" "MR Modifier Sequence") .
  Map.insert 0x00189117 (TagDictEntry "SQ" "MR Diffusion Sequence") .
  Map.insert 0x00189118 (TagDictEntry "SQ" "Cardiac Synchronization Sequence") .
  Map.insert 0x00189119 (TagDictEntry "SQ" "MR Averages Sequence") .
  Map.insert 0x00189125 (TagDictEntry "SQ" "MR FOV/Geometry Sequence") .
  Map.insert 0x00189126 (TagDictEntry "SQ" "Volume Localization Sequence") .
  Map.insert 0x00189151 (TagDictEntry "DT" "Frame Reference DateTime") .
  Map.insert 0x00189152 (TagDictEntry "SQ" "MR Metabolite Map Sequence") .
  Map.insert 0x00189155 (TagDictEntry "FD" "Parallel Reduction Factor out-of-plane") .
  Map.insert 0x00189168 (TagDictEntry "FD" "Parallel Reduction Factor Second Inplane") .
  Map.insert 0x00189170 (TagDictEntry "CS" "Respiratory Motion Compensation Technique") .
  Map.insert 0x00189171 (TagDictEntry "CS" "Respiratory Signal Source") .
  Map.insert 0x00189172 (TagDictEntry "CS" "Bulk Motion Compensation Technique") .
  Map.insert 0x00189174 (TagDictEntry "CS" "Applicable Safety Standard Agency") .
  Map.insert 0x00189175 (TagDictEntry "CS" "Applicable Safety Standard Description") .
  Map.insert 0x00189176 (TagDictEntry "SQ" "Operating Mode Sequence") .
  Map.insert 0x00189177 (TagDictEntry "CS" "Operating Mode Type") .
  Map.insert 0x00189178 (TagDictEntry "CS" "Operating Mode") .
  Map.insert 0x00189179 (TagDictEntry "CS" "Specific Absorption Rate Definition") .
  Map.insert 0x00189180 (TagDictEntry "CS" "Gradient Output Type") .
  Map.insert 0x00189181 (TagDictEntry "FD" "Specific Absorption Rate Value") .
  Map.insert 0x00189182 (TagDictEntry "FD" "Gradient Output") .
  Map.insert 0x00189197 (TagDictEntry "SQ" "MR Velocity Encoding Sequence") .
  Map.insert 0x00189199 (TagDictEntry "CS" "Water Referenced Phase Correction") .
  Map.insert 0x00189200 (TagDictEntry "CS" "MR Spectroscopy Acquisition Type") .
  Map.insert 0x00189226 (TagDictEntry "SQ" "MR Image Frame Type Sequence") .
  Map.insert 0x00189227 (TagDictEntry "SQ" "MR Spectroscopy Frame Type Sequence") .
  Map.insert 0x00189231 (TagDictEntry "US" "MR Acquisition Phase Encoding Steps in-plane") .
  Map.insert 0x00189232 (TagDictEntry "US" "MR Acquisition Phase Encoding Steps out-of-plane") .
  Map.insert 0x00189239 (TagDictEntry "SQ" "Specific Absorption Rate Sequence") .
  Map.insert 0x00189240 (TagDictEntry "US" "RF Echo Train Length") .
  Map.insert 0x00189241 (TagDictEntry "US" "Gradient Echo Train Length") .
  Map.insert 0x00189301 (TagDictEntry "SQ" "CT Acquisition Type Sequence") .
  Map.insert 0x00189304 (TagDictEntry "SQ" "CT Acquisition Details Sequence") .
  Map.insert 0x00189308 (TagDictEntry "SQ" "CT Table Dynamics Sequence") .
  Map.insert 0x00189312 (TagDictEntry "SQ" "CT Geometry Sequence") .
  Map.insert 0x00189314 (TagDictEntry "SQ" "CT Reconstruction Sequence") .
  Map.insert 0x00189321 (TagDictEntry "SQ" "CT Exposure Sequence") .
  Map.insert 0x00189325 (TagDictEntry "SQ" "CT X-Ray Details Sequence") .
  Map.insert 0x00189326 (TagDictEntry "SQ" "CT Position Sequence") .
  Map.insert 0x00189329 (TagDictEntry "SQ" "CT Image Frame Type Sequence") .
  Map.insert 0x00189338 (TagDictEntry "SQ" "Contrast/Bolus Ingredient Code Sequence") .
  Map.insert 0x00189340 (TagDictEntry "SQ" "Contrast Administration Profile Sequence") .
  Map.insert 0x00189341 (TagDictEntry "SQ" "Contrast/Bolus Usage Sequence") .
  Map.insert 0x00189345 (TagDictEntry "FD" "CTDIvol") .
  Map.insert 0x00189346 (TagDictEntry "SQ" "CTDI Phantom Type Code Sequence") .
  Map.insert 0x00189360 (TagDictEntry "SQ" "CT Additional X-Ray Source Sequence") .
  Map.insert 0x00189401 (TagDictEntry "SQ" "Projection Pixel Calibration Sequence") .
  Map.insert 0x00189405 (TagDictEntry "SQ" "Positioner Position Sequence") .
  Map.insert 0x00189406 (TagDictEntry "SQ" "Table Position Sequence") .
  Map.insert 0x00189407 (TagDictEntry "SQ" "Collimator Shape Sequence") .
  Map.insert 0x00189412 (TagDictEntry "SQ" "XA/XRF Frame Characteristics Sequence") .
  Map.insert 0x00189417 (TagDictEntry "SQ" "Frame Acquisition Sequence") .
  Map.insert 0x00189432 (TagDictEntry "SQ" "Field of View Sequence") .
  Map.insert 0x00189434 (TagDictEntry "SQ" "Exposure Control Sensing Regions Sequence") .
  Map.insert 0x00189451 (TagDictEntry "SQ" "Frame Detector Parameters Sequence") .
  Map.insert 0x00189455 (TagDictEntry "SQ" "Calibration Sequence") .
  Map.insert 0x00189456 (TagDictEntry "SQ" "Object Thickness Sequence") .
  Map.insert 0x00189462 (TagDictEntry "SQ" "Isocenter Reference System Sequence") .
  Map.insert 0x00189472 (TagDictEntry "SQ" "Frame Display Shutter Sequence") .
  Map.insert 0x00189476 (TagDictEntry "SQ" "X-Ray Geometry Sequence") .
  Map.insert 0x00189477 (TagDictEntry "SQ" "Irradiation Event Identification Sequence") .
  Map.insert 0x00189504 (TagDictEntry "SQ" "X-Ray 3D Frame Type Sequence") .
  Map.insert 0x00189506 (TagDictEntry "SQ" "Contributing Sources Sequence") .
  Map.insert 0x00189507 (TagDictEntry "SQ" "X-Ray 3D Acquisition Sequence") .
  Map.insert 0x00189530 (TagDictEntry "SQ" "X-Ray 3D Reconstruction Sequence") .
  Map.insert 0x00189538 (TagDictEntry "SQ" "Per Projection Acquisition Sequence") .
  Map.insert 0x00189601 (TagDictEntry "SQ" "Diffusion b-matrix Sequence") .
  Map.insert 0x00189732 (TagDictEntry "SQ" "PET Frame Acquisition Sequence") .
  Map.insert 0x00189733 (TagDictEntry "SQ" "PET Detector Motion Details Sequence") .
  Map.insert 0x00189734 (TagDictEntry "SQ" "PET Table Dynamics Sequence") .
  Map.insert 0x00189735 (TagDictEntry "SQ" "PET Position Sequence") .
  Map.insert 0x00189736 (TagDictEntry "SQ" "PET Frame Correction Factors Sequence") .
  Map.insert 0x00189737 (TagDictEntry "SQ" "Radiopharmaceutical Usage Sequence") .
  Map.insert 0x00189749 (TagDictEntry "SQ" "PET Reconstruction Sequence") .
  Map.insert 0x00189751 (TagDictEntry "SQ" "PET Frame Type Sequence") .
  Map.insert 0x00189771 (TagDictEntry "SQ" "Patient Physiological State Sequence") .
  Map.insert 0x00189772 (TagDictEntry "SQ" "Patient Physiological State Code Sequence") .
  Map.insert 0x00189803 (TagDictEntry "SQ" "Excluded Intervals Sequence") .
  Map.insert 0x00189806 (TagDictEntry "SQ" "US Image Description Sequence") .
  Map.insert 0x00189807 (TagDictEntry "SQ" "Image Data Type Sequence") .
  Map.insert 0x00189809 (TagDictEntry "SQ" "Transducer Scan Pattern Code Sequence") .
  Map.insert 0x0018980d (TagDictEntry "SQ" "Transducer Geometry Code Sequence") .
  Map.insert 0x0018980e (TagDictEntry "SQ" "Transducer Beam Steering Code Sequence") .
  Map.insert 0x0018980f (TagDictEntry "SQ" "Transducer Application Code Sequence") .
  Map.insert 0x0018a001 (TagDictEntry "SQ" "Contributing Equipment Sequence") .
  Map.insert 0x0020000d (TagDictEntry "UI" "Study Instance UID") .
  Map.insert 0x0020000e (TagDictEntry "UI" "Series Instance UID") .
  Map.insert 0x00200010 (TagDictEntry "SH" "Study ID") .
  Map.insert 0x00200011 (TagDictEntry "IS" "Series Number") .
  Map.insert 0x00200012 (TagDictEntry "IS" "Acquisition Number") .
  Map.insert 0x00200013 (TagDictEntry "IS" "Instance Number") .
  Map.insert 0x00200020 (TagDictEntry "CS" "Patient Orientation") .
  Map.insert 0x00200032 (TagDictEntry "DS" "Image Position (Patient)") .
  Map.insert 0x00200037 (TagDictEntry "DS" "Image Orientation (Patient)") .
  Map.insert 0x00200052 (TagDictEntry "UI" "Frame of Reference UID") .
  Map.insert 0x00200100 (TagDictEntry "IS" "Temporal Position Identifier") .
  Map.insert 0x00200105 (TagDictEntry "IS" "Number of Temporal Positions") .
  Map.insert 0x00201002 (TagDictEntry "IS" "Images in Acquisition") .
  Map.insert 0x00201040 (TagDictEntry "LO" "Patient Reference Indicator") .
  Map.insert 0x00201041 (TagDictEntry "DS" "Slice Position") .
  Map.insert 0x00204000 (TagDictEntry "LT" "Image Comments") .
  Map.insert 0x00209056 (TagDictEntry "SH" "Stack ID") .
  Map.insert 0x00209057 (TagDictEntry "UL" "In-Stack Position Number") .
  Map.insert 0x00209071 (TagDictEntry "SQ" "Frame Anatomy Sequence") .
  Map.insert 0x00209072 (TagDictEntry "CS" "Frame Laterality") .
  Map.insert 0x00209111 (TagDictEntry "SQ" "Frame Content Sequence") .
  Map.insert 0x00209113 (TagDictEntry "SQ" "Plane Position Sequence") .
  Map.insert 0x00209116 (TagDictEntry "SQ" "Plane Orientation Sequence") .
  Map.insert 0x00209128 (TagDictEntry "UL" "TemporalPositionIndex") .
  Map.insert 0x00209157 (TagDictEntry "UL" "Dimension Index Values") .
  Map.insert 0x00209164 (TagDictEntry "AT" "Dimension Organization UID") .
  Map.insert 0x00209165 (TagDictEntry "AT" "Dimension Index Pointer") .
  Map.insert 0x00209167 (TagDictEntry "UI" "Functional Group Pointer") .
  Map.insert 0x00209213 (TagDictEntry "LO" "Dimension Index Private Creator") .
  Map.insert 0x00209221 (TagDictEntry "SQ" "Dimension Organization Sequence") .
  Map.insert 0x00209222 (TagDictEntry "SQ" "Dimension Index Sequence") .
  Map.insert 0x00209238 (TagDictEntry "LO" "Functional Group Private Creator") .
  Map.insert 0x00209253 (TagDictEntry "SQ" "Respiratory Synchronization Sequence") .
  Map.insert 0x00209254 (TagDictEntry "FD" "Respiratory Interval Time") .
  Map.insert 0x00209255 (TagDictEntry "FD" "Nominal Respiratory Trigger Delay Time") .
  Map.insert 0x0020930e (TagDictEntry "SQ" "Plane Position (Volume) Sequence") .
  Map.insert 0x0020930f (TagDictEntry "SQ" "Plane Orientation (Volume) Sequence") .
  Map.insert 0x00209310 (TagDictEntry "SQ" "Temporal Position Sequence") .
  Map.insert 0x00209421 (TagDictEntry "LO" "Dimension Description Label") .
  Map.insert 0x00209450 (TagDictEntry "SQ" "Patient Orientation in Frame Sequence") .
  Map.insert 0x00209529 (TagDictEntry "SQ" "Contributing SOP Instances Reference Sequence") .
  Map.insert 0x00220015 (TagDictEntry "SQ" "Acquisition Device Type Code Sequence") .
  Map.insert 0x00220016 (TagDictEntry "SQ" "Illumination Type Code Sequence") .
  Map.insert 0x00220017 (TagDictEntry "SQ" "Light Path Filter Type Stack Code Sequence") .
  Map.insert 0x00220018 (TagDictEntry "SQ" "Image Path Filter Type Stack Code Sequence") .
  Map.insert 0x00220019 (TagDictEntry "SQ" "Lenses Code Sequence") .
  Map.insert 0x0022001a (TagDictEntry "SQ" "Channel Description Code Sequence") .
  Map.insert 0x0022001b (TagDictEntry "SQ" "Refractive State Sequence") .
  Map.insert 0x0022001c (TagDictEntry "SQ" "Mydriatic Agent Code Sequence") .
  Map.insert 0x0022001d (TagDictEntry "SQ" "Relative Image Position Code Sequence") .
  Map.insert 0x00220020 (TagDictEntry "SQ" "Stereo Pairs Sequence") .
  Map.insert 0x00220021 (TagDictEntry "SQ" "Left Image Sequence") .
  Map.insert 0x00220022 (TagDictEntry "SQ" "Right Image Sequence") .
  Map.insert 0x00220031 (TagDictEntry "SQ" "Ophthalmic Frame Location Sequence") .
  Map.insert 0x00220042 (TagDictEntry "SQ" "Mydriatic Agent Concentration Units Sequence") .
  Map.insert 0x00220058 (TagDictEntry "SQ" "Mydriatic Agent Sequence") .
  Map.insert 0x00280002 (TagDictEntry "US" "Samples per Pixel") .
  Map.insert 0x00280004 (TagDictEntry "CS" "Photometric Interpretation") .
  Map.insert 0x00280006 (TagDictEntry "US" "Planar Configuration") .
  Map.insert 0x00280008 (TagDictEntry "IS" "Number of Frames") .
  Map.insert 0x00280010 (TagDictEntry "US" "Rows") .
  Map.insert 0x00280011 (TagDictEntry "US" "Columns") .
  Map.insert 0x00280014 (TagDictEntry "US" "Ultrasound Color Data Present") .
  Map.insert 0x00280030 (TagDictEntry "DS" "Pixel Spacing") .
  Map.insert 0x00280051 (TagDictEntry "CS" "Corrected Image") .
  Map.insert 0x00280100 (TagDictEntry "US" "Bits Allocated") .
  Map.insert 0x00280101 (TagDictEntry "US" "Bits Stored") .
  Map.insert 0x00280102 (TagDictEntry "US" "High Bit") .
  Map.insert 0x00280103 (TagDictEntry "US" "Pixel Representation") .
  Map.insert 0x00280106 (TagDictEntry "US" "Smallest Image Pixel Value") .
  Map.insert 0x00280107 (TagDictEntry "US" "Largest Image Pixel Value") .
  Map.insert 0x00280301 (TagDictEntry "CS" "Burned In Annotation") .
  Map.insert 0x00281040 (TagDictEntry "CS" "Pixel Intensity Relationship") .
  Map.insert 0x00281050 (TagDictEntry "DS" "Window Centre") .
  Map.insert 0x00281051 (TagDictEntry "DS" "Window Width") .
  Map.insert 0x00281052 (TagDictEntry "DS" "Rescale Intercept") .
  Map.insert 0x00281053 (TagDictEntry "DS" "Rescale Slope") .
  Map.insert 0x00281054 (TagDictEntry "LO" "Rescale Type") .
  Map.insert 0x00281055 (TagDictEntry "LO" "Window Width & Centre Explanation") .
  Map.insert 0x00281352 (TagDictEntry "SQ" "Partial View Code Sequence") .
  Map.insert 0x00281401 (TagDictEntry "SQ" "Data Frame Assignment Sequence") .
  Map.insert 0x00281404 (TagDictEntry "SQ" "Blending LUT 1 Sequence") .
  Map.insert 0x0028140b (TagDictEntry "SQ" "Enhanced Palette Color Lookup Table Sequence") .
  Map.insert 0x0028140c (TagDictEntry "SQ" "Blending LUT 2 Sequence") .
  Map.insert 0x00282110 (TagDictEntry "CS" "Lossy Image Compression") .
  Map.insert 0x00282112 (TagDictEntry "DS" "Lossy Image Compression Ratio") .
  Map.insert 0x00283000 (TagDictEntry "SQ" "Modality LUT Sequence") .
  Map.insert 0x00283003 (TagDictEntry "LO" "LUT Explanation") .
  Map.insert 0x00283010 (TagDictEntry "SQ" "VOI LUT Sequence") .
  Map.insert 0x00283110 (TagDictEntry "SQ" "Softcopy VOI LUT Sequence") .
  Map.insert 0x00286100 (TagDictEntry "SQ" "Mask Subtraction Sequence") .
  Map.insert 0x00289001 (TagDictEntry "UL" "Data Point Rows") .
  Map.insert 0x00289002 (TagDictEntry "UL" "Data Point Columns") .
  Map.insert 0x00289110 (TagDictEntry "SQ" "Pixel Measures Sequence") .
  Map.insert 0x00289132 (TagDictEntry "SQ" "Frame VOI LUT Sequence") .
  Map.insert 0x00289145 (TagDictEntry "SQ" "Pixel Value Transformation Sequence") .
  Map.insert 0x00289415 (TagDictEntry "SQ" "Frame Pixel Shift Sequence") .
  Map.insert 0x00289422 (TagDictEntry "SQ" "Pixel Intensity Relationship LUT Sequence") .
  Map.insert 0x00289443 (TagDictEntry "SQ" "Frame Pixel Data Properties Sequence") .
  Map.insert 0x00289501 (TagDictEntry "SQ" "Pixel Shift Sequence") .
  Map.insert 0x00289502 (TagDictEntry "SQ" "Region Pixel Shift Sequence") .
  Map.insert 0x00289505 (TagDictEntry "SQ" "Multi-frame Presentation Sequence") .
  Map.insert 0x00321031 (TagDictEntry "SQ" "Requesting Physician Identification Sequence") .
  Map.insert 0x00321032 (TagDictEntry "PN" "Requesting Physician") .
  Map.insert 0x00321033 (TagDictEntry "LO" "Requesting Service") .
  Map.insert 0x00321034 (TagDictEntry "SQ" "Requesting Physician Identification Sequence") .
  Map.insert 0x00321060 (TagDictEntry "LO" "Request Procedure Description") .
  Map.insert 0x00321064 (TagDictEntry "SQ" "Requesting Service Code Sequence") .
  Map.insert 0x00324000 (TagDictEntry "LT" "Study Comments") .
  Map.insert 0x00380004 (TagDictEntry "SQ" "Referenced Patient Alias Sequence") .
  Map.insert 0x00380014 (TagDictEntry "SQ" "Issuer of Admission ID Sequence") .
  Map.insert 0x00380064 (TagDictEntry "SQ" "Issuer of Service Episode ID Sequence") .
  Map.insert 0x00380100 (TagDictEntry "SQ" "Pertinent Documents Sequence") .
  Map.insert 0x00380502 (TagDictEntry "SQ" "Patient Clinical Trial Participation Sequence") .
  Map.insert 0x003a0200 (TagDictEntry "SQ" "Channel Definition Sequence") .
  Map.insert 0x003a0208 (TagDictEntry "SQ" "Channel Source Sequence") .
  Map.insert 0x003a0209 (TagDictEntry "SQ" "Channel Source Modifiers Sequence") .
  Map.insert 0x003a020a (TagDictEntry "SQ" "Source Waveform Sequence") .
  Map.insert 0x003a0211 (TagDictEntry "SQ" "Channel Sensitivity Units Sequence") .
  Map.insert 0x003a0240 (TagDictEntry "SQ" "Waveform Presentation Group Sequence") .
  Map.insert 0x003a0242 (TagDictEntry "SQ" "Channel Display Sequence") .
  Map.insert 0x003a0300 (TagDictEntry "SQ" "Multiplexed Audio Channels Description Code Sequence") .
  Map.insert 0x00400002 (TagDictEntry "DA" "Scheduled Procedure Step Start Date") .
  Map.insert 0x00400003 (TagDictEntry "TM" "Scheduled Procedure Step Start Time") .
  Map.insert 0x00400004 (TagDictEntry "DA" "Scheduled Procedure Step End Date") .
  Map.insert 0x00400005 (TagDictEntry "TM" "Scheduled Procedure Step End Time") .
  Map.insert 0x00400007 (TagDictEntry "LO" "Scheduled Procedure Step Description") .
  Map.insert 0x00400008 (TagDictEntry "SQ" "Scheduled Procedure Step Sequence") .
  Map.insert 0x00400009 (TagDictEntry "SH" "Scheduled Procedure Step ID") .
  Map.insert 0x0040000a (TagDictEntry "SQ" "Stage Code Sequence") .
  Map.insert 0x0040000b (TagDictEntry "SQ" "Scheduled Performing Physician Identification Sequence") .
  Map.insert 0x00400026 (TagDictEntry "SQ" "Order Placer Identifier Sequence") .
  Map.insert 0x00400027 (TagDictEntry "SQ" "Order Filler Identifier Sequence") .
  Map.insert 0x00400036 (TagDictEntry "SQ" "Assigning Facility Sequence") .
  Map.insert 0x00400039 (TagDictEntry "SQ" "Assigning Jurisdiction Code Sequence") .
  Map.insert 0x0040003a (TagDictEntry "SQ" "Assigning Agency or Department Code Sequence") .
  Map.insert 0x00400100 (TagDictEntry "SQ" "Scheduled Procedure Step Sequence") .
  Map.insert 0x00400220 (TagDictEntry "SQ" "Referenced Non-Image Composite SOP Instance Sequence") .
  Map.insert 0x00400241 (TagDictEntry "AE" "Performed Station AE Title") .
  Map.insert 0x00400244 (TagDictEntry "DA" "Performed Procedure Step Start Date") .
  Map.insert 0x00400245 (TagDictEntry "TM" "Performed Procedure Step Start Time") .
  Map.insert 0x00400250 (TagDictEntry "DA" "Performed Procedure Step End Date") .
  Map.insert 0x00400251 (TagDictEntry "TM" "Performed Procedure Step End Time") .
  Map.insert 0x00400253 (TagDictEntry "SH" "Performed Procedure Step ID") .
  Map.insert 0x00400254 (TagDictEntry "LO" "Performed Procedure Step Description") .
  Map.insert 0x00400260 (TagDictEntry "SQ" "Performed Protocol Code Sequence") .
  Map.insert 0x00400270 (TagDictEntry "SQ" "Scheduled Step Attributes Sequence") .
  Map.insert 0x00400275 (TagDictEntry "SQ" "Request Attributes Sequence") .
  Map.insert 0x00400280 (TagDictEntry "ST" "Comments on the Performed Procedure Step") .
  Map.insert 0x00400281 (TagDictEntry "SQ" "Performed Procedure Step Discontinuation Reason Code Sequence") .
  Map.insert 0x00400293 (TagDictEntry "SQ" "Quantity Sequence") .
  Map.insert 0x00400295 (TagDictEntry "SQ" "Measuring Units Sequence") .
  Map.insert 0x00400296 (TagDictEntry "SQ" "Billing Item Sequence") .
  Map.insert 0x0040030e (TagDictEntry "SQ" "Exposure Dose Sequence") .
  Map.insert 0x00400320 (TagDictEntry "SQ" "Billing Procedure Step Sequence") .
  Map.insert 0x00400321 (TagDictEntry "SQ" "Film Consumption Sequence") .
  Map.insert 0x00400324 (TagDictEntry "SQ" "Billing Supplies and Devices Sequence") .
  Map.insert 0x00400340 (TagDictEntry "SQ" "Performed Series Sequence") .
  Map.insert 0x00400440 (TagDictEntry "SQ" "Protocol Context Sequence") .
  Map.insert 0x00400441 (TagDictEntry "SQ" "Content Item Modifier Sequence") .
  Map.insert 0x00400500 (TagDictEntry "SQ" "Scheduled Specimen Sequence") .
  Map.insert 0x00400513 (TagDictEntry "SQ" "Issuer of the Container Identifier Sequence") .
  Map.insert 0x00400515 (TagDictEntry "SQ" "Alternate Container Identifier Sequence") .
  Map.insert 0x00400518 (TagDictEntry "SQ" "Container Type Code Sequence") .
  Map.insert 0x00400520 (TagDictEntry "SQ" "Container Component Sequence") .
  Map.insert 0x00400555 (TagDictEntry "SQ" "Acquisition Context Sequence") .
  Map.insert 0x0040059a (TagDictEntry "SQ" "Specimen Type Code Sequence") .
  Map.insert 0x00400560 (TagDictEntry "SQ" "Specimen Description Sequence") .
  Map.insert 0x00400562 (TagDictEntry "SQ" "Issuer of the Specimen Identifier Sequence") .
  Map.insert 0x00400610 (TagDictEntry "SQ" "Specimen Preparation Sequence") .
  Map.insert 0x00400612 (TagDictEntry "SQ" "Specimen Preparation Step Content Item Sequence") .
  Map.insert 0x00400620 (TagDictEntry "SQ" "Specimen Localization Content Item Sequence") .
  Map.insert 0x0040071a (TagDictEntry "SQ" "Image Center Point Coordinates Sequence") .
  Map.insert 0x004008d8 (TagDictEntry "SQ" "Pixel Spacing Sequence") .
  Map.insert 0x004008da (TagDictEntry "SQ" "Coordinate System Axis Code Sequence") .
  Map.insert 0x004008ea (TagDictEntry "SQ" "Measurement Units Code Sequence") .
  Map.insert 0x00401001 (TagDictEntry "SH" "Requested Procedure ID") .
  Map.insert 0x0040100a (TagDictEntry "SQ" "Reason for Requested Procedure Code Sequence") .
  Map.insert 0x00401011 (TagDictEntry "SQ" "Intended Recipients of Results Identification Sequence") .
  Map.insert 0x00401012 (TagDictEntry "SQ" "Reason For Performed Procedure Code Sequence") .
  Map.insert 0x00404004 (TagDictEntry "SQ" "Scheduled Processing Applications Code Sequence") .
  Map.insert 0x00404007 (TagDictEntry "SQ" "Performed Processing Applications Code Sequence") .
  Map.insert 0x00404009 (TagDictEntry "SQ" "Human Performer Code Sequence") .
  Map.insert 0x00404015 (TagDictEntry "SQ" "Resulting General Purpose Performed Procedure Steps Sequence") .
  Map.insert 0x00404016 (TagDictEntry "SQ" "Referenced General Purpose Scheduled Procedure Step Sequence") .
  Map.insert 0x00404018 (TagDictEntry "SQ" "Scheduled Workitem Code Sequence") .
  Map.insert 0x00404019 (TagDictEntry "SQ" "Performed Workitem Code Sequence") .
  Map.insert 0x00404021 (TagDictEntry "SQ" "Input Information Sequence") .
  Map.insert 0x00404022 (TagDictEntry "SQ" "Relevant Information Sequence") .
  Map.insert 0x00404025 (TagDictEntry "SQ" "Scheduled Station Name Code Sequence") .
  Map.insert 0x00404026 (TagDictEntry "SQ" "Scheduled Station Class Code Sequence") .
  Map.insert 0x00404027 (TagDictEntry "SQ" "Scheduled Station Geographic Location Code Sequence") .
  Map.insert 0x00404028 (TagDictEntry "SQ" "Performed Station Name Code Sequence") .
  Map.insert 0x00404029 (TagDictEntry "SQ" "Performed Station Class Code Sequence") .
  Map.insert 0x00404030 (TagDictEntry "SQ" "Performed Station Geographic Location Code Sequence") .
  Map.insert 0x00404031 (TagDictEntry "SQ" "Requested Subsequent Workitem Code Sequence") .
  Map.insert 0x00404032 (TagDictEntry "SQ" "Non-DICOM Output Code Sequence") .
  Map.insert 0x00404033 (TagDictEntry "SQ" "Output Information Sequence") .
  Map.insert 0x00404034 (TagDictEntry "SQ" "Scheduled Human Performers Sequence") .
  Map.insert 0x00409094 (TagDictEntry "SQ" "Referenced Image Real World Value Mapping Sequence") .
  Map.insert 0x00409096 (TagDictEntry "SQ" "Real World Value Mapping Sequence") .
  Map.insert 0x00409098 (TagDictEntry "SQ" "Pixel Value Mapping Code Sequence") .
  Map.insert 0x00409210 (TagDictEntry "SH" "LUT Label") .
  Map.insert 0x0040a043 (TagDictEntry "SQ" "Concept Name Code Sequence") .
  Map.insert 0x0040a073 (TagDictEntry "SQ" "Verifying Observer Sequence") .
  Map.insert 0x0040a078 (TagDictEntry "SQ" "Author Observer Sequence") .
  Map.insert 0x0040a07a (TagDictEntry "SQ" "Participant Sequence") .
  Map.insert 0x0040a07c (TagDictEntry "SQ" "Custodial Organization Sequence") .
  Map.insert 0x0040a088 (TagDictEntry "SQ" "Verifying Observer Identification Code Sequence") .
  Map.insert 0x0040a168 (TagDictEntry "SQ" "Concept Code Sequence") .
  Map.insert 0x0040a170 (TagDictEntry "SQ" "Purpose of Reference Code Sequence") .
  Map.insert 0x0040a195 (TagDictEntry "SQ" "Modifier Code Sequence") .
  Map.insert 0x0040a300 (TagDictEntry "SQ" "Measured Value Sequence") .
  Map.insert 0x0040a301 (TagDictEntry "SQ" "Numeric Value Qualifier Code Sequence") .
  Map.insert 0x0040a360 (TagDictEntry "SQ" "Predecessor Documents Sequence") .
  Map.insert 0x0040a370 (TagDictEntry "SQ" "Referenced Request Sequence") .
  Map.insert 0x0040a372 (TagDictEntry "SQ" "Performed Procedure Code Sequence") .
  Map.insert 0x0040a375 (TagDictEntry "SQ" "Current Requested Procedure Evidence Sequence") .
  Map.insert 0x0040a385 (TagDictEntry "SQ" "Pertinent Other Evidence Sequence") .
  Map.insert 0x0040a390 (TagDictEntry "SQ" "HL7 Structured Document Reference Sequence") .
  Map.insert 0x0040a504 (TagDictEntry "SQ" "Content Template Sequence") .
  Map.insert 0x0040a525 (TagDictEntry "SQ" "Identical Documents Sequence") .
  Map.insert 0x0040a730 (TagDictEntry "SQ" "Content Sequence") .
  Map.insert 0x0040b020 (TagDictEntry "SQ" "Waveform Annotation Sequence") .
  Map.insert 0x0040e006 (TagDictEntry "SQ" "HL7 Document Type Code Sequence") .
  Map.insert 0x00420013 (TagDictEntry "SQ" "Source Instance Sequence") .
  Map.insert 0x00440007 (TagDictEntry "SQ" "Product Type Code Sequence") .
  Map.insert 0x00440013 (TagDictEntry "SQ" "Product Parameter Sequence") .
  Map.insert 0x00440019 (TagDictEntry "SQ" "Substance Administration Parameter Sequence") .
  Map.insert 0x00460014 (TagDictEntry "SQ" "Right Lens Sequence") .
  Map.insert 0x00460015 (TagDictEntry "SQ" "Left Lens Sequence") .
  Map.insert 0x00460016 (TagDictEntry "SQ" "Unspecified Laterality Lens Sequence") .
  Map.insert 0x00460018 (TagDictEntry "SQ" "Cylinder Sequence") .
  Map.insert 0x00460028 (TagDictEntry "SQ" "Prism Sequence") .
  Map.insert 0x00460050 (TagDictEntry "SQ" "Autorefraction Right Eye Sequence") .
  Map.insert 0x00460052 (TagDictEntry "SQ" "Autorefraction Left Eye Sequence") .
  Map.insert 0x00460070 (TagDictEntry "SQ" "Keratometry Right Eye Sequence") .
  Map.insert 0x00460071 (TagDictEntry "SQ" "Keratometry Left Eye Sequence") .
  Map.insert 0x00460074 (TagDictEntry "SQ" "Steep Keratometric Axis Sequence") .
  Map.insert 0x00460080 (TagDictEntry "SQ" "Flat Keratometric Axis Sequence") .
  Map.insert 0x00460097 (TagDictEntry "SQ" "Subjective Refraction Right Eye Sequence") .
  Map.insert 0x00460098 (TagDictEntry "SQ" "Subjective Refraction Left Eye Sequence") .
  Map.insert 0x00460100 (TagDictEntry "SQ" "Add Near Sequence") .
  Map.insert 0x00460101 (TagDictEntry "SQ" "Add Intermediate Sequence") .
  Map.insert 0x00460102 (TagDictEntry "SQ" "Add Other Sequence") .
  Map.insert 0x00460121 (TagDictEntry "SQ" "Visual Acuity Type Code Sequence") .
  Map.insert 0x00460122 (TagDictEntry "SQ" "Visual Acuity Right Eye Sequence") .
  Map.insert 0x00460123 (TagDictEntry "SQ" "Visual Acuity Left Eye Sequence") .
  Map.insert 0x00460124 (TagDictEntry "SQ" "Visual Acuity Both Eyes Open Sequence") .
  Map.insert 0x00460145 (TagDictEntry "SQ" "Referenced Refractive Measurements Sequence") .
  Map.insert 0x00500010 (TagDictEntry "SQ" "Device Sequence") .
  Map.insert 0x00500012 (TagDictEntry "SQ" "Container Component Type Code Sequence") .
  Map.insert 0x00540012 (TagDictEntry "SQ" "Energy Window Information Sequence") .
  Map.insert 0x00540013 (TagDictEntry "SQ" "Energy Window Range Sequence") .
  Map.insert 0x00540014 (TagDictEntry "DS" "Energy Window Lower Limit") .
  Map.insert 0x00540015 (TagDictEntry "DS" "Energy Window Upper Limit") .
  Map.insert 0x00540016 (TagDictEntry "SQ" "Radiopharmaceutical Information Sequence") .
  Map.insert 0x00540022 (TagDictEntry "SQ" "Detector Information Sequence") .
  Map.insert 0x00540032 (TagDictEntry "SQ" "Phase Information Sequence") .
  Map.insert 0x00540052 (TagDictEntry "SQ" "Rotation Information Sequence") .
  Map.insert 0x00540062 (TagDictEntry "SQ" "Gated Information Sequence") .
  Map.insert 0x00540063 (TagDictEntry "SQ" "Data Information Sequence") .
  Map.insert 0x00540072 (TagDictEntry "SQ" "Time Slot Information Sequence") .
  Map.insert 0x00540081 (TagDictEntry "US" "Number of Slices") .
  Map.insert 0x00540220 (TagDictEntry "SQ" "View Code Sequence") .
  Map.insert 0x00540222 (TagDictEntry "SQ" "View Modifier Code Sequence") .
  Map.insert 0x00540300 (TagDictEntry "SQ" "Radionuclide Code Sequence") .
  Map.insert 0x00540302 (TagDictEntry "SQ" "Administration Route Code Sequence") .
  Map.insert 0x00540304 (TagDictEntry "SQ" "Radiopharmaceutical Code Sequence") .
  Map.insert 0x00540306 (TagDictEntry "SQ" "Calibration Data Sequence") .
  Map.insert 0x00540410 (TagDictEntry "SQ" "Patient Orientation Code Sequence") .
  Map.insert 0x00540412 (TagDictEntry "SQ" "Patient Orientation Modifier Code Sequence") .
  Map.insert 0x00540414 (TagDictEntry "SQ" "Patient Gantry Relationship Code Sequence") .
  Map.insert 0x00541000 (TagDictEntry "CS" "Series Type") .
  Map.insert 0x00541001 (TagDictEntry "CS" "Units") .
  Map.insert 0x00541002 (TagDictEntry "CS" "Counts Source") .
  Map.insert 0x00541100 (TagDictEntry "CS" "Randoms Correction Method") .
  Map.insert 0x00541101 (TagDictEntry "LO" "Attenuation Correction Method") .
  Map.insert 0x00541102 (TagDictEntry "CS" "Decay Correction") .
  Map.insert 0x00541103 (TagDictEntry "LO" "Reconstruction Method") .
  Map.insert 0x00541104 (TagDictEntry "LO" "Detector Lines of Response Used") .
  Map.insert 0x00541105 (TagDictEntry "LO" "Scatter Correction Method") .
  Map.insert 0x00541200 (TagDictEntry "DS" "Axial Acceptance") .
  Map.insert 0x00541201 (TagDictEntry "IA" "Axial Mash") .
  Map.insert 0x00541300 (TagDictEntry "DS" "Frame Reference Time") .
  Map.insert 0x00541321 (TagDictEntry "DS" "Decay Factor") .
  Map.insert 0x00541322 (TagDictEntry "DS" "Dose Calibration Factor") .
  Map.insert 0x00541330 (TagDictEntry "US" "Image Index") .
  Map.insert 0x00603000 (TagDictEntry "SQ" "Histogram Sequence") .
  Map.insert 0x00620002 (TagDictEntry "SQ" "Segment Sequence") .
  Map.insert 0x00620003 (TagDictEntry "SQ" "Segmented Property Category Code Sequence") .
  Map.insert 0x0062000a (TagDictEntry "SQ" "Segment Identification Sequence") .
  Map.insert 0x0062000f (TagDictEntry "SQ" "Pre Deformation Matrix Registration Sequence") .
  Map.insert 0x00640002 (TagDictEntry "SQ" "Deformable Registration Sequence") .
  Map.insert 0x00640005 (TagDictEntry "SQ" "Deformable Registration Grid Sequence") .
  Map.insert 0x0064000f (TagDictEntry "SQ" "Segment Identification Sequence") .
  Map.insert 0x00640010 (TagDictEntry "SQ" "Post Deformation Matrix Registration Sequence") .
  Map.insert 0x00660002 (TagDictEntry "SQ" "Surface Sequence") .
  Map.insert 0x00660011 (TagDictEntry "SQ" "Surface Points Sequence") .
  Map.insert 0x00660012 (TagDictEntry "SQ" "Surface Points Normals Sequence") .
  Map.insert 0x00660013 (TagDictEntry "SQ" "Surface Mesh Primitives Sequence") .
  Map.insert 0x00660026 (TagDictEntry "SQ" "Triangle Strip Sequence") .
  Map.insert 0x00660027 (TagDictEntry "SQ" "Triangle Fan Sequence") .
  Map.insert 0x00660028 (TagDictEntry "SQ" "Line Sequence") .
  Map.insert 0x0066002b (TagDictEntry "SQ" "Referenced Surface Sequence") .
  Map.insert 0x0066002d (TagDictEntry "SQ" "Segment Surface Generation Algorithm Identification Sequence") .
  Map.insert 0x0066002e (TagDictEntry "SQ" "Segment Surface Source Instance Sequence") .
  Map.insert 0x0066002f (TagDictEntry "SQ" "Algorithm Family Code Sequence") .
  Map.insert 0x00660030 (TagDictEntry "SQ" "Algorithm Name Code Sequence") .
  Map.insert 0x00660034 (TagDictEntry "SQ" "Facet Sequence") .
  Map.insert 0x00660035 (TagDictEntry "SQ" "Surface Processing Algorithm Identification Sequence") .
  Map.insert 0x00880140 (TagDictEntry "UI" "Storage Media File-set UID") .
  Map.insert 0x00880200 (TagDictEntry "UI" "Icon Image Sequence") .
  Map.insert 0x20200020 (TagDictEntry "CS" "Polarity") .
  Map.insert 0x20500020 (TagDictEntry "CS" "Presentation LUT Shape") .
  Map.insert 0x300e0002 (TagDictEntry "CS" "Approval Status") .
  Map.insert 0x52009229 (TagDictEntry "SQ" "Shared Functional Groups Sequence") .
  Map.insert 0x52009230 (TagDictEntry "SQ" "Per-frame Functional Groups Sequence") .
  Map.insert 0x60000010 (TagDictEntry "US" "Overlay Rows") .
  Map.insert 0x60000011 (TagDictEntry "US" "Overlay Columns") .
  Map.insert 0x60000040 (TagDictEntry "CS" "Overlay Type") .
  Map.insert 0x60000050 (TagDictEntry "SS" "Overlay Origin") .
  Map.insert 0x60000100 (TagDictEntry "US" "Overlay Bits Allocated") .
  Map.insert 0x60000102 (TagDictEntry "US" "Overlay Bit Position") .
  Map.insert 0x60003000 (TagDictEntry "OB" "Overlay Data") .
  Map.insert 0x7fe00010 (TagDictEntry "OB" "Pixel Data") .
  Map.insert 0xfffafffa (TagDictEntry "SQ" "Digital Signatures Sequence") 
  $ Map.empty

-- | UID Dictionary
getUidDictionary :: UidDictionary
getUidDictionary =
  Map.insert cT_IMAGE_STORAGE "CT Image Storage" .
  Map.insert eNHANCED_CT_IMAGE_STORAGE "Enhanced CT Image Storage" .
  Map.insert eNHANCED_MR_IMAGE_STORAGE "Enhanced MR Image Storage" .
  Map.insert eNHANCED_PET_IMAGE_STORAGE "Enhanced PET Image Storage" .
  Map.insert mR_IMAGE_STORAGE "MR Image Storage" .
  Map.insert pOSITRON_EMISSION_TOMOGRAPHY_IMAGE_STORAGE "PET Image Storage" .
  Map.insert uLTRASOUND_IMAGE_STORAGE "US Image Storage" .
  Map.insert uLTRASOUND_MULTIFRAME_IMAGE_STORAGE "US Multiframe Image Storage"
  $ Map.empty