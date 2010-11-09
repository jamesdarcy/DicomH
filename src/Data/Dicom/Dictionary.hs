
module Data.Dicom.Dictionary
  (
    DictEntry(..),
    TagDictionary,
    getTagDictionary
  ) where

import qualified Data.Map as Map
import Data.Word

import Data.Dicom.Tag

data DictEntry = DictEntry {
  vr :: String,
  desc :: String
  } deriving (Eq, Show)

type TagDictionary = Map.Map Word32 DictEntry

getTagDictionary :: TagDictionary
getTagDictionary =
  Map.insert 0x00020000 (DictEntry "UL" "File Meta Information Group Length") .
  Map.insert 0x00020001 (DictEntry "OB" "File Meta Information Version") .
  Map.insert 0x00020002 (DictEntry "UI" "Media Storage SOP Class UID") .
  Map.insert 0x00020003 (DictEntry "UI" "Media Storage SOP Instance UID") .
  Map.insert 0x00020010 (DictEntry "UI" "Transfer Syntax UID") .
  Map.insert 0x00020012 (DictEntry "UI" "Implentation Class UID") .
  Map.insert 0x00020013 (DictEntry "SH" "Implementation Version Name") .
  Map.insert 0x00020016 (DictEntry "AE" "Source Application Entity Title") .
  Map.insert 0x00020100 (DictEntry "UI" "Private Information Creator UID") .
  Map.insert 0x00020102 (DictEntry "OB" "Private Information") .
  Map.insert 0x00080005 (DictEntry "CS" "Specific Character Set") .
  Map.insert 0x00080006 (DictEntry "SQ" "Language Code Sequence") .
  Map.insert 0x00080008 (DictEntry "CS" "Image Type") .
  Map.insert 0x00080012 (DictEntry "DA" "Instance Creation Date") .
  Map.insert 0x00080013 (DictEntry "TM" "Instance Creation Time") .
  Map.insert 0x00080014 (DictEntry "UI" "Instance Creator UID") .
  Map.insert 0x00080016 (DictEntry "UI" "SOP Class UID") .
  Map.insert 0x00080018 (DictEntry "UI" "SOP Instance UID") .
  Map.insert 0x00080020 (DictEntry "DA" "Study Date") .
  Map.insert 0x00080021 (DictEntry "DA" "Series Date") .
  Map.insert 0x00080022 (DictEntry "DA" "Acquisition Date") .
  Map.insert 0x00080023 (DictEntry "DA" "Content Date") .
  Map.insert 0x0008002a (DictEntry "DT" "Acquisition DateTime") .
  Map.insert 0x00080030 (DictEntry "TM" "Study Time") .
  Map.insert 0x00080031 (DictEntry "TM" "Series Time") .
  Map.insert 0x00080032 (DictEntry "TM" "Acquisition Time") .
  Map.insert 0x00080033 (DictEntry "TM" "Content Time") .
  Map.insert 0x00080050 (DictEntry "SH" "Accession Number") .
  Map.insert 0x00080051 (DictEntry "SQ" "Issuer of Accession Number Sequence") .
  Map.insert 0x00080060 (DictEntry "CS" "Modality") .
  Map.insert 0x00080070 (DictEntry "LO" "Manufacturer") .
  Map.insert 0x00080080 (DictEntry "LO" "Institution Name") .
  Map.insert 0x00080081 (DictEntry "ST" "Institution Address") .
  Map.insert 0x00080082 (DictEntry "SQ" "Institution Code Sequence") .
  Map.insert 0x00080090 (DictEntry "PN" "Referring Physician Name") .
  Map.insert 0x00080096 (DictEntry "SQ" "Referring Physician Identification Sequence") .
  Map.insert 0x00080100 (DictEntry "SH" "Code Value") .
  Map.insert 0x00080102 (DictEntry "SH" "Coding Scheme Designator") .
  Map.insert 0x00080103 (DictEntry "SH" "Coding Scheme Version") .
  Map.insert 0x00080104 (DictEntry "LO" "Code Meaning") .
  Map.insert 0x00080105 (DictEntry "CS" "Mapping Resource") .
  Map.insert 0x00080106 (DictEntry "DT" "Context Group Version") .
  Map.insert 0x0008010b (DictEntry "CS" "Context Group Extension Flag") .
  Map.insert 0x0008010d (DictEntry "UI" "Context Group Extension Creator UID") .
  Map.insert 0x0008010f (DictEntry "CS" "Context Identifier") .
  Map.insert 0x00080110 (DictEntry "SQ" "Coding Scheme Identification Sequence") .
  Map.insert 0x00081010 (DictEntry "SH" "Station Name") .
  Map.insert 0x00081030 (DictEntry "LO" "Study Description") .
  Map.insert 0x00081032 (DictEntry "SQ" "Procedure Code Sequence") .
  Map.insert 0x0008103e (DictEntry "LO" "Series Description") .
  Map.insert 0x0008103f (DictEntry "SQ" "Series Description Code Sequence") .
  Map.insert 0x00081040 (DictEntry "LO" "Institutional Department Name") .
  Map.insert 0x00081048 (DictEntry "PN" "Physician(s) of Record") .
  Map.insert 0x00081049 (DictEntry "SQ" "Physician(s) of Record Identification Sequence") .
  Map.insert 0x00081050 (DictEntry "PN" "Performing Physician Name") .
  Map.insert 0x00081052 (DictEntry "SQ" "Performing Physician Identification Sequence") .
  Map.insert 0x00081062 (DictEntry "SQ" "Physician(s) Reading Study Identification Sequence") .
  Map.insert 0x00081070 (DictEntry "PN" "Operator Name") .
  Map.insert 0x00081072 (DictEntry "SQ" "Operator Identification Sequence") .
  Map.insert 0x00081080 (DictEntry "LO" "Admitting Diagnoses Description") .
  Map.insert 0x00081084 (DictEntry "SQ" "Admitting Diagnoses Code Sequence") .
  Map.insert 0x00081090 (DictEntry "LO" "Manufacturer Model Name") .
  Map.insert 0x00081110 (DictEntry "SQ" "Referenced Study Sequence") .
  Map.insert 0x00081111 (DictEntry "SQ" "Referenced Performed Procedure Step Sequence") .
  Map.insert 0x00081115 (DictEntry "SQ" "Referenced Series Sequence") .
  Map.insert 0x00081120 (DictEntry "SQ" "Referenced Patient Sequence") .
  Map.insert 0x00081125 (DictEntry "SQ" "Referenced Visit Sequence") .
  Map.insert 0x00081134 (DictEntry "SQ" "Referenced Stereometric Instance Sequence") .
  Map.insert 0x0008113a (DictEntry "SQ" "Referenced Waveform Sequence") .
  Map.insert 0x00081140 (DictEntry "SQ" "Referenced Image Sequence") .
  Map.insert 0x0008114a (DictEntry "SQ" "Referenced Instance Sequence") .
  Map.insert 0x0008114b (DictEntry "SQ" "Referenced Real World Value Mapping Instance Sequence") .
  Map.insert 0x00081150 (DictEntry "UI" "Referenced SOP Class UID") .
  Map.insert 0x00081155 (DictEntry "UI" "Referenced SOP Instance UID") .
  Map.insert 0x00081160 (DictEntry "IS" "Referenced SOP Class UID") .
  Map.insert 0x00081164 (DictEntry "SQ" "Frame Extraction Sequence") .
  Map.insert 0x00081198 (DictEntry "SQ" "Failed SOP Sequence") .
  Map.insert 0x00081199 (DictEntry "SQ" "Referenced SOP Sequence") .
  Map.insert 0x00081200 (DictEntry "SQ" "Studies Containing Other Referenced Instances Sequence") .
  Map.insert 0x00081250 (DictEntry "SQ" "Related Series Sequence") .
  Map.insert 0x00082111 (DictEntry "ST" "Derivation Description") .
  Map.insert 0x00082112 (DictEntry "SQ" "Source Image Sequence") .
  Map.insert 0x00082133 (DictEntry "SQ" "Event Timer Sequence") .
  Map.insert 0x00082135 (DictEntry "SQ" "Event Code Sequence") .
  Map.insert 0x00082218 (DictEntry "SQ" "Anatomic Region Sequence") .
  Map.insert 0x00082220 (DictEntry "SQ" "Anatomic Region Modifier Sequence") .
  Map.insert 0x00082228 (DictEntry "SQ" "Primary Anatomic Structure Sequence") .
  Map.insert 0x00082229 (DictEntry "SQ" "Anatomic Structure, Space or Region Sequence") .
  Map.insert 0x00082230 (DictEntry "SQ" "Primary Anatomic Structure Modifier Sequence") .
  Map.insert 0x00083001 (DictEntry "SQ" "Alternate Representation Sequence") .
  Map.insert 0x00089007 (DictEntry "CS" "Frame Type") .
  Map.insert 0x00089092 (DictEntry "SQ" "Referenced Image Evidence Sequence") .
  Map.insert 0x00089121 (DictEntry "SQ" "Referenced Raw Data Sequence") .
  Map.insert 0x00089123 (DictEntry "UI" "Creator-Version UID") .
  Map.insert 0x00089124 (DictEntry "SQ" "Derivation Image Sequence") .
  Map.insert 0x00089154 (DictEntry "SQ" "Source Image Evidence Sequence") .
  Map.insert 0x00089205 (DictEntry "CS" "Pixel Presentation") .
  Map.insert 0x00089206 (DictEntry "CS" "Volumetric Properties") .
  Map.insert 0x00089207 (DictEntry "CS" "Volume Based Calculation Technique") .
  Map.insert 0x00089208 (DictEntry "CS" "Complex Image Component") .
  Map.insert 0x00089209 (DictEntry "CS" "Acquisition Contrast") .
  Map.insert 0x00089215 (DictEntry "SQ" "Derivation Code Sequence") .
  Map.insert 0x00089237 (DictEntry "SQ" "Referenced Presentation State Sequence") .
  Map.insert 0x00089410 (DictEntry "SQ" "Referenced Other Plane Sequence") .
  Map.insert 0x00089458 (DictEntry "SQ" "Frame Display Sequence") .
  Map.insert 0x00100010 (DictEntry "PN" "Patient Name") .
  Map.insert 0x00100020 (DictEntry "DS" "Patient's Size") .
  Map.insert 0x00100024 (DictEntry "SQ" "Issuer of Patient ID Qualifiers Sequence") .
  Map.insert 0x00100030 (DictEntry "DA" "Patient Birth Date") .
  Map.insert 0x00100032 (DictEntry "TM" "Patient Birth Time") .
  Map.insert 0x00100040 (DictEntry "CS" "Patient Sex") .
  Map.insert 0x00100050 (DictEntry "SQ" "Patient's Insurance Plan Code Sequence") .
  Map.insert 0x00100101 (DictEntry "SQ" "Patient's Primary Language Code Sequence") .
  Map.insert 0x00100102 (DictEntry "SQ" "Patient's Primary Language Modifier Code Sequence") .
  Map.insert 0x00101000 (DictEntry "LO" "Other Patient IDs") .
  Map.insert 0x00101002 (DictEntry "SQ" "Other Patient IDs Sequence") .
  Map.insert 0x00101010 (DictEntry "AS" "Patient Age") .
  Map.insert 0x00101020 (DictEntry "AS" "Patient Age") .
  Map.insert 0x00101030 (DictEntry "DS" "Patient Weight") .
  Map.insert 0x00102152 (DictEntry "LO" "Region of Residence") .
  Map.insert 0x001021c0 (DictEntry "US" "Pregnancy Status") .
  Map.insert 0x00102202 (DictEntry "SQ" "Patient Species Code Sequence") .
  Map.insert 0x00102293 (DictEntry "SQ" "Patient Breed Code Sequence") .
  Map.insert 0x00102294 (DictEntry "SQ" "Breed Registration Sequence") .
  Map.insert 0x00102296 (DictEntry "SQ" "Breed Registry Code Sequence") .
  Map.insert 0x00104000 (DictEntry "LT" "Patient Comments") .
  Map.insert 0x00120064 (DictEntry "SQ" "De-identification Method Code Sequence") .
  Map.insert 0x00120083 (DictEntry "SQ" "Consent for Clinical Trial Use Sequence") .
  Map.insert 0x00180010 (DictEntry "LO" "Contrast/Bolus Agent") .
  Map.insert 0x00180012 (DictEntry "SQ" "Contrast/Bolus Agent Sequence") .
  Map.insert 0x00180014 (DictEntry "SQ" "Contrast/Bolus Administration Route Sequence") .
  Map.insert 0x00180015 (DictEntry "CS" "Body Part Examined") .
  Map.insert 0x00180020 (DictEntry "CS" "Scanning Sequence") .
  Map.insert 0x00180021 (DictEntry "CS" "Sequence Variant") .
  Map.insert 0x00180022 (DictEntry "CS" "Scan Options") .
  Map.insert 0x00180023 (DictEntry "CS" "MR Acquisition Type") .
  Map.insert 0x00180024 (DictEntry "SH" "Sequence Name") .
  Map.insert 0x00180025 (DictEntry "CS" "Angio Flag") .
  Map.insert 0x00180026 (DictEntry "SQ" "Intervention Drug Information Sequence") .
  Map.insert 0x00180029 (DictEntry "SQ" "Intervention Drug Code Sequence") .
  Map.insert 0x0018002a (DictEntry "SQ" "Additional Drug Sequence") .
  Map.insert 0x00180036 (DictEntry "SQ" "Intervention Sequence") .
  Map.insert 0x00180050 (DictEntry "DS" "Slice Thickness") .
  Map.insert 0x00180060 (DictEntry "DS" "KVP") .
  Map.insert 0x00180080 (DictEntry "DS" "Repetition Time") .
  Map.insert 0x00180081 (DictEntry "DS" "Echo Time") .
  Map.insert 0x00180082 (DictEntry "DS" "Inversion Time") .
  Map.insert 0x00180083 (DictEntry "DS" "Number of Averages") .
  Map.insert 0x00180084 (DictEntry "DS" "Imaging Frequency") .
  Map.insert 0x00180085 (DictEntry "SH" "Imaged Nucleus") .
  Map.insert 0x00180086 (DictEntry "IS" "Echo Numbers") .
  Map.insert 0x00180087 (DictEntry "DS" "Magnetic Field Strength") .
  Map.insert 0x00180088 (DictEntry "DS" "Spacing Between Slices") .
  Map.insert 0x00180089 (DictEntry "IS" "Number of Phase Encoding Steps") .
  Map.insert 0x00180090 (DictEntry "DS" "Data Collection Diameter") .
  Map.insert 0x00180091 (DictEntry "IS" "Echo Train Length") .
  Map.insert 0x00180093 (DictEntry "DS" "Percent Sampling") .
  Map.insert 0x00180094 (DictEntry "DS" "Percent Phase Field of View") .
  Map.insert 0x00180095 (DictEntry "DS" "Pixel Bandwidth") .
  Map.insert 0x00181000 (DictEntry "LO" "Device Serial Number") .
  Map.insert 0x00181004 (DictEntry "LO" "Plate ID") .
  Map.insert 0x00181020 (DictEntry "LO" "Software Version") .
  Map.insert 0x00181030 (DictEntry "LO" "Protocol Name") .
  Map.insert 0x00181041 (DictEntry "DS" "Contrast/Bolus Volume") .
  Map.insert 0x00181042 (DictEntry "TM" "Contrast/Bolus Start Time") .
  Map.insert 0x00181044 (DictEntry "DS" "Contrast/Bolus Total Dose") .
  Map.insert 0x00181046 (DictEntry "DS" "Contrast Flow Rate") .
  Map.insert 0x00181047 (DictEntry "DS" "Contrast Flow Duration") .
  Map.insert 0x00181049 (DictEntry "DS" "Contrast/Bolus Ingredient Concentration") .
  Map.insert 0x00181072 (DictEntry "TM" "Radiopharmaceutical Start Time") .
  Map.insert 0x00181074 (DictEntry "DS" "Radionuclide Total Dose") .
  Map.insert 0x00181075 (DictEntry "DS" "Radionuclide Half Life") .
  Map.insert 0x00181076 (DictEntry "DS" "Radionuclide Positron Fraction") .
  Map.insert 0x00181081 (DictEntry "IS" "Low R-R Value") .
  Map.insert 0x00181082 (DictEntry "IS" "High R-R Value") .
  Map.insert 0x00181083 (DictEntry "IS" "Intervals Acquired") .
  Map.insert 0x00181084 (DictEntry "IS" "Intervals Rejected") .
  Map.insert 0x00181088 (DictEntry "IS" "Heart Rate") .
  Map.insert 0x00181100 (DictEntry "DS" "Reconstruction Diameter") .
  Map.insert 0x00181110 (DictEntry "DS" "Distance Source to Detector") .
  Map.insert 0x00181111 (DictEntry "DS" "Distance Source to Patient") .
  Map.insert 0x00181120 (DictEntry "DS" "Detector/Gantry Tilt") .
  Map.insert 0x00181130 (DictEntry "DS" "Table Height") .
  Map.insert 0x00181140 (DictEntry "CS" "Rotation Direction") .
  Map.insert 0x00181150 (DictEntry "IS" "Exposure Time") .
  Map.insert 0x00181151 (DictEntry "IS" "X-Ray Tube Current") .
  Map.insert 0x00181152 (DictEntry "IS" "Exposure") .
  Map.insert 0x00181155 (DictEntry "CS" "Radiation Setting") .
  Map.insert 0x00181160 (DictEntry "SH" "Filter Type") .
  Map.insert 0x00181162 (DictEntry "DS" "Intensifier Size") .
  Map.insert 0x00181164 (DictEntry "DS" "Imager Pixel Spacing") .
  Map.insert 0x00181170 (DictEntry "SH" "Generator Power") .
  Map.insert 0x00181181 (DictEntry "CS" "Collimator Type") .
  Map.insert 0x00181190 (DictEntry "DS" "Focal Spots") .
  Map.insert 0x00181200 (DictEntry "DA" "Date of Last Calibration") .
  Map.insert 0x00181201 (DictEntry "TM" "Time of Last Calibration") .
  Map.insert 0x00181210 (DictEntry "SH" "Convolution Kernel") .
  Map.insert 0x00181242 (DictEntry "IS" "Actual Frame Duration") .
  Map.insert 0x00181250 (DictEntry "SH" "Receive Coil Name") .
  Map.insert 0x00181251 (DictEntry "SH" "Transmit Coil Name") .
  Map.insert 0x00181260 (DictEntry "SH" "Plate Type") .
  Map.insert 0x00181310 (DictEntry "US" "Acquisition Matrix") .
  Map.insert 0x00181312 (DictEntry "CS" "In-plane Phase Encoding Direction") .
  Map.insert 0x00181314 (DictEntry "DS" "Flip Angle") .
  Map.insert 0x00181315 (DictEntry "CS" "Variable Flip Angle Flag") .
  Map.insert 0x00181316 (DictEntry "DS" "SAR") .
  Map.insert 0x00181318 (DictEntry "DS" "dB/dt") .
  Map.insert 0x00181400 (DictEntry "LO" "Acquisition Device Processing Description") .
  Map.insert 0x00181401 (DictEntry "LO" "Acquisition Device Processing Code") .
  Map.insert 0x00181402 (DictEntry "CS" "Cassette Orientation") .
  Map.insert 0x00181403 (DictEntry "CS" "Cassette Size") .
  Map.insert 0x00181404 (DictEntry "US" "Exposures on Plate") .
  Map.insert 0x00181510 (DictEntry "DS" "Positioner Primary Angle") .
  Map.insert 0x00181511 (DictEntry "DS" "Positioner Secondary Angle") .
  Map.insert 0x00185100 (DictEntry "CS" "Patient Position") .
  Map.insert 0x00185101 (DictEntry "CS" "View Position") .
  Map.insert 0x00185104 (DictEntry "SQ" "Projection Eponymous Name Code Sequence") .
  Map.insert 0x00186000 (DictEntry "DS" "Sensitivity") .
  Map.insert 0x00186011 (DictEntry "SQ" "Sequence of Ultrasound Regions") .
  Map.insert 0x00186012 (DictEntry "US" "Region Spatial Format") .
  Map.insert 0x00186014 (DictEntry "US" "Region Data Type") .
  Map.insert 0x00186016 (DictEntry "UL" "Region Flags") .
  Map.insert 0x00186018 (DictEntry "UL" "Region Location Min X0") .
  Map.insert 0x0018601a (DictEntry "UL" "Region Location Min Y0") .
  Map.insert 0x0018601c (DictEntry "UL" "Region Location Max X1") .
  Map.insert 0x0018601e (DictEntry "UL" "Region Location Max Y1") .
  Map.insert 0x00186020 (DictEntry "SL" "Reference Pixel X0") .
  Map.insert 0x00186022 (DictEntry "SL" "Reference Pixel Y0") .
  Map.insert 0x00186024 (DictEntry "US" "Physical Units X Direction") .
  Map.insert 0x00186026 (DictEntry "US" "Physical Units Y Direction") .
  Map.insert 0x00186028 (DictEntry "FD" "Reference Pixel Physical Value X") .
  Map.insert 0x0018602a (DictEntry "FD" "Reference Pixel Physical Value Y") .
  Map.insert 0x0018602c (DictEntry "FD" "Physical Delta X") .
  Map.insert 0x0018602e (DictEntry "FD" "Physical Delta Y") .
  Map.insert 0x00186030 (DictEntry "UL" "Transducer Frequency") .
  Map.insert 0x00189004 (DictEntry "CS" "Content Qualification") .
  Map.insert 0x00189005 (DictEntry "SH" "Pulse Sequence Name") .
  Map.insert 0x00189006 (DictEntry "SQ" "MR Imaging Modifier Sequence") .
  Map.insert 0x00189008 (DictEntry "CS" "Echo Pulse Sequence") .
  Map.insert 0x00189009 (DictEntry "CS" "Inversion Recovery") .
  Map.insert 0x00189010 (DictEntry "CS" "Flow Compensation") .
  Map.insert 0x00189011 (DictEntry "CS" "Multiple Spin Echo") .
  Map.insert 0x00189012 (DictEntry "CS" "Multi-planar Excitation") .
  Map.insert 0x00189014 (DictEntry "CS" "Phase Contrast") .
  Map.insert 0x00189015 (DictEntry "CS" "Time of Flight Contrast") .
  Map.insert 0x00189016 (DictEntry "CS" "Spoiling") .
  Map.insert 0x00189017 (DictEntry "CS" "Steady State Pulse Sequence") .
  Map.insert 0x00189018 (DictEntry "CS" "Echo Planar Pulse Sequence") .
  Map.insert 0x00189020 (DictEntry "CS" "Magnetization Transfer") .
  Map.insert 0x00189021 (DictEntry "CS" "T2 Preparation") .
  Map.insert 0x00189022 (DictEntry "CS" "Blood Signal Nulling") .
  Map.insert 0x00189024 (DictEntry "CS" "Saturation Recovery") .
  Map.insert 0x00189025 (DictEntry "CS" "Spectrally Selected Suppression") .
  Map.insert 0x00189026 (DictEntry "CS" "Spectrally Selected Excitation") .
  Map.insert 0x00189027 (DictEntry "CS" "Spatial Pre-saturation") .
  Map.insert 0x00189028 (DictEntry "CS" "Tagging") .
  Map.insert 0x00189029 (DictEntry "CS" "Oversampling Phase") .
  Map.insert 0x00189032 (DictEntry "CS" "Geometry of k-Space Traversal") .
  Map.insert 0x00189033 (DictEntry "CS" "Segmented k-Space Traversal") .
  Map.insert 0x00189034 (DictEntry "CS" "Rectilinear Phase Encode Reordering") .
  Map.insert 0x00189035 (DictEntry "FD" "Tag Thickness") .
  Map.insert 0x00189036 (DictEntry "CS" "Partial Fourier Direction") .
  Map.insert 0x00189037 (DictEntry "CS" "Cardiac Synchronization Technique") .
  Map.insert 0x00189041 (DictEntry "LO" "Receive Coil Manufacturer Name") .
  Map.insert 0x00189042 (DictEntry "SQ" "MR Receive Coil Sequence") .
  Map.insert 0x00189043 (DictEntry "CS" "Receive Coil Type") .
  Map.insert 0x00189044 (DictEntry "CS" "Quadrature Receive Coil") .
  Map.insert 0x00189045 (DictEntry "SQ" "Multi-Coil Definition Sequence") .
  Map.insert 0x00189046 (DictEntry "LO" "Multi-Coil Configuration") .
  Map.insert 0x00189047 (DictEntry "SH" "Multi-Coil Element Name") .
  Map.insert 0x00189048 (DictEntry "CS" "Multi-Coil Element Used") .
  Map.insert 0x00189049 (DictEntry "SQ" "MR Transmit Coil Sequence") .
  Map.insert 0x00189050 (DictEntry "LO" "Transmit Coil Manufacturer Name") .
  Map.insert 0x00189051 (DictEntry "CS" "Transmit Coil Type") .
  Map.insert 0x00189053 (DictEntry "FD" "Chemical Shift Reference") .
  Map.insert 0x00189058 (DictEntry "US" "MR Acquisition Frequency Encoding Steps") .
  Map.insert 0x00189059 (DictEntry "CS" "De-coupling") .
  Map.insert 0x00189064 (DictEntry "CS" "k-space Filtering") .
  Map.insert 0x00189069 (DictEntry "FD" "Parallel Reduction Factor In-plane") .
  Map.insert 0x00189073 (DictEntry "FD" "Acquisition Duration") .
  Map.insert 0x00189074 (DictEntry "DT" "Frame Acquisition DateTime") .
  Map.insert 0x00189076 (DictEntry "SQ" "Diffusion Gradient Direction Sequence") .
  Map.insert 0x00189077 (DictEntry "CS" "Parallel Acquisition") .
  Map.insert 0x00189080 (DictEntry "ST" "Metabolite Map Description") .
  Map.insert 0x00189081 (DictEntry "CS" "Partial Fourier") .
  Map.insert 0x00189082 (DictEntry "FD" "Effective Echo Time") .
  Map.insert 0x00189083 (DictEntry "SQ" "Metabolite Map Code Sequence") .
  Map.insert 0x00189084 (DictEntry "SQ" "Chemical Shift Sequence") .
  Map.insert 0x00189090 (DictEntry "FD" "Velocity Encoding Direction") .
  Map.insert 0x00189091 (DictEntry "FD" "Velocity Encoding Minimum Value") .
  Map.insert 0x00189093 (DictEntry "US" "Number of k-Space Trajectories") .
  Map.insert 0x00189094 (DictEntry "CS" "Coverage of k-Space") .
  Map.insert 0x00189098 (DictEntry "FD" "Transmitter Frequency") .
  Map.insert 0x00189100 (DictEntry "CS" "Resonant Nucleus") .
  Map.insert 0x00189101 (DictEntry "CS" "Frequency Correction") .
  Map.insert 0x00189103 (DictEntry "SQ" "MR Spectroscopy FOV/Geometry Sequence") .
  Map.insert 0x00189107 (DictEntry "SQ" "MR Spatial Saturation Sequence") .
  Map.insert 0x00189112 (DictEntry "SQ" "MR Timing and Related Parameters Sequence") .
  Map.insert 0x00189114 (DictEntry "SQ" "MR Echo Sequence") .
  Map.insert 0x00189115 (DictEntry "SQ" "MR Modifier Sequence") .
  Map.insert 0x00189117 (DictEntry "SQ" "MR Diffusion Sequence") .
  Map.insert 0x00189118 (DictEntry "SQ" "Cardiac Synchronization Sequence") .
  Map.insert 0x00189119 (DictEntry "SQ" "MR Averages Sequence") .
  Map.insert 0x00189125 (DictEntry "SQ" "MR FOV/Geometry Sequence") .
  Map.insert 0x00189126 (DictEntry "SQ" "Volume Localization Sequence") .
  Map.insert 0x00189151 (DictEntry "DT" "Frame Reference DateTime") .
  Map.insert 0x00189152 (DictEntry "SQ" "MR Metabolite Map Sequence") .
  Map.insert 0x00189155 (DictEntry "FD" "Parallel Reduction Factor out-of-plane") .
  Map.insert 0x00189168 (DictEntry "FD" "Parallel Reduction Factor Second Inplane") .
  Map.insert 0x00189170 (DictEntry "CS" "Respiratory Motion Compensation Technique") .
  Map.insert 0x00189171 (DictEntry "CS" "Respiratory Signal Source") .
  Map.insert 0x00189172 (DictEntry "CS" "Bulk Motion Compensation Technique") .
  Map.insert 0x00189174 (DictEntry "CS" "Applicable Safety Standard Agency") .
  Map.insert 0x00189175 (DictEntry "CS" "Applicable Safety Standard Description") .
  Map.insert 0x00189176 (DictEntry "SQ" "Operating Mode Sequence") .
  Map.insert 0x00189177 (DictEntry "CS" "Operating Mode Type") .
  Map.insert 0x00189178 (DictEntry "CS" "Operating Mode") .
  Map.insert 0x00189179 (DictEntry "CS" "Specific Absorption Rate Definition") .
  Map.insert 0x00189180 (DictEntry "CS" "Gradient Output Type") .
  Map.insert 0x00189181 (DictEntry "FD" "Specific Absorption Rate Value") .
  Map.insert 0x00189182 (DictEntry "FD" "Gradient Output") .
  Map.insert 0x00189197 (DictEntry "SQ" "MR Velocity Encoding Sequence") .
  Map.insert 0x00189199 (DictEntry "CS" "Water Referenced Phase Correction") .
  Map.insert 0x00189200 (DictEntry "CS" "MR Spectroscopy Acquisition Type") .
  Map.insert 0x00189226 (DictEntry "SQ" "MR Image Frame Type Sequence") .
  Map.insert 0x00189227 (DictEntry "SQ" "MR Spectroscopy Frame Type Sequence") .
  Map.insert 0x00189231 (DictEntry "US" "MR Acquisition Phase Encoding Steps in-plane") .
  Map.insert 0x00189232 (DictEntry "US" "MR Acquisition Phase Encoding Steps out-of-plane") .
  Map.insert 0x00189239 (DictEntry "SQ" "Specific Absorption Rate Sequence") .
  Map.insert 0x00189240 (DictEntry "US" "RF Echo Train Length") .
  Map.insert 0x00189241 (DictEntry "US" "Gradient Echo Train Length") .
  Map.insert 0x00189301 (DictEntry "SQ" "CT Acquisition Type Sequence") .
  Map.insert 0x00189304 (DictEntry "SQ" "CT Acquisition Details Sequence") .
  Map.insert 0x00189308 (DictEntry "SQ" "CT Table Dynamics Sequence") .
  Map.insert 0x00189312 (DictEntry "SQ" "CT Geometry Sequence") .
  Map.insert 0x00189314 (DictEntry "SQ" "CT Reconstruction Sequence") .
  Map.insert 0x00189321 (DictEntry "SQ" "CT Exposure Sequence") .
  Map.insert 0x00189325 (DictEntry "SQ" "CT X-Ray Details Sequence") .
  Map.insert 0x00189326 (DictEntry "SQ" "CT Position Sequence") .
  Map.insert 0x00189329 (DictEntry "SQ" "CT Image Frame Type Sequence") .
  Map.insert 0x00189338 (DictEntry "SQ" "Contrast/Bolus Ingredient Code Sequence") .
  Map.insert 0x00189340 (DictEntry "SQ" "Contrast Administration Profile Sequence") .
  Map.insert 0x00189341 (DictEntry "SQ" "Contrast/Bolus Usage Sequence") .
  Map.insert 0x00189345 (DictEntry "FD" "CTDIvol") .
  Map.insert 0x00189346 (DictEntry "SQ" "CTDI Phantom Type Code Sequence") .
  Map.insert 0x00189360 (DictEntry "SQ" "CT Additional X-Ray Source Sequence") .
  Map.insert 0x00189401 (DictEntry "SQ" "Projection Pixel Calibration Sequence") .
  Map.insert 0x00189405 (DictEntry "SQ" "Positioner Position Sequence") .
  Map.insert 0x00189406 (DictEntry "SQ" "Table Position Sequence") .
  Map.insert 0x00189407 (DictEntry "SQ" "Collimator Shape Sequence") .
  Map.insert 0x00189412 (DictEntry "SQ" "XA/XRF Frame Characteristics Sequence") .
  Map.insert 0x00189417 (DictEntry "SQ" "Frame Acquisition Sequence") .
  Map.insert 0x00189432 (DictEntry "SQ" "Field of View Sequence") .
  Map.insert 0x00189434 (DictEntry "SQ" "Exposure Control Sensing Regions Sequence") .
  Map.insert 0x00189451 (DictEntry "SQ" "Frame Detector Parameters Sequence") .
  Map.insert 0x00189455 (DictEntry "SQ" "Calibration Sequence") .
  Map.insert 0x00189456 (DictEntry "SQ" "Object Thickness Sequence") .
  Map.insert 0x00189462 (DictEntry "SQ" "Isocenter Reference System Sequence") .
  Map.insert 0x00189472 (DictEntry "SQ" "Frame Display Shutter Sequence") .
  Map.insert 0x00189476 (DictEntry "SQ" "X-Ray Geometry Sequence") .
  Map.insert 0x00189477 (DictEntry "SQ" "Irradiation Event Identification Sequence") .
  Map.insert 0x00189504 (DictEntry "SQ" "X-Ray 3D Frame Type Sequence") .
  Map.insert 0x00189506 (DictEntry "SQ" "Contributing Sources Sequence") .
  Map.insert 0x00189507 (DictEntry "SQ" "X-Ray 3D Acquisition Sequence") .
  Map.insert 0x00189530 (DictEntry "SQ" "X-Ray 3D Reconstruction Sequence") .
  Map.insert 0x00189538 (DictEntry "SQ" "Per Projection Acquisition Sequence") .
  Map.insert 0x00189601 (DictEntry "SQ" "Diffusion b-matrix Sequence") .
  Map.insert 0x00189732 (DictEntry "SQ" "PET Frame Acquisition Sequence") .
  Map.insert 0x00189733 (DictEntry "SQ" "PET Detector Motion Details Sequence") .
  Map.insert 0x00189734 (DictEntry "SQ" "PET Table Dynamics Sequence") .
  Map.insert 0x00189735 (DictEntry "SQ" "PET Position Sequence") .
  Map.insert 0x00189736 (DictEntry "SQ" "PET Frame Correction Factors Sequence") .
  Map.insert 0x00189737 (DictEntry "SQ" "Radiopharmaceutical Usage Sequence") .
  Map.insert 0x00189749 (DictEntry "SQ" "PET Reconstruction Sequence") .
  Map.insert 0x00189751 (DictEntry "SQ" "PET Frame Type Sequence") .
  Map.insert 0x00189771 (DictEntry "SQ" "Patient Physiological State Sequence") .
  Map.insert 0x00189772 (DictEntry "SQ" "Patient Physiological State Code Sequence") .
  Map.insert 0x00189803 (DictEntry "SQ" "Excluded Intervals Sequence") .
  Map.insert 0x00189806 (DictEntry "SQ" "US Image Description Sequence") .
  Map.insert 0x00189807 (DictEntry "SQ" "Image Data Type Sequence") .
  Map.insert 0x00189809 (DictEntry "SQ" "Transducer Scan Pattern Code Sequence") .
  Map.insert 0x0018980d (DictEntry "SQ" "Transducer Geometry Code Sequence") .
  Map.insert 0x0018980e (DictEntry "SQ" "Transducer Beam Steering Code Sequence") .
  Map.insert 0x0018980f (DictEntry "SQ" "Transducer Application Code Sequence") .
  Map.insert 0x0018a001 (DictEntry "SQ" "Contributing Equipment Sequence") .
  Map.insert 0x0020000d (DictEntry "UI" "Study Instance UID") .
  Map.insert 0x0020000e (DictEntry "UI" "Series Instance UID") .
  Map.insert 0x00200010 (DictEntry "SH" "Study ID") .
  Map.insert 0x00200011 (DictEntry "IS" "Series Number") .
  Map.insert 0x00200012 (DictEntry "IS" "Acquisition Number") .
  Map.insert 0x00200013 (DictEntry "IS" "Instance Number") .
  Map.insert 0x00200020 (DictEntry "CS" "Patient Orientation") .
  Map.insert 0x00200032 (DictEntry "DS" "Image Position (Patient)") .
  Map.insert 0x00200037 (DictEntry "DS" "Image Orientation (Patient)") .
  Map.insert 0x00200052 (DictEntry "UI" "Frame of Reference UID") .
  Map.insert 0x00200100 (DictEntry "IS" "Temporal Position Identifier") .
  Map.insert 0x00200105 (DictEntry "IS" "Number of Temporal Positions") .
  Map.insert 0x00201002 (DictEntry "IS" "Images in Acquisition") .
  Map.insert 0x00201040 (DictEntry "LO" "Patient Reference Indicator") .
  Map.insert 0x00201041 (DictEntry "DS" "Slice Position") .
  Map.insert 0x00204000 (DictEntry "LT" "Image Comments") .
  Map.insert 0x00209056 (DictEntry "SH" "Stack ID") .
  Map.insert 0x00209057 (DictEntry "UL" "In-Stack Position Number") .
  Map.insert 0x00209071 (DictEntry "SQ" "Frame Anatomy Sequence") .
  Map.insert 0x00209072 (DictEntry "CS" "Frame Laterality") .
  Map.insert 0x00209111 (DictEntry "SQ" "Frame Content Sequence") .
  Map.insert 0x00209113 (DictEntry "SQ" "Plane Position Sequence") .
  Map.insert 0x00209116 (DictEntry "SQ" "Plane Orientation Sequence") .
  Map.insert 0x00209128 (DictEntry "UL" "TemporalPositionIndex") .
  Map.insert 0x00209157 (DictEntry "UL" "Dimension Index Values") .
  Map.insert 0x00209164 (DictEntry "AT" "Dimension Organization UID") .
  Map.insert 0x00209165 (DictEntry "AT" "Dimension Index Pointer") .
  Map.insert 0x00209167 (DictEntry "UI" "Functional Group Pointer") .
  Map.insert 0x00209213 (DictEntry "LO" "Dimension Index Private Creator") .
  Map.insert 0x00209221 (DictEntry "SQ" "Dimension Organization Sequence") .
  Map.insert 0x00209222 (DictEntry "SQ" "Dimension Index Sequence") .
  Map.insert 0x00209238 (DictEntry "LO" "Functional Group Private Creator") .
  Map.insert 0x00209253 (DictEntry "SQ" "Respiratory Synchronization Sequence") .
  Map.insert 0x00209254 (DictEntry "FD" "Respiratory Interval Time") .
  Map.insert 0x00209255 (DictEntry "FD" "Nominal Respiratory Trigger Delay Time") .
  Map.insert 0x0020930e (DictEntry "SQ" "Plane Position (Volume) Sequence") .
  Map.insert 0x0020930f (DictEntry "SQ" "Plane Orientation (Volume) Sequence") .
  Map.insert 0x00209310 (DictEntry "SQ" "Temporal Position Sequence") .
  Map.insert 0x00209421 (DictEntry "LO" "Dimension Description Label") .
  Map.insert 0x00209450 (DictEntry "SQ" "Patient Orientation in Frame Sequence") .
  Map.insert 0x00209529 (DictEntry "SQ" "Contributing SOP Instances Reference Sequence") .
  Map.insert 0x00220015 (DictEntry "SQ" "Acquisition Device Type Code Sequence") .
  Map.insert 0x00220016 (DictEntry "SQ" "Illumination Type Code Sequence") .
  Map.insert 0x00220017 (DictEntry "SQ" "Light Path Filter Type Stack Code Sequence") .
  Map.insert 0x00220018 (DictEntry "SQ" "Image Path Filter Type Stack Code Sequence") .
  Map.insert 0x00220019 (DictEntry "SQ" "Lenses Code Sequence") .
  Map.insert 0x0022001a (DictEntry "SQ" "Channel Description Code Sequence") .
  Map.insert 0x0022001b (DictEntry "SQ" "Refractive State Sequence") .
  Map.insert 0x0022001c (DictEntry "SQ" "Mydriatic Agent Code Sequence") .
  Map.insert 0x0022001d (DictEntry "SQ" "Relative Image Position Code Sequence") .
  Map.insert 0x00220020 (DictEntry "SQ" "Stereo Pairs Sequence") .
  Map.insert 0x00220021 (DictEntry "SQ" "Left Image Sequence") .
  Map.insert 0x00220022 (DictEntry "SQ" "Right Image Sequence") .
  Map.insert 0x00220031 (DictEntry "SQ" "Ophthalmic Frame Location Sequence") .
  Map.insert 0x00220042 (DictEntry "SQ" "Mydriatic Agent Concentration Units Sequence") .
  Map.insert 0x00220058 (DictEntry "SQ" "Mydriatic Agent Sequence") .
  Map.insert 0x00280002 (DictEntry "US" "Samples per Pixel") .
  Map.insert 0x00280004 (DictEntry "CS" "Photometric Interpretation") .
  Map.insert 0x00280006 (DictEntry "US" "Planar Configuration") .
  Map.insert 0x00280008 (DictEntry "IS" "Number of Frames") .
  Map.insert 0x00280010 (DictEntry "US" "Rows") .
  Map.insert 0x00280011 (DictEntry "US" "Columns") .
  Map.insert 0x00280014 (DictEntry "US" "Ultrasound Color Data Present") .
  Map.insert 0x00280030 (DictEntry "DS" "Pixel Spacing") .
  Map.insert 0x00280051 (DictEntry "CS" "Corrected Image") .
  Map.insert 0x00280100 (DictEntry "US" "Bits Allocated") .
  Map.insert 0x00280101 (DictEntry "US" "Bits Stored") .
  Map.insert 0x00280102 (DictEntry "US" "High Bit") .
  Map.insert 0x00280103 (DictEntry "US" "Pixel Representation") .
  Map.insert 0x00280106 (DictEntry "US" "Smallest Image Pixel Value") .
  Map.insert 0x00280107 (DictEntry "US" "Largest Image Pixel Value") .
  Map.insert 0x00280301 (DictEntry "CS" "Burned In Annotation") .
  Map.insert 0x00281040 (DictEntry "CS" "Pixel Intensity Relationship") .
  Map.insert 0x00281050 (DictEntry "DS" "Window Centre") .
  Map.insert 0x00281051 (DictEntry "DS" "Window Width") .
  Map.insert 0x00281052 (DictEntry "DS" "Rescale Intercept") .
  Map.insert 0x00281053 (DictEntry "DS" "Rescale Slope") .
  Map.insert 0x00281054 (DictEntry "LO" "Rescale Type") .
  Map.insert 0x00281055 (DictEntry "LO" "Window Width & Centre Explanation") .
  Map.insert 0x00281352 (DictEntry "SQ" "Partial View Code Sequence") .
  Map.insert 0x00281401 (DictEntry "SQ" "Data Frame Assignment Sequence") .
  Map.insert 0x00281404 (DictEntry "SQ" "Blending LUT 1 Sequence") .
  Map.insert 0x0028140b (DictEntry "SQ" "Enhanced Palette Color Lookup Table Sequence") .
  Map.insert 0x0028140c (DictEntry "SQ" "Blending LUT 2 Sequence") .
  Map.insert 0x00282110 (DictEntry "CS" "Lossy Image Compression") .
  Map.insert 0x00282112 (DictEntry "DS" "Lossy Image Compression Ratio") .
  Map.insert 0x00283000 (DictEntry "SQ" "Modality LUT Sequence") .
  Map.insert 0x00283003 (DictEntry "LO" "LUT Explanation") .
  Map.insert 0x00283010 (DictEntry "SQ" "VOI LUT Sequence") .
  Map.insert 0x00283110 (DictEntry "SQ" "Softcopy VOI LUT Sequence") .
  Map.insert 0x00286100 (DictEntry "SQ" "Mask Subtraction Sequence") .
  Map.insert 0x00289001 (DictEntry "UL" "Data Point Rows") .
  Map.insert 0x00289002 (DictEntry "UL" "Data Point Columns") .
  Map.insert 0x00289110 (DictEntry "SQ" "Pixel Measures Sequence") .
  Map.insert 0x00289132 (DictEntry "SQ" "Frame VOI LUT Sequence") .
  Map.insert 0x00289145 (DictEntry "SQ" "Pixel Value Transformation Sequence") .
  Map.insert 0x00289415 (DictEntry "SQ" "Frame Pixel Shift Sequence") .
  Map.insert 0x00289422 (DictEntry "SQ" "Pixel Intensity Relationship LUT Sequence") .
  Map.insert 0x00289443 (DictEntry "SQ" "Frame Pixel Data Properties Sequence") .
  Map.insert 0x00289501 (DictEntry "SQ" "Pixel Shift Sequence") .
  Map.insert 0x00289502 (DictEntry "SQ" "Region Pixel Shift Sequence") .
  Map.insert 0x00289505 (DictEntry "SQ" "Multi-frame Presentation Sequence") .
  Map.insert 0x00321031 (DictEntry "SQ" "Requesting Physician Identification Sequence") .
  Map.insert 0x00321032 (DictEntry "PN" "Requesting Physician") .
  Map.insert 0x00321033 (DictEntry "LO" "Requesting Service") .
  Map.insert 0x00321034 (DictEntry "SQ" "Requesting Physician Identification Sequence") .
  Map.insert 0x00321060 (DictEntry "LO" "Request Procedure Description") .
  Map.insert 0x00321064 (DictEntry "SQ" "Requesting Service Code Sequence") .
  Map.insert 0x00324000 (DictEntry "LT" "Study Comments") .
  Map.insert 0x00380004 (DictEntry "SQ" "Referenced Patient Alias Sequence") .
  Map.insert 0x00380014 (DictEntry "SQ" "Issuer of Admission ID Sequence") .
  Map.insert 0x00380064 (DictEntry "SQ" "Issuer of Service Episode ID Sequence") .
  Map.insert 0x00380100 (DictEntry "SQ" "Pertinent Documents Sequence") .
  Map.insert 0x00380502 (DictEntry "SQ" "Patient Clinical Trial Participation Sequence") .
  Map.insert 0x003a0200 (DictEntry "SQ" "Channel Definition Sequence") .
  Map.insert 0x003a0208 (DictEntry "SQ" "Channel Source Sequence") .
  Map.insert 0x003a0209 (DictEntry "SQ" "Channel Source Modifiers Sequence") .
  Map.insert 0x003a020a (DictEntry "SQ" "Source Waveform Sequence") .
  Map.insert 0x003a0211 (DictEntry "SQ" "Channel Sensitivity Units Sequence") .
  Map.insert 0x003a0240 (DictEntry "SQ" "Waveform Presentation Group Sequence") .
  Map.insert 0x003a0242 (DictEntry "SQ" "Channel Display Sequence") .
  Map.insert 0x003a0300 (DictEntry "SQ" "Multiplexed Audio Channels Description Code Sequence") .
  Map.insert 0x00400002 (DictEntry "DA" "Scheduled Procedure Step Start Date") .
  Map.insert 0x00400003 (DictEntry "TM" "Scheduled Procedure Step Start Time") .
  Map.insert 0x00400004 (DictEntry "DA" "Scheduled Procedure Step End Date") .
  Map.insert 0x00400005 (DictEntry "TM" "Scheduled Procedure Step End Time") .
  Map.insert 0x00400007 (DictEntry "LO" "Scheduled Procedure Step Description") .
  Map.insert 0x00400008 (DictEntry "SQ" "Scheduled Procedure Step Sequence") .
  Map.insert 0x00400009 (DictEntry "SH" "Scheduled Procedure Step ID") .
  Map.insert 0x0040000a (DictEntry "SQ" "Stage Code Sequence") .
  Map.insert 0x0040000b (DictEntry "SQ" "Scheduled Performing Physician Identification Sequence") .
  Map.insert 0x00400026 (DictEntry "SQ" "Order Placer Identifier Sequence") .
  Map.insert 0x00400027 (DictEntry "SQ" "Order Filler Identifier Sequence") .
  Map.insert 0x00400036 (DictEntry "SQ" "Assigning Facility Sequence") .
  Map.insert 0x00400039 (DictEntry "SQ" "Assigning Jurisdiction Code Sequence") .
  Map.insert 0x0040003a (DictEntry "SQ" "Assigning Agency or Department Code Sequence") .
  Map.insert 0x00400100 (DictEntry "SQ" "Scheduled Procedure Step Sequence") .
  Map.insert 0x00400220 (DictEntry "SQ" "Referenced Non-Image Composite SOP Instance Sequence") .
  Map.insert 0x00400241 (DictEntry "AE" "Performed Station AE Title") .
  Map.insert 0x00400244 (DictEntry "DA" "Performed Procedure Step Start Date") .
  Map.insert 0x00400245 (DictEntry "TM" "Performed Procedure Step Start Time") .
  Map.insert 0x00400250 (DictEntry "DA" "Performed Procedure Step End Date") .
  Map.insert 0x00400251 (DictEntry "TM" "Performed Procedure Step End Time") .
  Map.insert 0x00400253 (DictEntry "SH" "Performed Procedure Step ID") .
  Map.insert 0x00400254 (DictEntry "LO" "Performed Procedure Step Description") .
  Map.insert 0x00400260 (DictEntry "SQ" "Performed Protocol Code Sequence") .
  Map.insert 0x00400270 (DictEntry "SQ" "Scheduled Step Attributes Sequence") .
  Map.insert 0x00400275 (DictEntry "SQ" "Request Attributes Sequence") .
  Map.insert 0x00400280 (DictEntry "ST" "Comments on the Performed Procedure Step") .
  Map.insert 0x00400281 (DictEntry "SQ" "Performed Procedure Step Discontinuation Reason Code Sequence") .
  Map.insert 0x00400293 (DictEntry "SQ" "Quantity Sequence") .
  Map.insert 0x00400295 (DictEntry "SQ" "Measuring Units Sequence") .
  Map.insert 0x00400296 (DictEntry "SQ" "Billing Item Sequence") .
  Map.insert 0x0040030e (DictEntry "SQ" "Exposure Dose Sequence") .
  Map.insert 0x00400320 (DictEntry "SQ" "Billing Procedure Step Sequence") .
  Map.insert 0x00400321 (DictEntry "SQ" "Film Consumption Sequence") .
  Map.insert 0x00400324 (DictEntry "SQ" "Billing Supplies and Devices Sequence") .
  Map.insert 0x00400340 (DictEntry "SQ" "Performed Series Sequence") .
  Map.insert 0x00400440 (DictEntry "SQ" "Protocol Context Sequence") .
  Map.insert 0x00400441 (DictEntry "SQ" "Content Item Modifier Sequence") .
  Map.insert 0x00400500 (DictEntry "SQ" "Scheduled Specimen Sequence") .
  Map.insert 0x00400513 (DictEntry "SQ" "Issuer of the Container Identifier Sequence") .
  Map.insert 0x00400515 (DictEntry "SQ" "Alternate Container Identifier Sequence") .
  Map.insert 0x00400518 (DictEntry "SQ" "Container Type Code Sequence") .
  Map.insert 0x00400520 (DictEntry "SQ" "Container Component Sequence") .
  Map.insert 0x00400555 (DictEntry "SQ" "Acquisition Context Sequence") .
  Map.insert 0x0040059a (DictEntry "SQ" "Specimen Type Code Sequence") .
  Map.insert 0x00400560 (DictEntry "SQ" "Specimen Description Sequence") .
  Map.insert 0x00400562 (DictEntry "SQ" "Issuer of the Specimen Identifier Sequence") .
  Map.insert 0x00400610 (DictEntry "SQ" "Specimen Preparation Sequence") .
  Map.insert 0x00400612 (DictEntry "SQ" "Specimen Preparation Step Content Item Sequence") .
  Map.insert 0x00400620 (DictEntry "SQ" "Specimen Localization Content Item Sequence") .
  Map.insert 0x0040071a (DictEntry "SQ" "Image Center Point Coordinates Sequence") .
  Map.insert 0x004008d8 (DictEntry "SQ" "Pixel Spacing Sequence") .
  Map.insert 0x004008da (DictEntry "SQ" "Coordinate System Axis Code Sequence") .
  Map.insert 0x004008ea (DictEntry "SQ" "Measurement Units Code Sequence") .
  Map.insert 0x00401001 (DictEntry "SH" "Requested Procedure ID") .
  Map.insert 0x0040100a (DictEntry "SQ" "Reason for Requested Procedure Code Sequence") .
  Map.insert 0x00401011 (DictEntry "SQ" "Intended Recipients of Results Identification Sequence") .
  Map.insert 0x00401012 (DictEntry "SQ" "Reason For Performed Procedure Code Sequence") .
  Map.insert 0x00404004 (DictEntry "SQ" "Scheduled Processing Applications Code Sequence") .
  Map.insert 0x00404007 (DictEntry "SQ" "Performed Processing Applications Code Sequence") .
  Map.insert 0x00404009 (DictEntry "SQ" "Human Performer Code Sequence") .
  Map.insert 0x00404015 (DictEntry "SQ" "Resulting General Purpose Performed Procedure Steps Sequence") .
  Map.insert 0x00404016 (DictEntry "SQ" "Referenced General Purpose Scheduled Procedure Step Sequence") .
  Map.insert 0x00404018 (DictEntry "SQ" "Scheduled Workitem Code Sequence") .
  Map.insert 0x00404019 (DictEntry "SQ" "Performed Workitem Code Sequence") .
  Map.insert 0x00404021 (DictEntry "SQ" "Input Information Sequence") .
  Map.insert 0x00404022 (DictEntry "SQ" "Relevant Information Sequence") .
  Map.insert 0x00404025 (DictEntry "SQ" "Scheduled Station Name Code Sequence") .
  Map.insert 0x00404026 (DictEntry "SQ" "Scheduled Station Class Code Sequence") .
  Map.insert 0x00404027 (DictEntry "SQ" "Scheduled Station Geographic Location Code Sequence") .
  Map.insert 0x00404028 (DictEntry "SQ" "Performed Station Name Code Sequence") .
  Map.insert 0x00404029 (DictEntry "SQ" "Performed Station Class Code Sequence") .
  Map.insert 0x00404030 (DictEntry "SQ" "Performed Station Geographic Location Code Sequence") .
  Map.insert 0x00404031 (DictEntry "SQ" "Requested Subsequent Workitem Code Sequence") .
  Map.insert 0x00404032 (DictEntry "SQ" "Non-DICOM Output Code Sequence") .
  Map.insert 0x00404033 (DictEntry "SQ" "Output Information Sequence") .
  Map.insert 0x00404034 (DictEntry "SQ" "Scheduled Human Performers Sequence") .
  Map.insert 0x00409094 (DictEntry "SQ" "Referenced Image Real World Value Mapping Sequence") .
  Map.insert 0x00409096 (DictEntry "SQ" "Real World Value Mapping Sequence") .
  Map.insert 0x00409098 (DictEntry "SQ" "Pixel Value Mapping Code Sequence") .
  Map.insert 0x00409210 (DictEntry "SH" "LUT Label") .
  Map.insert 0x0040a043 (DictEntry "SQ" "Concept Name Code Sequence") .
  Map.insert 0x0040a073 (DictEntry "SQ" "Verifying Observer Sequence") .
  Map.insert 0x0040a078 (DictEntry "SQ" "Author Observer Sequence") .
  Map.insert 0x0040a07a (DictEntry "SQ" "Participant Sequence") .
  Map.insert 0x0040a07c (DictEntry "SQ" "Custodial Organization Sequence") .
  Map.insert 0x0040a088 (DictEntry "SQ" "Verifying Observer Identification Code Sequence") .
  Map.insert 0x0040a168 (DictEntry "SQ" "Concept Code Sequence") .
  Map.insert 0x0040a170 (DictEntry "SQ" "Purpose of Reference Code Sequence") .
  Map.insert 0x0040a195 (DictEntry "SQ" "Modifier Code Sequence") .
  Map.insert 0x0040a300 (DictEntry "SQ" "Measured Value Sequence") .
  Map.insert 0x0040a301 (DictEntry "SQ" "Numeric Value Qualifier Code Sequence") .
  Map.insert 0x0040a360 (DictEntry "SQ" "Predecessor Documents Sequence") .
  Map.insert 0x0040a370 (DictEntry "SQ" "Referenced Request Sequence") .
  Map.insert 0x0040a372 (DictEntry "SQ" "Performed Procedure Code Sequence") .
  Map.insert 0x0040a375 (DictEntry "SQ" "Current Requested Procedure Evidence Sequence") .
  Map.insert 0x0040a385 (DictEntry "SQ" "Pertinent Other Evidence Sequence") .
  Map.insert 0x0040a390 (DictEntry "SQ" "HL7 Structured Document Reference Sequence") .
  Map.insert 0x0040a504 (DictEntry "SQ" "Content Template Sequence") .
  Map.insert 0x0040a525 (DictEntry "SQ" "Identical Documents Sequence") .
  Map.insert 0x0040a730 (DictEntry "SQ" "Content Sequence") .
  Map.insert 0x0040b020 (DictEntry "SQ" "Waveform Annotation Sequence") .
  Map.insert 0x0040e006 (DictEntry "SQ" "HL7 Document Type Code Sequence") .
  Map.insert 0x00420013 (DictEntry "SQ" "Source Instance Sequence") .
  Map.insert 0x00440007 (DictEntry "SQ" "Product Type Code Sequence") .
  Map.insert 0x00440013 (DictEntry "SQ" "Product Parameter Sequence") .
  Map.insert 0x00440019 (DictEntry "SQ" "Substance Administration Parameter Sequence") .
  Map.insert 0x00460014 (DictEntry "SQ" "Right Lens Sequence") .
  Map.insert 0x00460015 (DictEntry "SQ" "Left Lens Sequence") .
  Map.insert 0x00460016 (DictEntry "SQ" "Unspecified Laterality Lens Sequence") .
  Map.insert 0x00460018 (DictEntry "SQ" "Cylinder Sequence") .
  Map.insert 0x00460028 (DictEntry "SQ" "Prism Sequence") .
  Map.insert 0x00460050 (DictEntry "SQ" "Autorefraction Right Eye Sequence") .
  Map.insert 0x00460052 (DictEntry "SQ" "Autorefraction Left Eye Sequence") .
  Map.insert 0x00460070 (DictEntry "SQ" "Keratometry Right Eye Sequence") .
  Map.insert 0x00460071 (DictEntry "SQ" "Keratometry Left Eye Sequence") .
  Map.insert 0x00460074 (DictEntry "SQ" "Steep Keratometric Axis Sequence") .
  Map.insert 0x00460080 (DictEntry "SQ" "Flat Keratometric Axis Sequence") .
  Map.insert 0x00460097 (DictEntry "SQ" "Subjective Refraction Right Eye Sequence") .
  Map.insert 0x00460098 (DictEntry "SQ" "Subjective Refraction Left Eye Sequence") .
  Map.insert 0x00460100 (DictEntry "SQ" "Add Near Sequence") .
  Map.insert 0x00460101 (DictEntry "SQ" "Add Intermediate Sequence") .
  Map.insert 0x00460102 (DictEntry "SQ" "Add Other Sequence") .
  Map.insert 0x00460121 (DictEntry "SQ" "Visual Acuity Type Code Sequence") .
  Map.insert 0x00460122 (DictEntry "SQ" "Visual Acuity Right Eye Sequence") .
  Map.insert 0x00460123 (DictEntry "SQ" "Visual Acuity Left Eye Sequence") .
  Map.insert 0x00460124 (DictEntry "SQ" "Visual Acuity Both Eyes Open Sequence") .
  Map.insert 0x00460145 (DictEntry "SQ" "Referenced Refractive Measurements Sequence") .
  Map.insert 0x00500010 (DictEntry "SQ" "Device Sequence") .
  Map.insert 0x00500012 (DictEntry "SQ" "Container Component Type Code Sequence") .
  Map.insert 0x00540012 (DictEntry "SQ" "Energy Window Information Sequence") .
  Map.insert 0x00540013 (DictEntry "SQ" "Energy Window Range Sequence") .
  Map.insert 0x00540014 (DictEntry "DS" "Energy Window Lower Limit") .
  Map.insert 0x00540015 (DictEntry "DS" "Energy Window Upper Limit") .
  Map.insert 0x00540016 (DictEntry "SQ" "Radiopharmaceutical Information Sequence") .
  Map.insert 0x00540022 (DictEntry "SQ" "Detector Information Sequence") .
  Map.insert 0x00540032 (DictEntry "SQ" "Phase Information Sequence") .
  Map.insert 0x00540052 (DictEntry "SQ" "Rotation Information Sequence") .
  Map.insert 0x00540062 (DictEntry "SQ" "Gated Information Sequence") .
  Map.insert 0x00540063 (DictEntry "SQ" "Data Information Sequence") .
  Map.insert 0x00540072 (DictEntry "SQ" "Time Slot Information Sequence") .
  Map.insert 0x00540081 (DictEntry "US" "Number of Slices") .
  Map.insert 0x00540220 (DictEntry "SQ" "View Code Sequence") .
  Map.insert 0x00540222 (DictEntry "SQ" "View Modifier Code Sequence") .
  Map.insert 0x00540300 (DictEntry "SQ" "Radionuclide Code Sequence") .
  Map.insert 0x00540302 (DictEntry "SQ" "Administration Route Code Sequence") .
  Map.insert 0x00540304 (DictEntry "SQ" "Radiopharmaceutical Code Sequence") .
  Map.insert 0x00540306 (DictEntry "SQ" "Calibration Data Sequence") .
  Map.insert 0x00540410 (DictEntry "SQ" "Patient Orientation Code Sequence") .
  Map.insert 0x00540412 (DictEntry "SQ" "Patient Orientation Modifier Code Sequence") .
  Map.insert 0x00540414 (DictEntry "SQ" "Patient Gantry Relationship Code Sequence") .
  Map.insert 0x00541000 (DictEntry "CS" "Series Type") .
  Map.insert 0x00541001 (DictEntry "CS" "Units") .
  Map.insert 0x00541002 (DictEntry "CS" "Counts Source") .
  Map.insert 0x00541100 (DictEntry "CS" "Randoms Correction Method") .
  Map.insert 0x00541101 (DictEntry "LO" "Attenuation Correction Method") .
  Map.insert 0x00541102 (DictEntry "CS" "Decay Correction") .
  Map.insert 0x00541103 (DictEntry "LO" "Reconstruction Method") .
  Map.insert 0x00541104 (DictEntry "LO" "Detector Lines of Response Used") .
  Map.insert 0x00541105 (DictEntry "LO" "Scatter Correction Method") .
  Map.insert 0x00541200 (DictEntry "DS" "Axial Acceptance") .
  Map.insert 0x00541201 (DictEntry "IA" "Axial Mash") .
  Map.insert 0x00541300 (DictEntry "DS" "Frame Reference Time") .
  Map.insert 0x00541321 (DictEntry "DS" "Decay Factor") .
  Map.insert 0x00541322 (DictEntry "DS" "Dose Calibration Factor") .
  Map.insert 0x00541330 (DictEntry "US" "Image Index") .
  Map.insert 0x00603000 (DictEntry "SQ" "Histogram Sequence") .
  Map.insert 0x00620002 (DictEntry "SQ" "Segment Sequence") .
  Map.insert 0x00620003 (DictEntry "SQ" "Segmented Property Category Code Sequence") .
  Map.insert 0x0062000a (DictEntry "SQ" "Segment Identification Sequence") .
  Map.insert 0x0062000f (DictEntry "SQ" "Pre Deformation Matrix Registration Sequence") .
  Map.insert 0x00640002 (DictEntry "SQ" "Deformable Registration Sequence") .
  Map.insert 0x00640005 (DictEntry "SQ" "Deformable Registration Grid Sequence") .
  Map.insert 0x0064000f (DictEntry "SQ" "Segment Identification Sequence") .
  Map.insert 0x00640010 (DictEntry "SQ" "Post Deformation Matrix Registration Sequence") .
  Map.insert 0x00660002 (DictEntry "SQ" "Surface Sequence") .
  Map.insert 0x00660011 (DictEntry "SQ" "Surface Points Sequence") .
  Map.insert 0x00660012 (DictEntry "SQ" "Surface Points Normals Sequence") .
  Map.insert 0x00660013 (DictEntry "SQ" "Surface Mesh Primitives Sequence") .
  Map.insert 0x00660026 (DictEntry "SQ" "Triangle Strip Sequence") .
  Map.insert 0x00660027 (DictEntry "SQ" "Triangle Fan Sequence") .
  Map.insert 0x00660028 (DictEntry "SQ" "Line Sequence") .
  Map.insert 0x0066002b (DictEntry "SQ" "Referenced Surface Sequence") .
  Map.insert 0x0066002d (DictEntry "SQ" "Segment Surface Generation Algorithm Identification Sequence") .
  Map.insert 0x0066002e (DictEntry "SQ" "Segment Surface Source Instance Sequence") .
  Map.insert 0x0066002f (DictEntry "SQ" "Algorithm Family Code Sequence") .
  Map.insert 0x00660030 (DictEntry "SQ" "Algorithm Name Code Sequence") .
  Map.insert 0x00660034 (DictEntry "SQ" "Facet Sequence") .
  Map.insert 0x00660035 (DictEntry "SQ" "Surface Processing Algorithm Identification Sequence") .
  Map.insert 0x00880140 (DictEntry "UI" "Storage Media File-set UID") .
  Map.insert 0x00880200 (DictEntry "UI" "Icon Image Sequence") .
  Map.insert 0x20200020 (DictEntry "CS" "Polarity") .
  Map.insert 0x20500020 (DictEntry "CS" "Presentation LUT Shape") .
  Map.insert 0x300e0002 (DictEntry "CS" "Approval Status") .
  Map.insert 0x52009229 (DictEntry "SQ" "Shared Functional Groups Sequence") .
  Map.insert 0x52009230 (DictEntry "SQ" "Per-frame Functional Groups Sequence") .
  Map.insert 0x60000010 (DictEntry "US" "Overlay Rows") .
  Map.insert 0x60000011 (DictEntry "US" "Overlay Columns") .
  Map.insert 0x60000040 (DictEntry "CS" "Overlay Type") .
  Map.insert 0x60000050 (DictEntry "SS" "Overlay Origin") .
  Map.insert 0x60000100 (DictEntry "US" "Overlay Bits Allocated") .
  Map.insert 0x60000102 (DictEntry "US" "Overlay Bit Position") .
  Map.insert 0x7fe00010 (DictEntry "OB" "Pixel Data") 
  $ Map.empty

