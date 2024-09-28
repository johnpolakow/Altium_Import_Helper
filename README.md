# Altium_Import_Helper
Altium DelphiScript Application which imports UltraLibrarian zip files, SnapEda zip files, IntLib files, SchLib files, and PcbLib files. All files are imported and are combined into one LibPkg. This script handles the unzipping of the files, no need to unzip the files first. Also this script imports as many UltraLibrarian files as you have, all at once, and no need to import files individually.

## What This Script Does
Imports EDA files:
  - Unzips UltraLibrarian and SnapEda files
  - Extracts IntLibs to source SchLibs and PcbLibs
  - Imports UltraLibrarian text files
  - Imports SchLibs and PcbLibs
  - places source zip files in Archive dir
  - Improves aspects of the symbols and footprints as described below

### Processes Symbols in SchLib:
  - sets the pin font and size to constants defined in:  PIN_FONT_NAME, PIN_FONT_SIZE
  - sets active low pins so an overbar is in schematic symbol on pin name
  - filters symbol parameters, deletes many unecessary params which are included in symbols
  - sets symbol body to color defined in constant COLOR_SYMBOL_BODY_FILL
  - sets symbol body outline to color defined in constant COLOR_SYMBOL_OUTLINE
  - sets all text label objects to constant: DEFAULT_FONT
  - changes all lines in symbol to color blue
  - moves component so pin1 is at the origin, including for multipart sch symbols
  - fixes UltraLib footprint previews
  - makes MFR Part # visible, at bottom left of symbol body, including for multipart sch symbols
  - deletes default schematic symbol Component1
  - includes a Graphics_Symbol which has many graphics shapes to use in your imported symbols

### Processes Footprints in PcbLib:
  - deletes default footprint PcbComponent1
  - changes silkscreen pin 1 indicator of * to filled circle (Ultralibrarian uses asterisk as pin 1 indicator)
  - changes Mechanical Layer13 pin 1 indicator of '*' to filled circle (Ultralibrarian uses asterisk as pin 1 indicator)
  - changes linewidth of Mechanical13 Layer from 1 mil to 2mil
  - automatically maps STEP files for SnapEDA footprints'

## How to Use
For detailed instructions, read How_To_Install_and_Use_Import_Helper.pdf, which is included in repo. It makes importing EDA files extremely easy. Just throw all the files you want to import into a single directory.
1. Create a directory to place all the zip files/EDA files/SchLib files in. For instance create a directory 'Import' in your Downloads directory, and place all the files ti import inside the 'Import' dir. (import01 img)
2. Run the script inside Altium. You can either install the script as part of the GUI (instructions in How_To_Install_and_Use_Import_Helper.pdf) or run in standalone mode. To run in standalone mode without installing, do as below
 - In Altium select File -> Run Script (import02 img)
 - click Browse (import 03 img)
 - select AltiumImportPrj (import04 img)
 - select AltiumImport.pas (import05 img)
3. The script starts, click the folder icon (import06 img)
4. Select the 'Import' folder you created where all the zip files are placed. Note: you will not see any files in this window, only directories. (import07 img)
5. Click start. (import08 img)
6. The GUI will report progress as it imports the files (import09 img)
7. A message displays how many symbols and footprints were imported (import10 img)
8. The SchLib and PcbLib are open with all imported symbols and footprints (import 011 img)

## Awesome Things This Script Does (which UltraLibrarian provided script doesnt do)
