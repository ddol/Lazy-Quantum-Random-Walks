Installation instructions for the Mathematica package Quantum

These instructions are illustrated with graphs in the file readme.mht, which can be opened with Internet Explorer or with another Web Browser. We recommend to print and read that file before doing the installation.

INSTRUCTIONS

STEP 1. Enter Mathematica and write:
$UserBaseDirectory
Then press Shift-Enter. Mathematica will answer with the file location (directory, folder) where user packages must be installed.

STEP 2. Use your operative system (for example Windows) to open the location of the previous step. There must be a directory with the name Applications inside the location of the previous step. If it doesn't exist, create it.

STEP 3. Create a directory (folder) with the name Quantum inside the Applications directory of the previous step.
   
STEP 4. Copy the file Notation.m inside the Quantum directory of the previous step.

Steps 1 to 4 are enough for using the package. Now you can enter Mathematica and write
Needs["Quantum`Notation`"]
Then press Shift-Enter and the package will be loaded. If the installation was correct, you will see a message recommending to execute the SetQuantumAliases[] command.

If you want to have the documentation for the package inside Mathematica's Help System (Help Browser for Mathematica 5.2 or Documentation Center for Mathematica 6.0) then follow these steps:

STEP 5. Create a directory (folder) with the name Documentation inside the Quantum directory of step 3.

STEP 6. Create a directory (folder) with the name English inside the Documentation directory of the previous step.

STEP 7. Copy the files QuantumNotationDocumentation.nb and BrowserCategories.m inside the English directory of the previous step

STEP 8. Enter Mathematica. If you are using Mathematica version 5.2, then in Mathematica's Help menu select Rebuild Help Index. After this, the documentation of the package will appear in the Help Browser, at the end of the Add-ons & Links section. On the other hand, if you are using Mathematica version 6.0, then you do not need to do anything, the documentation of the package will appear in the Documentation Center, in the Installed Add-ons section.

Steps 1 to 8 are enough for having the package, with its documentation inside Mathematica's help system.

If you want to use the Palette (Toolbar) of the package, follow these steps:

STEP 9. Create a directory (folder) with the name FrontEnd inside the Quantum directory of step 3.

STEP 10. Create a directory (folder) with the name Palettes inside the FrontEnd directory of the previous step.

STEP 11. Copy the files QuantumNotationPaletteV52.nb and QuantumNotationPaletteV60.nb inside the Palettes directory of the previous step

STEP 12. Exit Mathematica and then enter Mathematica again. If you are using Mathematica version 5.2, then the palette will appear in Mathematica's File menu, in the Palettes submenu. If you are using Mathematica version 6.0, then the palette will appear in Mathematica's Palette menu.  

Steps 1 to 12 are enough for having the package, with documentation and palette (toolbar).

These instructions are illustrated with graphs in the file readme.mht, which can be opened with Internet Explorer or with another web Browser. We recommend to print and read that file before doing the installation.

Author: Jose Luis Gomez-Munoz
July 6, 2007
Mexico

Some material on the general installation of packages in Mathematica version 5.2 can be found in this link:
http://homepage.cem.itesm.mx/lgomez/createMathematicaAddOn.htm




