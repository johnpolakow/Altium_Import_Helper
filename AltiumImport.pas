{***************************************************************************
 *  Author:        John Polakowski, johnmpolakowski@gmail.com

 *  Version 1.0     09/01/24    Initial Version
 *  Version 1.1     09/08/24    Changes footprint pin 1 indicator from '*' to filled circle
 *  Version 1.2     09/12/24    Changes symbol pin length and places pin1 at the origin, works for multipart symbols
 *  Version 1.3     09/13/24    makes MFR Part # visible, at bottom left of symbol body
 *  Version 1.4     09/15/24    Maps STEP files for footprints if STEP file available
 *  Version 1.5     09/27/24    Added Graphics Primitives symbol
---------------------------------------------------------------------------
 AltiumImport.pas
 Altium DelphiScript which automates importing Schematic symbols and footprints.
 It will import any format of symbols and footprints: IntLib, SchLib, PcbLib,
 UltraLibrarian zip files, and SnapEDA zip files. Place all files to be imported
 into a single folder. When the program starts select the folder which contains
 the files to be imported. All files will be imported into a single LibPkg.
 NOTE:  to see the footprint previews in the schematic symbols, it is necessary
        to save the LibPkg project or SchLib and then go back and try to see
        the footprint preview again

 the file UL_Import.pas is slightly modified from the version UltraLibrarian
 uses in its downloads. Many new methods were added.

What this script does:
    Imports EDA files:
        Unzips UltraLibrarian and SnapEda files
        Extracts IntLibs to source SchLibs and PcbLibs
        Imports UltraLibrarian text files
        Imports SchLibs and PcbLibs
        places source zip files in Archive dir
        Improves aspects of the symbols and footprints as described below
    
    Processes Symbols in SchLib:
        sets the pin font and size to constants defined below:  PIN_FONT_NAME, PIN_FONT_SIZE
        sets active low pins so an overbar is in schematic symbol on pin name
        filters symbol parameters, deletes many unecessary params, see 'FilterComponentParameters' method to edit
        sets symbol body to color defined in constant below, COLOR_SYMBOL_BODY_FILL
        sets symbol body outline to color defined in constant below, COLOR_SYMBOL_OUTLINE
        sets all text label objects to constant: DEFAULT_FONT 
        changes all lines in symbol to color blue
        moves component so pin1 is at the origin, including for multipart sch symbols
        fixes UltraLib footprint previews
        makes MFR Part # visible, at bottom left of symbol body, including for multipart sch symbols
        deletes default schematic symbol Component1
               
    Processes Footprints in PcbLib:
        deletes default footprint PcbComponent1
        changes silkscreen pin 1 indicator of '*' to filled circle
        changes Mechanical Layer13 pin 1 indicator of '*' to filled circle
        changes linewidth of Mechanical13 Layer from 1 mil to 2mil
        automatically maps STEP files for SnapEDA footprints

 ***************************************************************************}



{ CONSTANTS, EDIT ACCORDING TO YOUR PREFERENCE }
const GRAPHICS_LIB_PATH = '';
const APPLY_FONTS_AND_COLORS_TO_SYMBOLS = True; // set this to True if you want below fonts and colors to take effect
const PIN_FONT_NAME = 'Arial';
const PIN_FONT_SIZE = 9;
const DEFAULT_FONT = 'Arial';
const DEFAULT_FONT_SIZE = 9;
const COLOR_TEXT = $B00000;     // BGR format
const COLOR_BLUE = $FF0000;     // BGR format
const COLOR_BLACK = $000000;    // BGR format
const COLOR_SYMBOL_OUTLINE = COLOR_BLUE;
const COLOR_SYMBOL_BODY_FILL = $B0FFFF;   // light yellow, BGR format
const SHOW_MANUFACTURER_PART_NUMBER = True;

{ DO NOT CHANGE BELOW VALUES }
const MAX_ZIP_FILE_SIZE = 5000000; // 5,000Kb- max zip file size to be considered a possible Ultralibrarian/SnapEDA or STEP file
const CUSTOM_SETTING = 1;         // used for setting custom pin fonts

{ GLOBAL VARIABLES }
var
    Rpt : TStringList;
    ArchiveDir : WideString;        { main Archive Folder }
    ZipArchiveDir : WideString;     { move EDA files here after importing }
    IntLibArchiveDir : WideString;  { move IntLib files here after importing }
    StepFileDir : WideString;       { move Step files here after importing }
    ImportDir : WideString;         { where to search for files to import, also where library is saved }

{******   METHOD DECLARATIONS   ******}
Function CreateLibPkg(  LibPkgPath : String,
                 Out LibPkgProject : IProject,
                       Out ProjDoc : IServerDocument,
                     Out PcbLibDoc : IServerDocument,
                     Out SchLibDoc : IServerDocument,
                        Out PcbLib : IPCB_Library,
                        Out SchLib : ISch_Document): Boolean; forward;
Function GetSchLib_from_LibPkg(LibPkg : IProject) : IDocument; forward;
Function GetPcbLib_from_LibPkg(LibPkg : IProject) : IDocument; forward;
Procedure ImportUltraLibrarianTextFiles(SchLib : ISchLib, PcbLib : IPcbLib, UltraLibTxtFiles : TStringList); forward;
Procedure ImportSchLibFiles(MainSchLib : ISchLib, SchLibImportFiles : TStringList); forward;
Procedure ImportPcbLibFiles(MainPcbLib : IPcbLib, PcbLibImportFiles : TStringList); forward;
Procedure CopySchLibParts(SourceSchLib : ISchLib, DestSchLib : ISchLib); forward;
Function GetNumSchLibParts(CurrentLib : ISchLib); forward;
Function GetUniqueSchComponentName(SchLib : ISch_Lib, const CompName : String) : String; forward;
Procedure FilterComponentArcSize(Component : ISch_Component); forward;
Procedure FixComponentRectangles(Component : ISch_Component); forward;
function GetSymbolBodyRectangle(Component : ISchComponent, PartNum : Integer) : ISch_Rectangle; forward;
Procedure FilterComponentParameters(Component : ISch_Component); forward;
Function GetParameter_Manufacturer_Part_Number(Component : ISch_Component) : ISch_Parameter; forward;
Function GetActiveLowPinName(PinName : String) : String; forward;
Function SplitPinNameByForwardSlash(PinName: string) : TStringList; forward;
Procedure SetNormalizedPinName(Pin : ISch_Pin); forward;
Procedure SetActiveLowPinName(Pin : ISch_Pin); forward;
Procedure SetPinFont(Pin : ISch_Pin); forward;
Function GetAllComponentPins(Component : ISch_Component) : TList; forward;
function GetTopLeftPin(Component : ISch_Component, PartNum : Integer) : ISch_Pin; forward;
procedure SetPinLength(Pin : ISch_Pin, MaxPinLength : Integer); forward;
function GetPinDesignatorMaxNumChars(Component : ISch_Component) : Integer; forward;
Procedure ProcessComponentPins(Component : ISch_Component); forward;
Procedure MoveComponentByXY(Component : ISch_Component, PartNum : Integer, MoveX : Integer, MoveY : Integer); forward;
Procedure MoveComponentToOrigin(Component : ISch_Component); forward;
Procedure SetComponentTextLabelFont(Component : ISch_Component); forward;
Procedure ShowManufacturerPartNumber(Component : ISch_Component); forward;
Procedure RefreshFootprintMappings(Component : ISch_Component); forward;
Procedure ProcessSchLibComponents(SchLib : ISch_Lib); forward;
Function GetPcbPin1(Footprint : IPCB_Footprint) : IPCB_Pad; forward;
Procedure AddFilledPin1_Indicator( Footprint, LocX : Double, LocY : Double, Diameter : Double, Layer : TLayer); forward;
Procedure AddOpenPin1_Indicator( Footprint, LocX : Double, LocY : Double, Diameter : Double, Layer : TLayer); forward;
Function GetSchematicSymbolWithFootprintName(MainSchLib : ISchLib, FootprintName : String) : ISch_Component; forward;
Function FootprintHasStepFile(Footprint : IPCB_Footprint) : Boolean; forward;
Procedure MapStepFiles(PcbLib : IPcbLib, MainSchLib : ISchLib); forward;
Procedure ProcessPcbFootprints(PcbLib : IPcbLib); forward;
Procedure CopyPcbLibParts(SourcePcbLib : IPcb_Lib, DestPcbLib : IPcb_Lib); forward;
Procedure ExtractIntLibSources(IntLibFiles : TStringList, MainDir : String); forward;

{******   FILE MANIPULATION METHOD DECLARATIONS   ******}
Procedure Extract_Zip_Files(SearchDir : String); forward;
Function FindZipFiles(DirectoryPath : String) : TStringList; forward;
Procedure UnzipFileToDir(ZipFilePath : String, DestinationDir : String); forward;
Function Is_Altium_File(Filepath : String) : Boolean; forward;
Function GetUltraLibrarianTextFiles(SearchDir : String) : TStringList; forward;
Function IsUltraLibrarianFile(Filepath : String) : Boolean; forward;
Function GetIntLibFiles(DirectoryPath : String) : TStringList; forward;
Function GetSchLibFiles(DirectoryPath : String) : TStringList; forward;
Function GetPcbLibFiles(DirectoryPath : String) : TStringList; forward;
Function GetSTEPFiles(DirectoryPath : String) : TStringList; forward;
Function GetFilesInDir(DirPath : String, SearchSubDirs : Boolean) : TStringList; forward;
Procedure MoveFileToDir(OldFilePath : String, DestDirectory : String); forward;
Procedure MoveFilesToDir(FileList : TStringList, DestinationDir : String); forward;
Function GetSubDirectories(const SearchDirectory: String); forward;
procedure DeleteDir(dirPath : TString); forward;
Function GetTimeStampString(); forward;
Procedure RemoveStringFromList(list: TStringList, strToRemove: string); forward;

{** MAIN PROGRAM METHOD DECLARATION **}
Procedure StartImport(); forward;       

{******   GUI CALLBACK METHOD DECLARATIONS   ******}
Procedure TFind_File.DirectoryChooserChange(Sender: TObject); forward;
procedure TFind_File.StartButtonClick(Sender: TObject); forward;
Procedure TFind_File.CancelButtonClick(Sender: TObject); forward;

{==============================================================================}
{=============  Create LibPkg, SchLib, and PcbLib  Files  =====================}
{==============================================================================}
Function CreateLibPkg(  LibPkgPath : String,
                 Out LibPkgProject : IProject,
                       Out ProjDoc : IServerDocument,
                     Out PcbLibDoc : IServerDocument,
                     Out SchLibDoc : IServerDocument,
                        Out PcbLib : IPCB_Library,
                        Out SchLib : ISch_Document): Boolean;
Var
    WorkSpace : IWorkSpace;
Begin
    Result := False;    { return val, whether all libraries were create successfully }
    WorkSpace := GetWorkSpace;
    If WorkSpace = Nil Then Exit;
    if SchServer = Nil then Client.StartServer('SCH'); 

    GUIOperationLabel.Caption := 'Creating Libraries ...';  { show operation we are doing in GUI }
    GUIOperationLabel.Update;  { update label in GUI }

    {******   CREATE LIBPKG   ******}
    { Create LibPkg Document (an IntLib after compiling)  }
    ProjDoc := Client.OpenNewDocument(cDocKind_IntegratedLibrary, 'Imported_Lib', 'Imported_Lib', False);
    If ProjDoc = Nil Then Begin
        ShowError('Could not create LibPkg Project');
        Exit;
    End;

    { Save LibPkg to file }
    If Not ProjDoc.DoSafeChangeFileNameAndSave(LibPkgPath + '.LibPkg', cDocKind_IntegratedLibrary) Then Begin
        ShowError('ProjDoc Save failed!');
        Exit;
    End;

    { Get LibPkg as a Project }
    LibPkgProject := WorkSpace.DM_GetProjectFromPath(LibPkgPath + '.LibPkg');
    If LibPkgProject = Nil Then Begin
        ShowError('Could not get LibPkg, LibPkgProject is Nil');
        Exit;
    End;

    {******   CREATE PCBLIB   ******}
    { Open PcbLib Document }
    PcbLibDoc := Client.OpenNewDocument(cDocKind_PcbLib, 'Footprints', 'Footprints', False);
    If PcbLibDoc = Nil Then Begin
        ShowError('Could not open PcbLibDoc');
        Exit;
    End;

    { Save PcbLib Document to file }
    If Not PcbLibDoc.DoSafeChangeFileNameAndSave(LibPkgPath + '.PcbLib', cDocKind_PcbLib) Then Begin
        ShowError('PcbLibDoc Save failed');
        Exit;
    End;

    { Add PcbLib Document to the LibPkg project }
    LibPkgProject.DM_AddSourceDocument(LibPkgPath + '.PcbLib');

    { Get PcbLib Interface (so we can add footprints) }
    PcbLib := PCBServer.GetPCBLibraryByPath(LibPkgPath + '.PcbLib');
    If PcbLib = Nil Then Begin
        ShowError('Could not get PcbLib Interface');
        Exit;
    End;

    {******   CREATE SCHLIB   ******}
    { Open SchLib Document }
    SchLibDoc := Client.OpenNewDocument(cDocKind_SchLib, 'Components', 'Components', False);    { returns IServerDocument}
    If SchLibDoc = Nil Then Begin
        ShowError('Could not open new SchLib');
        Exit;
    End;

    { Save SchLib Document to file }
    If Not SchLibDoc.DoSafeChangeFileNameAndSave(LibPkgPath + '.SchLib', cDocKind_SchLib) Then Begin
        ShowMessage('SchLibDoc Save failed');
        Exit;
    End;

    { Add SchLib Document to the LibPkg project }
    LibPkgProject.DM_AddSourceDocument(LibPkgPath + '.SchLib');

    { Get SchLib Interface (so we can add schematic symbols) }
    SchLib := SchServer.GetSchDocumentByPath(LibPkgPath + '.SchLib');
    If SchLib = Nil Then Begin
        SchLib := SchServer.LoadSchDocumentByPath(LibPkg_SchLib.DM_FullPath);
    End;

    {******   SAVE CREATED FILES   ******}
    ProjDoc.DoFileSave(cDocKind_IntegratedLibrary);
    PcbLibDoc.DoFileSave(cDocKind_PcbLib);
    SchLibDoc.DoFileSave(cDocKind_SchLib);

    LibPkgProject.DM_SetAsCurrentProject();     { set LibPkg as focused project }
    Result := True;     { If we got to here, all libraries were created successfully}
End;

Function GetSchLib_from_LibPkg(LibPkg : IProject) : IDocument;
Var
    FileExtension  : String;
    Document       : IDocument;
    i              : Integer;
Begin
    Result := nil;
    For i := 0 To LibPkg.DM_LogicalDocumentCount - 1 Do
    Begin
        Document := LibPkg.DM_LogicalDocuments(i);
        FileExtension := ExtractFileExt( Document.DM_FileName );
        if (FileExtension = '.SchLib') and (Document.DM_DocumentKind = 'SCHLIB') Then
        Begin
            Result := Document;
            Exit;
        End;
    End;
End;

Function GetPcbLib_from_LibPkg(LibPkg : IProject) : IDocument; 
Var
    i              : Integer;
    Document       : IDocument;
    FileExtension  : String;
Begin
    Result := nil;
    For i := 0 To LibPkg.DM_LogicalDocumentCount - 1 Do
    Begin
        Document := LibPkg.DM_LogicalDocuments(i);
        FileExtension := ExtractFileExt( Document.DM_FileName );
        if (FileExtension = '.PcbLib') and (Document.DM_DocumentKind = 'PCBLIB') Then
        Begin
            Result := Document;
            Exit;
        End;
    End;
End;

Procedure ImportUltraLibrarianTextFiles(SchLib : ISchLib, PcbLib : IPcbLib, UltraLibTxtFiles : TStringList);
Var
    i          : Integer;
    ImportFile : String;
    InFile     : TextFile;
    inp, tag   : String;
Begin
    { detect Altium 19, it has broken SCH FontMangaer so in that case use fixed font }
    BrokenSCHFontManager := False;
    If ('19.0' < GetCurrentProductBuild) and (GetCurrentProductBuild < '20.0') Then 
        BrokenSCHFontManager := True;

    GUIOperationLabel.Caption := 'Importing UltraLibrarian Files...';  { show current operation in GUI }
    GUIOperationLabel.Update;  { update text label in GUI }

    { Iterate  over all text files, import one at a time }
    for i := 0 to UltraLibTxtFiles.Count - 1 do begin
        ImportFile := UltraLibTxtFiles.Strings[i];

        GUIFileLabel.Caption := ExtractFileName(ImportFile) + ' ... ' + GetStringFromInteger(i+1) + ' of ' + GetStringFromInteger(UltraLibTxtFiles.Count); 
        GUIFileLabel.Update;   { show in GUI progress of importing PcbLibs }
        GUIProgressBar.Position := ((i+1)/UltraLibTxtFiles.Count)*100; GUIProgressBar.Update; { update GUI progress bar }

        AssignFile(InFile, ImportFile); { Open the File for reading, assign file pointer }
        Reset(InFile);
        While Not EOF(InFile) Do Begin
            ReadLn(InFile, inp);
            If VarIsNull(inp) Then Continue;

            StrChop(inp, ' ', tag, inp); { splits inp string by first ' ', left split returned in tag, right split in inp }
            tag := Trim(tag);   { trim whitespace }
            Case tag Of
                'StartFootprints': Begin
                    ImportFootprints(InFile, PcbLib, Rpt);
                End;
                'StartComponents': Begin
                    ImportComponents(InFile, SchLib, Rpt);
                End;
                '': Continue;
            End;
        End;
        CloseFile(InFile);
        DeleteFile(ImportFile); { Delete the text file now that we have finished importing it }
    end;
    GUIProgressBar.Position := 100; GUIProgressBar.Update;
End;

Procedure ImportSchLibFiles(MainSchLib : ISchLib, SchLibImportFiles : TStringList);
Var
    i                 : Integer;
    ServerSchLibDoc   : IServerDocument;
    MainSchLibDoc     : ISch_Document;
    ImportLibFilePath : Widestring;
    ImportLibDoc      : ISchLib;
Begin
    { start schematic server if it hasnt been started already }
    if SchServer = Nil then Client.StartServer('SCH'); 

    { Must open serverdoc for added schematic components to persist }
    ServerSchLibDoc := Client.OpenDocument(cDocKind_SchLib, MainSchLib.DM_FullPath); // opens the SchLib in the server
    
    {* Get ISch_Doc interface from the ISCH_Lib interface *}
    MainSchLibDoc := SchServer.GetSchDocumentByPath(MainSchLib.DM_FullPath); // load ISch_Document from SchLib path
    if MainSchLibDoc = Nil then
       MainSchLibDoc := SchServer.LoadSchDocumentByPath(LibPkg_SchLib.DM_FullPath);

    GUIOperationLabel.Caption := 'Importing SchLib files ...';  { show operation we are doing in GUI }
    GUIOperationLabel.Update;  { update label in GUI }

    {*  Copy all parts from each SchLib into the main SchLib *}
    For i := 0 To SchLibImportFiles.Count - 1 Do     // iterate over each SchLib  File
    Begin
        { Show in GUI SchLib import progress }
        GUIFileLabel.Caption := 'Importing ' + ExtractFileName(SchLibImportFiles.Strings[i]) + ' ...' + IndentString(3) +
                               GetStringFromInteger(i+1) + ' of ' + GetStringFromInteger(SchLibImportFiles.Count); 
        GUIFileLabel.Update;
        GUIProgressBar.Position := ((i+1)/SchLibImportFiles.Count)*100; { set progress bar, based on num files remaining }
        GUIProgressBar.Update;         { update GUI progress bar position }

        ImportLibFilePath := SchLibImportFiles.Strings[i];    // get the filepath of the SchLib to import
        ImportLibDoc := SchServer.GetSchDocumentByPath(ImportLibFilePath);   // returns ISch_Doc of the SchLib to import
        If ImportLibDoc = Nil then
            ImportLibDoc := SchServer.LoadSchDocumentByPath(ImportLibFilePath);   // if not open then load it
        CopySchLibParts(ImportLibDoc, MainSchLibDoc);    // copy parts out of SchLib file into our main SchLib
        DeleteFile( ImportLibFilePath ); // delete the SchLib we just copied parts from (no longer need)
    End;
    GUIProgressBar.Position := 100; GUIProgressBar.Update;  { reset progress bar }

    { Remove Component_1 from main SchLib and save }
    DeleteSchComponent_1( MainSchLibDoc );          // delete default component(s) if exists
    MainSchLibDoc.GraphicallyInvalidate;            // Refresh the library
    ServerSchLibDoc.DoFileSave(cDocKind_SchLib);    // save the library
End;

Procedure ImportPcbLibFiles(MainPcbLib : IPcbLib, PcbLibImportFiles : TStringList);
Var
    i               : Integer; 
    ServerPcbLibDoc : IServerDocument;
    MainPcbLibDoc   : IPCB_Library;
    ImportPcbLibDoc : IPCB_Library;
    ImportFilePath  : String;
Begin
    { start PCB server if it hasnt been started already }
    if PcbServer = Nil then Client.StartServer('PCB');

    { Must open serverdoc to add footprints } 
    ServerPcbLibDoc := Client.OpenDocument(cDocKind_PcbLib, MainPcbLib.DM_FullPath); // opens the PcbLib in the server
    //Client.ShowDocument( ServerPcbLibDoc );  // displays the PcbLib 

    { returns IPCB_Library interface representing the PCB document of the path }
    MainPcbLibDoc := PcbServer.GetPCBLibraryByPath( MainPcbLib.DM_FullPath ); // load IPCB_Library from PcbLib path
    if MainPcbLibDoc = Nil then begin
       MainPcbLibDoc := PcbServer.LoadPCBLibraryByPath( MainPcbLib.DM_FullPath );
    end;

    GUIOperationLabel.Caption := 'Importing PcbLib Files...';  { show current operation in GUI }
    GUIOperationLabel.Update;  { update text label in GUI }

    {*  Import each PcbLib into the main LibPkg PcbLib *}
    For i := 0 To PcbLibImportFiles.Count - 1 Do Begin    // iterate over each PcbLib File
        ImportFilePath := PcbLibImportFiles.Strings[i];

        GUIFileLabel.Caption := ExtractFileName(ImportFilePath) + ' ... ' + GetStringFromInteger(i+1) + ' of ' + GetStringFromInteger(PcbLibImportFiles.Count); 
        GUIFileLabel.Update;   { show in GUI progress of importing PcbLibs }
        GUIProgressBar.Position := ((i+1)/PcbLibImportFiles.Count)*100; GUIProgressBar.Update; { update GUI progress bar }

        ImportPcbLibDoc := PcbServer.GetPCBLibraryByPath( ImportFilePath );    // returns IPCB_Library interface
        If ImportPcbLibDoc = Nil then
            ImportPcbLibDoc := PcbServer.LoadPCBLibraryByPath( ImportFilePath );   
        CopyPcbLibParts(ImportPcbLibDoc, MainPcbLibDoc);    // copy parts out of PcbLib file into our PcbLib (part of LibPkg)
        DeleteFile( ImportFilePath );     // delete the PcbLib we just copied footprints from (no longer need)
    End;

    DeletePcbComponent_1( MainPcbLibDoc );
    GUIProgressBar.Position := 100; GUIProgressBar.Update;  { reset progress bar }

    ProcessPcbFootprints(MainPcbLibDoc);    { standardize footprints }

    ServerPcbLibDoc.DoFileSave(cDocKind_PcbLib);
    MainPcbLibDoc.Board.ViewManager_FullUpdate;
    MainPcbLibDoc.Board.GraphicalView_ZoomRedraw;
    MainPcbLibDoc.RefreshView;
End;

Procedure CopySchLibParts(SourceSchLib : ISchLib, DestSchLib : ISchLib);
Var
    SchLibIterator  : ISch_Iterator;
    SchLibPart      : ISch_Component;
    SourceComp      : ISch_Component;
    SourceCompName  : WideString;
    DestCompName    : WideString;
    CopyComp        : ISch_Component;
Begin
    // Iterate through parts in SchLib and copy to Main_SchLib
    SchLibIterator := SourceSchLib.SchLibIterator_Create;
    SchLibIterator.SetState_FilterAll;
    SchLibIterator.AddFilter_ObjectSet(MkSet(eSchComponent));
    SourceComp := SchLibIterator.FirstSchObject;
    While (SourceComp <> Nil) Do
    Begin
        CopyComp := SourceComp.Replicate;
        SourceCompName := SourceComp.LibReference;
        DestCompName := GetUniqueSchComponentName(DestSchLib, SourceCompName); // make sure name doesnt already exist in destination lib
        CopyComp.LibReference := DestCompName;
        CopyComp.SetState_SourceLibraryName(ExtractFileName(DestSchLib.DocumentName));
        CopyComp.Librarypath := ExtractFilePath(DestSchLib.DocumentName);
        DestSchLib.AddSchComponent(CopyComp);

        SourceComp := SchLibIterator.NextSchObject; // forward to next schematic symbol 
    End;
    SourceSchLib.SchIterator_Destroy(SchLibIterator);
End;

Function GetNumSchLibParts(CurrentLib : ISchLib);
Var
    LibReader : ILibCompInfoReader;
    CompInfo  : IComponentInfo;
    FileName  : String;
Begin
    If CurrentLib = Nil Then Exit;

    { Check if CurrentLib is a Library document or not } 
    If CurrentLib.ObjectID <> eSchLib Then Exit;
         
    { Set up Library Component Reader object. } 
    FileName := CurrentLib.DocumentName;
    LibReader := SchServer.CreateLibCompInfoReader(FileName);
    If LibReader = Nil Then Exit;

    LibReader.ReadAllComponentInfo;

    { Get number of components in SchLib }
    Result := LibReader.NumComponentInfos;
End;

Function GetUniqueSchComponentName(SchLib : ISch_Lib, const CompName : String) : String;
Var
    NewCompName       : String;
    CompNameSubStr    : String;
    NumericalSubStr   : String;
    ExtractedNumber   : Integer;
    IndexOfUnderscore : Integer;
    unique_name_found : Boolean;
Begin
    NewCompName := CompName;
    unique_name_found := False;

    While not unique_name_found do
    Begin
        { If GetState_SchComponentByLibRef returns a value, then the component name exists already in the SchLib }
        If SchLib.GetState_SchComponentByLibRef(NewCompName) <> nil Then Begin
            If StringIsNumberedDuplicate(NewCompName) then Begin    // if of the form CompName_2
                IndexOfUnderscore := GetLastIndexOfChar(NewCompName, '_');
                NumericalSubStr := Copy(NewCompName, IndexOfUnderscore+1, Length(NewCompName));   // end of string, should be only digits [0-9]
                CompNameSubStr := Copy(NewCompName, 1, IndexOfUnderscore-1);   // get just the component name before the underscore
                ExtractedNumber := StrToInt(NumericalSubStr);   // parse string to integer
                ExtractedNumber := ExtractedNumber + 1;     // increment the component name by one
                NewCompName := CompNameSubStr + '_' + GetStringFromInteger(ExtractedNumber); // put new CompName string together
            End
            Else NewCompName := CompName + '_1';    // return first numbered duplicate name
        End
        Else unique_name_found := True;
    End;
    Result := NewCompName;
End;

{ Remove some useless parameters that UltraLibrarian and SnapEDA adds to schematic symbols, modify as you see fit }
Procedure FilterComponentParameters(Component : ISch_Component);
Var
    ParameterIterator  : ISch_Iterator;
    Param              : ISch_Parameter;
    ParamName          : String;
    ParamValue         : String;
    CharIndex          : Integer;
    JSON_key           : String;
    JSON_value         : String;
    Parameter_MfrPartNumber : ISch_Parameter;
Begin
    ParameterIterator := Component.SchIterator_Create;    { create another iterator, for component parameters }
    ParameterIterator.SetState_FilterAll;
    ParameterIterator.AddFilter_ObjectSet(MkSet(eParameter));   { iterate through only parameters }
    try
        Param := ParameterIterator.FirstSchObject;
        While Param <> Nil Do
        Begin
            Param.Name := Trim(Param.Name);   { trim whitespace }
            ParamName := Param.Name;  
            ParamValue := Param.Text;
            Param.IsHidden := True;     { make sure visibility is turned off by default }
            Case ParamName Of
            { Parameters where the parameter name is normalized }
                'Mfr_Name': Begin
                    Param.Name := 'Manufacturer';       { change to standard parameter name }               
                End;
                'MF': Begin
                    Param.Name := 'Manufacturer';       { change to more descriptive parameter name }
                End;                       
                'Manufacturer_Part_Number': Begin
                    //Continue;                         { no change }
                End;
                'MP': Begin
                    Param.Name := 'Manufacturer_Part_Number';   { change to more descriptive name }
                End;
                'PartNumber': Begin
                    Param.Name := 'Manufacturer_Part_Number';   { change to more descriptive name }
                End;
            { Parameters which are removed }
                'Alternate_PartNumber': Begin
                    Component.RemoveSchObject(Param);     { remove 'Alternate_PartNumber' parameter }
                End;
                'MountType': Begin
                    Component.RemoveSchObject(Param);     { remove 'MountType' parameter }
                End;
                'CompType': Begin
                    Component.RemoveSchObject(Param);     { remove 'CompType' parameter }
                End;
                'Copyright': Begin
                    Component.RemoveSchObject(Param);     { remove 'Copyright' parameter }
                End;
                'RefDes': Begin
                    Component.RemoveSchObject(Param);     { remove 'RefDes' parameter, Designator attribute does this }
                End;
                'Type': Begin
                    Component.RemoveSchObject(Param);     { remove 'Type' parameter }
                End;
                'Availability': Begin
                    Component.RemoveSchObject(Param);     { remove 'Availability' parameter, useless }
                End;
                'SnapEDA_Link': Begin
                    Component.RemoveSchObject(Param);     { remove 'SnapEDA_Link' parameter, useless }
                End;
                'Check_prices': Begin
                    Component.RemoveSchObject(Param);     { remove 'Check_prices' parameter, useless }
                End;
                'Value': Begin
                    Component.RemoveSchObject(Param);     { remove 'Value' parameter, which is added through DbLib }
                End;
                'Package': Begin
                    Component.RemoveSchObject(Param);     { remove 'Package' parameter, which is added through DbLib }
                End;
                'TYPE': Begin
                    Component.RemoveSchObject(Param);     { remove 'TYPE' parameter }
                End;
                'PackageReference': Begin
                    Component.RemoveSchObject(Param);     { remove 'PackageReference' parameter }
                End;
                'Application_BuildNumber': Begin
                    Component.RemoveSchObject(Param);     { remove 'Application_BuildNumber' }
                End;
                'BuyPart': Begin
                    Component.RemoveSchObject(Param);     { remove 'BuyPart' parameter }
                End;
                'RoHS': Begin
                    Component.RemoveSchObject(Param);     { remove 'RoHS' parameter }
                End;
                'Published': Begin
                    Component.RemoveSchObject(Param);     { remove 'Published' parameter }
                End;
                'DatasheetVersion': Begin
                    Component.RemoveSchObject(Param);     { remove 'DatasheetVersion' parameter }
                End;
                'PackageVersion': Begin
                    Component.RemoveSchObject(Param);     { remove 'PackageVersion' parameter }
                End;
                'Publisher': Begin
                    Component.RemoveSchObject(Param);     { remove 'Publisher' parameter }
                End;
                'Supplier': Begin
                    Component.RemoveSchObject(Param);     { remove 'Supplier' parameter }
                End;
                'SUPPLIER_2': Begin
                    Component.RemoveSchObject(Param);     { remove 'SUPPLIER_2' parameter }
                End;             
                'PackageTopMarking': Begin
                    Component.RemoveSchObject(Param);     { remove 'PackageTopMarking' parameter }
                End;     
                'PowerWise': Begin
                    Component.RemoveSchObject(Param);     { remove 'PowerWise' parameter }
                End;             
                'Comment' : Begin
                    if Param.Text = '*' then
                        Param.Text := '';
                End;
                '': Continue;
            End; // end case statement

            { remove parameters which do not have a value assigned }
            if ((Param.Name <> 'Comment') and (Param.Name <> 'Manufacturer') and (Length(Param.Text) = 0)) then begin
                Component.RemoveSchObject(Param);
            end;
            
            { get rid of purchase crap }
            if ((StringContains(ParamName, 'Purchase-URL') or StringContains(ParamName, 'Purchase_URL')) or StringContains(ParamName, 'Price')) then begin
                Component.RemoveSchObject(Param);
            end;
                
            if (StringContains(ParamName, 'Version') or StringContains(ParamName, 'LatestRevision')) then begin
                Component.RemoveSchObject(Param);
            end;

            { occasionally components will have up to 10 Digikey links: Digikey_1, Digikey_2, Digikey_3, etc. remove additional ones }
            if ((StringContains(ParamName, 'Digi-Key') or (StringContains(ParamName, 'Digikey'))) and StringIsNumberedDuplicate(ParamName)) then begin
                Component.RemoveSchObject(Param);
            end;   

            // handles case where ParamValue is JSON encoded, for some reason SnapEda does this occasionally
            // check if ParamValue begins with '{' and ends with '}' and contains ':', which is JSON. for example:
            {'manufacturer': u'ISSI, Integrated Silicon Solution Inc'}
            if (StringBeginsWith(ParamValue, '{') and StringEndsWith(ParamValue, '}') and StringContains(ParamValue, ':')) then begin
                CharIndex := Pos(':', ParamValue);      // get position of colon
                JSON_key := Copy(ParamValue, 1, CharIndex - 1); // returns: 'manufacturer'
                JSON_key := GetBetween(JSON_key, '''', '''');   // gets between quotes, returns: manufacturer
                if StringsEqual(JSON_key, 'manufacturer') then begin
                    JSON_key := 'Manufacturer';
                end;
                
                JSON_value := Copy(ParamValue, CharIndex + 1, Length(ParamValue)); // gets after colon, returns: u'ISSI, Integrated Silicon Solution Inc'
                JSON_value := GetBetween(JSON_value, '''', '''');   // gets between quotes, returns: ISSI, Integrated Silicon Solution Inc

                Param.Name := JSON_key;     // update parameter name and value
                Param.Text := JSON_value;   
            end;

            Param := ParameterIterator.NextSchObject;   // get next parameter
        End;
    Finally
        Component.SchIterator_Destroy(ParameterIterator);
    End;

    { If parameter Manufacturer_Part_Number doesnt exist yet, create it, if exists set color and font }
    Parameter_MfrPartNumber := GetParameter_Manufacturer_Part_Number(Component);    
    if Parameter_MfrPartNumber = Nil then begin
        Parameter_MfrPartNumber := SchServer.SchObjectFactory(eParameter, eCreate_Default);
        Parameter_MfrPartNumber.Name := 'Manufacturer_Part_Number';
        Parameter_MfrPartNumber.Text := Component.LibReference; // value is the name of the component
        Parameter_MfrPartNumber.Orientation := eRotate0;
        Parameter_MfrPartNumber.FontID := SY_GetFontId(DEFAULT_FONT, DEFAULT_FONT_SIZE);
        Parameter_MfrPartNumber.Justification := eJustify_BottomLeft;
        Parameter_MfrPartNumber.Color := COLOR_TEXT;
        Parameter_MfrPartNumber.IsHidden := True;  // Turn Off Visibility
        Parameter_MfrPartNumber.OwnerPartId := 1;
        Component.AddSchObject(Parameter_MfrPartNumber);    // add the newly created parameter
    end else begin
        Parameter_MfrPartNumber.FontID := SY_GetFontId(DEFAULT_FONT, DEFAULT_FONT_SIZE);
        Parameter_MfrPartNumber.Color := COLOR_TEXT;
        Parameter_MfrPartNumber.Orientation := eRotate0;
        Parameter_MfrPartNumber.Justification := eJustify_BottomLeft;
    end;
End;

Function GetParameter_Manufacturer_Part_Number(Component : ISch_Component) : ISch_Parameter;
Var
    ParameterIterator  : ISch_Iterator;
    Param              : ISch_Parameter;
Begin
    Result := Nil;
    ParameterIterator := Component.SchIterator_Create;    { create another iterator, for component parameters }
    ParameterIterator.SetState_FilterAll;
    ParameterIterator.AddFilter_ObjectSet(MkSet(eParameter));   { iterate through only parameters }
    try
        Param := ParameterIterator.FirstSchObject;
        While Param <> Nil Do
        Begin
            if StringsEqual(Param.Name, 'Manufacturer_Part_Number') then begin
                Result := Param;
                Exit;
            end;
            Param := ParameterIterator.NextSchObject;   // get next parameter
        End;
    Finally
        Component.SchIterator_Destroy(ParameterIterator);
    End;
End;

{ Filter thickness of Arc linewidth relative to the radius of the Arcs, sometimes its way too thick }
Procedure FilterComponentArcSize(Component : ISch_Component);
Var
    ArcIterator     : ISch_Iterator;
    Arc             : ISch_Parameter;
    ArcRadius : Integer; 
    ArcLineWidth : Integer;
Begin
    ArcIterator := Component.SchIterator_Create;    { create another iterator, for component parameters }
    ArcIterator.SetState_FilterAll;
    ArcIterator.AddFilter_ObjectSet(MkSet(eArc));   { iterate over only Arcs }  
    try
        Arc := ArcIterator.FirstSchObject;
        While Arc <> Nil Do
        Begin
            // Arc.Radius;   { TDistance (Integer) }
            ArcRadius := CoordToMils( Arc.Radius );
            ArcLineWidth := Arc.LineWidth; { TSize: eSmall, eMedium, eLarge }

            { filter Arc size so we dont have massive filled circles in schem components }
            { sometimes UltraLibrarian makes way too big of filled circles in Sch components }
            if (Arc.LineWidth = eLarge) and (ArcRadius <= 5) then begin
                Arc.LineWidth := eMedium;
                Arc.Radius := MilsToCoord( 2 );
            end;
            Arc := ArcIterator.NextSchObject;
        End;    // end while
    Finally
        Component.SchIterator_Destroy(ArcIterator);
    End; // end try
End;

{ sets component body colors, linewidth, and sets body to transparent }
Procedure FixComponentRectangles(Component ISch_Component);
Var
    RectIterator : ISch_Iterator;
    Rect         : ISch_Rectangle;
Begin
    RectIterator := Component.SchIterator_Create;
    RectIterator.AddFilter_ObjectSet(MkSet(eRectangle));
    try
        Rect := RectIterator.FirstSchObject;
        While Rect <> Nil Do
        Begin
            Rect.LineWidth := eSmall;                   // set outline linewidth, ISch_Rectangle Interface property        
            Rect.Color     := COLOR_SYMBOL_OUTLINE;     // ISch_GraphicalObject interface property
            Rect.AreaColor := COLOR_SYMBOL_BODY_FILL;   // ISch_GraphicalObject interface property
            Rect.IsSolid   := True;                     // set filled rectangle, ISch_Rectangle Interface property
            Rect.Transparent := True;                   // set to transparent mode, ISch_Rectangle Interface property
            
            Rect := RectIterator.NextSchObject;         // get next rectangle
        End;
    Finally
        Component.SchIterator_Destroy(RectIterator);
    End;
End;

{ returns rectangle if all pins in sch symbol are on it, works for multipart sch symbols }
function GetSymbolBodyRectangle(Component : ISchComponent, PartNum : Integer) : ISch_Rectangle;
Var
    RectIterator  : ISch_Iterator;
    Rect          : ISch_Rectangle;
    SymbolPinList : TList;
    Pin           : ISch_Pin;
    IsSymbolBody  : Boolean;
    RectX1        : Integer;
    RectY1        : Integer;
    RectX2        : Integer;
    RectY2        : Integer;
    PinX          : Integer;
    PinY          : Integer;
    i             : Integer;
Begin
    Result := Nil;
    SymbolPinList := GetAllComponentPins(Component);

    RectIterator := Component.SchIterator_Create;
    RectIterator.AddFilter_ObjectSet(MkSet(eRectangle));
    try
        Rect := RectIterator.FirstSchObject;
        While Rect <> Nil Do
        Begin
            if Rect.OwnerPartId <> PartNum then begin  { only get symbol body for specific part in multipart symbols }
                Rect := RectIterator.NextSchObject;  // get next rectangle
                continue;
            end;
            
            { rectangle begin }
            IsSymbolBody := True;
            RectX1 := Rect.Location.X;
            RectY1 := Rect.Location.Y;
            RectX2 := Rect.Corner.X;
            RectY2 := Rect.Corner.Y;


            { determine if all pins are on the rectangle }
            for i:= 0 to SymbolPinList.Count-1 do begin
                Pin  := SymbolPinList.Items[i];
                if Pin.OwnerPartId = PartNum then begin
                    PinX := Pin.Location.X;
                    PinY := Pin.Location.Y;

                    { if pin is horizontal, PinX must be on the left side or right side of the rectangle}
                    if ((Pin.Orientation = eRotate0) or (Pin.Orientation = eRotate180)) then begin
                        if not ((PinX = RectX1) or (PinX = RectX2)) then begin
                            IsSymbolBody := False;
                            break;
                        end;
                    end;
                    { if pin is vertical, PinY must be on the top or bottom of the rectangle }
                    if ((Pin.Orientation = eRotate90) or (Pin.Orientation = eRotate270)) then begin
                        if not ((PinY = RectY1) or (PinY = RectY2)) then begin
                            IsSymbolBody := False;
                            break;
                        end;
                    end;
                end;
            end;

            if IsSymbolBody = True then
                Result := Rect;

            { rectangle end }
            Rect := RectIterator.NextSchObject;  // get next rectangle
        End;
    Finally
        Component.SchIterator_Destroy(RectIterator);
    End;
End;

{   Changes active low name from UltraLibrarian/SnapEDA format to Altium format,
    so a bar is put over the pin name. Tries to handle all conventions UL/SnapEDA uses. 
        *SYNC   -> S\Y\N\C\
        *RESET  -> R\E\S\E\T\
        *OUT11 (OUT11B) ->  O\U\T\1\1\ \(\O\U\T\1\1\B\)\
        IOUT*   ->  I\O\U\T\
        CS*     ->  C\S\
        REFCLK* ->  R\E\F\C\L\K\
        *OE     ->  O\E\
        E*      ->  E\
        *LDAC   ->  L\D\A\C\
        *G      ->  G\
        RST#    ->  R\S\T\
        ~CS     -> C\S\
        *PWRDN  ->  P\W\R\D\N\  }
Function GetActiveLowPinName(PinName : String) : String;
Var
    i : Integer;
Begin
    PinName := Trim( PinName );   // trim whitespace
    Result := PinName;

    {  '*' is used as indicator for ActiveLow by UltraLibrarian ie '*REFIN', 'IOUT*' }
    if StringBeginsWith(PinName, '*') or StringEndsWith(PinName, '*') then begin { '*CS' or 'CS*' }
        PinName := ReplaceCharInString(PinName, '*', '');    { delete '*', returns 'CS' }
        Result := '';
        for i := 1 to Length(PinName) do begin
            Result := Result + PinName[i] + '\';  { add '\' after every letter: 'C\S\'}
        end;
        Exit;
    end;

    {  '#' is used as indicator for ActiveLow by SnapEDA ie 'RST#' }
    if StringEndsWith(PinName, '#') then begin 
        PinName := ReplaceCharInString(PinName, '#', '');    { delete '#' }
        Result := '';
        for i := 1 to Length(PinName) do begin
            Result := Result + PinName[i] + '\';  { add '\' after every letter: 'R\S\T\'}
        end;
        Exit;
    end;

    {  '~' is used as indicator for ActiveLow by SnapEDA ie '~REFIN', '~CS' }
    if StringBeginsWith(PinName, '~') then begin { '~CS' }
        PinName := ReplaceCharInString(PinName, '~', '');    { delete '~' }
        Result := '';
        for i := 1 to Length(PinName) do begin
            Result := Result + PinName[i] + '\';  { add '\' after every letter: 'C\S\'}
        end;
        Exit;
    end;

    {  '!' is used as indicator for ActiveLow by SnapEDA ie '!REFIN', '!CS' }
    if StringBeginsWith(PinName, '!') then begin { '!CS' }
        PinName := ReplaceCharInString(PinName, '!', '');    { delete '!' }
        Result := '';
        for i := 1 to Length(PinName) do begin
            Result := Result + PinName[i] + '\';  { add '\' after every letter: 'C\S\'}
        end;
        Exit;
    end;
    
    { Special Cases }
    if (StringsEqual(PinName, 'RESET_N') or StringsEqual(PinName, 'NRESET')) then begin
        Result := 'R\E\S\E\T\';
        Exit;
    end;
    if (StringsEqual(PinName, 'RESETB') or StringsEqual(PinName, 'RESETN')) then begin
        Result := 'R\E\S\E\T\';
        Exit;
    end;
    if (StringsEqual(PinName, 'NRST') or StringsEqual(PinName, 'RSTN') or StringsEqual(PinName, 'RSTB') or StringsEqual(PinName, 'RST_N')) then begin
        Result := 'R\S\T\';
        Exit;
    end;
    if StringsEqual(PinName, 'INT_N') then begin
        Result := 'I\N\T\';
        Exit;
    end;
    if StringsEqual(PinName, 'IRQ_N') then begin
        Result := 'I\R\Q\';
        Exit;
    end;
    if (StringsEqual(PinName, 'POR_B') or StringsEqual(PinName, 'PORn')) then begin
        Result := 'P\O\R\';
        Exit;
    end;
    if StringsEqual(PinName, 'PMIC_RST_B') then begin
        Result := 'P\M\I\C\_\R\S\T\';
        Exit;
    end;
    if StringsEqual(PinName, 'WDOG_B') then begin
        Result := 'W\D\O\G\';
        Exit;
    end;
    if StringsEqual(PinName, 'SUSPENDB') then begin
        Result := 'S\U\S\P\E\N\D\';
        Exit;
    end;
    if StringsEqual(PinName, 'STANDBY_BAR') then begin
        Result := 'S\T\A\N\D\B\Y\';
        Exit;
    end;
    if (StringsEqual(PinName, 'NTRST') or StringsEqual(PinName, 'TRST_BAR') or StringsEqual(PinName, 'TRST_N') or StringsEqual(PinName, 'TRSTn')) then begin
        Result := 'T\R\S\T\';
        Exit;
    end;
    if StringsEqual(PinName, 'RTC_RESET_B') then begin
        Result := 'R\T\C\_\R\E\S\E\T\';
        Exit;
    end;
    if StringsEqual(PinName, 'OE_BAR') then begin
        Result := 'O\E\';
        Exit;
    end;
    if StringsEqual(PinName, 'OUTPUT_ENABLE_N') then begin
        Result := 'O\U\T\P\U\T\_\E\N\A\B\L\E\';
        Exit;
    end;
    if StringsEqual(PinName, 'ERROR_N') then begin
        Result := 'E\R\R\O\R\';
        Exit;
    end;
    if StringsEqual(PinName, 'FAULT_N') then begin
        Result := 'F\A\U\L\T\';
        Exit;
    end;

    if StringsEqual(PinName, 'ALERT_N') then begin
        Result := 'A\L\E\R\T\';
        Exit;
    end;
    if StringsEqual(PinName, 'SS_N') then begin
        Result := 'S\S\';
        Exit;
    end;
    if StringsEqual(PinName, 'CS_N') then begin
        Result := 'C\S\';
        Exit;
    end;
    if StringsEqual(PinName, 'SD_N') then begin
        Result := 'S\D\';
        Exit;
    end;
    if StringsEqual(PinName, 'PD_N') then begin
        Result := 'P\D\';
        Exit;
    end;    
End;

{   Splits PinName by '/' delimiter, and returns a string list, so can process individual active low names
    handles the case of not splitting 'I/O'. Does not split on whitespace. some examples:
        'OSC*/REFCLK*'  ->  'OSC*', 'REFCLK*'
        'OSC/REFCLK'    ->  'OSC', 'REFCLK'
        'CAP/2.5V'      ->  'CAP', '2.5V'
        'SYNC_IN/STATUS'->  'SYNC_IN', 'STATUS'
        'SCLK/SCL'      ->  'SCLK', 'SCL'
        '*SYNC'         ->  '*SYNC'
        '*OUT11 (OUT11)'-> '*OUT11 (OUT11)'    
        'DVDD_I/O'      ->  'DVDD_I/O'
        'I/O_UPDATE'    ->  'I/O_UPDATE'
        'TCLK_R/*F'     ->  'TCLK_R', '*F'  }
Function SplitPinNameByForwardSlash(PinName: string) : TStringList;
var
  SplitList  : TStringList;
  SubStrList : TStringList;
  NextStr    : String;
  SubStr     : String;
  i          : Integer;
begin
    SplitList := TStringList.Create;
    SubStrList := TStringList.Create;

    PinName := Trim( PinName );   // trim beginning and ending whitespace
    try
        SplitList.StrictDelimiter := true;
        SplitList.Delimiter := '/';
        SplitList.DelimitedText := PinName;
    finally
    end;

    for i:= 0 to SplitList.Count-1 do begin // handles special case of not splitting I/O
        SubStr := SplitList.Strings[i];
        if i < SplitList.Count-1 then begin
            NextStr := SplitList.Strings[i+1];
            if (StringEndsWith(SubStr, 'I') and StringBeginsWith(NextStr, 'O')) then begin
                SubStr := SubStr + '/' + NextStr;
                Inc(i);
            end;
        end;
        SubStrList.Add(SubStr);
    end;
    Result := SubStrList;
end;

{   Normalizes some pin names to standard names, removes numbered NC and GND pins
        D.N.C.  -> NC
        N.C.    -> NC
        NC_2    -> NC
        NC_15   -> NC
        NC_28   -> NC
        GND_2   -> GND
        GND_3   -> GND
        GND_4   -> GND  }
Procedure SetNormalizedPinName(Pin : ISch_Pin);
Var
    PinName : String;
    i       : Integer;
Begin
    PinName := Trim(Pin.Name);   // trim whitespace
    
    Case PinName Of
    { specific cases to fix pin names }
        'D.N.C.': Begin
            Pin.Name := 'NC';       { change to standard No Connect name }         
            Exit;      
        End;
        'DNC': Begin
            Pin.Name := 'NC';       { change to standard No Connect name }         
            Exit;      
        End;
        'N.C.': Begin
            Pin.Name := 'NC';       { change to standard No Connect name }  
            Exit;
        End;           
        'N/C': Begin
            Pin.Name := 'NC';       { change to standard No Connect name }  
            Exit;
        End;                
        'n.c.': Begin
            Pin.Name := 'NC';       { change to standard No Connect name }  
            Exit;
        End;        
    End; // end case statement

    { handle NC_2, NC_28, NC_15, etc }
    if (StringBeginsWith(PinName, 'NC_') and StringIsNumberedDuplicate(PinName)) then begin
        Pin.Name := 'NC';
        Exit;
    end;

    { handle GND_2, GND_3, GND_4, GND_5, etc.  }
    if (StringBeginsWith(PinName, 'GND_') and StringIsNumberedDuplicate(PinName)) then begin
        Pin.Name := 'GND';
        Exit;
    end;
    { handle VDD_2, VDD_3, VDD_4, VDD_5, etc.  }
    if (StringBeginsWith(PinName, 'VDD_') and StringIsNumberedDuplicate(PinName)) then begin
        Pin.Name := 'VDD';
        Exit;
    end;
End;

{ If is an active low name, transforms it into Altium's format so there's an overbar }
Procedure SetActiveLowPinName(Pin : ISch_Pin);
Var
    SubStrList : TStringList;
    SubStr     : String;
    CombinedPinName : String;
    i : Integer;
Begin
    CombinedPinName := '';
    SubStrList := SplitPinNameByForwardSlash(Pin.Name); { split pin name by '/' into stringlist }

    { process active low name for each substring name, then combine substrings into one string of pin name }
    for i := 0 to SubStrList.Count-1 do begin   
        if i = 0 then begin
            SubStr := SubStrList.Strings[i];
            CombinedPinName := GetActiveLowPinName(SubStr);
            continue
        end
        else begin
            SubStr := SubStrList.Strings[i];
            CombinedPinName := CombinedPinName + '/' + GetActiveLowPinName(SubStr);
        end;
    end;
    Pin.Name := CombinedPinName;
End;

{ changes pin name and designator to constants defined }
Procedure SetPinFont(Pin : ISch_Pin);
Begin
    Pin.Designator_FontMode := CUSTOM_SETTING;  // allows for setting custom font
    Pin.Designator_CustomFontID := SY_GetFontId(PIN_FONT_NAME, PIN_FONT_SIZE);
    Pin.Name_FontMode := CUSTOM_SETTING;        // allows for setting custom font
    Pin.Name_CustomFontID := SY_GetFontId(PIN_FONT_NAME, PIN_FONT_SIZE);
End;

function GetAllComponentPins(Component : ISch_Component) : TList;
var
    Pin          : ISch_Pin;
    PinIterator  : ISch_Iterator;
begin
    Result := TList.Create;
    PinIterator := Component.SchIterator_Create;
    PinIterator.AddFilter_ObjectSet(MkSet(ePin));

    Pin := PinIterator.FirstSchObject;
    while Pin <> Nil Do
    begin
        Result.Add(Pin);
        Pin := PinIterator.NextSchObject;
    end;
    Component.SchIterator_Destroy(PinIterator);
end;

function GetTopLeftPin(Component : ISch_Component, PartNum : Integer) : ISch_Pin;
Var
    Pin          : ISch_Pin;
    PinIterator  : ISch_Iterator;

    TopLeftPin : ISch_Pin;
    CurPinX    : Integer;
    CurPinY    : Integer;
    TopLeftX   : Integer;
    TopLeftY   : Integer;
Begin
    Result := Nil;
    PinIterator := Component.SchIterator_Create;
    PinIterator.AddFilter_ObjectSet(MkSet(ePin));
    Pin := PinIterator.FirstSchObject;
    while Pin <> Nil Do begin 
        if Pin.OwnerPartId =  PartNum then begin  { only iterate over pins of the same part(PartA, PartB, etc.), in multipart symbols }
            CurPinX := CoordToMils(Pin.Location.X);
            CurPinY := CoordToMils(Pin.Location.Y);

            if Result = Nil then begin
                Result := Pin;
                TopLeftX := CurPinX;
                TopLeftY := CurPinY;
            end;
            if CurPinX < TopLeftX then begin
                Result := Pin;
                TopLeftX := CurPinX;
                TopLeftY := CurPinY;
            end;
            if ((CurPinX = TopLeftX) and (CurPinY > TopLeftY)) then begin
                Result := Pin;
                TopLeftX := CurPinX;
                TopLeftY := CurPinY;
            end;
        end;
        Pin := PinIterator.NextSchObject;   // get next component pin
    end;
    Component.SchIterator_Destroy(PinIterator);
End;

procedure SetPinLength(Pin : ISch_Pin, MaxPinLength : Integer);
begin
    if CoordToMils(Pin.PinLength) > MaxPinLength then
        Pin.PinLength := MilsToCoord(MaxPinLength);
end;

function GetPinDesignatorMaxNumChars(Component : ISch_Component) : Integer;
Var
    Pin          : ISch_Pin;
    PinIterator  : ISch_Iterator;
Begin
    Result := 0;

    PinIterator := Component.SchIterator_Create;
    PinIterator.AddFilter_ObjectSet(MkSet(ePin));
    Pin := PinIterator.FirstSchObject;
    while Pin <> Nil Do begin  
        if Length(Pin.Designator) > Result then
            Result := Length(Pin.Designator);   // get num chars

        Pin := PinIterator.NextSchObject;   // get next component pin
    end;
    Component.SchIterator_Destroy(PinIterator);
End;

{ For all components in SchLib: Set Pin Font, set active low pin name if it is an active low pin,
  change pin length from 300mil to 200mil, if pin name isnt too long  }
Procedure ProcessComponentPins(Component : ISch_Component);
Var
    Pin          : ISch_Pin;
    PinIterator  : ISch_Iterator;

    PinDesignatorMaxNumChars : Integer; // max num chars for the designator for all pins
    MaxPinLength : Integer;             // max length for pins based on max char length of pin names
Begin
    { calculate the max pin length, UltralIbrarian makes pins 300mils when they should be 200 mil  }
    PinDesignatorMaxNumChars := GetPinDesignatorMaxNumChars(Component); // max number of characters that makes up any single pin name
    MaxPinLength := PinDesignatorMaxNumChars*100; // budget 100mils per char
    if MaxPinLength < 200 then
        MaxPinLength := 200;

    PinIterator := Component.SchIterator_Create;
    PinIterator.AddFilter_ObjectSet(MkSet(ePin));
    Pin := PinIterator.FirstSchObject;
    while Pin <> Nil Do begin  
        SetPinFont(Pin);                    // Sets font of name and designator
        SetNormalizedPinName(Pin);          // Fix things like: N.C.->NC, GND_3->GND
        SetActiveLowPinName(Pin);           // if pin is active low, set overbar
        SetPinLength(Pin, MaxPinLength);

        Pin := PinIterator.NextSchObject;   // get next component pin
    end;
    Component.SchIterator_Destroy(PinIterator);
End;

{ move each individual part in multipart schematic symbols. Moves all the components of the part, i.e. move PartB }
Procedure MoveComponentByXY(Component : ISch_Component, PartNum : Integer, MoveX : Integer, MoveY : Integer);
Var
  obj            : ISch_GraphicalObject;
  ObjectIterator : ISch_Iterator;
Begin
    { iterate over all objects in the schematic symbol, move only objects which of the specific part in multipart symbols }
    ObjectIterator := Component.SchIterator_Create();
    { filter to move only the following object types }
    ObjectIterator.AddFilter_ObjectSet(MkSet(eRectangle,eLine,eArc,eEllipticalArc,eRoundRectangle,ePie,eEllipse,ePolygon,ePolyline,eBezier,eLabel,ePin));
    obj := ObjectIterator.FirstSchObject();
    while (obj <> nil) do
    begin
        if obj.OwnerPartId = PartNum then begin
            obj.MoveByXY(MoveX, MoveY);
        end;
        obj := ObjectIterator.NextSchObject();
    end;
    Component.SchIterator_Destroy(ObjectIterator);
End;

{ move symbol body such that its top left pin (usually pin1) is at the origin }
Procedure MoveComponentToOrigin(Component : ISch_Component);
Var
    Pin1    : ISch_Pin;
    DeltaX  : Integer;
    DeltaY  : Integer;
    partnum : Integer;
Begin
    for partnum := 1 to Component.PartCount do begin    { handle each part of multipart symbols separately }
        Pin1 := GetTopLeftPin(Component, partnum);    { get pin1 for each part of multipart sch symbol }
        if Pin1 = Nil then Exit;

        { calculate what movement(in mils) you would need to put Pin1 connect point at the origin }
        Case Pin1.Orientation Of
            eRotate0: Begin    { pin horizontal, on right side of symbol }
                { ConnectPointX = Pin.Location.X + pin length }
                { ConnectPointY = Pin.Location.Y }
                DeltaX := 0 - CoordToMils(Pin1.Location.X) - CoordToMils(Pin1.PinLength);
                DeltaY := 0 - CoordToMils(Pin1.Location.Y);
            End;  
            eRotate90: Begin   { pin vertical, on top of symbol }
                { ConnectPointX = Pin.Location.X  }
                { ConnectPointY = Pin.Location.Y - pin length  }
                DeltaX := 0 - CoordToMils(Pin1.Location.X);
                DeltaY := 0 - CoordToMils(Pin1.Location.Y) - CoordToMils(Pin1.PinLength);
            End;    
            eRotate180: Begin  { pin horizontal, on left side of symbol }
                { ConnectPointX = Pin.Location.X - pin length  }
                { ConnectPointY = pin length + Pin.Location.Y  }
                DeltaX := 0 - CoordToMils(Pin1.Location.X) + CoordToMils(Pin1.PinLength);
                DeltaY := 0 - CoordToMils(Pin1.Location.Y);
            End;
            eRotate270: Begin  { pin vertical, on bottom side of symbol }
                { ConnectPointX = Pin.Location.X  }
                { ConnectPointY = Pin.Location.Y - pin length  }
                DeltaX := 0 - CoordToMils(Pin1.Location.X);
                DeltaY := 0 - CoordToMils(Pin1.Location.Y) + CoordToMils(Pin1.PinLength);
            End;
        End;     
        MoveComponentByXY(Component, partnum, MilsToCoord(DeltaX), MilsToCoord(DeltaY));
    end;
End;

Procedure SetComponentTextLabelFont(Component : ISch_Component);
Var
  TextLabel         : ISch_GraphicalObject;
  TextIterator      : ISch_Iterator;
Begin
    TextIterator := Component.SchIterator_Create;   // enumerate child objects of this symbol
    TextIterator.AddFilter_ObjectSet(MkSet(eLabel));    // only iterate text labels
    Try
        TextLabel := TextIterator.FirstSchObject;
        While TextLabel <> Nil Do
        Begin
            TextLabel.FontID := SY_GetFontId(DEFAULT_FONT, DEFAULT_FONT_SIZE);  // set text label Font
            TextLabel := TextIterator.NextSchObject;    // get next text label
        End;

    Finally
        Component.SchIterator_Destroy(TextIterator);
    End;
End;

{ turn on visibility for manufacturer part number and place at bottom left corner of symbol body rectangle }
Procedure ShowManufacturerPartNumber(Component : ISch_Component);
Var
    ParamMfrPartNum     : ISch_Parameter;
    SymbolBodyRectangle : ISch_Rectangle;
    BottomLeftCorner    : TLocation;
    LocX    : Integer;
    LocY    : Integer;
    partnum : Integer; // for multipart schematic symbols
    txt     : ISch_Label;   // text object of manufacturer_part_number
Begin
    ParamMfrPartNum := GetParameter_Manufacturer_Part_Number(Component);
    if ParamMfrPartNum <> Nil then begin
        for partnum := 1 to Component.PartCount do begin    { handle each part of multipart symbols separately }
            SymbolBodyRectangle := GetSymbolBodyRectangle(Component, partnum);
            if SymbolBodyRectangle <> Nil then begin
                LocX := SymbolBodyRectangle.Location.X;
                LocY := SymbolBodyRectangle.Location.Y - MilsToCoord(100);

                { create text object }
                txt := SchServer.SchObjectFactory(eLabel, eCreate_Default);
                txt.Location := Point(LocX, LocY);
                txt.FontID := SY_GetFontId(DEFAULT_FONT, DEFAULT_FONT_SIZE);
                txt.Orientation := eRotate0;
                txt.Color := COLOR_TEXT;
                txt.Text := ParamMfrPartNum.Text;
                txt.OwnerPartId := partnum;
                txt.Justification := eJustify_BottomLeft;
                Component.AddSchObject(txt);    // add the newly created text object to schematic component
            end;
        end;
    end;
    Component.GraphicallyInvalidate();
End;

{ SnapEDA schematic symbols (IntLibs) map the footprint to an absolute filepath, which is where the
  IntLib was created and no longer exists. For instance:
    C:\Users\Natasha\snapeda-altium\conversion_dir\SnapEDApcb.PcbLib
    C:\altium-converter\conversion_dir\Temp\SOP65P640X120-14N.PcbLib
  This method removes the absolute filepath so Altium will look in current PcbLib for the footprint.
  Also this method refreshes footprint mappings for all schematic symbols, which makes the
  footprint preview visible in the sch symbol. Otherwise need to close the SchLib and reopen 
  to see footprint previews.}
Procedure RefreshFootprintMappings(Component : ISch_Component);
Var
    ObjIterator : ISch_Iterator;
    obj         : ISch_Object;
    i           : Integer;
Begin
    ObjIterator := Component.SchIterator_Create();
    obj := ObjIterator.FirstSchObject();
    while (obj <> nil) do   { iterate over all component objects to find ISch_Implementation }
    begin
        if obj.ObjectID = eImplementation then begin
            For i := 0 To obj.DatafileLinkCount - 1 Do begin    { iterate over all DataFileLinks }
                obj.DatafileLink[i].Location := '';     { remove absolute filepath mapping }
            End;   
        End; 
        obj := ObjIterator.NextSchObject();
    end;
    Component.SchIterator_Destroy(ObjIterator);
    Component.ValidateImplementationsCurrentState();    { refreshes footprint mapping, so footprint is visible in preview }
End;

{     Processes Symbols in SchLib:
        sets the pin font and size to constants defined below:  PIN_FONT_NAME, PIN_FONT_SIZE
        sets active low pins so an overbar is in schematic symbol on pin name
        filters symbol parameters, deletes many unecessary params, see 'FilterComponentParameters' method to edit
        sets symbol body to color defined in constant  COLOR_SYMBOL_BODY_FILL
        sets symbol body outline to color defined in constant COLOR_SYMBOL_OUTLINE
        sets all text label objects to constant: DEFAULT_FONT   
        moves the top left pin(pin1) in the schematc symbol to the origin, works on multipart symbols
        displays the manufacturer part number at the bottom left of schematic symbol }
Procedure ProcessSchLibComponents(SchLib : ISch_Lib);
Var
    LibraryIterator    : ISch_Iterator;
    Component          : ISch_Component;
    SymbolBody         : ISch_Rect;
    NumComponents      : Integer;
    i                  : Integer;
Begin
    if APPLY_FONTS_AND_COLORS_TO_SYMBOLS = False then
        Exit;

    NumComponents := GetNumSchLibParts(SchLib);
    i := 0;

    { get SchLib iterator to traverse the SchLib }
    LibraryIterator := SchLib.SchLibIterator_Create;
    LibraryIterator.SetState_FilterAll;
    LibraryIterator.AddFilter_ObjectSet(MkSet(eSchComponent));  
    Try
        { get first component in the SchLib }
        Component := LibraryIterator.FirstSchObject;
        While Component <> Nil Do
        Begin
        { component start }
            GUIFileLabel.Caption := 'Processing ' + Component.LibReference + '... ' + GetStringFromInteger(i+1) + ' of ' + GetStringFromInteger(NumComponents); 
            GUIFileLabel.Update;   

            if StringBeginsWith(Component.LibReference, '*SYMBOL_GRAPHICS*') then begin   { do not modify graphics lib }
                Inc(i);
                Component := LibraryIterator.NextSchObject; 
                continue;
            end;

            ProcessComponentPins(Component);         
            FilterComponentParameters(Component);     
            FixComponentRectangles(Component);
            FilterComponentArcSize(Component);           
            SetComponentTextLabelFont(Component);            
            MoveComponentToOrigin(Component);
            if SHOW_MANUFACTURER_PART_NUMBER = True then
                ShowManufacturerPartNumber(Component);       
            RefreshFootprintMappings(Component);     

            Inc(i);
            GUIProgressBar.Position := ((i+1)/NumComponents)*100; GUIProgressBar.Update; { update GUI progress bar }
        { component end }
            Component := LibraryIterator.NextSchObject;   { get next symbol in SchLib }
        End;
    Finally
        SchLib.SchIterator_Destroy(LibraryIterator);    { all done, free the iterator }
    End;
    SchLib.GraphicallyInvalidate();
End;

{ Get pin 1 of the footprint }
Function GetPcbPin1(Footprint : IPCB_Footprint) : IPCB_Pad;
Var
    GIterator   : IPCB_GroupIterator;
    Pad         : IPCB_Pad;
    PadList     : TList;
    i           : Integer;
Begin
    Result := Nil;
    GIterator := Footprint.GroupIterator_Create;
    GIterator.AddFilter_ObjectSet(Mkset(ePadObject));
    try
        Pad     := GIterator.FirstPCBObject;
        While Pad <> nil do
        begin
            if ((Pad.Name = '1') or (Pad.Name = 'A1')) then begin
                Result := Pad;
            end;
            Pad := GIterator.NextPCBObject;
        end;
    finally
        Footprint.GroupIterator_Destroy(GIterator);
    end;
End;

{ Add Filled circle to indicate pin 1 }
Procedure AddFilledPin1_Indicator( Footprint, LocX : Double, LocY : Double, Diameter : Double, Layer : TLayer);
Var
    arc         : IPCB_Arc;
    Pin1        : IPCB_Pad;
    Pin1_MinDimension;      // minimum of height or width of the pad
    Pin1XSize   : Double;     // pad width
    Pin1YSize   : Double;     // pad height
    MaxDiameter : Double;   // max diameter of filled pin 1 indicator
Begin
    Pin1 := GetPcbPin1(Footprint);
    Pin1XSize := CoordToMils(Pin1.TopXSize);
    Pin1YSize := CoordToMils(Pin1.TopYSize);
    Pin1_MinDimension := Min(Pin1XSize, Pin1YSize);
    MaxDiameter := Pin1_MinDimension * 1.5; // set max diameter to 1.5x min dimension of pin1
    if Diameter > MaxDiameter then
        Diameter := MaxDiameter;

    arc := PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default);
    arc.XCenter := MilsToCoord(LocX);
    arc.YCenter := MilsToCoord(LocY);
    arc.Radius := MilsToCoord(Diameter/4);  
    arc.LineWidth := MilsToCoord(Diameter/2);
    arc.StartAngle := 0;
    arc.EndAngle := 360;
    arc.Layer := Layer;
    Footprint.AddPCBObject(arc);
    PCBServer.SendMessageToRobots(Footprint.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, arc.I_ObjectAddress);
End;

{ Add Open Circle (unfilled) to indicate pin 1 }
Procedure AddOpenPin1_Indicator( Footprint, LocX : Double, LocY : Double, Diameter : Double, Layer : TLayer);
Var
    arc         : IPCB_Arc;
    Pin1        : IPCB_Pad;
    Pin1_MinDimension;      // minimum of height or width of the pad
    Pin1XSize   : Double;     // pad width
    Pin1YSize   : Double;     // pad height
    MaxDiameter : Double;   // max diameter of filled pin 1 indicator
Begin
    Pin1 := GetPcbPin1(Footprint);
    Pin1XSize := CoordToMils(Pin1.TopXSize);
    Pin1YSize := CoordToMils(Pin1.TopYSize);
    Pin1_MinDimension := Min(Pin1XSize, Pin1YSize);
    MaxDiameter := Pin1_MinDimension * 1.5; // set max diameter to 1.5x min dimension of pin1
    if Diameter > MaxDiameter then
        Diameter := MaxDiameter;
    
    arc := PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default);
    arc.XCenter := MilsToCoord(LocX);
    arc.YCenter := MilsToCoord(LocY);
    arc.Radius := MilsToCoord((Diameter/2)-(Diameter/16));  
    arc.LineWidth := MilsToCoord(Diameter/8);
    arc.StartAngle := 0;
    arc.EndAngle := 360;
    arc.Layer := Layer;
    Footprint.AddPCBObject(arc);
    PCBServer.SendMessageToRobots(Footprint.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, arc.I_ObjectAddress);
End;

Function FootprintHasStepFile(Footprint : IPCB_Footprint) : Boolean;
Var
    GIterator   : IPCB_GroupIterator;
    Obj         : IPcb_Primitive;
Begin
    Result := False;
    GIterator := Footprint.GroupIterator_Create;
    Obj := GIterator.FirstPCBObject;
    while (Obj  <> Nil) Do
    begin
        if Obj.ObjectID = eComponentBodyObject then { if component has ComponentBodyObject, it is a mapped STEP File }
            Result := True;
        Obj  := GIterator.NextPCBObject;
    End;
    Footprint.GroupIterator_Destroy(GIterator); // End iterating Footprint primitives
End;

{ get the schematic symbol which has mapped footprint with FootprintName }
Function GetSchematicSymbolWithFootprintName(MainSchLib : ISchLib, FootprintName : String) : ISch_Component;
Var
    SchLibIterator  : ISch_Iterator;
    ObjectIterator  : ISch_Iterator; 
    SchLibPart      : ISch_Component;
    SchComponent    : ISch_Component;
    obj             : ISch_GraphicalObject;
    i               : Integer;
    MainSchLibDoc   : ISch_Document;
Begin
    Result := Nil;

    MainSchLibDoc := SchServer.GetSchDocumentByPath(MainSchLib.DM_FullPath); // load ISch_Document from SchLib path
    if MainSchLibDoc = Nil then
       MainSchLibDoc := SchServer.LoadSchDocumentByPath(LibPkg_SchLib.DM_FullPath);


    SchLibIterator := MainSchLibDoc.SchLibIterator_Create;
    SchLibIterator.SetState_FilterAll;
    SchLibIterator.AddFilter_ObjectSet(MkSet(eSchComponent));
    SchComponent := SchLibIterator.FirstSchObject;
    While (SchComponent <> Nil) Do      { iterate over schlib components }
    Begin
        ObjectIterator := SchComponent.SchIterator_Create();
        ObjectIterator.AddFilter_ObjectSet(MkSet(eImplementation)); { iterate through only ISch_Implementations of the component }
        obj := ObjectIterator.FirstSchObject();
        while (obj <> nil) do
        begin
            if StringsEqual(FootprintName, obj.ModelName) then begin { if symbol ModelName = FootprintName, we found match }
                Result := SchComponent;
                break;
            end;

            For i := 0 To obj.DatafileLinkCount - 1 do begin    { iterate over all model file mappings }
                if StringsEqual(FootprintName, obj.DatafileLink[i].EntityName) then begin    { if symbol DatafileLink.EntityName = FootprintName, we found match }
                    Result := SchComponent;
                    break;
                end;
            End;   
            obj := ObjectIterator.NextSchObject();
        end;
        SchComponent.SchIterator_Destroy(ObjectIterator);   { free the ObjectIterator }
        SchComponent := SchLibIterator.NextSchObject;       { forward to next schematic symbol }
    End;
    MainSchLibDoc.SchIterator_Destroy(SchLibIterator);
End;

{ Most SnapEDA IntLib files dont have STEP file mapped in the footprint, and include the STEP file in 
  separate download. The STEP filename is usually the same name as the sch symbol name or the footprint name. 
  This method gets the sch symbol name and matching footprint, then looks for a STEP file which matches.
  If a match is found the STEP file is mapped to the footprint }
Procedure MapStepFiles(PcbLib : IPcbLib, MainSchLib : ISchLib);
Var
    FootprintIterator : IPCB_LibraryIterator;
    Footprint         : IPCB_LibComponent;
    SchSymbol         : ISch_Component;
    StepFileList      : TStringList;
    StepFileName      : Widestring;
    BaseStepFileName  : Widestring;
    StepFilePath      : WideString;
    SchSymbolName     : WideString;
    FootprintName     : WideString;
    i                 : Integer;
    STEPmodel         : IPCB_ComponentBody;
    Model             : IPCB_Model;
Begin

    GUIOperationLabel.Caption := 'Mapping STEP Files...';  { show current operation in GUI }
    GUIOperationLabel.Update;  { update text label in GUI }
    GUIFileLabel.Caption := ''; 
    GUIFileLabel.Update;   { show in GUI progress of importing PcbLibs }

    FootprintIterator := PcbLib.LibraryIterator_Create;
    FootprintIterator.SetState_FilterAll;
    Try
        Footprint := FootprintIterator.FirstPCBObject;
        While Footprint <> Nil Do begin
            { check if footprint already has STEP file }
            if not FootprintHasStepFile(Footprint) then begin   
                { get all STEP files in source dir, to see if we can find a match to the footprint }
                StepFileList := GetSTEPFiles(ImportDir);

                { search SchLib for schematic symbol mapped to this footprint, get that schematic symbol name }
                SchSymbolName := '';
                SchSymbol := GetSchematicSymbolWithFootprintName(MainSchLib, Footprint.Name);
                if SchSymbol <> Nil then
                    SchSymbolName := SchSymbol.LibReference;  


                { matching STEP file is either named the symbol name or the footprint name.
                  schematic symbol sometimes has chars which are invalid filenames. The invalid chars
                  are replaced with underscores in Step file names. So must replace invalid chars in
                  symbol name with underscores before searching for matches. }
                SchSymbolName := StringReplace(SchSymbolName, '#', '_', eReplaceOne); { !! eReplaceOne == replace ALL!! }
                SchSymbolName := StringReplace(SchSymbolName, '/', '_', eReplaceOne); { !! eReplaceOne == replace ALL!! }
                SchSymbolName := StringReplace(SchSymbolName, '+', '_', eReplaceOne); { !! eReplaceOne == replace ALL!! }
                SchSymbolName := StringReplace(SchSymbolName, '*', '_', eReplaceOne); { !! eReplaceOne == replace ALL!! }
                SchSymbolName := StringReplace(SchSymbolName, ',', '_', eReplaceOne); { !! eReplaceOne == replace ALL!! }

                { Footprint Names dont have the invalid chars and dont need sanitizing }
                FootprintName := Footprint.Name;

                { search for step files which have name of either sch symbol name or footprint name }
                for i := 0 to StepFileList.Count - 1 do begin
                    StepFilePath := StepFileList.Strings[i];
                    StepFileName := ExtractFileName(StepFilePath);           // extracts filename with extension
                    BaseStepFileName := ExtractBaseFileName(StepFilePath);  // extracts the filename without extension

                    { if STEP filename matches the footprint name or sch symbol name, map it to this footprint }
                    if (StringsEqual(SchSymbolName, BaseStepFileName) or StringsEqual(FootprintName, BaseStepFileName)) then begin
                        STEPmodel := PcbServer.PCBObjectFactory(eComponentBodyObject,eNoDimension,eCreate_Default);
                        STEPmodel.SetState_FromModel;
                        Model := STEPmodel.ModelFactory_FromFilename(StepFilePath, false);
                        if Model <> Nil then begin
                            Model.SetState(90,0,0,0);
                            STEPmodel.Model := Model;
                            Footprint.AddPCBObject(STEPmodel);
                            GUIFileLabel.Caption := StepFileName + ' mapped to ' + Footprint.Name; 
                            GUIFileLabel.Update;   
                            break;
                        end;
                    end;
                end; 
            end;
            Footprint := FootprintIterator.NextPCBObject;   // go to next footprint
        End;
    Finally
        PcbLib.LibraryIterator_Destroy(FootprintIterator);
    End;
End;

{ Standardize Footprints }
Procedure ProcessPcbFootprints(PcbLib : IPcbLib);
Var
    FootprintIterator : IPCB_LibraryIterator;
    Footprint         : IPCB_LibComponent;
    GIterator  : IPCB_GroupIterator;
    Obj        : IPcb_Primitive;
    DeleteObj  : IPCB_Primitive;
    ObjLayer   : TLayer;
    DeleteList : TList;
    i          : Integer;
    TextSize   : Integer;
    FilledCircleDiameter : Double;
    LocX       : Integer;
    LocY       : Integer;
    Pin1       : IPCB_Pad;
    Width      : Double;
Begin
    FootprintIterator := PcbLib.LibraryIterator_Create;
    FootprintIterator.SetState_FilterAll;
    Try
        Footprint := FootprintIterator.FirstPCBObject;
        While Footprint <> Nil Do begin
            DeleteList := TInterfaceList.Create();
            GIterator := Footprint.GroupIterator_Create;
            Obj := GIterator.FirstPCBObject;
            while (Obj  <> Nil) Do
            begin
                Case Obj.ObjectID Of
                    eArcObject: Begin
                        Width := CoordToMils(Obj.LineWidth);
                        { change Mechanical13 Arc LineWidth from 0 to 2mils }
                        if (Width = 0) and (Obj.Layer = eMechanical13)then begin
                            Obj.LineWidth := MilsToCoord(2);
                        end;
                    End;
                    eTrackObject: Begin
                        Width := CoordToMils(Obj.Width);
                        { change Mechanical13 Track(Lines) Width to minimum 2mils }
                        if (Width < 2) and (Obj.Layer = eMechanical13)then begin
                            Obj.Width := MilsToCoord(2);
                        end;
                    End;
                    eTextObject: Begin
                        If Obj.Text = '' then begin
                            DeleteList.Add(Obj);    { delete empty text objects, sometimes they are present }
                            Obj  := GIterator.NextPCBObject;
                            continue;
                        end;

                        { replace '*' on silkscreen layer with filled circle }
                        if ((Obj.Text = '*') and (Obj.Layer = eTopOverlay))then begin
                            TextSize := CoordToMils(Obj.Size);
                            LocX := CoordToMils(Obj.XLocation);
                            LocY := CoordToMils(Obj.YLocation);
                            FilledCircleDiameter := 0.5*TextSize;
                            AddFilledPin1_Indicator( Footprint, LocX, LocY, FilledCircleDiameter, Obj.Layer); // add pin 1 indicator
                            DeleteList.Add(Obj); // remove the text star '*''
                        end;
                        { replace '*' on eMechanical13 layer with open circle }
                        if ((Obj.Text = '*') and (Obj.Layer = eMechanical13))then begin
                            TextSize := CoordToMils(Obj.Size);
                            LocX := CoordToMils(Obj.XLocation);
                            LocY := CoordToMils(Obj.YLocation);
                            FilledCircleDiameter := 0.5*TextSize;
                            AddOpenPin1_Indicator( Footprint, LocX, LocY, FilledCircleDiameter, Obj.Layer); // add pin 1 indicator
                            DeleteList.Add(Obj); // remove the text star '*'
                        end;
                        { occasionally SnapEda puts in Silkscreen Layer the text 'Designator'- delete it }
                        if (StringBeginsWith(Obj.Text, 'Designator') or StringBeginsWith(Obj.Text, '.Designator') or StringContains(Obj.Text, 'Comment')) then begin
                            DeleteList.Add(Obj);
                        end;
                    End;
                End;
                Obj  := GIterator.NextPCBObject;
            End;
            Footprint.GroupIterator_Destroy(GIterator); // End iterating current Footprint primitives

            try
                PCBServer.PreProcess;
                { delete the things we flagged }
                for i := 0 to DeleteList.Count - 1 do
                begin
                    DeleteObj   := DeleteList.Items[i];
                    Footprint.RemovePCBObject(DeleteObj);
                end;
            finally
                PCBServer.PostProcess;
                DeleteList.Free;
            end;

            Footprint := FootprintIterator.NextPCBObject;   // go to next footprint
        End;
    Finally
        PcbLib.LibraryIterator_Destroy(FootprintIterator);
    End;
    PcbLib.Board.GraphicallyInvalidate;
End;

{ Copy Footprints from one PcbLib to another PcbLib }
Procedure CopyPcbLibParts(SourcePcbLib : IPcb_Lib, DestPcbLib : IPcb_Lib);
Var
    i                : integer;
    SourceFootprint  : IPCB_Component;        // IPcb_LibComponent;
    CopyFP           : IPCB_Component;        // IPcb_LibComponent;
Begin
    PCBServer.PreProcess;
    // Iterate through parts in SourcePcbLib and copy to DestPcbLib
    for i := 0 to (SourcePcbLib.ComponentCount - 1)  do
    Begin
        SourceFootprint := SourcePcbLib.GetComponent(i);                // Get footprint to be copied (IPCB_LibComponent)
        CopyFP := DestPcbLib.GetComponentByName(SourceFootprint.Name);  // check if component name exists already in DestPcbLib

        If CopyFP = nil Then Begin    // if footprint name doesnt exist already in DestPcbLib
            CopyFP := DestPcbLib.CreateNewComponent;    // returns IPCB_LibComponent
            CopyFP.Name := SourceFootprint.Name;
        End
        Else Begin                  // footprint name exists already
            CopyFP.Name := DestPcbLib.GetUniqueCompName(SourceFootprint.Name);  // get nonexisting FP name
        End;

        SourceFootprint.CopyTo(CopyFP, eFullCopy);
        DestPcbLib.RegisterComponent(CopyFP);
    End;
    PCBServer.PostProcess;
End;

{*  Extract the SchLib and PcbLib from each IntLib file. *}
Procedure ExtractIntLibSources(IntLibFiles : TStringList, MainDir : String);
Var
    i : Integer;
    SubdirectoryList : TStringList;
    SchLibFiles      : TStringList;
    PcbLibFiles      : TStringList;
    Subdirectory     : String;
Begin
    If IntegratedLibraryManager = Nil then Exit; 

    If IntLibFiles.Count = 0 then Exit;     // no IntLibFiles to extract, so return

    GUIOperationLabel.Caption := 'Extracting IntLibs to SchLib and PcbLib ...';  { show operation we are doing in GUI }
    GUIOperationLabel.Update;  { update label in GUI }

    For i := 0 To IntLibFiles.Count - 1 Do
    Begin
        { Show in GUI IntLib extract progress }
        GUIFileLabel.Caption := 'Extracting ' + ExtractFileName(IntLibFiles.Strings[i]) + ' ...' + IndentString(3) +
                               GetStringFromInteger(i+1) + ' of ' + GetStringFromInteger(IntLibFiles.Count); 
        GUIFileLabel.Update;
        GUIProgressBar.Position := ((i+1)/IntLibFiles.Count)*100; { set progress bar, based on num files remaining }
        GUIProgressBar.Update;         { update GUI progress bar position }

        IntegratedLibraryManager.ExtractSources(IntLibFiles.Strings[i]);
    End;

    { After extracting IntLibs -> SchLib and PcbLib, move the IntLibs to the IntLib Archive directory}
    MoveFilesToDir(IntLibFiles, IntLibArchiveDir); 

    { The process of extraction places each SchLib and PcbLib in a new individual folder 
      Must enumerate all the created new folders then move the SchLib and PcbLib up to the main dir }
    SubdirectoryList := GetSubDirectories(MainDir); // get all folders in main dir
    RemoveStringFromList(SubdirectoryList, ArchiveDir);
    RemoveStringFromList(SubdirectoryList, StepFileDir); // remove StepFileDir from places to look for SchLib and PcbLib files

    {*  Move Extracted SchLib and PcbLib files up to base directory
            -visit each subdirectory, get SchLib and PcbLib files
            -move SchLib and PcbLib to base directory
            -delete the subdirectory    *}
    For i := 0 To SubdirectoryList.Count - 1 Do     // Iterate over each directory that was created during extraction
    Begin
        Subdirectory := SubdirectoryList.Strings[i];
        SchLibFiles := GetSchLibFiles( Subdirectory );  // Find all .SchLib files in the subdirectory
        PcbLibFiles := GetPcbLibFiles( Subdirectory );  // Find all .PcbLib files in the subdirectory
        MoveFilesToDir(SchLibFiles, MainDir);     // move .SchLib files up to base directory
        MoveFilesToDir(PcbLibFiles, MainDir);     // move .PcbLib files up to base directory
        DeleteDir( Subdirectory );         // delete the subdir after moving out SchLib and PcbLib
    End;
End;

{ unzip zip files in SearchDir}
Procedure Extract_Zip_Files(SearchDir : String);
Var
    ZipFiles      : TStringList;
    UnzippedFiles : TStringList;
    TempDir       : String;
    i             : Integer;
Begin
    SearchDir := IncludeTrailingPathDelimiter(SearchDir); // make sure includes backslash so we can concatenate paths
    TempDir := SearchDir + 'temp';
    DeleteDir(TempDir); // delete temp dir if it exists, so when we create it, it is empty

    { create temp dir, to extract zip files to }
    DirectoryCreate(TempDir);

    { get all zip files in the search dir }
    ZipFiles := FindZipFiles( SearchDir );

    { set GUI elements }
    GUIOperationLabel.Caption := 'Extracting Zip Files...';  GUIOperationLabel.Update;  { show label unzipping files }

    { unzip the files -> temp dir }
    for i := 0 to (ZipFiles.Count - 1) do
    begin
        UnzipFileToDir(ZipFiles.Strings[i], TempDir);    // extract contents of zipfile to temp dir
        MoveFileToDir(ZipFiles.Strings[i], ZipArchiveDir);  // move zip file to zip archive dir after extracting contents
    end; 

    { get all the unzipped files in temp dir, including any subdirectories created in temp }
    UnzippedFiles := GetFilesInDir(TempDir, True);

    { examine each file in temp dir, if it is a file we can import to Altium, move it to the base import dir }
    for i := 0 to (UnzippedFiles.Count - 1) do
    begin
        if Is_Altium_File( UnzippedFiles.Strings[i] ) then begin   // if can import file to Altium
            MoveFileToDir(UnzippedFiles.Strings[i], SearchDir);    // move zip file to base dir after extracting contents
        end;
    end;

    { after we have finished moving out the files we want from temp dir, delete the temp dir }
    DeleteDir(TempDir);
End;

// find zip files, only ones which are smaller than 500Kb
Function FindZipFiles(DirectoryPath : String) : TStringList;
Var
   ZipFiles     : TStringList;
   FilteredList : TStringList;
   i            : Integer;
   FileSize     : Integer;
Begin
    Result := nil;
    DirectoryPath := IncludeTrailingPathDelimiter(DirectoryPath); // Include trailing path delimiter if not already included
    If ( DirectoryExists(DirectoryPath) ) Then
    Begin
        Try
            FilteredList := TStringList.Create;
            ZipFiles := TStringList.Create;
            ZipFiles.Sorted := True;
            ZipFiles.Duplicates := dupIgnore;
            FindFiles(DirectoryPath,'*.zip',faAnyFile,False,ZipFiles); //result is stored in ZipFiles. False=dont look in subfolders
            Result :=  ZipFiles;

            i := 0;
            While i < ZipFiles.Count Do
            Begin
                FileSize := GetFileSize(ZipFiles.Strings[i]);
                if FileSize < MAX_ZIP_FILE_SIZE then
                begin
                    FilteredList.Add(ZipFiles.Strings[i]);  { keep only files less than 500kB }
                end;
                Inc(i); // Increment to the next item in the list
            End;
        Finally
            //Rpt.Add('');
        End;
    End;
End;

Procedure UnzipFileToDir(ZipFilePath : String, DestinationDir : String);
Var
    ZipFile : TXceedZip;
Begin
    { if file to move doesnt exist then Exit }
    if not (FileExists(ZipFilePath)) then 
        Exit;

    { if destination directory doesnt exist, create it }
    if not (DirectoryExists(DestinationDir)) then
    begin
        if (not DirectoryCreate(DestinationDir)) then    // if cannot create target directory, exit
            Exit;
    end;                                                      

    ZipFile := TXCeedZip.Create(ZipFilePath);
    ZipFile.UseTempFile := False;
    ZipFile.PreservePaths := False;  
    ZipFile.UnzipToFolder := DestinationDir; // extract zip file to temp dir
    ZipFile.Unzip;  // perform the unzip operation
End;

{  check if file is: .step, .SchLib, .PcbLib, .IntLib, UltraLib_text_file, basically any Altium file we can import }
Function Is_Altium_File(Filepath : String) : Boolean;
Var
    FileExtension : String;
Begin
    Result := False;

    FileExtension := ExtractFileExt(Filepath);
    if (StringsEqual(FileExtension, '.step') or StringsEqual(FileExtension, '.stp') ) then
    begin    
        Result := True;
        Exit;
    end;

    if (StringsEqual(FileExtension, '.IntLib') or StringsEqual(FileExtension, '.SchLib') or StringsEqual(FileExtension, '.PcbLib')) then
    begin    
        Result := True;
        Exit;
    end;

    if (StringsEqual(FileExtension, '.txt') and IsUltraLibrarianFile(Filepath)) then
    begin
        Result := True;
        Exit;
    end;
End;

{ Search Import Dir for UltraLibrarian text files to import }
Function GetUltraLibrarianTextFiles(SearchDir : String) : TStringList;
Var
    UltraLibTxtFiles : TStringList;
    TxtFiles : TStringList;
    i : Integer;
    CurFilePath : String;
Begin
    Result := nil;
    GUIOperationLabel.Caption := 'Searching for UltraLib Files to Import...';  GUIOperationLabel.Update;  { show status in GUI }
    SearchDir := IncludeTrailingPathDelimiter(SearchDir); // Include backslash if not already included
    If ( DirectoryExists(SearchDir) ) Then
    Begin
        Try
            TxtFiles := TStringList.Create;
            UltraLibTxtFiles := TStringList.Create;

            FindFiles(SearchDir,'*.txt',faAnyFile,False,TxtFiles); //result is stored in TxtFiles. False=dont look in subfolders
            For i := 0 To TxtFiles.Count - 1 Do     // examine each txtfile
            Begin
                CurFilePath := TxtFiles.Strings[i];
                if IsUltraLibrarianFile(CurFilePath) then begin
                    UltraLibTxtFiles.Add(CurFilePath);          // add file to UltraLib list
                end
                Else Begin
                    DeleteFile(CurFilePath);    // if not an UltraLib file, then delete it
                End;
            End;
        Finally
            //
        End;
        Result :=  UltraLibTxtFiles;
    End;
End;

{ returns true if the first line read contains '# Created by Ultra Librarian', otherwise checks for following two lines:
      Footprint (Name "CAP_GCM1555_0402_MUR-M")
      Component (Name "GCM1555C1H470JA16D") (PartCount 1) (DesPrefix "C?") }
Function IsUltraLibrarianFile(Filepath : String) : Boolean;
Var
    CheckFile               : TextFile;
    line_read               : String;
    Has_UltraLibrarian_Line : Boolean;
    Has_Footprint_Line      : Boolean;   
    Has_Component_Line      : Boolean;  
Begin
    Result := False;
    AssignFile(CheckFile, Filepath);
    Reset(CheckFile);

    While Not EOF(CheckFile) Do Begin
        ReadLn(CheckFile, line_read);
        If VarIsNull(line_read) Then Continue;

        // check for the line: # Created by Ultra Librarian 8.3.344 Copyright � 1999-2021
        If (StringContains(line_read, '# Created by Ultra Librarian') or StringContains(line_read, '-- Combined Altium Import --')) Then Begin
            Has_UltraLibrarian_Line := True;
            Result := True;
            CloseFile(CheckFile);
            Exit;
        End;

        // check for the line: Footprint (Name "CAP_GCM1555_0402_MUR-M")
        If (StringBeginsWith(line_read, 'Footprint (Name "') and StringEndsWith(line_read, '")')) Then Begin
            Has_Footprint_Line := True;
            Continue;
        End;

        // check for the line: Component (Name "GCM1555C1H470JA16D") (PartCount 1) (DesPrefix "C?")
        If (StringBeginsWith(line_read, 'Component (Name "') and StringEndsWith(line_read, '")')) and (StringContains(line_read, '(DesPrefix "')) Then Begin
            Has_Component_Line := True;
            Continue;
        End;
    End;
    CloseFile(CheckFile);

    if (Has_Footprint_Line and Has_Component_Line) then
        Result := True;       
End;

{ search for .IntLib files in the DirectoryPath }
Function GetIntLibFiles(DirectoryPath : String) : TStringList;
Var
    i           : Integer;
    IntLibFiles : TStringList;
Begin
    Result := nil;

    DirectoryPath := IncludeTrailingPathDelimiter(DirectoryPath); // Include backslash if not already included
    If ( DirectoryExists(DirectoryPath) ) Then
    Begin
        Try
            IntLibFiles := TStringList.Create;
            IntLibFiles.Sorted := True;
            IntLibFiles.Duplicates := dupIgnore;
            
            FindFiles(DirectoryPath,'*.IntLib',faAnyFile,False,IntLibFiles); //result is stored in IntLibFiles. False=dont look in subfolders
            Result :=  IntLibFiles;
        Finally
            Exit;
        End;
    End;
End;

{ search for .SchLib files in the DirectoryPath }
Function GetSchLibFiles(DirectoryPath : String) : TStringList;
Var
   SchLibFiles : TStringList;
   i           : Integer;
Begin
    Result := nil;
    DirectoryPath := IncludeTrailingPathDelimiter(DirectoryPath); // Include backslash if not already included
    If ( DirectoryExists(DirectoryPath) ) Then
    Begin
        Try
            SchLibFiles := TStringList.Create;
            SchLibFiles.Sorted := True;
            SchLibFiles.Duplicates := dupIgnore;
            
            FindFiles(DirectoryPath,'*.SchLib',faAnyFile,False,SchLibFiles); //result is stored in SchLibFiles. False=dont look in subfolders
            Result :=  SchLibFiles;
        Finally
            Exit;
        End;
    End;
End;

{ search for .PcbLib files in the DirectoryPath }
Function GetPcbLibFiles(DirectoryPath : String) : TStringList;
Var
   PcbLibFiles : TStringList;
   i           : Integer;
Begin
    Result := nil;

    DirectoryPath := IncludeTrailingPathDelimiter(DirectoryPath); // Include backslash if not already included
    If ( DirectoryExists(DirectoryPath) ) Then
    Begin
        Try
            PcbLibFiles := TStringList.Create;
            PcbLibFiles.Sorted := True;
            PcbLibFiles.Duplicates := dupIgnore;
            FindFiles(DirectoryPath,'*.PcbLib',faAnyFile,False,PcbLibFiles); //result is stored in PcbLibFiles. False=dont look in subfolders
            Result :=  PcbLibFiles;
        Finally
            Exit;
        End;
    End;
End;

{ search for .step files in the DirectoryPath }
Function GetSTEPFiles(DirectoryPath : String) : TStringList;
Var
   STEPFiles : TStringList;
   STPFiles  : TStringList;
   STPFile   : String;
   i : Integer;
Begin
    Result := nil;
    DirectoryPath := IncludeTrailingPathDelimiter(DirectoryPath); // Include trailing path delimiter if not already included
    If ( DirectoryExists(DirectoryPath) ) Then
    Begin
        Try
            { search for .step files }
            STEPFiles := TStringList.Create;
            STEPFiles.Sorted := True;
            STEPFiles.Duplicates := dupIgnore;
            FindFiles(DirectoryPath,'*.STEP',faAnyFile,False,STEPFiles); // stringlist result is stored in STEPFiles. False=dont look in subfolders
            
            { search for .stp files }
            STPFiles := TStringList.Create;
            STPFiles.Sorted := True;
            STPFiles.Duplicates := dupIgnore;
            FindFiles(DirectoryPath,'*.STP',faAnyFile,False,STPFiles);   // stringlist result is stored in STPFiles. False=dont look in subfolders           
            
            { add .STP files to Step file list }
            for i := 0 to (STPFiles.Count - 1) do
            begin
                STEPFiles.Add( STPFiles.Strings[i] );
            end;
            
            Result :=  STEPFiles;
        Finally
            Exit;
        End;
    End;
End;

{ filters out any results which are . or .. }
Function GetFilesInDir(DirPath : String, SearchSubDirs : Boolean) : TStringList;
Var
    FileList   : TStringList;
    DirList    : TStringList;
    RemoveList : TStringList;
    i          : Integer;
    FilePath   : String;
Begin
    FileList := TStringList.Create;     // holds results of file search
    DirList := TStringList.Create;   // holds any subdirs which exist in search dir
    RemoveList := TStringList.Create;   // holds list of things to remove from FileList

    FindFiles(DirPath, '*.*', faAnyFile, SearchSubDirs, FileList);  // search for files

    { search DirPath for any directories, must remove these from results so only returning files }
    FindFiles(DirPath, '*', faDirectory, SearchSubDirs, DirList);  // search for only directories

    { remove from the results any directories }
    for i := 0 to (DirList.Count - 1) do
    begin
        RemoveStringFromList(FileList, DirList.Strings[i]);
    end;

    Result := FileList;
End;

Procedure MoveFileToDir(OldFilePath : String, DestDirectory : String);
Var
    OldFileName : String;
    NewFilePath : String;
Begin
    { if file to move doesnt exist then Exit }
    if not (FileExists(OldFilePath)) then 
        Exit;

    { if destination directory doesnt exist, create it }
    if not (DirectoryExists(DestDirectory)) then begin       
        if (not DirectoryCreate(DestDirectory)) then    // if cannot create target directory, exit
            Exit;
    end;

    OldFileName := ExtractFileName(OldFilePath);
    NewFilePath := IncludeTrailingPathDelimiter(DestDirectory) + OldFileName;
    
    { If destination file exists already, delete it }
    if FileExists(NewFilePath) then
        DeleteFile(NewFilePath);
    
    RenameFile(OldFilePath, NewFilePath);  // rename file to move it
End;

Procedure MoveFilesToDir(FileList : TStringList, DestinationDir : String);
Var
   i : Integer;
   CurFileName : String;
   CurFilePath : Widestring;
   NewFilePath : Widestring;
Begin
    { if destination directory doesnt exist, create it }
    if not (DirectoryExists(DestinationDir)) then
        DirectoryCreate(DestinationDir);
    
    For i := 0 To FileList.Count - 1 Do     // Move each file one at a time
    Begin
        CurFilePath := FileList.Strings[i];
        CurFileName := ExtractFileName(CurFilePath);    // extract filename from CurFilepath
        NewFilePath := IncludeTrailingPathDelimiter(DestinationDir) + CurFileName;

        if FileExists(NewFilePath) and not StringsEqual(CurFilePath,NewFilePath) then
            DeleteFile(NewFilePath);    // delete destination file if it already exists (so no error is thrown)
        
        RenameFile(CurFilePath, NewFilePath);   // rename to move the file
    End;
End;

{***************************************************************************
 * procedure DeleteDir()
 *  Delete directory and all files and subdirectories contained within
 *
 ***************************************************************************}
procedure DeleteDir(dirPath : TString);
Var
    success       : Integer;
    i             : Integer;
    searchSubDirs : Boolean;
    fileList      : TStringList;
    dirList       : TStringList;
    dirName       : String;
Begin
    if not DirectoryExists(dirPath) then Exit;   //verifies path IS a  directory, and it exists
    
    fileList := TStringList.Create();    // Create string list to hold files to delete
    dirList := TStringList.Create();   // Create string list to hold subdirs to delete 

    { Find and delete any files in any nested subdirs. must be deleted first or dir deletion will fail }
    searchSubDirs := True;
    FindFiles(dirPath, '*.*', faAnyFile, searchSubDirs, fileList);  // searches all subdirs, returns results in fileList
    for i := 0 to (fileList.Count - 1) do           // Delete all the files we found
    begin
        DeleteFile(fileList[i]);  // Delete the file. 
    end; 

    { Find any subdirectories }
    searchSubDirs := True;
    FindFiles(dirPath, '*', faDirectory, searchSubDirs, dirList);

    { Delete all subdirectories- Note: Reverse the order of traversal! }
    for i := (dirList.Count - 1) downto 0 do
    begin
        dirName := ExtractFileName(dirList[i]);   // Extract just the last entry from directory path
        if ( (dirName <> '.') and (dirName <> '..') ) then  // Don't attempt to delete "." or ".."
        begin
            success := RemoveDir(dirList[i]);  // delete the directory
            if (not success) then   // try again
            begin
                Sleep(250); // delay 250 ms before trying again 
                RemoveDir(dirList[i]);
            end;
        end;
    end; 
    
    { Delete the main directory }
    RemoveDir(dirPath);   
End; // end DeleteDir() 

Function GetSubDirectories(const SearchDirectory: String);
Var
  SearchResults : TStringList;
  parentDir     : TString;
  currentDir    : TString;
Begin
  SearchResults := TStringList.Create;  // create results list

  SearchDirectory := IncludeTrailingPathDelimiter(SearchDirectory); // Ensure the directory path ends with a backslash
  parentDir := SearchDirectory + '..';  // included with search results, need to remove
  currentDir := SearchDirectory + '.';  // included with search results, need to remove

  FindFiles(SearchDirectory, '*', faDirectory,False, SearchResults);
  RemoveStringFromList(SearchResults, parentDir);
  RemoveStringFromList(SearchResults, currentDir);
  Result := SearchResults;
End;

{ returns something like: 08-07-24_5h52m, used for setting to unique library name }
Function GetTimeStampString();
var
    TimeStampStr : String;
    DateStr      : String;
    TimeStr      : String;
    DayInt       : Integer;
    DayStr       : String;
    MonthStr     : String;
    YearStr      : String;
    char_index   : Integer;
begin
    DateStr := GetCurrentDateString();  {  '8/7/2024'  }
    YearStr := GetSubstring(DateStr, Length(DateStr)-1, Length(DateStr)); { '24' }

    char_index := Pos('/', DateStr); // returns index of first '/'
    MonthStr := GetSubstring(DateStr, 1, char_index-1);     { '8' }
    DayStr := GetSubstring(DateStr, char_index+1, Length(DateStr)-5);     { '7' }  

    if Length(MonthStr) = 1 then
        MonthStr := '0' + MonthStr;     // add leading zero if month is only single digit
    if Length(DayStr) = 1 then
        DayStr := '0' + DayStr;     // add leading zero if day is only single digit
    DateStr :=  MonthStr + '-' + DayStr + '-' + YearStr;  {  '08-07-24'  }

    TimeStr := GetCurrentTimeString();  {  5:52:00 PM  }
    char_index := Pos(':', TimeStr);  // returns index of first ':'
    TimeStr[char_index] := 'h';       // replace first colon with hour letter
    char_index := Pos(':', TimeStr);  // returns index of second ':'
    TimeStr[char_index] := 'm';       // replace second colon with minute letter
    TimeStr := Copy(TimeStr, 1, char_index);   {  '5h52m'  }

    TimeStampStr := DateStr + '_' + TimeStr;
    Result := TimeStampStr;
end;

Procedure RemoveStringFromList(list: TStringList, strToRemove: string);
Var
    i: Integer;
Begin
    i := 0;
    While i < list.Count Do
    Begin
        If StringsEqual(list[i], strToRemove) Then   // StringsEqual is case insensitive
        Begin
            list.Delete(i); // Remove the string from the list
        End
        Else
        Begin
            Inc(i); // Move to the next item in the list
        End;
    End;
End;

{==========================  Main Program  ===================================}
Procedure StartImport();
Var
    LibPkgPath       : String;
    IntLibFiles      : TStringList;
    SchLibFiles      : TStringList;
    PcbLibFiles      : TStringList;
    StepFiles        : TStringList;
    UltraLibTxtFiles : TStringList; { UltraLibrarian text files to import }

    LibPkgProject : IProject;
    ProjDoc       : IServerDocument;
    PcbLibDoc     : IServerDocument;
    SchLibDoc     : IServerDocument;
    SchLib        : ISchLib;
    PcbLib        : IPcbLib;
    LibPkg_SchLib : IDocument;  { the SchLib Document which is part of LibPkg }
    LibPkg_PcbLib : IDocument;  { the PcbLib Document which is part of LibPkg }
    NumComponents : Integer;    { num Sch symbols imported }
    HistoryDir : String;
Begin
    Rpt := TStringList.Create;  { start report }
    ArchiveDir := ImportDir + 'Archive';    { main Archive folder }
    ZipArchiveDir := IncludeTrailingPathDelimiter(ArchiveDir) + 'Zip_Archive';   { where to place zip files after importing }
    IntLibArchiveDir := IncludeTrailingPathDelimiter(ArchiveDir) + 'IntLib_Archive';    { place IntLib files here after importing) }
    StepFileDir := ImportDir + 'STEP_Files';        {  place step files here after importing }
    DirectoryCreate(ArchiveDir);        { [ImportDir]\Archive }
    DirectoryCreate(ZipArchiveDir);     { [ImportDir]\Archive\Zip_Archive }  
    DirectoryCreate(IntLibArchiveDir);  { [ImportDir]\Archive\IntLib_Archive }
    DirectoryCreate(StepFileDir);       { [ImportDir]\STEP_Files }

    {******   CREATE NEW LIBPKG   ******}
    LibPkgPath := ImportDir + 'AltiumImport_' + GetTimeStampString();
    if not CreateLibPkg(LibPkgPath, LibPkgProject, ProjDoc, PcbLibDoc, SchLibDoc, PcbLib, SchLib) then
        Exit;   {  if cannot create LibPkg, then Exit }
    
    {******   UNZIP FILES   ******}
    Extract_Zip_Files( ImportDir ); { Unzip all files in import dir, keep only the files we can import into Altium }

    {******   FIND & IMPORT ULTRALIBRARIAN FILES   ******}
    UltraLibTxtFiles := GetUltraLibrarianTextFiles( ImportDir );  { Search for UltraLib text files in import dir }
    ImportUltraLibrarianTextFiles(SchLib, PcbLib, UltraLibTxtFiles); { import files into SchLib and PcbLib }

    {******   FIND INLTIB FILES   ******}
    IntLibFiles := GetIntLibFiles( ImportDir );    { Find all IntLib files in the import directory } 

    {******   EXTRACT INLTIBs to PCBLIBs and SCHLIBs   ******}
    ExtractIntLibSources(IntLibFiles, ImportDir);   // Extract each SchLib and PcbLib from IntLib, place in ImportDir
    
    {******   FIND SCHLIB FILES   ******}
    SchLibFiles := GetSchLibFiles( ImportDir );    { Find all SchLib files in the import directory }
    LibPkg_SchLib := GetSchLib_from_LibPkg(LibPkgProject);  { returns a document which is part of LibPkg logical documents }
    RemoveStringFromList(SchLibFiles, LibPkg_SchLib.DM_FullPath);   { remove main SchLib file from the list of files to import }
    
    {******   COPY SCH SYMBOLS FROM EACH SCHLIB TO MAIN SCHLIB   ******}
    ImportSchLibFiles(LibPkg_SchLib, SchLibFiles);

    {******   STANDARDIZE SCH SYMBOLS   ******}
    ProcessSchLibComponents(SchLib); // adjust component fonts, set colors and fonts, filter parameters, set active low pin names

    {******   FIND PCBLIB FILES   ******}
    PcbLibFiles := GetPcbLibFiles( ImportDir );
    LibPkg_PcbLib := GetPcbLib_from_LibPkg( LibPkgProject ); 
    RemoveStringFromList(PcbLibFiles, LibPkg_PcbLib.DM_FullPath);   { remove main PcbLib file from the list of files to import }

    {******   COPY FOOTPRINTS FROM EACH PCBLIB TO MAIN PCBLIB   ******}
    ImportPcbLibFiles(LibPkg_PcbLib, PcbLibFiles);

    {******   TRY TO MAP STEP FILES FOR FOOTPRINTS THAT DONT HAVE ONE   ******}
    MapStepFiles(PcbLib, LibPkg_SchLib);

    {******   MOVE STEP FILES TO ARCHIVE DIR   ******}
    StepFiles := GetSTEPFiles( ImportDir );   // get all step files in main directory
    MoveFilesToDir(StepFiles, StepFileDir);  // move them to Step File Directory

    {******   SAVE PROJECT FILES   ******}
    ProjDoc.DoFileSave(cDocKind_IntegratedLibrary);
    PcbLibDoc.DoFileSave(cDocKind_PcbLib);
    SchLibDoc.DoFileSave(cDocKind_SchLib);

    {******   SHOW THE SYMBOLS and FOOTPRINTS IMPORTED   ******}
    Client.ShowDocument(PcbLibDoc);  // displays the PcbLib 
    Client.ShowDocument(SchLibDoc);  // displays the SchLib 

    {******   CLEANUP AND SHOW RESULTS   ******}
    HistoryDir := IncludeTrailingPathDelimiter( ImportDir ) + 'History';
    DeleteDir(HistoryDir);  // delete History directory if it was created by Altium
    NumComponents := GetNumSchLibParts(SchLib); { count number of Sch symbols we imported }
    ShowInfo('Imported ' + IntToStr(NumComponents) + ' Schematic Symbols,  ' + IntToStr(PcbLib.ComponentCount) + ' Footprints.  Woohoo!!');
End;


{==========================  GUI CALLBACK FUNCTIONS  ===================================}

// callback function for directory pick button
Procedure TFind_File.DirectoryChooserChange(Sender: TObject);
Begin
    ImportDir := DirectoryChooser.Text; // Get import dir, by accessing Text field of DrectoryChooser widget
    DirectoryChooser.Update;
    ImportDir := IncludeTrailingPathDelimiter( ImportDir ); { make sure includes backslash so we can concatenate paths }
End; 

procedure TFind_File.StartButtonClick(Sender: TObject);     
var
    DestGraphicsLibPath : String;
begin

    // if import dir not nil and directory exists
    if (ImportDir <> '') and DirectoryExists(ImportDir) then begin
        DestGraphicsLibPath := IncludeTrailingPathDelimiter(ImportDir) + 'GraphicsPrimitives.SchLib';
        if (FileExists(GRAPHICS_LIB_PATH)) then begin
            FileCopy(GRAPHICS_LIB_PATH, DestGraphicsLibPath, True); { copy graphics symbols to import dir, so it is also imported }
        end;
        StartImport();
        Close;
    end
    else begin
        ShowError('Error: must select valid directory to import files from');
    end;
end;

Procedure TFind_File.CancelButtonClick(Sender: TObject);
Begin
    Close;
End;



