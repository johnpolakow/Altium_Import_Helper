{ Created by Ultra Librarian 
  Frank Frank, Accelerated Designs

  additions, noted as *NEW*,  by John Polakowski, johnmpolakowski@gmail.com }

const ForceSchFontID = 1;
var
    BrokenSCHFontManager : Boolean; { for Altium 19's broken SCH FontManager }

{ STRING UTIL METHOD DECLARATIONS }
function StringContains(BaseStr : String, Srch : String) : Boolean; forward;                        {* NEW *}
Function GetSubstring(InputStr : String, StartPos : Integer, EndPos : Integer) : String; forward;   {* NEW *}
function StringIsNumberedDuplicate(const InputString: String); forward;                             {* NEW *}
function StringIsInteger(const InputString: string): Boolean; forward;                              {* NEW *}
function GetLastIndexOfChar(const InputStr : String, SearchChar: String) : Integer; forward;        {* NEW *}
Function StringBeginsWith(BaseStr : String, SearchStr : Str) : Boolean; forward;                    {* NEW *}
Function StringEndsWith(BaseStr : String, EndStr : Str) : Boolean; forward;                         {* NEW *}
function ReplaceCharInString(InputString : String, OldChar : Char, NewChar : Char); forward;        {* NEW *}
Function CheckLeft(BaseStr: String, Srch: String): Boolean; forward;
Function LeftOf(BaseStr: String, Srch: String): String; forward;
Function RightOf(BaseStr: String, Srch: String): String; forward;
Procedure StrChop(BaseStr: String, Srch: String, Out LeftSide: String, Out RightSide: String); forward;
Function GetBetween(BaseStr: String, StartStr: String, EndStr: String): String; forward;
Function GetFileLocation(FilePath: String): String; forward;
Function ExtractBaseFileName(const AbsolutePath: string) : Widestring; forward;                     {* NEW *}

{ FOOTPRINT METHOD DECLARATIONS }
Function LayerFromString(LName: String): TLayer; forward;
Procedure FP_AddStep(fp: IPCB_LibComponent, Data: String);  forward;
Procedure FP_AddLine(fp: IPCB_Component, Data: String); forward;
Procedure FP_AddArc(fp: IPCB_Component, Data: String); forward;
Procedure FP_AddPoly(fp: IPCB_Component, Data: String, InFile: TextFile); forward;
Procedure FP_AddText(fp: IPCB_Component, Data: STring); forward;
Procedure FP_AddPad(fp: IPCB_Component, Data: String, InFile: TextFile); forward;
procedure DeletePcbComponent_1(PcbLib : IPCB_Library); forward;                                     {* NEW *}
Procedure ImportFootprints(InFile: TextFile, Lib: IPCB_Library, Errors: TStringList); forward;

{ SCHEMATIC SYMBOL METHOD DECLARATIONS }
Function TextJustificationFromString(Value: String): TTextJustification; forward;
Function SY_GetFont(Height: Double, Angle: TRotationBy90): TFontID; forward;
Function SY_GetFontId(FontName: TDynamicString, FontSize : Integer) : TFontID; forward;             {* NEW *}
Function SY_GetAngle(Angle : String): TRotationBy90; forward;
Procedure SY_AddLine(sy: ISch_Component, Data: String); forward;
Procedure SY_AddRect(sy: ISch_Component, Data: String); forward;
Procedure SY_AddArc(sy: ISch_Component, Data: String); forward;
Procedure SY_AddPoly(sy: ISch_Component, Data: String, InFile: TextFile); forward;
Procedure SY_AddText(sy: ISch_Component, Data: String); forward;
Procedure SY_AddParam(sy: ISch_Component, Data: String); forward;
Procedure SY_AddComment(sy: ISch_Component, Data: String); forward;
Procedure SY_AddPin(sy: ISch_Component, Data: String); forward;
procedure DeleteSchComponent_1(SchLibDoc : ISch_Document); forward;                                 {* NEW *}
Procedure ImportComponents(InFile: TextFile, Lib: ISch_Document, Errors: TStringList); forward;


{==============================================================================}
{====  String Utility Routines  ===============================================}
{==============================================================================}

function StringContains(BaseStr : String, Srch : String) : Boolean;
Var
    i : Integer;
    SubStr : String;
Begin
    Result := False;
    If Length(BaseStr) < Length(Srch) Then Exit;   // check to make sure the basestring is longer than the search string

    For i := 1 To Length(BaseStr) Do  
    Begin
        SubStr := Copy(BaseStr, i, i+Length(BaseStr));
        SubStr := Copy(SubStr, 1, Length(Srch));
        if StringsEqual(SubStr, Srch) Then
        Begin
            Result := True;
            Exit;
        End;
    End;
End;

Function GetSubstring(InputStr : String, StartPos : Integer, EndPos : Integer) : String;
Var
    SubString : String;
    i : Integer;
    ch : Char;
Begin
    SubString := '';
    for i := StartPos to Length(InputStr) do begin
        SubString := SubString + InputStr[i];
        if i = EndPos then
            break;
    end;
    Result := SubString;
End;

{***************************************************************************
 * function StringIsNumberedDuplicate
 *  Determines if InputString is of the form XXXXXXX_[0-9]. 
 *  Some examples that return True: C1608X5R_2, C1608X5R_3, ICE40LP1K-CB121_10
 *  Examples that return False: C1608X5R_A, C1608X5R2, ICE40LP1K-CB121_10.1  
 *  
 *  Returns:  True if InputString ends with _[0-9], false otherwise
 ***************************************************************************}
function StringIsNumberedDuplicate(const InputString: String);
Var
    IndexOfUnderscore : Integer;
    SubStr : String;
    ExtractedNumber : Integer;
Begin
    Result := False;
    IndexOfUnderscore := GetLastIndexOfChar(InputString, '_');
    If IndexOfUnderscore >=1 then
    Begin
        SubStr := Copy(InputString, IndexOfUnderscore+1, Length(InputString));   // get end of string including underscore
        If StringIsInteger(SubStr) then begin
            Result := True;
            Exit;
        End;
    End;
End;

{***************************************************************************
 * function StringIsInteger
 *  looks at every char in InputString to verify it is a digit [0-9]. if any char is not 0-9, returns false
 *  If this function returns True then can use GetStringFromInteger(InputString) without error
 *  
 *  Returns:  True if every char is [0-9], false otherwise
 ***************************************************************************}
function StringIsInteger(const InputString: string): Boolean;
var
    i: Integer;
begin
    Result := True; // Assume the string is an integer initially
  
    // Check each character in the string
    for i := 1 to Length(InputString) do
    begin
        // If any character is not a digit, then it's not an integer
        if not ((InputString[i] >= '0') and (InputString[i] <= '9')) then
        begin
            Result := False;
            Break;
        end;
    end;
end;

{***************************************************************************
 * function GetLastIndexOfChar
 *  searches InputStr for the last occurrence of SearchChar. 
 *  
 *  Returns:  index of last occurrence of SearchChar if found, if not found returns -1
 ***************************************************************************}
function GetLastIndexOfChar(const InputStr : String, SearchChar: String) : Integer;
Var
    i: Integer;
Begin
    Result := -1; // Initialize result to -1 (indicating not found)

    // Iterate through the string backwards to find the last SearchChar
    for i := Length(InputStr) downto 1 do
    Begin
        if InputStr[i] = SearchChar then
        Begin
            Result := i; // Store the index of the last occurrence of SearchChar
            Exit;       // return, we found the SearchChar
        End;
    End;
End;

Function StringBeginsWith(BaseStr : String, SearchStr : Str) : Boolean;
Var
    i : Integer;
    SubStr : String;
Begin
    Result := False;
    If Length(BaseStr) < Length(SearchStr) Then Exit;    // check to make sure the basestring is longer than the search string

    SubStr := Copy(BaseStr, 0, Length(SearchStr));
    if StringsEqual(SubStr, SearchStr) Then Begin        // compare ignore case
        Result := True;
        Exit;
    End;
End;

{***************************************************************************
 * function StringEndsWith()
 *  See if the larger BaseStr ends with the smaller SearchStr.
 *  
 *  Returns:  True if BaseStr ends with SearchStr, False otherwise.
 ***************************************************************************}
Function StringEndsWith(BaseStr : String, EndStr : Str) : Boolean;
Var
    i : Integer;
    SubStr : String;
    LengthEndStr : String;
    LengthBaseStr : String;
Begin
    Result := False;
    If Length(BaseStr) < Length(EndStr) Then Exit;    // check to make sure the basestring is longer than the search string

    LengthBaseStr := Length(BaseStr);
    LengthEndStr := Length(EndStr);
    SubStr := Copy(BaseStr, LengthBaseStr-LengthEndStr+1, LengthBaseStr);
    if StringsEqual(SubStr, EndStr) Then Begin        // compare ignore case
        Result := True;
        Exit;
    End;
End;

function ReplaceCharInString(InputString : String, OldChar : Char, NewChar : Char);
var
    i : integer;
    ch : Char;
begin
    Result := '';

    for i := 1 to Length(InputString) do
    begin
       ch := InputString[i];
       if AnsiPos(ch, OldChar) = 0 then     // if ch does not appear in String, AnsiPos returns 0
           Result := Result + ch
       else
           Result := Result + NewChar;
    end;
end;

// checks if BaseStr starts with Srch string
Function CheckLeft(BaseStr: String, Srch: String): Boolean;
Var
    i : Integer;
Begin
    Result := False;
    i := Length(Srch);
    If Length(BaseStr) < i Then Exit;
    If Copy(BaseStr, 1, i) = Srch Then Result := True;
End;

// gets all characters to the left of where Srch is in BaseStr
Function LeftOf(BaseStr: String, Srch: String): String;
Var
    i : Integer;
Begin
    i := Pos(Srch, BaseStr);
    If i > 0 Then Begin
        Result := Copy(BaseStr, 1, i - 1);
    End Else Begin
        Result := BaseStr;
    End;
End;

// returns the string to the left of the last occurrence of Srch string
Function LeftOfLast(BaseStr: String, Srch: String): String;
Var
    i, ls : Integer;
    lft, rgt : String;
Begin
    rgt := BaseStr;
    i := Pos(Srch, rgt);
    lft := '';
    If i > 0 Then Begin
        ls := Length(Srch);
        While i > 0 Do Begin
            lft := lft + Copy(rgt, 1, i - 1);
            rgt := Copy(rgt, i + ls, Length(rgt) - i + ls);
            i := Pos(Srch, rgt);
            If i > 0 then Begin
                lft := lft + Srch;
            End;
        End;
        Result := lft;
    End Else Begin
        Result := BaseStr;
    End;
End;

Function RightOf(BaseStr: String, Srch: String): String;
Var
    i, ls : Integer;
Begin
    i := Pos(Srch, BaseStr);
    If i > 0 Then Begin
        ls := Length(Srch);
        Result := Copy(BaseStr, i + ls, Length(BaseStr) - i + ls);
    End Else Begin
        Result := '';
    End;
End;

// splits BaseStr into left and right, splitting by position of first Srch string
Procedure StrChop(BaseStr: String, Srch: String, Out LeftSide: String, Out RightSide: String);
Var
    i, ls : Integer;
Begin
    i := Pos(Srch, BaseStr);
    If i <= 0 Then Begin
        LeftSide := BaseStr;
        RightSide := '';
    End Else Begin
        ls := Length(Srch);
        LeftSide := Copy(BaseStr, 1, i - 1);
        RightSide := Copy(BaseStr, i + ls, Length(BaseStr) - i + ls);
    End;
End;

Function GetBetween(BaseStr: String, StartStr: String, EndStr: String): String;
Begin
    Result := Leftof(RightOf(BaseStr, StartStr), EndStr);
End;

Function GetFileLocation(FilePath: String): String;
Var
    i : Integer;
    filename : String;
Begin
    filename := RightOf(FilePath, '\');
    i := Pos('\', filename);
    While i > 0 Do Begin
        filename := RightOf(filename, '\');
        i := Pos('\', filename);
    End;
    Result := LeftOf(Filepath, filename);
End;

{***************************************************************************
 * function ExtractBaseFileName()
 *  
 *  Returns:  returns the filename without extension.
 ***************************************************************************}
Function ExtractBaseFileName(const AbsolutePath: string) : Widestring;
var
    FileName: string;
    DotPos: Integer;
begin
    // Extract the file name with extension from the full path
    FileName := ExtractFileName(AbsolutePath);
    
    // Find the position of the last dot in the file name
    DotPos := LastDelimiter('.', FileName);
  
    // Extract the base file name (without the extension)
    if DotPos > 0 then begin
        Result := Copy(FileName, 1, DotPos - 1);
    end
    else begin
        Result := FileName; // No extension found, return the full file name
    end;
End;

{==============================================================================}
{====  Footprint Routines  ====================================================}
{==============================================================================}

Function LayerFromString(LName: String): TLayer;
Begin
    Case LName Of
    'NoLayer':         Result := eNoLayer;
    'TopLayer':        Result := eTopLayer;
    'MidLayer1':       Result := eMidLayer1;
    'MidLayer2':       Result := eMidLayer2;
    'MidLayer3':       Result := eMidLayer3;
    'MidLayer4':       Result := eMidLayer4;
    'MidLayer5':       Result := eMidLayer5;
    'MidLayer6':       Result := eMidLayer6;
    'MidLayer7':       Result := eMidLayer7;
    'MidLayer8':       Result := eMidLayer8;
    'MidLayer9':       Result := eMidLayer9;
    'MidLayer10':      Result := eMidLayer10;
    'MidLayer11':      Result := eMidLayer11;
    'MidLayer12':      Result := eMidLayer12;
    'MidLayer13':      Result := eMidLayer13;
    'MidLayer14':      Result := eMidLayer14;
    'MidLayer15':      Result := eMidLayer15;
    'MidLayer16':      Result := eMidLayer16;
    'MidLayer17':      Result := eMidLayer17;
    'MidLayer18':      Result := eMidLayer18;
    'MidLayer19':      Result := eMidLayer19;
    'MidLayer20':      Result := eMidLayer20;
    'MidLayer21':      Result := eMidLayer21;
    'MidLayer22':      Result := eMidLayer22;
    'MidLayer23':      Result := eMidLayer23;
    'MidLayer24':      Result := eMidLayer24;
    'MidLayer25':      Result := eMidLayer25;
    'MidLayer26':      Result := eMidLayer26;
    'MidLayer27':      Result := eMidLayer27;
    'MidLayer28':      Result := eMidLayer28;
    'MidLayer29':      Result := eMidLayer29;
    'MidLayer30':      Result := eMidLayer30;
    'BottomLayer':     Result := eBottomLayer;
    'TopOverlay':      Result := eTopOverlay;
    'BottomOverlay':   Result := eBottomOverlay;
    'TopPaste':        Result := eTopPaste;
    'BottomPaste':     Result := eBottomPaste;
    'TopSolder':       Result := eTopSolder;
    'BottomSolder':    Result := eBottomSolder;
    'InternalPlane1':  Result := eInternalPlane1;
    'InternalPlane2':  Result := eInternalPlane2;
    'InternalPlane3':  Result := eInternalPlane3;
    'InternalPlane4':  Result := eInternalPlane4;
    'InternalPlane5':  Result := eInternalPlane5;
    'InternalPlane6':  Result := eInternalPlane6;
    'InternalPlane7':  Result := eInternalPlane7;
    'InternalPlane8':  Result := eInternalPlane8;
    'InternalPlane9':  Result := eInternalPlane9;
    'InternalPlane10': Result := eInternalPlane10;
    'InternalPlane11': Result := eInternalPlane11;
    'InternalPlane12': Result := eInternalPlane12;
    'InternalPlane13': Result := eInternalPlane13;
    'InternalPlane14': Result := eInternalPlane14;
    'InternalPlane15': Result := eInternalPlane15;
    'InternalPlane16': Result := eInternalPlane16;
    'DrillGuide':      Result := eDrillGuide;
    'KeepOutLayer':    Result := eKeepOutLayer;
    'Mechanical1':     Result := eMechanical1;
    'Mechanical2':     Result := eMechanical2;
    'Mechanical3':     Result := eMechanical3;
    'Mechanical4':     Result := eMechanical4;
    'Mechanical5':     Result := eMechanical5;
    'Mechanical6':     Result := eMechanical6;
    'Mechanical7':     Result := eMechanical7;
    'Mechanical8':     Result := eMechanical8;
    'Mechanical9':     Result := eMechanical9;
    'Mechanical10':    Result := eMechanical10;
    'Mechanical11':    Result := eMechanical11;
    'Mechanical12':    Result := eMechanical12;
    'Mechanical13':    Result := eMechanical13;
    'Mechanical14':    Result := eMechanical14;
    'Mechanical15':    Result := eMechanical15;
    'Mechanical16':    Result := eMechanical16;
    'DrillDrawing':    Result := eDrillDrawing;
    'MultiLayer':      Result := eMultiLayer;
    'ConnectLayer':    Result := eConnectLayer;
    'BackGroundLayer': Result := eBackGroundLayer;
    'DRCErrorLayer':   Result := eDRCErrorLayer;
    'HighlightLayer':  Result := eHighlightLayer;
    'GridColor1':      Result := eGridColor1;
    'GridColor10':     Result := eGridColor10;
    'PadHoleLayer':    Result := ePadHoleLayer;
    'ViaHoleLayer':    Result := eViaHoleLayer;
    Else
        Result := eNoLayer;
    End;
End;


Procedure FP_AddStep(fp: IPCB_LibComponent, Data: String);
Var
   STEPFileName   : String;
   STEPmodel      : IPCB_ComponentBody;
   Model          : IPCB_Model;
Begin
    STEPFileName := ImportDir + GetBetween(Data, '(Name ', ')');
    STEPmodel := PcbServer.PCBObjectFactory(eComponentBodyObject,eNoDimension,eCreate_Default);
    Model := STEPmodel.ModelFactory_FromFilename(STEPFileName, false);
    STEPmodel.SetState_FromModel;
    //   Model.SetState(0,0,0,0);
    STEPmodel.Model := Model;
    fp.AddPCBObject(STEPmodel);
End;

Procedure FP_AddLine(fp: IPCB_Component, Data: String);
Var
    lin : IPCB_track;
    s1, s2 : String;
Begin
    lin := PCBServer.PCBObjectFactory(eTrackObject, eNoDimension, eCreate_Default);
    If lin = Nil Then Exit;
    StrChop(GetBetween(Data, '(Start ', ')'), ',', s1, s2);
    lin.X1 := MilsToCoord(Evaluate(s1));
    lin.Y1 := MilsToCoord(Evaluate(s2));
    StrChop(GetBetween(Data, '(End ', ')'), ',', s1, s2);
    lin.X2 := MilsToCoord(Evaluate(s1));
    lin.Y2 := MilsToCoord(Evaluate(s2));
    lin.Layer := LayerFromString(GetBetween(Data, '(Layer ', ')'));
    lin.Width := MilsToCoord(Evaluate(GetBetween(Data, '(Width ', ')')));
    fp.AddPCBObject(lin);
    PCBServer.SendMessageToRobots(fp.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, lin.I_ObjectAddress);
End;

Procedure FP_AddArc(fp: IPCB_Component, Data: String);
Var
    arc : IPCB_Arc;
    s1, s2 : String;
Begin
    arc := PCBServer.PCBObjectFactory(eArcObject, eNoDimension, eCreate_Default);
    If arc = Nil Then Exit;
    StrChop(GetBetween(Data, '(Location ', ')'), ',', s1, s2);
    arc.XCenter := MilsToCoord(Evaluate(s1));
    arc.YCenter := MilsToCoord(Evaluate(s2));
    arc.Radius := MilsToCoord(Evaluate(GetBetween(Data, '(Radius ', ')')));
    arc.LineWidth := MilsToCoord(Evaluate(GetBetween(Data, '(Width ', ')')));
    arc.StartAngle := Evaluate(GetBetween(Data, '(StartAngle ', ')'));
    arc.EndAngle := Evaluate(GetBetween(Data, '(EndAngle ', ')'));
    arc.Layer := LayerFromString(GetBetween(Data, '(Layer ', ')'));;
    fp.AddPCBObject(arc);
    PCBServer.SendMessageToRobots(fp.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, arc.I_ObjectAddress);
End;

Procedure FP_AddPoly(fp: IPCB_Component, Data: String, InFile: TextFile);
Var
    pol : IPCB_Region;
    cont : IPCB_Contour;
    pc: Integer;
    s1, s2, inp, tag : String;
Begin
    pol := PCBServer.PCBObjectFactory(eRegionObject, eNoDimension,eCreate_Default);
    If pol = Nil Then Exit;
    cont := pol.MainContour.Replicate();
    pol.Layer := LayerFromString(GetBetween(Data, '(Layer ', ')'));
    cont.Count := Evaluate(GetBetween(Data, '(PointCount ', ')'));
    pc := 0;
    While Not EOF(InFile) Do Begin
        ReadLn(InFile, inp);
        If VarIsNull(inp) Then Continue;
        inp := Trim(inp);
        StrChop(inp, ' ', tag, inp);
        tag := Trim(tag);
        Case tag Of
        'Point': Begin
            pc := pc + 1;
            StrChop(GetBetween(inp, '(', ')'), ',', s1, s2);
            cont.X[pc] := MilsToCoord(Evaluate(s1));
            cont.Y[pc] := MilsToCoord(Evaluate(s2));
            End;
        'EndPolygon': Break;
        Else Begin
            ShowMessage('Keyword Error: ' + tag);
            End;
        End;
    End;
    pol.SetOutlineContour(cont);
    If GetBetween(Data, '(Type ', ')') = 'KeepOut' Then Begin
        pol.IsKeepout := True;
    End;
    fp.AddPCBObject(pol);
    PCBServer.SendMessageToRobots(fp.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, pol.I_ObjectAddress);
End;

Procedure FP_AddText(fp: IPCB_Component, Data: STring);
Var
    txt : IPCB_Text;
    s1, s2 : String;
Begin
    txt := PCBServer.PCBObjectFactory(eTextObject, eNoDimension, eCreate_Default);
    If txt = Nil Then Exit;
    StrChop(GetBetween(Data, '(Location ', ')'), ',', s1, s2);
    txt.XLocation := MilsToCoord(Evaluate(s1));
    txt.YLocation := MilsToCoord(Evaluate(s2));
    txt.Layer := LayerFromString(GetBetween(Data, '(Layer ', ')'));
    txt.Size := MilsToCoord(Evaluate(GetBetween(Data, '(Height ', ')')));
    txt.Width := MilsToCoord(Evaluate(GetBetween(Data, '(Width ', ')')));
    If GetBetween(Data, '(Mirrored ', ')') = 'True' Then Begin
        txt.MirrorFlag := True;
    End;
    txt.Rotation := Evaluate(GetBetween(Data, '(Rotation ', ')'));
    txt.Text := GetBetween(Data, '(Value "', '")');
    // Justification? NOTE: TODO:
    fp.AddPCBObject(txt);
    PCBServer.SendMessageToRobots(fp.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, txt.I_ObjectAddress);
End;

Procedure FP_AddPad(fp: IPCB_Component, Data: String, InFile: TextFile);
Var
    s1, s2, inp, tag, lay : String;
    pad : IPCB_Pad;
    padsh : TShape;
    cache : TPadCache;
Begin
    pad := PcbServer.PCBObjectFactory(ePadObject, eNoDimension, eCreate_Default);
    pad.Name := GetBetween(Data, '(Name "', '")');
//    pad.Layer := LayerFromString(GetBetween(Data, '(Layer ', ')'));
    StrChop(GetBetween(Data, '(Location ', ')'), ',', s1, s2);
    pad.X := MilsToCoord(Evaluate(s1));
    pad.Y := MilsToCoord(Evaluate(s2));
    pad.Rotation := Evaluate(GetBetween(Data, '(Rotation ', ')')); // 2010-07-06 gbn
//    pad.Mode := ePadMode_LocalStack; // ePadMode_Simple, ePadMode_ExternalStack
    s1 := GetBetween(Data, '(ExpandPaste ', ')');
    s2 := GetBetween(Data, '(ExpandMask ', ')');
    If s1 <> '' || s2 <> '' Then Begin
       cache := pad.Cache;
       If s1 <> '' Then Begin
          cache.PasteMaskExpansionValid := eCacheManual;
          cache.PasteMaskExpansion := MilsToCoord(Evaluate(s1));
       End;
       If s2 <> '' Then Begin
          cache.SolderMaskExpansionValid := eCacheManual;
          cache.SolderMaskExpansion := MilsToCoord(Evaluate(s2));
       End;
       pad.Cache := cache;
    End;
    If GetBetween(Data, '(Surface ', ')') = 'True' Then Begin
        pad.Mode := ePadMode_Simple;
        pad.Layer := eTopLayer;
    End Else Begin
        pad.Mode := ePadMode_LocalStack;
    End;
    pad.Moveable := False;
    pad.HoleType := eRoundHole; // eSquareHole, eSlotHole
    pad.HoleSize := MilsToCoord(Evaluate(GetBetween(Data, '(HoleSize ', ')')));
    //2020-11-18 JRR Start; Let's set the Plated value, if present
    If GetBetween(Data, '(Plated ', ')') <> '' Then Begin
        pad.Plated := (GetBetween(Data, '(Plated ', ')')='True');
    End;
    //2020-11-18 JRR End
    While Not EOF(InFile) Do Begin
        ReadLn(InFile, inp);
        If VarIsNull(inp) Then Continue;
        inp := Trim(inp);
        StrChop(inp, ' ', tag, inp);
        tag := Trim(tag);
        Case tag Of
        'PadShape': Begin
            padsh := eNoShape;
            StrChop(GetBetween(inp, '(Size ', ')'), ',', s1, s2);
            Case GetBetween(inp, '(Shape ', ')') Of
            'NoShape': padsh := eNoShape;
            'Rounded': padsh := eRounded;
            'Rectangular': padsh := eRectangular;
            'Octagonal': padsh := eOctagonal;
            'CircleShape': padsh := eCircleShape;
            'ArcShape': padsh := eArcShape;
            'Terminator': padsh := eTerminator;
            'RoundedRectangle': padsh := eRoundedRectangular;
            'RotatedRectangle': padsh := eRotatedRectShape;
            Else padsh := eNoShape;
            End;
            lay := GetBetween(inp, '(Layer ', ')');
            If CheckLeft(lay, 'Top') Then Begin
                pad.TopShape := padsh;
                pad.TopXSize := MilsToCoord(Evaluate(s1));
                pad.TopYSize := MilsToCoord(Evaluate(s2));
            End Else If CheckLeft(lay, 'Mid') Then Begin
                pad.MidShape := padsh;
                pad.MidXSize := MilsToCoord(Evaluate(s1));
                pad.MidYSize := MilsToCoord(Evaluate(s2));
            End Else If CheckLeft(lay, 'Bot') Then Begin
                pad.BotShape := padsh;
                pad.BotXSize := MilsToCoord(Evaluate(s1));
                pad.BotYSize := MilsToCoord(Evaluate(s2));
            End;
            End;
        'EndPad': Begin
            Break;
            End;
        Else Begin
            ShowMessage('Keyword Error: ' + tag);
            End;
        End;
    End;
    fp.AddPCBObject(pad);
    PCBServer.SendMessageToRobots(fp.I_ObjectAddress, c_Broadcast, PCBM_BoardRegisteration, pad.I_ObjectAddress);
End;

procedure DeletePcbComponent_1(PcbLib : IPCB_Library);
Var
    Comp1Footprint : IPCB_Component;  // IPcb_LibComponent;
Begin
    Comp1Footprint := PcbLib.GetComponentByName('PCBCOMPONENT_1');
    if Comp1Footprint <> nil then
    begin
        PcbLib.DeRegisterComponent(Comp1Footprint);
        PcbLib.RemoveComponent(Comp1Footprint);
    end;
End;

Procedure ImportFootprints(InFile: TextFile, Lib: IPCB_Library, Errors: TStringList);
Var
    inp, tag, s, t : String;
    fp : IPCB_Component;
Begin
    While Not EOF(InFile) Do Begin
        ReadLn(InFile, inp);
        If VarIsNull(inp) Then Continue;
        inp := Trim(inp);   { trim whitespace }

        StrChop(inp, ' ', tag, inp);
        tag := Trim(tag);
        Case tag Of
        'Footprint': Begin
            // create a footprint reference
            fp := PCBServer.CreatePCBLibComp();
            If fp = Nil Then Begin
                Errors.Add('Error creating footprint.');
                Break;
            End;
            // add data to it
            fp.Name := GetBetween(inp, '(Name "', '")');
            // assign it to library
            Lib.RegisterComponent(fp);
            PCBServer.PreProcess();
            // add data to it
            fp.BeginModify();
            // set height
            t := GetBetween(inp, '(Height ', ')');
            If t <> '' Then Begin
                fp.Height := MilsToCoord(Evaluate(t));
            End;

            While Not EOF(InFile) Do Begin
                ReadLn(InFile, inp);
                If VarIsNull(inp) Then Continue;
                inp := Trim(inp);
                If CheckLeft(inp, '#') Then Continue;

                StrChop(inp, ' ', tag, inp);
                tag := Trim(tag);
                Case tag Of
                'Pad': Begin
                    FP_AddPad(fp, inp, InFile);
                    End;
                'Line': Begin
                    FP_AddLine(fp, inp);
                    End;
                'Arc': Begin
                    FP_AddArc(fp, inp);
                    End;
                'Polygon': Begin
                    FP_AddPoly(fp, inp, InFile);
                    End;
                'Text': Begin
                    FP_AddText(fp, inp);
                    End;
                'Step': Begin
                    FP_AddStep(fp, inp);
                    End;
                'EndFootprint': Begin
                    //ShowMessage('EndFootprint');
                    Break;
                    End;
                '': Continue;
                Else Begin
                    ShowMessage('Keyword Error: ' + tag);
                    Break;
                    End;
                End;
            End; // while not eof()
            fp.EndModify();
            PCBServer.PostProcess();
            // done with footprint
            End;
        'EndFootprints': Begin
            //ShowMessage('EndFootprint');
            Break;
            End;
        '': Continue;
        Else Begin
            ShowMessage('Keyword Error: ' + tag);
            Break;
            End;
        End; // case tag
    End; // while not eof()
    PCBServer.PostProcess();
End;

{==============================================================================}
{====  Symbol Routines  =======================================================}
{==============================================================================}

Function TextJustificationFromString(Value: String): TTextJustification;
Begin
    Case Value Of
    'BottomLeft':   Result := eJustify_BottomLeft;
    'BottomCenter': Result := eJustify_BottomCenter;
    'BottomRight': Result := eJustify_BottomRight;
    'CenterLeft': Result := eJustify_CenterLeft;
    'Center': Result := eJustify_Center;
    'CenterRight': Result := eJustify_CenterRight;
    'TopLeft': Result := eJustify_TopLeft;
    'TopCenter': Result := eJustify_TopCenter;
    'TopRight': Result := eJustify_TopRight;
    Else Result := eJustify_Center;
    End;
End;

Function SY_GetFont(Height: Double, Angle: TRotationBy90): TFontID;
Var
    sz : Integer;
    m1, ob : String;
    fontid : String;
Begin

    { sz := Round(Height / 10); }
    sz := (Height * 0.1);   // UltraLibrarian gives a height parameter in import txt file, which is 10x actual font size

    If BrokenSCHFontManager = True Then Begin
        Result := ForceSchFontID;
        Exit;
    End;
    // 2019-01-16 gbn
    Result := SchServer.FontManager.GetFontID(sz, Angle, False, False, False, False, DEFAULT_FONT);
End;

// Function to get font id by font name and font size
Function SY_GetFontId(FontName: TDynamicString, FontSize : Integer) : TFontID;
Begin
    Result    := SchServer.FontManager.GetFontID(FontSize,
                                                 0,         // rotation
                                                 False,     // underline
                                                 False,     // italic
                                                 False,     // Bold
                                                 False,     // StrikeOut
                                                 FontName);
End;

Function SY_GetAngle(Angle : String): TRotationBy90;
Begin
    Case Angle Of
    '90': Result := eRotate90;
    '180': Result := eRotate180;
    '270': Result := eRotate270;
    Else Result := eRotate0;
    End;
End;

Procedure SY_AddLine(sy: ISch_Component, Data: String);
Var
    lin : ISch_Line;
    s1, s2 ,s3: String;
Begin
    lin := SchServer.SchObjectFactory(eLine, eCreate_Default);
    If lin = Nil Then Exit;
    StrChop(GetBetween(Data, '(Start ', ')'), ',', s1, s2);
    lin.Location := Point(MilsToCoord(Evaluate(s1)), MilsToCoord(Evaluate(s2)));
    StrChop(GetBetween(Data, '(End ', ')'), ',', s1, s2);
    lin.Corner := Point(MilsToCoord(Evaluate(s1)), MilsToCoord(Evaluate(s2)));
    GetBetween(Data, 'Width ', ')');
    If s3 < 10 Then Begin
        lin.LineWidth := eSmall;
    End;
    If s3 > 10 Then Begin
        lin.LineWidth := eMedium;
    End;
    If s3 > 12 Then Begin
        lin.LineWidth := eLarge;
    End;
    lin.LineStyle := eLineStyleSolid;
    lin.Color := COLOR_BLUE;
    lin.OwnerPartId := Evaluate(GetBetween(Data, '(Part ', ')'));
    lin.OwnerPartDisplayMode := sy.DisplayMode;
    sy.AddSchObject(lin);
    SchServer.RobotManager.SendMessage(sy.I_ObjectAddress, c_BroadCast, SCHM_PrimitiveRegistration, lin.I_ObjectAddress);
End;

Procedure SY_AddRect(sy: ISch_Component, Data: String);
Var
    rect : ISch_Rectangle;
    s1, s2 ,s3: String;
Begin
    rect := SchServer.SchObjectFactory(eRectangle, eCreate_Default);
    If rect = Nil Then Exit;
    StrChop(GetBetween(Data, '(Start ', ')'), ',', s1, s2);
    rect.Location := Point(MilsToCoord(Evaluate(s1)), MilsToCoord(Evaluate(s2)));
    StrChop(GetBetween(Data, '(End ', ')'), ',', s1, s2);
    rect.Corner := Point(MilsToCoord(Evaluate(s1)), MilsToCoord(Evaluate(s2)));
    GetBetween(Data, 'Width ', ')');
    s3 := GetBetween(Data, '(Width ', ')');
    If s3 < 10 Then Begin
        rect.LineWidth := eSmall;
    End;
    If s3 > 10 Then Begin
        rect.LineWidth := eMedium;
    End;
    If s3 > 12 Then Begin
        rect.LineWidth := eLarge;
    End;
    rect.Transparent := True;
    rect.Color := COLOR_BLUE;
    rect.OwnerPartId := Evaluate(GetBetween(Data, '(Part ', ')'));
    rect.OwnerPartDisplayMode := sy.DisplayMode;
    sy.AddSchObject(rect);
    SchServer.RobotManager.SendMessage(sy.I_ObjectAddress, c_BroadCast, SCHM_PrimitiveRegistration, rect.I_ObjectAddress);
End;

Procedure SY_AddArc(sy: ISch_Component, Data: String);
Var
    arc : ISch_Arc;
    s1, s2, s3 : String;
Begin
    arc := SchServer.SchObjectFactory(eArc, eCreate_Default);
    If arc = Nil Then Exit;
    StrChop(GetBetween(Data, '(Location ', ')'), ',', s1, s2);
    arc.Location := Point(MilsToCoord(Evaluate(s1)), MilsToCoord(Evaluate(s2)));
    arc.Radius := MilsToCoord(Evaluate(GetBetween(Data, '(Radius ', ')')));
    s3 := GetBetween(Data, '(Width ', ')');
    If s3 < 10 Then Begin
        arc.LineWidth := eSmall;
    End;
    If s3 > 10 Then Begin
        arc.LineWidth := eMedium;
    End;
    If s3 > 12 Then Begin
        arc.LineWidth := eLarge;
    End;
    arc.Color := COLOR_BLUE;
    arc.StartAngle := Evaluate(GetBetween(Data, '(StartAngle ', ')'));
    arc.EndAngle := Evaluate(GetBetween(Data, '(EndAngle ', ')'));
    arc.OwnerPartId := Evaluate(GetBetween(Data, '(Part ', ')'));
    arc.OwnerPartDisplayMode := sy.DisplayMode;
    sy.AddSchObject(arc);
    SchServer.RobotManager.SendMessage(sy.I_ObjectAddress, c_BroadCast, SCHM_PrimitiveRegistration, arc.I_ObjectAddress);
End;

Procedure SY_AddPoly(sy: ISch_Component, Data: String, InFile: TextFile);
Var
    pol : ISch_Polygon;
    pc: Integer;
    s1, s2, inp, tag : String;
Begin
    pol := SchServer.SchObjectFactory(ePolygon, eCreate_Default);
    If pol = Nil Then Exit;

    pol.VerticesCount := Evaluate(GetBetween(Data, '(PointCount ', ')'));
    pc := 0;
    While Not EOF(InFile) Do Begin
        ReadLn(InFile, inp);
        If VarIsNull(inp) Then Continue;
        inp := Trim(inp);
        StrChop(inp, ' ', tag, inp);
        tag := Trim(tag);
        Case tag Of
        'Point': Begin
            pc := pc + 1;
            StrChop(GetBetween(inp, '(', ')'), ',', s1, s2);
            pol.Vertex[pc] := Point(MilsToCoord(Evaluate(s1)), MilsToCoord(Evaluate(s2)));
            End;
        'EndPolygon': Break;
        Else Begin
            ShowMessage('Keyword Error: ' + tag);
            End;
        End;
    End;
    pol.LineWidth := eZeroSize; // NOTE: TODO:
    pol.Color := COLOR_BLUE;
    pol.IsSolid := True;
    pol.OwnerPartId := Evaluate(GetBetween(Data, '(Part ', ')'));
    pol.OwnerPartDisplayMode := sy.DisplayMode;
    sy.AddSchObject(pol);
    SchServer.RobotManager.SendMessage(sy.I_ObjectAddress, c_BroadCast, SCHM_PrimitiveRegistration, pol.I_ObjectAddress);
End;

Procedure SY_AddText(sy: ISch_Component, Data: String);
Var 
    txt : ISch_Label;
    s1, s2 : String;
Begin
    txt := SchServer.SchObjectFactory(eLabel, eCreate_Default);
    If txt = Nil Then Exit;
    StrChop(GetBetween(Data, '(Location ', ')'), ',', s1, s2);
    txt.Location := Point(MilsToCoord(Evaluate(s1)), MilsToCoord(Evaluate(s2)));
    If GetBetween(Data, '(Mirrored ', ')') = 'True' Then Begin
        txt.IsMirrored := True;
    End;
    txt.Orientation := SY_GetAngle(GetBetween(Data, '(Rotation ', ')'));
    txt.FontID := SY_GetFont(Evaluate(GetBetween(Data, 'Height ', ')')), txt.Orientation);
    txt.Justification := TextJustificationFromString(GetBetween(Data, '(Justification ', ')'));
    txt.Color := COLOR_BLACK;
    txt.Text := GetBetween(Data, '(Value "', '")');
    txt.OwnerPartId := Evaluate(GetBetween(Data, '(Part ', ')'));
    txt.OwnerPartDisplayMode := sy.DisplayMode;
    sy.AddSchObject(txt);
    SchServer.RobotManager.SendMessage(sy.I_ObjectAddress, c_BroadCast, SCHM_PrimitiveRegistration, txt.I_ObjectAddress);
End;

Procedure SY_AddParam(sy: ISch_Component, Data: String);
Var
    prm : ISch_Parameter;
    s1, s2: String;
Begin
    prm := SchServer.SchObjectFactory(eParameter, eCreate_Default);
    If prm = Nil Then Exit;
    prm.IsHidden := True;
    If GetBetween(Data, '(Name ', '"') = 'Visible' Then Begin
        prm.IsHidden := False;
    End;
    prm.Name := GetBetween(Data, '(Name "', '")');
    StrChop(GetBetween(Data, '(Location ', ')'), ',', s1, s2);
    prm.Location := Point(MilsToCoord(Evaluate(s1)), MilsToCoord(Evaluate(s2)));
    If GetBetween(Data, '(Mirrored ', ')') = 'True' Then Begin
        prm.IsMirrored := True;
    End;
    prm.Orientation := SY_GetAngle(GetBetween(Data, '(Rotation ', ')'));
    prm.FontID := SY_GetFont(Evaluate(GetBetween(Data, 'Height ', ')')), prm.Orientation);
    prm.Justification := TextJustificationFromString(GetBetween(Data, '(Justification ', ')'));
    prm.Color := COLOR_BLACK;
    prm.Text := GetBetween(Data, '(Value "', '")');
    prm.OwnerPartId := Evaluate(GetBetween(Data, '(Part ', ')'));
    prm.OwnerPartDisplayMode := sy.DisplayMode;
    sy.AddSchObject(prm);
    SchServer.RobotManager.SendMessage(sy.I_ObjectAddress, c_BroadCast, SCHM_PrimitiveRegistration, prm.I_ObjectAddress);
End;

Procedure SY_AddComment(sy: ISch_Component, Data: String);
Var
    prm : ISch_Parameter;
    s1, s2: String;
Begin
    prm := SchServer.SchObjectFactory(eParameter, eCreate_Default);
    If prm = Nil Then Exit;
    prm.IsHidden := True;
    If GetBetween(Data, '(Name ', '"') = 'Visible' Then Begin
        prm.IsHidden := False;
    End;
    prm.Name := GetBetween(Data, '(Name "', '")');
    StrChop(GetBetween(Data, '(Location ', ')'), ',', s1, s2);
    prm.Location := Point(MilsToCoord(Evaluate(s1)), MilsToCoord(Evaluate(s2)));
    If GetBetween(Data, '(Mirrored ', ')') = 'True' Then Begin
        prm.IsMirrored := True;
    End;
    prm.Orientation := SY_GetAngle(GetBetween(Data, '(Rotation ', ')'));
    prm.FontID := SY_GetFont(Evaluate(GetBetween(Data, 'Height ', ')')), prm.Orientation);
    prm.Justification := TextJustificationFromString(GetBetween(Data, '(Justification ', ')'));
    prm.Color := COLOR_BLACK;
    prm.Text := GetBetween(Data, '(Value "', '")');
    prm.OwnerPartId := Evaluate(GetBetween(Data, '(Part ', ')'));
    prm.OwnerPartDisplayMode := sy.DisplayMode;
    sy.Comment := prm; // crashes, as of Altium 16.0.5
    SchServer.RobotManager.SendMessage(sy.I_ObjectAddress, c_BroadCast, SCHM_PrimitiveRegistration, prm.I_ObjectAddress);
End;

Procedure SY_AddPin(sy: ISch_Component, Data: String);
Var
    pin : ISch_Pin;
    s1, s2 : String;
    Font_ID : Integer;
Begin
    pin := SchServer.SchObjectFactory(ePin, eCreate_Default);
    If pin = Nil Then Exit;

    // Define the pin parameters.
    StrChop(GetBetween(Data, '(Location ', ')'), ',', s1, s2);
    pin.Location := Point(MilsToCoord(Evaluate(s1)), MilsToCoord(Evaluate(s2)));
    pin.Color := COLOR_BLACK;
    pin.Orientation := SY_GetAngle(GetBetween(Data, '(Rotation ', ')'));
    Case GetBetween(Data, '(PinType ', ')') Of
    'IO': pin.Electrical := eElectricIO;
    'Input': pin.Electrical := eElectricInput;
    'Output': pin.Electrical := eElectricOutput;
    'Passive': pin.Electrical := eElectricPassive;
    'OpenCollector': pin.Electrical := eElectricOpenCollector;
    'OpenEmitter': pin.Electrical := eElectricOpenEmitter;
    'HiZ': pin.Electrical := eElectricHiZ;
    'Power': pin.Electrical := eElectricPower;
    Else pin.Electrical := eElectricPassive;
    End;
    pin.PinLength := MilsToCoord(Evaluate(GetBetween(Data, '(Length ', ')')));
    pin.SwapId_Pin :=  GetBetween(Data, '(PinSwap ', ')');
    pin.SwapId_Part := GetBetween(Data, '(PartSwap ', ')');
    pin.SwapId_PartPin := GetBetween(Data, '(PinSeq ', ')');
    s1 := GetBetween(Data, '(Designator ', '")');
    pin.ShowDesignator := CheckLeft(s1, 'Visible');
    pin.Designator := RightOf(s1, '"');
    s1 := GetBetween(Data, '(Name ', '")');
    pin.ShowName := CheckLeft(s1, 'Visible');
    pin.Name := RightOf(s1, '"');
    pin.OwnerPartId := Evaluate(GetBetween(Data, '(Part ', ')'));

    Pin.Name_FontMode := 1; // sets the FontMode to 'custom'
    pin.Name_CustomFontID := SchServer.FontManager.GetFontID(PIN_FONT_SIZE, // font size
                                                 0,         // rotation
                                                 False,     // underline
                                                 False,     // italic
                                                 False,     // Bold
                                                 False,     // StrikeOut
                                                 PIN_FONT_NAME);
    Pin.Designator_FontMode := 1; // sets FontMode to 'custom'
    pin.Designator_CustomFontId := SchServer.FontManager.GetFontID(PIN_FONT_SIZE, // font size
                                                 0,         // rotation
                                                 False,     // underline
                                                 False,     // italic
                                                 False,     // Bold
                                                 False,     // StrikeOut
                                                 PIN_FONT_NAME);

    pin.OwnerPartDisplayMode := sy.DisplayMode;
    sy.AddSchObject(pin);
    SchServer.RobotManager.SendMessage(sy.I_ObjectAddress, c_BroadCast, SCHM_PrimitiveRegistration, pin.I_ObjectAddress);
End;

procedure DeleteSchComponent_1(SchLibDoc : ISch_Document);
var
    Component  : IComponent;
    ComponentName  : Widestring;
    SchIterator      : ISch_Iterator;
Begin
    SchIterator := SchLibDoc.SchLibIterator_Create;
    SchIterator.SetState_FilterAll;
    SchIterator.AddFilter_ObjectSet(MkSet(eSchComponent));
    Component := SchIterator.FirstSchObject;
    while (Component <> Nil) Do
    begin
        ComponentName := Component.LibReference;
        if StringContains(ComponentName, 'Component_1') then
        Begin
            SchLibDoc.RemoveSchComponent(Component);
        End;
        Component := SchIterator.NextSchObject;
    end;
    SchLibDoc.SchIterator_Destroy(SchIterator);
End;

{ Import Schematic Components }
Procedure ImportComponents(InFile: TextFile, Lib: ISch_Document, Errors: TStringList);
Var
    inp, tag, s, t : String;
    sy : ISch_Component;
    simp : ISch_Implementation;
Begin
    While Not EOF(InFile) Do Begin
        ReadLn(InFile, inp);
        If VarIsNull(inp) Then Continue;

        StrChop(inp, ' ', tag, inp);
        tag := Trim(tag);
        Case tag Of
        'Component': Begin
            // create a component reference
            sy := SchServer.SchObjectFactory(eSchComponent, eCreate_Default);
            If sy = Nil Then Begin
                Errors.Add('Error creating component.');
                Break;
            End;
            // Set up parameters for the library component.
            SchServer.ProcessControl.PreProcess(Lib, '');
            // Define the LibReference and add the component to the library.
            sy.LibReference := GetBetween(inp, '(Name "', '")');
            sy.Designator.Text := GetBetween(inp, '(DesPrefix "', '")');
            sy.ComponentDescription := 'Imported';
            sy.PartCount := Evaluate(GetBetween(inp, '(PartCount ', ')'));
            sy.CurrentPartId := 1;

            // add data to it
            While Not EOF(InFile) Do Begin
                ReadLn(InFile, inp);
                If VarIsNull(inp) Then Continue;
                StrChop(inp, ' ', tag, inp);
                Case tag Of
                'Description': Begin
                    //sy.SourceDescription := GetBetween(inp, '(Value "', '")'); ' SourceDescription doesnt exist (as of 16.0.5)
                    sy.ComponentDescription := GetBetween(inp, '(Value "', '")');
                    End;
                'Comment': Begin
                    //sy.Comment.UnderlyingString := GetBetween(inp, '(Value "', '")'); ' Comment.UnderlyingString doesnt exist (as of 16.0.5)
                    //sy.Comment.DisplayString := GetBetween(inp, '(Value "', '")'); // crashes
                    //SY_AddComment(sy, inp); // crashes (see function)
                    sy.Comment.text :=getbetween(inp, '(Value "', '")'); 
                    End;
                'Parameter': Begin
                    SY_AddParam(sy, inp);
                    End;
                'Pin': Begin
                    SY_AddPin(sy, inp);
                    End;
                'Line': Begin
                    SY_AddLine(sy, inp);
                    End;
                'Rectangle': Begin
                    SY_AddRect(sy, inp);
                    End;
                'Arc': Begin
                    SY_AddArc(sy, inp);
                    End;
                'Polygon': Begin
                    SY_AddPoly(sy, inp, InFile);
                    End;
                'Text': Begin
                    SY_AddText(sy, inp);
                    End;
                'Footprint': Begin
                    simp := sy.AddSchImplementation();
                    simp.ModelName := GetBetween(inp, '(Name "', '")');
                    simp.ModelType := cDocKind_PcbLib;
                    simp.AddDataFileLink(simp.ModelName, '', cDocKind_PcbLib);
                    simp.MapAsString := GetBetween(inp, '(Map "', '")');
                    End;
                'EndComponent': Begin
                    Lib.AddSchComponent(sy);
                    // Send a system notification that a new component has been added to the library.
                    SchServer.RobotManager.SendMessage(Lib.I_ObjectAddress, c_BroadCast, SCHM_PrimitiveRegistration, sy.I_ObjectAddress);
                    Lib.CurrentSchComponent := sy;
                    Break;
                    End;
                '': Continue;
                Else Begin
                    ShowMessage('Keyword Error: ' + tag);
                    Break;
                    End;
                End;
            End; // while not eof()
            // done creating schem component
            SchServer.ProcessControl.PostProcess(Lib, '');
            End;
        'EndComponents': Begin
            Break;
            End;
        '': Continue;
        Else Begin
            ShowMessage('Keyword Error: ' + tag);
            Break;
            End;
        End; // case tag
    End; // while not eof()
End;



