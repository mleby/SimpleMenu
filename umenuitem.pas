unit uMenuItem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uTools;

type

  { TMenuItemParser }
  TMenuItemType = (MITprog, MITmenu, MITmenudefault,
                  MITrunonce,
                  {$IFDEF Windows}
                  MITmenuwindow, MITwindow, MITwinkey, MITwinignore,
                  {$ENDIF}
                  MITmenufile, MITmenuprog, MITmenuprogreload, MITseparator, MITEndMenu, MITNone,
                  MITmenupath);

  TMenuItemParser = class(TObject)
  private
    FCmd: string;
    FInputLine: string;
    FItemType: TMenuItemType;
    FMenuId: integer;
    FName: String;
    FShortCut: string;
    FSubMenuCmd: string;
    FSubMenuId: integer;
    FSubMenuPath: string;
    FSubMenuReloadInterval: integer;


    function GetSearch: string;
    function GetSubMenuChar: string;
    Procedure QuoteTrim(Var lName: String); // remove leading and ending quotes
    Procedure setNameAndShotrCutKey(Const aName: String);

    procedure startNewMenu(const aLine: string);
    procedure startNewMenuDefault(const aLine: string);
    procedure endMenu;

    procedure prepareProg(const aLine: string);
    procedure prepareWindow(const aName, hWindow: string);
    procedure prepareRunOnce(const aLine: string);
    procedure prepareProgreload(const aLine: string);
    procedure prepareWindowmenu(const aLine: string);
    procedure preparePathmenu(const aLine: string);
    procedure prepareWinKey(const aLine: string);
    procedure prepareWinIgnore(const aLine: string);
    procedure prepareSeparator(const aLine: string);
    procedure includeItems(const aLine: string);
  public
    class function SplitMenuLine(const aLine: string): TStringList;

    constructor Create(const aLine: string);
    constructor Create(const aName, hWindow, aShortcut: String); // window item
    destructor Destroy; override;

    property Name: String read FName;
    property cmd: string read FCmd;
    property menuId: integer read FMenuId;
    property itemType: TMenuItemType read FItemType;
    property search: string read GetSearch;
    property shortcut: string read FShortCut;
    property subMenuId: integer read FSubMenuId;
    property subMenuPath: string read FSubMenuPath;
    property subMenuCmd: string read FSubMenuCmd;
    property subMenuReloadInterval: integer read FSubMenuReloadInterval;
    property subMenuChar: string read GetSubMenuChar;
  end;

function MitToStr(const aMenuType: TMenuItemType): string;
function strToMit(const aMenuTypeStr: string): TMenuItemType;


implementation

uses Dialogs, uMainForm, strutils, clipbrd
  {$IFDEF Windows}
  , uwinmanager
  {$ENDIF}
  ;

function MitToStr(const aMenuType: TMenuItemType): string;
begin
  WriteStr(Result, aMenuType);
end;

function strToMit(const aMenuTypeStr: string): TMenuItemType;
begin
  if aMenuTypeStr = '' then
    Result := MITNone
  else
    ReadStr(aMenuTypeStr, Result);
end;

{ TMenuItemParser }

procedure TMenuItemParser.prepareProg(const aLine: string);
var
  lSl: TStringList;
  i: integer;
begin
  lSl := SplitMenuLine(aLine);
  try
    FItemType := MITprog;

    if ContainsText(aLine, '#if_prog_exists') then
    begin
      if not FileExists(lSl[2]) then
      begin
        FCmd := '';
        Exit;
      end;
    end;

    setNameAndShotrCutKey(lSl[1]);

    for i := 2 to lsl.Count - 1 do
    begin
      if LowerCase(lSl[i]) <> '#if_prog_exists' then
        FCmd := FCmd + ' ' + lsl[i];
    end;

    fCmd := Trim(FCmd);
  finally
    FreeAndNil(lSl);
  end;
end;

procedure TMenuItemParser.prepareWindow(const aName, hWindow: string);
begin
  FItemType := MITwindow;
  setNameAndShotrCutKey(aName);
  FCmd := hWindow;
end;

procedure TMenuItemParser.prepareRunOnce(const aLine: string);
var
  lSl: TStringList;
  i: integer;
begin
  lSl := SplitMenuLine(aLine);
  try
    FItemType := MITprog; // on linux simple run
    {$IFDEF Windows}
    FItemType := MITrunonce;
    {$ENDIF}

    setNameAndShotrCutKey(lSl[1]);

    for i := 2 to lsl.Count - 1 do
      FCmd := FCmd + ' ' + lsl[i];

    fCmd := Trim(FCmd);
  finally
    FreeAndNil(lSl);
  end;
End;

procedure TMenuItemParser.prepareProgreload(const aLine: string);
var
  lSl: TStringList;
  i: integer;
begin
  lSl := SplitMenuLine(aLine);
  try
    FItemType := MITmenuprogreload;

    setNameAndShotrCutKey(lSl[1]);
    FSubMenuReloadInterval := StrToInt(lSl[2]);

    for i := 3 to lsl.Count - 1 do
      FSubMenuCmd := FSubMenuCmd + ' ' + lsl[i];

    FSubMenuCmd := Trim(FSubMenuCmd);
  finally
    FreeAndNil(lSl);
  end;
end;

{ TODO -cWM : jen pro windows}
procedure TMenuItemParser.prepareWindowmenu(const aLine: string);
var
  lSl: TStringList;
  i: integer;
begin
  lSl := SplitMenuLine(aLine);
  try
    FItemType := MITmenuwindow;

    setNameAndShotrCutKey(lSl[1]);
    FSubMenuReloadInterval := -1; // const 0s

    for i := 2 to lsl.Count - 1 do
      FSubMenuCmd := FSubMenuCmd + ' ' + lsl[i];

    //FSubMenuCmd := Trim(FSubMenuCmd);
  finally
    FreeAndNil(lSl);
  end;
end;

procedure TMenuItemParser.preparePathmenu(const aLine: string);
var
  lSl: TStringList;
  i: integer;
begin
  lSl := SplitMenuLine(aLine);
  try
    FItemType := MITmenupath;

    setNameAndShotrCutKey(lSl[1]);
    FSubMenuReloadInterval := -1; // const 0s

    // for i := 2 to lsl.Count - 1 do
    { #todo : check size of lsl }
    FSubMenuPath := Trim(ReplaceStr(lSl[2],'"','')); { #todo : replace only on start and end }

    for i := 3 to lsl.Count - 1 do
      FSubMenuCmd := FSubMenuCmd + ' ' + lsl[i];
    FSubMenuCmd := Trim(ReplaceStr(FSubMenuCmd,'"','')); { #todo : replace only on start and end }
  finally
    FreeAndNil(lSl);
  end;
end;

procedure TMenuItemParser.prepareWinKey(const aLine: string);
var
  lSl: TStringList;
begin
  lSl := SplitMenuLine(aLine);
  try
    FItemType := MITwinkey;

    FName := lSl[1];
    QuoteTrim(FName);
    FName := Trim(FName);

    FCmd := FName;

    FShortCut := lSl[2];

  finally
    FreeAndNil(lSl);
  end;
end;

procedure TMenuItemParser.prepareWinIgnore(const aLine: string);
var
  lSl: TStringList;
begin
  lSl := SplitMenuLine(aLine);
  try
    FItemType := MITwinignore;

    FName := lSl[1];
    QuoteTrim(FName);
    FName := Trim(FName);

    FCmd := FName;
  finally
    FreeAndNil(lSl);
  end;
end;

procedure TMenuItemParser.prepareSeparator(const aLine: string);
var
  lSl: TStringList;
begin
  lSl := SplitMenuLine(aLine);
  try
    FItemType := MITseparator;

    if lSl.Count >= 2 then
    begin
      FName := lSl[1];
      FName := FName.Replace('%clipbrd%', Clipboard.AsText);
    end;

    If (Length(FName) > 0) and (FName[1] = '#') Then
      Delete(FName, 1, 1);

    FName := FName.Replace('"', '');
  finally
    FreeAndNil(lSl);
  end;
end;

procedure TMenuItemParser.includeItems(const aLine: string);
var
  lSl: TStringList;
  lFileName, lFileNameCfg: string;
begin
  lSl := SplitMenuLine(aLine);
  try
    FItemType := MITNone;

    lFileName := lSl[1];
    lFileNameCfg := '';
    {$IFNDEF Windows}
    lFileNameCfg := GetEnvironmentVariable('HOME') + '/.icewm/' + lFileName; {TODO -oLebeda -cNone: správnou cestu}
    {$ENDIF}

    if FileExists(lFileName) then
      MainForm.LoadMenuFromFile(lFileName)
    else if FileExists(lFileNameCfg) then
      MainForm.LoadMenuFromFile(lFileNameCfg);

  finally
    FreeAndNil(lSl);
  end;
end;

class function TMenuItemParser.SplitMenuLine(const aLine: string): TStringList;
Var
  lLine: String;
begin
  Result := TStringList.Create;
  Result.Delimiter := ' ';
  lLine := StringReplace(aLine, '"', '"""', [rfReplaceAll]);

  // replace in name of menu before split
  {$IFDEF Windows}
  lLine := lLine.Replace('%CurDestop%', GetCurrentDesktopName());
  lLine := lLine.Replace('%computername%', GetEnvironmentVariable('COMPUTERNAME'));
  { #todo -cfeat : implementovat obecnou %env:variable% }
  {$ENDIF}

  Result.DelimitedText := lLine;
end;

procedure TMenuItemParser.startNewMenu(const aLine: string);
var
  lSl: TStringList;
begin
  lSl := SplitMenuLine(aLine);
  try
    FItemType := MITmenu;

    setNameAndShotrCutKey(lSl[1]);

    FSubMenuId := MainForm.AddMenu(FName, FMenuId, MITmenu);
  finally
    FreeAndNil(lSl);
  end;
end;

procedure TMenuItemParser.startNewMenuDefault(const aLine: string);
var
  lSl: TStringList;
begin
  lSl := SplitMenuLine(aLine);
  try
    FItemType := MITmenudefault;

    setNameAndShotrCutKey(lSl[1]);

    FSubMenuId := MainForm.AddMenu(FName, FMenuId, MITmenudefault);
  finally
    FreeAndNil(lSl);
  end;
end;

procedure TMenuItemParser.endMenu;
var
  lUpMenuId: integer;
begin
  lUpMenuId := MainForm.SQLMenu.FieldByName('upMenuId').AsInteger;
  FItemType := MITEndMenu;
  MainForm.setActiveMenu(lUpMenuId);
end;

function TMenuItemParser.GetSearch: string;
begin
  Result := NormalizeTerm(FName); // lowercase and remove diacritics
end;

function TMenuItemParser.GetSubMenuChar: string;
begin
  if FItemType in [MITmenu, MITmenufile, MITmenuprog, MITmenuprogreload, MITmenuwindow, MITmenupath] then
    Result := '>'
  else if FItemType = MITMenudefault then
    Result := '|'
  else
    Result := '';
end;

procedure TMenuItemParser.QuoteTrim(var lName: String);
Begin
  If lName[1] = '"' Then
    Delete(lName, 1, 1);
  If lName[Length(lName)] = '"' Then
    Delete(lName, Length(lName), 1);
End;

procedure TMenuItemParser.setNameAndShotrCutKey(const aName: String);
Var
  i: Integer;
  lName: String;
Begin
  // identify explicit shortcut
  for i := 1 to Length(aName) do
  begin
    if (aName[i] = '_') and (FShortCut = '') and (aName[i + 1] <> '_') then
      FShortCut := LowerCase(aName[i + 1])
    else
      lName := lName + aName[i];
  End;

  QuoteTrim(lName);

  lName := lName.Replace('%clipbrd%', Clipboard.AsText);

  lName := Trim(lName);
  FName := lName;
End;

constructor TMenuItemParser.Create(const aLine: string);
begin
  FInputLine := aLine;
  FMenuId := MainForm.SQLMenu.FieldByName('id').AsInteger;

  if AnsiStartsText('prog ', aLine) then
    prepareProg(aLine)
  else if AnsiStartsText('runonce ', aLine) then { TODO -cfeat : implementace pro windows }
    prepareRunOnce(aLine)
  else if AnsiStartsText('separator', aLine) then
    prepareSeparator(aLine)
  else if AnsiStartsText('menuwindow ', aLine) then
    prepareWindowmenu(aLine)
  else if AnsiStartsText('menupath ', aLine) then
    preparePathmenu(aLine)
  else if AnsiStartsText('winkey ', aLine) then
    prepareWinKey(aLine)
  else if AnsiStartsText('winignore ', aLine) then
    prepareWinIgnore(aLine)
  else if AnsiStartsText('menu ', aLine) then
    startNewMenu(aLine)
  else if AnsiStartsText('menudefault ', aLine) then
    startNewMenuDefault(aLine)
  else if AnsiStartsText('}', aLine) then
    endMenu
  else if AnsiStartsText('include ', aLine) then
    includeItems(aLine)
  else if AnsiStartsText('menuprogreload ', aLine) then
    prepareProgreload(aLine)
  //else if AnsiStartsText('menusearch ', aLine) then
  //  prepareSearch(aLine) {TODO -oLebeda -cNone: menusearch}
  // menufile
  // menuprog
  else
  begin
    FItemType := MITNone;
    WriteLn('!!!!!' + aLine);
  end;
end;

constructor TMenuItemParser.Create(const aName, hWindow, aShortcut: String);
begin
  FMenuId := MainForm.SQLMenu.FieldByName('id').AsInteger;
  FShortCut := aShortcut;
  prepareWindow(aName, hWindow)
end;

destructor TMenuItemParser.Destroy;
begin
  inherited Destroy;
end;

end.
