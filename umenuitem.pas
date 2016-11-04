unit uMenuItem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TMenuItemParser }
  TMenuItemType = (MITprog, MITmenu, MITrunonce, MITmenufile, MITmenuprog, MITmenuprogreload, MITseparator, MITEndMenu, MITNone);

  TMenuItemParser = class(TObject)
  private
    FCmd: string;
    FIcon: string;
    FInputLine: string;
    FItemType: TMenuItemType;
    FMenuId: integer;
    FName: string;
    FShortCut: string;
    FSubMenuCmd: string;
    FSubMenuId: integer;
    FSubMenuPath: string;
    FSubMenuReloadInterval: integer;


    function GetSearch: string;
    function GetSubMenuChar: string;
    Procedure setNameAndShotrCutKey(Const aName: String);

    function SplitMenuLine(const aLine: string): TStringList;

    procedure startNewMenu(const aLine: string);
    procedure endMenu(const aLine: string);

    procedure prepareProg(const aLine: string);
    procedure prepareProgreload(const aLine: string);
    procedure prepareSeparator(const aLine: string);
    procedure includeItems(const aLine: string);
  public

    constructor Create(const aLine: string);
    destructor Destroy; override;

    property Name: string read FName;
    property icon: string read FIcon;
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

uses Dialogs, uMainForm, strutils;

function MitToStr(const aMenuType: TMenuItemType): string;
begin
  WriteStr(Result, aMenuType);
end;

function strToMit(const aMenuTypeStr: string): TMenuItemType;
begin
  ReadStr(aMenuTypeStr, Result);
end;

{ TMenuItemParser }

Procedure TMenuItemParser.prepareProg(Const aLine: string);
var
  lSl: TStringList;
  i: integer;
begin
  lSl := SplitMenuLine(aLine);
  try
    FItemType := MITprog;

    setNameAndShotrCutKey(lSl[1]);
    FIcon := lSl[2];

    for i := 3 to lsl.Count - 1 do
      FCmd := FCmd + ' ' + lsl[i];

    fCmd := Trim(FCmd);
  finally
    FreeAndNil(lSl);
  end;
end;

Procedure TMenuItemParser.prepareProgreload(Const aLine: string);
var
  lSl: TStringList;
  i: integer;
begin
  lSl := SplitMenuLine(aLine);
  try
    FItemType := MITmenuprogreload;

    setNameAndShotrCutKey(lSl[1]);
    FIcon := lSl[2];
    FSubMenuReloadInterval := StrToInt(lSl[3]);

    for i := 4 to lsl.Count - 1 do
      FSubMenuCmd := FSubMenuCmd + ' ' + lsl[i];

    FSubMenuCmd := Trim(FSubMenuCmd);
  finally
    FreeAndNil(lSl);
  end;
end;

Procedure TMenuItemParser.prepareSeparator(Const aLine: string);
var
  lSl: TStringList;
  i: integer;
begin
  lSl := SplitMenuLine(aLine);
  try
    FItemType := MITseparator;

    if lSl.Count >= 2 then
      FName := lSl[1];

  finally
    FreeAndNil(lSl);
  end;
end;

Procedure TMenuItemParser.includeItems(Const aLine: string);
var
  lSl: TStringList;
  lFileName, lFileNameCfg: string;
begin
  lSl := SplitMenuLine(aLine);
  try
    FItemType := MITNone;

    lFileName := lSl[1];
    lFileNameCfg := GetEnvironmentVariable('HOME') + '/.icewm/' + lFileName;

    if FileExists(lFileName) then
      MainForm.LoadMenuFromFile(lFileName)
    else if FileExists(lFileNameCfg) then
      MainForm.LoadMenuFromFile(lFileNameCfg);

  finally
    FreeAndNil(lSl);
  end;
end;

Function TMenuItemParser.SplitMenuLine(Const aLine: string): TStringList;
begin
  Result := TStringList.Create;
  Result.Delimiter := ' ';
  Result.DelimitedText := aLine;
end;

Procedure TMenuItemParser.startNewMenu(Const aLine: string);
var
  lSl: TStringList;
begin
  lSl := SplitMenuLine(aLine);
  try
    FItemType := MITmenu;

    setNameAndShotrCutKey(lSl[1]);
    FIcon := lSl[2];
    FSubMenuId := MainForm.AddMenu(FName, FMenuId);
  finally
    FreeAndNil(lSl);
  end;

end;

Procedure TMenuItemParser.endMenu(Const aLine: string);
var
  lUpMenuId: integer;
  lMenuId: longint;
begin
  lMenuId := MainForm.SQLMenu.FieldByName('id').AsInteger;
  lUpMenuId := MainForm.SQLMenu.FieldByName('upMenuId').AsInteger;
  FItemType := MITEndMenu;
  MainForm.setActiveMenu(lUpMenuId);
end;

Function TMenuItemParser.GetSearch: string;
begin
  Result := FName;
end;

Function TMenuItemParser.GetSubMenuChar: string;
begin
  if FItemType in [MITmenu, MITmenufile, MITmenuprog, MITmenuprogreload] then
    Result := '>'
  else
    Result := '';
end;

Procedure TMenuItemParser.setNameAndShotrCutKey(const aName: String);
Var
  i: Integer;
  lName: String;
Begin
  for i := 1 to Length(aName) do
  begin
    if (aName[i] = '_') and (FShortCut = '') and (aName[i + 1] <> '_') then
      FShortCut := LowerCase(aName[i + 1])
    else
      lName := lName + aName[i];
  End;

  if FShortCut = '' then
    FShortCut := LowerCase(aName[1]);

  FName := lName;
End;

Constructor TMenuItemParser.Create(Const aLine: string);
begin
  FInputLine := aLine;
  FMenuId := MainForm.SQLMenu.FieldByName('id').AsInteger;

  if AnsiStartsText('prog ', aLine) then
    prepareProg(aLine)
  else if AnsiStartsText('separator', aLine) then
    prepareSeparator(aLine)
  else if AnsiStartsText('menu ', aLine) then
    startNewMenu(aLine)
  else if AnsiStartsText('}', aLine) then
    endMenu(aLine)
  else if AnsiStartsText('include ', aLine) then
    includeItems(aLine)
  else if AnsiStartsText('menuprogreload ', aLine) then
    prepareProgreload(aLine)
  // menufile
  // menuprog
  else
  begin
    FItemType := MITNone;
    WriteLn('!!!!!' + aLine);
  end;
end;

Destructor TMenuItemParser.Destroy;
begin
  inherited Destroy;
end;

end.
