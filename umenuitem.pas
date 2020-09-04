unit uMenuItem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uTools;

type

  { TMenuItemParser }
  TMenuItemType = (MITprog, MITmenu, MITrunonce, MITmenufile, MITmenuprog, MITmenuprogreload, MITseparator, MITEndMenu, MITNone);

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

    function SplitMenuLine(const aLine: string): TStringList;

    procedure startNewMenu(const aLine: string);
    procedure endMenu;

    procedure prepareProg(const aLine: string);
    procedure prepareRunOnce(const aLine: string);
    procedure prepareProgreload(const aLine: string);
    procedure prepareSeparator(const aLine: string);
    procedure includeItems(const aLine: string);
  public

    constructor Create(const aLine: string);
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

    for i := 2 to lsl.Count - 1 do
      FCmd := FCmd + ' ' + lsl[i];

    fCmd := Trim(FCmd);
  finally
    FreeAndNil(lSl);
  end;
end;

Procedure TMenuItemParser.prepareRunOnce(Const aLine: string);
var
  lSl: TStringList;
  i: integer;
begin
  lSl := SplitMenuLine(aLine);
  try
    FItemType := MITprog;

    setNameAndShotrCutKey(lSl[1]);

    {TODO -oLebeda -cNone: store somewhere runonce check param (3)}

    for i := 3 to lsl.Count - 1 do
      FCmd := FCmd + ' ' + lsl[i];

    fCmd := Trim(FCmd);
  finally
    FreeAndNil(lSl);
  end;
End;

Procedure TMenuItemParser.prepareProgreload(Const aLine: string);
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

Procedure TMenuItemParser.prepareSeparator(Const aLine: string);
var
  lSl: TStringList;
begin
  lSl := SplitMenuLine(aLine);
  try
    FItemType := MITseparator;

    if lSl.Count >= 2 then
      FName := lSl[1];

    If (Length(FName) > 0) and (FName[1] = '#') Then
      Delete(FName, 1, 1);

    FName := FName.Replace('"', '');
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
    {TODO -oLebeda -cNone: pouze pro linux}
    lFileNameCfg := GetEnvironmentVariable('HOME') + '/.icewm/' + lFileName; {TODO -oLebeda -cNone: správnou cestu}

    if FileExists(lFileName) then
      MainForm.LoadMenuFromFile(lFileName)
    else if FileExists(lFileNameCfg) then
      MainForm.LoadMenuFromFile(lFileNameCfg);

  finally
    FreeAndNil(lSl);
  end;
end;

Function TMenuItemParser.SplitMenuLine(Const aLine: string): TStringList;
Var
  lLine: String;
begin
  Result := TStringList.Create;
  Result.Delimiter := ' ';
  lLine := StringReplace(aLine, '"', '"""', [rfReplaceAll]);
  Result.DelimitedText := lLine;
end;

Procedure TMenuItemParser.startNewMenu(Const aLine: string);
var
  lSl: TStringList;
begin
  lSl := SplitMenuLine(aLine);
  try
    FItemType := MITmenu;

    setNameAndShotrCutKey(lSl[1]);

    FSubMenuId := MainForm.AddMenu(FName, FMenuId);
  finally
    FreeAndNil(lSl);
  end;

end;

Procedure TMenuItemParser.endMenu;
var
  lUpMenuId: integer;
begin
  lUpMenuId := MainForm.SQLMenu.FieldByName('upMenuId').AsInteger;
  FItemType := MITEndMenu;
  MainForm.setActiveMenu(lUpMenuId);
end;

Function TMenuItemParser.GetSearch: string;
begin
  Result := NormalizeTerm(FName); { TODO : lowercase and remove diacritics }
end;

Function TMenuItemParser.GetSubMenuChar: string;
begin
  if FItemType in [MITmenu, MITmenufile, MITmenuprog, MITmenuprogreload] then
    Result := '>'
  else
    Result := '';
end;

Procedure TMenuItemParser.QuoteTrim(Var lName: String);
Begin
  If lName[1] = '"' Then
    Delete(lName, 1, 1);
  If lName[Length(lName)] = '"' Then
    Delete(lName, Length(lName), 1);
End;

Procedure TMenuItemParser.setNameAndShotrCutKey(Const aName: String);
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
  FName := lName;
End;

Constructor TMenuItemParser.Create(Const aLine: string);
begin
  FInputLine := aLine;
  FMenuId := MainForm.SQLMenu.FieldByName('id').AsInteger;

  if AnsiStartsText('prog ', aLine) then
    prepareProg(aLine)
  else if AnsiStartsText('runonce ', aLine) then { TODO : odstranit jako systémově závislé }
    prepareRunOnce(aLine)
  else if AnsiStartsText('separator', aLine) then
    prepareSeparator(aLine)
  else if AnsiStartsText('menu ', aLine) then
    startNewMenu(aLine)
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

Destructor TMenuItemParser.Destroy;
begin
  inherited Destroy;
end;

end.
