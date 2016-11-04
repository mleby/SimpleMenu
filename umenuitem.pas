Unit uMenuItem;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils;

Type

  { TMenuItemParser }
  TMenuItemType = (prog, menu, runonce, menufile, menuprog, menuprogreload);

  TMenuItemParser = class(TObject)
  private
    FCmd: String;
    FIcon: String;
    FInputLine: String;
    FItemType: TMenuItemType;
    FMenuId: Integer;
    FName: String;
    FShortCut: String;
    FSubMenuCmd: String;
    FSubMenuId: Integer;
    FSubMenuPath: String;
    FSubMenuReloadInterval: Integer;


    Function GetSearch: String;
    Function GetSubMenuChar: String;
    procedure prepareProg(const aLine: String);
  public

    constructor Create(const aLine: String; const aMenuId: Integer);
    destructor Destroy; override;

    Property name: String read FName;
    Property icon: String read FIcon;
    Property cmd: String read FCmd;
    Property menuId: Integer Read FMenuId;
    Property itemType: TMenuItemType read FItemType;
    Property search: String read GetSearch;
    Property shortcut: String read FShortCut;
    Property subMenuId: Integer Read FSubMenuId;
    Property subMenuPath: String read FSubMenuPath;
    Property subMenuCmd: String read FSubMenuCmd;
    Property subMenuReloadInterval: Integer Read FSubMenuReloadInterval;
    Property subMenuChar: String Read GetSubMenuChar;
  end;

  function mtToStr(const aMenuType: TMenuItemType): String;
  function strToMt(const aMenuTypeStr: String): TMenuItemType;


Implementation

Uses Dialogs, strutils;

Function mtToStr(Const aMenuType: TMenuItemType): String;
Begin
  WriteStr(Result, aMenuType);
end;

Function strToMt(Const aMenuTypeStr: String): TMenuItemType;
Begin
  ReadStr(aMenuTypeStr, Result);
end;

{ TMenuItemParser }

Procedure TMenuItemParser.prepareProg(Const aLine: String);
Var
  lSl: TStringList;
  i: Integer;
Begin
  lSl := TStringList.Create;
  try
    lSl.Delimiter := ' ';
    lsl.DelimitedText := aLine;

    FItemType := prog;

    FName := lSl[1];
    FIcon := lSl[2];
    for i := 3 to lsl.Count - 1 do
      FCmd := FCmd + ' ' + lsl[i];
    fCmd := Trim(FCmd);

    {TODO -oLebeda -cNone: shortcut from name and correct name}


    //ShowMessage('name: ' + name);
    //ShowMessage('icon: ' + icon);
    //ShowMessage('cmd: ' + cmd);
  Finally
    FreeAndNil(lSl);
  End;
End;

Function TMenuItemParser.GetSearch: String;
Begin
  Result := FName;
  {TODO -oLebeda -cNone: implementovat normalizaci}
end;

Function TMenuItemParser.GetSubMenuChar: String;
Begin
  if FItemType in [menu, menufile, menuprog, menuprogreload] then
    Result := '>'
  else
    Result := '';
end;

Constructor TMenuItemParser.Create(Const aLine: String; Const aMenuId: Integer);
Begin
  FInputLine :=  aLine;
  FMenuId := aMenuId;

  if AnsiStartsText('prog', aLine) then
    prepareProg(aLine);
End;

Destructor TMenuItemParser.Destroy;
Begin
  Inherited Destroy;
End;

End.

