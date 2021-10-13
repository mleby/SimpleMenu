unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, sqlite3conn, sqldb, Forms, Controls, Graphics, DBGrids, ActnList, ExtCtrls, StdCtrls, Grids,
  AsyncProcess, LazUTF8, UTF8Process, uMenuItem,
  {$IFDEF Windows}
  uwinmanager,
  {$ENDIF}
  process;

type

  TFormMode = (FMNormal, FMCentral);

  { TMainForm }

  TMainForm = class(TForm)
    acList: TActionList;
    acDebug: TAction;
    acRun: TAction;
    acFind: TAction;
    acKeepOpen: TAction;
    acGlobalSearch: TAction;
    AsyncProcess1: TAsyncProcess;
    edFind: TEdit;
    MainGrid: TDBGrid;
    MainGridSubmenu: TDBGrid;
    MainGridShortCut: TDBGrid;
    MenuDS: TDataSource;
    MenuDB: TSQLite3Connection;
    MenuItemDS: TDataSource;
    pnlFind: TPanel;
    Process1: TProcess;
    ProcessUTF81: TProcessUTF8;
    SQLMenu: TSQLQuery;
    SQLMenuItems: TSQLQuery;
    SQLMenuItemsMaxWidth: TSQLQuery;
    SQLMenuItemsShortcut: TSQLQuery;
    SQLMenuItemsShortcutByCmd: TSQLQuery;
    SQLTransaction: TSQLTransaction;
    ThrTimer: TTimer;
    procedure acDebugExecute(Sender: TObject);
    Procedure acFindExecute(Sender: TObject);
    procedure acGlobalSearchExecute(Sender: TObject);
    procedure acGlobalSearchUpdate(Sender: TObject);
    Procedure acKeepOpenExecute(Sender: TObject);
    procedure acRunExecute(Sender: TObject);
    Procedure edFindKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure edFindKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    Procedure MainGridCellClick(Column: TColumn);
    Procedure MainGridDrawColumnCell(Sender: TObject; Const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure MainGridEnter(Sender: TObject);
    Procedure MainGridKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure MainGridKeyPress(Sender: TObject; Var Key: char);
    Procedure MainGridKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure MainGridSubmenuDrawColumnCell(Sender: TObject; Const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure pnlFindEnter(Sender: TObject);
    procedure pnlFindExit(Sender: TObject);
    procedure SQLMenuAfterInsert(DataSet: TDataSet);
    procedure SQLMenuAfterScroll(DataSet: TDataSet);
    Procedure ThrTimerTimer(Sender: TObject);
  private
    FExtraParam: string;
    FFormMode: TFormMode;
    FRecNo: LongInt;
    FKeepOpen: Boolean;
    FExecIfOne: Boolean;
    FDefaultItem: string;
    FKeyStop: Boolean;
    FSearchCount: LongInt;
    FLastResNo: Integer; // for navigation over separators
    FLastFind: String;
    Procedure AppDeactivate(Sender: TObject);
    Procedure closeFindPanel(Const aForce: Boolean = false);
    Procedure FindSwitch;
    Function GetMaxWidth: Integer;
    Function GetTextWidth(Const aText: String): Integer;
    function isExternalSearch: Boolean;
    function canExternalSearch: Boolean;
    Procedure LoadMenuFromLines(Const aLines: TStringList);
    Procedure LoadMenuFromProcess(Const aCmd: String);
    Procedure RunAsync(Const aCmd: string);
    Procedure setMenuShortcut(Const lId: String; aKeySet: String = '');
    Procedure SetSearchCount(Const aValue: LongInt);
    Procedure SetSeparatorRow(Const State: TGridDrawState; Const Column: TColumn; Const DataCol: Integer; Const Rect: TRect;
      Const aGrid: TDBGrid);
    procedure showMenu(const aOrderBy: String = 'id'; const aName: String = '');
    procedure SetFormSize;
    procedure NavigateUp;
    procedure WindowMenu(const aSubMenuId, aMenuItemId: Integer; const aRoot: Boolean = False);
    { private declarations }
  public
    function AddMenu(aName: string; aUpMenuId: longint; aCmd: string = ''; aPath: string = ''; aReloadInterval: integer = 0): integer;
    Function setActiveMenu(const aIdMenu: longint; const aOrderBy: String = 'id'): Boolean;
    procedure AddMenuItem(var lMenuItemParser: TMenuItemParser);
    procedure LoadMenuFromFile(const aFile: string);
    procedure AppendMenuItem(const aItem: string);

    Property FormMode: TFormMode Read FFormMode Write FFormMode;
    Property SearchCount: LongInt Read FSearchCount Write SetSearchCount;
    Property extraParam: string Read FExtraParam Write FExtraParam;
  end;

var
  MainForm: TMainForm;

implementation

uses strutils, debugForm, uHacks, StreamIO, LCLType, Dialogs, lconvencoding,
  LazUTF8Classes;

const C_SHORTCUT_MENU_CHARS = 'abcdefghijklmnopqrstuvwxyz0123456789';

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var ppi, ppiDesigned: Integer;
  lMenuName: String;
begin
  if Application.HasOption('h', 'help') then begin
    showMessage(
    'Usage: simpleMenu -(f|p|m|w) "menu file or cmd" [options...]' +#10#13 +
    '         one of -f/-p/-m must be specified as  point for menu' + #10#13 +
    '    -h --help             show this help' + #10#13 +
    '    -k --keep             keep menu open after choise' + #10#13 +
    '    -f X --file=X         path to menu file used as start point for menu' + #10#13 +
    '    -m X --menuitem=X     text content of menu' + #10#13 +
    '    -p X --process=X      command for generate menu' + #10#13 +
    '    -s X --search=X       count of menu items for automatic enable find' + #10#13 +
    '    -q X --query=X        automatic enable find entry and fill start query' + #10#13 +
    '    -r X --reload=X       dynamic menu with minimal chars for search' + #10#13 +
    '    -x X --showfile=X     extra options for menu cmd' + #10#13 +
    '    -w --windowmenu       window menu' + #10#13 +
    '    -1 X -execone=X       automatic execute if matched only one item, execute X if 0 items found or append X to menu');
    Halt;
  { TODO -cfeat : nahrazení %s pomocí uživatelského stringu : jako oddělovač - při spuštění při neakvním vyhledávání se zeptat }
  { TODO -crefactor : Vyčlenit práci s DB do samostatného DM }

  end;

  if not (Application.HasOption('f', 'file')
     or Application.HasOption('p', 'process')
     or Application.HasOption('m', 'menu')
     or Application.HasOption('w', 'windowmenu'))
  then
  begin
    showMessage('One menu source must be specified (file or process or menuitem).');
    Halt;
  End;


  ppiDesigned := self.DesignTimePPI;
  ppi := Screen.PixelsPerInch;
  MainForm.Constraints.MaxHeight := round(Screen.Height * 0.9 * (ppiDesigned/ppi));
  MainForm.Constraints.MaxWidth := round(Screen.Width * 0.9 * (ppiDesigned/ppi));

  FFormMode := FMNormal;
  // color

  // sure create DB
  MenuDB.Close;

  // only for dev
  DeleteFile('C:\tmp\debugMenu2.db'); // uncoment only for developnet (real DB for object inspector and design in lazarus)
  MenuDB.DatabaseName := 'C:\tmp\debugMenu2.db'; // uncoment only for developnet (real DB for object inspector and design in lazarus)
  //MenuDB.DatabaseName := ':memory:';

  MenuDB.Open;
  MenuDB.ExecuteDirect('PRAGMA encoding="UTF-8"');
  MenuDB.ExecuteDirect('CREATE TABLE IF NOT EXISTS menu (id INTEGER PRIMARY KEY , upMenuId INTEGER, name NOT NULL, cmd, path, load INTEGER, reloadInterval INTEGER)');
  MenuDB.ExecuteDirect('CREATE TABLE IF NOT EXISTS menuItem (id INTEGER PRIMARY KEY , menuId INTEGER NOT NULL, itemType, name, search, shortcut, cmd, subMenuPath, subMenuCmd, subMenuReloadInterval INTEGER, subMenuId INTEGER, subMenuChar, width INTEGER DEFAULT 100, FOREIGN KEY(menuId) REFERENCES menu(id))');
  MenuDB.Transaction.Commit;

  SQLMenu.Active := True;
  SQLMenuItems.Active := True;

  // fill root menu
  AddMenu('ROOT', 0);
  SQLMenu.First;

  { TODO -crefactor : command line params do lpr }
  // commandline parameters
  if Application.HasOption('c', 'center') then
  begin
    MainForm.Width := 500;
    MainForm.Height := 563;
    MainForm.Position := poScreenCenter;
    MainForm.FormMode := FMCentral;
  end;

  if Application.HasOption('s', 'search') then
    MainForm.SearchCount := StrToInt(Application.GetOptionValue('s', 'search'))
  else
    MainForm.SearchCount := MainForm.Constraints.MaxHeight div MainGrid.DefaultRowHeight;

  if Application.HasOption('f', 'file') then
  begin
    SQLMenu.Edit;
    SQLMenu.FieldByName('path').AsString := Application.GetOptionValue('f', 'file');
    SQLMenu.CheckBrowseMode;
    LoadMenuFromFile(SQLMenu.FieldByName('path').AsString);
  end;

  if Application.HasOption('w', 'windowmenu') then
  begin
    if Application.GetOptionValue('w', 'windowmenu') <> '' then
    begin
      SQLMenu.Edit;
      SQLMenu.FieldByName('path').AsString := Application.GetOptionValue('w', 'windowmenu');
      SQLMenu.CheckBrowseMode;
      LoadMenuFromFile(SQLMenu.FieldByName('path').AsString);
    end
  end;

  if Application.HasOption('r', 'reload') then
  begin
    SQLMenu.Edit;
    SQLMenu.FieldByName('Load').AsInteger := -1;
    SQLMenu.FieldByName('reloadInterval').AsInteger := -1 * StrToInt(Application.GetOptionValue('r', 'reload'));
    SQLMenu.CheckBrowseMode;
  End;

  if Application.HasOption('p', 'process') then
  begin
    SQLMenu.Edit;
    SQLMenu.FieldByName('cmd').AsString := Application.GetOptionValue('p', 'process');
    SQLMenu.CheckBrowseMode;
    LoadMenuFromProcess(SQLMenu.FieldByName('cmd').AsString);
  end;

  if Application.HasOption('x', 'extra') then
    MainForm.extraParam := Application.GetOptionValue('x', 'extra');

  MenuDB.Transaction.Commit;

  // open grid data
  SQLMenu.Active := True;
  SQLMenu.First;

  MainGrid.DataSource.DataSet.Active := False;
  MainGrid.DataSource.DataSet.Active := True;

  if not Application.HasOption('k', 'keep') then
  begin
      Application.OnDeactivate:=@AppDeactivate;
    FKeepOpen := False;
  End
  else
    FKeepOpen := True;

  if Application.HasOption('1', 'execone') then
  begin
    FExecIfOne := True;
    FDefaultItem := Application.GetOptionValue('1', 'execone');
  End
  else
    FExecIfOne := False;

  if Application.HasOption('q', 'query') then
  begin
    pnlFind.Visible := True;
    edFind.Text := Application.GetOptionValue('q', 'query');
  end;

  if Application.HasOption('w', 'windowmenu') then
     WindowMenu(0,0, True)
  else if Application.HasOption('m', 'menuitem') then
  begin
    lMenuName := Application.GetOptionValue('m', 'menu');
    showMenu('id', lMenuName);

    if SQLMenuItems.RecordCount = 1 then
    begin
      acRun.Execute;

      // root on current menu
      SQLMenu.Edit;
      SQLMenu.FieldByName('upMenuId').AsInteger := 0;
      SQLMenu.CheckBrowseMode;
    end;

    { TODO -cfix : Nefunguje filtr z cmd po zobrazení submenu }

    //MenuDB.ExecuteDirect('update menuItem set menuId = 1 where id = 2');
    //setActiveMenu(1);
  end
  else
    showMenu;
end;

procedure TMainForm.AppDeactivate(Sender: TObject);
begin
  if not FKeepOpen then
    MainForm.Close;
end;

procedure TMainForm.closeFindPanel(const aForce: Boolean);
Begin
  if FExecIfOne and (SQLMenuItems.RecordCount = 1) then
  begin
    AppDeactivate(self);
  end
  else if (edFind.Text <> '') or aForce then
  begin
    edFind.Text := '';
  //  showMenu; { TODO : jen pokud není zavíráno }
    MainGrid.SetFocus;
    pnlFind.Visible := false;
    MainGridShortCut.Visible := not pnlFind.Visible;
  End;
End;

procedure TMainForm.FindSwitch;
Begin
  if MainGrid.Focused then
    pnlFind.Visible := True
  else If Not pnlFind.Visible Then
    pnlFind.Visible := True
  Else If pnlFind.Visible And ((edFind.Text = '') or (edFind.Text = '*')) Then
  begin
    pnlFind.Visible := False;
    if (edFind.Text = '*') then
    begin
      edFind.Text := '';
      showMenu;
    end;
  end;

  If pnlFind.Visible And Not edFind.Focused Then
  Begin
    edFind.SetFocus;
  End
  Else
  Begin
    MainGrid.SetFocus;
  End;

  MainGridShortCut.Visible := not pnlFind.Visible;

  SetFormSize;
End;

function TMainForm.GetMaxWidth: Integer;
Begin
  if SQLMenuItemsMaxWidth.Active and (SQLMenuItemsMaxWidth.RecordCount = 1) and not SQLMenuItemsMaxWidth.FieldByName('width').IsNull then
    Result := SQLMenuItemsMaxWidth.FieldByName('width').AsInteger + 10
  else
    Result := 500;
End;

function TMainForm.GetTextWidth(const aText: String): Integer;
var
  W: integer;
  BM: TBitmap;
Begin
  BM := TBitmap.Create;
  try
    BM.Canvas.Font := MainGrid.Font;
    W := BM.Canvas.TextWidth(aText);
  Finally
    BM.Free;
  End;

  Result := W + 8; { TODO -crefactor : Magic const 8 }
  //Result := Length(aText) * 30
End;

function TMainForm.isExternalSearch: Boolean;
Var
  lReloadInterval: LongInt;
  lCmd: String;
  lDynCmd: SizeInt;
Begin
  lReloadInterval := SQLMenu.FieldByName('reloadInterval').AsInteger;
  lCmd := SQLMenu.FieldByName('cmd').AsString;
  lDynCmd := Pos('%s', lCmd);
  Result := (lReloadInterval < 0) And (lDynCmd > 0)
End;

function TMainForm.canExternalSearch: Boolean;
Var
  lReloadInterval: LongInt;
  lCmd: String;
  lDynCmd: SizeInt;
Begin
  lReloadInterval := SQLMenu.FieldByName('reloadInterval').AsInteger;
  lCmd := SQLMenu.FieldByName('cmd').AsString;
  lDynCmd := Pos('%s', lCmd);
  Result := (lReloadInterval < 0) And (Length(edFind.Text) >= abs(lReloadInterval)) And (lDynCmd > 0)
End;

procedure TMainForm.MainGridCellClick(Column: TColumn);
Begin
  acRun.Execute;
end;

procedure TMainForm.MainGridDrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
Begin
  SetSeparatorRow(State, Column, DataCol, Rect, MainGrid);
end;

procedure TMainForm.MainGridEnter(Sender: TObject);
begin
  if SQLMenuItems.RecordCount > 1 then
   while strToMit(SQLMenuItems.FieldByName('itemType').AsString) = MITseparator do
   begin
     SQLMenuItems.Next;
   end;
end;

procedure TMainForm.MainGridSubmenuDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
Begin
  SetSeparatorRow(State, Column, DataCol, Rect, MainGridSubmenu);
end;

procedure TMainForm.pnlFindEnter(Sender: TObject);
begin
  MainGridShortCut.Visible := not pnlFind.Visible;
end;

procedure TMainForm.pnlFindExit(Sender: TObject);
begin
  MainGridShortCut.Visible := not pnlFind.Visible;
end;

procedure TMainForm.MainGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
Begin
  // navigation over separators
  FLastResNo := SQLMenuItems.RecNo;

  if (Key = VK_Return) then
  begin
    FRecNo := SQLMenuItems.RecNo;
    SQLMenuItems.Prior; // ugly hack - enter goes to next
  End;
end;

procedure TMainForm.MainGridKeyPress(Sender: TObject; var Key: char);
Begin
  SQLMenuItemsShortcut.Close;
  SQLMenuItemsShortcut.ParamByName('idMenu').AsInteger := SQLMenu.FieldByName('id').AsInteger;
  SQLMenuItemsShortcut.ParamByName('shortcut').AsString := key;
  SQLMenuItemsShortcut.Open;

  if (SQLMenuItemsShortcut.RecordCount = 1) and SQLMenuItems.Locate('id', SQLMenuItemsShortcut.FieldByName('id').AsInteger, []) then
  begin
    acRun.Execute;
    key := #0;
  End
  else if (SQLMenuItemsShortcut.RecordCount > 1) then
  begin
    if not SQLMenuItems.EOF then
      SQLMenuItems.next
    else
      SQLMenuItems.first;

    while not SQLMenuItems.EOF do
    begin
      if SQLMenuItems.FieldByName('shortcut').AsString = key then
        exit;
      SQLMenuItems.Next;
    end;
    SQLMenuItems.Locate('shortcut', key, []);
  end
end;

procedure TMainForm.MainGridKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
Var
  lItemType: TMenuItemType;
begin
  if SQLMenuItems.RecordCount > 0 then
    lItemType := strToMit(SQLMenuItems.FieldByName('itemType').AsString)
  else
    lItemType := MITNone;

  if (Key = VK_Return) and (FRecNo <> SQLMenuItems.RecNo) and (FRecNo > 0) then
  begin
    SQLMenuItems.RecNo := FRecNo;
  end;

  if (Key = VK_Return) or ((Key = VK_RIGHT) and (lItemType in [MITmenu, MITmenuprog, MITmenufile, MITmenuprogreload, MITmenuwindow])) then
  begin
    if not FKeyStop then
    begin
      acRun.Execute;
    end
    else
    begin
      FKeyStop := false;
    end
  End
  else if ((Key = VK_LEFT) or (Key = VK_BACK) or (Key = VK_ESCAPE)) and (SQLMenu.FieldByName('upMenuId').AsInteger > 0) then
  begin
    NavigateUp
  End
  else if (Key = VK_ESCAPE) and (SQLMenu.FieldByName('upMenuId').AsInteger = 0) then
    MainForm.Close
  else if (key = VK_UP) and SQLMenuItems.BOF then
  begin
    SQLMenuItems.Last;
    FLastResNo := SQLMenuItems.RecNo + 1;
  end
  else if (key = VK_DOWN) and SQLMenuItems.EOF then
  begin
    SQLMenuItems.First;
    FLastResNo := SQLMenuItems.RecNo - 1;
  end;

  if (SQLMenuItems.RecordCount > 0) and (strToMit(SQLMenuItems.FieldByName('itemType').AsString) = MITseparator) then
    if SQLMenuItems.RecNo > FLastResNo then
      SQLMenuItems.Next
    else
      SQLMenuItems.Prior;
end;

procedure TMainForm.acDebugExecute(Sender: TObject);
var
  lForm: TDebugForm;
begin
  lForm := TDebugForm.Create(self);
  try
    lForm.ShowModal;
  finally
    FreeAndNil(lForm);
  end;
end;

procedure TMainForm.acFindExecute(Sender: TObject);
Begin
  FindSwitch;
end;

procedure TMainForm.acGlobalSearchExecute(Sender: TObject);
begin
  FindSwitch;
  If pnlFind.Visible And (edFind.Text = '') then
  begin
    edFind.Text := '*';
    edFind.SelStart := Length(edFind.Text);
  end;
end;

procedure TMainForm.acGlobalSearchUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not pnlFind.Visible;
end;

procedure TMainForm.acKeepOpenExecute(Sender: TObject);
Begin
  FKeepOpen := not FKeepOpen;
  if FKeepOpen then
  begin
    Self.BorderStyle := bsDialog;
    Self.FormStyle := fsNormal;
  End
  else
  begin
    Self.BorderStyle := bsNone;
    Self.FormStyle := fsSystemStayOnTop;
  End;
end;

procedure TMainForm.RunAsync(const aCmd: string);
var
  sl, slCmd: TStringList;
  lParams, lCmd, lPath, l, s, lPreCmd: string;
  lExe: RawByteString;
Const
  BEGIN_SL = 0;
begin
  sl := TStringList.Create;
  try
    lPreCmd := ReplaceText(aCmd, '%s', extraParam);
    lPreCmd := ReplaceText(lPreCmd, '''', '"');
    slCmd := tStringList.Create;
    slCmd.Delimiter := ' ';
    slCmd.DelimitedText := lPreCmd;
    lCmd := slCmd[BEGIN_SL];
    slCmd.Delete(BEGIN_SL);

    ////sl.StrictDelimiter := true;
    //sl.Delimiter := ' ';
    //sl.DelimitedText := lParams;
    //slCmd.AddStrings(sl);

    // expand path
    if FileExists(lCmd) then
      lExe := lCmd
    else
    begin
      lPath := GetEnvironmentVariable('PATH');
      lExe := ExeSearch(lCmd, lPath);

      {$IFDEF Windows}
      // on windows try search with extension .exe
      if lExe = '' then
         lExe := ExeSearch(lCmd + '.exe', lPath);
      {$ENDIF}
    End;

    //for s in slCmd do
    //  lParams := lParams + s;

    if lExe = '' then
       lExe := lCmd; // on windows this work ok ie. for pwsh.exe

    AsyncProcess1.Executable := lExe;
    { TODO -cfeat : add execute in specific directory }
    // AsyncProcess1.CurrentDirectory := ;
    AsyncProcess1.Parameters.Clear;
    AsyncProcess1.Parameters.AddStrings(slCmd);
    AsyncProcess1.Execute;

  finally
    FreeAndNil(slCmd);
    sl.Free;
  end;
end;

procedure TMainForm.setMenuShortcut(const lId: String; aKeySet: String);
Var
  lShCut: Char;
  s: Char;
  lMenuCount: LongInt;
Begin
  SQLMenuItems.First;
  While Not SQLMenuItems.EOF Do
  Begin
    If (SQLMenuItems.FieldByName('shortcut').AsString = '') And (strToMit(SQLMenuItems.FieldByName('itemType').AsString) <> MITseparator) Then
    Begin
      if Length(aKeySet) = 0 then
        aKeySet := SQLMenuItems.FieldByName('name').AsString;
      For s In aKeySet Do
      Begin
        lShCut := LowerCase(s);

        SQLMenuItemsShortcut.Close;
        SQLMenuItemsShortcut.ParamByName('idMenu').AsString := lId;
        SQLMenuItemsShortcut.ParamByName('shortcut').AsString := lShCut;
        SQLMenuItemsShortcut.Open;
        lMenuCount := SQLMenuItemsShortcut.RecordCount;

        If (lShCut <> '') And (lMenuCount = 0) And (lShCut In ['a' .. 'z', '0' .. '9']) Then
        Begin
          SQLMenuItems.Edit;
          SQLMenuItems.FieldByName('shortcut').AsString := lShCut;
          SQLMenuItems.CheckBrowseMode;
          SQLMenuItems.ApplyUpdates;
          break;
        End;
      End;
    End;
    SQLMenuItems.Next;
  End;
End;

procedure TMainForm.SetSearchCount(const aValue: LongInt);
Begin
  If FSearchCount = aValue Then Exit;
  FSearchCount := aValue;
End;

procedure TMainForm.SetSeparatorRow(const State: TGridDrawState;
  const Column: TColumn; const DataCol: Integer; const Rect: TRect;
  const aGrid: TDBGrid);
Begin
  If SQLMenuItems.Active And (SQLMenuItems.RecordCount > 0) Then
  Begin
    If strToMit(SQLMenuItems.FieldByName('itemType').AsString) = MITseparator Then
    Begin
      aGrid.Canvas.Font.Bold := true;
      aGrid.Canvas.Brush.Color := clSilver;

      aGrid.Canvas.FillRect(Rect);
      aGrid.DefaultDrawColumnCell(Rect, DataCol, Column, State);
    End;
  End;
End;

procedure TMainForm.acRunExecute(Sender: TObject);
Var
  lItemType: TMenuItemType;
  lSubMenuId, lMenuItemId, lLoad, lReload, lTime, lInterval: LongInt;
  lResult: Boolean;
  slCmd: TStringList;
  lCmd: String;
begin
  lItemType := strToMit(SQLMenuItems.FieldByName('itemType').AsString);

  if lItemType in [MITprog] then
  begin
    RunAsync(SQLMenuItems.FieldByName('cmd').AsString);
    if not FKeepOpen then
      MainForm.Close;
  End
  else if lItemType in [MITrunonce] then
  begin
    {$IFDEF Windows}
    {TODO -oLebeda -cfeat: for runonce check running program on current desktop by exe}
    slCmd := tStringList.Create;
    try
        slCmd.Delimiter := ' ';
        slCmd.DelimitedText := SQLMenuItems.FieldByName('cmd').AsString;
        lCmd := slCmd[0];
    finally
      FreeAndNil(slCmd);
    end;
    if not ActivateProcess(lCmd) then
    {$ENDIF}
    begin
      RunAsync(SQLMenuItems.FieldByName('cmd').AsString);
      if not FKeepOpen then
        MainForm.Close;
    end;
  End
  {$IFDEF Windows}
  else if lItemType =  MITwindow then
  begin
    ActivateWindow(SQLMenuItems.FieldByName('cmd').AsString)
  end
  else if lItemType =  MITmenuwindow then
  begin
    WindowMenu(SQLMenuItems.FieldByName('subMenuId').AsInteger, SQLMenuItems.FieldByName('id').AsInteger);
  end
  {$ENDIF}
  else if lItemType =  MITmenuprogreload then
  begin
    lSubMenuId := SQLMenuItems.FieldByName('subMenuId').AsInteger;
    lMenuItemId := SQLMenuItems.FieldByName('id').AsInteger;

    { TODO -crefactor : sjednotit s předchozí větví a obalit beginUpdate/EndUpdate }
    if lSubMenuId = 0 then
    begin
      // create menu
      lSubMenuId := AddMenu(
          SQLMenuItems.FieldByName('name').AsString,
          SQLMenu.FieldByName('id').AsInteger,
          SQLMenuItems.FieldByName('subMenuCmd').AsString,
          '',
          SQLMenuItems.FieldByName('subMenuReloadInterval').AsInteger
      );
      MenuDB.ExecuteDirect('update menuItem set subMenuId = ' + IntToStr(lSubMenuId) + ' where id = ' + IntToStr(lMenuItemId));
      LoadMenuFromProcess(SQLMenu.FieldByName('cmd').AsString);
    End
    else
    begin
      setActiveMenu(lSubMenuId);

      lLoad := SQLMenu.FieldByName('Load').AsInteger;
      lReload := SQLMenu.FieldByName('reloadInterval').AsInteger;
      lTime := DateTimeToTimeStamp(time).Time div 1000;
      lInterval := lTime - lLoad;
      if lInterval > lReload then
      begin
        MenuDB.ExecuteDirect('delete from menuItem where menuId = ' + IntToStr(lSubMenuId));
        LoadMenuFromProcess(SQLMenu.FieldByName('cmd').AsString);
      End;

    End;
    setActiveMenu(lSubMenuId); // reload after build
  End
  else if lItemType =  MITmenu then
  begin
    lResult := setActiveMenu(SQLMenuItems.FieldByName('subMenuId').AsInteger); // reload after navigate
    //if lResult then
    //   showMenu;
  End;

  closeFindPanel;
end;

procedure TMainForm.edFindKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
Begin
  if (Key = VK_DOWN) then
  begin
    if SQLMenuItems.EOF then
      SQLMenuItems.First
    else
      SQLMenuItems.Next;

    //SQLMenuItems.First;
    //MainGrid.SetFocus;
    key := 0;
  End
  else if (Key = VK_UP) then
  begin
    if SQLMenuItems.BOF then
      SQLMenuItems.Last
    else
      SQLMenuItems.Prior;

    //SQLMenuItems.Last;
    //MainGrid.SetFocus;
    key := 0;
  End
  else if (Key = VK_Return) then
  begin
    //MainGrid.SetFocus;
    Key := 0;
    FKeyStop := True;
    acRun.Execute;
  End;
end;

procedure TMainForm.edFindKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  lUpMenuId: Integer;
Begin
  if (Key = VK_ESCAPE) then
  begin
    lUpMenuId := SQLMenu.FieldByName('upMenuId').AsInteger;
    if (lUpMenuId = 0) or FExecIfOne then
      MainForm.Close
    else
    begin
      closeFindPanel(true);
      NavigateUp;
    end;
  End
  else if not((Key = VK_DOWN) or (Key = VK_UP) or (Key = VK_Return)) and (FLastFind <> edFind.Text) then
  begin
    if isExternalSearch then
    begin
      // restart timer
      ThrTimer.Enabled := false;
      ThrTimer.Enabled := True;
    End
    else
    begin
      showMenu;
      FLastFind := edFind.Text;
    End;
  End;
end;

procedure TMainForm.FormActivate(Sender: TObject);
Begin
  if FExecIfOne and (SQLMenuItems.RecordCount = 1) then
  begin
   MainForm.Close;
  end;

  if pnlFind.Visible then
    edFind.SelStart := Length(edFind.Text);
end;

procedure TMainForm.SQLMenuAfterInsert(DataSet: TDataSet);
begin
  SQLMenu.FieldByName('Load').AsInteger := DateTimeToTimeStamp(time).Time div 1000;
end;

procedure TMainForm.SQLMenuAfterScroll(DataSet: TDataSet);
begin
  { TODO -crefactor : smazat zakomentovaný kód a prázdné metody }
 //if not SQLMenu.Modified then
 //   showMenu;
end;

procedure TMainForm.ThrTimerTimer(Sender: TObject);
Begin
  showMenu;
  FLastFind := edFind.Text;
  ThrTimer.Enabled := False;
end;

procedure TMainForm.LoadMenuFromLines(const aLines: TStringList);
var
  lLine: String;
  i: integer;
  lMenuItemParser: TMenuItemParser;
begin
  // insert into menu
  for i := 0 to aLines.Count - 1 do
  begin
    lLine := Trim(aLines[i]);
    lLine := DelSpace1(lLine);
    if (lLine <> '') and (not AnsiStartsStr('#', lLine) or AnsiStartsStr('#!', lLine)) then
    begin
      if AnsiStartsStr('#!', lLine) then
        Delete(lLine, 1, 2);

      lMenuItemParser := TMenuItemParser.Create(lLine);
      try
        if not(lMenuItemParser.itemType in [MITEndMenu, MITNone]) then
          AddMenuItem(lMenuItemParser);
      finally
        FreeAndNil(lMenuItemParser);
      end;
    end;
  end;
end;

procedure TMainForm.LoadMenuFromProcess(const aCmd: String);
Var
  lSl: TStringListUTF8;
  j: Integer;
  //F:TextFile;
  F:TextFile;
  lLine: WideString;
  lVal, lEncCon, lEncDef: String;
  lExt, lCan: Boolean;
  c: WideChar;
Begin
  lExt := isExternalSearch;
  lCan := canExternalSearch;
  if not lExt or lCan then
  begin
    // load data from process
    //Process1.Options := [poWaitOnExit, poNoConsole, poUsePipes];
    ProcessUTF81.CurrentDirectory := GetEnvironmentVariable('HOME');
    ProcessUTF81.CommandLine := aCmd;
    ProcessUTF81.Execute;

    lSl := TStringListUTF8.Create;
    Try
      //Process1.WaitOnExit;
      //lSl.LoadFromStream(ProcessUTF81.Output);
      //lsl.SaveToFile('c:\tmp\menuDebug.txt');

      AssignStream(F, ProcessUTF81.Output);
      Reset(F);
      while not Eof(F) do
      begin
        Readln(F, lLine);

        lVal := RepairCzechCharacters(lLine);

        lSl.Append(lVal);
        //ShowMessage(lSl[0]);
      End;
      CloseFile(F);

      LoadMenuFromLines(lSl);
      //lsl.SaveToFile('c:\tmp\menuDebug.txt');

    Finally
      FreeAndNil(lSl);
    End;

  end;

  SQLMenu.Edit;
  SQLMenu.FieldByName('Load').AsInteger := DateTimeToTimeStamp(time).Time div 1000;
  SQLMenu.Post;
  SQLMenu.ApplyUpdates;
End;

procedure TMainForm.showMenu(const aOrderBy: String; const aName: String);
Var
  lSql: String;
  lId, lCmd: String;
  lSearchText, i: String;
  lGlobalSearch: Boolean;
  lRecNo: LongInt;
Begin
  MainGrid.BeginUpdate;
  MainGridShortCut.BeginUpdate;
  MainGridSubmenu.BeginUpdate;
  try
    // initialize local variables
    lGlobalSearch := false;
    lSearchText := '';

    // check global search
    if Length(edFind.Text) > 0 then
    begin
      if edFind.Text[1] = '*' then
      begin
        lGlobalSearch := true;
        lSearchText :=  copy(edFind.Text, 2, Integer.MaxValue);
      end
      else
      begin
        lGlobalSearch := false;
        lSearchText := edFind.Text;
      end
    end;

    // prepare search
    If (lSearchText <> '') then
    begin
      {$IFDEF Windows}
      lSearchText := lSearchText.Replace('%CurDestop%', GetCurrentDesktopName());
      {$ENDIF}
    end;


    // regenerate if reloadInterval < 0 and lSearchText and command contains %s
    if isExternalSearch and (lSearchText <> FLastFind) then
    begin
      MenuDB.ExecuteDirect('delete from menuItem where menuId = ' + SQLMenu.FieldByName('id').AsString);
      lCmd := ReplaceText(SQLMenu.FieldByName('cmd').AsString, '%s', '"' + lSearchText + '"');
      LoadMenuFromProcess(lCmd);

      SQLMenu.Edit;
      SQLMenu.FieldByName('Load').AsInteger := -1;
      SQLMenu.CheckBrowseMode;
      SQLMenu.ApplyUpdates;
    End;

    // open Main menu
    MenuItemDS.DataSet := nil;
    SQLMenuItems.Close;
    lId := SQLMenu.FieldByName('id').AsString;
    lSql := 'select id, menuId, itemType, '
            + ' name '
            + ' , search, shortcut, '
            + ' cmd, subMenuPath, subMenuCmd, subMenuReloadInterval, subMenuId, subMenuChar, width '
            + ' from menuItem where itemType <> ''MITwinkey''  ';

    if aName <> '' then
      lSql := lSql + ' and name = ''' + Trim(aName) + ''' '
    else if not lGlobalSearch then
    begin
      lSql := lSql + ' and menuId = ''' + lId + ''' '
    end
    else
    begin
      lSql := lSql + ' and itemType <> ''MITseparator'' ';
      lSql := lSql + ' and itemType <> ''MITmenu'' ';
    end;


    If (lSearchText <> '') and not isExternalSearch and (aName = '') Then
    begin
      lSql := lSql + ' and ((search like ''%' + lSearchText + '%'') or (name like ''%' + lSearchText + '%''))'; {TODO -oLebeda -cfeat: split lSearchText by space and simulate fuzzy search}
      lSql := lSql + ' and itemType <> ''MITseparator'' ';
    end;

    lSql := lSql + ' Order by ' + aOrderBy;

    SQLMenuItems.SQL.Text := lSql;
    SQLMenuItems.Open;

    // direct execute if FExecIfOne and only one item found
    if FExecIfOne then
    begin
      if (SQLMenuItems.RecordCount = 1) then
      begin
        acRun.Execute;
        AppDeactivate(self);
      end
      else
      begin
        AppendMenuItem(FDefaultItem);
        if (SQLMenuItems.RecordCount = 1) then { TODO -crefactor : deduplicate }
        begin
          acRun.Execute;
          AppDeactivate(self);
        end;
      end;
    end;

    // open max width query
    SQLMenuItemsMaxWidth.Close;
    SQLMenuItemsMaxWidth.ParamByName('id').AsString := lId;
    SQLMenuItemsMaxWidth.Open;

    // fill implicit shortcut
    if FSearchCount > SQLMenuItems.RecordCount then
    begin
      setMenuShortcut(lId);
      setMenuShortcut(lId, C_SHORTCUT_MENU_CHARS);
      SQLMenuItems.First;
    End;

    MenuItemDS.DataSet := SQLMenuItems;
  finally
    MainGrid.EndUpdate(true);
    MainGridShortCut.EndUpdate(true);
    MainGridSubmenu.EndUpdate(true);
  end;

  if (((FSearchCount > 0) and (FSearchCount <= SQLMenuItems.RecordCount)) or isExternalSearch) and (Not pnlFind.Visible) then
  begin
    // find panel visibility
    pnlFind.Visible := True;
    ActiveControl := edFind;
    if MainForm.CanFocus then
      edFind.SetFocus;
  End
  else
    if pnlFind.Visible then
      ActiveControl := edFind;

  // form size
  SetFormSize;
End;

procedure TMainForm.SetFormSize;
var
  lHeight, lWidth: integer;
const
  lHeighReserve = 2;
begin
  MainForm.Caption := SQLMenu.FieldByName('name').AsString;

  // height
  lHeight := MainGrid.DefaultRowHeight * SQLMenuItems.RecordCount + (2* MainForm.BorderWidth) + lHeighReserve;

  if pnlFind.Visible then
    lHeight := lHeight + pnlFind.Height;

  if MainForm.Constraints.MaxHeight >= lHeight then
  begin
    MainGridSubmenu.ScrollBars := ssNone;
    MainForm.Constraints.MinHeight := lHeight;
  end
  else
    MainGridSubmenu.ScrollBars := ssAutoVertical;

  MainForm.Height := lHeight;

  // width
  lWidth := GetMaxWidth + MainGridSubmenu.Width + MainGridShortCut.Width + (2* MainForm.BorderWidth);
   MainForm.Width := lWidth;

  // centralization
  if FFormMode = FMCentral then
  begin
    Top := (Screen.Height div 2) - (MainForm.Height div 2);
    Left := (Screen.Width div 2) - (MainForm.Width div 2);;
  End;

  // mouse to menu if isn't
  if Mouse.CursorPos.Y < top then
    Mouse.CursorPos := Point(Mouse.CursorPos.X ,top +10);

  if Mouse.CursorPos.Y > (top + MainForm.Height) then
    Mouse.CursorPos := Point(Mouse.CursorPos.X , (top + MainForm.Height) - 10);
end;

procedure TMainForm.NavigateUp;
Var
  lMenuId: LongInt;
Begin
  closeFindPanel;
  lMenuId := SQLMenu.FieldByName('id').AsInteger;
  setActiveMenu(SQLMenu.FieldByName('upMenuId').AsInteger);
  SQLMenuItems.Locate('subMenuId', lMenuId, []);
End;

procedure TMainForm.WindowMenu(const aSubMenuId, aMenuItemId: Integer; const aRoot: Boolean = False);
var
  lResult: Boolean;
  lInterval: LongInt;
  lTime: LongInt;
  lReload: LongInt;
  lLoad: LongInt;
  lSubMenuId, lMenuItemId: Integer;
begin
  { TODO -cWM : zlikvidovat duplicitní konstrukce}
  lSubMenuId := aSubMenuId;
  lMenuItemId := aMenuItemId;

  MainGrid.BeginUpdate;
  MainGridShortCut.BeginUpdate;
  MainGridSubmenu.BeginUpdate;
  try
    if lSubMenuId = 0 then // if 0 create new menuitem
    begin
      // create menu
      lSubMenuId := AddMenu(
          SQLMenuItems.FieldByName('name').AsString,
          SQLMenu.FieldByName('id').AsInteger,
          SQLMenuItems.FieldByName('subMenuCmd').AsString,
          '',
          SQLMenuItems.FieldByName('subMenuReloadInterval').AsInteger
      );
      MenuDB.ExecuteDirect('update menuItem set subMenuId = ' + IntToStr(
        lSubMenuId) + ' where id = ' + IntToStr(lMenuItemId));
      LoadMenuWindows(SQLMenu.FieldByName('cmd').AsString, Application.ExeName);
    end
    else
    begin
      setActiveMenu(lSubMenuId);

      lLoad := SQLMenu.FieldByName('Load').AsInteger;
      lReload := SQLMenu.FieldByName('reloadInterval').AsInteger;
      lTime := DateTimeToTimeStamp(time).Time div 1000;
      lInterval := lTime - lLoad;
      if lInterval > lReload then
      begin
        MenuDB.ExecuteDirect('delete from menuItem where menuId = ' + IntToStr(
          lSubMenuId));
        LoadMenuWindows(SQLMenu.FieldByName('cmd').AsString, Application.ExeName);
      end;

    end;
    lResult := setActiveMenu(lSubMenuId, 'name'); // reload after build

    if aRoot then
    begin
      SQLMenu.Edit;
      SQLMenu.FieldByName('upMenuId').AsInteger := 0;
      SQLMenu.CheckBrowseMode;
    end;

  finally
    MainGrid.EndUpdate(true);
    MainGridShortCut.EndUpdate(true);
    MainGridSubmenu.EndUpdate(true);
  end;
end;

function TMainForm.AddMenu(aName: string; aUpMenuId: longint; aCmd: string;
  aPath: string; aReloadInterval: integer): integer;
begin
  SQLMenu.Insert;

  SQLMenu.FieldByName('name').AsString := aName;
  SQLMenu.FieldByName('upMenuId').AsInteger := aUpMenuID;
  SQLMenu.FieldByName('cmd').AsString := aCmd;
  SQLMenu.FieldByName('path').AsString := aPath;
  SQLMenu.FieldByName('reloadInterval').AsInteger := aReloadInterval;

  SQLMenu.Post;
  SQLMenu.ApplyUpdates;

  Result := SQLMenu.FieldByName('id').AsInteger;
end;

function TMainForm.setActiveMenu(const aIdMenu: longint; const aOrderBy: String = 'id'): Boolean;
Begin
  // sure the changes are saved
  SQLMenu.CheckBrowseMode;
  SQLMenu.ApplyUpdates;

  Result := SQLMenu.Locate('id', aIdMenu, []);
  showMenu(aOrderBy);
End;

procedure TMainForm.AddMenuItem(var lMenuItemParser: TMenuItemParser);
begin
  SQLMenuItems.Append;
  //id, menuId, itemType, name, search, shortcut, cmd, subMenuPath, subMenuCmd, subMenuReloadInterval, subMenuId, subMenuChar
  SQLMenuItems.FieldByName('menuId').AsInteger := lMenuItemParser.menuId;
  SQLMenuItems.FieldByName('itemType').AsString := MitToStr(lMenuItemParser.itemType);
  SQLMenuItems.FieldByName('name').AsWideString := lMenuItemParser.Name; { TODO -cfeat : nahradit _ mezerami ve jméně }
  SQLMenuItems.FieldByName('search').AsString := lMenuItemParser.search;
  SQLMenuItems.FieldByName('shortcut').AsString := lMenuItemParser.shortcut;
  SQLMenuItems.FieldByName('cmd').AsString := lMenuItemParser.cmd;
  SQLMenuItems.FieldByName('subMenuPath').AsString := lMenuItemParser.subMenuPath;
  SQLMenuItems.FieldByName('subMenuCmd').AsString := lMenuItemParser.subMenuCmd;
  SQLMenuItems.FieldByName('subMenuReloadInterval').AsInteger := lMenuItemParser.subMenuReloadInterval;
  SQLMenuItems.FieldByName('subMenuId').AsInteger := lMenuItemParser.subMenuId;
  SQLMenuItems.FieldByName('subMenuChar').AsString := lMenuItemParser.subMenuChar;
  SQLMenuItems.FieldByName('width').AsInteger := GetTextWidth(lMenuItemParser.Name);
  SQLMenuItems.Post;
  SQLMenuItems.ApplyUpdates;
end;

procedure TMainForm.LoadMenuFromFile(const aFile: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(aFile);
    LoadMenuFromLines(sl);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TMainForm.AppendMenuItem(const aItem: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Add(aItem);
    LoadMenuFromLines(sl);
  finally
    FreeAndNil(sl);
  end;
end;

end.
