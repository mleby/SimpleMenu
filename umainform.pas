unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, sqlite3conn, sqldb, Forms, Controls, Graphics,
  DBGrids, ActnList, ExtCtrls, StdCtrls, Grids,
  AsyncProcess, Clipbrd, LazUTF8, UTF8Process, LazFileUtils, uMenuItem,
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
    procedure acFindExecute(Sender: TObject);
    procedure acGlobalSearchExecute(Sender: TObject);
    procedure acGlobalSearchUpdate(Sender: TObject);
    procedure acKeepOpenExecute(Sender: TObject);
    procedure acRunExecute(Sender: TObject);
    procedure edFindKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure edFindKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MainGridCellClick(Column: TColumn);
    procedure MainGridDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: integer; Column: TColumn; State: TGridDrawState);
    procedure MainGridEnter(Sender: TObject);
    procedure MainGridKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure MainGridKeyPress(Sender: TObject; var Key: char);
    procedure MainGridKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure MainGridSubmenuDrawColumnCell(Sender: TObject;
      const Rect: TRect; DataCol: integer; Column: TColumn; State: TGridDrawState);
    procedure pnlFindEnter(Sender: TObject);
    procedure pnlFindExit(Sender: TObject);
    procedure SQLMenuAfterInsert(DataSet: TDataSet);
    procedure SQLMenuAfterScroll(DataSet: TDataSet);
    procedure ThrTimerTimer(Sender: TObject);
  private
    FExtraParam: string;
    FFormMode: TFormMode;
    FRecNo: longint;
    FKeepOpen: boolean;
    FExecIfOne: boolean;
    FDefaultItem: string;
    FKeyStop: boolean;
    FSearchCount: longint;
    FLastResNo: integer; // for navigation over separators
    FLastFind: string;
    procedure AplyGeneratedMenu(const aRoot: boolean; const aMenuItemId: integer;
      const aSubMenuId: integer);
    procedure AppDeactivate(Sender: TObject);
    procedure closeFindPanel(const aForce: boolean = False);
    procedure FindSwitch;
    function GetMaxWidth: integer;
    function GetTextWidth(const aText: string): integer;
    function isExternalSearch: boolean;
    function canExternalSearch: boolean;
    procedure LoadMenuFromLines(const aLines: TStringList);
    procedure LoadMenuFromProcess(const aCmd: string);
    procedure RunAsync(const aCmd: string);
    procedure setMenuShortcut(const lId: string; aKeySet: string = '');
    procedure SetSearchCount(const aValue: longint);
    procedure SetSeparatorRow(const State: TGridDrawState; const Column: TColumn;
      const DataCol: integer; const Rect: TRect; const aGrid: TDBGrid);
    procedure showMenu(const aOrderBy: string = 'id'; const aName: string = '');
    procedure SetFormSize;
    procedure NavigateUp;
    procedure WindowMenu(const aSubMenuId, aMenuItemId: integer;
      const aRoot: boolean = False);
    procedure PathMenu(const aSubMenuId, aMenuItemId: integer;
      const aPath, aCmd: string; const aRoot: boolean = False);
    { private declarations }
  public
    function AddMenu(aName: string; aUpMenuId: longint; aCmd: string = '';
      aPath: string = ''; aReloadInterval: integer = 0): integer;
    function setActiveMenu(const aIdMenu: longint;
      const aOrderBy: string = 'id'): boolean;
    procedure AddMenuItem(var lMenuItemParser: TMenuItemParser);
    procedure LoadMenuFromFile(const aFile: string);
    procedure AppendMenuItem(const aItem: string);

    property FormMode: TFormMode read FFormMode write FFormMode;
    property SearchCount: longint read FSearchCount write SetSearchCount;
    property extraParam: string read FExtraParam write FExtraParam;
  end;

var
  MainForm: TMainForm;

implementation

uses LazStringUtils, strutils, debugForm, uHacks, StreamIO, LCLType,
  Dialogs, lconvencoding,
  LazUTF8Classes, FileUtil;

const
  C_SHORTCUT_MENU_CHARS = 'abcdefghijklmnopqrstuvwxyz0123456789';

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  ppi, ppiDesigned: integer;
  lMenuName: string;
begin
  if Application.HasOption('h', 'help') then
  begin
    ShowMessage(
      'Usage: simpleMenu -(f|p|m|w) "menu file or cmd" [options...]' +
      #10#13 + '         one of -f/-p/-m must be specified as  point for menu' +
      #10#13 + '    -h --help             show this help' + #10#13 +
      '    -k --keep             keep menu open after choise' + #10#13 +
      '    -f X --file=X         path to menu file used as start point for menu' +
      #10#13 + '    -m X --menuitem=X     text content of menu' +
      #10#13 + '    -p X --process=X      command for generate menu' +
      #10#13 + '    -s X --search=X       count of menu items for automatic enable find'
      + #10#13 +
      '    -q X --query=X        automatic enable find entry and fill start query' +
      #10#13 + '    -r X --reload=X       dynamic menu with minimal chars for search' +
      #10#13 + '    -x X --showfile=X     extra options for menu cmd' +
      #10#13 + '    -w --windowmenu       window menu' + #10#13 +
      '    -1 X -execone=X       automatic execute if matched only one item, execute X if 0 items found or append X to menu');
    Halt;
    { TODO -cfeat : nahrazení %s pomocí uživatelského stringu : jako oddělovač - při spuštění při neakvním vyhledávání se zeptat }
    { TODO -crefactor : Vyčlenit práci s DB do samostatného DM }

  end;

  if not (Application.HasOption('f', 'file') or
    Application.HasOption('p', 'process') or Application.HasOption('m', 'menu') or
    Application.HasOption('w', 'windowmenu')) then
  begin
    ShowMessage('One menu source must be specified (file or process or menuitem).');
    Halt;
  end;


  ppiDesigned := self.DesignTimePPI;
  ppi := Screen.PixelsPerInch;
  MainForm.Constraints.MaxHeight := round(Screen.Height * 0.9 * (ppiDesigned / ppi));
  MainForm.Constraints.MaxWidth := round(Screen.Width * 0.9 * (ppiDesigned / ppi));

  FFormMode := FMNormal;
  // color

  // sure create DB
  MenuDB.Close;

  // only for dev
  //DeleteFile('C:\tmp\debugMenu2.db'); // uncoment only for developnet (real DB for object inspector and design in lazarus)
  //MenuDB.DatabaseName := 'C:\tmp\debugMenu2.db'; // uncoment only for developnet (real DB for object inspector and design in lazarus)
  MenuDB.DatabaseName := ':memory:';

  MenuDB.Open;
  MenuDB.ExecuteDirect('PRAGMA encoding="UTF-8"');
  MenuDB.ExecuteDirect(
    'CREATE TABLE IF NOT EXISTS menu (id INTEGER PRIMARY KEY , upMenuId INTEGER, name NOT NULL, cmd, path, load INTEGER, reloadInterval INTEGER)');
  MenuDB.ExecuteDirect(
    'CREATE TABLE IF NOT EXISTS menuItem (id INTEGER PRIMARY KEY , menuId INTEGER NOT NULL, itemType, name, search, shortcut, cmd, subMenuPath, subMenuCmd, subMenuReloadInterval INTEGER, subMenuId INTEGER, subMenuChar, width INTEGER DEFAULT 100, FOREIGN KEY(menuId) REFERENCES menu(id))');
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
      SQLMenu.FieldByName('path').AsString :=
        Application.GetOptionValue('w', 'windowmenu');
      SQLMenu.CheckBrowseMode;
      LoadMenuFromFile(SQLMenu.FieldByName('path').AsString);
    end;
  end;

  if Application.HasOption('r', 'reload') then
  begin
    SQLMenu.Edit;
    SQLMenu.FieldByName('Load').AsInteger := -1;
    SQLMenu.FieldByName('reloadInterval').AsInteger :=
      -1 * StrToInt(Application.GetOptionValue('r', 'reload'));
    SQLMenu.CheckBrowseMode;
  end;

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
    Application.OnDeactivate := @AppDeactivate;
    FKeepOpen := False;
  end
  else
    FKeepOpen := True;

  if Application.HasOption('1', 'execone') then
  begin
    FExecIfOne := True;
    FDefaultItem := Application.GetOptionValue('1', 'execone');
  end
  else
    FExecIfOne := False;

  if Application.HasOption('q', 'query') then
  begin
    pnlFind.Visible := True;
    edFind.Text := Application.GetOptionValue('q', 'query');
  end;

  if Application.HasOption('w', 'windowmenu') then
    WindowMenu(0, 0, True)
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

    //MenuDB.ExecuteDirect('update menuItem set menuId = 1 where id = 2');
    //setActiveMenu(1);
  end
  else
    showMenu;
end;

procedure TMainForm.AppDeactivate(Sender: TObject);
begin
  if not FKeepOpen then
  begin
    if FExecIfOne then
      MainForm.Close;
  end;
end;

procedure TMainForm.AplyGeneratedMenu(const aRoot: boolean;
  const aMenuItemId: integer; const aSubMenuId: integer);
var
  lMenuItemId: integer;
  lSubMenuId: integer;
  lLoad: longint;
  lReload: longint;
  lTime: longint;
  lInterval: longint;
  lResult: boolean;
begin
  lSubMenuId := aSubMenuId;
  lMenuItemId := aMenuItemId;

  MainGrid.BeginUpdate;
  MainGridShortCut.BeginUpdate;
  MainGridSubmenu.BeginUpdate;
  try
    if lSubMenuId = 0 then // if 0 create new menuitem
    begin
      // create menu
      lSubMenuId := AddMenu(SQLMenuItems.FieldByName('name').AsString,
        SQLMenu.FieldByName('id').AsInteger,
        SQLMenuItems.FieldByName('subMenuCmd').AsString, '',
        SQLMenuItems.FieldByName('subMenuReloadInterval').AsInteger);
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
        MenuDB.ExecuteDirect('delete from menuItem where menuId = ' +
          IntToStr(lSubMenuId));
        LoadMenuWindows(SQLMenu.FieldByName('cmd').AsString, Application.ExeName
          );
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
    MainGrid.EndUpdate(True);
    MainGridShortCut.EndUpdate(True);
    MainGridSubmenu.EndUpdate(True);
  end;
end;

procedure TMainForm.closeFindPanel(const aForce: boolean);
begin
  if FExecIfOne and (SQLMenuItems.RecordCount = 1) then
  begin
    AppDeactivate(self);
  end
  else if (edFind.Text <> '') or aForce then
  begin
    edFind.Text := '';
    //  showMenu; { TODO : jen pokud není zavíráno }
    MainGrid.SetFocus;
    pnlFind.Visible := False;
    MainGridShortCut.Visible := not pnlFind.Visible;
  end;
end;

procedure TMainForm.FindSwitch;
begin
  if MainGrid.Focused then
    pnlFind.Visible := True
  else if not pnlFind.Visible then
    pnlFind.Visible := True
  else if pnlFind.Visible and ((edFind.Text = '') or (edFind.Text = '*')) then
  begin
    pnlFind.Visible := False;
    if (edFind.Text = '*') then
    begin
      edFind.Text := '';
      showMenu;
    end;
  end;

  if pnlFind.Visible and not edFind.Focused then
  begin
    edFind.SetFocus;
  end
  else
  begin
    MainGrid.SetFocus;
  end;

  MainGridShortCut.Visible := not pnlFind.Visible;

  SetFormSize;
end;

function TMainForm.GetMaxWidth: integer;
begin
  if SQLMenuItemsMaxWidth.Active and (SQLMenuItemsMaxWidth.RecordCount = 1) and
    not SQLMenuItemsMaxWidth.FieldByName('width').IsNull then
    Result := SQLMenuItemsMaxWidth.FieldByName('width').AsInteger + 10
  else
    Result := 500;
end;

function TMainForm.GetTextWidth(const aText: string): integer;
var
  W: integer;
  BM: TBitmap;
begin
  BM := TBitmap.Create;
  try
    BM.Canvas.Font := MainGrid.Font;
    W := BM.Canvas.TextWidth(aText);
  finally
    BM.Free;
  end;

  Result := W + 8; { TODO -crefactor : Magic const 8 }
  //Result := Length(aText) * 30
end;

function TMainForm.isExternalSearch: boolean;
var
  lReloadInterval: longint;
  lCmd: string;
  lDynCmd: SizeInt;
begin
  lReloadInterval := SQLMenu.FieldByName('reloadInterval').AsInteger;
  lCmd := SQLMenu.FieldByName('cmd').AsString;
  lDynCmd := Pos('%s', lCmd);
  Result := (lReloadInterval < 0) and (lDynCmd > 0);
end;

function TMainForm.canExternalSearch: boolean;
var
  lReloadInterval: longint;
  lCmd: string;
  lDynCmd: SizeInt;
begin
  lReloadInterval := SQLMenu.FieldByName('reloadInterval').AsInteger;
  lCmd := SQLMenu.FieldByName('cmd').AsString;
  lDynCmd := Pos('%s', lCmd);
  Result := (lReloadInterval < 0) and (Length(edFind.Text) >= abs(lReloadInterval)) and
    (lDynCmd > 0);
end;

procedure TMainForm.MainGridCellClick(Column: TColumn);
begin
  acRun.Execute;
end;

procedure TMainForm.MainGridDrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: integer; Column: TColumn; State: TGridDrawState);
begin
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
  const Rect: TRect; DataCol: integer; Column: TColumn; State: TGridDrawState);
begin
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

procedure TMainForm.MainGridKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  // navigation over separators
  FLastResNo := SQLMenuItems.RecNo;

  if (Key = VK_Return) then
  begin
    FRecNo := SQLMenuItems.RecNo;
    SQLMenuItems.Prior; // ugly hack - enter goes to next
  end;
end;

procedure TMainForm.MainGridKeyPress(Sender: TObject; var Key: char);
var
  lRecCount: longint;
begin
  SQLMenuItemsShortcut.Close;
  SQLMenuItemsShortcut.ParamByName('idMenu').AsInteger :=
    SQLMenu.FieldByName('id').AsInteger;
  SQLMenuItemsShortcut.ParamByName('shortcut').AsString := key;
  SQLMenuItemsShortcut.Open;

  lRecCount := SQLMenuItemsShortcut.RecordCount;

  if (lRecCount = 1) and SQLMenuItems.Locate('id',
    SQLMenuItemsShortcut.FieldByName('id').AsInteger, []) then
  begin
    acRun.Execute;
    key := #0;
  end
  else if (lRecCount > 1) then
  begin
    if not SQLMenuItems.EOF then
      SQLMenuItems.Next
    else
      SQLMenuItems.First;

    while not SQLMenuItems.EOF do
    begin
      if SQLMenuItems.FieldByName('shortcut').AsString = key then
        exit;
      SQLMenuItems.Next;
    end;
    SQLMenuItems.Locate('shortcut', key, []);
  end;
end;

procedure TMainForm.MainGridKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
var
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

  if (Key = VK_Return) or ((Key = VK_RIGHT) and
    (lItemType in [MITmenu, MITmenuprog, MITmenufile, MITmenuprogreload,
    MITmenuwindow, MITmenupath])) then
  begin
    if not FKeyStop then
    begin
      acRun.Execute;
    end
    else
    begin
      FKeyStop := False;
    end;
  end
  else if ((Key = VK_LEFT) or (Key = VK_BACK) or (Key = VK_ESCAPE)) and
    (SQLMenu.FieldByName('upMenuId').AsInteger > 0) then
  begin
    NavigateUp;
  end
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

  if (SQLMenuItems.RecordCount > 0) and
    (strToMit(SQLMenuItems.FieldByName('itemType').AsString) = MITseparator) then
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
begin
  FindSwitch;
end;

procedure TMainForm.acGlobalSearchExecute(Sender: TObject);
begin
  FindSwitch;
  if pnlFind.Visible and (edFind.Text = '') then
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
begin
  FKeepOpen := not FKeepOpen;
  if FKeepOpen then
  begin
    Self.BorderStyle := bsDialog;
    Self.FormStyle := fsNormal;
  end
  else
  begin
    Self.BorderStyle := bsNone;
    Self.FormStyle := fsSystemStayOnTop;
  end;
end;

procedure TMainForm.RunAsync(const aCmd: string);
var
  sl, slCmd: TStringList;
  lParam, lCmd, lPath, l, s, lPreCmd: string;
  lExe: rawbytestring;
const
  BEGIN_SL = 0;
begin
  Screen.Cursor := crHourGlass;
  sl := TStringList.Create;
  try
    lPreCmd := ReplaceText(aCmd, '%s', extraParam);
    lPreCmd := ReplaceText(lPreCmd, '''', '"');
    slCmd := TStringList.Create;
    slCmd.Delimiter := ' ';
    slCmd.DelimitedText := lPreCmd;
    lCmd := slCmd[BEGIN_SL];
    slCmd.Delete(BEGIN_SL);

    AsyncProcess1.ShowWindow := swoShow;
    AsyncProcess1.CurrentDirectory := GetEnvironmentVariable('HOME');
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
    end;

    if lExe = '' then
      lExe := lCmd; // on windows this work ok ie. for pwsh.exe

    AsyncProcess1.Executable := lExe;
    AsyncProcess1.Parameters.Clear;

    for s in slCmd do
    begin
      lParam := s.Replace('%clipbrd%', Clipboard.AsText);
      { #todo : %date% %time% %isodate% }

      { TODO : zpracovat parametry příkazu #max #min #x:N #y:N #width:N #height:N #dir:XY #noft}
      { #todo : #byrecent - big change - need store items with identification and counter of run (only for run/runonce items)}
      { #todo : #noft jen odstranit + přidat not like do FT dotazů }

      if lParam = '#max' then
        AsyncProcess1.ShowWindow := swoMaximize
      else if lParam = '#min' then
        AsyncProcess1.ShowWindow := swoMinimize
      else if StartsStr('#dir:', lParam) then
      begin
        lParam := ReplaceStr(lParam, '#dir:', '');
        AsyncProcess1.CurrentDirectory := lParam;
      end
      else
        AsyncProcess1.Parameters.Add(lParam);
    end;

    AsyncProcess1.Execute;

  finally
    FreeAndNil(slCmd);
    sl.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.setMenuShortcut(const lId: string; aKeySet: string);
var
  lShCut: char;
  s: char;
  lMenuCount: longint;
begin
  SQLMenuItems.First;
  while not SQLMenuItems.EOF do
  begin
    if (SQLMenuItems.FieldByName('shortcut').AsString = '') and
      (strToMit(SQLMenuItems.FieldByName('itemType').AsString) <> MITseparator) then
    begin
      if Length(aKeySet) = 0 then
        aKeySet := SQLMenuItems.FieldByName('name').AsString;
      for s in aKeySet do
      begin
        lShCut := LowerCase(s);

        SQLMenuItemsShortcut.Close;
        SQLMenuItemsShortcut.ParamByName('idMenu').AsString := lId;
        SQLMenuItemsShortcut.ParamByName('shortcut').AsString := lShCut;
        SQLMenuItemsShortcut.Open;
        lMenuCount := SQLMenuItemsShortcut.RecordCount;

        if (lShCut <> '') and (lMenuCount = 0) and
          (lShCut in ['a' .. 'z', '0' .. '9']) then
        begin
          SQLMenuItems.Edit;
          SQLMenuItems.FieldByName('shortcut').AsString := lShCut;
          SQLMenuItems.CheckBrowseMode;
          SQLMenuItems.ApplyUpdates;
          break;
        end;
      end;
    end;
    SQLMenuItems.Next;
  end;
end;

procedure TMainForm.SetSearchCount(const aValue: longint);
begin
  if FSearchCount = aValue then
    Exit;
  FSearchCount := aValue;
end;

procedure TMainForm.SetSeparatorRow(const State: TGridDrawState;
  const Column: TColumn; const DataCol: integer; const Rect: TRect;
  const aGrid: TDBGrid);
begin
  if SQLMenuItems.Active and (SQLMenuItems.RecordCount > 0) then
  begin
    if strToMit(SQLMenuItems.FieldByName('itemType').AsString) = MITseparator then
    begin
      aGrid.Canvas.Font.Bold := True;
      aGrid.Canvas.Brush.Color := clSilver;

      aGrid.Canvas.FillRect(Rect);
      aGrid.DefaultDrawColumnCell(Rect, DataCol, Column, State);
    end;
  end;
end;

procedure TMainForm.acRunExecute(Sender: TObject);
var
  lItemType: TMenuItemType;
  lSubMenuId, lMenuItemId, lLoad, lReload, lTime, lInterval: longint;
  lResult: boolean;
  slCmd: TStringList;
  lCmd: string;
begin
  Screen.Cursor := crHourGlass;
  try
    lItemType := strToMit(SQLMenuItems.FieldByName('itemType').AsString);

    if lItemType in [MITprog] then
    begin
      RunAsync(SQLMenuItems.FieldByName('cmd').AsString);
      if not FKeepOpen then
        MainForm.Close;
    end
    else if lItemType in [MITrunonce] then
    begin
      {$IFDEF Windows}
      slCmd := TStringList.Create;
      try
        slCmd.Delimiter := ' ';
        slCmd.DelimitedText := SQLMenuItems.FieldByName('cmd').AsString;
        lCmd := slCmd[0];
      finally
        FreeAndNil(slCmd);
      end;
      if not ActivateProcess(lCmd) then
      {$ENDIF}
        RunAsync(SQLMenuItems.FieldByName('cmd').AsString);
      if not FKeepOpen then
        MainForm.Close;
    end
    {$IFDEF Windows}
    else if lItemType = MITwindow then
    begin
      ActivateWindow(SQLMenuItems.FieldByName('cmd').AsString);
      if not FKeepOpen then
        MainForm.Close;
    end
    else if lItemType = MITmenuwindow then
    begin
      closeFindPanel(True); // reset after change menu
      WindowMenu(SQLMenuItems.FieldByName('subMenuId').AsInteger,
        SQLMenuItems.FieldByName('id').AsInteger);
    end
    {$ENDIF}
    else if lItemType = MITmenupath then
    begin
      closeFindPanel(True); // reset after change menu
      PathMenu(SQLMenuItems.FieldByName('subMenuId').AsInteger,
        SQLMenuItems.FieldByName('id').AsInteger,
        SQLMenuItems.FieldByName('subMenuPath').AsString,
        SQLMenuItems.FieldByName('subMenuCmd').AsString);
    end
    else if lItemType = MITmenuprogreload then
    begin
      closeFindPanel(True); // reset after change menu
      lSubMenuId := SQLMenuItems.FieldByName('subMenuId').AsInteger;
      lMenuItemId := SQLMenuItems.FieldByName('id').AsInteger;

      if lSubMenuId = 0 then
      begin
        // create menu
        lSubMenuId := AddMenu(SQLMenuItems.FieldByName('name').AsString,
          SQLMenu.FieldByName('id').AsInteger,
          SQLMenuItems.FieldByName('subMenuCmd').AsString, '',
          SQLMenuItems.FieldByName('subMenuReloadInterval').AsInteger);
        MenuDB.ExecuteDirect('update menuItem set subMenuId = ' +
          IntToStr(lSubMenuId) + ' where id = ' + IntToStr(lMenuItemId));
        LoadMenuFromProcess(SQLMenu.FieldByName('cmd').AsString);
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
          MenuDB.ExecuteDirect('delete from menuItem where menuId = ' +
            IntToStr(lSubMenuId));
          LoadMenuFromProcess(SQLMenu.FieldByName('cmd').AsString);
        end;

      end;
      setActiveMenu(lSubMenuId); // reload after build
    end
    else if lItemType = MITmenu then
    begin
      closeFindPanel(True); // reset after change menu
      lResult := setActiveMenu(SQLMenuItems.FieldByName('subMenuId').AsInteger);
    end;
  finally
    Screen.Cursor := crDefault;
  end;

  closeFindPanel;
end;

procedure TMainForm.edFindKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (Key = VK_DOWN) then
  begin
    if SQLMenuItems.EOF then
      SQLMenuItems.First
    else
      SQLMenuItems.Next;

    //SQLMenuItems.First;
    //MainGrid.SetFocus;
    key := 0;
  end
  else if (Key = VK_UP) then
  begin
    if SQLMenuItems.BOF then
      SQLMenuItems.Last
    else
      SQLMenuItems.Prior;

    //SQLMenuItems.Last;
    //MainGrid.SetFocus;
    key := 0;
  end
  else if (Key = VK_Return) then
  begin
    //MainGrid.SetFocus;
    Key := 0;
    FKeyStop := True;
    acRun.Execute;
  end;
end;

procedure TMainForm.edFindKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
var
  lUpMenuId: integer;
begin
  if (Key = VK_ESCAPE) then
  begin
    lUpMenuId := SQLMenu.FieldByName('upMenuId').AsInteger;
    if (lUpMenuId = 0) or FExecIfOne then
      MainForm.Close
    else
    begin
      closeFindPanel(True);
      NavigateUp;
    end;
  end
  else if not ((Key = VK_DOWN) or (Key = VK_UP) or (Key = VK_Return)) and
    (FLastFind <> edFind.Text) then
  begin
    if isExternalSearch then
    begin
      // restart timer
      ThrTimer.Enabled := False;
      ThrTimer.Enabled := True;
    end
    else
    begin
      showMenu;
      FLastFind := edFind.Text;
    end;
  end;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  if FExecIfOne and (SQLMenuItems.RecordCount = 1) then
  begin
    // on windows - if form cant get focus, run app without focus too...
    MainForm.AlphaBlend := True;
    MainForm.AlphaBlendValue := 10;
    //MainForm.Close;
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
begin
  showMenu;
  FLastFind := edFind.Text;
  ThrTimer.Enabled := False;
end;

procedure TMainForm.LoadMenuFromLines(const aLines: TStringList);
var
  lLine: string;
  i: integer;
  lMenuItemParser: TMenuItemParser;
begin
  // insert into menu
  for i := 0 to aLines.Count - 1 do
  begin
    lLine := Trim(aLines[i]);
    lLine := DelSpace1(lLine);
    if (lLine <> '') and (not AnsiStartsStr('#', lLine) or
      AnsiStartsStr('#!', lLine)) then
    begin
      if AnsiStartsStr('#!', lLine) then
        Delete(lLine, 1, 2);

      lMenuItemParser := TMenuItemParser.Create(lLine);
      try
        if not (lMenuItemParser.itemType in [MITEndMenu, MITNone]) then
          AddMenuItem(lMenuItemParser);
      finally
        FreeAndNil(lMenuItemParser);
      end;
    end;
  end;
end;

procedure TMainForm.LoadMenuFromProcess(const aCmd: string);
var
  lSl: TStringListUTF8;
  j: integer;
  //F:TextFile;
  F: TextFile;
  lLine: WideString;
  lVal, lEncCon, lEncDef: string;
  lExt, lCan: boolean;
  c: widechar;
begin
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
    try
      //Process1.WaitOnExit;
      //lSl.LoadFromStream(ProcessUTF81.Output);
      //lsl.SaveToFile('c:\tmp\menuDebug.txt');

      AssignStream(F, ProcessUTF81.Output);
      Reset(F);
      while not EOF(F) do
      begin
        Readln(F, lLine);

        lVal := RepairCzechCharacters(lLine);

        lSl.Append(lVal);
        //ShowMessage(lSl[0]);
      end;
      CloseFile(F);

      LoadMenuFromLines(lSl);
      //lsl.SaveToFile('c:\tmp\menuDebug.txt');

    finally
      FreeAndNil(lSl);
    end;

  end;

  SQLMenu.Edit;
  SQLMenu.FieldByName('Load').AsInteger := DateTimeToTimeStamp(time).Time div 1000;
  SQLMenu.Post;
  SQLMenu.ApplyUpdates;
end;

procedure TMainForm.showMenu(const aOrderBy: string; const aName: string);
var
  lSql: string;
  lId, lCmd: string;
  lSearchText, i: string;
  lGlobalSearch: boolean;
  lRecNo: longint;
begin
  MainGrid.BeginUpdate;
  MainGridShortCut.BeginUpdate;
  MainGridSubmenu.BeginUpdate;
  try
    // initialize local variables
    lGlobalSearch := False;
    lSearchText := '';

    // check global search
    if Length(edFind.Text) > 0 then
    begin
      if edFind.Text[1] = '*' then
      begin
        lGlobalSearch := True;
        lSearchText := copy(edFind.Text, 2, integer.MaxValue);
      end
      else
      begin
        lGlobalSearch := False;
        lSearchText := edFind.Text;
      end;
    end;

    // prepare search
    //if (lSearchText <> '') then
    //begin
    //  {$IFDEF Windows}
    //  lSearchText := lSearchText.Replace('%CurDestop%', GetCurrentDesktopName());
    //  {$ENDIF}
    //end;


    // regenerate if reloadInterval < 0 and lSearchText and command contains %s
    if isExternalSearch and (lSearchText <> FLastFind) then
    begin
      MenuDB.ExecuteDirect('delete from menuItem where menuId = ' +
        SQLMenu.FieldByName('id').AsString);
      lCmd := ReplaceText(SQLMenu.FieldByName('cmd').AsString, '%s',
        '"' + lSearchText + '"');
      LoadMenuFromProcess(lCmd);

      SQLMenu.Edit;
      SQLMenu.FieldByName('Load').AsInteger := -1;
      SQLMenu.CheckBrowseMode;
      SQLMenu.ApplyUpdates;
    end;

    // open Main menu
    MenuItemDS.DataSet := nil;
    SQLMenuItems.Close;
    lId := SQLMenu.FieldByName('id').AsString;
    lSql := 'select id, menuId, itemType, ' + ' name ' +
      ' , search, shortcut, ' +
      ' cmd, subMenuPath, subMenuCmd, subMenuReloadInterval, subMenuId, subMenuChar, width '
      + ' from menuItem where itemType <> ''MITwinkey''  ';
    { #todo : MITwinignore - doplnit }

    if aName <> '' then
      lSql := lSql + ' and name = ''' + Trim(aName) + ''' '
    else if not lGlobalSearch then
    begin
      lSql := lSql + ' and menuId = ''' + lId + ''' ';
    end
    else
    begin
      lSql := lSql + ' and itemType <> ''MITseparator'' ';
      lSql := lSql + ' and itemType <> ''MITmenu'' ';
    end;


    if (lSearchText <> '') and not isExternalSearch and (aName = '') then
    begin
      lSql := lSql + ' and ((search like ''%' + lSearchText +
        '%'') or (name like ''%' + lSearchText + '%''))';
      {TODO -oLebeda -cfeat: split lSearchText by space and simulate fuzzy search}
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
    end;

    MenuItemDS.DataSet := SQLMenuItems;
  finally
    MainGrid.EndUpdate(True);
    MainGridShortCut.EndUpdate(True);
    MainGridSubmenu.EndUpdate(True);
  end;

  if (((FSearchCount > 0) and (FSearchCount <= SQLMenuItems.RecordCount)) or
    isExternalSearch) and (not pnlFind.Visible) then
  begin
    // find panel visibility
    pnlFind.Visible := True;
    ActiveControl := edFind;
    if MainForm.CanFocus then
      edFind.SetFocus;
  end
  else
  if pnlFind.Visible then
    ActiveControl := edFind;

  // form size
  SetFormSize;
end;

procedure TMainForm.SetFormSize;
var
  lHeight, lWidth: integer;
const
  lHeighReserve = 2;
begin
  MainForm.Caption := SQLMenu.FieldByName('name').AsString;

  // height
  lHeight := MainGrid.DefaultRowHeight * SQLMenuItems.RecordCount +
    (2 * MainForm.BorderWidth) + lHeighReserve;

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
  lWidth := GetMaxWidth + MainGridSubmenu.Width + MainGridShortCut.Width +
    (2 * MainForm.BorderWidth);
  MainForm.Width := lWidth;

  // centralization
  if FFormMode = FMCentral then
  begin
    Top := (Screen.Height div 2) - (MainForm.Height div 2);
    Left := (Screen.Width div 2) - (MainForm.Width div 2);
    ;
  end;

  // mouse to menu if isn't
  if Mouse.CursorPos.Y < top then
    Mouse.CursorPos := Point(Mouse.CursorPos.X, top + 10);

  if Mouse.CursorPos.Y > (top + MainForm.Height) then
    Mouse.CursorPos := Point(Mouse.CursorPos.X, (top + MainForm.Height) - 10);
end;

procedure TMainForm.NavigateUp;
var
  lMenuId: longint;
begin
  closeFindPanel;
  lMenuId := SQLMenu.FieldByName('id').AsInteger;
  setActiveMenu(SQLMenu.FieldByName('upMenuId').AsInteger);
  SQLMenuItems.Locate('subMenuId', lMenuId, []);
end;

procedure TMainForm.WindowMenu(const aSubMenuId, aMenuItemId: integer;
  const aRoot: boolean = False);
begin
  { TODO -cWM : zlikvidovat duplicitní konstrukce}
  AplyGeneratedMenu(aRoot, aMenuItemId, aSubMenuId);
end;

procedure TMainForm.PathMenu(const aSubMenuId, aMenuItemId: integer;
  const aPath, aCmd: string; const aRoot: boolean);
var
  lResult: boolean;
  Attributes: integer;
  lSubMenuId, lMenuItemId: integer;

  procedure generatePathMenu;
  const
    FM_FLAG = '#filemask:';
  var
    lMenuItemParser: TMenuItemParser;
    lDirName: string;
    lBaseName: string;
    lFullName: string;
    lCmd, lFileMask, lFlagName, lWord, lModifiedStr, lLocalDirName: string;
    i, CounterVar: integer;
    MenuList: TStringList;
    lListOfFolders: TStringList;
    lListOfFiles: TStringList;
    lRecursive, lFindDir, lFindFile, lByDateTime, lReverse: boolean;
    lFlagPos, lFlagValPos, lFlagEnd: SizeInt;
    lModified: longint;
    lModifiedDateTime: TDateTime;
    lNameFields: SysUtils.TStringArray;
    lSortedListOfFiles: TStrings;
  begin
    lCmd := aCmd;

    if ContainsText(lCmd, FM_FLAG) then
    begin
      { #todo : separate method extractFlagVal }
      lFlagName := FM_FLAG;
      lFlagPos := Pos(lFlagName, lCmd);
      lFlagValPos := lFlagPos + Length(lFlagName);
      lFlagEnd := Pos(' ', lCmd, lFlagValPos);
      lFileMask := ExtractSubstr(lCmd, lFlagValPos, [' ']);
      lCmd := ReplaceStr(lCmd, lFlagName + lFileMask, '');
    end
    else
      lFileMask := '*';

    // get flag
    lRecursive := ContainsText(lCmd, '#recursive');
    lFindDir := not ContainsText(lCmd, '#nodir');
    lFindFile := not ContainsText(lCmd, '#nofile');
    lByDateTime := ContainsText(lCmd, '#datetime');
    lReverse := ContainsText(lCmd, '#reverse');

    // remove flag from command
    lCmd := ReplaceStr(lCmd, '#recursive', '');
    lCmd := ReplaceStr(lCmd, '#nodir', '');
    lCmd := ReplaceStr(lCmd, '#nofile', '');
    lCmd := ReplaceStr(lCmd, '#datetime', '');
    lcmd := ReplaceStr(lCmd, '#reverse', '');

    { #todo : more paths at once }
    { #todo : #localmenufile:menu.txt #menufile:globalmenu.txt }
    { #todo : filenamereplace .git include gitmenu.txt }
    { #todo : filenamecontains .idea prog .... }

    MenuList := TStringList.Create;
    try
      MenuList.Add('separator "Path: ' + aPath + '"');

      { #todo : add as submenu to all directories by #menufile or #localmenufile}
      Menulist.Add('prog "Double Commander" doublecmd.exe "' + aPath + '"');
      Menulist.Add('prog "Explorer" explorer.exe "' + aPath + '" #max');
      Menulist.Add('prog "PowerShell" wt.exe -d "' + aPath + '"');

      { #todo : separate method }
      // directories
      if lFindDir then
      begin
        Menulist.Add('separator Directories');
        lListOfFolders := TStringList.Create;
        try
          FileUtil.FindAllDirectories(lListOfFolders, aPath, lRecursive);
          lListOfFolders.Sort;
          for i := 0 to (lListOfFolders.Count - 1) do
          begin
            lFullName := lListOfFolders[i];
            lBaseName := ExtractFileName(lFullName);
            MenuList.Add('menupath "' + StringReplace(lBaseName, '_',
              '__', [rfIgnoreCase]) + '" "' + lFullName + '" "' + lCmd + '"');
          end;
        finally
          lListOfFolders.Free;
        end;

      end;

      { #todo : separate method }
      // files
      if lFindFile then
      begin
        Menulist.Add('separator Files');
        lListOfFiles := TStringList.Create;
        try
          FileUtil.FindAllFiles(lListOfFiles, aPath, lFileMask, lRecursive);

          // add extra attributes
          if lByDateTime then
          begin
            for i := 0 to (lListOfFiles.Count - 1) do
            begin
              lFullName := lListOfFiles[i];
              lModified := FileAge(lFullName);
              lModifiedDateTime := FileDateTodateTime(lModified);
              DateTimeToString(lModifiedStr, 'yyyy-mm-dd hh:nn', lModifiedDateTime);
              lListOfFiles[i] := '[' + lModifiedStr + ']*' + lFullName;
            end;
          end;

          // sort
          lListOfFiles.Sort;
          if lReverse then
            lSortedListOfFiles := lListOfFiles.Reverse
          else
            lSortedListOfFiles := lListOfFiles;

          try
            // build menu items
            for i := 0 to (lSortedListOfFiles.Count - 1) do
            begin
              if lSortedListOfFiles[i].Contains('*') then
              begin
                lNameFields := lSortedListOfFiles[i].Split('*');
                lModifiedStr := lNameFields[0];
                lFullName := lNameFields[1];
              end
              else
                lFullName := lSortedListOfFiles[i];

              lDirName := ExtractFileDir(lFullName);
              lLocalDirName := lDirName.Replace(AppendPathDelim(aPath), '');

              lBaseName := ExtractFileName(lFullName);
              if lRecursive then
                lBaseName := lLocalDirName + PathDelim + lBaseName;
              if lByDateTime then
                lBaseName := lModifiedStr + ' ' + lBaseName;



              MenuList.Add('prog "' + StringReplace(lBaseName, '_', '__', [rfIgnoreCase]) +
                '" "' + lCmd + '" "' + lFullName + '" "#dir:' + lDirName + '"');
              { #todo : special items }
            end;
          finally
            lSortedListOfFiles.Free;
          end;

        finally
          lListOfFiles.Free;
        end;
      end;

      // menu
      //ShowMessage(MenuList.Text);
      for i := 0 to (MenuList.Count - 1) do
      begin
        lMenuItemParser := TMenuItemParser.Create(MenuList[i]);
        try
          MainForm.AddMenuItem(lMenuItemParser);
        finally
          FreeAndNil(lMenuItemParser);
        end;
      end;

    finally
      MenuList.Free;
    end;
  end;

begin
  lSubMenuId := aSubMenuId;
  lMenuItemId := aMenuItemId;

  MainGrid.BeginUpdate;
  MainGridShortCut.BeginUpdate;
  MainGridSubmenu.BeginUpdate;
  try
    if lSubMenuId = 0 then // if 0 create new menuitem
    begin
      // create menu
      lSubMenuId := AddMenu(SQLMenuItems.FieldByName('name').AsString,
        SQLMenu.FieldByName('id').AsInteger,
        SQLMenuItems.FieldByName('subMenuCmd').AsString, '',
        SQLMenuItems.FieldByName('subMenuReloadInterval').AsInteger);
      MenuDB.ExecuteDirect('update menuItem set subMenuId = ' +
        IntToStr(lSubMenuId) + ' where id = ' + IntToStr(lMenuItemId));
      generatePathMenu;
    end
    else
    begin
      MenuDB.ExecuteDirect('delete from menuItem where menuId = ' +
        IntToStr(lMenuItemId));
      lResult := setActiveMenu(lMenuItemId);

      generatePathMenu;

    end;

    lResult := setActiveMenu(lMenuItemId); // reload after build

  finally
    MainGrid.EndUpdate(True);
    MainGridShortCut.EndUpdate(True);
    MainGridSubmenu.EndUpdate(True);
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

function TMainForm.setActiveMenu(const aIdMenu: longint;
  const aOrderBy: string = 'id'): boolean;
begin
  // sure the changes are saved
  SQLMenu.CheckBrowseMode;
  SQLMenu.ApplyUpdates;

  Result := SQLMenu.Locate('id', aIdMenu, []);
  showMenu(aOrderBy);
end;

procedure TMainForm.AddMenuItem(var lMenuItemParser: TMenuItemParser);
begin
  SQLMenuItems.Append;
  //id, menuId, itemType, name, search, shortcut, cmd, subMenuPath, subMenuCmd, subMenuReloadInterval, subMenuId, subMenuChar
  SQLMenuItems.FieldByName('menuId').AsInteger := lMenuItemParser.menuId;
  SQLMenuItems.FieldByName('itemType').AsString := MitToStr(lMenuItemParser.itemType);
  SQLMenuItems.FieldByName('name').AsWideString := lMenuItemParser.Name;
  { TODO -cfeat : nahradit _ mezerami ve jméně }
  SQLMenuItems.FieldByName('search').AsString := lMenuItemParser.search;
  SQLMenuItems.FieldByName('shortcut').AsString := lMenuItemParser.shortcut;
  SQLMenuItems.FieldByName('cmd').AsString := lMenuItemParser.cmd;
  SQLMenuItems.FieldByName('subMenuPath').AsString := lMenuItemParser.subMenuPath;
  SQLMenuItems.FieldByName('subMenuCmd').AsString := lMenuItemParser.subMenuCmd;
  SQLMenuItems.FieldByName('subMenuReloadInterval').AsInteger :=
    lMenuItemParser.subMenuReloadInterval;
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
