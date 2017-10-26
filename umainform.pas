unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, sqlite3conn, sqldb, Forms, Controls, Graphics, DBGrids, ActnList, ExtCtrls, StdCtrls, Grids,
  AsyncProcess, uMenuItem, process;

type

  TFormMode = (FMNormal, FMCentral);

  { TMainForm }

  TMainForm = class(TForm)
    acList: TActionList;
    acDebug: TAction;
    acRun: TAction;
    acFind: TAction;
    acKeepOpen: TAction;
    AsyncProcess1: TAsyncProcess;
    edFind: TEdit;
    MainGrid: TDBGrid;
    MainGridShortCut: TDBGrid;
    MainGridIcon: TDBGrid;
    MenuDS: TDataSource;
    MenuDB: TSQLite3Connection;
    MenuItemDS: TDataSource;
    pnlFind: TPanel;
    Process1: TProcess;
    SQLMenu: TSQLQuery;
    SQLMenuItems: TSQLQuery;
    SQLMenuItemsMaxWidth: TSQLQuery;
    SQLMenuItemsShortcut: TSQLQuery;
    SQLTransaction: TSQLTransaction;
    procedure acDebugExecute(Sender: TObject);
    Procedure acFindExecute(Sender: TObject);
    Procedure acKeepOpenExecute(Sender: TObject);
    procedure acRunExecute(Sender: TObject);
    Procedure edFindKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure edFindKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure MainGridCellClick(Column: TColumn);
    Procedure MainGridDrawColumnCell(Sender: TObject; Const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure MainGridIconDrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: integer; Column: TColumn; State: TGridDrawState);
    Procedure MainGridKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure MainGridKeyPress(Sender: TObject; Var Key: char);
    Procedure MainGridKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure MainGridShortCutDrawColumnCell(Sender: TObject; Const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure SQLMenuAfterInsert(DataSet: TDataSet);
    procedure SQLMenuAfterScroll(DataSet: TDataSet);
  private
    FFormMode: TFormMode;
    FRecNo: LongInt;
    FKeepOpen: Boolean;
    FKeyStop: Boolean;
    FSearchCount: LongInt;
    FIconPathHolder: TStringList;
    FLastResNo: Integer; // for navigation over separators
    Procedure AppDeactivate(Sender: TObject);
    Procedure closeFindPanel(Const aForce: Boolean = false);
    Procedure FindSwitch;
    Function GetMaxWidth: Integer;
    Function GetTextWidth(Const aText: String): Integer;
    Procedure LoadMenuFromLines(Const aLines: TStringList);
    Procedure LoadMenuFromProcess(Const aCmd: String);
    Procedure RunAsync(Const aCmd: string);
    Procedure SetSearchCount(Const aValue: LongInt);
    Procedure SetSeparatorRow(Const State: TGridDrawState; Const Column: TColumn; Const DataCol: Integer; Const Rect: TRect;
      Const aGrid: TDBGrid);
    Procedure showMenu;
    procedure SetFormSize;
    procedure NavigateUp;
    Function GetFullIconPath(aName: String): String;
    { private declarations }
  public
    function AddMenu(aName: string; aUpMenuId: longint; aCmd: string = ''; aPath: string = ''; aReloadInterval: integer = 0): integer;
    Function setActiveMenu(const aIdMenu: longint): Boolean;
    procedure AddMenuItem(var lMenuItemParser: TMenuItemParser);
    procedure LoadMenuFromFile(const aFile: string);

    Property FormMode: TFormMode Read FFormMode Write FFormMode;
    Property SearchCount: LongInt Read FSearchCount Write SetSearchCount;
  end;

var
  MainForm: TMainForm;

implementation

uses strutils, debugForm, StreamIO, LCLType, Dialogs;

{$R *.lfm}

{ TMainForm }

Procedure TMainForm.FormCreate(Sender: TObject);
begin
  MainForm.Constraints.MaxHeight := round(Screen.Height * 0.9);
  MainForm.Constraints.MaxWidth := round(Screen.Width * 0.9);

  FFormMode := FMNormal;
  // color

  // sure create DB
  MenuDB.Close;
  //DeleteFile('/tmp/debugMenu.db'); // uncoment only for developnet (real DB for object inspector and design in lazarus)
  //MenuDB.DatabaseName := '/tmp/debugMenu.db'; // uncoment only for developnet (real DB for object inspector and design in lazarus)
  MenuDB.DatabaseName := ':memory:';
  MenuDB.Open;
  MenuDB.ExecuteDirect('CREATE TABLE IF NOT EXISTS menu (id INTEGER PRIMARY KEY , upMenuId INTEGER, name NOT NULL, cmd, path, load INTEGER, reloadInterval INTEGER)');
  MenuDB.ExecuteDirect('CREATE TABLE IF NOT EXISTS menuItem (id INTEGER PRIMARY KEY , menuId INTEGER NOT NULL, itemType, name, search, icon, shortcut, cmd, subMenuPath, subMenuCmd, subMenuReloadInterval INTEGER, subMenuId INTEGER, subMenuChar, width INTEGER DEFAULT 100, FOREIGN KEY(menuId) REFERENCES menu(id))');
  MenuDB.Transaction.Commit;

  SQLMenu.Active := True;
  SQLMenuItems.Active := True;

  // fill root menu
  AddMenu('ROOT', 0);
  SQLMenu.First;

  // commandline parameters
  if Application.HasOption('c', 'center') then
  begin
    MainForm.Width := 500; {TODO -oLebeda -cNone: umožnit zvolit}
    MainForm.Height := 563;
    MainForm.Position := poScreenCenter;
    MainForm.FormMode := FMCentral;
  end;

  if Application.HasOption('s', 'search') then
    MainForm.SearchCount := StrToInt(Application.GetOptionValue('s', 'search'))
  else
    MainForm.SearchCount := MaxInt;

  if Application.HasOption('f', 'file') then
  begin
    SQLMenu.Edit;
    SQLMenu.FieldByName('path').AsString := Application.GetOptionValue('f', 'file');
    SQLMenu.CheckBrowseMode;
    LoadMenuFromFile(SQLMenu.FieldByName('path').AsString);
  end;

  if Application.HasOption('p', 'process') then
  begin
    SQLMenu.Edit;
    SQLMenu.FieldByName('cmd').AsString := Application.GetOptionValue('p', 'process');
    SQLMenu.CheckBrowseMode;
    LoadMenuFromProcess(SQLMenu.FieldByName('cmd').AsString);
  end;

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
end;

Procedure TMainForm.FormDestroy(Sender: TObject);
Begin
  if Assigned(FIconPathHolder) then
    FreeAndNil(FIconPathHolder);
end;

Procedure TMainForm.AppDeactivate(Sender: TObject);
begin
  if not FKeepOpen then
    MainForm.Close;
end;

Procedure TMainForm.closeFindPanel(Const aForce: Boolean);
Begin
  if (edFind.Text <> '') or aForce then
  begin
    edFind.Text := '';
    showMenu;
    MainGrid.SetFocus;
    pnlFind.Visible := false;
  End;
End;

Procedure TMainForm.FindSwitch;
Begin
  if MainGrid.Focused then
    pnlFind.Visible := True
  else If Not pnlFind.Visible Then
    pnlFind.Visible := True
  Else If pnlFind.Visible And (edFind.Text = '') Then
    pnlFind.Visible := False;

  If pnlFind.Visible And Not edFind.Focused Then
  Begin
    edFind.SetFocus;
  End
  Else
  Begin
    MainGrid.SetFocus;
  End;

  SetFormSize;
End;

Function TMainForm.GetMaxWidth: Integer;
Begin
  if SQLMenuItemsMaxWidth.Active and (SQLMenuItemsMaxWidth.RecordCount = 1) then
    Result := SQLMenuItemsMaxWidth.FieldByName('width').AsInteger + 10
  else
    Result := 500;
End;

Function TMainForm.GetTextWidth(const aText: String): Integer;
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

  Result := W + 8;
  //Result := Length(aText) * 30
End;

Procedure TMainForm.MainGridCellClick(Column: TColumn);
Begin
  acRun.Execute;
end;

Procedure TMainForm.MainGridDrawColumnCell(Sender: TObject; Const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
Begin
  SetSeparatorRow(State, Column, DataCol, Rect, MainGrid);
end;

Procedure TMainForm.MainGridShortCutDrawColumnCell(Sender: TObject; Const Rect: TRect; DataCol: Integer; Column: TColumn;
  State: TGridDrawState);
Begin
  SetSeparatorRow(State, Column, DataCol, Rect, MainGridShortCut);
end;

Procedure TMainForm.MainGridIconDrawColumnCell(Sender: TObject; Const Rect: TRect; DataCol: integer; Column: TColumn; State: TGridDrawState);
var
  bmpImage: TPicture;
  lIconName: string;
begin
  if Column.FieldName = 'icon' then
    with MainGridIcon.Canvas do
    begin
      fillRect(rect);
      bmpImage := TPicture.Create;
      try
        lIconName := MainGridIcon.DataSource.DataSet.FieldByName('icon').AsString;
        lIconName := GetFullIconPath(lIconName);

        if FileExists(lIconName) then
          bmpImage.LoadFromFile(lIconName)
        else
          bmpImage.Clear;


        StretchDraw(Rect, bmpImage.Bitmap);
        //Column.Width := MainGridIcon.DefaultRowHeight;
      finally
        bmpimage.Free;
      end;
    end;

  //SetSeparatorRow(State, Column, DataCol, Rect, MainGridIcon);
end;

Function TMainForm.GetFullIconPath(aName: String): String;
Var
  lDir: string;
  i: Integer;
Begin
  if FileExists(aName) then
    Result := aName
  else
  begin
    if not Assigned(FIconPathHolder) then
      FIconPathHolder := TStringList.Create;

    if FileExists(GetEnvironmentVariable('HOME') + '/.simpleMenuIcons') then
      FIconPathHolder.LoadFromFile(GetEnvironmentVariable('HOME') + '/.simpleMenuIcons');

    for i := 0 to FIconPathHolder.Count - 1 do
    begin
      lDir := IncludeTrailingPathDelimiter(FIconPathHolder[i]);
      if FileExists(lDir + aName + '.png') then
      begin
        Result := lDir + aName + '.png';
        Break;
      End
      else if FileExists(lDir + aName + '.xpm') then
      begin
        Result := lDir + aName + '.xpm';
        Break;
      End
      else if FileExists(lDir + aName + '_16x16.xpm') then
      begin
        Result := lDir + aName + '_16x16.xpm';
        Break;
      End;
    End;
  end
End;

Procedure TMainForm.MainGridKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
Begin
  // navigation over separators
  FLastResNo := SQLMenuItems.RecNo;

  if (Key = VK_Return) then
  begin
    FRecNo := SQLMenuItems.RecNo;
    SQLMenuItems.Prior; // ugly hack - enter goes to next
  End;
end;

Procedure TMainForm.MainGridKeyPress(Sender: TObject; Var Key: char);
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

Procedure TMainForm.MainGridKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
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

  if (Key = VK_Return) or ((Key = VK_RIGHT) and (lItemType in [MITmenu, MITmenuprog, MITmenufile, MITmenuprogreload])) then
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

  if strToMit(SQLMenuItems.FieldByName('itemType').AsString) = MITseparator then
    if SQLMenuItems.RecNo > FLastResNo then
      SQLMenuItems.Next
    else
      SQLMenuItems.Prior;
end;

Procedure TMainForm.acDebugExecute(Sender: TObject);
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

Procedure TMainForm.acFindExecute(Sender: TObject);
Begin
  FindSwitch;
end;

Procedure TMainForm.acKeepOpenExecute(Sender: TObject);
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

Procedure TMainForm.RunAsync(Const aCmd{, aParams, aDir, aPath, aName}: string);
var
  sl, slCmd: TStringList;
  lParams, lCmd, lPath, l: string;
  lExe: RawByteString;
Const
  BEGIN_SL = 0;
begin
  // replace macros
  //if aDir <> '' then
  //  lParams := StringReplace(aParams, '%d', '"' + aDir + '"', [rfReplaceAll]);
  //
  //if aPath <> '' then
  //  lParams := StringReplace(lParams, '%p', '"' + aPath + '"', [rfReplaceAll]);
  //
  //if aName <> '' then
  //  lParams := StringReplace(lParams, '%f', '"' + aName + '"', [rfReplaceAll]);

  sl := TStringList.Create;
  try
    slCmd := tStringList.Create;
    slCmd.Delimiter := ' ';
    slCmd.DelimitedText := aCmd;
    lCmd := slCmd[BEGIN_SL];
    slCmd.Delete(BEGIN_SL);

    //sl.StrictDelimiter := true;
    sl.Delimiter := ' ';
    sl.DelimitedText := lParams;
    slCmd.AddStrings(sl);

    //for l in slCmd do
    //  ShowMessage(l);

    // execute process
    //if aDir <> '' then
    //  runAsyncProcess.CurrentDirectory := aDir;

    // expand path
    if FileExists(lCmd) then
      lExe := lCmd
    else
    begin
      lPath := GetEnvironmentVariable('PATH');
      lExe := ExeSearch(lCmd, lPath);
    End;

    AsyncProcess1.Executable := lExe;
    AsyncProcess1.Parameters.Clear;
    AsyncProcess1.Parameters.AddStrings(slCmd);
    AsyncProcess1.Execute;

  finally
    FreeAndNil(slCmd);
    sl.Free;
  end;
end;

Procedure TMainForm.SetSearchCount(Const aValue: LongInt);
Begin
  If FSearchCount = aValue Then Exit;
  FSearchCount := aValue;
End;

Procedure TMainForm.SetSeparatorRow(Const State: TGridDrawState; Const Column: TColumn; Const DataCol: Integer; Const Rect: TRect; const aGrid: TDBGrid);
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

Procedure TMainForm.acRunExecute(Sender: TObject);
Var
  lItemType: TMenuItemType;
  lSubMenuId, lMenuItemId, lLoad, lReload, lTime, lInterval: LongInt;
begin
  lItemType := strToMit(SQLMenuItems.FieldByName('itemType').AsString);

  if lItemType =  MITprog then
  begin
    RunAsync(SQLMenuItems.FieldByName('cmd').AsString);
    // AsyncProcess1.CommandLine := SQLMenuItems.FieldByName('cmd').AsString;
    //AsyncProcess1.Execute;
    if not FKeepOpen then
      MainForm.Close;
  End
  else if lItemType =  MITmenuprogreload then
  begin
    lSubMenuId := SQLMenuItems.FieldByName('subMenuId').AsInteger;
    lMenuItemId := SQLMenuItems.FieldByName('id').AsInteger;

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
    setActiveMenu(SQLMenuItems.FieldByName('subMenuId').AsInteger);
  End;

  closeFindPanel;
end;

Procedure TMainForm.edFindKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
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
    acRun.Execute;
    Key := 0;
  End;
  if (Key = VK_Return) then
    FKeyStop := True;
end;

Procedure TMainForm.edFindKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
Begin
  if (Key = VK_ESCAPE) then
  begin
    if SQLMenu.FieldByName('upMenuId').AsInteger = 0 then
      MainForm.Close;

    closeFindPanel(true);
    NavigateUp;
  End
  else if ((Key = VK_DELETE) or (Key = VK_BACK)) and (edFind.Text = '')  then
    acFind.Execute
  else if not((Key = VK_DOWN) or (Key = VK_UP) or (Key = VK_Return)) then
    showMenu;
end;

Procedure TMainForm.FormActivate(Sender: TObject);
Begin
  {TODO -oLebeda -cNone: reload menu ??}
end;

Procedure TMainForm.SQLMenuAfterInsert(DataSet: TDataSet);
begin
  SQLMenu.FieldByName('Load').AsInteger := DateTimeToTimeStamp(time).Time div 1000;
end;

Procedure TMainForm.SQLMenuAfterScroll(DataSet: TDataSet);
begin
 if not SQLMenu.Modified then
    showMenu;
end;

Procedure TMainForm.LoadMenuFromLines(Const aLines: TStringList);
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

Procedure TMainForm.LoadMenuFromProcess(Const aCmd: String);
Var
  lSl: TStringList;
  i: Integer;
  F:Text;
  lLine: String;
Begin
  // load data from process
  //Process1.Options := [poWaitOnExit, poNoConsole, poUsePipes];
  Process1.CurrentDirectory := GetEnvironmentVariable('HOME');
  Process1.CommandLine := aCmd;
  Process1.Execute;

  lSl := TStringList.Create;
  Try
    //Process1.WaitOnExit;
    //lSl.LoadFromStream(Process1.Output);
    AssignStream(F, Process1.Output);
    Reset(F);
    while not Eof(F) do
    begin
      Readln(F, lLine);
      //ShowMessage('příliš žluťoučký kůň');
      //ShowMessage(AnsiToUTF8(lLine));
      lSl.Append(AnsiToUTF8(lLine));
    End;
    CloseFile(F);

    LoadMenuFromLines(lSl);

    SQLMenu.Edit;
    SQLMenu.FieldByName('Load').AsInteger := DateTimeToTimeStamp(time).Time div 1000;
    SQLMenu.Post;
    SQLMenu.ApplyUpdates;
  Finally
    FreeAndNil(lSl);
  End;
End;

Procedure TMainForm.showMenu;
Var
  lSql: String;
  lId: String;
  lMenuCount: LongInt;
  s, lShCut: Char;
Begin
  // open Main menu
  MenuItemDS.DataSet := nil;
  SQLMenuItems.Close;
  lId := SQLMenu.FieldByName('id').AsString;
  lSql := 'select id, menuId, itemType, name, search, icon, shortcut, '
                     + ' cmd, subMenuPath, subMenuCmd, subMenuReloadInterval, subMenuId, subMenuChar, width '
                     + ' from menuItem where menuId = ''' + lId + ''' ';

  If edFind.Text <> '' Then
    lSql := lSql + ' and name like ''%' + edFind.Text + '%'' ';

  lSql := lSql + ' Order by id';

  SQLMenuItems.SQL.Text := lSql;
  SQLMenuItems.Open;

  // open max width query
  SQLMenuItemsMaxWidth.Close;
  SQLMenuItemsMaxWidth.ParamByName('id').AsString := lId;
  SQLMenuItemsMaxWidth.Open;

  // fill implicit shortcut
  SQLMenuItems.First;
  while not SQLMenuItems.EOF do
  begin
    if (SQLMenuItems.FieldByName('shortcut').AsString = '') and (strToMit(SQLMenuItems.FieldByName('itemType').AsString) <> MITseparator) then
    begin
      for s in SQLMenuItems.FieldByName('name').AsString do
      begin
        lShCut := LowerCase(s);

        SQLMenuItemsShortcut.Close;
        SQLMenuItemsShortcut.ParamByName('idMenu').AsString := lId;
        SQLMenuItemsShortcut.ParamByName('shortcut').AsString := lShCut;
        SQLMenuItemsShortcut.Open;
        lMenuCount := SQLMenuItemsShortcut.RecordCount;

        if (lShCut <> '') and (lMenuCount = 0) and (lShCut in ['a'..'z','0'..'9']) then
        begin
          SQLMenuItems.Edit;
          SQLMenuItems.FieldByName('shortcut').AsString := lShCut;
          SQLMenuItems.CheckBrowseMode;
          SQLMenuItems.ApplyUpdates;
          break;
        End;
      end;
    End;
    SQLMenuItems.Next;
  End;
  SQLMenuItems.First;

  MenuItemDS.DataSet := SQLMenuItems;

  // find panel visibility
  if (FSearchCount > 0) and (FSearchCount <= SQLMenuItems.RecordCount) and (Not pnlFind.Visible) then
  begin
    pnlFind.Visible := True;
    ActiveControl := edFind;
    if MainForm.CanFocus then
      edFind.SetFocus;
  End;

  if pnlFind.Visible then
    ActiveControl := edFind;

  // form size
  SetFormSize;
End;

Procedure TMainForm.SetFormSize;
var
  lHeight, lWidth: integer;
begin
  MainForm.Caption := SQLMenu.FieldByName('name').AsString;

  // height
  lHeight := MainGrid.DefaultRowHeight * SQLMenuItems.RecordCount + (2* MainForm.BorderWidth);

  if pnlFind.Visible then
    lHeight := lHeight + pnlFind.Height;

  if MainForm.Constraints.MaxHeight >= lHeight then
  begin
    MainGridShortCut.ScrollBars := ssNone;
    MainForm.Constraints.MinHeight := lHeight;
  end
  else
    MainGridShortCut.ScrollBars := ssAutoVertical;

  MainForm.Height := lHeight;

  // width
  lWidth := GetMaxWidth + MainGridIcon.Width + MainGridShortCut.Width + (2* MainForm.BorderWidth);
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

Procedure TMainForm.NavigateUp;
Var
  lMenuId: LongInt;
Begin
  closeFindPanel;
  lMenuId := SQLMenu.FieldByName('id').AsInteger;
  setActiveMenu(SQLMenu.FieldByName('upMenuId').AsInteger);
  SQLMenuItems.Locate('subMenuId', lMenuId, []);
End;

Function TMainForm.AddMenu(aName: string; aUpMenuId: longint; aCmd: string; aPath: string; aReloadInterval: integer): integer;
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

Function TMainForm.setActiveMenu(Const aIdMenu: longint): Boolean;
Begin
  Result := SQLMenu.Locate('id', aIdMenu, []);
End;

Procedure TMainForm.AddMenuItem(Var lMenuItemParser: TMenuItemParser);
begin
  SQLMenuItems.Insert;
  //id, menuId, itemType, name, search, icon, shortcut, cmd, subMenuPath, subMenuCmd, subMenuReloadInterval, subMenuId, subMenuChar
  SQLMenuItems.FieldByName('menuId').AsInteger := lMenuItemParser.menuId;
  SQLMenuItems.FieldByName('itemType').AsString := MitToStr(lMenuItemParser.itemType);
  SQLMenuItems.FieldByName('name').AsString := lMenuItemParser.Name;
  SQLMenuItems.FieldByName('search').AsString := lMenuItemParser.search;
  SQLMenuItems.FieldByName('icon').AsString := lMenuItemParser.icon;
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

Procedure TMainForm.LoadMenuFromFile(Const aFile: string);
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

end.
