unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, sqlite3conn, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids, ActnList, ExtCtrls, StdCtrls, Grids,
  AsyncProcess, uMenuItem, process;

type

  { TMainForm }

  TMainForm = class(TForm)
    acList: TActionList;
    acDebug: TAction;
    acRun: TAction;
    AsyncProcess1: TAsyncProcess;
    MainGrid: TDBGrid;
    Edit1: TEdit;
    MenuDS: TDataSource;
    MenuDB: TSQLite3Connection;
    MenuItemDS: TDataSource;
    pnlBottom: TPanel;
    Process1: TProcess;
    SQLMenu: TSQLQuery;
    SQLMenuItems: TSQLQuery;
    SQLTransaction: TSQLTransaction;
    procedure acDebugExecute(Sender: TObject);
    procedure acRunExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    Procedure MainGridCellClick(Column: TColumn);
    procedure MainGridDrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: integer; Column: TColumn; State: TGridDrawState);
    procedure MainGridKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    Procedure MainGridKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
    procedure SQLMenuAfterInsert(DataSet: TDataSet);
    procedure SQLMenuAfterScroll(DataSet: TDataSet);
  private
    Procedure LoadMenuFromLines(Const aLines: TStringList);
    Procedure LoadMenuFromProcess(Const aCmd: String);
    procedure setWidthForm;
    procedure NavigateUp;
    { private declarations }
  public
    function AddMenu(aName: string; aUpMenuId: longint; aCmd: string = ''; aPath: string = ''; aReloadInterval: integer = 0): integer;
    Function setActiveMenu(const aIdMenu: longint): Boolean;
    procedure AddMenuItem(var lMenuItemParser: TMenuItemParser);
    procedure LoadMenuFromFile(const aFile: string);
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses dateutils, strutils, debugForm, LCLType;

{$R *.lfm}

{ TMainForm }

Procedure TMainForm.FormCreate(Sender: TObject);
var
  lFile: string;
  lMenuId: integer;
begin
  // color

  // sure create DB
  MenuDB.Close;
  DeleteFile('/tmp/debugMenu.db'); // uncoment only for developnet (real DB for object inspector and design in lazarus)
  MenuDB.DatabaseName := '/tmp/debugMenu.db'; // uncoment only for developnet (real DB for object inspector and design in lazarus)
  //MenuDB.DatabaseName := ':memory:';
  MenuDB.Open;
  MenuDB.ExecuteDirect(
    'CREATE TABLE IF NOT EXISTS menu (id INTEGER PRIMARY KEY , upMenuId INTEGER, name NOT NULL, cmd, path, load INTEGER, reloadInterval INTEGER)');
  MenuDB.ExecuteDirect(
    'CREATE TABLE IF NOT EXISTS menuItem (id INTEGER PRIMARY KEY , menuId INTEGER NOT NULL, itemType, name, search, icon, shortcut, cmd, subMenuPath, subMenuCmd, subMenuReloadInterval INTEGER, subMenuId INTEGER, subMenuChar, FOREIGN KEY(menuId) REFERENCES menu(id))');
  MenuDB.Transaction.Commit;

  SQLMenu.Active := True;
  SQLMenuItems.Active := True;

  // fill root menu
  lMenuId := AddMenu('ROOT', 0);
  SQLMenu.First;



  if Application.HasOption('f', 'file') then
  begin
    lFile := Application.GetOptionValue('f', 'file');
    LoadMenuFromFile(lFile);
  end;

  MenuDB.Transaction.Commit;

  // open grid data
  SQLMenu.Active := True;
  SQLMenu.First;

  MainGrid.DataSource.DataSet.Active := False;
  MainGrid.DataSource.DataSet.Active := True;
  setWidthForm;
end;

Procedure TMainForm.MainGridCellClick(Column: TColumn);
Begin
  acRun.Execute;
end;

Procedure TMainForm.MainGridDrawColumnCell(Sender: TObject; Const Rect: TRect; DataCol: integer; Column: TColumn; State: TGridDrawState);
var
  bmpImage: TPicture;
  intX, intY: integer;
  lIconName: string;
begin
  if Column.FieldName = 'icon' then
    with MainGrid.Canvas do
    begin
      fillRect(rect);
      bmpImage := TPicture.Create;
      try
        lIconName := MainGrid.DataSource.DataSet.FieldByName('icon').AsString;

        // expansion of icons (from IceWM docs)
        if FileExists(lIconName) then
          bmpImage.LoadFromFile(lIconName)
        else if FileExists(GetEnvironmentVariable('HOME') + '/.icewm/icons/' + lIconName + '.png') then
          bmpImage.LoadFromFile(GetEnvironmentVariable('HOME') + '/.icewm/icons/' + lIconName + '.png')
        else if FileExists(GetEnvironmentVariable('HOME') + '/.icewm/icons/' + lIconName + '.xpm') then
          bmpImage.LoadFromFile(GetEnvironmentVariable('HOME') + '/.icewm/icons/' + lIconName + '.xpm')
        else if FileExists('/usr/share/pixmaps/' + lIconName + '.png') then
          bmpImage.LoadFromFile('/usr/share/pixmaps/' + lIconName + '.png')
        else if FileExists('/usr/share/pixmaps/' + lIconName + '.xpm') then
          bmpImage.LoadFromFile('/usr/share/pixmaps/' + lIconName + '.xpm')
        else if FileExists('/usr/share/icewm/icons/' + lIconName + '_16x16.xpm') then
          bmpImage.LoadFromFile('/usr/share/icewm/icons/' + lIconName + '_16x16.xpm')

        else if FileExists('/usr/share/icons/hicolor/16x16/apps/' + lIconName + '.png') then
          bmpImage.LoadFromFile('/usr/share/icons/hicolor/16x16/apps/' + lIconName + '.png')
        else if FileExists('/usr/share/icons/hicolor/32x32/apps/' + lIconName + '.png') then
          bmpImage.LoadFromFile('/usr/share/icons/hicolor/32x32/apps/' + lIconName + '.png')
        else if FileExists('/usr/share/icons/hicolor/48x48/apps/' + lIconName + '.png') then
          bmpImage.LoadFromFile('/usr/share/icons/hicolor/48x48/apps/' + lIconName + '.png')

        else if FileExists('/usr/share/icons/locolor/16x16/apps/' + lIconName + '.png') then
          bmpImage.LoadFromFile('/usr/share/icons/locolor/16x16/apps/' + lIconName + '.png')
        else if FileExists('/usr/share/icons/locolor/32x32/apps/' + lIconName + '.png') then
          bmpImage.LoadFromFile('/usr/share/icons/locolor/32x32/apps/' + lIconName + '.png')
        else if FileExists('/usr/share/icons/locolor/48x48/apps/' + lIconName + '.png') then
          bmpImage.LoadFromFile('/usr/share/icons/locolor/48x48/apps/' + lIconName + '.png')

        else
          bmpImage.Clear;


        StretchDraw(Rect, bmpImage.Bitmap);
        Column.Width := MainGrid.DefaultRowHeight;
      finally
        bmpimage.Free;
      end;
    end;
end;

Procedure TMainForm.MainGridKeyDown(Sender: TObject; Var Key: word; Shift: TShiftState);
Var
  lItemType: TMenuItemType;
begin
  if SQLMenuItems.RecordCount > 0 then
    lItemType := strToMit(SQLMenuItems.FieldByName('itemType').AsString)
  else
    lItemType := MITNone;

  if (Key = VK_Return) or ((Key = VK_RIGHT) and (lItemType in [MITmenu, MITmenuprog, MITmenufile, MITmenuprogreload])) then
  begin
    acRun.Execute

  End
  else if ((Key = VK_LEFT) or (Key = VK_BACK))and (SQLMenu.FieldByName('upMenuId').AsInteger > 0) then
  begin
    NavigateUp
  End
  else if Key = VK_ESCAPE then
    MainForm.Close
  else
    inherited;
end;

Procedure TMainForm.MainGridKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
Begin

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

Procedure TMainForm.acRunExecute(Sender: TObject);
Var
  lItemType: TMenuItemType;
  lSubMenuId, lMenuItemId, lLoad, lReload, lTime, lInterval: LongInt;
  lSubMenuCmd: String;
begin
  lItemType := strToMit(SQLMenuItems.FieldByName('itemType').AsString);

  if lItemType =  MITprog then
  begin
    AsyncProcess1.CommandLine := SQLMenuItems.FieldByName('cmd').AsString;
    AsyncProcess1.Execute;
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
end;

Procedure TMainForm.SQLMenuAfterInsert(DataSet: TDataSet);
begin
  SQLMenu.FieldByName('Load').AsInteger := DateTimeToTimeStamp(time).Time div 1000;
end;

Procedure TMainForm.SQLMenuAfterScroll(DataSet: TDataSet);
var
  lId: longint;
begin
  SQLMenuItems.Close;
  lId := SQLMenu.FieldByName('id').AsInteger;
  SQLMenuItems.ParamByName('id').AsInteger := lId;
  SQLMenuItems.Open;
  MainGrid.DataSource.DataSet.Last;
  MainGrid.DataSource.DataSet.first;
  setWidthForm;
end;

Procedure TMainForm.LoadMenuFromLines(Const aLines: TStringList);
var
  lLine: string;
  i: integer;
  lMenuItemParser: TMenuItemParser;
  lMenuId, lActMenuId: longint;
begin
  // insert into menu
  for i := 0 to aLines.Count - 1 do
  begin
    lLine := Trim(aLines[i]);
    lLine := DelSpace1(lLine);
    if (lLine <> '') and not AnsiStartsStr('#', lLine) then
    begin
      //lActMenuId := SQLMenu.FieldByName('id').AsInteger;
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

Procedure TMainForm.LoadMenuFromProcess(const aCmd: String);
Var
  lSl: TStringList;
  i: Integer;
Begin
  // load data from process
  Process1.Options := [poWaitOnExit, poNoConsole, poUsePipes];
  Process1.CurrentDirectory := GetEnvironmentVariable('HOME');
  Process1.CommandLine := aCmd;
  Process1.Execute;

  lSl := TStringList.Create;
  Try
    lSl.LoadFromStream(Process1.Output);
    LoadMenuFromLines(lSl);

    SQLMenu.Edit;
    SQLMenu.FieldByName('Load').AsInteger := DateTimeToTimeStamp(time).Time div 1000;
    SQLMenu.Post;
    SQLMenu.ApplyUpdates;
  Finally
    FreeAndNil(lSl);
  End;
End;

Procedure TMainForm.setWidthForm;
var
  lWidth: integer;
  i: integer;
begin
  lWidth := 16 + MainGrid.DefaultRowHeight; // scroolbar wight + first column
  for i := 1 to MainGrid.Columns.Count - 1 do // from second column
    lWidth := lWidth + MainGrid.Columns[i].Width;

  MainForm.Width := lWidth;
end;

Procedure TMainForm.NavigateUp;
Var
  lMenuId: LongInt;
Begin
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
Var
  lMenuId: LongInt;
Begin
  Result := SQLMenu.Locate('id', aIdMenu, []);
  lMenuId := MainForm.SQLMenu.FieldByName('id').AsInteger;
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
