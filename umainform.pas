Unit uMainForm;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, db, sqlite3conn, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids, ActnList, ExtCtrls, StdCtrls, Grids,
  AsyncProcess;

Type

  { TMainForm }

  TMainForm = Class(TForm)
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
    SQLMenu: TSQLQuery;
    SQLMenuItems: TSQLQuery;
    SQLTransaction: TSQLTransaction;
    Procedure acDebugExecute(Sender: TObject);
    Procedure acRunExecute(Sender: TObject);
    Procedure FormActivate(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure MainGridDrawColumnCell(Sender: TObject; Const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
    Procedure MainGridKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure SQLMenuAfterInsert(DataSet: TDataSet);
    Procedure SQLMenuAfterScroll(DataSet: TDataSet);
    Procedure SQLMenuItemsAfterOpen(DataSet: TDataSet);
  Private
    Procedure setWidthForm;
    { private declarations }
  Public
    { public declarations }
  End;

Var
  MainForm: TMainForm;

Implementation

Uses dateutils, strutils, uMenuItem, debugForm, LCLType;

{$R *.lfm}

{ TMainForm }

Procedure TMainForm.FormCreate(Sender: TObject);
Var
  lFile, lLine: String;
  sl: TStringList;
  i: Integer;
  lMenuItemParser: TMenuItemParser;
Begin
  // color
  {TODO -oLebeda -cNone: configure colors...}

  // sure create DB
  MenuDB.Close;
  DeleteFile('/tmp/debugMenu.db'); // uncoment only for developnet (real DB for object inspector and design in lazarus)
  MenuDB.DatabaseName := '/tmp/debugMenu.db'; // uncoment only for developnet (real DB for object inspector and design in lazarus)
  //MenuDB.DatabaseName := ':memory:';
  MenuDB.Open;
  MenuDB.ExecuteDirect('CREATE TABLE IF NOT EXISTS menu (id INTEGER PRIMARY KEY , upMenuId INTEGER, name NOT NULL, cmd, path, load INTEGER, reloadInterval INTEGER)');
  MenuDB.ExecuteDirect('CREATE TABLE IF NOT EXISTS menuItem (id INTEGER PRIMARY KEY , menuId INTEGER NOT NULL, itemType, name, search, icon, shortcut, cmd, subMenuPath, subMenuCmd, subMenuReloadInterval INTEGER, subMenuId INTEGER, subMenuChar, FOREIGN KEY(menuId) REFERENCES menu(id))');
  MenuDB.Transaction.Commit;

  // fill root menu
  SQLMenu.Active := true;
  SQLMenu.Insert;
  SQLMenu.FieldByName('name').AsString := 'ROOT'; {TODO -oLebeda -cNone: pojmenování na cmdline}
  {TODO -oLebeda -cNone: založit do GitHub a vyrobit základní projekt}
  SQLMenu.Post;
  SQLMenu.ApplyUpdates;

  SQLMenuItems.Active := True;

  if Application.HasOption('f', 'file') then
  begin
    lFile := Application.GetOptionValue('f', 'file'); {TODO -oLebeda -cNone: podpora více souborů najednou}
    sl := TStringList.Create;
    try
      sl.LoadFromFile(lFile);

      // insert into menu
      for i := 0 to sl.Count - 1 do
      begin
        lLine := Trim(sl[i]);
        lLine := DelSpace1(lLine);
        if (lLine <> '') and not AnsiStartsStr('#', lLine) then
        begin
          lMenuItemParser := TMenuItemParser.Create(lLine, SQLMenu.FieldByName('id').AsInteger);
          try
            SQLMenuItems.Insert;
            //id, menuId, itemType, name, search, icon, shortcut, cmd, subMenuPath, subMenuCmd, subMenuReloadInterval, subMenuId, subMenuChar
            SQLMenuItems.FieldByName('menuId').AsInteger := lMenuItemParser.menuId;
            SQLMenuItems.FieldByName('itemType').AsString := mtToStr(lMenuItemParser.itemType);
            SQLMenuItems.FieldByName('name').AsString := lMenuItemParser.name;
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
          Finally
            FreeAndNil(lMenuItemParser);
          End;
        End;
      end;

    Finally
      FreeAndNil(sl);
    End;
  End;

  MenuDB.Transaction.Commit;

  // open grid data
  SQLMenu.Active := true;
  SQLMenu.First;

  MainGrid.DataSource.DataSet.Active := False;
  MainGrid.DataSource.DataSet.Active := true;
  setWidthForm;
end;

Procedure TMainForm.MainGridDrawColumnCell(Sender: TObject; Const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  bmpImage: TPicture;
  intX, intY: Integer;
  lIconName: String;
Begin
  if Column.FieldName = 'icon' then
    With MainGrid.Canvas Do
    begin
      fillRect(rect);
        bmpImage := TPicture.Create;
        try
          lIconName := MainGrid.DataSource.DataSet.FieldByName('icon').AsString;

          {TODO -oLebeda -cNone: vymyslet expanzi ikon}
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
          else
            bmpImage.Clear;


          StretchDraw(Rect, bmpImage.Bitmap);
          Column.Width := MainGrid.DefaultRowHeight;
        finally
          bmpimage.Free;
        end;
    End;
end;

Procedure TMainForm.MainGridKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
Begin
  if Key = VK_Return then
    acRun.Execute
  else
    Inherited;
end;

Procedure TMainForm.acDebugExecute(Sender: TObject);
Var
  lForm: TDebugForm;
Begin
  lForm := TDebugForm.Create(self);
  try
    lForm.ShowModal;
  Finally
    FreeAndNil(lForm);
  End;
end;

Procedure TMainForm.acRunExecute(Sender: TObject);
Begin
  AsyncProcess1.CommandLine := SQLMenuItems.FieldByName('cmd').AsString;
  AsyncProcess1.Execute;

  MainForm.Close; {TODO -oLebeda -cNone: volitelně}
end;

Procedure TMainForm.FormActivate(Sender: TObject);
Begin


end;

Procedure TMainForm.SQLMenuAfterInsert(DataSet: TDataSet);
Begin
  SQLMenu.FieldByName('Load').AsInteger := DateTimeToTimeStamp(time).Time;
end;

Procedure TMainForm.SQLMenuAfterScroll(DataSet: TDataSet);
Var
  lId: LongInt;
Begin
  SQLMenuItems.Close;
  lId := SQLMenu.FieldByName('id').AsInteger;
  SQLMenuItems.ParamByName('id').AsInteger := lId;
  SQLMenuItems.Open;
end;

Procedure TMainForm.SQLMenuItemsAfterOpen(DataSet: TDataSet);
Begin

end;

Procedure TMainForm.setWidthForm;
Var
  lWidth: Integer;
  i: Integer;
Begin
  {TODO -oLebeda -cNone: scrollbar only when need}
  lWidth := 16 + MainGrid.DefaultRowHeight; // scroolbar wight + first column
  For i := 1 To MainGrid.Columns.Count - 1 Do // from second column
      lWidth := lWidth + MainGrid.Columns[i].Width;

    MainForm.Width := lWidth;
End;

End.

