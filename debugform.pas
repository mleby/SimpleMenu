Unit debugForm;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  PairSplitter, ExtCtrls, StdCtrls, DBCtrls, uMainForm;

Type

  { TDebugForm }

  TDebugForm = Class(TForm)
    DBEdit1: TDBEdit;
    DBEdit2: TDBEdit;
    DBEdit3: TDBEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MenuGrid: TDBGrid;
    MenuGrid1: TDBGrid;
    MenuItemGrid: TDBGrid;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel1: TPanel;
    Procedure FormCreate(Sender: TObject);
  Private
    { private declarations }
  Public
    { public declarations }
  End;

Implementation

{$R *.lfm}

{ TDebugForm }

Procedure TDebugForm.FormCreate(Sender: TObject);
Begin
  //MenuGrid.DataSource.DataSet.Active := true;
  //MenuItemGrid.DataSource.DataSet.Active := true;
end;

End.

