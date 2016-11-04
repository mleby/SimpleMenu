Unit debugForm;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids, PairSplitter, uMainForm;

Type

  { TDebugForm }

  TDebugForm = Class(TForm)
    MenuGrid: TDBGrid;
    MenuItemGrid: TDBGrid;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
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

