unit uInputForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TInputForm }

  TInputForm = class(TForm)
    Button1: TButton;
    InputEdit: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure InputEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private

  public

  end;

implementation

uses LCLType;

{$R *.lfm}

{ TInputForm }

procedure TInputForm.Button1Click(Sender: TObject);
begin
  Self.Close;
end;

procedure TInputForm.InputEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    if (Key = VK_Return) then
  begin
    Self.Close;
  end;
end;

end.

