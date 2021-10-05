Program simpleMenu;

{$mode objfpc}{$H+}

Uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  sysutils, Forms, Dialogs, Interfaces, uMainForm, uMenuItem, debugForm, uTools,
  uwinmanager
  { you can add units after this };

{$R *.res}

Begin
  {$if declared(useHeapTrace)}
  globalSkipIfNoLeaks := true; // supported as of debugger version 3.2.0
  // setHeapTraceOutput('simpleMenuMem.log'); // supported as of debugger version 3.2.0
  {$endIf}

  Application.Scaled:=True;

  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);

  {$IFDEF Windows}
  Application.MainFormOnTaskBar := False;
  Application.TaskBarBehavior := tbSingleButton;
  {$ENDIF}

  Application.Run;
End.

