Program simpleMenu;

{$mode objfpc}{$H+}

Uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  sysutils,
  Interfaces, // this includes the LCL widgetset
  Forms, uMainForm, uMenuItem, debugForm
  { you can add units after this };

{$R *.res}

Begin
  if Application.HasOption('h', 'help') then begin
    writeln('Usage: simpleMenu -(f|p) "menu file or cmd" [options...]');
    writeln('         one of -f or -p must be specified as start point for menu');
    writeln('    -h --help             show this help');
    writeln('    -f X --file=X         path to menu file used as start point for menu');
    writeln('    -p X --process=X      command for generate menu used as start point for menu');
    writeln('    -s X --search=X       count of menu items for automatic enable find feature');
    writeln('    -r X --reload=X       enable dynamic menu filtering and count minimal chars for search');
    writeln('    -x X --showfile=X     extra options for menu cmd');
    exit;
  end;

  if not (Application.HasOption('f', 'file') or Application.HasOption('p', 'process'))then
  begin
    writeln('One menu source must be specified (file or process).');
    exit;
  End;

  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
End.

