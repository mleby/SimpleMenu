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
    writeln('Usage: Mloc [options...]');
    writeln('    -h --help             show this help');
    //writeln('    -l --localdb          path to database file, default: $HOME/.mlocate.db');
    //writeln('    -t --tag              tag');
    //writeln('    -k --keep             keep window after run action');
    //writeln('    -f --showfile         show path to file in result grid');
    //writeln('    -p --path=X           paths for search');
    //writeln('    -s --search=X         pattern for search');
    //writeln('    -w --where=X          additional where part for search');
    //writeln('    -q --query            automatic run search after start');
    //writeln('    -d --delay=X          delay input for run automatic query in ms, default: 750');
    //writeln('    -a --auto=X           count of character in input for run automatic query, default: 3');
    //writeln('    -n --name=X           set title for window, default: search');
    //{TODO -oLebeda -cNone: debug výpisy do konzole}

    exit;
  end;

  if Application.HasOption('f', 'file') and Application.HasOption('p', 'process')
    or not (Application.HasOption('f', 'file') or Application.HasOption('p', 'process'))then
  begin
    writeln('One menu source must be specified (file or process).');
    exit;
  End;

  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
End.

