unit uwinmanager;

// Windows and desktops manager - only for MS Windows system

{ TODO -cWM : only for MS Windows}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, uMenuItem,
  windows, JwaPsApi, LConvEncoding;

procedure LoadMenuWindows(const aFilter, aSelfExe: String);
procedure ActivateWindow(const aWindowHandle: String);

implementation

uses uMainForm;

// https://github.com/Ciantic/VirtualDesktopAccessor
function GetWindowDesktopNumber(hWindow: HWND): Integer; stdcall; external 'VirtualDesktopAccessor.dll';

function GetCurrentActiveProcessPath(hWindow: Hwnd): String;
var
  pid     : DWORD;
  hProcess: THandle;
  path    : array[0..4095] of Char;
begin
  GetWindowThreadProcessId(hWindow, pid);

  hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, FALSE, pid);
  if hProcess <> 0 then
    try
      if GetModuleFileNameEx(hProcess, 0, @path[0], Length(path)) = 0 then
        RaiseLastOSError;

      result := path;
    finally
      CloseHandle(hProcess);
    end
  else
    RaiseLastOSError;
end;

function LongHexToDec(Str: string): QWord;
var
  Counter : Integer;
  DecValue: QWord;
begin
  Result  :=0;
  DecValue:=1;
  Str     :=AnsiUpperCase(Str);
  for Counter:=Length(Str) downto 1 do
    begin
      case Str[Counter] of
        '1'..'9': Result:=Result+(Ord(Str[Counter])-Ord('0'))*DecValue;
        'A'..'F': Result:=Result+(Ord(Str[Counter])-Ord('A')+10)*DecValue;
      end;
      DecValue:=DecValue shl 4;
    end;
end;

procedure LoadMenuWindows(const aFilter, aSelfExe: String);
var
  hDesktop, hWindow: Hwnd;
  Buffer: array[0..255] of char;
  lTitle, lClass, lMenuTitle, lExeFile, lFullExe: String;
  lDesktop: Integer;
  lMenuItemParser: TMenuItemParser;
begin
  // load list windows
  hDesktop := GetDeskTopWindow;
  hWindow := GetWindow(hDesktop, GW_CHILD);
  while hWindow <> 0 do begin
    GetWindowText(hWindow, Buffer, 255);
    ShowWindow(FindWindow('Shell_TrayWnd', nil), SW_HIDE);

    lDesktop := GetWindowDesktopNumber(hWindow);

    if (Buffer <> '') and IsWindowVisible(hWindow) and (lDesktop >= 0) then
    { TODO -cWM : filter window list}
    begin
      lTitle := Buffer;
      //lTitle:= AnsiToUtf8(lTitle);
      lTitle:= CP1250ToUTF8(lTitle); // Hack for czech encoding :-(

      GetClassName(hWindow, Buffer, 255);
      lClass := buffer;

      //List.Add(IntToStr(lDesktop + 1) + ' | ' + lTitle + ' | ' + GetCurrentActiveProcessPath(hWindow) + ' | ' + lClass + ' | '+ IntToHex(hWindow,4));
      lFullExe := GetCurrentActiveProcessPath(hWindow);
      lExeFile := ExtractFileName(lFullExe);
      lMenuTitle := '[' + IntToStr(lDesktop + 1) + '] ' + lTitle + ' (' + lExeFile +')';

      //showmessage(lExeFile + ' <> ' + ExtractFileName(aSelfExe));
      if lExeFile <> ExtractFileName(aSelfExe) then // not menu itself
      begin
        lMenuItemParser := TMenuItemParser.Create(lMenuTitle, IntToHex(hWindow,4));
        try
          MainForm.AddMenuItem(lMenuItemParser);
        finally
          FreeAndNil(lMenuItemParser);
        end;
      end;
    end;
    hWindow := GetWindow(hWindow, GW_HWNDNEXT);
  end;
end;

procedure ActivateWindow(const aWindowHandle: String);
var hWindow: Hwnd;
begin
  hWindow := LongHexToDec(aWindowHandle);
  if IsIconic(hWindow) then
    ShowWindow(hWindow,SW_RESTORE);
  SetForegroundWindow(hWindow);
  SetActiveWindow(hWindow);
end;

end.

