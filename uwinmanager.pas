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
function ActivateProcess(const aExe: String): Boolean;
function GetCurrentDesktopName(): string;

implementation

uses uMainForm;

// https://github.com/Ciantic/VirtualDesktopAccessor
function GetWindowDesktopNumber(hWindow: HWND): Integer; stdcall; external 'VirtualDesktopAccessor.dll';
function IsPinnedWindow(hwnd: HWND): Integer; stdcall; external 'VirtualDesktopAccessor.dll';
function GetCurrentDesktopNumber(): Integer; stdcall; external 'VirtualDesktopAccessor.dll';

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
  lTitle, lClass, lMenuTitle, lExeFile, lFullExe, i, lShortCut: String;
  lDesktop, lIsPined: Integer;
  lMenuItemParser: TMenuItemParser;
begin
  // load list windows
  hDesktop := GetDeskTopWindow;
  hWindow := GetWindow(hDesktop, GW_CHILD);
  while hWindow <> 0 do begin
    GetWindowText(hWindow, Buffer, 255);
    //ShowWindow(FindWindow('Shell_TrayWnd', nil), SW_HIDE);

    lDesktop := GetWindowDesktopNumber(hWindow);
    lIsPined := IsPinnedWindow(hWindow);

    if (Buffer <> '') and IsWindowVisible(hWindow) and ((lDesktop >= 0) or (lIsPined = 1)) then
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

      MainForm.SQLMenuItemsShortcutByCmd.ParamByName('cmd').AsString := '%'+lExeFile+'%';
      MainForm.SQLMenuItemsShortcutByCmd.open;
      if MainForm.SQLMenuItemsShortcutByCmd.RecordCount >= 1 then
        lShortCut := MainForm.SQLMenuItemsShortcutByCmd.FieldByName('shortcut').AsString;
      MainForm.SQLMenuItemsShortcutByCmd.close;

      //showmessage(lExeFile + ' <> ' + ExtractFileName(aSelfExe));
      if lExeFile <> ExtractFileName(aSelfExe) then // not menu itself
      begin
        lMenuItemParser := TMenuItemParser.Create(lMenuTitle, IntToHex(hWindow,4), lShortCut);
        try
          MainForm.AddMenuItem(lMenuItemParser);
        finally
          FreeAndNil(lMenuItemParser);
        end;
      end;
    end;
    hWindow := GetWindow(hWindow, GW_HWNDNEXT);
  end;

      { TODO : Nefunkční volání }
    // aply postmenusql script
    //if MainForm.PostMenuSQL.Script.Count > 0 then
    //begin
    //  //ShowMessage(PostMenuSQL.Script.Text);
    //  for i in MainForm.PostMenuSQL.Script do
    //    MainForm.MenuDB.ExecuteDirect(i);
    //  //MenuDB.ExecuteDirect('update menuItem set shortcut=''d'' where itemType = ''MITwindow'' and name like ''%(bds.exe)''');
    //end;
end;

function ActivateProcess(const aExe: String): Boolean;
var
  hDesktop, hWindow: Hwnd;
  Buffer: array[0..255] of char;
  lTitle, lClass, lMenuTitle, lExeFile, lFullExe: String;
  lDesktop, lIsPined: Integer;
  lMenuItemParser: TMenuItemParser;
begin
  Result := False;

  // load list windows
  hDesktop := GetDeskTopWindow;
  hWindow := GetWindow(hDesktop, GW_CHILD);
  while hWindow <> 0 do begin
    //GetWindowText(hWindow, Buffer, 255);
    //ShowWindow(FindWindow('Shell_TrayWnd', nil), SW_HIDE);

    lDesktop := GetWindowDesktopNumber(hWindow);
    lIsPined := IsPinnedWindow(hWindow);

    if IsWindowVisible(hWindow) and ((lDesktop = GetCurrentDesktopNumber()) or (lIsPined = 1)) then
    begin
      lFullExe := GetCurrentActiveProcessPath(hWindow);

      if aExe = lFullExe then // not menu itself
      begin
        ActivateWindow(IntToHex(hWindow,4));
        Result := True;
      end;
    end;
    hWindow := GetWindow(hWindow, GW_HWNDNEXT);
  end;
end;

function GetCurrentDesktopName(): string;
begin
  Result := IntToStr(GetCurrentDesktopNumber() + 1); // numbering from 0, bad name from 1
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

