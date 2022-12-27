unit uwinmanager;

// Windows and desktops manager - only for MS Windows system

{ TODO -cWM : only for MS Windows}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, uMenuItem,
  windows, JwaPsApi, LConvEncoding;

procedure LoadMenuWindows(const aFilter, aSelfExe: String);
function ActivateWindow(const aWindowHandle: String): Boolean;
function ActivateProcess(const aExe: String): Boolean;
function GetCurrentWindow: Hwnd;
//function GetCurrentDesktopName(): string;

implementation

uses uMainForm;

//// https://github.com/Ciantic/VirtualDesktopAccessor
//function GetWindowDesktopNumber(hWindow: HWND): Integer; stdcall; external 'VirtualDesktopAccessor.dll';
//function IsPinnedWindow(hwnd: HWND): Integer; stdcall; external 'VirtualDesktopAccessor.dll';
//function GetCurrentDesktopNumber(): Integer; stdcall; external 'VirtualDesktopAccessor.dll';

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
  //lDesktop, lIsPined: Integer;
  lMenuItemParser: TMenuItemParser;
  lIsAppWindow, lIsWindowVisible: Boolean;
begin
  // load list windows
  hDesktop := GetDeskTopWindow;
  hWindow := GetWindow(hDesktop, GW_CHILD);
  while hWindow <> 0 do begin
    lShortCut := ''; // initialize value

    GetWindowText(hWindow, Buffer, 255);
    //ShowWindow(FindWindow('Shell_TrayWnd', nil), SW_HIDE);

    //lDesktop := GetWindowDesktopNumber(hWindow);
    //lIsPined := IsPinnedWindow(hWindow);

    lIsAppWindow := GetWindowLongPtr(hWindow, GWL_STYLE) and WS_EX_APPWINDOW<>0;
    lIsWindowVisible := IsWindowVisible(hWindow);

    if (Buffer <> '') and lIsWindowVisible and lIsAppWindow then
    begin
      lTitle := Buffer;
      //lTitle:= AnsiToUtf8(lTitle);
      lTitle:= CP1250ToUTF8(lTitle); // Hack for czech encoding :-(

      GetClassName(hWindow, Buffer, 255);
      lClass := buffer;

      //List.Add(IntToStr(lDesktop + 1) + ' | ' + lTitle + ' | ' + GetCurrentActiveProcessPath(hWindow) + ' | ' + lClass + ' | '+ IntToHex(hWindow,4));
      try // https://stackoverflow.com/questions/5951631/how-to-get-captions-of-actual-windows-currently-running
        lFullExe := GetCurrentActiveProcessPath(hWindow);
      Except
        lFullExe := 'ADMIN'
      end;
      lExeFile := ExtractFileName(lFullExe);
      lMenuTitle := (* '[' + IntToStr(lDesktop + 1) + '] ' *) '(' + lExeFile +') ' + lTitle;

      MainForm.SQLMenuItemsShortcutByCmd.ParamByName('cmd').AsString := '%'+lExeFile+'%';
      MainForm.SQLMenuItemsShortcutByCmd.ParamByName('name').AsString := lExeFile;
      MainForm.SQLMenuItemsShortcutByCmd.open;
      if MainForm.SQLMenuItemsShortcutByCmd.RecordCount >= 1 then
      begin
        MainForm.SQLMenuItemsShortcutByCmd.First;
        lShortCut := MainForm.SQLMenuItemsShortcutByCmd.FieldByName('shortcut').AsString;
      end;
      MainForm.SQLMenuItemsShortcutByCmd.close;

      if lExeFile <> ExtractFileName(aSelfExe) then // not add to menu self
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
end;

function ActivateProcess(const aExe: String): Boolean;
var
  hDesktop, hWindow: Hwnd;
  Buffer: array[0..255] of char;
  lTitle, lClass, lMenuTitle, lExeFile, lFullExe: String;
  //lDesktop, lIsPined: Integer;
  lMenuItemParser: TMenuItemParser;
begin
  Result := False;

  // load list windows
  hDesktop := GetDeskTopWindow;
  hWindow := GetWindow(hDesktop, GW_CHILD);
  while hWindow <> 0 do begin
    //GetWindowText(hWindow, Buffer, 255);
    //ShowWindow(FindWindow('Shell_TrayWnd', nil), SW_HIDE);

    //lDesktop := GetWindowDesktopNumber(hWindow);
    //lIsPined := IsPinnedWindow(hWindow);

    if IsWindowVisible(hWindow) (* and ((lDesktop = GetCurrentDesktopNumber()) or (lIsPined = 1)) *) then
    begin
      lFullExe := GetCurrentActiveProcessPath(hWindow);

      if lFullExe.Contains(aExe) then // not menu itself
      begin
        Result := ActivateWindow(IntToHex(hWindow,4));
        exit;
      end;
    end;
    hWindow := GetWindow(hWindow, GW_HWNDNEXT);
  end;
end;

function GetCurrentWindow: Hwnd;
begin
  result := GetActiveWindow;
end;

//function GetCurrentDesktopName(): string;
//begin
  //Result := IntToStr(GetCurrentDesktopNumber() + 1); // numbering from 0, bad name from 1
//end;

function ActivateWindow(const aWindowHandle: String): Boolean;
var hWindow: Hwnd;
  { TODO : test and delete death code }
  //actHandle: Hwnd;
  //lCnt: Integer;
begin
  Result := False;
  //lCnt := 0;
  hWindow := LongHexToDec(aWindowHandle);
  //actHandle := GetFocus;
  //while ((actHandle <> hWindow) and (lCnt < 10)) do { TODO : const for max cnt }
  begin
    if IsIconic(hWindow) then
      ShowWindow(hWindow,SW_RESTORE);
    SetForegroundWindow(hWindow);
    SetActiveWindow(hWindow);
    SetFocus(hWindow);
    //Sleep(50);
    //Inc(lCnt);
  end;
  Result := True;
end;

end.

