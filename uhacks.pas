unit uHacks;

// dirty and ugly code - but I not know how do it better

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function RepairCzechCharacters(aVal: string): string;

implementation

uses StrUtils;

function RepairCzechCharacters(aVal: string): string;
begin
  // ugly hack, but only works - for czech only :-(
  //{$IFDEF Windows}
  Result := aVal;
  Result := ReplaceStr(Result, '├í', 'á');
  Result := ReplaceStr(Result, '─Ź', 'č');
  Result := ReplaceStr(Result, '─Ć', 'ď');
  Result := ReplaceStr(Result, '├ę', 'é');
  Result := ReplaceStr(Result, '─Ť', 'ě');
  Result := ReplaceStr(Result, '├ş', 'í');
  Result := ReplaceStr(Result, '┼ł', 'ň');
  Result := ReplaceStr(Result, '├│', 'ó');
  Result := ReplaceStr(Result, '┼Ö', 'ř');
  Result := ReplaceStr(Result, '┼í', 'š');
  Result := ReplaceStr(Result, '┼ą', 'ť');
  Result := ReplaceStr(Result, '├║', 'ú');
  Result := ReplaceStr(Result, '┼»', 'ů');
  Result := ReplaceStr(Result, '├Ż', 'ý');
  Result := ReplaceStr(Result, '┼ż', 'ž');
  Result := ReplaceStr(Result, '├ü', 'Á');
  Result := ReplaceStr(Result, '─î', 'Č');
  Result := ReplaceStr(Result, '─Ä', 'Ď');
  Result := ReplaceStr(Result, '├ë', 'É');
  Result := ReplaceStr(Result, '─Ü', 'Ě');
  Result := ReplaceStr(Result, '├Ź', 'Í');
  Result := ReplaceStr(Result, '┼ç', 'Ň');
  Result := ReplaceStr(Result, '├ô', 'Ó');
  Result := ReplaceStr(Result, '┼ś', 'Ř');
  Result := ReplaceStr(Result, '┼á', 'Š');
  Result := ReplaceStr(Result, '┼Ą', 'Ť');
  Result := ReplaceStr(Result, '├Ü', 'Ú');
  Result := ReplaceStr(Result, '┼«', 'Ů');
  Result := ReplaceStr(Result, '├Ł', 'Ý');
  Result := ReplaceStr(Result, '┼Ż', 'Ž');
  //{$ENDIF}
end;

end.

