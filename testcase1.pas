Unit TestCase1;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

Type

  TTestCase1 = Class(TTestCase)
  protected
    Procedure SetUp; override;
    Procedure TearDown; override;
  Published
    Procedure TestConvertEnum;
  End;

Implementation

Uses uMenuItem;

Procedure TTestCase1.TestConvertEnum;
Begin
  AssertEquals('prog', mtToStr(prog));
  AssertTrue(prog = strToMt('prog'));
End;

Procedure TTestCase1.SetUp;
Begin

End;

Procedure TTestCase1.TearDown;
Begin

End;

Initialization

  RegisterTest(TTestCase1);
End.

