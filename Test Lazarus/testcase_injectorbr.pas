unit testcase_injectorbr;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TFake = System.TInterfacedObject;

  IMyClass = Interface
  ['{62A5DFDB-ADA7-4DDA-8524-1CA04242E9F5}']
    function GetMessage: String;
  end;

  { TMyClass }

  TMyClass = class(TInterfacedObject, IMyClass)
  private
  public
    function GetMessage: String;
    class function New: IMyClass;
  end;

  TTestInjectorBr= class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInjectorSington;
  end;

implementation

uses
  app.injector;

{ TMyClass }

function TMyClass.GetMessage: String;
begin
  Result := 'TMyClass Message!';
end;

class function TMyClass.New: IMyClass;
begin
  Result := Self.Create;
end;

procedure TTestInjectorBr.SetUp;
begin

end;

procedure TTestInjectorBr.TearDown;
begin

end;

procedure TTestInjectorBr.TestInjectorSington;
var
  LMyClass1: TMyClass;
  LMyClass2: TMyClass;
  LInjector: TInjectorBr;
begin
  LInjector := TInjectorBr.Create;
  try
    LInjector.Singleton<TMyClass>;

    LMyCLass1 := TMyClass(LInjector.Get<TMyClass>);
    LMyCLass2 := TMyClass(LInjector.Get<TMyClass>);

    AssertTrue('different objects', LMyClass1 = LMyClass2);
  finally
    LInjector.Free;
  end;
end;

initialization

  RegisterTest(TTestInjectorBr);
end.

