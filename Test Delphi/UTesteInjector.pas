unit UTesteInjector;

interface

uses
  Rtti,
  TypInfo,
  SysUtils,
  DUnitX.TestFramework;

type
  IMyClass = Interface
  ['{62A5DFDB-ADA7-4DDA-8524-1CA04242E9F5}']
    function GetMessage: String;
  end;

  TFake = System.TInterfacedObject;


  TMyClass = class(TInterfacedObject, IMyClass)
  private
  public
    function GetMessage: String;
    class function New: IMyClass;
  end;


  [TestFixture]
  TTestInjector = class(TObject)
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestInjectorSington;
    [Test]
    procedure TestInjectorLazyLoad;
    [Test]
    procedure TestInjectorInterface;
    [Test]
    procedure TestInjectorInterfaceRefCountEqualTo;
    [Test]
    procedure TestInjectorInterfaceRefCountEqualThree;
  end;

implementation

uses
  app.injector;

procedure TTestInjector.Setup;
begin
end;

procedure TTestInjector.TearDown;
begin

end;

procedure TTestInjector.TestInjectorInterface;
var
  LMyClass1: IMyClass;
  LMyClass2: IMyClass;
begin
  InjectorBr.RegisterInterface<IMyClass, TMyClass>;

  LMyCLass1 := InjectorBr.GetInterface<IMyClass>;
  LMyCLass2 := InjectorBr.GetInterface<IMyClass>;

  Assert.AreEqual(TFake(LMyClass1).GetHashCode, TFake(LMyClass2).GetHashCode, 'different objects' );
  Assert.AreEqual(TFake(LMyClass1).RefCount, TFake(LMyClass2).RefCount, 'different objects' );
end;

procedure TTestInjector.TestInjectorInterfaceRefCountEqualThree;
var
  LMyClass1: IMyClass;
  LMyClass2: IMyClass;
begin
  InjectorBr.RegisterInterface<IMyClass, TMyClass>;

  LMyCLass1 := InjectorBr.GetInterface<IMyClass>;
  LMyCLass2 := InjectorBr.GetInterface<IMyClass>;

  Assert.AreEqual(TFake(LMyClass1).RefCount, 3, 'different objects' );
end;

procedure TTestInjector.TestInjectorInterfaceRefCountEqualTo;
var
  LMyClass1: IMyClass;
begin
  InjectorBr.RegisterInterface<IMyClass, TMyClass>;

  LMyCLass1 := InjectorBr.GetInterface<IMyClass>;
  Assert.AreEqual(TFake(LMyClass1).RefCount, 2, 'different objects' );
end;

procedure TTestInjector.TestInjectorLazyLoad;
var
  LMyClass1: TMyClass;
  LMyClass2: TMyClass;
begin
  InjectorBr.RegisterLazy<TMyClass>;

  LMyCLass1 := InjectorBr.Get<TMyClass>;
  LMyCLass2 := InjectorBr.Get<TMyClass>;

  Assert.AreEqual(LMyClass1.GetHashCode, LMyClass2.GetHashCode, 'different objects' );
end;

procedure TTestInjector.TestInjectorSington;
var
  LMyClass1: TMyClass;
  LMyClass2: TMyClass;
begin
  InjectorBr.RegisterSington<TMyClass>;

  LMyCLass1 := InjectorBr.Get<TMyClass>;
  LMyCLass2 := InjectorBr.Get<TMyClass>;

  Assert.AreEqual(LMyClass1.GetHashCode, LMyClass2.GetHashCode, 'different objects' );
end;

{ TMyClass }

function TMyClass.GetMessage: String;
begin
  Result := 'TMyClass Message!';
end;

class function TMyClass.New: IMyClass;
begin
  Result := Self.Create;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestInjector);
end.
