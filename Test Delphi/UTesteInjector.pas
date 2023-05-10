unit UTesteInjector;

interface

uses
  Rtti,
  TypInfo,
  SysUtils,
  DUnitX.TestFramework;

type
  TFake = System.TInterfacedObject;

  IMyClass = Interface
  ['{62A5DFDB-ADA7-4DDA-8524-1CA04242E9F5}']
    function GetMessage: String;
  end;

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
    procedure TestInjectorFactory;
    [Test]
    procedure TestInjectorInterface;
    [Test]
    procedure TestInjectorInterfaceName;
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

procedure TTestInjector.TestInjectorFactory;
var
  LMyClass1: TMyClass;
  LMyClass2: TMyClass;
  LInjector: TInjectorBr;
begin
  LInjector := TInjectorBr.Create;
  try
    LInjector.Factory<TMyClass>;

    LMyCLass1 := LInjector.Get<TMyClass>;
    LMyCLass2 := LInjector.Get<TMyClass>;

    Assert.AreNotEqual(LMyClass1, LMyClass2, 'equal objects' );
  finally
    LInjector.Free;
  end;
end;

procedure TTestInjector.TestInjectorInterface;
var
  LMyClass1: IMyClass;
  LMyClass2: IMyClass;
  LInjector: TInjectorBr;
begin
  LInjector := TInjectorBr.Create;
  try
    LInjector.SingletonInterface<IMyClass, TMyClass>;

    LMyCLass1 := LInjector.GetInterface<IMyClass>;
    LMyCLass2 := LInjector.GetInterface<IMyClass>;

    Assert.AreEqual(TFake(LMyClass1), TFake(LMyClass2), 'different objects' );
    Assert.AreEqual(TFake(LMyClass1).RefCount, TFake(LMyClass2).RefCount, 'different objects' );
  finally
    LInjector.Free;
  end;
end;

procedure TTestInjector.TestInjectorInterfaceName;
var
  LMyClass1: IMyClass;
  LMyClass2: IMyClass;
  LInjector: TInjectorBr;
begin
  LInjector := TInjectorBr.Create;
  try
    LInjector.SingletonInterface<IMyClass, TMyClass>('TMyClass');

    LMyCLass1 := LInjector.GetInterface<IMyClass>('TMyClass');
    LMyCLass2 := LInjector.GetInterface<IMyClass>('TMyClass');

    Assert.AreEqual(TFake(LMyClass1), TFake(LMyClass2), 'different objects' );
    Assert.AreEqual(TFake(LMyClass1).RefCount, TFake(LMyClass2).RefCount, 'different objects' );
  finally
    LInjector.Free;
  end;
end;

procedure TTestInjector.TestInjectorInterfaceRefCountEqualThree;
var
  LMyClass1: IMyClass;
  LMyClass2: IMyClass;
  LInjector: TInjectorBr;
begin
  LInjector := TInjectorBr.Create;
  try
    LInjector.SingletonInterface<IMyClass, TMyClass>;

    LMyCLass1 := LInjector.GetInterface<IMyClass>;
    LMyCLass2 := LInjector.GetInterface<IMyClass>;

    Assert.AreEqual(TFake(LMyClass1).RefCount, 3, 'different objects' );
  finally
    LInjector.Free;
  end;
end;

procedure TTestInjector.TestInjectorInterfaceRefCountEqualTo;
var
  LMyClass1: IMyClass;
  LInjector: TInjectorBr;
begin
  LInjector := TInjectorBr.Create;
  try
    LInjector.SingletonInterface<IMyClass, TMyClass>;

    LMyCLass1 := LInjector.GetInterface<IMyClass>;
    Assert.AreEqual(TFake(LMyClass1).RefCount, 2, 'different objects' );
  finally
    LInjector.Free;
  end;
end;

procedure TTestInjector.TestInjectorLazyLoad;
var
  LMyClass1: TMyClass;
  LMyClass2: TMyClass;
  LInjector: TInjectorBr;
begin
  LInjector := TInjectorBr.Create;
  try
    LInjector.Singleton<TMyClass>;

    LMyCLass1 := LInjector.Get<TMyClass>;
    LMyCLass2 := LInjector.Get<TMyClass>;

    Assert.AreEqual(LMyClass1, LMyClass2, 'different objects' );
  finally
    LInjector.Free;
  end;
end;

procedure TTestInjector.TestInjectorSington;
var
  LMyClass1: TMyClass;
  LMyClass2: TMyClass;
  LInjector: TInjectorBr;
begin
  LInjector := TInjectorBr.Create;
  try
    LInjector.Singleton<TMyClass>;

    LMyCLass1 := LInjector.Get<TMyClass>;
    LMyCLass2 := LInjector.Get<TMyClass>;

    Assert.AreEqual(LMyClass1, LMyClass2, 'different objects' );
  finally
    LInjector.Free;
  end;
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
