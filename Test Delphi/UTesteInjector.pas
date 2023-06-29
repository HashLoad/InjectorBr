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
  public
    function GetMessage: String;
  end;

  // Testar Auto Inject
  IParamClass = Interface
    ['{6A1154CC-51D2-47BE-8B19-4C949D6A5881}']
    function GetMessage: String;
  end;

  TParamClass = class(TInterfacedObject, IParamClass)
  public
    function GetMessage: String;
  end;

  TMyClassParam = class
  private
    FClass: TParamClass;
    FInterface: IParamClass;
  public
    constructor Create(const AClass: TParamClass;
                       const AInterface: IParamClass);
    property ParamClass: TParamClass read FClass;
    property ParamInterface: IParamClass read FInterface;
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
    procedure TestInjectorInterfaceTag;
    [Test]
    procedure TestInjectorInterfaceRefCountEqualOne;
    [Test]
    procedure TestInjectorInterfaceRefCountEqualTo;
    [Test]
    procedure TestInjectorInterfaceGetMessage;
    [Test]
    procedure TestInjectorAutoInjectParams;
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

    Assert.AreEqual(LMyClass1, LMyClass2, '(MyClass1 <> MyClass2)' );
    Assert.AreEqual(TFake(LMyClass1).RefCount, TFake(LMyClass2).RefCount, '(MyClass1.RefCount <> MyClass2.RefCount)' );
  finally
    LInjector.Free;
  end;
end;

procedure TTestInjector.TestInjectorInterfaceGetMessage;
var
  LMyClass1: IMyClass;
  LInjector: TInjectorBr;
begin
  LInjector := TInjectorBr.Create;
  try
    LInjector.SingletonInterface<IMyClass, TMyClass>;

    LMyCLass1 := LInjector.GetInterface<IMyClass>;
    Assert.AreEqual(LMyClass1.GetMessage, 'TMyClass Message!', '(Message <> TMyClass Message!)' );
  finally
    LInjector.Free;
  end;
end;

procedure TTestInjector.TestInjectorInterfaceTag;
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

    Assert.AreEqual(LMyClass1, LMyClass2, '(LMyClass1 <> LMyClass2)' );
  finally
    LInjector.Free;
  end;
end;

procedure TTestInjector.TestInjectorInterfaceRefCountEqualTo;
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

    Assert.AreEqual(TFake(LMyClass1).RefCount, 2, 'MyClass1.RefCount <> 2' );
  finally
    LInjector.Free;
  end;
end;

procedure TTestInjector.TestInjectorInterfaceRefCountEqualOne;
var
  LMyClass1: IMyClass;
  LInjector: TInjectorBr;
begin
  LInjector := TInjectorBr.Create;
  try
    LInjector.SingletonInterface<IMyClass, TMyClass>;

    LMyCLass1 := LInjector.GetInterface<IMyClass>;
    Assert.AreEqual(TFake(LMyClass1).RefCount, 1, 'MyClass1.RefCount <> 1' );
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
    LInjector.SingletonLazy<TMyClass>;

    LMyCLass1 := LInjector.Get<TMyClass>;
    LMyCLass2 := LInjector.Get<TMyClass>;

    Assert.AreEqual(LMyClass1, LMyClass2, 'LMyClass1 <> LMyClass2');
  finally
    LInjector.Free;
  end;
end;

procedure TTestInjector.TestInjectorAutoInjectParams;
var
  LParamClass: TParamClass;
  LParamInterface: IParamClass;
  LMyClassParam: TMyClassParam;
  LMyClassParam1: TMyClassParam;
  LInjector: TInjectorBr;
begin
  LInjector := TInjectorBr.Create;
  try
    LInjector.Singleton<TParamClass>;
    LInjector.SingletonInterface<IParamClass, TParamClass>;
    // TMyClassParam.Create(const AClass: TParamClass; const AInterface: IParamClass);
    LInjector.Singleton<TMyClassParam>;
    // Auto Inject Params
    LMyClassParam := LInjector.Get<TMyClassParam>;

    Assert.IsNotNull(LMyClassParam.ParamClass, 'ParamClass is nil');
    Assert.IsNotNull(LMyClassParam.ParamInterface, 'ParamInterface is nil');
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

    Assert.AreEqual(LMyClass1, LMyClass2, 'LMyClass1 <> LMyClass2' );
  finally
    LInjector.Free;
  end;
end;

{ TMyClass }

function TMyClass.GetMessage: String;
begin
  Result := 'TMyClass Message!';
end;

{ TMyClassParam }

constructor TMyClassParam.Create(const AClass: TParamClass;
  const AInterface: IParamClass);
var
  L1: string;
  L2: string;
begin
  FClass := AClass;
  FInterface := AInterface;
  L1 := FClass.GetMessage;
  L2 := FInterface.GetMessage;
end;

{ TParamClass }

function TParamClass.GetMessage: String;
begin
  Result := 'TParamClass Message!';
end;

initialization
  TDUnitX.RegisterTestFixture(TTestInjector);
end.
