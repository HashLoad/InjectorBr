unit app.injector.events;

interface

uses
  Rtti,
  SysUtils;

type
  TConstructorParams = TArray<TValue>;
  TConstructorCallback = TFunc<TConstructorParams>;

  TInjectorEvents = class
  private
    FOnDestroy: TProc<TObject>;
    FOnCreate: TProc<TObject>;
    FOnParams: TConstructorCallback;
    procedure _SetOnDestroy(const AOnDestroy: TProc<TObject>);
    procedure _SetOnCreate(const AOnCreate: TProc<TObject>);
    procedure _SetOnParams(const Value: TConstructorCallback);
  public
    property OnDestroy: TProc<TObject> read FOnDestroy write _SetOnDestroy;
    property OnCreate: TProc<TObject> read FOnCreate write _SetOnCreate;
    property OnParams: TConstructorCallback read FOnParams write _SetOnParams;
  end;

implementation

{ TInjectorEvents }

procedure TInjectorEvents._SetOnDestroy(const AOnDestroy: TProc<TObject>);
begin
  FOnDestroy := TProc<TObject>(AOnDestroy);
end;

procedure TInjectorEvents._SetOnParams(const Value: TConstructorCallback);
begin
  FOnParams := Value;
end;

procedure TInjectorEvents._SetOnCreate(const AOnCreate: TProc<TObject>);
begin
  FOnCreate := AOnCreate;
end;

end.
