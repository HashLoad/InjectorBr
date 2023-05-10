unit app.injector.events;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Rtti,
  {$ifdef fpc}
  app.injector.lazarus,
  {$endif}
  SysUtils;

type
  TConstructorParams = TArray<TValue>;
  TConstructorCallback = TFunc<TConstructorParams>;

  TInjectorEvents = class
  private
    FOnDestroy: TProc<TObject>;
    FOnCreate: TProc<TObject>;
    FOnParams: TConstructorCallback;
    procedure _SetOnDestroy(AOnDestroy: TProc<TObject>);
    procedure _SetOnCreate(AOnCreate: TProc<TObject>);
    procedure _SetOnParams(Value: TConstructorCallback);
  public
    property OnDestroy: TProc<TObject> read FOnDestroy write _SetOnDestroy;
    property OnCreate: TProc<TObject> read FOnCreate write _SetOnCreate;
    property OnParams: TConstructorCallback read FOnParams write _SetOnParams;
  end;

implementation

{ TInjectorEvents }

procedure TInjectorEvents._SetOnDestroy(AOnDestroy: TProc<TObject>);
begin
  FOnDestroy := TProc<TObject>(AOnDestroy);
end;

procedure TInjectorEvents._SetOnParams(Value: TConstructorCallback);
begin
  FOnParams := Value;
end;

procedure TInjectorEvents._SetOnCreate(AOnCreate: TProc<TObject>);
begin
  FOnCreate := AOnCreate;
end;

end.
