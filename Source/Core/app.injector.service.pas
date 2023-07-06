{
         APPInjector Brasil - Dependency Injection for Delphi


                   Copyright (c) 2023, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Versão 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos é permitido copiar e distribuir cópias deste documento de
       licença, mas mudá-lo não é permitido.

       Esta versão da GNU Lesser General Public License incorpora
       os termos e condições da versão 3 da GNU General Public License
       Licença, complementado pelas permissões adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{ @abstract(APPInjectorBr Framework)
  @created(15 Mar 2023)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @author(Site : https://www.isaquepinheiro.com.br)
}

unit app.injector.service;

interface

uses
  Rtti,
  TypInfo,
  SysUtils,
  Generics.Collections,
  app.injector.events;

type
  TInjectionMode = (imSingleton, imFactory);
  TConstructorEvents = TObjectDictionary<string, TInjectorEvents>;

  TServiceData = class
  private
    FServiceClass: TClass;
    FInjectionMode: TInjectionMode;
    FInstance: TObject;
    FInterface: TValue;
    FGuid: TGUID;
    function _FactoryInstance<T: class>(const AInjectorEvents: TConstructorEvents;
      const AParams: TConstructorParams = nil): T;
    function _FactoryInterface<I: IInterface>(const AKey: string;
      const AInjectorEvents: TConstructorEvents;
      const AParams: TConstructorParams = nil): TValue;
    function _Factory(const AParams: TConstructorParams): TValue;
  public
    constructor Create(const AServiceClass: TClass;
      const AInstance: TObject;
      const AInjectionMode: TInjectionMode); overload;
    constructor CreateInterface(const AServiceClass: TClass;
      const AGuid: TGUID;
      const AInterface: TValue;
      const AInjectionMode: TInjectionMode); overload;
    destructor Destroy; override;
    function ServiceClass: TClass;
    function InjectionMode: TInjectionMode;
    function AsInstance: TObject; overload;
    function GetInstance<T: class>(
      const AInjectorEvents: TConstructorEvents;
      const AParams: TConstructorParams): T; overload;
    function GetInterface<I: IInterface>(const AKey: string;
      const AInjectorEvents: TConstructorEvents;
      const AParams: TConstructorParams): I;
  end;

implementation

constructor TServiceData.Create(const AServiceClass: TClass;
  const AInstance: TObject;
  const AInjectionMode: TInjectionMode);
begin
  FServiceClass := AServiceClass;
  FInstance := AInstance;
  FInjectionMode := AInjectionMode;
end;

constructor TServiceData.CreateInterface(const AServiceClass: TClass;
  const AGuid: TGuid;
  const AInterface: TValue;
  const AInjectionMode: TInjectionMode);
begin
  FServiceClass := AServiceClass;
  FGuid := AGuid;
  FInterface := AInterface;
  FInjectionMode := AInjectionMode;
end;

destructor TServiceData.Destroy;
begin
  if Assigned(FInstance) then
    FInstance.Free;
  inherited;
end;

{ TODO -oIsaque -cECLBr : Avaliar se deve usar o ECLBr para instanciar a classe }
function TServiceData._Factory(const AParams: TConstructorParams): TValue;
var
  LContext: TRttiContext;
  LTypeObject: TRttiType;
  LMetaClass: TClass;
  LConstructorMethod: TRttiMethod;
  LValue: TValue;
begin
  Result := nil;
  LContext := TRttiContext.Create;
  try
    LTypeObject := LContext.GetType(FServiceClass);
    LMetaClass := LTypeObject.AsInstance.MetaClassType;
    LConstructorMethod := LTypeObject.GetMethod('Create');
    LValue := LConstructorMethod.Invoke(LMetaClass, AParams);
    Result := LValue;
  finally
    LContext.Free;
  end;
end;

function TServiceData._FactoryInstance<T>(const AInjectorEvents: TConstructorEvents;
 const AParams: TConstructorParams): T;
var
  LResult: TValue;
  LOnCreate: TProc<T>;
  LOnParams: TFunc<TConstructorParams>;
  LResultParams: TConstructorParams;
begin
  Result := nil;
  LResultParams := [];
  if AInjectorEvents.ContainsKey(T.Classname) then
  begin
    LOnParams := TFunc<TConstructorParams>(AInjectorEvents.Items[T.ClassName].OnParams);
    if Assigned(LOnParams) then
      LResultParams := LOnParams();
  end
  else
  begin
    if Length(AParams) > 0 then
      LResultParams := AParams;
  end;
  LResult := _Factory(LResultParams);
  if not LResult.IsObjectInstance then
    Exit;
  Result := LResult.AsType<T>;
  // OnCreate
  if AInjectorEvents.ContainsKey(T.Classname) then
  begin
    LOnCreate := TProc<T>(AInjectorEvents.Items[T.ClassName].OnCreate);
    if Assigned(LOnCreate) then
      LOnCreate(Result);
  end;
end;

function TServiceData._FactoryInterface<I>(const AKey: string;
  const AInjectorEvents: TConstructorEvents;
  const AParams: TConstructorParams): TValue;
var
  LResult: TValue;
  LOnCreate: TProc<I>;
  LOnParams: TFunc<TConstructorParams>;
  LResultParams: TConstructorParams;
begin
  Result := nil;
  LResultParams := [];
  if AInjectorEvents.ContainsKey(AKey) then
  begin
    LOnParams := TFunc<TConstructorParams>(AInjectorEvents.Items[AKey].OnParams);
    if Assigned(LOnParams) then
      LResultParams := LOnParams();
  end
  else
  begin
    if Length(AParams) > 0 then
      LResultParams := AParams;
  end;
  LResult := _Factory(LResultParams);
  if not LResult.IsObjectInstance then
    Exit;
  // OnCreate
  if AInjectorEvents.ContainsKey(AKey) then
  begin
    LOnCreate := TProc<I>(AInjectorEvents.Items[AKey].OnCreate);
    if Assigned(LOnCreate) then
      LOnCreate(Result.AsType<I>);
  end;
  Result := LResult;
end;

function TServiceData.AsInstance: TObject;
begin
  Result := FInstance;
end;

function TServiceData.GetInstance<T>(const AInjectorEvents: TConstructorEvents;
  const AParams: TConstructorParams): T;
begin
  Result := nil;
  case FInjectionMode of
    imSingleton:
    begin
      if not Assigned(FInstance) then
        FInstance := _FactoryInstance<T>(AInjectorEvents, AParams);
      Result := FInstance as T;
    end;
    imFactory: Result := _FactoryInstance<T>(AInjectorEvents, AParams);
  end;
end;

function TServiceData.GetInterface<I>(const AKey: string;
  const AInjectorEvents: TConstructorEvents;
  const AParams: TConstructorParams): I;
begin
  Result := nil;
  if not FInterface.IsObjectInstance then
  begin
    try
      FInterface := _FactoryInterface<I>(AKey, AInjectorEvents, AParams);
    except
      FInterface := TValue.From(nil);
      raise;
    end;
  end;
  Result := FInterface.AsType<I>;
end;

function TServiceData.InjectionMode: TInjectionMode;
begin
  Result := FInjectionMode;
end;

function TServiceData.ServiceClass: TClass;
begin
  Result := FServiceClass;
end;

end.
