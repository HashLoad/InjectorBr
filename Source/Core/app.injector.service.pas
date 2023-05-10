{
         APPInjector Brasil - Dependency Injection for Delphi/Lazarus


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

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Rtti,
  TypInfo,
  SysUtils,
  Generics.Collections,
  {$ifdef fpc}
  app.injector.lazarus,
  {$endif}
  app.injector.events;

type
  TInjectionMode = (imSingleton, imFactory);
  TConstructorEvents = TObjectDictionary<string, TInjectorEvents>;

  TServiceData = class
  private
    FServiceClass: TClass;
    FInjectionMode: TInjectionMode;
    FInstance: TObject;
    FGuid: TGUID;
    FInterface: IInterface;
    function _FactoryInstance<T: class>(
      AInjectorEvents: TConstructorEvents): T;
    function _FactoryInterface<I: IInterface>(AKey: string;
      AInjectorEvents: TConstructorEvents): IInterface;
    function _Factory(AParams: TConstructorParams): TValue;
  public
    constructor Create(AServiceClass: TClass;
      AInstance: TObject;
      AInjectionMode: TInjectionMode); overload;
    constructor CreateInterface(AServiceClass: TClass;
      AGuid: TGUID;
      AInterface: IInterface;
      AInjectionMode: TInjectionMode); overload;
    destructor Destroy; override;
    function ServiceClass: TClass;
    function InjectionMode: TInjectionMode;
    function GetInstance: TObject; overload;
    function GetInstance<T: class>(
      AInjectorEvents: TConstructorEvents): T; overload;
    function GetInterface<I: IInterface>(AKey: string;
      AInjectorEvents: TConstructorEvents): IInterface;
  end;

implementation

constructor TServiceData.Create(AServiceClass: TClass;
  AInstance: TObject;
  AInjectionMode: TInjectionMode);
begin
  FServiceClass := AServiceClass;
  FInstance := AInstance;
  FInjectionMode := AInjectionMode;
end;

constructor TServiceData.Createinterface(AServiceClass: TClass;
  AGuid: TGuid;
  AInterface: IInterface;
  AInjectionMode: TInjectionMode);
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
function TServiceData._Factory(AParams: TConstructorParams): TValue;
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

function TServiceData._FactoryInstance<T>(
  AInjectorEvents: TConstructorEvents): T;
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
  end;
  LResult := _Factory(LResultParams);
  if LResult.IsEmpty then
    Exit;
  Result := T(LResult.AsObject);
  // OnCreate
  if AInjectorEvents.ContainsKey(T.Classname) then
  begin
    LOnCreate := TProc<T>(AInjectorEvents.Items[T.ClassName].OnCreate);
    if Assigned(LOnCreate) then
      LOnCreate(Result);
  end;
end;

function TServiceData._FactoryInterface<I>(AKey: string;
  AInjectorEvents: TConstructorEvents): IInterface;
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
  end;
  LResult := _Factory(LResultParams);
  if LResult.IsEmpty then
    Exit;
  Result := LResult.AsInterface;
  // OnCreate
  if AInjectorEvents.ContainsKey(AKey) then
  begin
    LOnCreate := TProc<I>(AInjectorEvents.Items[AKey].OnCreate);
    if Assigned(LOnCreate) then
      LOnCreate(Result);
  end;
end;

function TServiceData.GetInstance: TObject;
begin
  Result := FInstance;
end;

function TServiceData.GetInstance<T>(
  AInjectorEvents: TConstructorEvents): T;
begin
  Result := nil;
  case FInjectionMode of
    imSingleton:
    begin
      if not Assigned(FInstance) then
        FInstance := _FactoryInstance<T>(AInjectorEvents);
      Result := T(FInstance);
    end;
    imFactory:
    begin
      Result := _FactoryInstance<T>(AInjectorEvents);
    end;
  end;
end;

function TServiceData.GetInterface<I>(AKey: string;
  AInjectorEvents: TConstructorEvents): IInterface;
begin
  Result := nil;
  if not Assigned(FInterface) then
    FInterface := _FactoryInterface<I>(AKey, AInjectorEvents);
  Result := FInterface;
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
