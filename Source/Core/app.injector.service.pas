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

interface

uses
  Rtti,
  TypInfo,
  SysUtils,
  Generics.Collections,
  app.injector.events;

type
  TInjectionMode = (imSingleton, imFactory);

  TServiceData = class
  private
    FServiceClass: TClass;
    FInjectionMode: TInjectionMode;
    FInstance: TObject;
    FGuid: TGUID;
    FInterface: IInterface;
    function _FactoryInstance<T: class>(
      const AInjectorEvents: TDictionary<string, TInjectorEvents>): TObject;
    function _FactoryInterface<I: IInterface>(const AKey: string;
      const AInjectorEvents: TDictionary<string, TInjectorEvents>): IInterface;
    function _Factory: TValue;
  public
    constructor Create(const AServiceClass: TClass;
      const AInstance: TObject;
      const AInjectionMode: TInjectionMode); overload;
    constructor CreateInterface(const AServiceClass: TClass;
      const AGuid: TGUID;
      const AInterface: IInterface;
      const AInjectionMode: TInjectionMode); overload;
    destructor Destroy; override;
    property ServiceClass: TClass read FServiceClass;
    property InjectionMode: TInjectionMode read FInjectionMode;
    function GetInstance<T: class>(
      const AInjectorEvents: TDictionary<string, TInjectorEvents>): T; overload;
    function GetInstance: TObject; overload;
    function GetInterface<I: IInterface>(const AKey: string;
      const AInjectorEvents: TDictionary<string, TInjectorEvents>): IInterface;
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

constructor TServiceData.Createinterface(const AServiceClass: TClass;
  const AGuid: TGuid;
  const AInterface: IInterface;
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

function TServiceData._Factory: TValue;
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
    LValue := LConstructorMethod.Invoke(LMetaClass, []);
    Result := LValue;
  finally
    LContext.Free;
  end;
end;

function TServiceData._FactoryInstance<T>(
  const AInjectorEvents: TDictionary<string, TInjectorEvents>): TObject;
var
  LResult: TValue;
  LOnCreate: TProc<T>;
begin
  Result := nil;
  LResult := _Factory;
  if LResult.IsEmpty then
    Exit;
  Result := LResult.AsObject;
  // OnCreate
  if AInjectorEvents.ContainsKey(T.Classname) then
  begin
    LOnCreate := TProc<T>(AInjectorEvents.Items[T.ClassName].OnCreate);
    if Assigned(LOnCreate) then
      LOnCreate(Result);
  end;
end;

function TServiceData._FactoryInterface<I>(const AKey: string;
  const AInjectorEvents: TDictionary<string, TInjectorEvents>): IInterface;
var
  LResult: TValue;
  LOnCreate: TProc<I>;
begin
  Result := nil;
  LResult := _Factory;
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
  const AInjectorEvents: TDictionary<string, TInjectorEvents>): T;
begin
  Result := nil;
  case FInjectionMode of
    imSingleton:
    begin
      if not Assigned(FInstance) then
        FInstance := _FactoryInstance<T>(AInjectorEvents);
      Result := FInstance as T;
    end;
    imFactory:
    begin
      Result := _FactoryInstance<T>(AInjectorEvents) as T;
    end;
  end;
end;

function TServiceData.GetInterface<I>(const AKey: string;
  const AInjectorEvents: TDictionary<string, TInjectorEvents>): IInterface;
begin
  Result := nil;
  if not Assigned(FInterface) then
    FInterface := _FactoryInterface<I>(AKey, AInjectorEvents);
  Result := FInterface;
end;

end.
