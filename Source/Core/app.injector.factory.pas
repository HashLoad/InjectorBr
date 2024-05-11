{
         AppInjector Brasil - Dependency Injection for Delphi


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

{
  @abstract(AppInjectorBr Framework)
  @created(15 Mar 2023)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @author(Site : https://www.isaquepinheiro.com.br)
}

unit app.injector.factory;

interface

uses
  Rtti,
  SysUtils,
  app.injector.service;

type
  TInjectorFactory = class
  private
    function _FactoryInternal(const Args: TArray<TValue>): TServiceData;
  public
    function FactorySingleton<T: class, constructor>(): TServiceData;
    function FactoryInterface<I: IInterface>(const AClass: TClass;
      const AGuid: TGUID): TServiceData;
    function Factory<T: class, constructor>(): TServiceData;
  end;

implementation

{ TInjectorCore }

function TInjectorFactory.Factory<T>(): TServiceData;
var
  LArgs: TArray<TValue>;
begin
  SetLength(LArgs, 3);
  LArgs[0] := TValue.From<TClass>(T);
  LArgs[1] := TValue.From<TObject>(nil);
  LArgs[2] := TValue.From<TInjectionMode>(TInjectionMode.imFactory);
  Result := _FactoryInternal(LArgs);
end;

function TInjectorFactory.FactoryInterface<I>(const AClass: TClass;
  const AGuid: TGUID): TServiceData;
var
  LArgs: TArray<TValue>;
begin
  SetLength(LArgs, 4);
  LArgs[0] := TValue.From<TClass>(AClass);
  LArgs[1] := TValue.From<TGUID>(AGuid);
  LArgs[2] := TValue.From<TValue>(TValue.Empty);
  LArgs[3] := TValue.From<TInjectionMode>(TInjectionMode.imSingleton);
  Result := _FactoryInternal(LArgs);
end;

function TInjectorFactory.FactorySingleton<T>(): TServiceData;
var
  LArgs: TArray<TValue>;
begin
  SetLength(LArgs, 3);
  LArgs[0] := TValue.From<TClass>(T);
  LArgs[1] := TValue.From<TObject>(nil);
  LArgs[2] := TValue.From<TInjectionMode>(TInjectionMode.imSingleton);
  Result := _FactoryInternal(LArgs);
end;

function TInjectorFactory._FactoryInternal(const Args: TArray<TValue>): TServiceData;
var
  LContext: TRttiContext;
  LTypeService: TRttiType;
  LConstructorMethod: TRttiMethod;
  LInstance: TValue;
begin
  LContext := TRttiContext.Create;
  try
    LTypeService := LContext.GetType(TServiceData);
    if Length(Args) = 3 then
      LConstructorMethod := LTypeService.GetMethod('Create')
    else
      LConstructorMethod := LTypeService.GetMethod('CreateInterface');
    LInstance := LConstructorMethod.Invoke(LTypeService.AsInstance.MetaClassType, Args);
    Result := TServiceData(LInstance.AsObject);
  finally
    LContext.Free;
  end;
end;

end.
