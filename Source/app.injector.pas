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

unit app.injector;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Rtti,
  TypInfo,
  SysUtils,
  {$ifdef fpc}
  app.injector.lazarus,
  {$endif}
  Generics.Collections,
  app.injector.service,
  app.injector.container,
  app.injector.events;

type
  TConstructorParams = app.injector.events.TConstructorParams;

  TInjectorBr = class(TInjectorContainer)
  private class var
    FInstance: TInjectorBr;
  private
    procedure _AddEvents<T>(const AClassName: string;
      const AOnCreate: TProc<T>;
      const AOnDestroy: TProc<T>;
      const AOnConstructorParams: TConstructorCallback = nil);
  protected
    function GetTry<T: class, constructor>(const ATag: String = ''): T;
    function GetInterfaceTry<I: IInterface>(const ATag: string = ''): I;
  public
    procedure AddInjector(const ATag: String;
      const AInstance: TInjectorBr);
    procedure AddInstance<T: class>(const AInstance: TObject);
    procedure Singleton<T: class, constructor>(
      const AOnCreate: TProc<T> = nil;
      const AOnDestroy: TProc<T> = nil;
      const AOnConstructorParams: TConstructorCallback = nil);
    procedure SingletonLazy<T: class>(
      const AOnCreate: TProc<T> = nil;
      const AOnDestroy: TProc<T> = nil;
      const AOnConstructorParams: TConstructorCallback = nil);
    procedure SingletonInterface<I: IInterface; T: class, constructor>(
      const ATag: string = '';
      const AOnCreate: TProc<T> = nil;
      const AOnDestroy: TProc<T> = nil;
      const AOnConstructorParams: TConstructorCallback = nil);
    procedure Factory<T: class, constructor>(
      const AOnCreate: TProc<T> = nil;
      const AOnDestroy: TProc<T> = nil;
      const AOnConstructorParams: TConstructorCallback = nil);
    procedure Remove<T: class>(const ATag: string = '');
    function &Get<T: class, constructor>(const ATag: String = ''): T;
    function GetInterface<I: IInterface>(const ATag: string = ''): I;
    function GetInstances: TObjectDictionary<string, TServiceData>;
  end;

function AppInjectorBr: TInjectorBr;

implementation

{ TInjectorBr }

function AppInjectorBr: TInjectorBr;
begin
  if not Assigned(TInjectorBr.FInstance) then
    TInjectorBr.FInstance := TInjectorBr.Create;
  Result := TInjectorBr.FInstance;
end;

procedure TInjectorBr.Singleton<T>(const AOnCreate: TProc<T>;
  const AOnDestroy: TProc<T>;
  const AOnConstructorParams: TConstructorCallback);
var
  LValue: TServiceData;
  LResult: TObject;
  LKey: string;
begin
  LKey := T.ClassName;
  if FRepositoryReference.ContainsKey(LKey) then
    raise Exception.Create(Format('Class %s registered!', [LKey]));
  FRepositoryReference.Add(LKey, TServiceData);
  // Singleton
  LValue := FInjectorFactory.FactorySingleton<T>();
  FInstances.Add(LKey, LValue);
  // Events
  _AddEvents<T>(LKey, AOnCreate, AOnDestroy, AOnConstructorParams);
end;

procedure TInjectorBr.SingletonInterface<I, T>(const ATag: string;
  const AOnCreate: TProc<T>;
  const AOnDestroy: TProc<T>;
  const AOnConstructorParams: TConstructorCallback);
var
  LGuid: TGUID;
  LGuidstring: string;
begin
  LGuid := GetTypeData(TypeInfo(I)).Guid;
  LGuidstring := GUIDTostring(LGuid);
  if ATag <> '' then
    LGuidstring := ATag;
  if FRepositoryInterface.ContainsKey(LGuidstring) then
    raise Exception.Create(Format('Interface %s registered!', [T.ClassName]));
  FRepositoryInterface.Add(LGuidstring, TPair<TClass, TGUID>.Create(T, LGuid));
  // Events
  _AddEvents<T>(LGuidstring, AOnCreate, AOnDestroy, AOnConstructorParams);
end;

procedure TInjectorBr.SingletonLazy<T>(const AOnCreate: TProc<T>;
  const AOnDestroy: TProc<T>;
  const AOnConstructorParams: TConstructorCallback);
begin
  if FRepositoryReference.ContainsKey(T.ClassName) then
    raise Exception.Create(Format('Class %s registered!', [T.ClassName]));
  FRepositoryReference.Add(T.ClassName, TServiceData);
  // Events
  _AddEvents<T>(T.ClassName, AOnCreate, AOnDestroy, AOnConstructorParams);
end;

procedure TInjectorBr.AddInjector(const ATag: String;
  const AInstance: TInjectorBr);
var
  LValue: TServiceData;
begin
  if FRepositoryReference.ContainsKey(ATag) then
    raise Exception.Create(Format('Injector %s registered!', [ATag]));
  FRepositoryReference.Add(ATag, TServiceData);
  LValue := TServiceData.Create(TInjectorBr,
                                AInstance,
                                TInjectionMode.imSingleton);
  FInstances.Add(ATag, LValue);
end;

procedure TInjectorBr.AddInstance<T>(const AInstance: TObject);
var
  LValue: TServiceData;
begin
  if FRepositoryReference.ContainsKey(T.ClassName) then
    raise Exception.Create(Format('Instance %s registered!', [AInstance.ClassName]));
  FRepositoryReference.Add(T.ClassName, TServiceData);
  // Factory
  LValue := TServiceData.Create(T, AInstance, TInjectionMode.imSingleton);
  FInstances.Add(T.ClassName, LValue);
end;

procedure TInjectorBr.Factory<T>(const AOnCreate: TProc<T>;
  const AOnDestroy: TProc<T>;
  const AOnConstructorParams: TConstructorCallback);
var
  LValue: TServiceData;
begin
  if FRepositoryReference.ContainsKey(T.ClassName) then
    raise Exception.Create(Format('Class %s registered!', [T.ClassName]));
  FRepositoryReference.Add(T.ClassName, TServiceData);
  // Factory
  LValue := FInjectorFactory.Factory<T>();
  FInstances.Add(T.ClassName, LValue);
  // Events
  _AddEvents<T>(T.ClassName, AOnCreate, AOnDestroy, AOnConstructorParams);
end;

function TInjectorBr.Get<T>(const ATag: String): T;
var
  LValue: TServiceData;
  LTag: string;
begin
  Result := nil;
  LTag := ATag;
  if LTag = '' then
    LTag := T.ClassName;
  if not FRepositoryReference.ContainsKey(T.ClassName) then
    raise Exception.Create(Format('Class %s UnRegistered!', [LTag]));
  // Lazy
  if not FInstances.ContainsKey(T.ClassName) then
  begin
    LValue := FInjectorFactory.FactorySingleton<T>;
    FInstances.Add(T.ClassName, LValue);
  end;
  Result := FInstances.Items[T.ClassName].GetInstance<T>(FInjectorEvents);
end;

function TInjectorBr.GetInstances: TObjectDictionary<string, TServiceData>;
begin
  Result := FInstances;
end;

function TInjectorBr.GetInterface<I>(const ATag: string): I;
var
  LValue: TServiceData;
  LGuid: TGUID;
  LGuidstring: string;
  LClassParam: TClass;
  LGuidParam: TGUID;
begin
  Result := nil;
  LGuid := GetTypeData(TypeInfo(I)).Guid;
  LGuidstring := GUIDTostring(LGuid);
  if ATag <> '' then
    LGuidstring := ATag;
  if not FRepositoryInterface.ContainsKey(LGuidstring) then
    raise Exception.Create(Format('Interface %s UnRegistered!', ['']));
  // SingletonLazy
  if not FInstances.ContainsKey(LGuidstring) then
  begin
    LClassParam := FRepositoryInterface.Items[LGuidstring].Key;
    LGuidParam := FRepositoryInterface.Items[LGuidstring].Value;
    LValue := FInjectorFactory.FactoryInterface<I>(LClassParam, LGuidParam);
    FInstances.Add(LGuidstring, LValue);
  end;
  Result := I(FInstances.Items[LGuidstring].GetInterface<I>(LGuidstring, FInjectorEvents));
end;

function TInjectorBr.GetInterfaceTry<I>(const ATag: string): I;
var
  LValue: TServiceData;
  LGuid: TGUID;
  LGuidstring: string;
  LClassParam: TClass;
  LGuidParam: TGUID;
begin
  Result := nil;
  LGuid := GetTypeData(TypeInfo(I)).Guid;
  LGuidstring := GUIDTostring(LGuid);
  if ATag <> '' then
    LGuidstring := ATag;
  if not FRepositoryInterface.ContainsKey(LGuidstring) then
    Exit;
  // SingletonLazy
  if not FInstances.ContainsKey(LGuidstring) then
  begin
    LClassParam := FRepositoryInterface.Items[LGuidstring].Key;
    LGuidParam := FRepositoryInterface.Items[LGuidstring].Value;
    LValue := FInjectorFactory.FactoryInterface<I>(LClassParam, LGuidParam);
    FInstances.Add(LGuidstring, LValue);
  end;
  Result := I(FInstances.Items[LGuidstring].GetInterface<I>(LGuidstring, FInjectorEvents));
end;

function TInjectorBr.GetTry<T>(const ATag: String): T;
var
  LValue: TServiceData;
  LTag: string;
begin
  Result := nil;
  LTag := ATag;
  if LTag = '' then
    LTag := T.ClassName;
  if not FRepositoryReference.ContainsKey(LTag) then
    Exit;
  // Lazy
  if not FInstances.ContainsKey(LTag) then
  begin
    LValue := FInjectorFactory.FactorySingleton<T>;
    FInstances.Add(LTag, LValue);
  end;
  Result := FInstances.Items[LTag].GetInstance<T>(FInjectorEvents);
end;

procedure TInjectorBr.Remove<T>(const ATag: string);
var
  LTag: string;
  LOnDestroy: TProc<T>;
begin
  LTag := ATag;
  if LTag = '' then
    LTag := T.ClassName;
  // OnDestroy
  if FInjectorEvents.ContainsKey(LTag) then
  begin
    LOnDestroy := TProc<T>(FInjectorEvents.Items[LTag].OnDestroy);
    if Assigned(LOnDestroy) then
      LOnDestroy(T(FInstances.Items[LTag].GetInstance));
  end;
  if FRepositoryReference.ContainsKey(LTag) then
    FRepositoryReference.Remove(LTag);
  if FRepositoryInterface.ContainsKey(LTag) then
    FRepositoryInterface.Remove(LTag);
  if FInjectorEvents.ContainsKey(LTag) then
    FInjectorEvents.Remove(LTag);
  if FInstances.ContainsKey(LTag) then
    FInstances.Remove(LTag);
end;

procedure TInjectorBr._AddEvents<T>(const AClassName: string;
  const AOnCreate: TProc<T>;
  const AOnDestroy: TProc<T>;
  const AOnConstructorParams: TConstructorCallback);
var
  LEvents: TInjectorEvents;
begin
  if (not Assigned(AOnDestroy)) and (not Assigned(AOnCreate)) and
     (not Assigned(AOnConstructorParams)) then
    Exit;
  if FInjectorEvents.ContainsKey(AClassname) then
    Exit;
  LEvents := TInjectorEvents.Create;
  LEvents.OnDestroy := TProc<TObject>(AOnDestroy);
  LEvents.OnCreate := TProc<TObject>(AOnCreate);
  LEvents.OnParams := AOnConstructorParams;
  //
  FInjectorEvents.AddOrSetValue(AClassname, LEvents);
end;

initialization

finalization
  if Assigned(TInjectorBr.FInstance) then
    TInjectorBr.FInstance.Free;

end.
