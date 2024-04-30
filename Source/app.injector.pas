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

  PInjectorBr = ^TInjectorBr;
  TInjectorBr = class(TInjectorContainer)
  strict private
    procedure _AddEvents<T>(const AClassName: String;
      const AOnCreate: TProc<T>;
      const AOnDestroy: TProc<T>;
      const AOnConstructorParams: TConstructorCallback = nil);
    function _ResolverInterfaceType(const AHandle: PTypeInfo;
      const AGUID: TGUID): TValue;
    function _ResolverParams(const AClass: TClass): TConstructorParams; overload;
  protected
    function GetTry<T: class, constructor>(const ATag: String = ''): T;
    function GetInterfaceTry<I: IInterface>(const ATag: String = ''): I;
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
      const ATag: String = '';
      const AOnCreate: TProc<T> = nil;
      const AOnDestroy: TProc<T> = nil;
      const AOnConstructorParams: TConstructorCallback = nil);
    procedure Factory<T: class, constructor>(
      const AOnCreate: TProc<T> = nil;
      const AOnDestroy: TProc<T> = nil;
      const AOnConstructorParams: TConstructorCallback = nil);
    procedure Remove<T: class>(const ATag: String = '');
    function GetInstances: TObjectDictionary<String, TServiceData>;
    function Get<T: class, constructor>(const ATag: String = ''): T;
    function GetInterface<I: IInterface>(const ATag: String = ''): I;
  end;

function Injector: TInjectorBr;

var
  InjectorBr: PInjectorBr = nil;

implementation

{ TInjectorBr }

function Injector: TInjectorBr;
begin
  Result := InjectorBr^;
end;

procedure TInjectorBr.Singleton<T>(const AOnCreate: TProc<T>;
  const AOnDestroy: TProc<T>;
  const AOnConstructorParams: TConstructorCallback);
var
  LValue: TServiceData;
  LKey: String;
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

procedure TInjectorBr.SingletonInterface<I, T>(const ATag: String;
  const AOnCreate: TProc<T>;
  const AOnDestroy: TProc<T>;
  const AOnConstructorParams: TConstructorCallback);
var
  LGuid: TGUID;
  LGuidString: String;
begin
  LGuid := GetTypeData(TypeInfo(I)).Guid;
  LGuidString := GUIDToString(LGuid);
  if ATag <> '' then
    LGuidString := ATag;
  if FRepositoryInterface.ContainsKey(LGuidString) then
    raise Exception.Create(Format('Interface %s registered!', [T.ClassName]));
  FRepositoryInterface.Add(LGuidString, TPair<TClass, TGUID>.Create(T, LGuid));
  // Events
  _AddEvents<T>(LGuidString, AOnCreate, AOnDestroy, AOnConstructorParams);
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

function TInjectorBr.GetInstances: TObjectDictionary<String, TServiceData>;
begin
  Result := FInstances;
end;

function TInjectorBr.Get<T>(const ATag: String): T;
var
  LItem: TServiceData;
begin
  Result := GetTry<T>(ATag);
  if Result <> nil then
    Exit;
  for LItem in GetInstances.Values do
  begin
    if LItem.AsInstance is TInjectorBr then
    begin
      Result := TInjectorBr(LItem.AsInstance).GetTry<T>(ATag);
      if Result <> nil then
        Exit;
    end;
  end;
end;

function TInjectorBr.GetInterface<I>(const ATag: String): I;
var
  LItem: TServiceData;
begin
  Result := GetInterfaceTry<I>(ATag);
  if Result <> nil then
    Exit;
  for LItem in GetInstances.Values do
  begin
    if LItem.AsInstance is TInjectorBr then
    begin
      Result := TInjectorBr(LItem.AsInstance).GetInterfaceTry<I>(ATag);
      if Result <> nil then
        Exit;
    end;
  end;
end;

function TInjectorBr.GetTry<T>(const ATag: String): T;
var
  LValue: TServiceData;
  LParams: TConstructorParams;
  LTag: String;
begin
  Result := nil;
  LTag := ATag;
  if LTag = '' then
    LTag := T.ClassName;
  if not FRepositoryReference.ContainsKey(LTag) then
    Exit;
  // Lazy
  LParams := [];
  if not FInstances.ContainsKey(LTag) then
  begin
    LValue := FInjectorFactory.FactorySingleton<T>;
    FInstances.Add(LTag, LValue);
  end;
  if (FInstances.Items[LTag].AsInstance = nil) and (FInjectorEvents.Count = 0) then
    LParams := _ResolverParams(FInstances.Items[LTag].ServiceClass);
  Result := FInstances.Items[LTag].GetInstance<T>(FInjectorEvents, LParams);
end;

function TInjectorBr.GetInterfaceTry<I>(const ATag: String): I;
var
  LServiceData: TServiceData;
  LParams: TConstructorParams;
  LGuid: TGUID;
  LGuidString: String;
  LKey: TClass;
  LValue: TGUID;
begin
  Result := nil;
  LGuid := GetTypeData(TypeInfo(I)).Guid;
  LGuidString := GUIDToString(LGuid);
  if ATag <> '' then
    LGuidString := ATag;
  if not FRepositoryInterface.ContainsKey(LGuidString) then
    Exit;
  // SingletonLazy
  LParams := [];
  if not FInstances.ContainsKey(LGuidString) then
  begin
    LKey := FRepositoryInterface.Items[LGuidString].Key;
    LValue := FRepositoryInterface.Items[LGuidString].Value;
    LServiceData := FInjectorFactory.FactoryInterface<I>(LKey, LValue);
    FInstances.Add(LGuidString, LServiceData);
  end;
  if (FInstances.Items[LGuidString].AsInstance = nil) and (FInjectorEvents.Count = 0) then
    LParams := _ResolverParams(FInstances.Items[LGuidString].ServiceClass);
  Result := FInstances.Items[LGuidString].GetInterface<I>(LGuidString, FInjectorEvents, LParams);
end;

procedure TInjectorBr.Remove<T>(const ATag: String);
var
  LTag: String;
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
      LOnDestroy(T(FInstances.Items[LTag].AsInstance));
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

procedure TInjectorBr._AddEvents<T>(const AClassName: String;
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

function TInjectorBr._ResolverInterfaceType(const AHandle: PTypeInfo;
  const AGUID: TGUID): TValue;
var
  LValue: TValue;
  LResult: TValue;
  LInterface: IInterface;
begin
  Result := TValue.From(nil);
  LValue := TValue.From(GetInterface<IInterface>(GUIDToString(AGUID)));
  if Supports(LValue.AsInterface, AGUID, LInterface) then
  begin
    TValue.Make(@LInterface, AHandle, LResult);
    Result := LResult;
  end;
end;

function TInjectorBr._ResolverParams(const AClass: TClass): TConstructorParams;

  function ToStringParams(const AValues: TArray<TValue>): String;
  var
    LIndex: Integer;
  begin
    Result := '';
    for LIndex := 0 to High(AValues) do
    begin
      Result := Result + AValues[LIndex].ToString;
      if LIndex < High(AValues) then
        Result := Result + ', ';
    end;
  end;

var
  LRttiContext: TRttiContext;
  LRttiType: TRttiType;
  LRttiMethod: TRttiMethod;
  LParameter: TRttiParameter;
  LParameterType: TRttiType;
  LInterfaceType: TRttiInterfaceType;
  LParameters: TArray<TRttiParameter>;
  LParameterValues: TArray<TValue>;
  LFor: integer;
begin
  Result := [];
  LRttiContext := TRttiContext.Create;
  try
    LRttiType := LRttiContext.GetType(AClass);
    if not Assigned(LRttiType) then
      exit;
    LRttiMethod := LRttiType.GetMethod('Create');
    LParameters := LRttiMethod.GetParameters;
    SetLength(LParameterValues, Length(LParameters));
    try
      for LFor := 0 to High(LParameters) do
      begin
        LParameter := LParameters[LFor];
        LParameterType := LParameter.ParamType;
        case LParameterType.TypeKind of
          tkClass, tkClassRef:
          begin
            LParameterValues[LFor] := TValue.From(Get<TObject>(String(LParameterType.Handle.Name)))
                                            .Cast(LParameterType.Handle);
          end;
          tkInterface:
          begin
            LInterfaceType := LRttiContext.GetType(LParameterType.Handle) as TRttiInterfaceType;
            LParameterValues[LFor] := _ResolverInterfaceType(LParameterType.Handle,
                                                             LInterfaceType.GUID);
          end;
          else
            LParameterValues[LFor] := TValue.From(nil);
        end;
      end;
    except
      on E: Exception do
        raise Exception.Create(E.Message + ' => ' + ToStringParams(LParameterValues));
    end;
    Result := LParameterValues;
  finally
    LRttiContext.Free;
  end;
end;

initialization
  New(InjectorBr);
  InjectorBr^ := TInjectorBr.Create;

finalization
  if Assigned(InjectorBr) then
  begin
    InjectorBr^.Free;
    Dispose(InjectorBr);
  end;

end.


