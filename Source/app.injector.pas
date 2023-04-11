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

unit app.injector;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Rtti,
  TypInfo,
  SysUtils,
  Generics.Collections,
  app.injector.factory;

type
  TInjectorBr = class(TInjectorFactory)
  private
    class var FInstance: TInjectorBr;
    procedure GetLazy<T: class, constructor>();
  public
    procedure RegisterSington<T: class, constructor>(); overload;
    procedure RegisterLazy<T: class>(); overload;
    procedure RegisterInterface<I: IInterface; T: class, constructor>(); overload;
    function &Get<T: class, constructor>(): T; overload;
    function &GetInterface<I: IInterface>(): I; overload;
    function New<T: class, constructor>(): T; deprecated 'use Factory<T>() instead';
    function Factory<T: class, constructor>(): T;
  end;

function InjectorBr: TInjectorBr;

implementation

{ TInjectorBr }

function InjectorBr: TInjectorBr;
begin
  if not Assigned(TInjectorBr.FInstance) then
    TInjectorBr.FInstance := TInjectorBr.Create;
  Result := TInjectorBr.FInstance;
end;

procedure TInjectorBr.RegisterSington<T>;
var
  LContext: TRttiContext;
  LType: TRttiType;
begin
  if not FRepository.ContainsKey(T.ClassName) then
  begin
    LContext := TRttiContext.Create;
    try
      LType := LContext.GetType(T);
      FRepository.Add(T.ClassName, TClass(LType.AsInstance.MetaClassType.Create));
    finally
      LContext.Free;
    end;
  end;
end;

procedure TInjectorBr.RegisterLazy<T>;
begin
  if FRepositoryLazy.IndexOf(T.ClassName) = -1 then
    FRepositoryLazy.Add(T.ClassName);
end;

procedure TInjectorBr.RegisterInterface<I, T>;
var
  LGuid: string;
begin
  LGuid := GUIDToString(GetTypeData(TypeInfo(I)).Guid);
  if not FRegisterInterfaces.ContainsKey(LGuid) then
    FRegisterInterfaces.Add(LGuid, T);
end;

function TInjectorBr.Factory<T>: T;
begin
  if not FRepository.ContainsKey(T.ClassName) then
    raise Exception.Create('Unregistered class!');

  Result := T.Create;
end;

function TInjectorBr.Get<T>: T;
begin
  Result := nil;

  GetLazy<T>;
  if not FRepository.ContainsKey(T.ClassName) then
    Exit;

  Result := T(FRepository.Items[T.ClassName]);
end;

function TInjectorBr.GetInterface<I>: I;
var
  LContext: TRttiContext;
  LType: TRttiType;
  LMethod: TRttiMethod;
  LValue: TValue;
  LGuid: string;
  LInterface: I;
begin
  Result := nil;
  LGuid := GUIDToString(GetTypeData(TypeInfo(I)).Guid);
  if FRepositoryInterface.ContainsKey(LGuid) then
    Exit(I(FRepositoryInterface.Items[LGuid]));

  if FRegisterInterfaces.ContainsKey(LGuid) then
  begin
    LContext := TRttiContext.Create;
    try
      LType := LContext.GetType(FRegisterInterfaces.Items[LGuid]);
      LMethod := LType.GetMethod('New');
      if LMethod <> nil then
        LValue := LMethod.Invoke(LType.AsInstance.MetaClassType,[])
      else
        raise Exception.Create('Implement the method in the class "class function New: IInterfaceYourClass" with "Result := Self.Create;"');

      LInterface := I(LValue.AsInterface);
      FRepositoryInterface.Add(LGuid, LInterface);
      Result := LInterface;
    finally
      LContext.Free;
    end;
  end
  else
    raise Exception.Create('Unregistered interface!');
end;

function TInjectorBr.New<T>: T;
begin
  if not FRepository.ContainsKey(T.ClassName) then
    raise Exception.Create('Unregistered class!');

  Result := T.Create;
end;

procedure TInjectorBr.GetLazy<T>;
begin
  if not FRepository.ContainsKey(T.ClassName) then
    if (FRepositoryLazy.IndexOf(T.ClassName) > -1) then
      RegisterSington<T>;
end;

initialization

finalization
  if Assigned(TInjectorBr.FInstance) then
    TInjectorBr.FInstance.Free;

end.
