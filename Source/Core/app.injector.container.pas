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

unit app.injector.container;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  SysUtils,
  Generics.Collections,
  app.injector.factory,
  app.injector.service,
  app.injector.abstract,
  app.injector.events;

type
  TDictionaryServiceDataHelper = class helper for TDictionary<string, TServiceData>
  public
    procedure SafeRemove(const AKey: string);
  end;

  TDictionaryPairDataHelper = class helper for TDictionary<string, TPair<TClass, TGUID>>
  public
    procedure SafeRemove(const AKey: string);
  end;

  TDictionaryClassDataHelper = class helper for TDictionary<string, TClass>
  public
    procedure SafeRemove(const AKey: string);
  end;

  TDictionaryEventsDataHelper = class helper for TDictionary<string, TInjectorEvents>
  public
    procedure SafeRemove(const AKey: string);
  end;

  TInjectorContainer = class(TInjectorAbstract)
  protected
    FInjectorFactory: TInjectorFactory;
    FRepositoryReference: TDictionary<string, TClass>;
    FRepositoryInterface: TDictionary<string, TPair<TClass, TGUID>>;
    FInstances: TDictionary<string, TServiceData>;
    FInjectorEvents: TDictionary<string, TInjectorEvents>;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ TInjectorFactory }

constructor TInjectorContainer.Create;
begin
  FInjectorFactory := TInjectorFactory.Create;
  FRepositoryReference := TDictionary<string, TClass>.Create;
  FRepositoryInterface := TDictionary<string, TPair<TClass, TGUID>>.Create;
  FInjectorEvents := TObjectDictionary<string, TInjectorEvents>.Create([doOwnsValues]);
  FInstances := TObjectDictionary<string, TServiceData>.Create([doOwnsValues]);
end;

destructor TInjectorContainer.Destroy;
begin
  FRepositoryReference.Free;
  FRepositoryInterface.Free;
  FInjectorEvents.Free;
  FInjectorFactory.Free;
  FInstances.Free;
  inherited;
end;

procedure TDictionaryServiceDataHelper.SafeRemove(const AKey: string);
begin
  if Self.ContainsKey(AKey) then
    Self.Remove(AKey);
end;

procedure TDictionaryPairDataHelper.SafeRemove(const AKey: string);
begin
  if Self.ContainsKey(AKey) then
    Self.Remove(AKey);
end;

procedure TDictionaryClassDataHelper.SafeRemove(const AKey: string);
begin
  if Self.ContainsKey(AKey) then
    Self.Remove(AKey);
end;

procedure TDictionaryEventsDataHelper.SafeRemove(const AKey: string);
begin
  if Self.ContainsKey(AKey) then
    Self.Remove(AKey);
end;

end.
