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
  TInjectorContainer = class(TInjectorAbstract)
  protected
    FInjectorFactory: TInjectorFactory;
    FRepositoryReference: TDictionary<string, TClass>;
    FRepositoryInterface: TDictionary<string, TPair<TClass, TGUID>>;
    FInstances: TObjectDictionary<string, TServiceData>;
    FInjectorEvents: TConstructorEvents;
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
  FInstances := TObjectDictionary<string, TServiceData>.Create([doOwnsValues]);
  FInjectorEvents := TConstructorEvents.Create([doOwnsValues]);
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

end.
