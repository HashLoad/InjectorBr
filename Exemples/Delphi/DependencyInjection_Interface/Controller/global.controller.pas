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

unit global.controller;

interface

uses
  DB,
  Rtti,
  Classes,
  SysUtils,
  Controls,
  global.controller.interfaces,
  dfe.engine.interfaces;

type
  TGlobalController = class(TInterfacedObject, IGlobalController)
  private
    FDFeEngine: IDFeEngine;
  public
    constructor Create;
    class function New: IGlobalController;
    procedure DFeExecute;
  end;

implementation

uses
  app.injector;

{ TGlobalController }

constructor TGlobalController.Create;
begin
  inherited;
  FDFeEngine := InjectorBr.GetInterface<IDFeEngine>;
end;

procedure TGlobalController.DFeExecute;
begin
  FDFeEngine.Execute;
end;

class function TGlobalController.New: IGlobalController;
begin
  Result := Self.Create;
end;

initialization
  InjectorBr.SingletonInterface<IGlobalController, TGlobalController>;

end.

