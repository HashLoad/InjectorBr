{
         APPInjector Brasil - Dependency Injection for Delphi/Lazarus


                   Copyright (c) 2023, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Vers�o 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos � permitido copiar e distribuir c�pias deste documento de
       licen�a, mas mud�-lo n�o � permitido.

       Esta vers�o da GNU Lesser General Public License incorpora
       os termos e condi��es da vers�o 3 da GNU General Public License
       Licen�a, complementado pelas permiss�es adicionais listadas no
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
  dfe.engine.acbr,
  dfe.engine.tecnospeed;

type
  TGlobalController = class
  private
    FDFeEngineACBr: TDFeEngineACBr;
    FDFeEngineTS: TDFeEngineTecnoSpeed;
  public
    constructor Create;
    procedure DFeExecuteACBr;
    procedure DFeExecuteTS;
  end;

implementation

uses
  app.injector;

{ TGlobalController }

constructor TGlobalController.Create;
begin
  inherited;
  FDFeEngineACBr := InjectorBr.Get<TDFeEngineACBr>;
  FDFeEngineTS := InjectorBr.Get<TDFeEngineTecnoSpeed>;
end;

procedure TGlobalController.DFeExecuteACBr;
begin
  FDFeEngineACBr.Execute;
end;

procedure TGlobalController.DFeExecuteTS;
begin
  FDFeEngineTS.Execute;
end;

end.
