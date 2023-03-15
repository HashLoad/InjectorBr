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

unit dfe.engine.tecnospeed;

interface

uses
  SysUtils,
  dfe.engine.interfaces;

type
  TDFeEngineTecnoSpeed = class(TInterfacedObject, IDFeEngine)
  public
    class function New: IDFeEngine;
    procedure Execute;
  end;

implementation

{ TDFeEngineTecnoSpeed }

procedure TDFeEngineTecnoSpeed.Execute;
begin
  raise Exception.Create('DFe Engine TecnoSpeed');
end;

class function TDFeEngineTecnoSpeed.New: IDFeEngine;
begin
  Result := Self.Create;
end;

//initialization
//  InjectorBr.RegisterInterface<IDFeEngine, TDFeEngineTecnoSpeed>;


end.
