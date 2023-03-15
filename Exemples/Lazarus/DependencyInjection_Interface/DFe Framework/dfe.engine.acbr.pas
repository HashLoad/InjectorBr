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

unit dfe.engine.acbr;

interface

uses
  SysUtils,
  dfe.engine.interfaces;

type
  TDFeEngineACBr = class(TInterfacedObject, IDFeEngine)
  public
    class function New: IDFeEngine;
    procedure Execute;
  end;

implementation

{ TDFeEngineACBr }

procedure TDFeEngineACBr.Execute;
begin
  raise Exception.Create('DFe Engine ACBr');
end;

class function TDFeEngineACBr.New: IDFeEngine;
begin
  Result := Self.Create;
end;

//initialization
//  InjectorBr.RegisterInterface<IDFeEngine, TDFeEngineACBr>;

end.
