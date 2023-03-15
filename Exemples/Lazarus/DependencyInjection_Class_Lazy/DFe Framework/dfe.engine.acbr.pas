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

unit dfe.engine.acbr;

interface

uses
  SysUtils;

type
  TDFeEngineACBr = class
  public
    procedure Execute;
  end;

implementation

{ TDFeEngineACBr }

procedure TDFeEngineACBr.Execute;
begin
  raise Exception.Create('DFe Engine ACBr');
end;

//initialization
//  InjectorBr.RegisterInterface<IDFeEngine, TDFeEngineACBr>;

end.
