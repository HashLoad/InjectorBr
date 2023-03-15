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

unit app.injector.factory;

interface

uses Generics.Collections,
     app.injector.abstract;

type
  TInjectorFactory = class(TInjectorAbstract)
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TInjectorFactory }

constructor TInjectorFactory.Create;
begin
  FRegisterInterfaces := TDictionary<string, TClass>.Create;
  FRepository := TDictionary<string, TClass>.Create;
  FRepositoryLazy := TList<string>.Create;
  FRepositoryInterface := TDictionary<string, IInterface>.Create;
end;

destructor TInjectorFactory.Destroy;
var
  LValue: TClass;
begin
  for LValue in FRepository.Values do
    if Assigned(LValue) then
      TObject(LValue).Free;
  FRepository.Free;
  FRepositoryLazy.Free;
  FRegisterInterfaces.Clear;
  FRegisterInterfaces.Free;
  FRepositoryInterface.Clear;
  FRepositoryInterface.Free;
  inherited;
end;

end.
