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

unit injection.form.view;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  base.form.view,
  // Usando o Framework - Dependência só da Unit da Interface
  global.controller.interfaces,
  app.injector;

type
  TFormInjectorBr = class(TFormBase)
    ButtonEngine: TButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonEngineClick(Sender: TObject);
  private
    { Private declarations }
    FController: IGlobalController;
  public
    { Public declarations }
  end;

//var
//  FormEmitente: TFormEmitente;

implementation

uses
  dfe.engine.interfaces;

{$R *.dfm}

procedure TFormInjectorBr.FormCreate(Sender: TObject);
begin
  inherited;
  FController := InjectorBr.GetInterface<IGlobalController>;
end;

procedure TFormInjectorBr.ButtonEngineClick(Sender: TObject);
begin
  inherited;
  FController.DFeExecute;
end;

end.

