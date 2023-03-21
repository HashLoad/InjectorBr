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
  // Usando o Framework - Depend�ncia s� da Unit da Interface
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

