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
  // Usando o Framework
  global.controller,
  app.injector;

type
  TFormInjctorBr = class(TFormBase)
    ButtonEngine: TButton;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonEngineClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FController: TGlobalController;
  public
    { Public declarations }
  end;

//var
//  FormEmitente: TFormEmitente;

implementation

{$R *.dfm}

procedure TFormInjctorBr.FormCreate(Sender: TObject);
begin
  inherited;
  FController := InjectorBr.Get<TGlobalController>;
end;

procedure TFormInjctorBr.Button1Click(Sender: TObject);
begin
  inherited;
  FController.DFeExecuteTS;
end;

procedure TFormInjctorBr.ButtonEngineClick(Sender: TObject);
begin
  inherited;
  FController.DFeExecuteACBr;
end;

end.
