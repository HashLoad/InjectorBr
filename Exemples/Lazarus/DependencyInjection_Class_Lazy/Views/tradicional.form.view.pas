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

unit tradicional.form.view;

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
  // Tradicional
  global.controller;

type
  TFormTradicional = class(TFormBase)
    ButtonOpen: TButton;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonOpenClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FController: TGlobalController;
  public
    { Public declarations }
  end;

//var
//  FormConfiguracao: TFormConfiguracao;

implementation

uses
  app.injector;

{$R *.dfm}

procedure TFormTradicional.Button1Click(Sender: TObject);
begin
  inherited;
  FController.DFeExecuteTS;
end;

procedure TFormTradicional.ButtonOpenClick(Sender: TObject);
begin
  inherited;
  FController.DFeExecuteACBr;
end;

procedure TFormTradicional.FormCreate(Sender: TObject);
begin
  inherited;
  FController := TGlobalController.Create;
end;

procedure TFormTradicional.FormDestroy(Sender: TObject);
begin
  FController.Free;
  inherited;
end;

end.
