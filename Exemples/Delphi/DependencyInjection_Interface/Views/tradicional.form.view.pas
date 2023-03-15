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
  // Tradicional - Dependência de 2 Units
  global.controller,
  global.controller.interfaces;

type
  TFormTradicional = class(TFormBase)
    ButtonOpen: TButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonOpenClick(Sender: TObject);
  private
    { Private declarations }
    FController: IGlobalController;
  public
    { Public declarations }
  end;

//var
//  FormConfiguracao: TFormConfiguracao;

implementation

uses
  app.injector;

{$R *.dfm}

procedure TFormTradicional.ButtonOpenClick(Sender: TObject);
begin
  inherited;
  FController.DFeExecute;
end;

procedure TFormTradicional.FormCreate(Sender: TObject);
begin
  inherited;
  FController := TGlobalController.New;
end;

end.
