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

unit principal.form.view;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  ImageList,
  Actions,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  WinXCtrls,
  StdCtrls,
  CategoryButtons,
  Buttons,
  ImgList,
  ComCtrls,
  ActnList,
  pngimage;

type
  TPrincipalView = class(TForm)
    pnlToolbar: TPanel;
    PanelForm: TPanel;
    SV: TSplitView;
    catMenuItems: TCategoryButtons;
    imlIcons: TImageList;
    imgMenu: TImage;
    ActionList1: TActionList;
    ACT_Home: TAction;
    lblTitle: TLabel;
    ACT_Tradicional: TAction;
    ACT_Injection: TAction;
    ACT_ExecuteAll: TAction;
    Label1: TLabel;
    procedure ACT_TradicionalExecute(Sender: TObject);
    procedure ACT_InjectionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  PrincipalView: TPrincipalView;

implementation

uses
  tradicional.form.view,
  injection.form.view,
  // Dependency Injection Plugin
  dfe.engine.acbr,
  dfe.engine.tecnospeed,
  // InjectorBr
  app.injector;

{$R *.dfm}

procedure TPrincipalView.ACT_InjectionExecute(Sender: TObject);
var
  LForm: TFormInjctorBr;
begin
  if PanelForm.ControlCount > 0 then
    PanelForm.Controls[0].Free;

  LForm := TFormInjctorBr.Create(PanelForm);
  LForm.Parent := PanelForm;
  LForm.Show;
end;

procedure TPrincipalView.ACT_TradicionalExecute(Sender: TObject);
var
  LForm: TFormTradicional;
begin
  if PanelForm.ControlCount > 0 then
    PanelForm.Controls[0].Free;

  LForm := TFormTradicional.Create(PanelForm);
  LForm.Parent := PanelForm;
  LForm.Show;
end;

procedure TPrincipalView.FormCreate(Sender: TObject);
begin
  InjectorBr.RegisterSington<TDFeEngineTecnoSpeed>;
  InjectorBr.RegisterSington<TDFeEngineACBr>;
end;

initialization
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;

end.
