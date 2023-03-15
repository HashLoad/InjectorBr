unit principal.form.view;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ActnList,
  Buttons, StdCtrls;

type

  { TFormPrincipal }

  TFormPrincipal = class(TForm)
    ActionList1: TActionList;
    ACT_ExecuteAll: TAction;
    ACT_Home: TAction;
    ACT_Injection: TAction;
    ACT_Tradicional: TAction;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    imgMenu: TImage;
    imlIcons: TImageList;
    Label1: TLabel;
    lblTitle: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    PanelForm: TPanel;
    PanelForm1: TPanel;
    pnlToolbar: TPanel;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FormPrincipal: TFormPrincipal;

implementation

uses
  tradicional.form.view,
  injector.form.view,
  // Dependency Injection Plugin
  dfe.engine.acbr,
  dfe.engine.tecnospeed,
  dfe.engine.interfaces,
  // InjectorBr
  app.injector;

{$R *.lfm}

{ TFormPrincipal }

procedure TFormPrincipal.BitBtn1Click(Sender: TObject);
var
  LForm: TFormTradicional;
begin
  if PanelForm.ControlCount > 0 then
    PanelForm.Controls[0].Free;

  LForm := TFormTradicional.Create(PanelForm);
  LForm.Parent := PanelForm;
  LForm.Show;
end;

procedure TFormPrincipal.BitBtn2Click(Sender: TObject);
var
  LForm: TFormInjectorBr;
begin
  if PanelForm.ControlCount > 0 then
    PanelForm.Controls[0].Free;

  LForm := TFormInjectorBr.Create(PanelForm);
  LForm.Parent := PanelForm;
  LForm.Show;
end;

procedure TFormPrincipal.FormCreate(Sender: TObject);
begin
  //  InjectorBr.RegisterInterface<IDFeEngine, TDFeEngineTecnoSpeed>;
    InjectorBr.RegisterInterface<IDFeEngine, TDFeEngineACBr>;
end;

end.

