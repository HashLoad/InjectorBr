unit tradicional.form.view;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  base.form.view,
  // Tradicional - Dependência de 2 Units
  global.controller,
  global.controller.interfaces;

type

  { TFormTradicional }

  TFormTradicional = class(TFormBase)
    ButtonOpen: TButton;
    Label1: TLabel;
    procedure ButtonOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FController: IGlobalController;
  public

  end;

//var
//  FormTradicional: TFormTradicional;

implementation

uses
  app.injector;

{$R *.lfm}

{ TFormTradicional }

procedure TFormTradicional.ButtonOpenClick(Sender: TObject);
begin
  FController.DFeExecute;
end;

procedure TFormTradicional.FormCreate(Sender: TObject);
begin
  FController := TGlobalController.New;
end;

end.

