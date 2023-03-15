unit injector.form.view;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  base.form.view,
  // Usando o Framework - Dependência só da Unit da Interface
  global.controller.interfaces,
  app.injector;

type

  { TFormInjectorBr }

  TFormInjectorBr = class(TFormBase)
    ButtonEngine: TButton;
    Label1: TLabel;
    procedure ButtonEngineClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FController: IGlobalController;
  public

  end;

//var
//  FormInjectorBr: TFormInjectorBr;

implementation

uses
  dfe.engine.interfaces;

{$R *.lfm}

{ TFormInjectorBr }

procedure TFormInjectorBr.ButtonEngineClick(Sender: TObject);
begin
  FController.DFeExecute;
end;

procedure TFormInjectorBr.FormCreate(Sender: TObject);
begin
  FController := InjectorBr.GetInterface<IGlobalController>;
end;

end.

