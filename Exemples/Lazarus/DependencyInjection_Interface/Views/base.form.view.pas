unit base.form.view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TFormBase }

  TFormBase = class(TForm)
    ButtonClose: TButton;
    LabelTitle: TLabel;
    PanelTitle: TPanel;
    procedure ButtonCloseClick(Sender: TObject);
  private

  public

  end;

//var
//  FormBase: TFormBase;

implementation

{$R *.lfm}

{ TFormBase }

procedure TFormBase.ButtonCloseClick(Sender: TObject);
begin
  Self.Close;
end;

end.
