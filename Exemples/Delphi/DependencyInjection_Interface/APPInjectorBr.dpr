program APPInjectorBr;

uses
  Forms,
  Themes,
  principal.form.view in 'Views\principal.form.view.pas' {PrincipalView},
  base.form.view in 'Views\base.form.view.pas' {FormBase},
  tradicional.form.view in 'Views\tradicional.form.view.pas' {FormTradicional},
  injection.form.view in 'Views\injection.form.view.pas' {FormInjctorBr},
  global.controller.interfaces in 'Controller\global.controller.interfaces.pas',
  global.controller in 'Controller\global.controller.pas',
  dfe.engine.acbr in 'DFe Framework\dfe.engine.acbr.pas',
  dfe.engine.interfaces in 'DFe Framework\dfe.engine.interfaces.pas',
  dfe.engine.tecnospeed in 'DFe Framework\dfe.engine.tecnospeed.pas',
  app.injector.abstract in '..\..\..\Source\app.injector.abstract.pas',
  app.injector.factory in '..\..\..\Source\app.injector.factory.pas',
  app.injector in '..\..\..\Source\app.injector.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TPrincipalView, PrincipalView);
  Application.Run;
end.
