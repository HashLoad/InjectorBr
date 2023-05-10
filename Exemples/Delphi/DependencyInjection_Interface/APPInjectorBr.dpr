program APPInjectorBr;

uses
  Forms,
  Themes,
  principal.form.view in 'Views\principal.form.view.pas' {PrincipalView},
  base.form.view in 'Views\base.form.view.pas' {FormBase},
  tradicional.form.view in 'Views\tradicional.form.view.pas' {FormTradicional},
  injection.form.view in 'Views\injection.form.view.pas' {FormInjectorBr},
  global.controller.interfaces in 'Controller\global.controller.interfaces.pas',
  global.controller in 'Controller\global.controller.pas',
  dfe.engine.acbr in 'DFe Framework\dfe.engine.acbr.pas',
  dfe.engine.interfaces in 'DFe Framework\dfe.engine.interfaces.pas',
  dfe.engine.tecnospeed in 'DFe Framework\dfe.engine.tecnospeed.pas',
  app.injector in '..\..\..\Source\app.injector.pas',
  app.injector.abstract in '..\..\..\Source\Core\app.injector.abstract.pas',
  app.injector.container in '..\..\..\Source\Core\app.injector.container.pas',
  app.injector.events in '..\..\..\Source\Core\app.injector.events.pas',
  app.injector.factory in '..\..\..\Source\Core\app.injector.factory.pas',
  app.injector.service in '..\..\..\Source\Core\app.injector.service.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TPrincipalView, PrincipalView);
  Application.Run;
end.
