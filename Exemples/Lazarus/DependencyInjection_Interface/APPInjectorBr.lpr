program APPInjectorBr;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, app.injector, app.injector.factory, app.injector.abstract,
  sparta_Generics.Collections, sparta_Generics.Defaults, sparta_Generics.Hashes,
  sparta_Generics.Helpers, sparta_Generics.MemoryExpanders,
  sparta_Generics.Strings, base.form.view, principal.form.view,
  global.controller, global.controller.interfaces, dfe.engine.tecnospeed,
  dfe.engine.interfaces, dfe.engine.acbr, tradicional.form.view,
  injector.form.view;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormPrincipal, FormPrincipal);
  Application.Run;
end.

