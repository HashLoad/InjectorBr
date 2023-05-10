program Test_InjectorBr;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, testcase_injectorbr, app.injector,
  app.injector.abstract, app.injector.container, app.injector.events,
  app.injector.factory, app.injector.service, app.injector.lazarus;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

