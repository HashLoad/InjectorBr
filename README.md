# InjectorBr Framework for Delphi

InjectorBr is a dependency injection framework for Delphi applications. Dependency injection is a software design pattern that allows decoupling components of an application, making them more modular and easier to maintain. With it, developers can easily create modular and flexible applications, avoiding tightly coupled dependencies and reducing code complexity. The framework also includes features for managing component lifecycle.

<p align="center">
  <a href="https://www.isaquepinheiro.com.br">
    <img src="https://www.isaquepinheiro.com.br/projetos/injectorbr-framework-for-delphi-opensource-17400.png" width="200" height="200">
  </a>
</p>

## 🏛 Delphi Versions
Embarcadero Delphi XE and higher.

## ⚙️ Install
Installation using the [`boss install`](https://github.com/HashLoad/boss) command:
```sh
boss install "https://github.com/HashLoad/injectorbr"
```

## :hammer: Dependency injection features with InjectorBr

:heavy_check_mark: `Recurso 1`: ```Injector.Register<TClass>``` to (Class)

:heavy_check_mark: `Recurso 2`: ```Injector.RegisterLazy<TClass>``` to (Class for LazyLoad)

:heavy_check_mark: `Recurso 3`: ```InjectorInterface<IInterface>``` to (Interface)

In addition to these three methods, the framework also offers the feature of creating a new instance of a class that is already registered. To do this, simply use the command:

:heavy_check_mark: ```Injector<TClass>.New``` for (New Instance)

## ⚡️ How to use

#### Using with interfaces
```Delphi
{ /////////////////////// Registering ///////////////////////// }

unit dfe.engine.acbr;

interface

uses
  SysUtils,
  dfe.engine.interfaces;

type
  TDFeEngineACBr = class(TInterfacedObject, IDFeEngine)
  public
    class function New: IDFeEngine;
    procedure Execute;
  end;

implementation

{ TDFeEngineACBr }

procedure TDFeEngineACBr.Execute;
begin
  raise Exception.Create('DFe Engine ACBr');
end;

class function TDFeEngineACBr.New: IDFeEngine;
begin
  Result := Self.Create;
end;

initialization
  InjectorBr.RegisterInterface<IDFeEngine, TDFeEngineACBr>;

end.

{ /////////////////////// Recovering ///////////////////////// }

unit global.controller;

interface

uses
  DB,
  Rtti,
  Classes,
  SysUtils,
  Controls,
  global.controller.interfaces,
  dfe.engine.interfaces;

type
  TGlobalController = class(TInterfacedObject, IGlobalController)
  private
    FDFeEngine: IDFeEngine;
  public
    constructor Create;
    procedure DFeExecute;
  end;

implementation

uses
  app.injector;

{ TGlobalController }

constructor TGlobalController.Create;
begin
  inherited;
  FDFeEngine := InjectorBr.GetInterface<IDFeEngine>;
end;

procedure TGlobalController.DFeExecute;
begin
  FDFeEngine.Execute;
end;

end.
```
#### Using with classes

```Delphi
{ /////////////////////// Registering ///////////////////////// }

unit dfe.engine.acbr;

interface

uses
  SysUtils;

type
  TDFeEngineACBr = class
  public
    procedure Execute;
  end;

implementation

{ TDFeEngineACBr }

procedure TDFeEngineACBr.Execute;
begin
  raise Exception.Create('DFe Engine ACBr');
end;

initialization
  InjectorBr.RegisterSington<TDFeEngineACBr>;

end.

{ /////////////////////// Recovering ///////////////////////// }

unit global.controller;

interface

uses
  DB,
  Rtti,
  Classes,
  SysUtils,
  Controls,
  global.controller.interfaces,
  dfe.engine.acbr;

type
  TGlobalController = class(TInterfacedObject, IGlobalController)
  private
    FDFeEngine: TDFeEngineACBr;
  public
    constructor Create;
    procedure DFeExecute;
  end;

implementation

uses
  app.injector;

{ TGlobalController }

constructor TGlobalController.Create;
begin
  inherited;
  FDFeEngine := InjectorBr.Get<TDFeEngineACBr>;
end;

procedure TGlobalController.DFeExecute;
begin
  FDFeEngine.Execute;
end;

end.
```

#### Using with class and lazyLoad

```Delphi
{ /////////////////////// Registering ///////////////////////// }

unit dfe.engine.acbr;

interface

uses
  SysUtils;

type
  TDFeEngineACBr = class
  public
    procedure Execute;
  end;

implementation

{ TDFeEngineACBr }

procedure TDFeEngineACBr.Execute;
begin
  raise Exception.Create('DFe Engine ACBr');
end;

initialization
  InjectorBr.RegisterLazy<TDFeEngineACBr>;

end.

{ /////////////////////// Recovering ///////////////////////// }

unit global.controller;

interface

uses
  DB,
  Rtti,
  Classes,
  SysUtils,
  Controls,
  global.controller.interfaces,
  dfe.engine.acbr;

type
  TGlobalController = class(TInterfacedObject, IGlobalController)
  private
    FDFeEngine: TDFeEngineACBr;
  public
    constructor Create;
    procedure DFeExecute;
  end;

implementation

uses
  app.injector;

{ TGlobalController }

constructor TGlobalController.Create;
begin
  inherited;
  FDFeEngine := InjectorBr.Get<TDFeEngineACBr>;
end;

procedure TGlobalController.DFeExecute;
begin
  FDFeEngine.Execute;
end;

end.
```

## ✍️ License
[![License](https://img.shields.io/badge/Licence-LGPL--3.0-blue.svg)](https://opensource.org/licenses/LGPL-3.0)

## ⛏️ Contribution

Our team would love to receive contributions to this open-source project. If you have any ideas or bug fixes, feel free to open an issue or submit a pull request.

[![Issues](https://img.shields.io/badge/Issues-channel-orange)](https://github.com/HashLoad/ormbr/issues)

To submit a pull request, follow these steps:

1. Fork the project.
2. Create a new branch. (`git checkout -b my-new-feature`)
3. Make your changes and commit. (`git commit -am 'Adding new functionality'`)
4. Push the branch. (`git push origin my-new-feature`)
5. Open a pull request.

## 📬 Contact
[![Telegram](https://img.shields.io/badge/Telegram-channel-blue)](https://t.me/hashload)

## 💲 Donation
[![Doação](https://img.shields.io/badge/PagSeguro-contribua-green)](https://pag.ae/bglQrWD)
