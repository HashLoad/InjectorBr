# ResultPairBr Framework for Delphi

InjectorBr é um framework de injeção de dependência para aplicações Delphi. A injeção de dependência é um padrão de projeto de software que permite desacoplar componentes de uma aplicação, tornando-as mais modulares e fáceis de manter, com ele os desenvolvedores podem facilmente criar aplicações modulares e flexíveis, evitando dependências acopladas e reduzindo a complexidade do código. O framework também inclui recursos para gerenciamento de ciclo de vida dos componentes.

<p align="center">
  <a href="https://www.isaquepinheiro.com.br">
    <img src="https://www.isaquepinheiro.com.br/projetos/injectorbr-framework-for-delphi-opensource-17400.png" width="200" height="200">
  </a>
</p>

## 🏛 Delphi Versions
Embarcadero Delphi XE e superior.

## ⚙️ Instalação
Instalação usando o [`boss install`](https://github.com/HashLoad/boss) commando:
```sh
boss install "https://github.com/HashLoad/injectorbr"
```

## :hammer: Recuros de injeção de dependência com InjectorBr

:heavy_check_mark: `Recurso 1`: ```Injector.Register<TClass>``` para (Class)

:heavy_check_mark: `Recurso 2`: ```Injector.RegisterLazy<TClass>``` para (Class for LazyLoad)

:heavy_check_mark: `Recurso 3`: ```InjectorInterface<IInterface>``` para (Interface)

Além dessas três formas o framework ainda oferece o recurso de criar uma nova instância de uma classe que já esteja registras, para isso basta usar o comando:

:heavy_check_mark: ```Injector<TClass>.New``` para (Nova instância)

## ⚡️ Como usar

#### Usando com interfaces
```Delphi
{ /////////////////////// Registrando ///////////////////////// }

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

{ /////////////////////// Recuperando ///////////////////////// }

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
#### Usando com classes

```Delphi
{ /////////////////////// Registrando ///////////////////////// }

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

{ /////////////////////// Recuperando ///////////////////////// }

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

#### Usando com classe e lazyLoad

```Delphi
{ /////////////////////// Registrando ///////////////////////// }

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

{ /////////////////////// Recuperando ///////////////////////// }

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

## ⛏️ Contribuição

Nossa equipe adoraria receber contribuições para este projeto open source. Se você tiver alguma ideia ou correção de bug, sinta-se à vontade para abrir uma issue ou enviar uma pull request.

[![Issues](https://img.shields.io/badge/Issues-channel-orange)](https://github.com/HashLoad/ormbr/issues)

Para enviar uma pull request, siga estas etapas:

1. Faça um fork do projeto
2. Crie uma nova branch (`git checkout -b minha-nova-funcionalidade`)
3. Faça suas alterações e commit (`git commit -am 'Adicionando nova funcionalidade'`)
4. Faça push da branch (`git push origin minha-nova-funcionalidade`)
5. Abra uma pull request

## 📬 Contato
[![Telegram](https://img.shields.io/badge/Telegram-channel-blue)](https://t.me/hashload)

## 💲 Doação
[![Doação](https://img.shields.io/badge/PagSeguro-contribua-green)](https://pag.ae/bglQrWD)
