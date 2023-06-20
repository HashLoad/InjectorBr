{
         APPInjector Brasil - Dependency Injection for Delphi


                   Copyright (c) 2023, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Versão 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos é permitido copiar e distribuir cópias deste documento de
       licença, mas mudá-lo não é permitido.

       Esta versão da GNU Lesser General Public License incorpora
       os termos e condições da versão 3 da GNU General Public License
       Licença, complementado pelas permissões adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{ @abstract(APPInjectorBr Framework)
  @created(15 Mar 2023)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @author(Site : https://www.isaquepinheiro.com.br)
}

unit app.injector.events;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Rtti,
  {$ifdef fpc}
  app.injector.lazarus,
  {$endif}
  SysUtils;

type
  TConstructorParams = TArray<TValue>;
  TConstructorCallback = TFunc<TConstructorParams>;

  TInjectorEvents = class
  private
    FOnDestroy: TProc<TObject>;
    FOnCreate: TProc<TObject>;
    FOnParams: TConstructorCallback;
    procedure _SetOnDestroy(const AOnDestroy: TProc<TObject>);
    procedure _SetOnCreate(const AOnCreate: TProc<TObject>);
    procedure _SetOnParams(const Value: TConstructorCallback);
  public
    property OnDestroy: TProc<TObject> read FOnDestroy write _SetOnDestroy;
    property OnCreate: TProc<TObject> read FOnCreate write _SetOnCreate;
    property OnParams: TConstructorCallback read FOnParams write _SetOnParams;
  end;

implementation

{ TInjectorEvents }

procedure TInjectorEvents._SetOnDestroy(const AOnDestroy: TProc<TObject>);
begin
  FOnDestroy := TProc<TObject>(AOnDestroy);
end;

procedure TInjectorEvents._SetOnParams(const Value: TConstructorCallback);
begin
  FOnParams := Value;
end;

procedure TInjectorEvents._SetOnCreate(const AOnCreate: TProc<TObject>);
begin
  FOnCreate := AOnCreate;
end;

end.
