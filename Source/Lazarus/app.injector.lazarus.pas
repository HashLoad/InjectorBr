unit app.injector.lazarus;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Rtti,
  Generics.Collections;

type
  TProc = procedure;
  TProc<T> = procedure (Arg1: T);
  TProc<T1,T2> = procedure (Arg1: T1; Arg2: T2);
  TProc<T1,T2,T3> = procedure (Arg1: T1; Arg2: T2; Arg3: T3);
  TProc<T1,T2,T3,T4> = procedure (Arg1: T1; Arg2: T2; Arg3: T3; Arg4: T4);

  TFunc<TResult> = function: TResult;
  TFunc<T,TResult> = function (Arg1: T): TResult;
  TFunc<T1,T2,TResult> = function (Arg1: T1; Arg2: T2): TResult;
  TFunc<T1,T2,T3,TResult> = function (Arg1: T1; Arg2: T2; Arg3: T3): TResult;
  TFunc<T1,T2,T3,T4,TResult> = function (Arg1: T1; Arg2: T2; Arg3: T3; Arg4: T4): TResult;

  TPredicate<T> = function (Arg1: T): Boolean;

implementation

end.

