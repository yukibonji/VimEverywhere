(*
Recently switching back to Windows after a year and half on Linux and OS X exclusively, I'm constantly annoyed by certain keys not doing what I expect!

- CTRL-H should delete the previous character. Equivalent to Backspace (everywhere!)
- CTRL-W should delete the previous word. Equivalent to CTRL+Backspace (everywhere!)

This is all that it remaps currently. I plan to add all kinds of Vim keybindings like JKHL for arrow keys, B/W to navigate by words, U for Undo, D/Y/P for Cut/Copy/Paste, etc.

It works by intercepting *all* keystrokes on the system and then using SendKeys to relay.
*)

open System
open System.Diagnostics
open System.Runtime.InteropServices
open System.Windows.Forms
 
[<Literal>]
let WH_KEYBOARD_LL = 13
 
[<StructLayout(LayoutKind.Sequential)>]
type KBDLLHOOKSTRUCT =
 val vkCode : uint32
 val scanCode : uint32
 val flags : uint32
 val time : uint32
 val dwExtraInfo : nativeint

type LowLevelKeyboardProc = delegate of int * nativeint * KBDLLHOOKSTRUCT -> nativeint
 
[<DllImport("kernel32.dll", SetLastError = true)>]
extern nativeint GetModuleHandle(string lpModuleName)
 
[<DllImport("user32.dll", SetLastError = true)>]
extern bool UnhookWindowsHookEx(nativeint hhk)
 
[<DllImport("user32.dll", SetLastError = true)>]
extern nativeint SetWindowsHookEx(int idhook, LowLevelKeyboardProc proc, nativeint hMod, uint32 threadId)
 
[<DllImport("user32.dll", SetLastError = true)>]
extern nativeint CallNextHookEx(IntPtr hhk, int nCode, IntPtr wParam, KBDLLHOOKSTRUCT lParam)
 
let SetHook (proc: LowLevelKeyboardProc) =
  use curProc = Process.GetCurrentProcess ()
  use curMod = curProc.MainModule
  SetWindowsHookEx(WH_KEYBOARD_LL, proc, GetModuleHandle(curMod.ModuleName), 0u)

type KeyEvent = Send of string | Hold | None
 
let NOT_INJECTED = 0b00010000u
let KEY_DOWN = 0b10000000u

let mutable held = []
let mutable control = false
 
type App(handler) as x =
  inherit ApplicationContext()
  let rec callback (code : int) (wparam : nativeint) (lparam : KBDLLHOOKSTRUCT) : nativeint =
    let key = int >> enum
    if lparam.flags &&& NOT_INJECTED = 0u then
      if lparam.flags &&& KEY_DOWN = 0u then
        match lparam.vkCode |> key with
        | Keys.LControlKey | Keys.RControlKey -> control <- true
        | _ -> ()
      else
        match lparam.vkCode |> key with
        | Keys.LControlKey | Keys.RControlKey -> control <- false
        | _ -> ()
    match handler code wparam lparam with
    | Send keys -> // send specific keys
      SendKeys.Send keys
      held <- []
      nativeint 1
    | Hold -> // accumulate keys while later events decide what to do with them
      held <- (hook, code, wparam, lparam) :: held
      nativeint 1
    | None -> // nothing special. allow held keys through (e.g. CTRL followed by some key we're *not* remapping)
      if held.Length > 0 then
        held |> List.rev |> List.iter (fun (h, c, w, l) -> CallNextHookEx(h, c, w, l) |> ignore)
        held <- []
      CallNextHookEx(hook, code, wparam, lparam)
  and proc = new LowLevelKeyboardProc(callback)
  and hook = SetHook proc
  override x.ExitThreadCore() =
    UnhookWindowsHookEx(hook) |> ignore
    base.ExitThreadCore()
 
printfn "KeyMagic!"

Application.Run(new App(fun code wparam lparam ->
  let key = int >> enum
  if lparam.flags &&& NOT_INJECTED = 0u && lparam.flags &&& KEY_DOWN = 0u then
    match lparam.vkCode |> key with
    | Keys.H -> if control then Send "{BS}" else None
    | Keys.W -> if control then Send "^{BS}" else None
    | Keys.LControlKey | Keys.RControlKey -> Hold
    | k ->
      if control then
        let c = lparam.vkCode |> key |> sprintf "^%A"
        if c.Length = 2 then c.ToLower() |> Send else None
      else None
    | _ -> None
  else None))