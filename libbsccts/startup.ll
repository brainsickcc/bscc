; -*- indent-tabs-mode: nil -*-

; The entry point for programs compiled with bscc
; Copyright © 2012 Iain Nicol

; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU Affero General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU Affero General Public License for more details.
;
; You should have received a copy of the GNU Affero General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

declare x86_stdcallcc void @proc_foo() ;; FIXME: only for testing string to variant coercion
@putslog = private constant [75 x i8] c"PUTS CALLED.  IF YOU CANNOT SEE MSGBOX THEN MAYBE libvbstd ITSELF IS WRONG\00"
declare i32 @puts(i8* nocapture) nounwind

declare x86_stdcallcc void @proc_main()

; Note that clang shows us we maybe need different code for different
; targets:
;   i686-w64-mingw32: nounwindb
;   x86_64-w64-mingw32: nounwind uwtable
define i32 @main() nounwind
{
  ; FIXME: this was only so I could test string to variant coercion
  call x86_stdcallcc void @proc_foo()
  call i32 @puts(i8* getelementptr inbounds ([75 x i8]* @putslog, i32 0, i32 0))

  ; TODO: any initialization should go here.
  call x86_stdcallcc void @proc_main()
  ;; FIXME: what if no forms are loaded?
  call x86_stdcallcc void @"\01_pump@0"()
  ret i32 0
}

declare x86_stdcallcc void @"\01_pump@0"() #1
