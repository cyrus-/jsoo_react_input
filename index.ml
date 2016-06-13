open React;;
open Tyxml_js;;
open Lwt.Infix;; 

let str_eq x y = match String.compare x y with 
| 0 -> true
| _ -> false

(* helper for binding an event handler using Lwt *)
module Ev = Lwt_js_events
let bind_event ev elem handler = 
  let handler evt _ = handler evt in 
  Ev.(async @@ (fun () -> ev elem handler))

(* create an input and a reactive signal tracking its 
 * string value *)
let r_input attrs = 
  let rs, rf = S.create "" in 
  let i_elt = Html5.input ~a:attrs () in 
  let i_dom = To_dom.of_input i_elt in 
  let _ = bind_event Ev.inputs i_dom (fun _ -> 
    Lwt.return @@ (rf (Js.to_string i_dom##value))) in 
  (rs, i_elt, i_dom)

let (rs, i_elt, i_dom) = (r_input [])   

let div = Html5.(
  div [
    i_elt;
    div [R.Html5.pcdata rs];
    button ~a:[
      (* gets rid of attrib if boolean signal is false *)
      R.filter_attrib 
        (a_disabled `Disabled) 
        (S.map (str_eq "") rs)
    ] [pcdata "Test"]
  ])

(* Find the element with id "body" *)
let body = Dom_html.getElementById "body" 

(* Convert hello to a DOM element and append it to body *)
let _ = Dom.appendChild body (Tyxml_js.To_dom.of_div div)


