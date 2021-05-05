module Index

open Elmish

type Todo = { Id: string; Description: string }
type Model = { DropDownOptions: Todo list; SelectedOption: Todo option }

type Msg =
    | SetDropDownOption of Todo
    | LoadedDropDown of Todo list

let dropDownOptions =
    [ { Id = "1"; Description = "Clean desk" }
      { Id = "2"; Description = "Take out trash" }
      { Id = "3"; Description = "Walk the dog" }
      { Id = "4"; Description = "Cook dinner" } ]

let loadDropDownData () = async {
    do! Async.Sleep 5000
    return dropDownOptions
}

let init () : Model * Cmd<Msg> =
    let model = { DropDownOptions = []; SelectedOption = None }
    model, Cmd.OfAsync.perform loadDropDownData () LoadedDropDown

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | SetDropDownOption id -> { model with SelectedOption = Some id }, Cmd.none
    | LoadedDropDown data -> { model with DropDownOptions = data; SelectedOption = List.tryHead data }, Cmd.none

open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Fulma

let view (model: Model) (dispatch: Msg -> unit) =
    Section.section [] [

        match model.DropDownOptions with
        | [] -> str "Loading data!"
        | _ -> ()

        div [] [
            match model.SelectedOption with
            | Some selected -> str $"Current selected value is: {selected}, since that is what the Value prop is on the select option"
            | None -> ()
        ]

        Select.select [ ] [
            select [
                match model.SelectedOption with
                | Some o -> Value o
                | None -> ()
                OnChange (fun x -> dispatch (SetDropDownOption x.target?value))
            ] [
                for row in model.DropDownOptions do
                    option [ Value row ] [ str row.Description ]
            ]
        ]
    ]