module Index

open Elmish

type Todo = { Id: string; Description: string }
type Model = { DropDownOptions: Todo list; SelectedOption: string }

type Msg =
    | SetDropDownOption of id: string

let dropDownOptions =
    [ { Id = "1"; Description = "Clean desk" }
      { Id = "2"; Description = "Take out trash" }
      { Id = "3"; Description = "Walk the dog" }
      { Id = "4"; Description = "Cook dinner" } ]

let init () : Model * Cmd<Msg> =
    let model = { DropDownOptions = dropDownOptions; SelectedOption = "1"  }
    model, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | SetDropDownOption id -> { model with SelectedOption = id }, Cmd.none

open Fulma
open Fable.React.Props
open Fable.React
let view (model: Model) (dispatch: Msg -> unit) =
    Box.box' [] [
        div [] [str (sprintf "Current selected value is: %s, since that is what the Value prop is on the select option" model.SelectedOption)]
        Select.select [ ]
            [ select [ Value model.SelectedOption; OnChange (fun x  -> dispatch (SetDropDownOption x.Value)) ]
                [ for row in model.DropDownOptions do
                    option [ Value row.Id ] [ str row.Description ] ] ]
    ]


