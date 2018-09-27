[%bs.raw {|require('./TodoApp.css')|}];
/*   [@bs.module] external logo: string = "./logo.svg";*/

type item = {
  title: string,
  completed: bool,
};

/* Short for ReasonReact.string */
let str = ReasonReact.string;

let valueFromEvent = evt: string =>
  (evt |> ReactEventRe.Form.target |> ReactDOMRe.domElementToObj)##value;

module Input = {
  type state = string;
  let component = ReasonReact.reducerComponent("Input");
  let make = (~onSubmit, _children) => {
    ...component,
    initialState: () => "",
    reducer: (newText, _text) => ReasonReact.Update(newText),
    render: ({state: text, send}) =>
      <div>
        <input
          value=text
          type_="text"
          placeholder="Write something to do"
          onChange={event => send(valueFromEvent(event))}
        />
        <button
          onClick={
            _ => {
              onSubmit(text);
              send("");
            }
          }>
          {str("Add Something")}
        </button>
      </div>,
  };
};

module TodoItem = {
  let component = ReasonReact.statelessComponent("TodoItem");
  let make = (~onToggle, ~item, _children) => {
    ...component,
    render: _self =>
      <div>
        <input type_="checkbox" onClick=onToggle checked={item.completed} />
        <span className={item.completed ? "todo-done" : ""}>
          {str(item.title)}
        </span>
      </div>,
  };
};

type state = {items: list(item)};

type action =
  | AddItem(string)
  | ToggleItem(int);

let component = ReasonReact.reducerComponent("TodoApp");

let newItem = text => {title: text, completed: false};

let make = _children => {
  ...component,
  initialState: () => {
    items: [{title: "Write some things to do", completed: false}],
  },
  reducer: (action, {items}) =>
    switch (action) {
    | AddItem(text) =>
      ReasonReact.Update({items: [newItem(text), ...items]})
    | ToggleItem(id) =>
      let items =
        List.mapi(
          (idx, item) =>
            idx == id ? {...item, completed: !item.completed} : item,
          items,
        );
      ReasonReact.Update({items: items});
    },
  render: ({state: {items}, send}) => {
    let numItems = List.length(items);
    <div className="container">
      <div> {str("What to do")} </div>
      <Input onSubmit={text => send(AddItem(text))} />
      <div>
        {
          ReasonReact.array(
            Array.of_list(
              List.mapi(
                (idx, item) =>
                  <TodoItem onToggle={_e => send(ToggleItem(idx))} item />,
                items,
              ),
            ),
          )
        }
      </div>
      <div> {str(string_of_int(numItems) ++ " items")} </div>
    </div>;
  },
};