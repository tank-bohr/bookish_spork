# Chuck

Erlang API-client library for https://api.chucknorris.io/

## Run

`$ rebar3 shell`

## Usage

- Retrieve a random chuck joke

```erlang
chuck:random().
%% <<"Saddam didn't invent mustard gas, Chuck Norris ate baked beans and farted">>
```

- Retrieve a random chuck norris joke from a given category.

```erlang
chuck:random("dev").
%% <<"Chuck Norris can delete the Recycling Bin.">>
```

- Retrieve a list of available categories.

```erlang
chuck:categories().
%% [<<"explicit">>,<<"dev">>,<<"movie">>,<<"food">>, ...]
```

## Test

`rebar3 ct`
