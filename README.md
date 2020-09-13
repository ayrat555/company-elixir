# company-elixir

`company-mode` completion backend for Elixir.

It uses a separate IEx (Elixir's interactive shell) process to fetch completions from.

WARNING: This is an experimental package. It can be used as an example of an async backend for the Company completion framework - see [my post](https://www.badykov.com/emacs/2020/05/05/async-company-mode-backend/). It works but I don't use anymore.

## Installation

Add `company-elixir` to your load path:

``` emacs-lisp
(add-to-list 'load-path "path/to/company-elixir.el")
```

Add `elixir-mode` hook:

``` emacs-lisp
(add-hook 'elixir-mode-hook 'company-elixir-hook)
```

### Notes

- Since `company-elixir` uses IEx to fetch completions from, your Elixir project should be in a correct state so it can be compiled to run IEx.
- `company-elixir` finds completions for modules only by full path. For example, `Foo.Bar.` will work, `alias Foo.Bar; Bar.` won't work. See `More features` section.
- Keep in mind that EIx starts your application. So if your processes do some heavy work, you may want to disable them. `company-elixir` supports customization of iex command with `company-elixir-iex-command` variable. For example, you can modify it with your custom environment variables.

### More features

- Support of completion of aliased modules will come soon.

### Potential features

Since we already obtained `IEx` process, we can use it to:

- Fetch docs with [`b/1`](https://hexdocs.pm/iex/IEx.Helpers.html#b/1)
- Interpret Elixir expressions from Emacs
