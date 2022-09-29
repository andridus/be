defmodule Be.View do
  @moduledoc false

  alias Phoenix.LiveView

  defmacro __using__(opts) do
    quote do
      use Phoenix.LiveView, unquote(opts)

      import Phoenix.LiveView.Helpers
      import Be.View

      Module.register_attribute(__MODULE__, :file_path, [])
      Module.register_attribute(__MODULE__, :npm_import, accumulate: true)
      Module.register_attribute(__MODULE__, :fields, accumulate: true)
      Module.register_attribute(__MODULE__, :events, accumulate: true)
      Module.register_attribute(__MODULE__, :effects, accumulate: true)
      Module.register_attribute(__MODULE__, :javascripts, accumulate: true)
      Module.register_attribute(__MODULE__, :commands, accumulate: true)
      @before_compile Be.View
    end
  end

  defmacro __before_compile__(env) do
    [init_component(env)]
  end

  defp init_component(_env) do
    prelude =
      quote do
        @doc false
        def mount(params, session, socket) do
          state =
            Map.merge(
              %{__params__: params, __session__: session["_csrf_token"]},
              parse_fields(@fields)
            )

          socket =
            socket
            |> assign(:__fields__, @fields)
            |> assign(:__params__, params)
            |> assign(:__session__, session)
            |> assign(:__module__, __MODULE__)
            |> assign(:run_once, false)
            |> apply_default_fields()
            |> apply_effects(@effects)

          {:ok,
           socket
           |> apply_command({:javascript, {:__main__, [state]}}, @commands, @javascripts)
           |> apply_command({:javascript, {:__svelte__, [state]}}, @commands, @javascripts)}
        end
      end

    postlude =
      quote do
        @doc """
          Get the URI from socket, and let available on assigns
        """
        def handle_params(params, uri, socket) do
          path = uri |> get_paths(params)
          {:noreply, assign(socket, __path__: path)}
        end

        @doc false
        defp handle_event_with_params(event, params, socket) do
          event = if is_atom(event), do: event, else: String.to_atom(event)
          func = Keyword.get(@events, event)

          case is_function(func, 2) && func.(socket.assigns, params) do
            {data, command} ->
              ## clean cant change assigns
              ## Apply commands on data
              socket =
                data
                |> clean_assigns()
                |> then(&assign(socket, &1))
                |> apply_command(command, @commands, @javascripts)

              {clean_assigns(socket.assigns), socket}

            data when is_map(data) ->
              {clean_assigns(data), socket}

            false ->
              {clean_assigns(socket.assigns), socket}
          end
        end

        def handle_event(event, params, socket) do
          {assigns, socket} = handle_event_with_params(event, params, socket)
          {:noreply, assign(socket, assigns)}
        end

        def handle_info({event, params}, socket) do
          opts = params[:__opts__] || []
          func = Keyword.get(@events, event)
          {assigns, socket} = handle_event_with_params(event, params, socket)

          if opts[:effect] == true && opts[:every] do
            Process.send_after(self(), {event, __opts__: opts}, opts[:every])
          end

          {:noreply, assign(socket, assigns)}
        end

        def handle_params(_handled, _params, socket) do
          {:noreply, socket}
        end

        @doc """
          Get events went by js
        """
        def handle_info(%{event: event, payload: payload}, socket) do
          Regex.run(~r/\[(.*)\]\[(.*)\]\[(.*)\]/, event)
          |> case do
            [_, module, "undefined", atom] ->
              module = module |> String.split("_") |> Module.concat()
              Process.send(self(), {String.to_atom(atom), payload}, [])

            [_, module, id, atom] ->
              module
              |> String.split("_")
              |> Module.concat()
              |> LiveView.send_update(
                id: id,
                __port__: String.to_atom(atom),
                __payload__: payload
              )

            _ ->
              nil
          end

          {:noreply, socket}
        end
      end

    quote do
      unquote(prelude)
      unquote(postlude)
    end
  end

  def call_event(event, args \\ %{}) do
    send(self(), {event, args})
  end

  def clean_assigns(assigns) do
    assigns |> Map.drop([:flash, :__event__, :__opts__])
  end

  @doc """
    Macro only to use a JS string highlight
  """
  defmacro sigil_J({:<<>>, meta, [template]}, []) do
    ast = EEx.compile_string(template, line: meta[:line] + 1)

    quote line: meta[:line] do
      unquote(ast)
    end
  end

  def parse_fields(fields) do
    for {key, _field, opts} <- fields, into: %{}, do: {key, opts[:default]}
  end

  def header(npm_imports \\ []) do
    """
    #{Enum.map(npm_imports, &("import " <> &1 <> ";\n"))}
    \n
    /*
    This file was generated automatically don't change it manually.
    */\
    """
  end

  def header_functions(prefix) do
    """
    \n
    const SEND = p => typeof p == 'object' ? window.__live_push__({module: '#{prefix}', ...p}) : null;
    const RECEIVE = (event, p) => addEventListener(`phx:#{prefix}:${event}`, e => p(e.detail, e));
    """
  end

  def parse_prefix(module), do: String.split("#{module}", ".") |> Enum.join("_")

  defmacro script(name, opts) do
    content =
      if opts[:file] do
        dir = Path.dirname(__CALLER__.file)
        file = Path.join([dir, opts[:file] <> ".js"])

        file
        |> File.read()
        |> case do
          {:ok, content} ->
            content

          _ ->
            IO.warn("the '#{dir}' not have a js file with the filename '#{Path.basename(file)}'")
            nil
        end
      else
        opts[:do]
      end

    {name, args} =
      case name do
        {_name, _args} = n -> n
        :__main__ -> {:__main__, [:state]}
        name -> {name, []}
      end

    quote do
      name = unquote(name)
      cnt = unquote(content)
      prefix = parse_prefix(__MODULE__)
      hash = "#{prefix}_#{name}"

      js_output_dir =
        Path.join([Application.get_env(:be, :assets_path, File.cwd!()), "assets/js/be/"])

      File.mkdir_p!(js_output_dir)
      dest_file = Path.join([js_output_dir, "#{hash}.js"])
      index_file = Path.join([js_output_dir, "index.js"])
      npm_imports = Module.get_attribute(__MODULE__, :npm_import) |> Enum.reverse()

      content = [
        header(npm_imports),
        header_functions(prefix),
        "export default function(e){\n const params = e.detail;\n",
        cnt,
        "}"
      ]

      delimiter_start = "/****** #{prefix}_#{name} START****/"
      delimiter_end = "/****** #{prefix}_#{name} END****/"

      content_index = [
        delimiter_start,
        "import #{prefix}_#{name} from './#{hash}';",
        "window.addEventListener(`phx:#{hash}`, #{prefix}_#{name});",
        delimiter_end
      ]

      if !File.exists?(index_file), do: File.touch!(index_file)
      src = File.read!(index_file)
      c = content_index |> Enum.join()
      c = String.replace(src, c, "")

      String.splitter(src, [delimiter_start, delimiter_end])
      |> Enum.take(3)
      |> case do
        [h, c, e] ->
          File.write!(index_file, [h, content_index, e] |> List.flatten())

        _ ->
          File.write!(index_file, [c, content_index, "\n"] |> List.flatten())
      end

      File.write!(dest_file, content)
      Module.put_attribute(__MODULE__, :javascripts, {name, "#{hash}", unquote(args)})
    end
  end

  defmacro svelte(dom_id, opts \\ []) do
    {content, file} =
      case opts[:do] do
        nil ->
          file = String.replace(__CALLER__.file, ".ex", ".svelte")

          file
          |> File.read()
          |> case do
            {:ok, content} ->
              {content, file}

            _ ->
              IO.warn(
                "the '#{Path.basename(__CALLER__.file)}' that have a 'svelte' function needs have a content into function or to be placed into a '.svelte' file with the filename '#{Path.basename(file)}'"
              )

              {nil, nil}
          end

        content ->
          {content, nil}
      end

    quote do
      cnt = unquote(content)
      file = unquote(file)

      if cnt do
        name = "__svelte__"
        dom = unquote(dom_id)
        prefix = String.split("#{__MODULE__}", ".") |> Enum.join("_")
        hash = "#{prefix}_svelte"
        js_output_dir = Path.join([File.cwd!(), "assets/js/be"])
        File.mkdir_p!(js_output_dir)
        dest_file = Path.join([js_output_dir, "#{hash}.svelte"])
        index_file = Path.join([js_output_dir, "index.js"])
        delimiter_start = "/****** #{prefix}_#{name} START****/"
        delimiter_end = "/****** #{prefix}_#{name} END****/"

        content_index = [
          delimiter_start,
          "import #{prefix}_#{name} from './#{hash}.svelte';",
          "window.addEventListener(`phx:#{hash}`, function(e){new #{prefix}_#{name}({target: document.querySelector('##{dom}'), props: e.detail?.state});});",
          delimiter_end
        ]

        if !File.exists?(index_file), do: File.touch!(index_file)
        src = File.read!(index_file)
        c = content_index |> Enum.join()
        c = String.replace(src, c, "")

        String.splitter(src, [delimiter_start, delimiter_end])
        |> Enum.take(3)
        |> case do
          [h, c, e] ->
            File.write!(index_file, [h, content_index, e] |> List.flatten())

          _ ->
            File.write!(index_file, [c, content_index, "\n"] |> List.flatten())
        end

        String.splitter(cnt, ["<script>"])
        |> Enum.take(2)
        |> case do
          [h, e] ->
            File.write!(dest_file, ["<script>", header_functions(prefix), e] |> List.flatten())

          _ ->
            File.write!(dest_file, [cnt] |> List.flatten())
        end

        Module.put_attribute(__MODULE__, :javascripts, {:__svelte__, "#{hash}", [:state]})
      end
    end
  end

  defmacro event(mfld, opts \\ []) do
    funct_0 = {:&, [], [{:/, [], [{{:., [], [{:__MODULE__, [], nil}, mfld]}, [], []}, 2]}]}
    funct = opts[:do] || funct_0

    quote do
      Module.put_attribute(__MODULE__, :events, {unquote(mfld), unquote(funct)})
    end
  end

  defmacro view(page \\ nil, opts \\ [], do: block) do
    block =
      block
      |> Macro.prewalk(fn
        {:<<>>, x, args} -> {:<<>>, x, parse_heex(args)}
        c -> c
      end)

    pattern =
      opts
      |> Code.eval_quoted()
      |> case do
        {[], _} -> if is_nil(page), do: %{}, else: %{live_action: page}
        {opts, []} -> Map.merge(%{live_action: page}, opts)
      end
      |> Macro.escape()

    quote do
      def render(unquote(pattern) = var!(assigns)) do
        unquote(block)
      end
    end
  end

  @doc """
    command :command, opts
    command {:command, args}, opts (when args is a list of atom)
  """
  defmacro command(cmd, opts \\ []) do
    {atom, args, total_args} =
      case cmd do
        {atom, args} -> {atom, args, 2}
        atom -> {atom, [], 1}
      end

    funct = {:&, [], [{:/, [], [{{:., [], [{:__MODULE__, [], nil}, atom]}, [], []}, total_args]}]}
    opts = Keyword.put_new(opts, :do, funct)
    cmd = {atom, args}

    quote do
      Module.put_attribute(__MODULE__, :commands, {unquote(cmd), unquote(opts)})
    end
  end

  @doc """
    effect :event, opts
  """
  defmacro effect(event, opts \\ []) do
    quote do
      Module.put_attribute(__MODULE__, :effects, {unquote(event), unquote(opts)})
    end
  end

  @doc """
    field :field, :type,
          default: "value" || function,
          format: function
  """
  defmacro field(fld, type, opts) do
    quote do
      Module.put_attribute(__MODULE__, :fields, {unquote(fld), unquote(type), unquote(opts)})
    end
  end

  def apply_default_fields(socket) do
    fields = socket.assigns[:__fields__]
    apply_default_fields(socket, fields)
  end

  def apply_default_fields(socket, []), do: socket

  def apply_default_fields(socket, [{fld, type, opts} | tail]) do
    socket
    |> apply_fields_private({fld, type, opts})
    |> apply_default_fields(tail)
  end

  def apply_fields(socket) do
    fields = socket.assigns[:__fields__]
    apply_fields(socket, fields)
  end

  def apply_fields(socket, []), do: socket

  def apply_fields(socket, [{fld, type, opts} | tail]) do
    apply_fields_private(socket, {fld, type, opts})
    |> apply_default_fields(tail)
  end

  def apply_fields_private(socket, {fld, _type, opts}) do
    default = opts[:default]

    value =
      cond do
        is_nil(default) -> opts[:value]
        :else -> default
      end

    format = opts[:format]
    # GET VALUE
    value =
      cond do
        is_function(value, 0) -> value.()
        is_function(value, 1) -> value.(socket)
        is_function(value) -> value
        :else -> value
      end

    # FORMAT VALUE
    value = if is_function(format), do: format.(value), else: value

    socket
    |> LiveView.assign(fld, value)
  end

  def apply_fields_private(socket, _), do: socket

  def merge_differences(socket, assigns) do
    assigns =
      Map.to_list(assigns)
      |> Enum.map(fn {key, value} -> {key, :field, value: value} end)

    apply_default_fields(socket, assigns)
  end

  def apply_command(_socket, _list, _commands, _javascripts \\ [])
  def apply_command(socket, [], _commands, _javascripts), do: socket

  def apply_command(socket, [command | list], commands, javascripts) do
    socket
    |> apply_command(command, commands, javascripts)
    |> apply_command(list, commands, javascripts)
  end

  def apply_command(socket, {:javascript, atom}, _commands, javascripts) do
    {key, args} =
      case atom do
        {key, args} -> {key, args}
        key -> {key, []}
      end

    Enum.find(javascripts, fn
      {c, _, _args} -> c == key
      _ -> false
    end)
    |> case do
      nil ->
        socket

      {_, name, args1} ->
        args = Enum.zip(args1, args) |> Map.new()
        LiveView.push_event(socket, name, args)
    end
  end

  def apply_command(socket, {:push_event, event}, _c, _j) do
    module = parse_prefix(socket.assigns.__module__)
    LiveView.push_event(socket, "#{module}:#{event}", %{})
  end

  def apply_command(socket, {:push_event, event, params}, _c, _j) do
    module = parse_prefix(socket.assigns.__module__)
    LiveView.push_event(socket, "#{module}:#{event}", %{data: params})
  end

  def apply_command(socket, {:event, evt}, _commands, _javascripts) do
    module = socket.assigns.__module__
    call_event_priv(socket, %{id: socket.assigns[:id], module: module, event: evt, opts: []})
  end

  def apply_command(socket, {command, args}, commands, _javascripts) do
    Enum.find(commands, fn
      {{c, _args}, _} -> c == command
      _ -> false
    end)
    |> case do
      {{_command, args_keys}, [do: function]} ->
        function.(socket, Enum.zip(args_keys, args) |> Map.new())

      nil ->
        socket
    end
  end

  def apply_command(socket, command, commands, _javascripts) when is_atom(command) do
    Enum.find(commands, fn
      {{c, []}, _} -> c == command
      _ -> false
    end)
    |> case do
      {_command, [do: function]} ->
        function.(socket)

      nil ->
        socket
    end
  end

  def apply_effects(socket, []), do: socket

  def apply_effects(socket, [sub | tail]) do
    run_effect(socket, sub)
    apply_effects(socket, tail)
  end

  def run_effect(%{assigns: %{run_once: false}} = socket, {event, opts}) do
    module = socket.assigns.__module__
    call_event_priv(socket, %{id: socket.assigns[:id], module: module, event: event, opts: opts})
  end

  def run_effect(socket, _), do: socket

  defp call_event_priv(socket, %{id: nil, module: _module, event: event, opts: opts}) do
    case opts[:every] do
      nil ->
        Process.send_after(self(), {event, __opts__: opts}, 0)

      sec ->
        opts = opts ++ [effect: true]
        Process.send_after(self(), {event, __opts__: opts}, sec)
    end

    socket
  end

  defp call_event_priv(socket, %{id: id, module: module, event: event, opts: opts}) do
    case opts[:every] do
      nil ->
        LiveView.send_update(self(), module, id: id, __event__: event)

      sec ->
        opts = opts ++ [effect: true]

        LiveView.send_update_after(
          self(),
          module,
          [id: id, __event__: event, __opts__: opts],
          sec
        )
    end

    socket
  end

  def get_opts(fields, field, key_opt, default \\ nil) do
    Enum.find(fields, fn {key, _, _} -> key == field end)
    |> case do
      nil -> "[no-schema-field]"
      {_key, _type, opts} -> opts[key_opt] || default
    end
    |> maybe_parse_function()
  end

  def maybe_parse_function({module, function, args}) do
    apply(:"#{module}.Api", function, args)
  end

  def maybe_parse_function(result), do: result

  def parse_heex([content]) do
    # replace $() to get_opts() function
    [String.replace(content, "$(", "get_opts(@__fields__, ")]
  end

  def c(opts) do
    LiveView.Helpers.live_component(Map.merge(opts, %{id: Be.unique(10)}))
  end

  def get_paths(uri, params \\ %{}) do
    %URI{path: path} = URI.parse(uri)

    {current, _previous} =
      String.split(path, "/")
      |> Enum.reverse()
      |> case do
        [current_page, ""] -> {current_page, nil}
        [current_page | previous] -> {current_page, previous |> Enum.reverse() |> Enum.join("/")}
        [] -> {path, nil}
      end

    %{
      path: path,
      current_page: current,
      path_params: params
    }
  end
end
