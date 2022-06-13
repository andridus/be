defmodule Be.Component do
  alias Be.View
  @moduledoc false
  defmacro __using__(_opts) do
    quote do
      use Phoenix.LiveComponent
      @before_compile UI.Component
      import Be.View
      Module.register_attribute(__MODULE__, :file_path, [])
      Module.register_attribute(__MODULE__, :npm_import, accumulate: true)
      Module.register_attribute(__MODULE__, :fields, accumulate: true)
      Module.register_attribute(__MODULE__, :events, accumulate: true)
      Module.register_attribute(__MODULE__, :effects, accumulate: true)
      Module.register_attribute(__MODULE__, :javascripts, accumulate: true)
      Module.register_attribute(__MODULE__, :commands, accumulate: true)
    end
  end

  defmacro __before_compile__(env) do
    [init_component(env)]
  end

  defp init_component(_env) do
    prelude =
      quote do
        @doc false
        def mount(socket) do
          {:ok,
           socket
           |> assign(:__fields__, @fields)
           |> assign(:__module__, __MODULE__)
           |> assign(:run_once, false)
           |> apply_default_fields()}
        end
      end
    postlude =
      quote do
        @doc false
        def handle_event(event, params, socket) do
          func = Keyword.get(@events, String.to_atom(event))

          {assigns, socket} =
            if is_function(func, 2) do
              case func.(socket.assigns, params) do
                {data, command} ->
                  {data, socket |> View.apply_command(command, @commands, @javascripts)}

                data when is_map(data) ->
                  {data, socket}
              end
            else
              {socket.assigns, socket}
            end

          assigns = assigns |> clean_assigns()
          {:noreply, assign(socket, assigns)}
        end
      end

    init =
      quote do
        def update(%{__port__: event, __payload__: payload} = params, socket) do
          func = Keyword.get(@events, event)

          {assigns, socket} =
            if is_function(func, 2) do
              func.(socket.assigns, payload)
              |> case do
                {data, command} ->
                  {data, socket |> View.apply_command(command, @commands, @javascripts)}

                data when is_map(data) ->
                  {data, socket}
              end
            else
              {socket.assigns, socket}
            end

          assigns = assigns |> clean_assigns()
          {:ok, socket |> assign(assigns)}
        end

        def update(%{__event__: event} = params, socket) do
          opts = params[:__opts__] || []
          func = Keyword.get(@events, event)

          {assigns, socket} =
            if is_function(func, 2) do
              func.(socket.assigns, params)
              |> case do
                {data, command} ->
                  {data, socket |> View.apply_command(command, @commands, @javascripts)}

                data when is_map(data) ->
                  {data, socket}
              end
            else
              {socket.assigns, socket}
            end

          if opts[:effect] == true && opts[:every] do
            send_update_after(
              self(),
              socket.assigns.self_module,
              [id: socket.assigns.id, __event__: event, __opts__: opts],
              opts[:every]
            )
          end

          assigns = assigns |> clean_assigns()
          {:ok, socket |> assign(assigns)}
        end

        def update(assigns, socket) do
          {:ok,
           socket
           |> assign(assigns)
           |> View.apply_effects(@effects)
           |> View.apply_fields()
           |> View.merge_differences(assigns)}
        end
      end

    quote do
      unquote(prelude)
      unquote(postlude)
      unquote(init)
    end
  end
end
