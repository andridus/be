defmodule Be.Api do
  @moduledoc false
  defmacro __using__(_) do
    quote do
      import Ecto.Query
      import Be.Api

      def schema, do: @schema

      def changeset(changeset, params, type \\ "create") do
        apply(schema(), :"changeset_#{type}", [changeset, params])
      end

      def json_fields(append) do
        schema().__json__() ++ append
      end

      def get_by(params \\ [where: [], order: [asc: :inserted_at]]) do
        get_by!(params)
        |> case do
          nil -> {:error, :not_found}
          data -> {:ok, data}
        end
      end

      def get_by!(params \\ [where: [], order: [asc: :inserted_at]]) do
        params
        |> default_params()
        |> repo().one()
      end

      def get(id, params \\ [where: [], order: [asc: :inserted_at]]) do
        get!(id, params)
        |> case do
          nil -> {:error, :not_found}
          data -> {:ok, data}
        end
      end

      def get!(id, params \\ [where: [], order: [asc: :inserted_at]]) do
        params
        |> default_params()
        |> repo().get(id)
      end

      def blank(), do: struct(schema())

      def all(params \\ [where: [], order: []]) do
        params
        |> default_params()
        |> repo().all()
      end

      def insert(%Ecto.Changeset{} = model),
        do: model |> repo().insert()
      def insert(params) do
        schema()
        |> struct()
        |> schema().changeset_insert(params)
        |> repo().insert()
      end
      def exists?(params) do
        params
        |> default_params()
        |> repo().exists?()
      end

      def update(%Ecto.Changeset{} = model),
        do: model |> repo().update()
      def update(%{"id" => id} = model) do
        params = Map.drop(model, ["id"])
        __update_model__(id, params)
      end
      def update(%{id: id} = model) do
        params = Map.drop(model, [:id])
        __update_model__(id, params)
      end
      defp __update_model__(id, params) do
        id
        |> get!()
        |> schema().changeset_update(params)
        |> repo().update()
      end

      def delete(id) when is_bitstring(id),
        do: id |> get!() |> delete()
      def delete(model), do: model |> repo().delete()

      def default_params(params, sc \\ nil) do
        sch = from(sc || schema())
        params
        |> Enum.reduce(sch, fn
          {:where, params}, sch ->
            params =
              Enum.reduce(params, sch, fn
                {{:ilike, key}, value}, sch ->
                  value = "%#{value}%"
                  sch |> where([p], ilike(field(p, ^key), ^value))

                {{:in, key}, value}, sch ->
                  sch |> where([p], field(p, ^key) in ^value)

                {key, nil}, sch ->
                  sch |> where([p], is_nil(field(p, ^key)))

                x, sch ->
                  sch |> where(^Keyword.new([x]))
              end)
            {:or_where, params}, sch ->
              params =
                Enum.reduce(params, sch, fn
                  {{:ilike, key}, value}, sch ->
                    value = "%#{value}%"
                    sch |> or_where([p], ilike(field(p, ^key), ^value))

                  {{:in, key}, value}, sch ->
                    sch |> or_where([p], field(p, ^key) in ^value)

                  {key, nil}, sch ->
                    sch |> or_where([p], is_nil(field(p, ^key)))

                  x, sch ->
                    sch |> or_where(^Keyword.new([x]))
                end)
          {:order, params}, sch ->
            sch |> order_by(^params)

          {:preload, params}, sch ->
            lst = Enum.reduce(params, [], fn
              {k, v}, acc ->
                {_k, _t, opts} = schema().__live_fields__() |> List.keyfind!(k,0)
                [{k, default_params(v, opts[:schema])} | acc]
              k, acc when is_atom(k) ->
                [k | acc]
            end) |> Enum.reverse()
            sch |> preload(^lst)

          {:select, params}, sch ->
            sch |> select([c], map(c, ^params))

          {:limit, params}, sch ->
            sch |> limit(^params)

          {:offset, params}, sch ->
            sch |> offset(^params)

          _, sch ->
            sch
        end)
      end

      def count(params \\ []) do
        params
        |> default_params()
        |> repo().aggregate(:count, :id)
      end

      @doc """
        get json data
      """
      def json(_model, _include \\ [])
      def json(nil, _), do: nil
      def json(model, include) do
        model
        |> preload_json(include)
        |> Map.take(json_fields(include))
      end

      def insert_or_update(%Ecto.Changeset{action: :insert} = model), do: insert(model)
      def insert_or_update(%Ecto.Changeset{action: :update, data: %{id: _id}} = model), do: update(model)
      def insert_or_update(%{id: id} = model) when not is_nil(id), do: update(model)
      def insert_or_update(model), do: insert(model)

      defoverridable [changeset: 2, changeset: 3, json_fields: 1, get: 1, get: 2, get_by: 1, all: 1, insert: 1, update: 1, delete: 1, json: 2, count: 1]
    end
  end

  def repo, do: Application.get_env(:be, :repo)

  def preload_json(model, include \\ []) do
    model
    |> repo().preload(include)
    |> Map.take(include)
    |> Map.to_list()
    |> Enum.map(fn {key, value} ->
      module = get_module(model, key, include)
      if is_list(value) do
        {key, Enum.map(value, &(apply(module, :json, [&1]) |> unwrap()))}
      else
        {key, apply(module, :json, [value]) |> unwrap()}
      end
    end)
    |> Map.new()
    |> then(& Map.merge(model, &1))
  end

  def get_module(model, key, _includes) do
    Ecto.build_assoc(model, key, %{})
    |> Map.get(:__struct__)
    |> to_string()
    |> String.replace("Schema", "Api")
    |> atomize()
  end

  def atomize(string) when is_bitstring(string) do
    string |> String.to_existing_atom()
  rescue
    ArgumentError -> String.to_atom(string)
  end

  def unwrap({:ok, value}), do: value
  def unwrap(err), do: err
  def unwrap!({_, value}), do: value
end
